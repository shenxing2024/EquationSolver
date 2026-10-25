#include "history_store.h"

#include <windows.h>

#include <bcrypt.h>

#include <algorithm>
#include <array>
#include <atomic>
#include <fstream>
#include <set>
#include <sstream>
#include <stdexcept>
#include <tuple>

namespace history {
namespace {

constexpr size_t kMaximumFileBytes = 8 * 1024 * 1024;
constexpr size_t kMaximumImportEntries = 10000;
constexpr char kHistoryHeader[] = "#EquationSolver-History\t1";
constexpr char kDeviceHeader[] = "EquationSolver-device-v1\n";

[[noreturn]] void fail() {
    throw std::runtime_error("EquationSolver history storage or format error");
}

bool validFingerprint(const std::string& value) {
    return value.size() == 64 && std::all_of(value.begin(), value.end(), [](char ch) {
               return (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f');
           });
}

std::string readBytes(const std::filesystem::path& path, size_t limit) {
    std::ifstream input(path, std::ios::binary | std::ios::ate);
    if (!input)
        fail();
    const auto length = input.tellg();
    if (length < 0 || static_cast<unsigned long long>(length) > limit)
        fail();
    std::string bytes(static_cast<size_t>(length), '\0');
    input.seekg(0);
    if (!bytes.empty() && !input.read(bytes.data(), static_cast<std::streamsize>(bytes.size())))
        fail();
    return bytes;
}

// 先完成临时文件的写入和刷新，再替换目标；写入失败时保留原文件。
void writeAtomically(const std::filesystem::path& path, const std::string& bytes) {
    static std::atomic<unsigned long> sequence{0};
    std::filesystem::path temporary;
    HANDLE file = INVALID_HANDLE_VALUE;
    for (int attempt = 0; attempt < 16; ++attempt) {
        temporary = path;
        temporary += L".tmp-" + std::to_wstring(GetCurrentProcessId()) + L"-" +
                     std::to_wstring(GetTickCount64()) + L"-" + std::to_wstring(++sequence);
        file = CreateFileW(temporary.c_str(), GENERIC_WRITE, 0, nullptr, CREATE_NEW, FILE_ATTRIBUTE_NORMAL,
                           nullptr);
        if (file != INVALID_HANDLE_VALUE || GetLastError() != ERROR_FILE_EXISTS)
            break;
    }
    if (file == INVALID_HANDLE_VALUE)
        fail();
    DWORD written = 0;
    const bool saved = WriteFile(file, bytes.data(), static_cast<DWORD>(bytes.size()), &written, nullptr) &&
                       written == bytes.size() && FlushFileBuffers(file);
    const bool closed = CloseHandle(file) != FALSE;
    if (!saved || !closed ||
        !MoveFileExW(temporary.c_str(), path.c_str(), MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH)) {
        DeleteFileW(temporary.c_str());
        fail();
    }
}

std::string encodeField(const std::wstring& value) {
    if (value.empty())
        return {};
    const int count = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, value.data(),
                                          static_cast<int>(value.size()), nullptr, 0, nullptr, nullptr);
    if (count <= 0)
        fail();
    std::string bytes(static_cast<size_t>(count), '\0');
    if (!WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, value.data(), static_cast<int>(value.size()),
                             bytes.data(), count, nullptr, nullptr))
        fail();
    std::string escaped;
    for (char ch : bytes) {
        switch (ch) {
        case '\\':
            escaped += "\\\\";
            break;
        case '\t':
            escaped += "\\t";
            break;
        case '\r':
            escaped += "\\r";
            break;
        case '\n':
            escaped += "\\n";
            break;
        default:
            escaped += ch;
            break;
        }
    }
    return escaped;
}

std::wstring decodeField(const std::string& value) {
    std::string bytes;
    for (size_t i = 0; i < value.size(); ++i) {
        char ch = value[i];
        if (ch == '\\') {
            if (++i == value.size())
                fail();
            switch (value[i]) {
            case '\\':
                ch = '\\';
                break;
            case 't':
                ch = '\t';
                break;
            case 'r':
                ch = '\r';
                break;
            case 'n':
                ch = '\n';
                break;
            default:
                fail();
            }
        }
        bytes += ch;
    }
    if (bytes.empty())
        return {};
    const int count = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, bytes.data(),
                                          static_cast<int>(bytes.size()), nullptr, 0);
    if (count <= 0)
        fail();
    std::wstring result(static_cast<size_t>(count), L'\0');
    if (!MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, bytes.data(), static_cast<int>(bytes.size()),
                             result.data(), count))
        fail();
    return result;
}

bool validTime(const std::wstring& time) {
    if (time.size() != 19)
        return false;
    for (size_t i = 0; i < time.size(); ++i) {
        const wchar_t separator = i == 4 || i == 7 ? L'-' : i == 10 ? L' ' : i == 13 || i == 16 ? L':' : 0;
        if (separator ? time[i] != separator : (time[i] < L'0' || time[i] > L'9'))
            return false;
    }
    const int year = std::stoi(time.substr(0, 4));
    const int month = std::stoi(time.substr(5, 2));
    const int day = std::stoi(time.substr(8, 2));
    constexpr int days[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    if (year < 1601 || month < 1 || month > 12)
        return false;
    const bool leap = year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
    return day >= 1 && day <= days[month - 1] + (month == 2 && leap ? 1 : 0) &&
           std::stoi(time.substr(11, 2)) < 24 && std::stoi(time.substr(14, 2)) < 60 &&
           std::stoi(time.substr(17, 2)) < 60;
}

void validate(const Entry& entry) {
    if (!validTime(entry.time) || entry.type.empty() || entry.type.size() > 256 || entry.equation.empty() ||
        entry.equation.size() > 65536 || entry.equation.find_first_not_of(L" \t\r\n") == std::wstring::npos)
        fail();
    for (const auto* field : {&entry.time, &entry.type, &entry.equation}) {
        for (wchar_t ch : *field) {
            if (ch < 32 && (field != &entry.equation || (ch != L'\t' && ch != L'\r' && ch != L'\n')))
                fail();
        }
    }
}

std::string markerContents(const std::string& fingerprint) {
    return std::string(kDeviceHeader) + fingerprint + "\n";
}

} // namespace

std::filesystem::path defaultDirectory() {
    std::wstring base(32768, L'\0');
    const DWORD count =
        GetEnvironmentVariableW(L"LOCALAPPDATA", base.data(), static_cast<DWORD>(base.size()));
    if (!count || count >= base.size())
        fail(); // 不回退到共享临时目录或当前目录，避免把无关目录识别为用户数据。
    base.resize(count);
    const std::filesystem::path directory = std::filesystem::path(base) / L"EquationSolver";
    if (!directory.is_absolute())
        fail();
    return directory;
}

std::string deviceFingerprint() {
    wchar_t identity[256]{};
    DWORD bytes = sizeof(identity);
    const LSTATUS status =
        RegGetValueW(HKEY_LOCAL_MACHINE, L"SOFTWARE\\Microsoft\\Cryptography", L"MachineGuid",
                     RRF_RT_REG_SZ | RRF_SUBKEY_WOW6464KEY, nullptr, identity, &bytes);
    if (status != ERROR_SUCCESS || !identity[0])
        fail();
    std::wstring material = L"EquationSolver/device/v1:";
    for (const wchar_t ch : std::wstring(identity))
        material += ch >= L'A' && ch <= L'Z' ? ch + (L'a' - L'A') : ch;
    BCRYPT_ALG_HANDLE algorithm = nullptr;
    if (BCryptOpenAlgorithmProvider(&algorithm, BCRYPT_SHA256_ALGORITHM, nullptr, 0) < 0)
        fail();
    std::array<unsigned char, 32> digest{};
    const NTSTATUS hashed = BCryptHash(algorithm, nullptr, 0, reinterpret_cast<PUCHAR>(material.data()),
                                       static_cast<ULONG>(material.size() * sizeof(wchar_t)), digest.data(),
                                       static_cast<ULONG>(digest.size()));
    BCryptCloseAlgorithmProvider(algorithm, 0);
    if (hashed < 0)
        fail();
    constexpr char digits[] = "0123456789abcdef";
    std::string result;
    for (unsigned char ch : digest) {
        result += digits[ch >> 4];
        result += digits[ch & 15];
    }
    return result;
}

std::vector<Entry> readBackup(const std::filesystem::path& path) {
    std::string bytes = readBytes(path, kMaximumFileBytes);
    if (bytes.rfind("\xEF\xBB\xBF", 0) == 0)
        bytes.erase(0, 3);
    std::istringstream stream(bytes);
    std::vector<Entry> result;
    std::string line;
    bool firstLine = true;
    while (std::getline(stream, line)) {
        if (!line.empty() && line.back() == '\r')
            line.pop_back();
        if (firstLine && line == kHistoryHeader) {
            firstLine = false;
            continue;
        }
        firstLine = false;
        const size_t first = line.find('\t');
        const size_t second = first == std::string::npos ? first : line.find('\t', first + 1);
        if (first == std::string::npos || second == std::string::npos ||
            line.find('\t', second + 1) != std::string::npos)
            fail();
        Entry entry{decodeField(line.substr(0, first)),
                    decodeField(line.substr(first + 1, second - first - 1)),
                    decodeField(line.substr(second + 1))};
        validate(entry);
        result.push_back(std::move(entry));
        if (result.size() > kMaximumImportEntries)
            fail();
    }
    return result;
}

void writeBackup(const std::filesystem::path& path, const std::vector<Entry>& entries) {
    if (entries.size() > kMaximumImportEntries)
        fail();
    std::string bytes = std::string(kHistoryHeader) + "\r\n";
    for (const auto& entry : entries) {
        validate(entry);
        bytes += encodeField(entry.time) + '\t' + encodeField(entry.type) + '\t' +
                 encodeField(entry.equation) + "\r\n";
        if (bytes.size() > kMaximumFileBytes)
            fail();
    }
    writeAtomically(path, bytes);
}

std::vector<Entry> merge(const std::vector<Entry>& existing, const std::vector<Entry>& incoming) {
    std::vector<Entry> result;
    std::set<std::tuple<std::wstring, std::wstring, std::wstring>> seen;
    for (const auto* source : {&existing, &incoming}) {
        for (const auto& entry : *source) {
            validate(entry);
            if (seen.emplace(entry.time, entry.type, entry.equation).second)
                result.push_back(entry);
        }
    }
    std::stable_sort(result.begin(), result.end(),
                     [](const Entry& a, const Entry& b) { return a.time < b.time; });
    if (result.size() > kMaximumEntries)
        result.erase(result.begin(), result.end() - kMaximumEntries);
    return result;
}

Store::Store(std::filesystem::path directory) : directory_(std::move(directory)) {
    if (!directory_.is_absolute())
        fail();
}

Startup Store::initialize(const std::string& fingerprint) {
    ready_ = false;
    if (!validFingerprint(fingerprint))
        fail(); // 识别失败不等于新电脑，必须在任何删除/覆盖前退出。
    const auto marker = directory_ / L"device-state.dat";
    const bool found = std::filesystem::exists(marker);
    Startup startup = Startup::FirstRun;
    if (found) {
        const std::string content = readBytes(marker, 256);
        const std::string prefix = kDeviceHeader;
        if (content.size() != prefix.size() + 65 || content.rfind(prefix, 0) != 0 || content.back() != '\n' ||
            !validFingerprint(content.substr(prefix.size(), 64)))
            fail(); // 损坏的标记也不是首次运行的证据。
        startup = content == markerContents(fingerprint) ? Startup::ExistingDevice : Startup::DifferentDevice;
    }
    if (startup != Startup::ExistingDevice) {
        std::filesystem::create_directories(directory_);
        // 当前唯一持久化的使用记录是 history.tsv；不遍历目录，不删除用户的导出文件。
        writeBackup(directory_ / L"history.tsv", {});
        writeAtomically(marker, markerContents(fingerprint));
    }
    fingerprint_ = fingerprint;
    ready_ = true;
    return startup;
}

void Store::requireReady() const {
    if (!ready_ || readBytes(directory_ / L"device-state.dat", 256) != markerContents(fingerprint_))
        fail();
}

std::vector<Entry> Store::load() const {
    requireReady();
    const auto path = directory_ / L"history.tsv";
    return std::filesystem::exists(path) ? merge({}, readBackup(path)) : std::vector<Entry>{};
}

void Store::save(const std::vector<Entry>& entries) const {
    requireReady();
    writeBackup(directory_ / L"history.tsv", merge({}, entries));
}

void Store::exportTo(const std::filesystem::path& path, const std::vector<Entry>& entries) const {
    // 备份必须放在数据目录以外，避免用户把设备标记或正在使用的日志覆盖成备份。
    if (std::filesystem::weakly_canonical(path.parent_path()) ==
        std::filesystem::weakly_canonical(directory_))
        fail();
    writeBackup(path, entries);
}

} // namespace history
