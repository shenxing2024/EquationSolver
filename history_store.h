// 日志持久化、备份交换和本机首次运行检查；与界面解耦，便于用隔离目录测试。
#pragma once

#include <cstddef>
#include <filesystem>
#include <string>
#include <vector>

namespace history {

constexpr size_t kMaximumEntries = 100;

struct Entry {
    std::wstring time;
    std::wstring type;
    std::wstring equation;
};

enum class Startup { ExistingDevice, FirstRun, DifferentDevice };

std::filesystem::path defaultDirectory();
// 只返回 SHA-256 摘要，不存储或导出原始 Windows 系统标识。
std::string deviceFingerprint();

// UTF-8 TSV，支持多行方程、中文及引号；同时兼容旧版无文件头的日志。
std::vector<Entry> readBackup(const std::filesystem::path& path);
void writeBackup(const std::filesystem::path& path, const std::vector<Entry>& entries);
std::vector<Entry> merge(const std::vector<Entry>& existing, const std::vector<Entry>& incoming);

class Store {
  public:
    explicit Store(std::filesystem::path directory);
    Startup initialize(const std::string& fingerprint);
    std::vector<Entry> load() const;
    void save(const std::vector<Entry>& entries) const;
    void exportTo(const std::filesystem::path& path, const std::vector<Entry>& entries) const;

  private:
    void requireReady() const;
    std::filesystem::path directory_;
    std::string fingerprint_;
    bool ready_ = false;
};

} // namespace history
