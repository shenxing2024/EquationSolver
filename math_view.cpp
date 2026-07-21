// 将一维 Unicode 数学文本拆成普通文本、分数和根式，再用 GDI 进行二维排版。
// 控件自行管理滚动位置、暗色主题、复制操作和紧凑行距。

#include "math_view.h"

#include <algorithm>
#include <cstring>
#include <cwctype>
#include <limits>
#include <string>
#include <vector>
#include <windowsx.h>

namespace mathview {
namespace {

constexpr int kScrollbarWidth = 16;
constexpr int kArrowHeight = 16;
constexpr int kPaddingX = 12;
constexpr int kPaddingY = 9;
constexpr int kScrollStep = 42;

enum class ElementKind { Text, Fraction, Radical };

struct Element {
    ElementKind kind = ElementKind::Text;
    std::wstring first;
    std::wstring second;
};

struct ViewState {
    std::wstring text;
    HFONT font = nullptr;
    bool darkMode = false;
    int scrollY = 0;
    int contentHeight = 0;
    bool dragging = false;
    int dragOffset = 0;
};

COLORREF backgroundColor(const ViewState& state) {
    return state.darkMode ? RGB(31, 35, 42) : RGB(255, 255, 255);
}

COLORREF textColor(const ViewState& state) {
    return state.darkMode ? RGB(232, 236, 242) : RGB(35, 44, 55);
}

bool isAtomCharacter(wchar_t ch) {
    return std::iswalnum(ch) || ch == L'√' || ch == L'∛' || ch == L'i' || ch == L'₁' || ch == L'₂' ||
           ch == L'₃' || ch == L'ₓ' || ch == L'ᵧ' || ch == L'^' || ch == L'_' || ch == L'.' || ch == L'-';
}

bool parseFraction(const std::wstring& line, size_t start, Element& element, size_t& consumed) {
    // 从除号向两侧扩展“原子”，避免把相邻的加减项纳入分子或分母。
    size_t slash = std::wstring::npos;
    size_t numeratorEnd = start;
    std::wstring numerator;
    if (start < line.size() && line[start] == L'(') {
        int depth = 0;
        size_t close = start;
        for (; close < line.size(); ++close) {
            if (line[close] == L'(')
                ++depth;
            else if (line[close] == L')' && --depth == 0)
                break;
        }
        if (close >= line.size() || close + 1 >= line.size() || line[close + 1] != L'/')
            return false;
        numerator = line.substr(start + 1, close - start - 1);
        slash = close + 1;
        numeratorEnd = close + 1;
    } else {
        size_t cursor = start;
        while (cursor < line.size() && isAtomCharacter(line[cursor]))
            ++cursor;
        if (cursor == start || cursor >= line.size() || line[cursor] != L'/')
            return false;
        numerator = line.substr(start, cursor - start);
        slash = cursor;
        numeratorEnd = cursor;
    }
    (void)numeratorEnd;
    size_t denominatorStart = slash + 1;
    if (denominatorStart >= line.size())
        return false;
    std::wstring denominator;
    size_t end = denominatorStart;
    if (line[denominatorStart] == L'(') {
        int depth = 0;
        for (; end < line.size(); ++end) {
            if (line[end] == L'(')
                ++depth;
            else if (line[end] == L')' && --depth == 0) {
                ++end;
                break;
            }
        }
        if (end > line.size())
            return false;
        denominator = line.substr(denominatorStart + 1, end - denominatorStart - 2);
    } else {
        while (end < line.size() && isAtomCharacter(line[end]))
            ++end;
        if (end == denominatorStart)
            return false;
        denominator = line.substr(denominatorStart, end - denominatorStart);
    }
    element = {ElementKind::Fraction, numerator, denominator};
    consumed = end - start;
    return true;
}

bool parseRadical(const std::wstring& line, size_t start, Element& element, size_t& consumed) {
    // 同时识别 √、∛ 与 √[n]{...}，并保留嵌套内容供递归绘制。
    if (line[start] != L'√' && line[start] != L'∛')
        return false;
    size_t cursor = start + 1;
    std::wstring index;
    if (line[start] == L'∛')
        index = L"3";
    if (cursor < line.size() && line[cursor] == L'[') {
        const size_t close = line.find(L']', cursor + 1);
        if (close != std::wstring::npos) {
            index = line.substr(cursor + 1, close - cursor - 1);
            cursor = close + 1;
        }
    }
    std::wstring radicand;
    if (cursor < line.size() && line[cursor] == L'{') {
        const size_t close = line.find(L'}', cursor + 1);
        if (close == std::wstring::npos)
            return false;
        radicand = line.substr(cursor + 1, close - cursor - 1);
        cursor = close + 1;
    } else {
        const size_t begin = cursor;
        while (cursor < line.size() && (std::iswdigit(line[cursor]) || line[cursor] == L'.'))
            ++cursor;
        if (cursor == begin)
            return false;
        radicand = line.substr(begin, cursor - begin);
    }
    element = {ElementKind::Radical, radicand, index};
    consumed = cursor - start;
    return true;
}

std::vector<Element> parseElements(const std::wstring& line) {
    std::vector<Element> result;
    std::wstring plain;
    auto flush = [&]() {
        if (!plain.empty()) {
            result.push_back({ElementKind::Text, plain, L""});
            plain.clear();
        }
    };
    size_t i = 0;
    while (i < line.size()) {
        Element element;
        size_t consumed = 0;
        if (parseFraction(line, i, element, consumed) || parseRadical(line, i, element, consumed)) {
            flush();
            result.push_back(std::move(element));
            i += consumed;
        } else {
            plain += line[i++];
        }
    }
    flush();
    return result;
}

int measureText(HDC dc, const std::wstring& text) {
    SIZE size{};
    GetTextExtentPoint32W(dc, text.c_str(), static_cast<int>(text.size()), &size);
    return size.cx;
}

struct ElementMetrics {
    int width = 0;
    int height = 0;
};

int inlineMathWidth(HDC dc, const std::wstring& text, int baseHeight);
void drawInlineMath(HDC dc, const std::wstring& text, int x, int top, int lineHeight, int baseHeight,
                    COLORREF color);

ElementMetrics measureElement(HDC dc, const Element& element, int baseHeight) {
    if (element.kind == ElementKind::Text)
        return {measureText(dc, element.first), baseHeight + 6};
    if (element.kind == ElementKind::Fraction) {
        return {std::max(inlineMathWidth(dc, element.first, baseHeight),
                         inlineMathWidth(dc, element.second, baseHeight)) +
                    8,
                baseHeight * 2 + 9};
    }
    const int indexWidth = element.second.empty() ? 0 : std::max(5, measureText(dc, element.second) / 2);
    int radicandHeight = baseHeight + 4;
    for (const auto& part : parseElements(element.first)) {
        radicandHeight = std::max(radicandHeight, measureElement(dc, part, baseHeight).height);
    }
    return {measureText(dc, L"√") + inlineMathWidth(dc, element.first, baseHeight) + indexWidth + 4,
            std::max(baseHeight + 8, radicandHeight + 4)};
}

void drawElement(HDC dc, const Element& element, int x, int top, int lineHeight, int baseHeight,
                 COLORREF color) {
    SetTextColor(dc, color);
    if (element.kind == ElementKind::Text) {
        TextOutW(dc, x, top + (lineHeight - baseHeight) / 2, element.first.c_str(),
                 static_cast<int>(element.first.size()));
        return;
    }
    if (element.kind == ElementKind::Fraction) {
        const int numeratorWidth = inlineMathWidth(dc, element.first, baseHeight);
        const int denominatorWidth = inlineMathWidth(dc, element.second, baseHeight);
        const int width = std::max(numeratorWidth, denominatorWidth) + 8;
        const int center = top + lineHeight / 2;
        drawInlineMath(dc, element.first, x + (width - numeratorWidth) / 2, center - baseHeight - 2,
                       baseHeight + 4, baseHeight, color);
        HPEN pen = CreatePen(PS_SOLID, 1, color);
        HGDIOBJ oldPen = SelectObject(dc, pen);
        MoveToEx(dc, x + 1, center, nullptr);
        LineTo(dc, x + width - 1, center);
        SelectObject(dc, oldPen);
        DeleteObject(pen);
        drawInlineMath(dc, element.second, x + (width - denominatorWidth) / 2, center + 1, baseHeight + 4,
                       baseHeight, color);
        return;
    }

    int radicandHeight = baseHeight + 4;
    for (const auto& part : parseElements(element.first)) {
        radicandHeight = std::max(radicandHeight, measureElement(dc, part, baseHeight).height);
    }
    const int radicandTop = top + (lineHeight - radicandHeight) / 2;
    const int y = radicandTop + (radicandHeight - baseHeight) / 2;
    const int rootWidth = measureText(dc, L"√");
    int cursor = x;
    if (!element.second.empty()) {
        LOGFONTW logFont{};
        HFONT currentFont = reinterpret_cast<HFONT>(GetCurrentObject(dc, OBJ_FONT));
        GetObjectW(currentFont, sizeof(logFont), &logFont);
        logFont.lfHeight = static_cast<LONG>(logFont.lfHeight * 0.62);
        HFONT indexFont = CreateFontIndirectW(&logFont);
        HGDIOBJ oldFont = SelectObject(dc, indexFont);
        TextOutW(dc, cursor, y, element.second.c_str(), static_cast<int>(element.second.size()));
        const int indexWidth = measureText(dc, element.second);
        SelectObject(dc, oldFont);
        DeleteObject(indexFont);
        cursor += std::max(5, indexWidth - 2);
    }
    TextOutW(dc, cursor, y, L"√", 1);
    cursor += rootWidth - 1;
    drawInlineMath(dc, element.first, cursor, radicandTop, radicandHeight, baseHeight, color);
    const int radicandWidth = inlineMathWidth(dc, element.first, baseHeight);
    HPEN pen = CreatePen(PS_SOLID, 1, color);
    HGDIOBJ oldPen = SelectObject(dc, pen);
    MoveToEx(dc, cursor - 1, y + 1, nullptr);
    LineTo(dc, cursor + radicandWidth + 1, y + 1);
    SelectObject(dc, oldPen);
    DeleteObject(pen);
}

int inlineMathWidth(HDC dc, const std::wstring& text, int baseHeight) {
    int width = 0;
    for (const auto& element : parseElements(text)) {
        width += measureElement(dc, element, baseHeight).width;
    }
    return width;
}

void drawInlineMath(HDC dc, const std::wstring& text, int x, int top, int lineHeight, int baseHeight,
                    COLORREF color) {
    int cursor = x;
    for (const auto& element : parseElements(text)) {
        drawElement(dc, element, cursor, top, lineHeight, baseHeight, color);
        cursor += measureElement(dc, element, baseHeight).width;
    }
}

std::vector<std::wstring> splitLines(const std::wstring& text) {
    std::vector<std::wstring> lines;
    std::wstring line;
    for (wchar_t ch : text) {
        if (ch == L'\n') {
            if (!line.empty() && line.back() == L'\r')
                line.pop_back();
            lines.push_back(line);
            line.clear();
        } else {
            line += ch;
        }
    }
    if (!line.empty() && line.back() == L'\r')
        line.pop_back();
    lines.push_back(line);
    return lines;
}

int layoutAndDraw(ViewState& state, HDC dc, const RECT& client, bool draw, bool hasScrollbar,
                  int maxVisualLines, bool plainText) {
    HFONT font = state.font ? state.font : reinterpret_cast<HFONT>(GetStockObject(DEFAULT_GUI_FONT));
    HGDIOBJ oldFont = SelectObject(dc, font);
    const int oldBackgroundMode = SetBkMode(dc, TRANSPARENT);
    TEXTMETRICW metrics{};
    GetTextMetricsW(dc, &metrics);
    const int baseHeight = metrics.tmHeight;
    const int right = client.right - (hasScrollbar ? kScrollbarWidth : 0) - kPaddingX;
    const int availableWidth = std::max(80, right - kPaddingX);
    int y = kPaddingY - state.scrollY;
    int visualLines = 0;
    bool limitReached = false;

    for (const auto& line : splitLines(state.text)) {
        if (visualLines >= maxVisualLines)
            break;
        const std::vector<Element> elements =
            plainText ? std::vector<Element>{{ElementKind::Text, line, L""}} : parseElements(line);
        int lineHeight = baseHeight + 3;
        for (const auto& element : elements)
            lineHeight = std::max(lineHeight, measureElement(dc, element, baseHeight).height);
        int x = kPaddingX;
        if (elements.empty()) {
            y += std::max(6, baseHeight / 2);
            continue;
        }
        for (const auto& element : elements) {
            if (element.kind == ElementKind::Text) {
                std::wstring chunk;
                for (wchar_t ch : element.first) {
                    const std::wstring candidate = chunk + ch;
                    const int candidateWidth = measureText(dc, candidate);
                    if (!chunk.empty() && x + candidateWidth > kPaddingX + availableWidth) {
                        if (draw)
                            drawElement(dc, {ElementKind::Text, chunk, L""}, x, y, lineHeight, baseHeight,
                                        textColor(state));
                        ++visualLines;
                        if (visualLines >= maxVisualLines) {
                            limitReached = true;
                            break;
                        }
                        y += lineHeight;
                        x = kPaddingX;
                        chunk.assign(1, ch);
                    } else {
                        chunk = candidate;
                    }
                }
                if (limitReached)
                    break;
                if (!chunk.empty()) {
                    const int width = measureText(dc, chunk);
                    if (draw)
                        drawElement(dc, {ElementKind::Text, chunk, L""}, x, y, lineHeight, baseHeight,
                                    textColor(state));
                    x += width;
                }
            } else {
                const auto elementMetrics = measureElement(dc, element, baseHeight);
                if (x > kPaddingX && x + elementMetrics.width > right) {
                    ++visualLines;
                    if (visualLines >= maxVisualLines) {
                        limitReached = true;
                        break;
                    }
                    y += lineHeight;
                    x = kPaddingX;
                }
                if (draw)
                    drawElement(dc, element, x, y, lineHeight, baseHeight, textColor(state));
                x += elementMetrics.width;
            }
        }
        if (limitReached)
            break;
        ++visualLines;
        y += lineHeight;
    }
    SelectObject(dc, oldFont);
    SetBkMode(dc, oldBackgroundMode);
    return y + state.scrollY + kPaddingY;
}

RECT thumbRect(const ViewState& state, const RECT& client) {
    const int trackTop = kArrowHeight;
    const int clientHeight = static_cast<int>(client.bottom);
    const int trackHeight = std::max(1, clientHeight - 2 * kArrowHeight);
    const int viewport = std::max(1, clientHeight);
    const int thumbHeight = state.contentHeight <= viewport
                                ? trackHeight
                                : std::max(28, trackHeight * viewport / state.contentHeight);
    const int maxScroll = std::max(0, state.contentHeight - viewport);
    const int travel = std::max(0, trackHeight - thumbHeight);
    const int thumbTop = trackTop + (maxScroll == 0 ? 0 : travel * state.scrollY / maxScroll);
    return {client.right - 12, thumbTop, client.right - 4, thumbTop + thumbHeight};
}

void clampScroll(ViewState& state, const RECT& client) {
    state.scrollY =
        std::clamp(state.scrollY, 0, std::max(0, state.contentHeight - static_cast<int>(client.bottom)));
}

void drawScrollbar(HDC dc, const RECT& client, const ViewState& state) {
    // 滑块长度按“可见高度 / 内容高度”计算，并沿用界面的圆角视觉样式。
    RECT track{client.right - kScrollbarWidth, 0, client.right, client.bottom};
    HBRUSH trackBrush = CreateSolidBrush(state.darkMode ? RGB(27, 30, 36) : RGB(250, 250, 250));
    FillRect(dc, &track, trackBrush);
    DeleteObject(trackBrush);

    const COLORREF accent = state.darkMode ? RGB(155, 160, 168) : RGB(105, 108, 112);
    HPEN pen = CreatePen(PS_SOLID, 1, accent);
    HBRUSH brush = CreateSolidBrush(accent);
    HGDIOBJ oldPen = SelectObject(dc, pen);
    HGDIOBJ oldBrush = SelectObject(dc, brush);
    POINT up[]{{client.right - 8, 4}, {client.right - 12, 10}, {client.right - 4, 10}};
    POINT down[]{{client.right - 12, client.bottom - 10},
                 {client.right - 4, client.bottom - 10},
                 {client.right - 8, client.bottom - 4}};
    Polygon(dc, up, 3);
    Polygon(dc, down, 3);
    if (state.contentHeight > client.bottom) {
        const RECT thumb = thumbRect(state, client);
        RoundRect(dc, thumb.left, thumb.top, thumb.right, thumb.bottom, 8, 8);
    }
    SelectObject(dc, oldBrush);
    SelectObject(dc, oldPen);
    DeleteObject(brush);
    DeleteObject(pen);
}

void copyTextToClipboard(HWND window, const std::wstring& text) {
    if (!OpenClipboard(window))
        return;
    EmptyClipboard();
    const size_t bytes = (text.size() + 1) * sizeof(wchar_t);
    HGLOBAL memory = GlobalAlloc(GMEM_MOVEABLE, bytes);
    if (memory) {
        void* data = GlobalLock(memory);
        memcpy(data, text.c_str(), bytes);
        GlobalUnlock(memory);
        SetClipboardData(CF_UNICODETEXT, memory);
    }
    CloseClipboard();
}

LRESULT CALLBACK windowProc(HWND window, UINT message, WPARAM wParam, LPARAM lParam) {
    auto* state = reinterpret_cast<ViewState*>(GetWindowLongPtrW(window, GWLP_USERDATA));
    if (message == WM_CREATE) {
        state = new ViewState();
        SetWindowLongPtrW(window, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(state));
        return 0;
    }
    if (!state)
        return DefWindowProcW(window, message, wParam, lParam);

    switch (message) {
    case WM_SETTEXT:
        state->text = lParam ? reinterpret_cast<const wchar_t*>(lParam) : L"";
        state->scrollY = 0;
        InvalidateRect(window, nullptr, TRUE);
        return TRUE;
    case WM_GETTEXTLENGTH:
        return static_cast<LRESULT>(state->text.size());
    case WM_GETTEXT: {
        const size_t capacity = static_cast<size_t>(wParam);
        if (capacity == 0)
            return 0;
        auto* destination = reinterpret_cast<wchar_t*>(lParam);
        const size_t count = std::min(capacity - 1, state->text.size());
        memcpy(destination, state->text.data(), count * sizeof(wchar_t));
        destination[count] = L'\0';
        return static_cast<LRESULT>(count);
    }
    case WM_SETFONT:
        state->font = reinterpret_cast<HFONT>(wParam);
        if (lParam)
            InvalidateRect(window, nullptr, TRUE);
        return 0;
    case kSetDarkMode:
        state->darkMode = wParam != 0;
        InvalidateRect(window, nullptr, TRUE);
        return 0;
    case WM_ERASEBKGND:
        return 1;
    case WM_PAINT: {
        PAINTSTRUCT paint{};
        HDC dc = BeginPaint(window, &paint);
        RECT client{};
        GetClientRect(window, &client);
        HDC memoryDc = CreateCompatibleDC(dc);
        HBITMAP bitmap = CreateCompatibleBitmap(dc, client.right, client.bottom);
        HGDIOBJ oldBitmap = SelectObject(memoryDc, bitmap);
        HBRUSH background = CreateSolidBrush(backgroundColor(*state));
        FillRect(memoryDc, &client, background);
        DeleteObject(background);
        const LONG_PTR style = GetWindowLongPtrW(window, GWL_STYLE);
        const bool hasScrollbar = (style & kVerticalScroll) != 0;
        const bool plainText = (style & kPlainText) != 0;
        const int maxVisualLines = (style & kTwoLineSummary) != 0 ? 2 : std::numeric_limits<int>::max();
        if (!hasScrollbar)
            state->scrollY = 0;
        state->contentHeight =
            layoutAndDraw(*state, memoryDc, client, false, hasScrollbar, maxVisualLines, plainText);
        clampScroll(*state, client);
        layoutAndDraw(*state, memoryDc, client, true, hasScrollbar, maxVisualLines, plainText);
        if (hasScrollbar)
            drawScrollbar(memoryDc, client, *state);
        HPEN borderPen = CreatePen(PS_SOLID, 1, state->darkMode ? RGB(70, 76, 87) : RGB(205, 211, 220));
        HGDIOBJ oldPen = SelectObject(memoryDc, borderPen);
        HGDIOBJ oldBrush = SelectObject(memoryDc, GetStockObject(NULL_BRUSH));
        RoundRect(memoryDc, 0, 0, client.right, client.bottom, 18, 18);
        SelectObject(memoryDc, oldBrush);
        SelectObject(memoryDc, oldPen);
        DeleteObject(borderPen);
        BitBlt(dc, 0, 0, client.right, client.bottom, memoryDc, 0, 0, SRCCOPY);
        SelectObject(memoryDc, oldBitmap);
        DeleteObject(bitmap);
        DeleteDC(memoryDc);
        EndPaint(window, &paint);
        return 0;
    }
    case WM_SIZE: {
        RECT client{};
        GetClientRect(window, &client);
        HRGN region = CreateRoundRectRgn(0, 0, client.right + 1, client.bottom + 1, 18, 18);
        SetWindowRgn(window, region, TRUE);
        return 0;
    }
    case WM_MOUSEWHEEL: {
        if ((GetWindowLongPtrW(window, GWL_STYLE) & kVerticalScroll) == 0)
            return 0;
        RECT client{};
        GetClientRect(window, &client);
        state->scrollY -= GET_WHEEL_DELTA_WPARAM(wParam) / WHEEL_DELTA * kScrollStep;
        clampScroll(*state, client);
        InvalidateRect(window, nullptr, FALSE);
        return 0;
    }
    case WM_LBUTTONDOWN: {
        SetFocus(window);
        RECT client{};
        GetClientRect(window, &client);
        const int x = GET_X_LPARAM(lParam);
        const int y = GET_Y_LPARAM(lParam);
        if ((GetWindowLongPtrW(window, GWL_STYLE) & kVerticalScroll) && x >= client.right - kScrollbarWidth) {
            const RECT thumb = thumbRect(*state, client);
            POINT point{x, y};
            if (y < kArrowHeight)
                state->scrollY -= kScrollStep;
            else if (y >= client.bottom - kArrowHeight)
                state->scrollY += kScrollStep;
            else if (PtInRect(&thumb, point)) {
                state->dragging = true;
                state->dragOffset = y - thumb.top;
                SetCapture(window);
            } else if (y < thumb.top)
                state->scrollY -= client.bottom;
            else
                state->scrollY += client.bottom;
            clampScroll(*state, client);
            InvalidateRect(window, nullptr, FALSE);
        }
        return 0;
    }
    case WM_MOUSEMOVE:
        if (state->dragging) {
            RECT client{};
            GetClientRect(window, &client);
            const int clientHeight = static_cast<int>(client.bottom);
            const int trackHeight = std::max(1, clientHeight - 2 * kArrowHeight);
            const RECT thumb = thumbRect(*state, client);
            const int thumbHeight = thumb.bottom - thumb.top;
            const int travel = std::max(1, trackHeight - thumbHeight);
            const int position =
                std::clamp(GET_Y_LPARAM(lParam) - state->dragOffset - kArrowHeight, 0, travel);
            const int maxScroll = std::max(0, state->contentHeight - clientHeight);
            state->scrollY = position * maxScroll / travel;
            InvalidateRect(window, nullptr, FALSE);
        }
        return 0;
    case WM_LBUTTONUP:
        if (state->dragging) {
            state->dragging = false;
            ReleaseCapture();
        }
        return 0;
    case WM_KEYDOWN: {
        RECT client{};
        GetClientRect(window, &client);
        if (wParam == 'C' && (GetKeyState(VK_CONTROL) & 0x8000)) {
            copyTextToClipboard(window, state->text);
            return 0;
        }
        if ((GetWindowLongPtrW(window, GWL_STYLE) & kVerticalScroll) == 0) {
            return DefWindowProcW(window, message, wParam, lParam);
        }
        if (wParam == VK_UP)
            state->scrollY -= kScrollStep;
        else if (wParam == VK_DOWN)
            state->scrollY += kScrollStep;
        else if (wParam == VK_PRIOR)
            state->scrollY -= client.bottom;
        else if (wParam == VK_NEXT)
            state->scrollY += client.bottom;
        else if (wParam == VK_HOME)
            state->scrollY = 0;
        else if (wParam == VK_END)
            state->scrollY = std::max(0, state->contentHeight - static_cast<int>(client.bottom));
        else
            return DefWindowProcW(window, message, wParam, lParam);
        clampScroll(*state, client);
        InvalidateRect(window, nullptr, FALSE);
        return 0;
    }
    case WM_NCDESTROY:
        delete state;
        SetWindowLongPtrW(window, GWLP_USERDATA, 0);
        return DefWindowProcW(window, message, wParam, lParam);
    }
    return DefWindowProcW(window, message, wParam, lParam);
}

} // namespace

bool registerClass(HINSTANCE instance) {
    WNDCLASSEXW windowClass{};
    windowClass.cbSize = sizeof(windowClass);
    windowClass.lpfnWndProc = windowProc;
    windowClass.hInstance = instance;
    windowClass.hCursor = LoadCursorW(nullptr, IDC_ARROW);
    windowClass.lpszClassName = kClassName;
    return RegisterClassExW(&windowClass) != 0 || GetLastError() == ERROR_CLASS_ALREADY_EXISTS;
}

void setDarkMode(HWND window, bool darkMode) {
    SendMessageW(window, kSetDarkMode, darkMode ? 1 : 0, 0);
}

int preferredWidth(HWND window) {
    auto* state = reinterpret_cast<ViewState*>(GetWindowLongPtrW(window, GWLP_USERDATA));
    if (!state)
        return 0;
    HDC dc = GetDC(window);
    HFONT font = state->font ? state->font : reinterpret_cast<HFONT>(GetStockObject(DEFAULT_GUI_FONT));
    HGDIOBJ oldFont = SelectObject(dc, font);
    TEXTMETRICW metrics{};
    GetTextMetricsW(dc, &metrics);
    int maximum = 0;
    for (const auto& line : splitLines(state->text)) {
        int width = 0;
        for (const auto& element : parseElements(line)) {
            width += measureElement(dc, element, metrics.tmHeight).width;
        }
        maximum = std::max(maximum, width);
    }
    SelectObject(dc, oldFont);
    ReleaseDC(window, dc);
    return maximum + 2 * kPaddingX + 2;
}

int requiredHeight(HWND window, int width, int maxVisualLines) {
    auto* state = reinterpret_cast<ViewState*>(GetWindowLongPtrW(window, GWLP_USERDATA));
    if (!state || width <= 0)
        return 0;
    ViewState copy = *state;
    copy.scrollY = 0;
    HDC dc = GetDC(window);
    RECT virtualClient{0, 0, width, 10000};
    const bool plainText = (GetWindowLongPtrW(window, GWL_STYLE) & kPlainText) != 0;
    const int height = layoutAndDraw(copy, dc, virtualClient, false, false, maxVisualLines, plainText);
    ReleaseDC(window, dc);
    return height + 2;
}

} // namespace mathview
