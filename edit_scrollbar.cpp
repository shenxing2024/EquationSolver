// 该控件只负责滚动条的绘制和拖动；文本编辑、选区及光标仍由原生 EDIT 控件处理。

#include "edit_scrollbar.h"

#include <algorithm>
#include <commctrl.h>
#include <windowsx.h>

namespace editscroll {
namespace {

constexpr int kScrollbarWidth = 16;
constexpr int kArrowHeight = 16;
constexpr int kMinimumThumbHeight = 28;
constexpr UINT kSetDarkMode = WM_APP + 73;
constexpr UINT_PTR kEditSubclassId = 0x45534352;

struct ScrollbarState {
    HWND edit = nullptr;
    bool darkMode = false;
    bool dragging = false;
    int dragOffset = 0;
};

struct ScrollMetrics {
    int lineCount = 1;
    int visibleLines = 1;
    int firstLine = 0;
    int maxFirstLine = 0;
};

ScrollMetrics metrics(HWND edit) {
    ScrollMetrics result;
    if (!edit)
        return result;

    result.lineCount = std::max(1, static_cast<int>(SendMessageW(edit, EM_GETLINECOUNT, 0, 0)));
    result.firstLine = std::max(0, static_cast<int>(SendMessageW(edit, EM_GETFIRSTVISIBLELINE, 0, 0)));

    RECT formatRect{};
    SendMessageW(edit, EM_GETRECT, 0, reinterpret_cast<LPARAM>(&formatRect));
    HDC dc = GetDC(edit);
    const HFONT font = reinterpret_cast<HFONT>(SendMessageW(edit, WM_GETFONT, 0, 0));
    HGDIOBJ oldFont = font ? SelectObject(dc, font) : nullptr;
    TEXTMETRICW textMetric{};
    GetTextMetricsW(dc, &textMetric);
    if (oldFont)
        SelectObject(dc, oldFont);
    ReleaseDC(edit, dc);

    const int lineHeight = std::max(1, static_cast<int>(textMetric.tmHeight));
    result.visibleLines = std::max(1, static_cast<int>(formatRect.bottom - formatRect.top) / lineHeight);
    result.maxFirstLine = std::max(0, result.lineCount - result.visibleLines);
    result.firstLine = std::clamp(result.firstLine, 0, result.maxFirstLine);
    return result;
}

RECT thumbRect(const ScrollMetrics& scroll, const RECT& client) {
    const int trackHeight = std::max(1, static_cast<int>(client.bottom) - 2 * kArrowHeight);
    const int thumbHeight =
        scroll.maxFirstLine == 0
            ? trackHeight
            : std::max(kMinimumThumbHeight, trackHeight * scroll.visibleLines / scroll.lineCount);
    const int travel = std::max(0, trackHeight - thumbHeight);
    const int thumbTop =
        kArrowHeight + (scroll.maxFirstLine == 0 ? 0 : travel * scroll.firstLine / scroll.maxFirstLine);
    return {client.right - 12, thumbTop, client.right - 4, thumbTop + thumbHeight};
}

void scrollTo(HWND scrollbar, ScrollbarState& state, int targetLine) {
    const ScrollMetrics scroll = metrics(state.edit);
    const int clamped = std::clamp(targetLine, 0, scroll.maxFirstLine);
    SendMessageW(state.edit, EM_LINESCROLL, 0, clamped - scroll.firstLine);
    InvalidateRect(scrollbar, nullptr, FALSE);
}

LRESULT CALLBACK editSubclassProc(HWND edit, UINT message, WPARAM wParam, LPARAM lParam, UINT_PTR subclassId,
                                  DWORD_PTR reference) {
    const HWND scrollbar = reinterpret_cast<HWND>(reference);
    if (message == WM_MOUSEWHEEL) {
        const int lines = -GET_WHEEL_DELTA_WPARAM(wParam) / WHEEL_DELTA * 3;
        SendMessageW(edit, EM_LINESCROLL, 0, lines);
        InvalidateRect(scrollbar, nullptr, FALSE);
        return 0;
    }

    const LRESULT result = DefSubclassProc(edit, message, wParam, lParam);
    switch (message) {
    case WM_SETTEXT:
    case WM_CHAR:
    case WM_KEYDOWN:
    case WM_KEYUP:
    case WM_LBUTTONDOWN:
    case WM_LBUTTONUP:
    case WM_MOUSEMOVE:
    case WM_VSCROLL:
    case WM_SIZE:
    case EM_LINESCROLL:
    case EM_REPLACESEL:
    case WM_CUT:
    case WM_PASTE:
    case WM_CLEAR:
        InvalidateRect(scrollbar, nullptr, FALSE);
        break;
    case WM_NCDESTROY:
        RemoveWindowSubclass(edit, editSubclassProc, subclassId);
        break;
    default:
        break;
    }
    return result;
}

void paintScrollbar(HWND window, const ScrollbarState& state) {
    PAINTSTRUCT paint{};
    HDC dc = BeginPaint(window, &paint);
    RECT client{};
    GetClientRect(window, &client);
    HDC memoryDc = CreateCompatibleDC(dc);
    HBITMAP bitmap = CreateCompatibleBitmap(dc, std::max(1L, client.right), std::max(1L, client.bottom));
    HGDIOBJ oldBitmap = SelectObject(memoryDc, bitmap);

    const COLORREF trackColor = state.darkMode ? RGB(27, 30, 36) : RGB(250, 250, 250);
    HBRUSH trackBrush = CreateSolidBrush(trackColor);
    FillRect(memoryDc, &client, trackBrush);
    DeleteObject(trackBrush);

    const COLORREF accent = state.darkMode ? RGB(155, 160, 168) : RGB(105, 108, 112);
    HPEN pen = CreatePen(PS_SOLID, 1, accent);
    HBRUSH brush = CreateSolidBrush(accent);
    HGDIOBJ oldPen = SelectObject(memoryDc, pen);
    HGDIOBJ oldBrush = SelectObject(memoryDc, brush);
    POINT up[]{{client.right - 8, 4}, {client.right - 12, 10}, {client.right - 4, 10}};
    POINT down[]{{client.right - 12, client.bottom - 10},
                 {client.right - 4, client.bottom - 10},
                 {client.right - 8, client.bottom - 4}};
    Polygon(memoryDc, up, 3);
    Polygon(memoryDc, down, 3);
    const ScrollMetrics scroll = metrics(state.edit);
    if (scroll.maxFirstLine > 0) {
        const RECT thumb = thumbRect(scroll, client);
        RoundRect(memoryDc, thumb.left, thumb.top, thumb.right, thumb.bottom, 8, 8);
    }

    SelectObject(memoryDc, oldBrush);
    SelectObject(memoryDc, oldPen);
    DeleteObject(brush);
    DeleteObject(pen);
    BitBlt(dc, 0, 0, client.right, client.bottom, memoryDc, 0, 0, SRCCOPY);
    SelectObject(memoryDc, oldBitmap);
    DeleteObject(bitmap);
    DeleteDC(memoryDc);
    EndPaint(window, &paint);
}

LRESULT CALLBACK windowProc(HWND window, UINT message, WPARAM wParam, LPARAM lParam) {
    auto* state = reinterpret_cast<ScrollbarState*>(GetWindowLongPtrW(window, GWLP_USERDATA));
    if (message == WM_NCCREATE) {
        const auto* create = reinterpret_cast<CREATESTRUCTW*>(lParam);
        state = new ScrollbarState();
        state->edit = static_cast<HWND>(create->lpCreateParams);
        SetWindowLongPtrW(window, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(state));
    }
    if (!state)
        return DefWindowProcW(window, message, wParam, lParam);

    switch (message) {
    case WM_ERASEBKGND:
        return 1;
    case WM_PAINT:
        paintScrollbar(window, *state);
        return 0;
    case kSetDarkMode:
        state->darkMode = wParam != 0;
        InvalidateRect(window, nullptr, TRUE);
        return 0;
    case WM_LBUTTONDOWN: {
        SetFocus(state->edit);
        RECT client{};
        GetClientRect(window, &client);
        const int y = GET_Y_LPARAM(lParam);
        const ScrollMetrics scroll = metrics(state->edit);
        const RECT thumb = thumbRect(scroll, client);
        POINT point{GET_X_LPARAM(lParam), y};
        if (y < kArrowHeight)
            scrollTo(window, *state, scroll.firstLine - 1);
        else if (y >= client.bottom - kArrowHeight)
            scrollTo(window, *state, scroll.firstLine + 1);
        else if (scroll.maxFirstLine > 0 && PtInRect(&thumb, point)) {
            state->dragging = true;
            state->dragOffset = y - thumb.top;
            SetCapture(window);
        } else if (y < thumb.top)
            scrollTo(window, *state, scroll.firstLine - scroll.visibleLines);
        else
            scrollTo(window, *state, scroll.firstLine + scroll.visibleLines);
        return 0;
    }
    case WM_MOUSEMOVE:
        if (state->dragging) {
            RECT client{};
            GetClientRect(window, &client);
            const ScrollMetrics scroll = metrics(state->edit);
            const RECT thumb = thumbRect(scroll, client);
            const int trackHeight = std::max(1, static_cast<int>(client.bottom) - 2 * kArrowHeight);
            const int travel = std::max(1, trackHeight - static_cast<int>(thumb.bottom - thumb.top));
            const int position =
                std::clamp(GET_Y_LPARAM(lParam) - state->dragOffset - kArrowHeight, 0, travel);
            scrollTo(window, *state, position * scroll.maxFirstLine / travel);
        }
        return 0;
    case WM_LBUTTONUP:
        if (state->dragging) {
            state->dragging = false;
            ReleaseCapture();
        }
        return 0;
    case WM_NCDESTROY:
        if (state->edit && IsWindow(state->edit))
            RemoveWindowSubclass(state->edit, editSubclassProc, kEditSubclassId);
        delete state;
        SetWindowLongPtrW(window, GWLP_USERDATA, 0);
        return DefWindowProcW(window, message, wParam, lParam);
    default:
        return DefWindowProcW(window, message, wParam, lParam);
    }
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

HWND create(HINSTANCE instance, HWND parent, HWND edit) {
    HWND scrollbar = CreateWindowExW(0, kClassName, L"", WS_CHILD | WS_VISIBLE, 0, 0, kScrollbarWidth, 100,
                                     parent, nullptr, instance, edit);
    if (scrollbar)
        SetWindowSubclass(edit, editSubclassProc, kEditSubclassId, reinterpret_cast<DWORD_PTR>(scrollbar));
    return scrollbar;
}

void setDarkMode(HWND scrollbar, bool darkMode) {
    if (scrollbar)
        SendMessageW(scrollbar, kSetDarkMode, darkMode ? TRUE : FALSE, 0);
}

void refresh(HWND scrollbar) {
    if (scrollbar)
        InvalidateRect(scrollbar, nullptr, FALSE);
}

} // namespace editscroll
