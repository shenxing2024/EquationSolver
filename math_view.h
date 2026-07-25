#pragma once

// 自绘数学文本控件：以二维形式排版分数、根式，并提供可选的定制滚动条。

#include <windows.h>

#include <utility>
#include <vector>

namespace mathview {

constexpr wchar_t kClassName[] = L"EquationSolverMathView";
constexpr UINT kSetDarkMode = WM_APP + 41;
constexpr UINT kSetMaxVisualLines = WM_APP + 42;
constexpr UINT kGetLastClickedLine = WM_APP + 43;
constexpr UINT kSetClickableLineGroups = WM_APP + 44;
// 创建窗口时通过附加样式控制滚动及最多两行的结果摘要。
constexpr DWORD kVerticalScroll = 0x00000001L;
constexpr DWORD kTwoLineSummary = 0x00000002L;
// 输入说明需要保留引号等原始输入符号，不将其解析为二维根式。
constexpr DWORD kPlainText = 0x00000004L;
// 日志控件用此样式把所点击的源文本行通知父窗口。
constexpr DWORD kClickableLines = 0x00000008L;
constexpr int kLineClickedNotification = 1;

bool registerClass(HINSTANCE instance);
void setDarkMode(HWND window, bool darkMode);
void setMaxVisualLines(HWND window, int maxVisualLines);
int lastClickedLine(HWND window);
void setClickableLineGroups(HWND window, const std::vector<std::pair<int, int>>& groups);
int preferredWidth(HWND window);
int requiredHeight(HWND window, int width, int maxVisualLines = 2);

} // namespace mathview
