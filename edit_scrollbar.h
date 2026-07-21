#pragma once

// 为原生多行编辑框提供与数学过程视图一致的自绘垂直滚动条。

#include <windows.h>

namespace editscroll {

constexpr wchar_t kClassName[] = L"EquationSolverEditScrollbar";

bool registerClass(HINSTANCE instance);
HWND create(HINSTANCE instance, HWND parent, HWND edit);
void setDarkMode(HWND scrollbar, bool darkMode);
void refresh(HWND scrollbar);

} // namespace editscroll
