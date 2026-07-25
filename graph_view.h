#pragma once

// Win32 图像视图：在同一坐标系绘制等号两侧函数或二元方程组的两条隐式曲线。

#include <string>
#include <utility>
#include <vector>
#include <windows.h>

namespace graphview {

constexpr wchar_t kClassName[] = L"EquationSolverGraphView";

struct GraphRequest {
    // input 使用解析器规范化后的表达式，因此未知数固定为 x（以及方程组中的 y）。
    std::wstring input;
    bool system2 = false;
    bool darkMode = false;
    bool degreeMode = false;
    std::wstring leftLegend;
    std::wstring rightLegend;
    std::wstring intersectionLabel;
    std::wstring xAxisLabel = L"x";
    std::wstring yAxisLabel = L"y";
    // 已知实根对应的交点，用于在曲线上绘制强调标记。
    std::vector<std::pair<double, double>> markers;
};

bool registerClass(HINSTANCE instance);
void setData(HWND window, const GraphRequest& request);
void setDarkMode(HWND window, bool darkMode);

} // namespace graphview
