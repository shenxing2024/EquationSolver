#pragma once

// 基础代数求解接口。此层只处理已经提取好的数值系数，不负责文本解析。

#include <array>
#include <string>
#include <utility>

namespace equation {

enum class Relation { Less, LessEqual, Greater, GreaterEqual };

struct SolveResult {
    std::wstring summary;
    std::wstring process;
    // 解析器在内部使用 x/y/z；这里保留用户字母及规范化后的原式供界面恢复显示。
    std::array<wchar_t, 3> variables{L'x', L'y', L'z'};
    int variableCount = 0;
    std::wstring normalizedInput;
    // 含虚数单位的关系式不能绘制为普通实平面函数图像。
    bool usesImaginaryUnit = false;
    // 含参数的关系式只输出等价表达式、定义域/分支或隐式解集，不展示过程。
    bool isRelation = false;

    SolveResult() = default;
    SolveResult(std::wstring summaryText, std::wstring processText)
        : summary(std::move(summaryText)), process(std::move(processText)) {}
};

// 输出整数、最简小分数或去除无意义末尾零的小数。
std::wstring formatNumber(double value);

SolveResult solveLinear(double a, double b, double c);
SolveResult solveLinear2(const std::array<std::array<double, 3>, 2>& equations);
SolveResult solveQuadratic(double a, double b, double c);
SolveResult solveLinear3(const std::array<std::array<double, 4>, 3>& equations);
SolveResult solveInequality(double a, double b, Relation relation, double c);

} // namespace equation
