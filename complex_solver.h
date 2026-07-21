#pragma once

// 含虚数单位 i 的独立解析与求解入口，避免改变原有实系数多项式的数据结构。

#include "solver.h"

#include <string>

namespace equation {

// 仅匹配作为常量出现的 i/I；pi 和 sin 中的字母 i 不会被误判。
bool hasImaginaryUnitToken(const std::wstring& text);

// normalizedText 使用内部 x/y/z 变量名；displayText 用于生成用户可读的推导过程。
SolveResult solveComplexText(const std::wstring& normalizedText, const std::wstring& displayText);

} // namespace equation
