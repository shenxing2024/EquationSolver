// 含参数的关系式：精确的表达式变形、分支及定义域；不能显式反解时保留隐式解集。
#pragma once

#include "expression_parser.h"

namespace equation {
SolveResult solveGeneralRelation(const std::wstring& canonicalInput, int variableCount, AngleUnit unit);
}
