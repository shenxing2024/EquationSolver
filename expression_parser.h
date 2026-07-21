#pragma once

// 文本解析层的公开接口：识别题型、规范化未知数，并生成精确答案与推导过程。

#include "solver.h"

#include <stdexcept>
#include <string>
#include <vector>

namespace equation {

enum class InputKind { Auto, Linear, Linear2, Quadratic, Linear3, Inequality, Fraction, Trigonometric };
enum class AngleUnit { Radians, Degrees };

// 面向界面的可恢复输入错误；消息统一使用 UTF-8，便于三语言转换。
class ParseError : public std::runtime_error {
  public:
    explicit ParseError(const std::string& message) : std::runtime_error(message) {}
};

// 解析并求解用户输入。Auto 会依据行数、关系符和表达式结构选择求解路径。
SolveResult solveText(const std::wstring& text, InputKind kind = InputKind::Auto,
                      AngleUnit angleUnit = AngleUnit::Radians);

// 将内部使用的 x/y/z 恢复为用户原先输入的未知数字母。
std::wstring applyVariableNames(const std::wstring& text, const SolveResult& result);

} // namespace equation
