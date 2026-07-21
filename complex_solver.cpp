// 复数代数解析器：支持 i、实数常量、四则运算、整数幂、隐式乘法和一至三个未知数。

#include "complex_solver.h"

#include "expression_parser.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <complex>
#include <cwchar>
#include <cwctype>
#include <map>
#include <sstream>
#include <vector>

namespace equation {
namespace {

constexpr double kEpsilon = 1e-10;
constexpr double kPi = 3.141592653589793238462643383279502884;
constexpr double kEuler = 2.718281828459045235360287471352662498;
using Complex = std::complex<double>;
using Powers = std::array<int, 3>;

bool isZero(const Complex& value) {
    return std::abs(value) < kEpsilon;
}

double factorialValue(double value) {
    if (!std::isfinite(value) || value < 0.0 || std::abs(value - std::round(value)) >= kEpsilon)
        throw ParseError("阶乘只能用于非负整数");
    if (value > 170.0)
        throw ParseError("阶乘参数不能超过 170");
    double result = 1.0;
    for (int factor = 2; factor <= static_cast<int>(std::round(value)); ++factor)
        result *= factor;
    return result;
}

Complex clean(Complex value) {
    if (std::abs(value.real()) < kEpsilon)
        value.real(0.0);
    if (std::abs(value.imag()) < kEpsilon)
        value.imag(0.0);
    return value;
}

std::wstring componentText(double value) {
    return formatNumber(std::abs(value) < kEpsilon ? 0.0 : value);
}

std::wstring complexText(Complex value, bool parenthesize = false) {
    value = clean(value);
    const double real = value.real();
    const double imaginary = value.imag();
    std::wstring result;
    if (std::abs(imaginary) < kEpsilon) {
        result = componentText(real);
    } else {
        const double magnitude = std::abs(imaginary);
        const std::wstring imaginaryText =
            std::abs(magnitude - 1.0) < kEpsilon ? L"i" : componentText(magnitude) + L"i";
        if (std::abs(real) < kEpsilon)
            result = imaginary < 0.0 ? L"-" + imaginaryText : imaginaryText;
        else
            result = componentText(real) + (imaginary < 0.0 ? L" - " : L" + ") + imaginaryText;
    }
    if (parenthesize && std::abs(real) >= kEpsilon && std::abs(imaginary) >= kEpsilon)
        return L"(" + result + L")";
    return result;
}

// 系数仍按幂次数组存储，因此同一实现可处理一元二次式和多元一次式。
struct ComplexPolynomial {
    std::map<Powers, Complex> terms;

    static ComplexPolynomial constant(Complex value) {
        ComplexPolynomial result;
        if (!isZero(value))
            result.terms[{0, 0, 0}] = value;
        return result;
    }

    static ComplexPolynomial variable(int index) {
        ComplexPolynomial result;
        Powers powers{0, 0, 0};
        powers[static_cast<size_t>(index)] = 1;
        result.terms[powers] = 1.0;
        return result;
    }

    void normalize() {
        for (auto iterator = terms.begin(); iterator != terms.end();) {
            iterator->second = clean(iterator->second);
            if (isZero(iterator->second))
                iterator = terms.erase(iterator);
            else
                ++iterator;
        }
    }

    bool isConstant() const {
        for (const auto& [powers, coefficient] : terms)
            if (!isZero(coefficient) && powers != Powers{0, 0, 0})
                return false;
        return true;
    }

    bool isZeroPolynomial() const {
        return terms.empty();
    }

    Complex constantValue() const {
        const auto found = terms.find({0, 0, 0});
        return found == terms.end() ? Complex{} : found->second;
    }

    Complex coefficient(int x, int y = 0, int z = 0) const {
        const auto found = terms.find({x, y, z});
        return found == terms.end() ? Complex{} : found->second;
    }

    int degree(int variable) const {
        int result = 0;
        for (const auto& [powers, coefficient] : terms)
            if (!isZero(coefficient))
                result = std::max(result, powers[static_cast<size_t>(variable)]);
        return result;
    }

    int totalDegree() const {
        int result = 0;
        for (const auto& [powers, coefficient] : terms)
            if (!isZero(coefficient))
                result = std::max(result, powers[0] + powers[1] + powers[2]);
        return result;
    }

    bool usesVariable(int variable) const {
        for (const auto& [powers, coefficient] : terms)
            if (!isZero(coefficient) && powers[static_cast<size_t>(variable)] != 0)
                return true;
        return false;
    }

    Complex evaluate(Complex x, Complex y = {}, Complex z = {}) const {
        Complex result{};
        for (const auto& [powers, coefficient] : terms) {
            result += coefficient * std::pow(x, powers[0]) * std::pow(y, powers[1]) * std::pow(z, powers[2]);
        }
        return result;
    }
};

ComplexPolynomial add(const ComplexPolynomial& left, const ComplexPolynomial& right,
                      Complex rightScale = 1.0) {
    ComplexPolynomial result = left;
    for (const auto& [powers, coefficient] : right.terms)
        result.terms[powers] += rightScale * coefficient;
    result.normalize();
    return result;
}

ComplexPolynomial multiply(const ComplexPolynomial& left, const ComplexPolynomial& right) {
    ComplexPolynomial result;
    for (const auto& [leftPowers, leftCoefficient] : left.terms) {
        for (const auto& [rightPowers, rightCoefficient] : right.terms) {
            Powers powers{leftPowers[0] + rightPowers[0], leftPowers[1] + rightPowers[1],
                          leftPowers[2] + rightPowers[2]};
            if (powers[0] + powers[1] + powers[2] > 8)
                throw ParseError("含 i 的方程化简后次数不能超过 8");
            result.terms[powers] += leftCoefficient * rightCoefficient;
        }
    }
    result.normalize();
    return result;
}

ComplexPolynomial power(ComplexPolynomial base, int exponent) {
    ComplexPolynomial result = ComplexPolynomial::constant(1.0);
    while (exponent > 0) {
        if (exponent & 1)
            result = multiply(result, base);
        exponent >>= 1;
        if (exponent)
            base = multiply(base, base);
    }
    return result;
}

struct ComplexRational {
    ComplexPolynomial numerator = ComplexPolynomial::constant(0.0);
    ComplexPolynomial denominator = ComplexPolynomial::constant(1.0);
};

ComplexRational add(const ComplexRational& left, const ComplexRational& right, Complex rightScale = 1.0) {
    return {add(multiply(left.numerator, right.denominator), multiply(right.numerator, left.denominator),
                rightScale),
            multiply(left.denominator, right.denominator)};
}

ComplexRational multiply(const ComplexRational& left, const ComplexRational& right) {
    return {multiply(left.numerator, right.numerator), multiply(left.denominator, right.denominator)};
}

ComplexRational divide(const ComplexRational& left, const ComplexRational& right) {
    if (right.numerator.isZeroPolynomial())
        throw ParseError("计算过程中出现除以 0");
    return {multiply(left.numerator, right.denominator), multiply(left.denominator, right.numerator)};
}

ComplexRational power(const ComplexRational& value, int exponent) {
    if (exponent == 0) {
        if (value.numerator.isZeroPolynomial())
            throw ParseError("0 的 0 次幂没有定义");
        return {ComplexPolynomial::constant(1.0), ComplexPolynomial::constant(1.0)};
    }
    if (exponent > 0)
        return {power(value.numerator, exponent), power(value.denominator, exponent)};
    if (value.numerator.isZeroPolynomial())
        throw ParseError("0 不能取负数指数幂");
    return {power(value.denominator, -exponent), power(value.numerator, -exponent)};
}

enum class TokenType {
    Number,
    Imaginary,
    Variable,
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    Factorial,
    LeftParen,
    RightParen,
    Equal,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    End
};

struct Token {
    TokenType type = TokenType::End;
    double number = 0.0;
    int variable = -1;
};

std::wstring normalizeSymbols(std::wstring text) {
    for (wchar_t& ch : text) {
        if (ch == L'×' || ch == L'·')
            ch = L'*';
        else if (ch == L'÷')
            ch = L'/';
        else if (ch == L'−' || ch == L'–' || ch == L'—')
            ch = L'-';
        else if (ch == L'（' || ch == L'[' || ch == L'［' || ch == L'【' || ch == L'{' || ch == L'｛')
            ch = L'(';
        else if (ch == L'）' || ch == L']' || ch == L'］' || ch == L'】' || ch == L'}' || ch == L'｝')
            ch = L')';
        else if (ch == L'＝')
            ch = L'=';
        else if (ch == L'！')
            ch = L'!';
    }
    std::wstring result;
    result.reserve(text.size() + 4);
    for (wchar_t ch : text) {
        if (ch == L'²')
            result += L"^2";
        else
            result += ch;
    }
    return result;
}

std::vector<Token> tokenize(const std::wstring& original) {
    const std::wstring text = normalizeSymbols(original);
    std::vector<Token> tokens;
    for (size_t index = 0; index < text.size();) {
        const wchar_t ch = text[index];
        if (std::iswspace(ch)) {
            ++index;
            continue;
        }
        Token token;
        if (std::iswdigit(ch) || ch == L'.') {
            wchar_t* end = nullptr;
            token.number = std::wcstod(text.c_str() + index, &end);
            if (end == text.c_str() + index || !std::isfinite(token.number))
                throw ParseError("数字格式不正确");
            token.type = TokenType::Number;
            index = static_cast<size_t>(end - text.c_str());
            tokens.push_back(token);
            continue;
        }
        if (ch == L'π' || ((ch == L'p' || ch == L'P') && index + 1 < text.size() &&
                           (text[index + 1] == L'i' || text[index + 1] == L'I'))) {
            token.type = TokenType::Number;
            token.number = kPi;
            index += ch == L'π' ? 1 : 2;
            tokens.push_back(token);
            continue;
        }
        if (ch == L'e' || ch == L'E') {
            token.type = TokenType::Number;
            token.number = kEuler;
            ++index;
            tokens.push_back(token);
            continue;
        }
        if (ch == L'i' || ch == L'I') {
            token.type = TokenType::Imaginary;
            ++index;
            tokens.push_back(token);
            continue;
        }
        if (ch == L'x' || ch == L'X' || ch == L'y' || ch == L'Y' || ch == L'z' || ch == L'Z') {
            token.type = TokenType::Variable;
            token.variable = (ch == L'x' || ch == L'X') ? 0 : ((ch == L'y' || ch == L'Y') ? 1 : 2);
            ++index;
            tokens.push_back(token);
            continue;
        }
        ++index;
        switch (ch) {
        case L'+':
            token.type = TokenType::Plus;
            break;
        case L'-':
            token.type = TokenType::Minus;
            break;
        case L'*':
            token.type = TokenType::Star;
            break;
        case L'/':
            token.type = TokenType::Slash;
            break;
        case L'^':
            token.type = TokenType::Caret;
            break;
        case L'!':
            token.type = TokenType::Factorial;
            break;
        case L'(':
            token.type = TokenType::LeftParen;
            break;
        case L')':
            token.type = TokenType::RightParen;
            break;
        case L'=':
            token.type = TokenType::Equal;
            break;
        case L'<':
            token.type = TokenType::Less;
            if (index < text.size() && text[index] == L'=') {
                token.type = TokenType::LessEqual;
                ++index;
            }
            break;
        case L'>':
            token.type = TokenType::Greater;
            if (index < text.size() && text[index] == L'=') {
                token.type = TokenType::GreaterEqual;
                ++index;
            }
            break;
        case L'≤':
            token.type = TokenType::LessEqual;
            break;
        case L'≥':
            token.type = TokenType::GreaterEqual;
            break;
        default:
            throw ParseError("含 i 的方程中包含无法识别的字符");
        }
        tokens.push_back(token);
    }
    tokens.push_back({TokenType::End});
    return tokens;
}

struct ComplexRelation {
    ComplexRational left;
    ComplexRational right;
    TokenType relation = TokenType::Equal;
};

class ComplexParser {
  public:
    explicit ComplexParser(const std::wstring& text) : tokens_(tokenize(text)) {}

    ComplexRelation parseRelation() {
        ComplexRelation result;
        result.left = parseExpression();
        result.relation = current().type;
        if (result.relation != TokenType::Equal && result.relation != TokenType::Less &&
            result.relation != TokenType::LessEqual && result.relation != TokenType::Greater &&
            result.relation != TokenType::GreaterEqual)
            throw ParseError("原式中需要有等号或不等号");
        ++index_;
        result.right = parseExpression();
        if (current().type != TokenType::End)
            throw ParseError("关系符右侧还有无法解析的内容");
        return result;
    }

  private:
    const Token& current() const {
        return tokens_[index_];
    }

    static bool startsPrimary(TokenType type) {
        return type == TokenType::Number || type == TokenType::Imaginary || type == TokenType::Variable ||
               type == TokenType::LeftParen;
    }

    ComplexRational parseExpression() {
        ComplexRational result = parseTerm();
        while (current().type == TokenType::Plus || current().type == TokenType::Minus) {
            const bool subtract = current().type == TokenType::Minus;
            ++index_;
            result = add(result, parseTerm(), subtract ? -1.0 : 1.0);
        }
        return result;
    }

    ComplexRational parseTerm() {
        ComplexRational result = parseUnary();
        while (current().type == TokenType::Star || current().type == TokenType::Slash ||
               startsPrimary(current().type)) {
            const TokenType operation = current().type;
            if (operation == TokenType::Star || operation == TokenType::Slash)
                ++index_;
            const ComplexRational right = parseUnary();
            result = operation == TokenType::Slash ? divide(result, right) : multiply(result, right);
        }
        return result;
    }

    ComplexRational parseUnary() {
        if (current().type == TokenType::Plus) {
            ++index_;
            return parseUnary();
        }
        if (current().type == TokenType::Minus) {
            ++index_;
            return multiply({ComplexPolynomial::constant(-1.0), ComplexPolynomial::constant(1.0)},
                            parseUnary());
        }
        return parsePower();
    }

    ComplexRational parsePower() {
        ComplexRational result = parsePrimary();
        while (current().type == TokenType::Factorial) {
            if (!result.numerator.isConstant() || !result.denominator.isConstant())
                throw ParseError("含未知数的表达式不能直接取阶乘");
            const Complex denominator = result.denominator.constantValue();
            if (isZero(denominator))
                throw ParseError("计算过程中出现除以 0");
            const Complex value = result.numerator.constantValue() / denominator;
            if (std::abs(value.imag()) >= kEpsilon)
                throw ParseError("阶乘只能用于非负整数");
            result = {ComplexPolynomial::constant(factorialValue(value.real())),
                      ComplexPolynomial::constant(1.0)};
            ++index_;
        }
        if (current().type != TokenType::Caret)
            return result;
        ++index_;
        bool grouped = false;
        if (current().type == TokenType::LeftParen) {
            grouped = true;
            ++index_;
        }
        int sign = 1;
        if (current().type == TokenType::Plus || current().type == TokenType::Minus) {
            if (current().type == TokenType::Minus)
                sign = -1;
            ++index_;
        }
        if (current().type != TokenType::Number ||
            std::abs(current().number - std::round(current().number)) >= kEpsilon) {
            throw ParseError("含 i 的方程目前支持整数指数幂");
        }
        const int exponent = sign * static_cast<int>(std::round(current().number));
        if (std::abs(exponent) > 8)
            throw ParseError("含 i 的方程指数绝对值不能超过 8");
        ++index_;
        if (grouped) {
            if (current().type != TokenType::RightParen)
                throw ParseError("指数缺少右括号");
            ++index_;
        }
        return power(result, exponent);
    }

    ComplexRational parsePrimary() {
        if (current().type == TokenType::Number) {
            const double value = current().number;
            ++index_;
            return {ComplexPolynomial::constant(value), ComplexPolynomial::constant(1.0)};
        }
        if (current().type == TokenType::Imaginary) {
            ++index_;
            return {ComplexPolynomial::constant(Complex(0.0, 1.0)), ComplexPolynomial::constant(1.0)};
        }
        if (current().type == TokenType::Variable) {
            const int variable = current().variable;
            ++index_;
            return {ComplexPolynomial::variable(variable), ComplexPolynomial::constant(1.0)};
        }
        if (current().type == TokenType::LeftParen) {
            ++index_;
            ComplexRational result = parseExpression();
            if (current().type != TokenType::RightParen)
                throw ParseError("缺少右括号 )");
            ++index_;
            return result;
        }
        throw ParseError("此处应为数字、i、未知数或左括号");
    }

    std::vector<Token> tokens_;
    size_t index_ = 0;
};

std::vector<std::wstring> splitLines(const std::wstring& text) {
    std::vector<std::wstring> lines;
    std::wstring current;
    for (const wchar_t ch : text) {
        if (ch == L'\r')
            continue;
        if (ch == L'\n' || ch == L';' || ch == L'；') {
            if (!current.empty())
                lines.push_back(current);
            current.clear();
        } else {
            current += ch;
        }
    }
    if (!current.empty())
        lines.push_back(current);
    return lines;
}

std::wstring polynomialText(const ComplexPolynomial& polynomial) {
    if (polynomial.isZeroPolynomial())
        return L"0";
    std::vector<std::pair<Powers, Complex>> terms(polynomial.terms.begin(), polynomial.terms.end());
    std::sort(terms.begin(), terms.end(), [](const auto& left, const auto& right) {
        const int leftDegree = left.first[0] + left.first[1] + left.first[2];
        const int rightDegree = right.first[0] + right.first[1] + right.first[2];
        return leftDegree != rightDegree ? leftDegree > rightDegree : left.first > right.first;
    });
    std::wstring result;
    bool first = true;
    constexpr std::array<const wchar_t*, 3> names{L"x", L"y", L"z"};
    for (const auto& [powers, rawCoefficient] : terms) {
        const Complex coefficient = clean(rawCoefficient);
        if (isZero(coefficient))
            continue;
        const bool hasVariable = powers != Powers{0, 0, 0};
        const bool negativeReal = std::abs(coefficient.imag()) < kEpsilon && coefficient.real() < 0.0;
        Complex magnitude = negativeReal ? -coefficient : coefficient;
        if (!first)
            result += negativeReal ? L" - " : L" + ";
        else if (negativeReal)
            result += L"-";
        const bool unitCoefficient = std::abs(magnitude - Complex(1.0, 0.0)) < kEpsilon;
        if (!hasVariable || !unitCoefficient)
            result += complexText(magnitude, hasVariable);
        for (int variable = 0; variable < 3; ++variable) {
            if (powers[static_cast<size_t>(variable)] == 0)
                continue;
            result += names[static_cast<size_t>(variable)];
            if (powers[static_cast<size_t>(variable)] > 1)
                result += L"^" + std::to_wstring(powers[static_cast<size_t>(variable)]);
        }
        first = false;
    }
    return result.empty() ? L"0" : result;
}

ComplexRational differenceOf(const ComplexRelation& relation) {
    if (relation.relation != TokenType::Equal)
        throw ParseError("复数没有大小关系，含 i 的原式不能使用不等号");
    return add(relation.left, relation.right, -1.0);
}

SolveResult solveOne(const std::wstring& line, const std::wstring& displayText) {
    const ComplexRational difference = differenceOf(ComplexParser(line).parseRelation());
    const ComplexPolynomial& polynomial = difference.numerator;
    const ComplexPolynomial& denominator = difference.denominator;
    if (polynomial.usesVariable(1) || polynomial.usesVariable(2) || denominator.usesVariable(1) ||
        denominator.usesVariable(2)) {
        throw ParseError("含 i 的单个方程只能包含一个未知数");
    }
    const int degree = polynomial.degree(0);
    if (degree > 2 || polynomial.totalDegree() > 2)
        throw ParseError("含 i 的一元方程目前支持一次或二次方程");

    std::wostringstream process;
    process << L"输入原式：" << displayText << L"\r\n\r\n"
            << L"把 i 作为虚数单位（i² = -1），移项、通分并合并同类项：\r\n"
            << L"    " << polynomialText(polynomial) << L" = 0\r\n\r\n";

    std::vector<Complex> candidates;
    if (degree == 0) {
        if (polynomial.isZeroPolynomial()) {
            process << L"等式恒成立。";
            return {L"结论：任意复数均为解", process.str()};
        }
        process << L"等式恒不成立。";
        return {L"结论：方程无解", process.str()};
    }
    const Complex a = polynomial.coefficient(2);
    const Complex b = polynomial.coefficient(1);
    const Complex c = polynomial.coefficient(0);
    if (degree == 1) {
        candidates.push_back(clean(-c / b));
        process << L"这是复数范围内的一元一次方程，两边除以 x 的系数：\r\n"
                << L"    x = " << complexText(candidates.front()) << L"。\r\n";
    } else {
        const Complex delta = clean(b * b - 4.0 * a * c);
        const Complex rootDelta = clean(std::sqrt(delta));
        candidates.push_back(clean((-b + rootDelta) / (2.0 * a)));
        candidates.push_back(clean((-b - rootDelta) / (2.0 * a)));
        process << L"在复数范围内计算判别式并使用求根公式：\r\n"
                << L"    Δ = b² - 4ac = " << complexText(delta) << L"\r\n"
                << L"    x = (-b ± √Δ)/(2a)\r\n";
    }

    std::vector<Complex> valid;
    for (const Complex candidate : candidates) {
        if (isZero(denominator.evaluate(candidate))) {
            process << L"验根 x = " << complexText(candidate) << L"：使原式分母为 0，舍去。\r\n";
            continue;
        }
        bool duplicate = false;
        for (const Complex existing : valid)
            duplicate = duplicate || std::abs(existing - candidate) < 1e-8;
        if (!duplicate)
            valid.push_back(candidate);
    }
    if (valid.empty())
        return {L"结论：方程无解", process.str()};
    if (valid.size() == 1)
        return {L"唯一解：x = " + complexText(valid.front()), process.str()};
    return {L"两个复数根：x₁ = " + complexText(valid[0]) + L"，x₂ = " + complexText(valid[1]), process.str()};
}

SolveResult solveSystem(const std::vector<std::wstring>& lines, const std::wstring& displayText) {
    const int count = static_cast<int>(lines.size());
    if (count != 2 && count != 3)
        throw ParseError("含 i 的线性方程组需要输入两行或三行方程");
    std::array<std::array<Complex, 4>, 3> matrix{};
    for (int row = 0; row < count; ++row) {
        const ComplexRational difference =
            differenceOf(ComplexParser(lines[static_cast<size_t>(row)]).parseRelation());
        if (!difference.denominator.isConstant() || isZero(difference.denominator.constantValue()))
            throw ParseError("含 i 的方程组暂不支持未知数位于分母");
        if (difference.numerator.totalDegree() > 1)
            throw ParseError("含 i 的方程组中的每个方程都必须是一次方程");
        const Complex divisor = difference.denominator.constantValue();
        for (int column = 0; column < count; ++column)
            matrix[row][column] =
                difference.numerator.coefficient(column == 0, column == 1, column == 2) / divisor;
        matrix[row][count] = -difference.numerator.constantValue() / divisor;
    }

    int pivotRow = 0;
    std::array<int, 3> pivotColumns{-1, -1, -1};
    for (int column = 0; column < count && pivotRow < count; ++column) {
        int best = pivotRow;
        for (int row = pivotRow + 1; row < count; ++row)
            if (std::abs(matrix[row][column]) > std::abs(matrix[best][column]))
                best = row;
        if (isZero(matrix[best][column]))
            continue;
        if (best != pivotRow)
            std::swap(matrix[best], matrix[pivotRow]);
        const Complex pivot = matrix[pivotRow][column];
        for (int item = column; item <= count; ++item)
            matrix[pivotRow][item] /= pivot;
        for (int row = 0; row < count; ++row) {
            if (row == pivotRow || isZero(matrix[row][column]))
                continue;
            const Complex factor = matrix[row][column];
            for (int item = column; item <= count; ++item)
                matrix[row][item] -= factor * matrix[pivotRow][item];
        }
        pivotColumns[pivotRow] = column;
        ++pivotRow;
    }
    for (int row = 0; row < count; ++row) {
        bool allZero = true;
        for (int column = 0; column < count; ++column)
            allZero = allZero && isZero(matrix[row][column]);
        if (allZero && !isZero(matrix[row][count]))
            return {L"结论：方程组无解", L"输入原方程组：\r\n" + displayText +
                                             L"\r\n\r\n把 i 作为虚数单位（i² = -1），高斯消元后出现矛盾行。"};
    }
    if (pivotRow < count)
        return {L"结论：方程组有无穷多个复数解",
                L"输入原方程组：\r\n" + displayText +
                    L"\r\n\r\n把 i 作为虚数单位（i² = -1），高斯消元后存在自由变量。"};

    std::array<Complex, 3> solution{};
    for (int row = 0; row < count; ++row)
        solution[static_cast<size_t>(pivotColumns[row])] = clean(matrix[row][count]);
    std::wstring summary = L"唯一解：";
    constexpr std::array<const wchar_t*, 3> names{L"x", L"y", L"z"};
    for (int variable = 0; variable < count; ++variable) {
        if (variable)
            summary += L"，";
        summary += names[static_cast<size_t>(variable)] + std::wstring(L" = ") +
                   complexText(solution[static_cast<size_t>(variable)]);
    }
    std::wostringstream process;
    process << L"输入原方程组：\r\n"
            << displayText << L"\r\n\r\n"
            << L"把 i 作为虚数单位（i² = -1），对复系数增广矩阵进行高斯消元。\r\n"
            << L"消元完成后直接读出：\r\n    " << summary.substr(4) << L"。";
    return {summary, process.str()};
}

} // namespace

bool hasImaginaryUnitToken(const std::wstring& text) {
    for (size_t index = 0; index < text.size();) {
        const wchar_t lower = static_cast<wchar_t>(std::towlower(text[index]));
        if (lower == L'p' && index + 1 < text.size() && std::towlower(text[index + 1]) == L'i') {
            index += 2;
            continue;
        }
        if (lower == L's' && index + 2 < text.size() && std::towlower(text[index + 1]) == L'i' &&
            std::towlower(text[index + 2]) == L'n') {
            index += 3;
            continue;
        }
        if (lower == L'i')
            return true;
        ++index;
    }
    return false;
}

SolveResult solveComplexText(const std::wstring& normalizedText, const std::wstring& displayText) {
    const std::vector<std::wstring> lines = splitLines(normalizedText);
    if (lines.empty())
        throw ParseError("请输入需要求解的原方程");
    SolveResult result =
        lines.size() == 1 ? solveOne(lines.front(), displayText) : solveSystem(lines, displayText);
    result.usesImaginaryUnit = true;
    return result;
}

} // namespace equation
