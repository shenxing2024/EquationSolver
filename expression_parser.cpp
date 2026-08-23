// 将用户文本转换为 Token、递归下降表达式和多项式/有理式，随后分派到
// 精确代数、绝对值、分数指数或三角方程求解路径。

#include "expression_parser.h"

#include "complex_solver.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <complex>
#include <cstdint>
#include <cstdlib>
#include <cwchar>
#include <cwctype>
#include <iomanip>
#include <limits>
#include <map>
#include <numeric>
#include <optional>
#include <sstream>
#include <vector>

namespace equation {
namespace {

constexpr double kEpsilon = 1e-10;
constexpr double kPi = 3.141592653589793238462643383279502884;
constexpr double kEuler = 2.718281828459045235360287471352662498;
using Powers = std::array<int, 3>;

std::wstring approximateNumber(double value) {
    if (std::abs(value) < 0.0005)
        value = 0.0;
    std::wostringstream out;
    out << std::fixed << std::setprecision(3) << value;
    return out.str();
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

// ---------- 精确数值与根式表示 ----------

struct ExactFraction {
    std::int64_t numerator = 0;
    std::int64_t denominator = 1;

    ExactFraction() = default;
    ExactFraction(std::int64_t n, std::int64_t d = 1) : numerator(n), denominator(d) {
        if (denominator == 0)
            throw ParseError("分母不能为 0");
        if (denominator < 0) {
            numerator = -numerator;
            denominator = -denominator;
        }
        const std::int64_t divisor = std::gcd(numerator < 0 ? -numerator : numerator, denominator);
        if (divisor > 1) {
            numerator /= divisor;
            denominator /= divisor;
        }
    }

    bool isZero() const {
        return numerator == 0;
    }
    double value() const {
        return static_cast<double>(numerator) / static_cast<double>(denominator);
    }
    std::wstring text() const {
        if (denominator == 1)
            return std::to_wstring(numerator);
        return std::to_wstring(numerator) + L"/" + std::to_wstring(denominator);
    }
};

ExactFraction operator-(const ExactFraction& left, const ExactFraction& right) {
    return {left.numerator * right.denominator - right.numerator * left.denominator,
            left.denominator * right.denominator};
}

ExactFraction operator+(const ExactFraction& left, const ExactFraction& right) {
    return {left.numerator * right.denominator + right.numerator * left.denominator,
            left.denominator * right.denominator};
}

ExactFraction operator*(const ExactFraction& left, const ExactFraction& right) {
    return {left.numerator * right.numerator, left.denominator * right.denominator};
}

ExactFraction operator/(const ExactFraction& left, const ExactFraction& right) {
    if (right.numerator == 0)
        throw ParseError("计算过程中出现除以 0");
    return {left.numerator * right.denominator, left.denominator * right.numerator};
}

std::optional<ExactFraction> rationalize(double value) {
    if (!std::isfinite(value))
        return std::nullopt;
    const bool negative = value < 0;
    const double target = std::abs(value);
    constexpr std::int64_t maxDenominator = 1000000;
    constexpr double tolerance = 1e-14;

    std::int64_t oldNumerator = 0, numerator = 1;
    std::int64_t oldDenominator = 1, denominator = 0;
    double remainder = target;
    for (int iteration = 0; iteration < 32; ++iteration) {
        const long double wholeWide = std::floor(static_cast<long double>(remainder));
        if (wholeWide > static_cast<long double>(std::numeric_limits<std::int64_t>::max()))
            break;
        const auto whole = static_cast<std::int64_t>(wholeWide);
        if (numerator != 0 && whole > (std::numeric_limits<std::int64_t>::max() - oldNumerator) / numerator)
            break;
        if (denominator != 0 &&
            whole > (std::numeric_limits<std::int64_t>::max() - oldDenominator) / denominator)
            break;
        const std::int64_t nextNumerator = whole * numerator + oldNumerator;
        const std::int64_t nextDenominator = whole * denominator + oldDenominator;
        if (nextDenominator <= 0 || nextDenominator > maxDenominator)
            break;
        oldNumerator = numerator;
        numerator = nextNumerator;
        oldDenominator = denominator;
        denominator = nextDenominator;
        const double approximation = static_cast<double>(numerator) / denominator;
        if (std::abs(approximation - target) <= tolerance * std::max(1.0, target)) {
            return ExactFraction(negative ? -numerator : numerator, denominator);
        }
        const double fractional = remainder - whole;
        if (fractional < 1e-15)
            break;
        remainder = 1.0 / fractional;
    }
    return std::nullopt;
}

std::int64_t safeLcm(std::int64_t left, std::int64_t right) {
    const std::int64_t divisor = std::gcd(left, right);
    if (left > std::numeric_limits<std::int64_t>::max() / (right / divisor)) {
        throw ParseError("系数过大，无法保持精确分数");
    }
    return left * (right / divisor);
}

struct IntegerEquation {
    std::array<std::int64_t, 4> values{0, 0, 0, 0};
};

std::optional<IntegerEquation> toIntegerEquation(const std::array<double, 4>& values) {
    std::array<ExactFraction, 4> fractions;
    std::int64_t commonDenominator = 1;
    for (size_t i = 0; i < values.size(); ++i) {
        const auto fraction = rationalize(values[i]);
        if (!fraction)
            return std::nullopt;
        fractions[i] = *fraction;
        commonDenominator = safeLcm(commonDenominator, fraction->denominator);
    }
    IntegerEquation result;
    for (size_t i = 0; i < values.size(); ++i) {
        result.values[i] = fractions[i].numerator * (commonDenominator / fractions[i].denominator);
    }
    std::int64_t divisor = 0;
    for (const auto value : result.values)
        divisor = std::gcd(divisor, value < 0 ? -value : value);
    if (divisor > 1)
        for (auto& value : result.values)
            value /= divisor;
    return result;
}

std::int64_t integerSquareRoot(std::int64_t value) {
    if (value <= 0)
        return 0;
    auto root = static_cast<std::int64_t>(std::sqrt(static_cast<long double>(value)));
    while ((root + 1) <= value / (root + 1))
        ++root;
    while (root > value / root)
        --root;
    return root;
}

std::pair<std::int64_t, std::int64_t> simplifySquareRoot(std::int64_t value) {
    std::int64_t outside = 1;
    std::int64_t inside = value;
    auto extractSquare = [&](std::int64_t factor) {
        const std::int64_t square = factor * factor;
        while (inside % square == 0) {
            outside *= factor;
            inside /= square;
        }
    };
    extractSquare(2);
    constexpr std::int64_t maxTrialFactor = 1000000;
    for (std::int64_t factor = 3; factor <= maxTrialFactor && factor <= inside / factor; factor += 2) {
        extractSquare(factor);
    }
    const std::int64_t remainingRoot = integerSquareRoot(inside);
    if (remainingRoot > 1 && remainingRoot * remainingRoot == inside) {
        outside *= remainingRoot;
        inside = 1;
    }
    return {outside, inside};
}

std::wstring radicalTerm(std::int64_t coefficient, std::int64_t radicand, bool imaginary) {
    std::wstring result;
    if (coefficient != 1)
        result += std::to_wstring(coefficient);
    if (radicand != 1)
        result += L"√" + std::to_wstring(radicand);
    if (imaginary)
        result += L"i";
    return result.empty() ? L"1" : result;
}

bool checkedIntegerPower(std::int64_t base, int degree, std::int64_t limit, std::int64_t& result) {
    result = 1;
    for (int i = 0; i < degree; ++i) {
        if (base != 0 && result > limit / base)
            return false;
        result *= base;
    }
    return true;
}

std::wstring simplifiedNthRoot(const ExactFraction& value, int degree) {
    std::int64_t numerator = value.numerator;
    std::int64_t denominator = value.denominator;
    std::int64_t outsideNumerator = 1;
    std::int64_t outsideDenominator = 1;
    constexpr std::int64_t maxTrialFactor = 100000;
    for (std::int64_t factor = 2; factor <= maxTrialFactor; ++factor) {
        std::int64_t factorPower = 0;
        if (!checkedIntegerPower(factor, degree, std::max(numerator, denominator), factorPower))
            break;
        while (numerator % factorPower == 0) {
            numerator /= factorPower;
            outsideNumerator *= factor;
        }
        while (denominator % factorPower == 0) {
            denominator /= factorPower;
            outsideDenominator *= factor;
        }
        if (factorPower > numerator && factorPower > denominator)
            break;
    }
    const ExactFraction outside(outsideNumerator, outsideDenominator);
    const ExactFraction inside(numerator, denominator);
    if (inside.numerator == inside.denominator)
        return outside.text();
    const std::wstring insideText = inside.text();
    std::wstring radical;
    if (degree == 3)
        radical = inside.denominator == 1 ? L"∛" + insideText : L"∛{" + insideText + L"}";
    else
        radical = L"√[" + std::to_wstring(degree) + L"]{" + insideText + L"}";
    std::wstring result;
    if (outside.numerator != 1)
        result += std::to_wstring(outside.numerator);
    result += radical;
    if (outside.denominator != 1)
        result += L"/" + std::to_wstring(outside.denominator);
    return result;
}

std::wstring symbolicNumber(double value) {
    if (const auto fraction = rationalize(value))
        return fraction->text();
    const bool negative = value < 0;
    const double magnitude = std::abs(value);
    for (int degree = 2; degree <= 9; ++degree) {
        const double powered = std::pow(magnitude, degree);
        const auto exactPowered = rationalize(powered);
        if (!exactPowered || exactPowered->numerator <= 0)
            continue;
        if (exactPowered->denominator > 10000)
            continue;
        if (exactPowered->numerator > 1000000000 || exactPowered->denominator > 1000000000)
            continue;
        std::wstring text;
        if (degree == 2 && exactPowered->denominator != 1) {
            const std::int64_t rationalizedRadicand = exactPowered->numerator * exactPowered->denominator;
            if (rationalizedRadicand <= 0 || rationalizedRadicand > 1000000000)
                continue;
            const auto [outside, inside] = simplifySquareRoot(rationalizedRadicand);
            const ExactFraction coefficient(outside, exactPowered->denominator);
            if (inside == 1) {
                text = coefficient.text();
            } else {
                text = radicalTerm(coefficient.numerator, inside, false);
                if (coefficient.denominator != 1)
                    text += L"/" + std::to_wstring(coefficient.denominator);
            }
        } else if (degree == 2) {
            const auto [outside, inside] = simplifySquareRoot(exactPowered->numerator);
            text = radicalTerm(outside, inside, false);
        } else {
            text = simplifiedNthRoot(*exactPowered, degree);
        }
        return negative ? L"-" + text : text;
    }
    return formatNumber(value);
}

struct ExactRootExpression {
    ExactFraction rational;
    ExactFraction radicalCoefficient;
    std::int64_t radicand = 1;

    double value() const {
        return rational.value() + radicalCoefficient.value() * std::sqrt(static_cast<double>(radicand));
    }

    std::wstring text() const {
        if (radicalCoefficient.isZero())
            return rational.text();
        if (radicand == 1)
            return (rational + radicalCoefficient).text();

        std::int64_t denominator = safeLcm(rational.denominator, radicalCoefficient.denominator);
        std::int64_t rationalNumerator =
            rational.numerator * (denominator / rational.denominator);
        std::int64_t radicalNumerator =
            radicalCoefficient.numerator * (denominator / radicalCoefficient.denominator);
        std::int64_t divisor = std::gcd(std::abs(rationalNumerator), std::abs(radicalNumerator));
        divisor = std::gcd(divisor, denominator);
        if (divisor > 1) {
            rationalNumerator /= divisor;
            radicalNumerator /= divisor;
            denominator /= divisor;
        }

        const std::int64_t radicalMagnitude = std::abs(radicalNumerator);
        std::wstring radicalText;
        if (radicalMagnitude != 1)
            radicalText += std::to_wstring(radicalMagnitude);
        radicalText += L"√" + std::to_wstring(radicand);

        std::wstring numeratorText;
        if (rationalNumerator == 0) {
            numeratorText = radicalNumerator < 0 ? L"-" + radicalText : radicalText;
        } else {
            numeratorText = std::to_wstring(rationalNumerator) +
                            (radicalNumerator < 0 ? L" - " : L" + ") + radicalText;
        }
        if (denominator == 1)
            return numeratorText;
        if (rationalNumerator == 0)
            return numeratorText + L"/" + std::to_wstring(denominator);
        return L"(" + numeratorText + L")/" + std::to_wstring(denominator);
    }
};

struct ExactRootResult {
    bool available = false;
    bool hasRealRoots = true;
    std::vector<std::wstring> texts;
    std::vector<double> values;
    std::vector<ExactRootExpression> expressions;
    std::wstring calculation;
};

ExactRootResult exactRoots(double coefficientA, double coefficientB, double coefficientC) {
    const auto integer = toIntegerEquation({coefficientA, coefficientB, coefficientC, 0.0});
    if (!integer)
        return {};
    std::int64_t a = integer->values[0];
    std::int64_t b = integer->values[1];
    std::int64_t c = integer->values[2];
    ExactRootResult result;
    result.available = true;
    if (a == 0) {
        if (b == 0)
            return result;
        const ExactFraction root(-c, b);
        result.texts.push_back(root.text());
        result.values.push_back(root.value());
        result.expressions.push_back({root, ExactFraction(0), 1});
        result.calculation = L"x = -c/b = " + root.text();
        return result;
    }

    const long double deltaWide = static_cast<long double>(b) * b - static_cast<long double>(4) * a * c;
    if (deltaWide > static_cast<long double>(std::numeric_limits<std::int64_t>::max()) ||
        deltaWide < static_cast<long double>(std::numeric_limits<std::int64_t>::min()))
        return {};
    const std::int64_t delta = static_cast<std::int64_t>(std::llround(deltaWide));
    result.calculation = L"Δ = b² - 4ac = " + std::to_wstring(delta) + L"\r\n";
    const bool imaginary = delta < 0;
    const std::int64_t absoluteDelta = delta < 0 ? -delta : delta;
    const std::int64_t squareRoot = integerSquareRoot(absoluteDelta);
    const std::int64_t denominator = 2 * a;
    if (!imaginary && squareRoot * squareRoot == absoluteDelta) {
        const ExactFraction first(-b + squareRoot, denominator);
        const ExactFraction second(-b - squareRoot, denominator);
        result.texts.push_back(first.text());
        result.values.push_back(first.value());
        result.expressions.push_back({first, ExactFraction(0), 1});
        if (first.numerator != second.numerator || first.denominator != second.denominator) {
            result.texts.push_back(second.text());
            result.values.push_back(second.value());
            result.expressions.push_back({second, ExactFraction(0), 1});
        }
        result.calculation += L"√Δ = " + std::to_wstring(squareRoot) + L"，代入求根公式并约分。";
        return result;
    }

    const auto [outsideInitial, inside] = simplifySquareRoot(absoluteDelta);
    std::int64_t outside = outsideInitial;
    std::int64_t constant = -b;
    std::int64_t divisor = std::gcd(constant < 0 ? -constant : constant, outside);
    divisor = std::gcd(divisor, denominator < 0 ? -denominator : denominator);
    std::int64_t finalDenominator = denominator / (divisor == 0 ? 1 : divisor);
    constant /= (divisor == 0 ? 1 : divisor);
    outside /= (divisor == 0 ? 1 : divisor);
    if (finalDenominator < 0) {
        finalDenominator = -finalDenominator;
        constant = -constant;
    }
    const std::wstring radical = radicalTerm(outside, inside, imaginary);
    auto makeText = [&](bool plus) {
        std::wstring numeratorText;
        if (constant == 0) {
            numeratorText = plus ? radical : L"-" + radical;
        } else {
            numeratorText = std::to_wstring(constant) + (plus ? L" + " : L" - ") + radical;
        }
        if (finalDenominator == 1)
            return numeratorText;
        if (constant == 0 && numeratorText.find(L' ') == std::wstring::npos) {
            return numeratorText + L"/" + std::to_wstring(finalDenominator);
        }
        return L"(" + numeratorText + L")/" + std::to_wstring(finalDenominator);
    };
    result.texts = {makeText(true), makeText(false)};
    if (imaginary) {
        result.hasRealRoots = false;
    } else {
        const double root = std::sqrt(static_cast<double>(delta));
        result.values = {(-b + root) / denominator, (-b - root) / denominator};
        result.expressions = {
            {ExactFraction(constant, finalDenominator),
             ExactFraction(outside, finalDenominator), inside},
            {ExactFraction(constant, finalDenominator),
             ExactFraction(-outside, finalDenominator), inside}};
    }
    result.calculation += L"化简根式后，代入 x = (-b ± √Δ)/(2a)。";
    return result;
}

struct Polynomial {
    std::map<Powers, double> terms;

    static Polynomial constant(double value) {
        Polynomial result;
        if (std::abs(value) >= kEpsilon)
            result.terms[{0, 0, 0}] = value;
        return result;
    }

    static Polynomial variable(int index) {
        Polynomial result;
        Powers powers{0, 0, 0};
        powers[index] = 1;
        result.terms[powers] = 1.0;
        return result;
    }

    void clean() {
        for (auto it = terms.begin(); it != terms.end();) {
            if (std::abs(it->second) < kEpsilon)
                it = terms.erase(it);
            else
                ++it;
        }
    }

    bool isZero() const {
        return terms.empty();
    }

    bool isConstant() const {
        return terms.empty() || (terms.size() == 1 && terms.begin()->first == Powers{0, 0, 0});
    }

    double constantValue() const {
        const auto found = terms.find({0, 0, 0});
        return found == terms.end() ? 0.0 : found->second;
    }

    bool usesVariable(int index) const {
        for (const auto& [powers, coefficient] : terms) {
            if (std::abs(coefficient) >= kEpsilon && powers[index] != 0)
                return true;
        }
        return false;
    }

    int degree(int index) const {
        int result = 0;
        for (const auto& [powers, coefficient] : terms) {
            if (std::abs(coefficient) >= kEpsilon)
                result = std::max(result, powers[index]);
        }
        return result;
    }

    int totalDegree() const {
        int result = 0;
        for (const auto& [powers, coefficient] : terms) {
            if (std::abs(coefficient) >= kEpsilon) {
                result = std::max(result, powers[0] + powers[1] + powers[2]);
            }
        }
        return result;
    }

    double coefficient(int xPower, int yPower = 0, int zPower = 0) const {
        const auto found = terms.find({xPower, yPower, zPower});
        return found == terms.end() ? 0.0 : found->second;
    }

    double evaluate(double x, double y = 0.0, double z = 0.0) const {
        double result = 0.0;
        for (const auto& [powers, coefficient] : terms) {
            result += coefficient * std::pow(x, powers[0]) * std::pow(y, powers[1]) * std::pow(z, powers[2]);
        }
        return result;
    }
};

Polynomial add(const Polynomial& left, const Polynomial& right, double rightScale = 1.0) {
    Polynomial result = left;
    for (const auto& [powers, coefficient] : right.terms)
        result.terms[powers] += rightScale * coefficient;
    result.clean();
    return result;
}

Polynomial multiply(const Polynomial& left, const Polynomial& right) {
    Polynomial result;
    for (const auto& [leftPowers, leftCoefficient] : left.terms) {
        for (const auto& [rightPowers, rightCoefficient] : right.terms) {
            Powers powers{leftPowers[0] + rightPowers[0], leftPowers[1] + rightPowers[1],
                          leftPowers[2] + rightPowers[2]};
            result.terms[powers] += leftCoefficient * rightCoefficient;
        }
    }
    result.clean();
    return result;
}

Polynomial power(Polynomial base, int exponent) {
    Polynomial result = Polynomial::constant(1.0);
    while (exponent > 0) {
        if (exponent & 1)
            result = multiply(result, base);
        exponent >>= 1;
        if (exponent)
            base = multiply(base, base);
    }
    return result;
}

// 有理式始终保存为“多项式分子 / 多项式分母”，便于统一处理分式和定义域。
struct RationalExpression {
    Polynomial numerator = Polynomial::constant(0.0);
    Polynomial denominator = Polynomial::constant(1.0);
};

RationalExpression add(const RationalExpression& left, const RationalExpression& right,
                       double rightScale = 1.0) {
    return {add(multiply(left.numerator, right.denominator), multiply(right.numerator, left.denominator),
                rightScale),
            multiply(left.denominator, right.denominator)};
}

RationalExpression multiply(const RationalExpression& left, const RationalExpression& right) {
    return {multiply(left.numerator, right.numerator), multiply(left.denominator, right.denominator)};
}

RationalExpression divide(const RationalExpression& left, const RationalExpression& right) {
    if (right.numerator.isZero())
        throw ParseError("不能除以恒为 0 的表达式");
    return {multiply(left.numerator, right.denominator), multiply(left.denominator, right.numerator)};
}

RationalExpression power(const RationalExpression& value, int exponent) {
    if (exponent == 0) {
        if (value.numerator.isZero())
            throw ParseError("0 的 0 次幂没有定义");
        return {Polynomial::constant(1.0), Polynomial::constant(1.0)};
    }
    if (exponent > 0) {
        return {power(value.numerator, exponent), power(value.denominator, exponent)};
    }
    if (value.numerator.isZero())
        throw ParseError("0 不能取负数指数幂");
    return {power(value.denominator, -exponent), power(value.numerator, -exponent)};
}

// ---------- 词法单元与指数语法 ----------

enum class TokenType {
    Number,
    Variable,
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    Factorial,
    LeftParen,
    RightParen,
    Sin,
    Cos,
    Tan,
    Cot,
    Equal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    End
};

struct Token {
    TokenType type = TokenType::End;
    double number = 0.0;
    int variable = -1;
    size_t position = 0;
};

struct ParsedExponent {
    int numerator = 1;
    int denominator = 1;
    size_t nextIndex = 0;
};

std::optional<ParsedExponent> exponentAt(const std::vector<Token>& tokens, size_t index) {
    bool parenthesized = false;
    if (tokens[index].type == TokenType::LeftParen) {
        parenthesized = true;
        ++index;
    }
    int sign = 1;
    if (tokens[index].type == TokenType::Plus || tokens[index].type == TokenType::Minus) {
        if (tokens[index].type == TokenType::Minus)
            sign = -1;
        ++index;
    }
    if (tokens[index].type != TokenType::Number)
        return std::nullopt;
    const auto exact = rationalize(tokens[index].number);
    if (!exact)
        return std::nullopt;
    std::int64_t numerator = sign * exact->numerator;
    std::int64_t denominator = exact->denominator;
    ++index;
    if (parenthesized && tokens[index].type == TokenType::Slash) {
        ++index;
        int denominatorSign = 1;
        if (tokens[index].type == TokenType::Plus || tokens[index].type == TokenType::Minus) {
            if (tokens[index].type == TokenType::Minus)
                denominatorSign = -1;
            ++index;
        }
        if (tokens[index].type != TokenType::Number)
            return std::nullopt;
        const auto divisor = rationalize(tokens[index].number);
        if (!divisor || divisor->numerator == 0 || divisor->denominator != 1)
            return std::nullopt;
        denominator *= denominatorSign * divisor->numerator;
        ++index;
    }
    if (parenthesized) {
        if (tokens[index].type != TokenType::RightParen)
            return std::nullopt;
        ++index;
    }
    if (denominator < 0) {
        numerator = -numerator;
        denominator = -denominator;
    }
    const std::int64_t divisor = std::gcd(numerator < 0 ? -numerator : numerator, denominator);
    numerator /= divisor;
    denominator /= divisor;
    if (numerator < -32 || numerator > 32 || denominator < 1 || denominator > 16) {
        throw ParseError("指数的绝对值不能超过 32，分母不能超过 16");
    }
    return ParsedExponent{static_cast<int>(numerator), static_cast<int>(denominator), index};
}

bool exponentBaseUsesX(const std::vector<Token>& tokens, size_t caretIndex) {
    if (caretIndex == 0)
        return false;
    const Token& previous = tokens[caretIndex - 1];
    if (previous.type == TokenType::Variable)
        return previous.variable == 0;
    if (previous.type != TokenType::RightParen)
        return false;
    int depth = 1;
    for (size_t i = caretIndex - 1; i-- > 0;) {
        if (tokens[i].type == TokenType::RightParen)
            ++depth;
        else if (tokens[i].type == TokenType::LeftParen && --depth == 0) {
            for (size_t j = i + 1; j + 1 < caretIndex; ++j) {
                if (tokens[j].type == TokenType::Variable && tokens[j].variable == 0)
                    return true;
            }
            return false;
        }
    }
    return false;
}

int fractionalVariableScale(const std::vector<Token>& tokens) {
    std::int64_t scale = 1;
    for (size_t i = 0; i + 1 < tokens.size(); ++i) {
        if (tokens[i].type != TokenType::Caret || !exponentBaseUsesX(tokens, i))
            continue;
        const auto exponent = exponentAt(tokens, i + 1);
        if (!exponent)
            throw ParseError("指数格式不正确，请使用如 ^-2、^(1/2) 或 ^(-2/3) 的格式");
        scale = safeLcm(scale, exponent->denominator);
        if (scale > 16)
            throw ParseError("分数指数的公分母不能超过 16");
    }
    return static_cast<int>(scale);
}

Polynomial fractionalMonomialPower(const Polynomial& value, int numerator, int denominator) {
    if (value.isZero()) {
        if (numerator <= 0)
            throw ParseError("0 不能取零次幂或负数指数幂");
        return Polynomial::constant(0.0);
    }
    if (value.terms.size() != 1) {
        throw ParseError("分数指数目前支持数值或单项式；含加减法的底数请先换元化简");
    }
    const auto& [powers, coefficient] = *value.terms.begin();
    if (coefficient < 0 && denominator % 2 == 0) {
        throw ParseError("偶数次根的底数不能为负数");
    }
    Powers resultPowers{};
    for (int i = 0; i < 3; ++i) {
        const long long scaled = static_cast<long long>(powers[i]) * numerator;
        if (scaled % denominator != 0) {
            throw ParseError("该分数指数无法化为当前支持的单项式形式");
        }
        resultPowers[i] = static_cast<int>(scaled / denominator);
    }
    const double rooted = coefficient < 0 ? -std::pow(-coefficient, 1.0 / denominator)
                                          : std::pow(coefficient, 1.0 / denominator);
    const double resultCoefficient = std::pow(rooted, numerator);
    if (!std::isfinite(resultCoefficient))
        throw ParseError("指数运算结果超出可处理范围");
    Polynomial result;
    result.terms[resultPowers] = resultCoefficient;
    result.clean();
    return result;
}

RationalExpression fractionalPower(const RationalExpression& value, int numerator, int denominator) {
    if (numerator == 0)
        return power(value, 0);
    const int magnitude = numerator < 0 ? -numerator : numerator;
    Polynomial poweredNumerator = fractionalMonomialPower(value.numerator, magnitude, denominator);
    Polynomial poweredDenominator = fractionalMonomialPower(value.denominator, magnitude, denominator);
    if (numerator < 0) {
        if (poweredNumerator.isZero())
            throw ParseError("0 不能取负数指数幂");
        return {poweredDenominator, poweredNumerator};
    }
    return {poweredNumerator, poweredDenominator};
}

int groupingRank(wchar_t ch) {
    if (ch == L'(' || ch == L')' || ch == L'（' || ch == L'）')
        return 1;
    if (ch == L'[' || ch == L']' || ch == L'［' || ch == L'］' || ch == L'【' || ch == L'】')
        return 2;
    if (ch == L'{' || ch == L'}' || ch == L'｛' || ch == L'｝')
        return 3;
    return 0;
}

bool isOpeningGrouping(wchar_t ch) {
    return ch == L'(' || ch == L'（' || ch == L'[' || ch == L'［' || ch == L'【' || ch == L'{' || ch == L'｛';
}

void validateGrouping(const std::wstring& text) {
    std::vector<int> stack;
    for (wchar_t ch : text) {
        const int rank = groupingRank(ch);
        if (rank == 0)
            continue;
        if (isOpeningGrouping(ch)) {
            if (!stack.empty() && rank >= stack.back()) {
                throw ParseError(
                    "括号嵌套顺序错误：只能大括号套中括号、中括号套小括号，同种括号不能互相嵌套");
            }
            stack.push_back(rank);
        } else {
            if (stack.empty() || stack.back() != rank)
                throw ParseError("括号不匹配");
            stack.pop_back();
        }
    }
    if (!stack.empty())
        throw ParseError("括号不匹配");
}

// ---------- 输入规范化与括号规则 ----------

std::wstring normalizeInput(std::wstring text) {
    for (wchar_t& ch : text) {
        if (ch == L'×' || ch == L'·')
            ch = L'*';
        else if (ch == L'÷')
            ch = L'/';
        else if (ch == L'−' || ch == L'–' || ch == L'—')
            ch = L'-';
        else if (ch == L'（')
            ch = L'(';
        else if (ch == L'）')
            ch = L')';
        else if (ch == L'[' || ch == L'［' || ch == L'【' || ch == L'{' || ch == L'｛')
            ch = L'(';
        else if (ch == L']' || ch == L'］' || ch == L'】' || ch == L'}' || ch == L'｝')
            ch = L')';
        else if (ch == L'＝')
            ch = L'=';
        else if (ch == L'！')
            ch = L'!';
        else if (ch == L'“' || ch == L'”' || ch == L'＂')
            ch = L'"';
    }
    std::wstring result;
    result.reserve(text.size() + 8);
    for (wchar_t ch : text) {
        if (ch == L'²')
            result += L"^2";
        else if (ch == L'³')
            result += L"^3";
        else if (ch == L'⁴')
            result += L"^4";
        else
            result += ch;
    }
    return result;
}

std::wstring displayRootNotation(const std::wstring& original) {
    const std::wstring text = normalizeInput(original);
    std::wstring result;
    for (size_t i = 0; i < text.size();) {
        if (text[i] != L'"') {
            result += text[i++];
            continue;
        }
        const size_t firstQuote = text.find(L'"', i + 1);
        if (firstQuote == std::wstring::npos) {
            result += text[i++];
            continue;
        }
        const std::wstring firstPart = text.substr(i + 1, firstQuote - i - 1);
        wchar_t* end = nullptr;
        const long possibleDegree = std::wcstol(firstPart.c_str(), &end, 10);
        const bool isDegree =
            end != firstPart.c_str() && end && *end == L'\0' && possibleDegree >= 2 && possibleDegree <= 16;
        const size_t secondQuote = isDegree ? text.find(L'"', firstQuote + 1) : std::wstring::npos;
        if (isDegree && secondQuote != std::wstring::npos) {
            const std::wstring radicand = text.substr(firstQuote + 1, secondQuote - firstQuote - 1);
            if (possibleDegree == 2)
                result += L"√{" + radicand + L"}";
            else if (possibleDegree == 3)
                result += L"∛{" + radicand + L"}";
            else
                result += L"√[" + std::to_wstring(possibleDegree) + L"]{" + radicand + L"}";
            i = secondQuote + 1;
        } else {
            result += L"√{" + firstPart + L"}";
            i = firstQuote + 1;
        }
    }
    return result;
}

std::vector<Token> tokenize(const std::wstring& original) {
    const std::wstring text = normalizeInput(original);
    std::vector<Token> tokens;
    size_t i = 0;
    while (i < text.size()) {
        const wchar_t ch = text[i];
        if (std::iswspace(ch)) {
            ++i;
            continue;
        }
        Token token;
        token.position = i;
        auto functionAt = [&](const wchar_t* name, TokenType type) -> bool {
            const size_t length = std::wcslen(name);
            if (i + length > text.size())
                return false;
            for (size_t offset = 0; offset < length; ++offset) {
                if (std::towlower(text[i + offset]) != name[offset])
                    return false;
            }
            size_t after = i + length;
            while (after < text.size() && std::iswspace(text[after]))
                ++after;
            if (after >= text.size() || text[after] != L'(')
                return false;
            token.type = type;
            tokens.push_back(token);
            i += length;
            return true;
        };
        if (functionAt(L"sin", TokenType::Sin) || functionAt(L"cos", TokenType::Cos) ||
            functionAt(L"tan", TokenType::Tan) || functionAt(L"cot", TokenType::Cot)) {
            continue;
        }
        if (ch == L'"') {
            const size_t firstQuote = text.find(L'"', i + 1);
            if (firstQuote == std::wstring::npos)
                throw ParseError("根式缺少右侧双引号 \"");
            auto parseDecimal = [&](const std::wstring& literal, double& output) {
                std::wstring value = literal;
                const size_t beginPosition = value.find_first_not_of(L" \t\r\n");
                if (beginPosition == std::wstring::npos)
                    return false;
                const size_t endPosition = value.find_last_not_of(L" \t\r\n");
                value = value.substr(beginPosition, endPosition - beginPosition + 1);
                std::wstring lower = value;
                std::transform(lower.begin(), lower.end(), lower.begin(),
                               [](wchar_t item) { return static_cast<wchar_t>(std::towlower(item)); });
                if (lower == L"pi" || value == L"π") {
                    output = kPi;
                    return true;
                }
                if (lower == L"e") {
                    output = kEuler;
                    return true;
                }
                const wchar_t* begin = value.c_str();
                wchar_t* end = nullptr;
                output = std::wcstod(begin, &end);
                while (end && *end && std::iswspace(*end))
                    ++end;
                return begin != end && end && *end == L'\0' && std::isfinite(output);
            };
            auto parseLiteral = [&](const std::wstring& literal, double& output) {
                std::wstring valueText = literal;
                while (!valueText.empty() && std::iswspace(valueText.front()))
                    valueText.erase(valueText.begin());
                while (!valueText.empty() && std::iswspace(valueText.back()))
                    valueText.pop_back();
                bool absolute = false;
                if (valueText.size() >= 2 && valueText.front() == L'|' && valueText.back() == L'|') {
                    absolute = true;
                    valueText = valueText.substr(1, valueText.size() - 2);
                }
                const size_t slash = valueText.find(L'/');
                if (slash == std::wstring::npos) {
                    if (!parseDecimal(valueText, output))
                        return false;
                    if (absolute)
                        output = std::abs(output);
                    return true;
                }
                if (valueText.find(L'/', slash + 1) != std::wstring::npos)
                    return false;
                double numerator = 0.0;
                double denominator = 0.0;
                if (!parseDecimal(valueText.substr(0, slash), numerator) ||
                    !parseDecimal(valueText.substr(slash + 1), denominator) ||
                    std::abs(denominator) < kEpsilon)
                    return false;
                output = numerator / denominator;
                if (absolute)
                    output = std::abs(output);
                return std::isfinite(output);
            };

            int rootDegree = 2;
            std::wstring radicandText = text.substr(i + 1, firstQuote - i - 1);
            size_t closingQuote = firstQuote;
            double possibleDegree = 0.0;
            const size_t secondQuote = text.find(L'"', firstQuote + 1);
            if (secondQuote != std::wstring::npos && parseLiteral(radicandText, possibleDegree) &&
                possibleDegree >= 2 && possibleDegree <= 16 &&
                std::abs(possibleDegree - std::round(possibleDegree)) < kEpsilon) {
                const std::wstring possibleRadicand =
                    text.substr(firstQuote + 1, secondQuote - firstQuote - 1);
                double checkedRadicand = 0.0;
                if (parseLiteral(possibleRadicand, checkedRadicand)) {
                    rootDegree = static_cast<int>(std::round(possibleDegree));
                    radicandText = possibleRadicand;
                    closingQuote = secondQuote;
                }
            }
            double radicand = 0.0;
            if (!parseLiteral(radicandText, radicand)) {
                throw ParseError("双引号根式中的被开方数需要是整数、小数、分数或数值绝对值");
            }
            if (radicand < 0 && rootDegree % 2 == 0)
                throw ParseError("偶次根号下不能是负数");
            token.type = TokenType::Number;
            token.number =
                radicand < 0 ? -std::pow(-radicand, 1.0 / rootDegree) : std::pow(radicand, 1.0 / rootDegree);
            tokens.push_back(token);
            i = closingQuote + 1;
            continue;
        }
        if (std::iswdigit(ch) || ch == L'.') {
            wchar_t* end = nullptr;
            token.number = std::wcstod(text.c_str() + i, &end);
            if (end == text.c_str() + i || !std::isfinite(token.number))
                throw ParseError("数字格式不正确");
            token.type = TokenType::Number;
            i = static_cast<size_t>(end - text.c_str());
            tokens.push_back(token);
            continue;
        }
        if (ch == L'π' || ((ch == L'p' || ch == L'P') && i + 1 < text.size() &&
                           (text[i + 1] == L'i' || text[i + 1] == L'I'))) {
            token.type = TokenType::Number;
            token.number = kPi;
            i += ch == L'π' ? 1 : 2;
            tokens.push_back(token);
            continue;
        }
        if (ch == L'e' || ch == L'E') {
            token.type = TokenType::Number;
            token.number = kEuler;
            ++i;
            tokens.push_back(token);
            continue;
        }
        if (ch == L'x' || ch == L'X' || ch == L'y' || ch == L'Y' || ch == L'z' || ch == L'Z') {
            token.type = TokenType::Variable;
            token.variable = (ch == L'x' || ch == L'X') ? 0 : ((ch == L'y' || ch == L'Y') ? 1 : 2);
            ++i;
            tokens.push_back(token);
            continue;
        }
        ++i;
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
            if (i < text.size() && text[i] == L'=')
                ++i;
            break;
        case L'<':
            token.type = TokenType::Less;
            if (i < text.size() && text[i] == L'=') {
                token.type = TokenType::LessEqual;
                ++i;
            }
            break;
        case L'>':
            token.type = TokenType::Greater;
            if (i < text.size() && text[i] == L'=') {
                token.type = TokenType::GreaterEqual;
                ++i;
            }
            break;
        case L'≤':
            token.type = TokenType::LessEqual;
            break;
        case L'≥':
            token.type = TokenType::GreaterEqual;
            break;
        default:
            throw ParseError("包含无法识别的字符");
        }
        tokens.push_back(token);
    }
    tokens.push_back({TokenType::End, 0.0, -1, text.size()});
    return tokens;
}

struct ParsedRelation {
    RationalExpression left;
    RationalExpression right;
    TokenType relation = TokenType::Equal;
};

// 递归下降解析器直接构造 RationalExpression，隐式乘法在 parseTerm 中处理。
class Parser {
  public:
    explicit Parser(const std::wstring& text, AngleUnit angleUnit = AngleUnit::Radians)
        : tokens_(tokenize(text)), fractionalScale_(fractionalVariableScale(tokens_)), angleUnit_(angleUnit) {
    }

    int fractionalScale() const {
        return fractionalScale_;
    }

    ParsedRelation parseRelation() {
        ParsedRelation result;
        result.left = parseExpression();
        const TokenType relation = current().type;
        if (relation != TokenType::Equal && relation != TokenType::Less && relation != TokenType::LessEqual &&
            relation != TokenType::Greater && relation != TokenType::GreaterEqual) {
            throw ParseError("原式中需要有等号或不等号");
        }
        result.relation = relation;
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
        return type == TokenType::Number || type == TokenType::Variable || type == TokenType::LeftParen ||
               type == TokenType::Sin || type == TokenType::Cos || type == TokenType::Tan ||
               type == TokenType::Cot;
    }

    RationalExpression parseExpression() {
        RationalExpression result = parseTerm();
        while (current().type == TokenType::Plus || current().type == TokenType::Minus) {
            const bool subtract = current().type == TokenType::Minus;
            ++index_;
            result = add(result, parseTerm(), subtract ? -1.0 : 1.0);
        }
        return result;
    }

    RationalExpression parseTerm() {
        RationalExpression result = parseUnary();
        while (current().type == TokenType::Star || current().type == TokenType::Slash ||
               startsPrimary(current().type)) {
            const TokenType operation = current().type;
            if (operation == TokenType::Star || operation == TokenType::Slash)
                ++index_;
            RationalExpression right = parseUnary();
            result = operation == TokenType::Slash ? divide(result, right) : multiply(result, right);
        }
        return result;
    }

    RationalExpression parseUnary() {
        if (current().type == TokenType::Plus) {
            ++index_;
            return parseUnary();
        }
        if (current().type == TokenType::Minus) {
            ++index_;
            return multiply({Polynomial::constant(-1.0), Polynomial::constant(1.0)}, parseUnary());
        }
        return parsePower();
    }

    RationalExpression parsePower() {
        RationalExpression result = parsePrimary();
        while (current().type == TokenType::Factorial) {
            if (!result.numerator.isConstant() || !result.denominator.isConstant())
                throw ParseError("含未知数的表达式不能直接取阶乘");
            const double denominator = result.denominator.constantValue();
            if (std::abs(denominator) < kEpsilon)
                throw ParseError("计算过程中出现除以 0");
            result = {Polynomial::constant(factorialValue(result.numerator.constantValue() / denominator)),
                      Polynomial::constant(1.0)};
            ++index_;
        }
        if (current().type == TokenType::Caret) {
            ++index_;
            const auto exponent = exponentAt(tokens_, index_);
            if (!exponent) {
                throw ParseError("指数格式不正确，请使用如 ^-2、^(1/2) 或 ^(-2/3) 的格式");
            }
            index_ = exponent->nextIndex;
            result = exponent->denominator == 1
                         ? power(result, exponent->numerator)
                         : fractionalPower(result, exponent->numerator, exponent->denominator);
        }
        return result;
    }

    RationalExpression parsePrimary() {
        if (current().type == TokenType::Number) {
            const double value = current().number;
            ++index_;
            return {Polynomial::constant(value), Polynomial::constant(1.0)};
        }
        if (current().type == TokenType::Variable) {
            const int variable = current().variable;
            ++index_;
            Polynomial value = Polynomial::variable(variable);
            if (variable == 0 && fractionalScale_ > 1)
                value = power(value, fractionalScale_);
            return {value, Polynomial::constant(1.0)};
        }
        if (current().type == TokenType::Sin || current().type == TokenType::Cos ||
            current().type == TokenType::Tan || current().type == TokenType::Cot) {
            const TokenType function = current().type;
            ++index_;
            if (current().type != TokenType::LeftParen)
                throw ParseError("三角函数后需要使用括号");
            ++index_;
            const RationalExpression argument = parseExpression();
            if (current().type != TokenType::RightParen)
                throw ParseError("三角函数缺少右括号");
            ++index_;
            if (!argument.numerator.isConstant() || !argument.denominator.isConstant()) {
                throw ParseError(
                    "含未知数的三角方程目前支持 sin(ax+b)=c、cos(ax+b)=c、tan(ax+b)=c 或 cot(ax+b)=c");
            }
            const double denominator = argument.denominator.constantValue();
            if (std::abs(denominator) < kEpsilon)
                throw ParseError("三角函数参数中出现除以 0");
            const double inputAngle = argument.numerator.constantValue() / denominator;
            const double angle = angleUnit_ == AngleUnit::Degrees ? inputAngle * kPi / 180.0 : inputAngle;
            double value = 0.0;
            if (function == TokenType::Sin)
                value = std::sin(angle);
            else if (function == TokenType::Cos)
                value = std::cos(angle);
            else if (function == TokenType::Tan) {
                if (std::abs(std::cos(angle)) < 1e-12)
                    throw ParseError("tan 在该角度无定义");
                value = std::tan(angle);
            } else {
                if (std::abs(std::sin(angle)) < 1e-12)
                    throw ParseError("cot 在该角度无定义");
                value = std::cos(angle) / std::sin(angle);
            }
            return {Polynomial::constant(value), Polynomial::constant(1.0)};
        }
        if (current().type == TokenType::LeftParen) {
            ++index_;
            RationalExpression result = parseExpression();
            if (current().type != TokenType::RightParen)
                throw ParseError("缺少右括号 )");
            ++index_;
            return result;
        }
        throw ParseError("此处应为数字、未知数或左括号");
    }

    std::vector<Token> tokens_;
    size_t index_ = 0;
    int fractionalScale_ = 1;
    AngleUnit angleUnit_ = AngleUnit::Radians;
};

std::vector<std::wstring> splitLines(const std::wstring& text) {
    std::vector<std::wstring> result;
    std::wstring current;
    auto flush = [&]() {
        const auto begin = current.find_first_not_of(L" \t\r\n");
        if (begin != std::wstring::npos) {
            const auto end = current.find_last_not_of(L" \t\r\n");
            result.push_back(current.substr(begin, end - begin + 1));
        }
        current.clear();
    };
    for (wchar_t ch : text) {
        if (ch == L'\n' || ch == L';' || ch == L'；')
            flush();
        else if (ch != L'\r')
            current += ch;
    }
    flush();
    return result;
}

Relation toRelation(TokenType type) {
    switch (type) {
    case TokenType::Less:
        return Relation::Less;
    case TokenType::LessEqual:
        return Relation::LessEqual;
    case TokenType::Greater:
        return Relation::Greater;
    case TokenType::GreaterEqual:
        return Relation::GreaterEqual;
    default:
        throw ParseError("这里需要不等号");
    }
}

Relation reverseRelation(Relation relation) {
    switch (relation) {
    case Relation::Less:
        return Relation::Greater;
    case Relation::LessEqual:
        return Relation::GreaterEqual;
    case Relation::Greater:
        return Relation::Less;
    case Relation::GreaterEqual:
        return Relation::LessEqual;
    }
    return relation;
}

std::wstring relationSymbol(Relation relation) {
    switch (relation) {
    case Relation::Less:
        return L"<";
    case Relation::LessEqual:
        return L"≤";
    case Relation::Greater:
        return L">";
    case Relation::GreaterEqual:
        return L"≥";
    }
    return L"?";
}

std::wstring polynomialText(const Polynomial& polynomial) {
    if (polynomial.isZero())
        return L"0";
    std::vector<std::pair<Powers, double>> terms(polynomial.terms.begin(), polynomial.terms.end());
    std::sort(terms.begin(), terms.end(), [](const auto& left, const auto& right) {
        const int leftDegree = left.first[0] + left.first[1] + left.first[2];
        const int rightDegree = right.first[0] + right.first[1] + right.first[2];
        if (leftDegree != rightDegree)
            return leftDegree > rightDegree;
        return left.first > right.first;
    });
    std::wstring result;
    bool first = true;
    for (const auto& [powers, coefficient] : terms) {
        if (std::abs(coefficient) < kEpsilon)
            continue;
        const bool negative = coefficient < 0;
        const double magnitude = std::abs(coefficient);
        if (first) {
            if (negative)
                result += L"-";
        } else {
            result += negative ? L" - " : L" + ";
        }
        const bool hasVariable = powers[0] || powers[1] || powers[2];
        if (!hasVariable || std::abs(magnitude - 1.0) >= kEpsilon)
            result += symbolicNumber(magnitude);
        const wchar_t* names[] = {L"x", L"y", L"z"};
        for (int i = 0; i < 3; ++i) {
            if (powers[i] > 0) {
                result += names[i];
                if (powers[i] > 1)
                    result += L"^" + std::to_wstring(powers[i]);
            }
        }
        first = false;
    }
    return result.empty() ? L"0" : result;
}

void ensureOnlyX(const Polynomial& value) {
    if (value.usesVariable(1) || value.usesVariable(2))
        throw ParseError("该题型只能包含未知数 x");
}

void ensureUnivariateDegree(const Polynomial& value, int maxDegree) {
    ensureOnlyX(value);
    if (value.totalDegree() > maxDegree || value.degree(0) > maxDegree) {
        throw ParseError("化简后的方程次数超出当前题型支持范围");
    }
}

SolveResult prependProcess(const std::wstring& input, const Polynomial& polynomial, SolveResult result) {
    std::wstring prefix = L"输入原式：" + displayRootNotation(input) + L"\r\n" +
                          L"解析、移项并合并同类项：\r\n" + L"    " + polynomialText(polynomial) +
                          L" = 0\r\n\r\n";
    result.process = prefix + result.process;
    return result;
}

std::wstring withSubstitutionVariable(std::wstring text) {
    std::replace(text.begin(), text.end(), L'x', L't');
    return text;
}

std::wstring poweredCandidateText(double baseValue, const std::wstring& baseText, int exponent) {
    const double value = std::pow(baseValue, exponent);
    const std::wstring symbolic = symbolicNumber(value);
    if (symbolic.find(L'.') == std::wstring::npos)
        return symbolic;

    // Cardano 形式有时会得到 -a - ∛b - ∛c。奇次幂时把整体负号
    // 提到括号外，写成 -(a + ∛b + ∛c)^n，更接近标准手写形式。
    if (exponent % 2 == 1 && !baseText.empty() && baseText.front() == L'-') {
        std::vector<std::wstring> terms;
        size_t termStart = 1;
        int depth = 0;
        bool allNegative = true;
        for (size_t i = 1; i < baseText.size();) {
            const wchar_t ch = baseText[i];
            if (ch == L'(' || ch == L'{' || ch == L'[')
                ++depth;
            else if (ch == L')' || ch == L'}' || ch == L']')
                --depth;
            if (depth == 0 && i + 2 < baseText.size() && baseText.compare(i, 3, L" - ") == 0) {
                terms.push_back(baseText.substr(termStart, i - termStart));
                termStart = i + 3;
                i += 3;
                continue;
            }
            if (depth == 0 && i + 2 < baseText.size() && baseText.compare(i, 3, L" + ") == 0) {
                allNegative = false;
                break;
            }
            ++i;
        }
        if (allNegative) {
            terms.push_back(baseText.substr(termStart));
            if (terms.size() > 2)
                std::stable_sort(terms.begin() + 1, terms.end());
            std::wstring magnitude;
            for (const auto& term : terms) {
                if (!magnitude.empty())
                    magnitude += L" + ";
                magnitude += term;
            }
            return L"-(" + magnitude + L")^" + std::to_wstring(exponent);
        }
    }
    return L"(" + baseText + L")^" + std::to_wstring(exponent);
}

long double evaluateCoefficients(const std::vector<long double>& coefficients, long double value) {
    long double result = 0.0L;
    for (auto it = coefficients.rbegin(); it != coefficients.rend(); ++it)
        result = result * value + *it;
    return result;
}

void appendDistinctRoot(std::vector<long double>& roots, long double root) {
    for (const long double existing : roots) {
        if (std::abs(existing - root) <= 1e-10L * std::max({1.0L, std::abs(existing), std::abs(root)}))
            return;
    }
    roots.push_back(root);
}

std::vector<long double> realPolynomialRoots(std::vector<long double> coefficients) {
    while (coefficients.size() > 1 && std::abs(coefficients.back()) < 1e-18L)
        coefficients.pop_back();
    const int degree = static_cast<int>(coefficients.size()) - 1;
    if (degree <= 0)
        return {};
    if (degree == 1)
        return {-coefficients[0] / coefficients[1]};

    std::vector<long double> derivative(static_cast<size_t>(degree));
    for (int i = 1; i <= degree; ++i)
        derivative[static_cast<size_t>(i - 1)] = coefficients[i] * i;
    std::vector<long double> critical = realPolynomialRoots(std::move(derivative));
    std::sort(critical.begin(), critical.end());

    const long double leading = std::abs(coefficients.back());
    long double bound = 1.0L;
    for (int i = 0; i < degree; ++i)
        bound = std::max(bound, 1.0L + std::abs(coefficients[i]) / leading);
    std::vector<long double> points{-bound};
    for (long double value : critical) {
        if (value > -bound && value < bound)
            points.push_back(value);
    }
    points.push_back(bound);

    auto nearZero = [&](long double x, long double value) {
        long double scale = 1.0L;
        const long double magnitude = std::max(1.0L, std::abs(x));
        long double powerValue = 1.0L;
        for (const long double coefficient : coefficients) {
            scale += std::abs(coefficient) * powerValue;
            powerValue *= magnitude;
        }
        return std::abs(value) <= 1e-12L * scale;
    };

    std::vector<long double> roots;
    for (long double point : points) {
        const long double value = evaluateCoefficients(coefficients, point);
        if (nearZero(point, value))
            appendDistinctRoot(roots, point);
    }
    for (size_t i = 0; i + 1 < points.size(); ++i) {
        long double left = points[i];
        long double right = points[i + 1];
        long double leftValue = evaluateCoefficients(coefficients, left);
        const long double rightValue = evaluateCoefficients(coefficients, right);
        if (nearZero(left, leftValue) || nearZero(right, rightValue) || (leftValue < 0) == (rightValue < 0))
            continue;
        for (int iteration = 0; iteration < 160; ++iteration) {
            const long double middle = (left + right) / 2.0L;
            const long double middleValue = evaluateCoefficients(coefficients, middle);
            if (nearZero(middle, middleValue)) {
                left = right = middle;
                break;
            }
            if ((leftValue < 0) != (middleValue < 0)) {
                right = middle;
            } else {
                left = middle;
                leftValue = middleValue;
            }
        }
        appendDistinctRoot(roots, (left + right) / 2.0L);
    }
    std::sort(roots.begin(), roots.end());
    return roots;
}

// ---------- 一元三次与四次方程：精确因式、综合除法和复数数值根 ----------

std::wstring subscriptNumber(size_t value) {
    static constexpr std::array<wchar_t, 10> digits{L'₀', L'₁', L'₂', L'₃', L'₄',
                                                    L'₅', L'₆', L'₇', L'₈', L'₉'};
    std::wstring text;
    do {
        text.insert(text.begin(), digits[value % 10]);
        value /= 10;
    } while (value > 0);
    return text;
}

std::vector<std::complex<double>> complexPolynomialRoots(std::vector<double> coefficients) {
    while (coefficients.size() > 1 && std::abs(coefficients.back()) < kEpsilon)
        coefficients.pop_back();
    const size_t degree = coefficients.size() - 1;
    if (degree == 0)
        return {};
    if (degree == 1)
        return {{-coefficients[0] / coefficients[1], 0.0}};

    const double leading = coefficients.back();
    for (double& coefficient : coefficients)
        coefficient /= leading;
    double radius = 1.0;
    for (size_t i = 0; i < degree; ++i)
        radius = std::max(radius, 1.0 + std::abs(coefficients[i]));

    const auto evaluate = [&](std::complex<double> value) {
        std::complex<double> result{};
        for (auto it = coefficients.rbegin(); it != coefficients.rend(); ++it)
            result = result * value + *it;
        return result;
    };

    std::vector<std::complex<double>> roots(degree);
    for (size_t i = 0; i < degree; ++i) {
        const double angle = 2.0 * kPi * (static_cast<double>(i) + 0.35) / degree;
        roots[i] = std::polar(radius, angle);
    }
    for (int iteration = 0; iteration < 2000; ++iteration) {
        std::vector<std::complex<double>> next = roots;
        double maximumChange = 0.0;
        for (size_t i = 0; i < degree; ++i) {
            std::complex<double> denominator{1.0, 0.0};
            for (size_t j = 0; j < degree; ++j)
                if (i != j)
                    denominator *= roots[i] - roots[j];
            if (std::abs(denominator) < 1e-18)
                denominator += std::complex<double>(1e-12, 1e-12);
            const std::complex<double> correction = evaluate(roots[i]) / denominator;
            next[i] = roots[i] - correction;
            maximumChange = std::max(maximumChange, std::abs(correction));
        }
        roots.swap(next);
        if (maximumChange < 1e-13)
            break;
    }
    for (auto& root : roots) {
        if (std::abs(root.real()) < 5e-11)
            root.real(0.0);
        if (std::abs(root.imag()) < 5e-9)
            root.imag(0.0);
    }
    std::sort(roots.begin(), roots.end(), [](const auto& left, const auto& right) {
        if (std::abs(left.imag() - right.imag()) > 1e-8)
            return left.imag() < right.imag();
        return left.real() < right.real();
    });
    return roots;
}

std::optional<std::vector<std::int64_t>> integerPolynomial(const std::vector<double>& coefficients) {
    std::vector<ExactFraction> fractions;
    fractions.reserve(coefficients.size());
    std::int64_t commonDenominator = 1;
    for (const double coefficient : coefficients) {
        const auto fraction = rationalize(coefficient);
        if (!fraction)
            return std::nullopt;
        fractions.push_back(*fraction);
        commonDenominator = safeLcm(commonDenominator, fraction->denominator);
    }
    std::vector<std::int64_t> integers(coefficients.size());
    for (size_t i = 0; i < coefficients.size(); ++i) {
        const long double value =
            static_cast<long double>(fractions[i].numerator) * (commonDenominator / fractions[i].denominator);
        if (std::abs(value) > static_cast<long double>(std::numeric_limits<std::int64_t>::max()))
            return std::nullopt;
        integers[i] = static_cast<std::int64_t>(value);
    }
    std::int64_t divisor = 0;
    for (const auto value : integers)
        divisor = std::gcd(divisor, value < 0 ? -value : value);
    if (divisor > 1)
        for (auto& value : integers)
            value /= divisor;
    return integers;
}

std::vector<std::int64_t> positiveDivisors(std::int64_t value) {
    value = value < 0 ? -value : value;
    if (value == 0 || value > 100000000)
        return {};
    std::vector<std::int64_t> divisors;
    for (std::int64_t candidate = 1; candidate <= value / candidate; ++candidate) {
        if (value % candidate != 0)
            continue;
        divisors.push_back(candidate);
        if (candidate != value / candidate)
            divisors.push_back(value / candidate);
    }
    std::sort(divisors.begin(), divisors.end());
    return divisors;
}

std::optional<ExactFraction> rationalPolynomialRoot(const std::vector<double>& coefficients) {
    if (coefficients.empty())
        return std::nullopt;
    if (std::abs(coefficients.front()) < kEpsilon)
        return ExactFraction(0, 1);
    const auto integers = integerPolynomial(coefficients);
    if (!integers || integers->front() == 0 || integers->back() == 0)
        return std::nullopt;
    const auto numerators = positiveDivisors(integers->front());
    const auto denominators = positiveDivisors(integers->back());
    for (const std::int64_t denominator : denominators) {
        for (const std::int64_t numerator : numerators) {
            for (const int sign : {-1, 1}) {
                const ExactFraction candidate(sign * numerator, denominator);
                const long double x = candidate.value();
                long double value = 0.0L;
                long double scale = 1.0L;
                for (auto it = integers->rbegin(); it != integers->rend(); ++it) {
                    value = value * x + *it;
                    scale = scale * std::max(1.0L, std::abs(x)) + std::abs(static_cast<long double>(*it));
                }
                if (std::abs(value) <= 1e-11L * scale)
                    return candidate;
            }
        }
    }
    return std::nullopt;
}

std::vector<double> syntheticDivide(const std::vector<double>& coefficients, double root) {
    if (coefficients.size() <= 1)
        return {};
    std::vector<double> quotient(coefficients.size() - 1, 0.0);
    quotient.back() = coefficients.back();
    for (size_t index = quotient.size() - 1; index > 0; --index)
        quotient[index - 1] = coefficients[index] + root * quotient[index];
    return quotient;
}

std::wstring coefficientPolynomialText(const std::vector<double>& coefficients) {
    Polynomial polynomial;
    for (size_t power = 0; power < coefficients.size(); ++power) {
        if (std::abs(coefficients[power]) >= kEpsilon)
            polynomial.terms[{static_cast<int>(power), 0, 0}] = coefficients[power];
    }
    return polynomialText(polynomial);
}

struct DisplayRoot {
    std::complex<double> value{};
    std::wstring text;
    bool approximate = false;
};

std::wstring approximateComplex(std::complex<double> value) {
    if (std::abs(value.imag()) < 5e-9)
        return approximateNumber(value.real());
    if (std::abs(value.real()) < 5e-9)
        return (value.imag() < 0 ? L"-" : L"") + approximateNumber(std::abs(value.imag())) + L"i";
    return approximateNumber(value.real()) + (value.imag() < 0 ? L" - " : L" + ") +
           approximateNumber(std::abs(value.imag())) + L"i";
}

void appendDisplayRoot(std::vector<DisplayRoot>& roots, DisplayRoot root) {
    for (auto& existing : roots) {
        if (std::abs(existing.value - root.value) <= 1e-7 * std::max(1.0, std::abs(root.value))) {
            if (existing.approximate && !root.approximate)
                existing = std::move(root);
            return;
        }
    }
    roots.push_back(std::move(root));
}

std::wstring exactSquareRootText(double radicand, const std::wstring& exactRadicand) {
    const std::wstring simplified = symbolicNumber(std::sqrt(radicand));
    if (simplified.find(L'.') == std::wstring::npos)
        return simplified;
    return L"√{" + exactRadicand + L"}";
}

struct ExactSquareRootForm {
    std::int64_t coefficient = 0;
    std::int64_t radicand = 1;
    std::int64_t denominator = 1;

    double value() const {
        return static_cast<double>(coefficient) * std::sqrt(static_cast<double>(radicand)) /
               static_cast<double>(denominator);
    }

    std::wstring text() const {
        if (coefficient == 0)
            return L"0";
        if (radicand == 1)
            return ExactFraction(coefficient, denominator).text();
        std::wstring result;
        if (coefficient != 1)
            result += std::to_wstring(coefficient);
        result += L"√" + std::to_wstring(radicand);
        if (denominator != 1)
            result += L"/" + std::to_wstring(denominator);
        return result;
    }
};

std::optional<ExactSquareRootForm> exactSquareRootForm(const ExactFraction& value) {
    if (value.numerator < 0)
        return std::nullopt;
    if (value.numerator == 0)
        return ExactSquareRootForm{};
    if (value.numerator > std::numeric_limits<std::int64_t>::max() / value.denominator)
        return std::nullopt;
    const std::int64_t rationalizedRadicand = value.numerator * value.denominator;
    const auto [outside, inside] = simplifySquareRoot(rationalizedRadicand);
    const std::int64_t divisor = std::gcd(outside, value.denominator);
    return ExactSquareRootForm{outside / divisor, inside, value.denominator / divisor};
}

struct ExactRadicalExpression {
    std::wstring text;
    std::optional<ExactFraction> rational;
    double value = 0.0;
};

ExactRadicalExpression combineRationalAndSquareRoot(const ExactFraction& rational,
                                                    const ExactSquareRootForm& root, int rootSign) {
    if (root.coefficient == 0 || rootSign == 0)
        return {rational.text(), rational, rational.value()};
    if (root.radicand == 1) {
        const ExactFraction signedRoot(rootSign * root.coefficient, root.denominator);
        const ExactFraction combined =
            rational - ExactFraction(-signedRoot.numerator, signedRoot.denominator);
        return {combined.text(), combined, combined.value()};
    }

    // 分别约分有理项与根式项。相比先通分成
    // (a ± b√c)/d，这种写法可直接显示成 a/d ± √c/e，
    // 也能让数学控件把两个分数分别排成上下结构。
    std::wstring radical;
    if (root.coefficient != 1)
        radical += std::to_wstring(root.coefficient);
    radical += L"√" + std::to_wstring(root.radicand);
    if (root.denominator != 1)
        radical = L"(" + radical + L")/" + std::to_wstring(root.denominator);

    std::wstring text;
    if (!rational.isZero())
        text = rational.text();
    if (text.empty())
        text = rootSign < 0 ? L"-" + radical : radical;
    else
        text += rootSign < 0 ? L" - " + radical : L" + " + radical;
    return {text, std::nullopt, rational.value() + rootSign * root.value()};
}

struct SignedExactTerm {
    double value = 0.0;
    std::wstring magnitude;
};

std::wstring joinSignedTerms(const std::vector<SignedExactTerm>& terms) {
    std::wstring result;
    for (const auto& term : terms) {
        if (std::abs(term.value) < 1e-13)
            continue;
        const bool negative = term.value < 0.0;
        if (result.empty())
            result = negative ? L"-" + term.magnitude : term.magnitude;
        else
            result += negative ? L" - " + term.magnitude : L" + " + term.magnitude;
    }
    return result.empty() ? L"0" : result;
}

struct ExactCubeRootTerm {
    double value = 0.0;
    std::wstring magnitude;
};

std::optional<ExactCubeRootTerm> exactCubeRootTerm(const ExactFraction& base, const ExactSquareRootForm& root,
                                                   int rootSign) {
    ExactRadicalExpression radicand = combineRationalAndSquareRoot(base, root, rootSign);
    const bool negative = radicand.value < 0.0;
    if (negative) {
        radicand =
            combineRationalAndSquareRoot(ExactFraction(-base.numerator, base.denominator), root, -rootSign);
    }
    std::wstring magnitude;
    if (radicand.rational)
        magnitude = simplifiedNthRoot(*radicand.rational, 3);
    else
        magnitude = L"∛{" + radicand.text + L"}";
    return ExactCubeRootTerm{negative ? -std::cbrt(radicand.value) : std::cbrt(radicand.value), magnitude};
}

std::optional<SolveResult> solveCubicByCardano(const std::wstring& text, const Polynomial& polynomial,
                                               const std::vector<double>& coefficients,
                                               std::vector<DisplayRoot>* rootsOutput = nullptr) {
    const auto integer = integerPolynomial(coefficients);
    if (!integer || integer->size() != 4 || integer->back() == 0)
        return std::nullopt;
    const long double a = static_cast<long double>((*integer)[3]);
    const long double b = static_cast<long double>((*integer)[2]);
    const long double c = static_cast<long double>((*integer)[1]);
    const long double d = static_cast<long double>((*integer)[0]);
    const auto p = rationalize(static_cast<double>((3.0L * a * c - b * b) / (3.0L * a * a)));
    const auto q = rationalize(
        static_cast<double>((2.0L * b * b * b - 9.0L * a * b * c + 27.0L * a * a * d) / (27.0L * a * a * a)));
    const auto shift = rationalize(static_cast<double>(-b / (3.0L * a)));
    if (!p || !q || !shift)
        return std::nullopt;
    const long double discriminantValue = std::pow(static_cast<long double>(q->value()) / 2.0L, 2) +
                                          std::pow(static_cast<long double>(p->value()) / 3.0L, 3);
    const auto discriminant = rationalize(static_cast<double>(discriminantValue));
    if (!discriminant)
        return std::nullopt;

    const SignedExactTerm shiftTerm{shift->value(),
                                    ExactFraction(std::abs(shift->numerator), shift->denominator).text()};
    std::vector<DisplayRoot> roots;
    std::wostringstream process;
    process << L"输入原式：" << displayRootNotation(text) << L"\r\n"
            << L"移项并合并同类项：\r\n    " << polynomialText(polynomial) << L" = 0\r\n\r\n";

    if (discriminant->numerator >= 0) {
        const auto squareRoot = exactSquareRootForm(*discriminant);
        if (!squareRoot)
            return std::nullopt;
        const ExactFraction base(-q->numerator, 2 * q->denominator);
        const auto first = exactCubeRootTerm(base, *squareRoot, 1);
        const auto second = exactCubeRootTerm(base, *squareRoot, -1);
        if (!first || !second)
            return std::nullopt;
        const std::wstring realRootText = joinSignedTerms(
            {shiftTerm, {first->value, first->magnitude}, {second->value, second->magnitude}});
        const double realRoot = shift->value() + first->value + second->value;
        appendDisplayRoot(roots, {{realRoot, 0.0}, realRootText, false});

        const std::wstring sumText =
            joinSignedTerms({{first->value, first->magnitude}, {second->value, second->magnitude}});
        const std::wstring differenceText =
            joinSignedTerms({{first->value, first->magnitude}, {-second->value, second->magnitude}});
        const std::wstring complexRealText =
            shift->isZero() ? L"-(" + sumText + L")/2" : shift->text() + L" - (" + sumText + L")/2";
        const std::wstring complexImaginaryText = L"√3·(" + differenceText + L")/2";
        const double complexReal = shift->value() - (first->value + second->value) / 2.0;
        const double complexImaginary = std::sqrt(3.0) * std::abs(first->value - second->value) / 2.0;
        appendDisplayRoot(roots, {{complexReal, -complexImaginary},
                                  complexRealText + L" - " + complexImaginaryText + L"i",
                                  false});
        appendDisplayRoot(
            roots,
            {{complexReal, complexImaginary}, complexRealText + L" + " + complexImaginaryText + L"i", false});

        process << L"方法一：卡尔达诺公式\r\n"
                << L"令 x = u " << (shift->numerator < 0 ? L"- " : L"+ ")
                << ExactFraction(std::abs(shift->numerator), shift->denominator).text()
                << L"，得到降幂三次方程 u³ + pu + q = 0。\r\n"
                << L"    u³ + (" << p->text() << L")u + (" << q->text() << L") = 0\r\n"
                << L"    p = " << p->text() << L"，q = " << q->text() << L"，D = (q/2)² + (p/3)³ = "
                << discriminant->text() << L"\r\n"
                << L"    A³ = -q/2 + √D = " << base.text() << L" + " << squareRoot->text() << L"\r\n"
                << L"    B³ = -q/2 - √D = " << base.text() << L" - " << squareRoot->text() << L"\r\n"
                << L"设 A = " << (first->value < 0 ? L"-" : L"") << first->magnitude << L"，B = "
                << (second->value < 0 ? L"-" : L"") << second->magnitude
                << L"，代入卡尔达诺公式得到结果中的三个精确根。\r\n\r\n"
                << L"方法二：降次与复数单位根法\r\n"
                << L"沿用降幂方程 u³ + (" << p->text() << L")u + (" << q->text() << L") = 0，"
                << L"并取上面算出的 A、B。\r\n"
                << L"令 ω = (-1 + √3i)/2，则 ω³ = 1 且 1 + ω + ω² = 0。\r\n"
                << L"三组配对必须保持乘积 AB = -p/3：\r\n"
                << L"    u₁ = A + B = " << sumText << L"\r\n"
                << L"    u₂ = ωA + ω²B = -(" << sumText << L")/2 + √3·(" << differenceText << L")i/2\r\n"
                << L"    u₃ = ω²A + ωB = -(" << sumText << L")/2 - √3·(" << differenceText << L")i/2\r\n"
                << L"最后由 x = u + (" << shift->text()
                << L") 分别还原，得到方法一中的一实根和一对共轭复根。";
    } else {
        const ExactFraction radiusRadicand(-p->numerator, 3 * p->denominator);
        const std::wstring radiusText = symbolicNumber(std::sqrt(radiusRadicand.value()));
        const long double denominatorRadicandValue =
            -std::pow(static_cast<long double>(p->value()) / 3.0L, 3);
        const auto denominatorRadicand = rationalize(static_cast<double>(denominatorRadicandValue));
        if (!denominatorRadicand)
            return std::nullopt;
        const std::wstring denominatorRootText = symbolicNumber(std::sqrt(denominatorRadicand->value()));
        const ExactFraction negativeQ(-q->numerator, q->denominator);
        const std::wstring argumentText =
            q->isZero() ? L"0" : L"(" + negativeQ.text() + L")/(2·" + denominatorRootText + L")";
        const double argument = -q->value() / (2.0 * std::sqrt(denominatorRadicand->value()));
        const double theta = std::acos(std::clamp(argument, -1.0, 1.0));
        for (int index = 0; index < 3; ++index) {
            std::wstring angle = L"acos(" + argumentText + L")";
            if (index > 0)
                angle += L" + " + std::to_wstring(2 * index) + L"π";
            const std::wstring oscillation = L"2·" + radiusText + L"·cos((" + angle + L")/3)";
            const std::wstring rootText = joinSignedTerms({shiftTerm, {1.0, oscillation}});
            const double rootValue = shift->value() + 2.0 * std::sqrt(radiusRadicand.value()) *
                                                          std::cos((theta + 2.0 * index * kPi) / 3.0);
            appendDisplayRoot(roots, {{rootValue, 0.0}, rootText, false});
        }
        std::sort(roots.begin(), roots.end(),
                  [](const auto& left, const auto& right) { return left.value.real() < right.value.real(); });
        process << L"方法一：三角形式的卡尔达诺公式\r\n"
                << L"令 x = u " << (shift->numerator < 0 ? L"- " : L"+ ")
                << ExactFraction(std::abs(shift->numerator), shift->denominator).text()
                << L"，得到：\r\n    u³ + (" << p->text() << L")u + (" << q->text() << L") = 0\r\n"
                << L"判别量 D = " << discriminant->text() << L" < 0，三个根均为实数。令 θ = acos("
                << argumentText << L")，使用结果中保留的精确余弦形式求根。\r\n"
                << L"    uₖ = 2√(-p/3)·cos((θ + 2kπ)/3)，k = 0, 1, 2\r\n\r\n"
                << L"方法二：三倍角公式\r\n"
                << L"在降幂方程中令 u = 2√(-p/3)·cos φ，代入并提取公因子后得到：\r\n"
                << L"    cos(3φ) = " << argumentText << L"\r\n"
                << L"利用 cos(3φ) = 4cos³φ - 3cosφ，取 θ = acos(" << argumentText << L")，则：\r\n"
                << L"    φ₀ = θ/3，φ₁ = (θ + 2π)/3，φ₂ = (θ + 4π)/3\r\n"
                << L"把三个 φ 依次代回 u，再由 x = u + (" << shift->text()
                << L") 还原，得到与方法一相同的三个实根。";
    }

    std::wstring summary = L"解：";
    for (size_t index = 0; index < roots.size(); ++index) {
        if (index)
            summary += L"，";
        summary += L"x" + subscriptNumber(index + 1) + L" = " + roots[index].text;
    }
    if (rootsOutput)
        *rootsOutput = roots;
    return SolveResult{summary, process.str()};
}

std::optional<SolveResult> solveQuarticByFerrari(const std::wstring& text, const Polynomial& polynomial,
                                                 const std::vector<double>& coefficients,
                                                 std::vector<DisplayRoot>* rootsOutput = nullptr) {
    const auto integer = integerPolynomial(coefficients);
    if (!integer || integer->size() != 5 || integer->back() == 0)
        return std::nullopt;
    const long double a = static_cast<long double>((*integer)[4]);
    const long double b = static_cast<long double>((*integer)[3]);
    const long double c = static_cast<long double>((*integer)[2]);
    const long double d = static_cast<long double>((*integer)[1]);
    const long double e = static_cast<long double>((*integer)[0]);
    const auto p = rationalize(static_cast<double>((8.0L * a * c - 3.0L * b * b) / (8.0L * a * a)));
    const auto q = rationalize(
        static_cast<double>((b * b * b - 4.0L * a * b * c + 8.0L * a * a * d) / (8.0L * a * a * a)));
    const auto r = rationalize(static_cast<double>(
        (-3.0L * b * b * b * b + 16.0L * a * b * b * c - 64.0L * a * a * b * d + 256.0L * a * a * a * e) /
        (256.0L * a * a * a * a)));
    const auto shift = rationalize(static_cast<double>(-b / (4.0L * a)));
    if (!p || !q || !r || !shift)
        return std::nullopt;

    // When q = 0, translating the variable has exposed a biquadratic equation.
    // Keep both the t-roots and the subsequent square roots symbolic, including
    // the case where the intermediate t-roots are complex.
    if (q->isZero()) {
        const ExactRootResult tRoots = exactRoots(1.0, p->value(), r->value());
        if (!tRoots.available || tRoots.texts.empty())
            return std::nullopt;

        std::vector<std::complex<double>> tValues;
        if (tRoots.hasRealRoots) {
            for (double value : tRoots.values)
                tValues.emplace_back(value, 0.0);
        } else {
            const std::complex<double> discriminant(p->value() * p->value() - 4.0 * r->value(), 0.0);
            const std::complex<double> squareRoot = std::sqrt(discriminant);
            tValues = {(-p->value() + squareRoot) / 2.0, (-p->value() - squareRoot) / 2.0};
        }
        if (tValues.size() != tRoots.texts.size())
            return std::nullopt;

        const auto shiftedText = [&](const std::wstring& magnitude, bool plus) {
            if (shift->isZero())
                return plus ? magnitude : L"-" + magnitude;
            return shift->text() + (plus ? L" + " : L" - ") + magnitude;
        };
        std::vector<DisplayRoot> roots;
        for (size_t index = 0; index < tValues.size(); ++index) {
            const std::complex<double> t = tValues[index];
            std::wstring squareRootText;
            if (std::abs(t.imag()) < 1e-10) {
                if (std::abs(t.real()) < 1e-10) {
                    appendDisplayRoot(roots, {{shift->value(), 0.0}, shift->text(), false});
                    continue;
                }
                if (t.real() > 0.0) {
                    squareRootText = exactSquareRootText(t.real(), tRoots.texts[index]);
                } else {
                    squareRootText =
                        exactSquareRootText(-t.real(), L"-(" + tRoots.texts[index] + L")") + L"i";
                }
            } else {
                squareRootText = L"√{" + tRoots.texts[index] + L"}";
            }
            const std::complex<double> root = std::sqrt(t);
            const std::complex<double> shiftValue(shift->value(), 0.0);
            appendDisplayRoot(roots, {shiftValue + root, shiftedText(squareRootText, true), false});
            appendDisplayRoot(roots, {shiftValue - root, shiftedText(squareRootText, false), false});
        }
        std::sort(roots.begin(), roots.end(), [](const auto& left, const auto& right) {
            const bool leftReal = std::abs(left.value.imag()) < 1e-8;
            const bool rightReal = std::abs(right.value.imag()) < 1e-8;
            if (leftReal != rightReal)
                return leftReal;
            if (std::abs(left.value.real() - right.value.real()) > 1e-8)
                return left.value.real() < right.value.real();
            return left.value.imag() < right.value.imag();
        });

        std::wstring summary = L"解：";
        for (size_t index = 0; index < roots.size(); ++index) {
            if (index)
                summary += L"，";
            summary += L"x" + subscriptNumber(index + 1) + L" = " + roots[index].text;
        }
        const std::wstring firstT = tRoots.texts.front();
        const std::wstring secondT = tRoots.texts.size() > 1 ? tRoots.texts[1] : tRoots.texts.front();
        std::wostringstream process;
        process << L"输入原式：" << displayRootNotation(text) << L"\r\n"
                << L"移项并合并同类项：\r\n    " << polynomialText(polynomial) << L" = 0\r\n\r\n"
                << L"方法一：平移换元与双二次换元法\r\n"
                << L"令 x = y " << (shift->numerator < 0 ? L"- " : L"+ ")
                << ExactFraction(std::abs(shift->numerator), shift->denominator).text()
                << L"，原方程化为 y⁴ + py² + r = 0。\r\n"
                << L"    y⁴ + (" << p->text() << L")y² + (" << r->text() << L") = 0\r\n"
                << L"再令 t = y²，精确求解 t² + (" << p->text() << L")t + (" << r->text()
                << L") = 0：\r\n";
        for (size_t index = 0; index < tRoots.texts.size(); ++index)
            process << L"    t" << subscriptNumber(index + 1) << L" = " << tRoots.texts[index] << L"\r\n";
        process << L"对每个 t 解 y² = t，最后由 x = y + (" << shift->text()
                << L") 还原并保留精确根式。\r\n\r\n"
                << L"方法二：平移后因式分解法\r\n"
                << L"根据上面两个 t 根，降幂四次式可直接写成：\r\n"
                << L"    (y² - (" << firstT << L"))(y² - (" << secondT
                << L")) = 0\r\n"
                << L"分别令两个因式为 0：\r\n"
                << L"    y² = " << firstT << L"，或 y² = " << secondT << L"\r\n"
                << L"每个等式各取正、负平方根，再由 x = y + (" << shift->text()
                << L") 还原，所得四个精确根与方法一逐一相同。";
        if (rootsOutput)
            *rootsOutput = roots;
        return SolveResult{summary, process.str()};
    }

    const ExactFraction twiceP(2 * p->numerator, p->denominator);
    const ExactFraction pSquared = *p * *p;
    const ExactFraction fourR(4 * r->numerator, r->denominator);
    const ExactFraction twiceQ(2 * q->numerator, q->denominator);
    const ExactFraction qSquared = *q * *q;
    const std::vector<double> resolventCoefficients{-qSquared.value(), (pSquared - fourR).value(),
                                                    twiceP.value(), 1.0};
    Polynomial resolvent;
    for (int power = 0; power <= 3; ++power) {
        const double coefficient = resolventCoefficients[static_cast<size_t>(power)];
        if (std::abs(coefficient) >= kEpsilon)
            resolvent.terms[{power, 0, 0}] = coefficient;
    }
    std::vector<DisplayRoot> resolventRoots;
    if (!solveCubicByCardano(L"m³+2pm²+(p²-4r)m-q²=0", resolvent, resolventCoefficients, &resolventRoots)) {
        return std::nullopt;
    }
    const auto selected = std::find_if(resolventRoots.begin(), resolventRoots.end(), [](const auto& root) {
        return std::abs(root.value.imag()) < 1e-8 && root.value.real() > 1e-10;
    });
    if (selected == resolventRoots.end())
        return std::nullopt;

    const double m = selected->value.real();
    const std::wstring mText = selected->text;
    const double s = std::sqrt(m);
    const std::wstring sText = exactSquareRootText(m, mText);
    const ExactFraction minusTwoP(-2 * p->numerator, p->denominator);
    const ExactFraction twiceQMagnitude(std::abs(twiceQ.numerator), twiceQ.denominator);
    const std::wstring quotientMagnitude = L"(" + twiceQMagnitude.text() + L")/(" + sText + L")";
    const SignedExactTerm rationalTerm{
        minusTwoP.value(), ExactFraction(std::abs(minusTwoP.numerator), minusTwoP.denominator).text()};
    const SignedExactTerm mTerm{-m, mText};
    const double quotientValue = twiceQ.value() / s;
    const SignedExactTerm quotientTerm{quotientValue, quotientMagnitude};
    const std::vector<SignedExactTerm> firstDiscriminantTerms{rationalTerm, mTerm, quotientTerm};
    const std::vector<SignedExactTerm> secondDiscriminantTerms{
        rationalTerm, mTerm, {-quotientValue, quotientMagnitude}};
    const double firstDiscriminant = minusTwoP.value() - m + quotientValue;
    const double secondDiscriminant = minusTwoP.value() - m - quotientValue;
    const std::wstring firstDiscriminantText = joinSignedTerms(firstDiscriminantTerms);
    const std::wstring secondDiscriminantText = joinSignedTerms(secondDiscriminantTerms);
    const std::wstring firstMagnitudeText = joinSignedTerms({{-rationalTerm.value, rationalTerm.magnitude},
                                                             {-mTerm.value, mTerm.magnitude},
                                                             {-quotientTerm.value, quotientTerm.magnitude}});
    const std::wstring secondMagnitudeText = joinSignedTerms({{-rationalTerm.value, rationalTerm.magnitude},
                                                              {-mTerm.value, mTerm.magnitude},
                                                              {quotientValue, quotientMagnitude}});

    const auto squareRootText = [&](double value, const std::wstring& positiveText,
                                    const std::wstring& negativeText) {
        if (std::abs(value) < 1e-11)
            return std::wstring(L"0");
        if (value > 0.0)
            return exactSquareRootText(value, positiveText);
        return exactSquareRootText(-value, negativeText) + L"i";
    };
    const std::wstring firstSquareRoot =
        squareRootText(firstDiscriminant, firstDiscriminantText, firstMagnitudeText);
    const std::wstring secondSquareRoot =
        squareRootText(secondDiscriminant, secondDiscriminantText, secondMagnitudeText);
    const auto shiftedRootText = [&](const std::wstring& numerator) {
        const std::wstring yText = L"(" + numerator + L")/2";
        return shift->isZero() ? yText : shift->text() + L" + " + yText;
    };

    const std::complex<double> firstRoot = std::sqrt(std::complex<double>(firstDiscriminant, 0.0));
    const std::complex<double> secondRoot = std::sqrt(std::complex<double>(secondDiscriminant, 0.0));
    const std::complex<double> shiftValue(shift->value(), 0.0);
    std::vector<DisplayRoot> roots{
        {shiftValue + (-s + firstRoot) / 2.0, shiftedRootText(L"-" + sText + L" + " + firstSquareRoot),
         false},
        {shiftValue + (-s - firstRoot) / 2.0, shiftedRootText(L"-" + sText + L" - " + firstSquareRoot),
         false},
        {shiftValue + (s + secondRoot) / 2.0, shiftedRootText(sText + L" + " + secondSquareRoot), false},
        {shiftValue + (s - secondRoot) / 2.0, shiftedRootText(sText + L" - " + secondSquareRoot), false}};
    std::sort(roots.begin(), roots.end(), [](const auto& left, const auto& right) {
        const bool leftReal = std::abs(left.value.imag()) < 1e-8;
        const bool rightReal = std::abs(right.value.imag()) < 1e-8;
        if (leftReal != rightReal)
            return leftReal;
        if (std::abs(left.value.real() - right.value.real()) > 1e-8)
            return left.value.real() < right.value.real();
        return left.value.imag() < right.value.imag();
    });

    std::wstring summary = L"解：";
    for (size_t index = 0; index < roots.size(); ++index) {
        if (index)
            summary += L"，";
        summary += L"x" + subscriptNumber(index + 1) + L" = " + roots[index].text;
    }
    std::wostringstream process;
    const std::wstring tFactorText =
        L"((" + p->text() + L") + (" + mText + L") - (" + q->text() + L")/(" + sText + L"))/2";
    const std::wstring uFactorText =
        L"((" + p->text() + L") + (" + mText + L") + (" + q->text() + L")/(" + sText + L"))/2";
    process << L"输入原式：" << displayRootNotation(text) << L"\r\n"
            << L"移项并合并同类项：\r\n    " << polynomialText(polynomial) << L" = 0\r\n\r\n"
            << L"方法一：费拉里法\r\n"
            << L"令 x = y " << (shift->numerator < 0 ? L"- " : L"+ ")
            << ExactFraction(std::abs(shift->numerator), shift->denominator).text()
            << L"，原方程化为 y⁴ + py² + qy + r = 0。\r\n"
            << L"    p = " << p->text() << L"，q = " << q->text() << L"，r = " << r->text()
            << L"\r\n令 m 满足预解三次方程：\r\n"
            << L"    m³ + 2pm² + (p² - 4r)m - q² = 0\r\n"
            << L"取正实根 m = " << mText << L"，再令 s = √m = " << sText
            << L"。\r\n把四次式配成两个二次式后，两个判别式分别为：\r\n"
            << L"    Δ₁ = " << firstDiscriminantText << L"\r\n"
            << L"    Δ₂ = " << secondDiscriminantText << L"\r\n"
            << L"代入二次方程求根公式，并由 x = y + (" << shift->text()
            << L") 还原，得到结果中的四个精确根式。\r\n\r\n"
            << L"方法二：配方并分解为两个二次因式\r\n"
            << L"设 y⁴ + py² + qy + r = (y² + sy + t)(y² - sy + u)。\r\n"
            << L"比较 y²、y 和常数项系数，有：\r\n"
            << L"    t + u - s² = p，s(u - t) = q，tu = r\r\n"
            << L"由 s = " << sText << L" 解得：\r\n"
            << L"    t = " << tFactorText << L"\r\n"
            << L"    u = " << uFactorText << L"\r\n"
            << L"因此原式分解为：\r\n"
            << L"    (y² + (" << sText << L")y + (" << tFactorText << L"))"
            << L"(y² - (" << sText << L")y + (" << uFactorText << L")) = 0\r\n"
            << L"分别解两个二次方程：\r\n"
            << L"    y = (-s ± √Δ₁)/2，或 y = (s ± √Δ₂)/2\r\n"
            << L"最后由 x = y + (" << shift->text() << L") 还原，四个根与费拉里法一致。";
    if (rootsOutput)
        *rootsOutput = roots;
    return SolveResult{summary, process.str()};
}

SolveResult solveHigherDegreeEquation(const std::wstring& text, const Polynomial& polynomial, int degree,
                                      std::vector<DisplayRoot>* rootsOutput = nullptr) {
    ensureUnivariateDegree(polynomial, degree);
    std::vector<double> coefficients(static_cast<size_t>(degree + 1), 0.0);
    for (int power = 0; power <= degree; ++power)
        coefficients[static_cast<size_t>(power)] = polynomial.coefficient(power);
    if (std::abs(coefficients.back()) < kEpsilon)
        throw ParseError("方程最高次项系数不能为 0");

    if (degree == 3 && std::abs(coefficients[2]) < kEpsilon && std::abs(coefficients[1]) < kEpsilon) {
        const auto radicand = rationalize(-coefficients[0] / coefficients[3]);
        const double realRoot = std::cbrt(-coefficients[0] / coefficients[3]);
        if (radicand && !rationalize(realRoot)) {
            const ExactFraction magnitude(std::abs(radicand->numerator), radicand->denominator);
            const std::wstring magnitudeText = simplifiedNthRoot(magnitude, 3);
            const std::wstring realText = realRoot < 0 ? L"-" + magnitudeText : magnitudeText;
            const std::wstring complexReal = (realRoot < 0 ? L"" : L"-") + magnitudeText + L"/2";
            const std::wstring complexImaginary = L"√3·" + magnitudeText + L"/2";
            const std::wstring summary = L"解：x₁ = " + realText + L"，x₂ = " + complexReal + L" - " +
                                         complexImaginary + L"i，x₃ = " + complexReal + L" + " +
                                         complexImaginary + L"i";
            std::wostringstream process;
            process << L"输入原式：" << displayRootNotation(text) << L"\r\n"
                    << L"移项并合并同类项：\r\n    " << polynomialText(polynomial) << L" = 0\r\n\r\n"
                    << L"方法一：开立方与单位根法\r\n"
                    << L"先移项并除以三次项系数，得到 x³ = " << radicand->text() << L"。\r\n"
                    << L"令 r = " << realText << L"，则 r³ = " << radicand->text() << L"。\r\n"
                    << L"三个三次单位根为 1、(-1 + √3i)/2、(-1 - √3i)/2，"
                    << L"分别乘以 r 得到全部三个根。\r\n\r\n"
                    << L"方法二：因式分解法\r\n"
                    << L"把方程写成 x³ - r³ = 0，并使用立方差公式：\r\n"
                    << L"    (x - (" << realText << L"))(x² + (" << realText << L")x + (" << realText
                    << L")²) = 0\r\n"
                    << L"第一个因式给出 x = " << realText << L"。\r\n"
                    << L"第二个因式的判别式为 Δ = -3r²，代入求根公式：\r\n"
                    << L"    x = -r/2 ± (√3·r/2)i\r\n"
                    << L"化简后得到与方法一相同的一对共轭复根。";
            if (rootsOutput) {
                *rootsOutput = {{{realRoot, 0.0}, realText, false},
                                {{-realRoot / 2.0, -std::sqrt(3.0) * std::abs(realRoot) / 2.0},
                                 complexReal + L" - " + complexImaginary + L"i",
                                 false},
                                {{-realRoot / 2.0, std::sqrt(3.0) * std::abs(realRoot) / 2.0},
                                 complexReal + L" + " + complexImaginary + L"i",
                                 false}};
            }
            return {summary, process.str()};
        }
    }

    if (degree == 4 && std::abs(coefficients[3]) < kEpsilon && std::abs(coefficients[1]) < kEpsilon) {
        const ExactRootResult tRoots = exactRoots(coefficients[4], coefficients[2], coefficients[0]);
        if (tRoots.available && tRoots.hasRealRoots && !tRoots.texts.empty()) {
            const ExactRootResult negativeTRoots =
                exactRoots(coefficients[4], -coefficients[2], coefficients[0]);
            std::vector<DisplayRoot> roots;
            for (size_t i = 0; i < tRoots.values.size(); ++i) {
                const double t = tRoots.values[i];
                if (std::abs(t) < kEpsilon) {
                    appendDisplayRoot(roots, {{0.0, 0.0}, L"0", false});
                } else if (t > 0) {
                    const std::wstring value = exactSquareRootText(t, tRoots.texts[i]);
                    appendDisplayRoot(roots, {{std::sqrt(t), 0.0}, value, false});
                    appendDisplayRoot(roots, {{-std::sqrt(t), 0.0}, L"-" + value, false});
                } else {
                    std::wstring magnitudeText;
                    for (size_t negativeIndex = 0; negativeIndex < negativeTRoots.values.size();
                         ++negativeIndex) {
                        if (std::abs(negativeTRoots.values[negativeIndex] + t) < 1e-8) {
                            magnitudeText = negativeTRoots.texts[negativeIndex];
                            break;
                        }
                    }
                    if (magnitudeText.empty())
                        magnitudeText = L"-(" + tRoots.texts[i] + L")";
                    const std::wstring value = exactSquareRootText(-t, magnitudeText) + L"i";
                    appendDisplayRoot(roots, {{0.0, std::sqrt(-t)}, value, false});
                    appendDisplayRoot(roots, {{0.0, -std::sqrt(-t)}, L"-" + value, false});
                }
            }
            std::sort(roots.begin(), roots.end(), [](const auto& left, const auto& right) {
                if (std::abs(left.value.real() - right.value.real()) > 1e-8)
                    return left.value.real() < right.value.real();
                return left.value.imag() < right.value.imag();
            });
            std::wstring summary = L"解：";
            for (size_t i = 0; i < roots.size(); ++i) {
                if (i)
                    summary += L"，";
                summary += L"x" + subscriptNumber(i + 1) + L" = " + roots[i].text;
            }
            const std::wstring firstT = tRoots.texts.front();
            const std::wstring secondT = tRoots.texts.size() > 1 ? tRoots.texts[1] : tRoots.texts.front();
            std::wostringstream process;
            process << L"输入原式：" << displayRootNotation(text) << L"\r\n"
                    << L"移项并合并同类项：\r\n    " << polynomialText(polynomial) << L" = 0\r\n\r\n"
                    << L"方法一：换元法\r\n"
                    << L"令 t = x²，原方程化为关于 t 的一元二次方程：\r\n    "
                    << symbolicNumber(coefficients[4]) << L"t² + " << symbolicNumber(coefficients[2])
                    << L"t + " << symbolicNumber(coefficients[0]) << L" = 0\r\n"
                    << tRoots.calculation << L"\r\n再由 x² = t 求出上述各根。\r\n\r\n"
                    << L"方法二：因式分解法\r\n"
                    << L"由方法一算得的两个 t 根，四次多项式可写为：\r\n"
                    << L"    " << symbolicNumber(coefficients[4]) << L"(x² - (" << firstT
                    << L"))(x² - (" << secondT << L")) = 0\r\n"
                    << L"根据零乘积性质，分别解：\r\n"
                    << L"    x² = " << firstT << L"，或 x² = " << secondT << L"\r\n"
                    << L"对非负 t 取 x = ±√t；对负 t 取 x = ±√(-t)i，"
                    << L"化简后所得根与换元法一致。";
            if (rootsOutput)
                *rootsOutput = roots;
            return {summary, process.str()};
        }
    }

    std::vector<double> remaining = coefficients;
    std::vector<ExactFraction> rationalFactors;
    struct SyntheticStep {
        ExactFraction root;
        std::vector<double> dividend;
        std::vector<double> quotient;
    };
    std::vector<SyntheticStep> syntheticSteps;
    std::vector<DisplayRoot> roots;
    while (remaining.size() > 3) {
        const auto rationalRoot = rationalPolynomialRoot(remaining);
        if (!rationalRoot)
            break;
        rationalFactors.push_back(*rationalRoot);
        appendDisplayRoot(roots, {{rationalRoot->value(), 0.0}, rationalRoot->text(), false});
        std::vector<double> quotient = syntheticDivide(remaining, rationalRoot->value());
        syntheticSteps.push_back({*rationalRoot, remaining, quotient});
        remaining = std::move(quotient);
        while (remaining.size() > 1 && std::abs(remaining.back()) < kEpsilon)
            remaining.pop_back();
    }
    if (degree == 3 && rationalFactors.empty()) {
        if (const auto exactCardano = solveCubicByCardano(text, polynomial, coefficients, rootsOutput))
            return *exactCardano;
    }
    if (degree == 4 && rationalFactors.empty()) {
        if (const auto exactFerrari = solveQuarticByFerrari(text, polynomial, coefficients, rootsOutput))
            return *exactFerrari;
    }

    bool exactRemainder = true;
    const int remainingDegree = static_cast<int>(remaining.size()) - 1;
    if (remainingDegree == 2) {
        const ExactRootResult exact = exactRoots(remaining[2], remaining[1], remaining[0]);
        if (exact.available && !exact.texts.empty()) {
            if (exact.hasRealRoots) {
                for (size_t i = 0; i < exact.texts.size(); ++i)
                    appendDisplayRoot(roots, {{exact.values[i], 0.0}, exact.texts[i], false});
            } else {
                const double real = -remaining[1] / (2.0 * remaining[2]);
                const double imaginary =
                    std::sqrt(-(remaining[1] * remaining[1] - 4.0 * remaining[2] * remaining[0])) /
                    std::abs(2.0 * remaining[2]);
                appendDisplayRoot(roots, {{real, imaginary}, exact.texts[0], false});
                appendDisplayRoot(roots, {{real, -imaginary}, exact.texts[1], false});
            }
        } else {
            exactRemainder = false;
        }
    } else if (remainingDegree == 1) {
        const ExactRootResult exact = exactRoots(0.0, remaining[1], remaining[0]);
        if (exact.available && !exact.texts.empty())
            appendDisplayRoot(roots, {{exact.values.front(), 0.0}, exact.texts.front(), false});
        else
            exactRemainder = false;
    } else if (remainingDegree > 0) {
        exactRemainder = false;
    }
    if (!exactRemainder) {
        for (const auto root : complexPolynomialRoots(remaining))
            appendDisplayRoot(roots, {root, approximateComplex(root), true});
    }
    std::sort(roots.begin(), roots.end(), [](const auto& left, const auto& right) {
        const bool leftReal = std::abs(left.value.imag()) < 1e-8;
        const bool rightReal = std::abs(right.value.imag()) < 1e-8;
        if (leftReal != rightReal)
            return leftReal;
        if (std::abs(left.value.real() - right.value.real()) > 1e-8)
            return left.value.real() < right.value.real();
        return left.value.imag() < right.value.imag();
    });

    std::wstring summary = L"解：";
    for (size_t i = 0; i < roots.size(); ++i) {
        if (i)
            summary += L"，";
        summary += L"x" + subscriptNumber(i + 1) + (roots[i].approximate ? L" ≈ " : L" = ") + roots[i].text;
    }
    std::wostringstream process;
    process << L"输入原式：" << displayRootNotation(text) << L"\r\n"
            << L"移项并合并同类项：\r\n    " << polynomialText(polynomial) << L" = 0\r\n\r\n";
    if (!rationalFactors.empty()) {
        process << L"方法一：有理根定理与因式分解法\r\n";
        for (const auto& root : rationalFactors)
            process << L"检验得到有理根 x = " << root.text() << L"，提取因式 (x - (" << root.text()
                    << L"))。\r\n";
        process << L"继续分解剩余低次多项式，得到结果中的全部根。\r\n\r\n"
                << L"方法二：综合除法与低次方程公式\r\n"
                << L"按降幂顺序写出系数，并对每个有理根执行“首项落下、乘根、与下一项相加”：\r\n";
        for (size_t index = 0; index < syntheticSteps.size(); ++index) {
            const auto& step = syntheticSteps[index];
            process << L"第 " << index + 1 << L" 次综合除法，使用根 r = " << step.root.text() << L"：\r\n"
                    << L"    " << coefficientPolynomialText(step.dividend) << L"\r\n"
                    << L"    = (x - (" << step.root.text() << L"))("
                    << coefficientPolynomialText(step.quotient) << L")\r\n";
        }
        process << L"降次后得到 " << coefficientPolynomialText(remaining) << L" = 0。\r\n";
        if (remainingDegree == 2)
            process << L"对这个二次方程计算判别式并使用求根公式；\r\n";
        else if (remainingDegree == 1)
            process << L"对这个一次方程移项并除以 x 的系数；\r\n";
        process << L"合并综合除法得到的根与低次方程的根，即为原方程的全部根。\r\n";
    } else {
        process << L"使用 Durand-Kerner 数值迭代同时求出多项式的全部实根和复数根。\r\n";
    }
    process << L"将各根代回标准多项式验算，残差均在允许误差内。";
    if (rootsOutput)
        *rootsOutput = roots;
    return {summary, process.str()};
}

// ---------- 二元二次方程组：Sylvester 结式、回代和实数验根 ----------

using UniPolynomial = std::vector<long double>;

void cleanUniPolynomial(UniPolynomial& polynomial) {
    while (!polynomial.empty() && std::abs(polynomial.back()) < 1e-14L)
        polynomial.pop_back();
}

UniPolynomial addUniPolynomial(UniPolynomial left, const UniPolynomial& right, long double scale = 1.0L) {
    left.resize(std::max(left.size(), right.size()), 0.0L);
    for (size_t i = 0; i < right.size(); ++i)
        left[i] += scale * right[i];
    cleanUniPolynomial(left);
    return left;
}

UniPolynomial multiplyUniPolynomial(const UniPolynomial& left, const UniPolynomial& right) {
    if (left.empty() || right.empty())
        return {};
    UniPolynomial result(left.size() + right.size() - 1, 0.0L);
    for (size_t i = 0; i < left.size(); ++i)
        for (size_t j = 0; j < right.size(); ++j)
            result[i + j] += left[i] * right[j];
    cleanUniPolynomial(result);
    return result;
}

UniPolynomial coefficientInY(const Polynomial& polynomial, int yPower) {
    UniPolynomial result;
    for (const auto& [powers, coefficient] : polynomial.terms) {
        if (powers[1] != yPower || powers[2] != 0)
            continue;
        result.resize(std::max(result.size(), static_cast<size_t>(powers[0] + 1)), 0.0L);
        result[static_cast<size_t>(powers[0])] += coefficient;
    }
    cleanUniPolynomial(result);
    return result;
}

UniPolynomial resultantInY(const Polynomial& first, const Polynomial& second) {
    const int firstDegree = first.degree(1);
    const int secondDegree = second.degree(1);
    if (firstDegree == 0 && secondDegree == 0)
        return {};
    const int size = firstDegree + secondDegree;
    std::vector<std::vector<UniPolynomial>> matrix(static_cast<size_t>(size),
                                                   std::vector<UniPolynomial>(static_cast<size_t>(size)));
    std::vector<UniPolynomial> firstCoefficients;
    std::vector<UniPolynomial> secondCoefficients;
    for (int power = firstDegree; power >= 0; --power)
        firstCoefficients.push_back(coefficientInY(first, power));
    for (int power = secondDegree; power >= 0; --power)
        secondCoefficients.push_back(coefficientInY(second, power));
    for (int row = 0; row < secondDegree; ++row)
        for (size_t column = 0; column < firstCoefficients.size(); ++column)
            matrix[static_cast<size_t>(row)][static_cast<size_t>(row) + column] = firstCoefficients[column];
    for (int row = 0; row < firstDegree; ++row)
        for (size_t column = 0; column < secondCoefficients.size(); ++column)
            matrix[static_cast<size_t>(secondDegree + row)][static_cast<size_t>(row) + column] =
                secondCoefficients[column];

    UniPolynomial determinant;
    std::vector<int> permutation(static_cast<size_t>(size), -1);
    std::vector<bool> used(static_cast<size_t>(size), false);
    const auto expand = [&](const auto& self, int row) -> void {
        if (row == size) {
            int inversions = 0;
            for (int i = 0; i < size; ++i)
                for (int j = i + 1; j < size; ++j)
                    if (permutation[static_cast<size_t>(i)] > permutation[static_cast<size_t>(j)])
                        ++inversions;
            UniPolynomial term{1.0L};
            for (int i = 0; i < size; ++i)
                term = multiplyUniPolynomial(
                    term, matrix[static_cast<size_t>(i)][static_cast<size_t>(permutation[i])]);
            determinant = addUniPolynomial(std::move(determinant), term, inversions % 2 ? -1.0L : 1.0L);
            return;
        }
        for (int column = 0; column < size; ++column) {
            if (used[static_cast<size_t>(column)])
                continue;
            used[static_cast<size_t>(column)] = true;
            permutation[static_cast<size_t>(row)] = column;
            self(self, row + 1);
            used[static_cast<size_t>(column)] = false;
        }
    };
    expand(expand, 0);
    cleanUniPolynomial(determinant);
    return determinant;
}

std::wstring uniPolynomialText(const UniPolynomial& polynomial) {
    if (polynomial.empty())
        return L"0";
    std::wstring result;
    bool first = true;
    for (int power = static_cast<int>(polynomial.size()) - 1; power >= 0; --power) {
        const double coefficient = static_cast<double>(polynomial[static_cast<size_t>(power)]);
        if (std::abs(coefficient) < 1e-10)
            continue;
        const bool negative = coefficient < 0;
        const double magnitude = std::abs(coefficient);
        if (first) {
            if (negative)
                result += L"-";
        } else {
            result += negative ? L" - " : L" + ";
        }
        if (power == 0 || std::abs(magnitude - 1.0) >= kEpsilon)
            result += symbolicNumber(magnitude);
        if (power > 0) {
            result += L"x";
            if (power > 1)
                result += L"^" + std::to_wstring(power);
        }
        first = false;
    }
    return result.empty() ? L"0" : result;
}

std::vector<double> yCandidates(const Polynomial& polynomial, double x, bool& identity) {
    const double a = polynomial.coefficient(0, 2);
    const double b = polynomial.coefficient(0, 1) + polynomial.coefficient(1, 1) * x;
    const double c = polynomial.coefficient(0, 0) + polynomial.coefficient(1, 0) * x +
                     polynomial.coefficient(2, 0) * x * x;
    identity = false;
    if (std::abs(a) >= 1e-9) {
        const double delta = b * b - 4.0 * a * c;
        if (delta < -1e-8)
            return {};
        const double root = std::sqrt(std::max(0.0, delta));
        std::vector<double> values{(-b + root) / (2.0 * a)};
        if (root > 1e-8)
            values.push_back((-b - root) / (2.0 * a));
        return values;
    }
    if (std::abs(b) >= 1e-9)
        return {-c / b};
    identity = std::abs(c) < 1e-7;
    return {};
}

bool satisfiesPolynomial(const Polynomial& polynomial, double x, double y) {
    double scale = 1.0;
    for (const auto& [powers, coefficient] : polynomial.terms)
        scale += std::abs(coefficient) * std::pow(std::max(1.0, std::abs(x)), powers[0]) *
                 std::pow(std::max(1.0, std::abs(y)), powers[1]);
    return std::abs(polynomial.evaluate(x, y)) <= 1e-7 * scale;
}

struct SystemPoint {
    double x = 0.0;
    double y = 0.0;
};

void appendSystemPoint(std::vector<SystemPoint>& points, double x, double y) {
    for (const auto& point : points)
        if (std::hypot(point.x - x, point.y - y) <= 1e-7 * std::max({1.0, std::abs(x), std::abs(y)}))
            return;
    points.push_back({x, y});
}

std::pair<std::wstring, bool> coordinateText(double value) {
    if (const auto fraction = rationalize(value))
        return {fraction->text(), false};
    return {approximateNumber(value), true};
}

struct ExactSystemPointText {
    std::optional<std::wstring> x;
    std::optional<std::wstring> y;
};

std::optional<ExactFraction> nearbySimpleFraction(double value) {
    if (!std::isfinite(value))
        return std::nullopt;
    constexpr std::int64_t maxDenominator = 10000;
    constexpr double tolerance = 1e-7;
    for (std::int64_t denominator = 1; denominator <= maxDenominator; ++denominator) {
        const long double scaled = static_cast<long double>(value) * denominator;
        if (scaled > static_cast<long double>(std::numeric_limits<std::int64_t>::max()) ||
            scaled < static_cast<long double>(std::numeric_limits<std::int64_t>::min())) {
            return std::nullopt;
        }
        const auto numerator = static_cast<std::int64_t>(std::llround(scaled));
        const double candidate = static_cast<double>(numerator) / denominator;
        if (std::abs(candidate - value) <= tolerance * std::max(1.0, std::abs(value)))
            return ExactFraction(numerator, denominator);
    }
    return std::nullopt;
}

bool verifiedRationalPoint(const Polynomial& polynomial, const ExactFraction& x,
                           const ExactFraction& y) {
    const long double xValue = static_cast<long double>(x.numerator) / x.denominator;
    const long double yValue = static_cast<long double>(y.numerator) / y.denominator;
    long double value = 0.0L;
    long double scale = 1.0L;
    for (const auto& [powers, coefficient] : polynomial.terms) {
        const auto exactCoefficient = rationalize(coefficient);
        if (!exactCoefficient)
            return false;
        long double term = static_cast<long double>(exactCoefficient->numerator) /
                           exactCoefficient->denominator;
        for (int power = 0; power < powers[0]; ++power)
            term *= xValue;
        for (int power = 0; power < powers[1]; ++power)
            term *= yValue;
        value += term;
        scale += std::abs(term);
    }
    return std::abs(value) <= 1e-15L * scale;
}

void fillVerifiedRationalPointTexts(const std::array<Polynomial, 2>& equations,
                                    const std::vector<SystemPoint>& points,
                                    std::vector<ExactSystemPointText>& texts) {
    for (size_t index = 0; index < points.size(); ++index) {
        const auto x = nearbySimpleFraction(points[index].x);
        const auto y = nearbySimpleFraction(points[index].y);
        if (!x || !y || !verifiedRationalPoint(equations[0], *x, *y) ||
            !verifiedRationalPoint(equations[1], *x, *y)) {
            continue;
        }
        texts[index].x = x->text();
        texts[index].y = y->text();
    }
}

std::optional<Polynomial> linearRelationForY(const std::array<Polynomial, 2>& equations) {
    for (const auto& equation : equations) {
        if (equation.totalDegree() <= 1 && std::abs(equation.coefficient(0, 1)) >= kEpsilon)
            return equation;
    }

    const std::array<Powers, 3> quadraticTerms{{{2, 0, 0}, {1, 1, 0}, {0, 2, 0}}};
    for (const Powers& powers : quadraticTerms) {
        const double firstCoefficient =
            equations[0].coefficient(powers[0], powers[1], powers[2]);
        const double secondCoefficient =
            equations[1].coefficient(powers[0], powers[1], powers[2]);
        if (std::abs(firstCoefficient) < kEpsilon || std::abs(secondCoefficient) < kEpsilon)
            continue;
        Polynomial relation =
            add(multiply(Polynomial::constant(secondCoefficient), equations[0]),
                multiply(Polynomial::constant(firstCoefficient), equations[1]), -1.0);
        relation.clean();
        if (relation.totalDegree() <= 1 && std::abs(relation.coefficient(0, 1)) >= kEpsilon)
            return relation;
    }
    return std::nullopt;
}

std::vector<ExactSystemPointText> exactSystemPointTexts(
    const UniPolynomial& resultant, const std::array<Polynomial, 2>& equations,
    const std::vector<SystemPoint>& points) {
    std::vector<ExactSystemPointText> texts(points.size());
    if (resultant.empty() || resultant.size() > 3)
        return texts;

    const double a = resultant.size() > 2 ? static_cast<double>(resultant[2]) : 0.0;
    const double b = resultant.size() > 1 ? static_cast<double>(resultant[1]) : 0.0;
    const double c = static_cast<double>(resultant[0]);
    const ExactRootResult roots = exactRoots(a, b, c);
    if (!roots.available || !roots.hasRealRoots || roots.expressions.empty())
        return texts;

    std::optional<ExactFraction> xScale;
    std::optional<ExactFraction> constant;
    if (const auto relation = linearRelationForY(equations)) {
        const auto xCoefficient = rationalize(relation->coefficient(1, 0));
        const auto yCoefficient = rationalize(relation->coefficient(0, 1));
        const auto constantCoefficient = rationalize(relation->coefficient(0, 0));
        if (xCoefficient && yCoefficient && constantCoefficient && !yCoefficient->isZero()) {
            const ExactFraction negativeX(-xCoefficient->numerator, xCoefficient->denominator);
            const ExactFraction negativeConstant(-constantCoefficient->numerator,
                                                 constantCoefficient->denominator);
            xScale = negativeX / *yCoefficient;
            constant = negativeConstant / *yCoefficient;
        }
    }

    for (size_t pointIndex = 0; pointIndex < points.size(); ++pointIndex) {
        const ExactRootExpression* closest = nullptr;
        double closestDistance = std::numeric_limits<double>::infinity();
        for (const ExactRootExpression& expression : roots.expressions) {
            const double distance = std::abs(expression.value() - points[pointIndex].x);
            if (distance < closestDistance) {
                closestDistance = distance;
                closest = &expression;
            }
        }
        if (!closest || closestDistance >
                            1e-7 * std::max(1.0, std::abs(points[pointIndex].x))) {
            continue;
        }

        texts[pointIndex].x = closest->text();
        if (xScale && constant) {
            ExactRootExpression yExpression;
            yExpression.rational = *xScale * closest->rational + *constant;
            yExpression.radicalCoefficient = *xScale * closest->radicalCoefficient;
            yExpression.radicand = closest->radicand;
            if (std::abs(yExpression.value() - points[pointIndex].y) <=
                1e-7 * std::max(1.0, std::abs(points[pointIndex].y))) {
                texts[pointIndex].y = yExpression.text();
            }
        }
    }
    return texts;
}

SolveResult solveQuadraticSystem(const std::vector<std::wstring>& lines, AngleUnit angleUnit) {
    if (lines.size() != 2)
        throw ParseError("二元二次方程组需要输入两行方程");
    std::array<Polynomial, 2> equations;
    for (size_t i = 0; i < lines.size(); ++i) {
        Parser parser(lines[i], angleUnit);
        const ParsedRelation relation = parser.parseRelation();
        if (relation.relation != TokenType::Equal)
            throw ParseError("方程组中的每一行都必须使用等号 =");
        if (parser.fractionalScale() > 1)
            throw ParseError("二元二次方程组暂不支持分数指数");
        const RationalExpression difference = add(relation.left, relation.right, -1.0);
        if (!difference.denominator.isConstant())
            throw ParseError("二元二次方程组暂不支持未知数位于分母");
        equations[i] = difference.numerator;
        if (equations[i].usesVariable(2))
            throw ParseError("二元二次方程组只能包含两个未知数");
        if (equations[i].totalDegree() > 2)
            throw ParseError("二元二次方程组中每个方程的总次数不能超过 2");
    }
    if (!equations[0].usesVariable(1) && !equations[1].usesVariable(1))
        throw ParseError("二元二次方程组必须包含两个未知数");
    if (equations[0].totalDegree() <= 1 && equations[1].totalDegree() <= 1) {
        std::array<std::array<double, 3>, 2> linear{};
        for (size_t i = 0; i < 2; ++i)
            linear[i] = {equations[i].coefficient(1, 0), equations[i].coefficient(0, 1),
                         -equations[i].coefficient(0, 0)};
        SolveResult result = solveLinear2(linear);
        result.process = L"输入方程组：\r\n    " + lines[0] + L"\r\n    " + lines[1] +
                         L"\r\n\r\n化简后两个方程均为一次方程，转入二元一次方程组求解。\r\n\r\n" +
                         result.process;
        return result;
    }

    UniPolynomial resultant = resultantInY(equations[0], equations[1]);
    if (resultant.empty()) {
        std::wstring process = L"输入方程组：\r\n    " + lines[0] + L"\r\n    " + lines[1] +
                               L"\r\n\r\n消元所得结式恒为 0，两个方程存在公共因式或公共曲线。";
        return {L"结论：方程组有无穷多个实数解", process};
    }
    std::vector<long double> xRoots;
    if (resultant.size() <= 3) {
        const double a = resultant.size() > 2 ? static_cast<double>(resultant[2]) : 0.0;
        const double b = resultant.size() > 1 ? static_cast<double>(resultant[1]) : 0.0;
        const ExactRootResult exactResultantRoots =
            exactRoots(a, b, static_cast<double>(resultant[0]));
        if (exactResultantRoots.available && exactResultantRoots.hasRealRoots &&
            !exactResultantRoots.expressions.empty()) {
            for (const ExactRootExpression& root : exactResultantRoots.expressions) {
                const long double rational = static_cast<long double>(root.rational.numerator) /
                                             root.rational.denominator;
                const long double radical =
                    static_cast<long double>(root.radicalCoefficient.numerator) /
                    root.radicalCoefficient.denominator *
                    std::sqrt(static_cast<long double>(root.radicand));
                xRoots.push_back(rational + radical);
            }
        }
    }
    if (xRoots.empty()) {
        std::vector<long double> resultantCoefficients = resultant;
        xRoots = realPolynomialRoots(std::move(resultantCoefficients));
    }
    std::vector<SystemPoint> points;
    bool hasVerticalFamily = false;
    for (const long double xRoot : xRoots) {
        const double x = static_cast<double>(xRoot);
        bool firstIdentity = false;
        bool secondIdentity = false;
        std::vector<double> candidates = yCandidates(equations[0], x, firstIdentity);
        if (firstIdentity)
            candidates = yCandidates(equations[1], x, secondIdentity);
        if (firstIdentity && secondIdentity) {
            hasVerticalFamily = true;
            continue;
        }
        for (const double y : candidates)
            if (satisfiesPolynomial(equations[0], x, y) && satisfiesPolynomial(equations[1], x, y))
                appendSystemPoint(points, x, y);
    }
    if (hasVerticalFamily)
        return {L"结论：方程组有无穷多个实数解", L"消元后出现整条竖直公共解集，因此实数解有无穷多个。"};
    if (points.empty()) {
        std::wostringstream process;
        process << L"输入方程组：\r\n    " << lines[0] << L"\r\n    " << lines[1]
                << L"\r\n\r\n方法一：结式消元法\r\n"
                << L"消去 y 得 R(x) = " << uniPolynomialText(resultant)
                << L" = 0。\r\n该结式没有产生同时满足两个原方程的实数坐标。";
        return {L"结论：二元二次方程组无实数解", process.str()};
    }
    std::sort(points.begin(), points.end(), [](const auto& left, const auto& right) {
        if (std::abs(left.x - right.x) > 1e-8)
            return left.x < right.x;
        return left.y < right.y;
    });
    std::vector<ExactSystemPointText> exactCoordinates =
        exactSystemPointTexts(resultant, equations, points);
    fillVerifiedRationalPointTexts(equations, points, exactCoordinates);
    std::wstring summary = L"实数解：";
    for (size_t i = 0; i < points.size(); ++i) {
        if (i)
            summary += L"；";
        const auto [fallbackXText, xApproximate] = coordinateText(points[i].x);
        const auto [fallbackYText, yApproximate] = coordinateText(points[i].y);
        const std::wstring xText = exactCoordinates[i].x.value_or(fallbackXText);
        const std::wstring yText = exactCoordinates[i].y.value_or(fallbackYText);
        const std::wstring suffix = points.size() > 1 ? subscriptNumber(i + 1) : L"";
        summary += L"x" + suffix + (exactCoordinates[i].x || !xApproximate ? L" = " : L" ≈ ") +
                   xText + L"，y" + suffix +
                   (exactCoordinates[i].y || !yApproximate ? L" = " : L" ≈ ") + yText;
    }

    Polynomial reducedRelation;
    std::wstring eliminationDescription;
    const std::array<std::pair<Powers, const wchar_t*>, 3> quadraticTerms{
        {{{2, 0, 0}, L"x²"}, {{1, 1, 0}, L"xy"}, {{0, 2, 0}, L"y²"}}};
    for (const auto& [powers, label] : quadraticTerms) {
        const double firstCoefficient =
            equations[0].coefficient(powers[0], powers[1], powers[2]);
        const double secondCoefficient =
            equations[1].coefficient(powers[0], powers[1], powers[2]);
        if (std::abs(firstCoefficient) < kEpsilon || std::abs(secondCoefficient) < kEpsilon)
            continue;
        reducedRelation =
            add(multiply(Polynomial::constant(secondCoefficient), equations[0]),
                multiply(Polynomial::constant(firstCoefficient), equations[1]), -1.0);
        if (reducedRelation.isZero())
            continue;
        eliminationDescription =
            L"用 (" + symbolicNumber(secondCoefficient) + L")×(1) - (" +
            symbolicNumber(firstCoefficient) + L")×(2)，消去 " + label + L" 项";
        break;
    }

    std::wostringstream process;
    process << L"输入方程组：\r\n    " << lines[0] << L"\r\n    " << lines[1]
            << L"\r\n\r\n方法一：结式消元法\r\n"
            << L"把两个方程都看成关于 y 的二次多项式，计算结式并消去 y：\r\n    R(x) = "
            << uniPolynomialText(resultant) << L" = 0\r\n"
            << L"解出所有实数 x，再逐一回代求 y，并舍去不同时满足两式的候选点。\r\n\r\n"
            << L"方法二：加减消元与代入法\r\n"
            << L"先把两个方程写成左边为多项式、右边为 0 的标准式：\r\n"
            << L"    (1) " << polynomialText(equations[0]) << L" = 0\r\n"
            << L"    (2) " << polynomialText(equations[1]) << L" = 0\r\n";
    if (!eliminationDescription.empty()) {
        process << eliminationDescription << L"：\r\n"
                << L"    " << polynomialText(reducedRelation) << L" = 0\r\n"
                << L"这就是少一个二次项的降次关系。把它与 (1) 联立，先求一个未知数，再代入求另一个。\r\n";
    } else {
        process << L"选择只在一个方程中出现的二次项，从另一个方程解出较简单的关系并代入。\r\n";
    }
    for (size_t i = 0; i < points.size(); ++i) {
        const auto [fallbackXText, xApproximate] = coordinateText(points[i].x);
        const auto [fallbackYText, yApproximate] = coordinateText(points[i].y);
        const std::wstring xText = exactCoordinates[i].x.value_or(fallbackXText);
        const std::wstring yText = exactCoordinates[i].y.value_or(fallbackYText);
        process << L"第 " << i + 1 << L" 组：取 x "
                << (exactCoordinates[i].x || !xApproximate ? L"= " : L"≈ ") << xText
                << L"，代入降次关系及 (1)，解得 y "
                << (exactCoordinates[i].y || !yApproximate ? L"= " : L"≈ ") << yText
                << L"。\r\n"
                << L"验算第 " << i + 1 << L" 组解：代入两个原方程，左右两边均相等。\r\n";
    }
    process << L"全部候选点均已完成回代筛选，所得坐标与结式法一致。\r\n";
    return {summary, process.str()};
}

// ---------- 绝对值、分数指数与三角方程专用路径 ----------

struct AbsoluteSegment {
    size_t begin = 0;
    size_t end = 0;
    std::wstring content;
    RationalExpression expression;
};

std::vector<AbsoluteSegment> absoluteSegments(const std::wstring& text, AngleUnit angleUnit) {
    std::vector<AbsoluteSegment> segments;
    size_t opening = std::wstring::npos;
    for (size_t i = 0; i < text.size(); ++i) {
        if (text[i] == L'"') {
            const size_t firstQuote = text.find(L'"', i + 1);
            if (firstQuote == std::wstring::npos)
                break;
            const std::wstring firstPart = text.substr(i + 1, firstQuote - i - 1);
            wchar_t* end = nullptr;
            const long possibleDegree = std::wcstol(firstPart.c_str(), &end, 10);
            const bool isDegree = end != firstPart.c_str() && end && *end == L'\0' && possibleDegree >= 2 &&
                                  possibleDegree <= 16;
            if (isDegree) {
                const size_t secondQuote = text.find(L'"', firstQuote + 1);
                if (secondQuote != std::wstring::npos) {
                    i = secondQuote;
                    continue;
                }
            }
            i = firstQuote;
            continue;
        }
        if (text[i] != L'|')
            continue;
        if (opening == std::wstring::npos) {
            opening = i;
        } else {
            if (i == opening + 1)
                throw ParseError("绝对值符号 || 中间不能为空");
            const std::wstring content = text.substr(opening + 1, i - opening - 1);
            Parser parser(content + L"=0", angleUnit);
            const ParsedRelation parsed = parser.parseRelation();
            if (parser.fractionalScale() > 1) {
                throw ParseError("绝对值内部暂不支持含未知数的分数指数");
            }
            segments.push_back({opening, i, content, parsed.left});
            opening = std::wstring::npos;
        }
    }
    if (opening != std::wstring::npos)
        throw ParseError("绝对值缺少右侧竖线 |");
    if (segments.size() > 4)
        throw ParseError("一条方程最多支持 4 组绝对值");
    return segments;
}

std::wstring expandAbsoluteBranch(const std::wstring& text, const std::vector<AbsoluteSegment>& segments,
                                  unsigned branch) {
    std::wstring result;
    size_t cursor = 0;
    for (size_t i = 0; i < segments.size(); ++i) {
        const auto& segment = segments[i];
        result += text.substr(cursor, segment.begin - cursor);
        if ((branch & (1u << i)) == 0)
            result += L"(" + segment.content + L")";
        else
            result += L"(-1*(" + segment.content + L"))";
        cursor = segment.end + 1;
    }
    result += text.substr(cursor);
    return result;
}

double evaluateRational(const RationalExpression& expression, double x) {
    const double denominator = expression.denominator.evaluate(x);
    if (std::abs(denominator) < kEpsilon)
        return std::numeric_limits<double>::quiet_NaN();
    return expression.numerator.evaluate(x) / denominator;
}

struct AbsoluteSolution {
    double value = 0.0;
    std::wstring text;
    bool approximate = false;
};

SolveResult solveAbsoluteEquation(const std::wstring& text, AngleUnit angleUnit) {
    const auto segments = absoluteSegments(text, angleUnit);
    if (segments.empty())
        throw ParseError("原式中没有完整的绝对值符号 | |");

    std::wostringstream process;
    process << L"输入原式：" << displayRootNotation(text) << L"\r\n\r\n"
            << L"根据绝对值内部表达式的正负，分情况去掉绝对值符号。\r\n";
    std::vector<AbsoluteSolution> solutions;
    const unsigned branchCount = 1u << segments.size();
    for (unsigned branch = 0; branch < branchCount; ++branch) {
        const std::wstring expanded = expandAbsoluteBranch(text, segments, branch);
        Parser parser(expanded, angleUnit);
        const ParsedRelation parsed = parser.parseRelation();
        if (parsed.relation != TokenType::Equal) {
            throw ParseError("当前绝对值求解支持方程，暂不支持绝对值不等式");
        }
        const RationalExpression difference = add(parsed.left, parsed.right, -1.0);
        ensureOnlyX(difference.numerator);
        ensureOnlyX(difference.denominator);
        const int degree = difference.numerator.degree(0);
        if (degree > 16)
            throw ParseError("去掉绝对值后的方程次数不能超过 16");

        process << L"\r\n情况 " << branch + 1 << L"：";
        for (size_t i = 0; i < segments.size(); ++i) {
            if (i)
                process << L"，";
            process << segments[i].content << ((branch & (1u << i)) == 0 ? L" ≥ 0" : L" ≤ 0");
        }
        process << L"\r\n去掉绝对值后：" << expanded << L"\r\n"
                << L"    " << polynomialText(difference.numerator) << L" = 0\r\n";

        if (degree == 0) {
            if (difference.numerator.isZero()) {
                throw ParseError("该绝对值方程含有恒等分支，暂无法用有限个根表示解集");
            }
            process << L"该分支无解。\r\n";
            continue;
        }

        std::vector<double> candidates;
        std::vector<std::wstring> candidateTexts;
        bool approximate = false;
        if (degree <= 2) {
            const ExactRootResult exact =
                exactRoots(difference.numerator.coefficient(2), difference.numerator.coefficient(1),
                           difference.numerator.coefficient(0));
            if (exact.available && exact.hasRealRoots) {
                candidates = exact.values;
                candidateTexts = exact.texts;
                process << exact.calculation << L"\r\n";
            }
        } else if (degree <= 4) {
            std::vector<DisplayRoot> exactRootsForBranch;
            (void)solveHigherDegreeEquation(expanded, difference.numerator, degree, &exactRootsForBranch);
            const bool exactAvailable = !exactRootsForBranch.empty() &&
                                        std::none_of(exactRootsForBranch.begin(), exactRootsForBranch.end(),
                                                     [](const auto& root) { return root.approximate; });
            if (exactAvailable) {
                for (const auto& root : exactRootsForBranch) {
                    if (std::abs(root.value.imag()) >= 1e-8)
                        continue;
                    candidates.push_back(root.value.real());
                    candidateTexts.push_back(root.text);
                }
                process << L"该分支为三次或四次方程，使用精确代数公式求根并保留分数与根式。\r\n";
            } else {
                approximate = true;
            }
        } else {
            approximate = true;
        }
        if (approximate) {
            std::vector<long double> coefficients(static_cast<size_t>(degree + 1), 0.0L);
            for (const auto& [powers, coefficient] : difference.numerator.terms) {
                coefficients[static_cast<size_t>(powers[0])] = coefficient;
            }
            for (long double root : realPolynomialRoots(std::move(coefficients))) {
                candidates.push_back(static_cast<double>(root));
                candidateTexts.push_back(approximateNumber(static_cast<double>(root)));
            }
        }

        for (size_t candidateIndex = 0; candidateIndex < candidates.size(); ++candidateIndex) {
            const double candidate = candidates[candidateIndex];
            const std::wstring candidateText = candidateIndex < candidateTexts.size()
                                                   ? candidateTexts[candidateIndex]
                                                   : symbolicNumber(candidate);
            bool valid = std::abs(difference.denominator.evaluate(candidate)) >= kEpsilon;
            for (size_t i = 0; valid && i < segments.size(); ++i) {
                const double inside = evaluateRational(segments[i].expression, candidate);
                if (!std::isfinite(inside))
                    valid = false;
                else if ((branch & (1u << i)) == 0)
                    valid = inside >= -1e-9;
                else
                    valid = inside <= 1e-9;
            }
            process << L"验根 x " << (approximate ? L"≈ " : L"= ") << candidateText
                    << (valid ? L"：满足本分支条件，保留。\r\n" : L"：不满足本分支条件，舍去。\r\n");
            if (!valid)
                continue;
            bool duplicate = false;
            for (const auto& solution : solutions) {
                if (std::abs(solution.value - candidate) <=
                    1e-9 * std::max({1.0, std::abs(solution.value), std::abs(candidate)})) {
                    duplicate = true;
                    break;
                }
            }
            if (!duplicate)
                solutions.push_back({candidate, candidateText, approximate});
        }
    }

    std::sort(solutions.begin(), solutions.end(),
              [](const auto& left, const auto& right) { return left.value < right.value; });
    if (solutions.empty())
        return {L"结论：方程无解", process.str()};
    if (solutions.size() == 1) {
        return {(solutions[0].approximate ? L"数值解：x ≈ " : L"唯一解：x = ") + solutions[0].text,
                process.str()};
    }
    std::wstring summary = L"实数解：";
    for (size_t i = 0; i < solutions.size(); ++i) {
        if (i)
            summary += L"，";
        summary +=
            L"x" + std::to_wstring(i + 1) + (solutions[i].approximate ? L" ≈ " : L" = ") + solutions[i].text;
    }
    return {summary, process.str()};
}

SolveResult solveFractionalPowerEquation(const std::wstring& text, const RationalExpression& difference,
                                         int scale) {
    const Polynomial& polynomial = difference.numerator;
    const Polynomial& denominator = difference.denominator;
    ensureOnlyX(polynomial);
    ensureOnlyX(denominator);
    if (polynomial.degree(0) > 32)
        throw ParseError("换元后的方程次数不能超过 32");

    const int degree = polynomial.degree(0);
    const double a = polynomial.coefficient(2);
    const double b = polynomial.coefficient(1);
    const double c = polynomial.coefficient(0);
    std::vector<double> candidates;
    std::vector<std::wstring> candidateTexts;
    ExactRootResult exact;
    bool directPowerEquation = false;
    bool numericalHighDegree = false;
    bool exactAlgebraicHighDegree = false;
    if (degree > 2) {
        directPowerEquation = true;
        for (const auto& [powers, coefficient] : polynomial.terms) {
            if (std::abs(coefficient) < kEpsilon)
                continue;
            if (powers[1] != 0 || powers[2] != 0 || (powers[0] != 0 && powers[0] != degree)) {
                directPowerEquation = false;
                break;
            }
        }
        if (!directPowerEquation) {
            if (degree <= 4) {
                std::vector<DisplayRoot> transformedRoots;
                (void)solveHigherDegreeEquation(text, polynomial, degree, &transformedRoots);
                exactAlgebraicHighDegree = !transformedRoots.empty() &&
                                           std::none_of(transformedRoots.begin(), transformedRoots.end(),
                                                        [](const auto& root) { return root.approximate; });
                if (exactAlgebraicHighDegree) {
                    for (const auto& root : transformedRoots) {
                        if (std::abs(root.value.imag()) >= 1e-8)
                            continue;
                        candidates.push_back(root.value.real());
                        candidateTexts.push_back(root.text);
                    }
                }
            }
            if (!exactAlgebraicHighDegree) {
                numericalHighDegree = true;
                std::vector<long double> coefficients(static_cast<size_t>(degree + 1), 0.0L);
                for (const auto& [powers, coefficient] : polynomial.terms) {
                    coefficients[static_cast<size_t>(powers[0])] = coefficient;
                }
                for (long double root : realPolynomialRoots(std::move(coefficients))) {
                    candidates.push_back(static_cast<double>(root));
                    candidateTexts.push_back(approximateNumber(static_cast<double>(root)));
                }
            }
        } else {
            const double leading = polynomial.coefficient(degree);
            const double right = -c / leading;
            if (right >= 0.0 || degree % 2 == 1) {
                const double root =
                    right < 0.0 ? -std::pow(-right, 1.0 / degree) : std::pow(right, 1.0 / degree);
                candidates.push_back(root);
                candidateTexts.push_back(symbolicNumber(root));
                if (degree % 2 == 0 && right > kEpsilon) {
                    candidates.push_back(-root);
                    candidateTexts.push_back(L"-" + symbolicNumber(root));
                }
            }
        }
    } else if (degree > 0) {
        exact = exactRoots(a, b, c);
        if (exact.available && exact.hasRealRoots) {
            candidates = exact.values;
            candidateTexts = exact.texts;
        } else if (!exact.available && degree == 1 && std::abs(b) >= kEpsilon) {
            candidates = {-c / b};
            candidateTexts = {symbolicNumber(candidates.front())};
        } else if (!exact.available && degree == 2) {
            const double delta = b * b - 4.0 * a * c;
            if (delta >= -kEpsilon) {
                const double root = std::sqrt(std::max(0.0, delta));
                candidates = {(-b + root) / (2.0 * a)};
                candidateTexts = {symbolicNumber(candidates.front())};
                if (root > kEpsilon) {
                    candidates.push_back((-b - root) / (2.0 * a));
                    candidateTexts.push_back(symbolicNumber(candidates.back()));
                }
            }
        }
    }

    const std::wstring rootName = scale == 2 ? L"√x" : L"√[" + std::to_wstring(scale) + L"]{x}";
    std::wostringstream process;
    process << L"输入原式：" << displayRootNotation(text) << L"\r\n\r\n"
            << L"含分数指数，令 t = " << rootName << L"，则 x = t^" << scale << L"。\r\n";
    if (scale % 2 == 0)
        process << L"因为是偶次根，还必须满足 t ≥ 0。\r\n";
    process << L"换元后，原方程化为关于 t 的方程：\r\n"
            << L"    " << withSubstitutionVariable(polynomialText(polynomial)) << L" = 0\r\n\r\n";
    if (degree == 0) {
        if (std::abs(c) < kEpsilon) {
            process << L"等式恒成立，但仍需满足分数指数及分母的定义域。";
            return {L"解集：定义域内的所有实数", process.str()};
        }
        process << L"等式不成立，因此原方程无解。";
        return {L"结论：方程无解", process.str()};
    }
    if (exact.available) {
        process << withSubstitutionVariable(exact.calculation) << L"\r\n";
    } else if (directPowerEquation) {
        process << L"这是单项幂方程，直接开 " << degree << L" 次方求出 t。\r\n";
    } else if (exactAlgebraicHighDegree) {
        process << L"换元后得到三次或四次方程，使用精确代数公式求根，并保留分数与根式。\r\n";
    } else if (numericalHighDegree) {
        process << L"换元后得到一般高次方程。用导数划分单调区间，再以二分法求出全部实根。\r\n"
                << L"以下结果为经过原式验证的数值近似。\r\n";
    }

    std::vector<double> validValues;
    std::vector<std::wstring> validTexts;
    for (size_t i = 0; i < candidates.size(); ++i) {
        const double t = candidates[i];
        const std::wstring tText = i < candidateTexts.size() ? candidateTexts[i] : symbolicNumber(t);
        process << L"检验换元根 t = " << tText << L"：";
        if (scale % 2 == 0 && t < -kEpsilon) {
            process << L"不满足 t ≥ 0，舍去。\r\n";
            continue;
        }
        if (std::abs(denominator.evaluate(t)) < kEpsilon) {
            process << L"使原式分母为 0，舍去。\r\n";
            continue;
        }
        const double x = std::pow(t, scale);
        const std::wstring xText = numericalHighDegree ? approximateNumber(std::pow(t, scale))
                                                       : poweredCandidateText(t, tText, scale);
        bool duplicate = false;
        for (double existing : validValues) {
            if (std::abs(existing - x) <= 1e-9 * std::max({1.0, std::abs(existing), std::abs(x)})) {
                duplicate = true;
                break;
            }
        }
        process << L"满足定义域，由 x = t^" << scale << (numericalHighDegree ? L" 得 x ≈ " : L" 得 x = ")
                << xText << (duplicate ? L"（与已有解相同）。\r\n" : L"。\r\n");
        if (!duplicate) {
            validValues.push_back(x);
            validTexts.push_back(xText);
        }
    }
    if (validTexts.empty()) {
        if (candidates.empty())
            process << L"换元方程没有满足条件的实数根。";
        return {L"结论：方程无解", process.str()};
    }
    std::wstring summary;
    if (numericalHighDegree)
        summary = validTexts.size() == 1 ? L"数值解：x ≈ " : L"数值解：";
    else
        summary = validTexts.size() == 1 ? L"唯一解：x = " : L"实数解：";
    if (validTexts.size() == 1) {
        summary += validTexts.front();
    } else {
        for (size_t i = 0; i < validTexts.size(); ++i) {
            if (i)
                summary += L"，";
            summary +=
                L"x" + std::to_wstring(i + 1) + (numericalHighDegree ? L" ≈ " : L" = ") + validTexts[i];
        }
    }
    return {summary, process.str()};
}

SolveResult solveOneEquation(const std::wstring& text, InputKind requested, AngleUnit angleUnit) {
    Parser parser(text, angleUnit);
    const ParsedRelation parsed = parser.parseRelation();
    if (parsed.relation != TokenType::Equal)
        throw ParseError("方程应使用等号 =");
    const RationalExpression difference = add(parsed.left, parsed.right, -1.0);
    if (parser.fractionalScale() > 1) {
        return solveFractionalPowerEquation(text, difference, parser.fractionalScale());
    }
    const Polynomial& polynomial = difference.numerator;
    const Polynomial& denominator = difference.denominator;
    ensureOnlyX(polynomial);
    ensureOnlyX(denominator);

    const bool hasVariableDenominator = denominator.usesVariable(0);
    InputKind kind = requested;
    if (kind == InputKind::Auto) {
        if (hasVariableDenominator)
            kind = InputKind::Fraction;
        else if (polynomial.degree(0) >= 4)
            kind = InputKind::Quartic;
        else if (polynomial.degree(0) == 3)
            kind = InputKind::Cubic;
        else
            kind = polynomial.degree(0) == 2 ? InputKind::Quadratic : InputKind::Linear;
    }

    if (kind == InputKind::Fraction) {
        ensureUnivariateDegree(polynomial, 2);
        const double a = polynomial.coefficient(2);
        const double b = polynomial.coefficient(1);
        const double c = polynomial.coefficient(0);
        const double coefficientScale = std::max({1.0, std::abs(a), std::abs(b), std::abs(c)});
        const std::wstring kText = symbolicNumber(std::abs(a));
        const std::wstring displayedInput = displayRootNotation(text);
        std::wstring displayedKText = kText;
        const size_t equalsPosition = displayedInput.rfind(L'=');
        if (equalsPosition != std::wstring::npos) {
            std::wstring rightSide = displayedInput.substr(equalsPosition + 1);
            while (!rightSide.empty() && std::iswspace(rightSide.front()))
                rightSide.erase(rightSide.begin());
            while (!rightSide.empty() && std::iswspace(rightSide.back()))
                rightSide.pop_back();
            if ((rightSide.find(L'√') != std::wstring::npos || rightSide.find(L'∛') != std::wstring::npos) &&
                rightSide.find_first_of(L"xXyYzZ") == std::wstring::npos) {
                displayedKText = rightSide;
            }
        }
        const bool symbolicSymmetric =
            a < 0 && b > 0 && std::abs(a + c) <= 1e-8 * coefficientScale &&
            std::abs(b - 2.0) <= 1e-8 * coefficientScale &&
            (kText.find(L'√') != std::wstring::npos || kText.find(L'∛') != std::wstring::npos);
        std::wostringstream process;
        process << L"输入原式：" << displayRootNotation(text) << L"\r\n\r\n";
        if (hasVariableDenominator) {
            process << L"第 1 步：确定定义域，所有分母均不能为 0：\r\n"
                    << L"    " << polynomialText(denominator) << L" ≠ 0\r\n\r\n"
                    << L"第 2 步：方程两边同乘公共分母并合并同类项：\r\n";
        } else {
            process << L"分母中不含未知数，直接移项并合并同类项：\r\n";
        }
        if (symbolicSymmetric) {
            process << L"令 K = " << displayedKText << L"，去分母后的方程精确写为：\r\n"
                    << L"    -Kx² + 2x + K = 0\r\n\r\n";
        } else {
            process << L"    " << polynomialText(polynomial) << L" = 0\r\n\r\n";
        }
        std::vector<double> candidates;
        std::vector<std::wstring> candidateTexts;
        const ExactRootResult exact = exactRoots(a, b, c);
        if (std::abs(a) >= kEpsilon) {
            if (exact.available) {
                process << exact.calculation << L"\r\n";
                if (exact.hasRealRoots) {
                    candidates = exact.values;
                    candidateTexts = exact.texts;
                }
            } else if (symbolicSymmetric) {
                const double k = std::abs(a);
                const double nestedRoot = std::sqrt(1.0 + k * k);
                candidates = {(1.0 + nestedRoot) / k, (1.0 - nestedRoot) / k};
                candidateTexts = {L"(1 + √{1 + K²})/K", L"(1 - √{1 + K²})/K"};
                process << L"判别式 Δ = 4(1 + K²)，代入求根公式并保留根式：\r\n"
                        << L"    x₁ = (1 + √{1 + K²})/K\r\n"
                        << L"    x₂ = (1 - √{1 + K²})/K\r\n";
            } else {
                const double delta = b * b - 4 * a * c;
                process << L"判别式 Δ = b² - 4ac = " << formatNumber(delta) << L"。\r\n";
                if (delta >= -kEpsilon) {
                    const double root = std::sqrt(std::max(0.0, delta));
                    candidates.push_back((-b + root) / (2 * a));
                    candidateTexts.push_back(formatNumber(candidates.back()));
                    if (root > kEpsilon) {
                        candidates.push_back((-b - root) / (2 * a));
                        candidateTexts.push_back(formatNumber(candidates.back()));
                    }
                }
            }
        } else if (std::abs(b) >= kEpsilon) {
            if (exact.available && !exact.texts.empty()) {
                candidates = exact.values;
                candidateTexts = exact.texts;
            } else {
                candidates.push_back(-c / b);
                candidateTexts.push_back(symbolicNumber(candidates.front()));
            }
            process << L"解一元一次方程，得到候选根 x = " << candidateTexts.front() << L"。\r\n";
        } else if (std::abs(c) < kEpsilon) {
            process << L"去分母后等式恒成立，定义域内的所有实数都是解。";
            return {L"解集：定义域内的所有实数", process.str()};
        }

        std::vector<double> valid;
        std::vector<std::wstring> validTexts;
        for (size_t candidateIndex = 0; candidateIndex < candidates.size(); ++candidateIndex) {
            const double candidate = candidates[candidateIndex];
            const std::wstring candidateText = candidateIndex < candidateTexts.size()
                                                   ? candidateTexts[candidateIndex]
                                                   : formatNumber(candidate);
            const double denominatorValue = denominator.evaluate(candidate);
            process << L"验根：x = " << candidateText << L" 时，";
            if (std::abs(denominatorValue) < kEpsilon) {
                process << L"公共分母 = 0，不满足定义域，是增根，舍去。\r\n";
            } else {
                process << L"公共分母 ≠ 0，原方程成立，保留。\r\n";
                valid.push_back(candidate);
                validTexts.push_back(candidateText);
            }
        }
        if (valid.empty()) {
            if (candidates.empty())
                process << L"方程没有实数候选根。";
            return {L"结论：分式方程无实数解", process.str()};
        }
        std::wstring summary = symbolicSymmetric ? L"令 K = " + displayedKText + L"；解：" : L"解：";
        for (size_t i = 0; i < valid.size(); ++i) {
            if (i)
                summary += L"，";
            summary += L"x" + (valid.size() > 1 ? std::to_wstring(i + 1) : L"") + L" = " + validTexts[i];
        }
        return {summary, process.str()};
    }

    if (hasVariableDenominator)
        throw ParseError("检测到未知数位于分母，请选择“分式方程”或“自动识别”");
    if (kind == InputKind::Linear) {
        ensureUnivariateDegree(polynomial, 1);
        const double b = polynomial.coefficient(1);
        const double c = polynomial.coefficient(0);
        const ExactRootResult exact = exactRoots(0.0, b, c);
        if (exact.available && !exact.texts.empty()) {
            std::wostringstream process;
            process << L"输入原式：" << displayRootNotation(text) << L"\r\n"
                    << L"移项、通分并合并同类项：\r\n"
                    << L"    " << polynomialText(polynomial) << L" = 0\r\n\r\n"
                    << L"将系数化为最简整数比后：\r\n"
                    << L"    " << exact.calculation;
            return {L"唯一解：x = " + exact.texts.front(), process.str()};
        }
        if (std::abs(b) >= kEpsilon) {
            const double root = -c / b;
            const std::wstring rootText = symbolicNumber(root);
            std::wostringstream process;
            process << L"输入原式：" << displayRootNotation(text) << L"\r\n"
                    << L"移项并合并同类项：\r\n"
                    << L"    " << polynomialText(polynomial) << L" = 0\r\n\r\n"
                    << L"解得 x = " << rootText << L"。";
            return {L"唯一解：x = " + rootText, process.str()};
        }
        return prependProcess(text, polynomial, solveLinear(b, c, 0.0));
    }
    if (kind == InputKind::Quadratic) {
        ensureUnivariateDegree(polynomial, 2);
        const double a = polynomial.coefficient(2);
        const double b = polynomial.coefficient(1);
        const double c = polynomial.coefficient(0);
        if (std::abs(a) < kEpsilon)
            return solveOneEquation(text, InputKind::Linear, angleUnit);
        const ExactRootResult exact = exactRoots(a, b, c);
        if (exact.available && !exact.texts.empty()) {
            std::wstring summary;
            if (!exact.hasRealRoots) {
                summary = L"复数根：x₁ = " + exact.texts[0] + L"，x₂ = " + exact.texts[1];
            } else if (exact.texts.size() == 1) {
                summary = L"两个相等实根：x₁ = x₂ = " + exact.texts[0];
            } else {
                summary = L"两个实根：x₁ = " + exact.texts[0] + L"，x₂ = " + exact.texts[1];
            }
            std::wostringstream process;
            process << L"输入原式：" << displayRootNotation(text) << L"\r\n"
                    << L"移项、通分并合并同类项：\r\n"
                    << L"    " << polynomialText(polynomial) << L" = 0\r\n\r\n"
                    << L"将所有系数化为最简整数比，使用求根公式：\r\n"
                    << exact.calculation << L"\r\n"
                    << L"结果保留最简分数和根式。";
            return {summary, process.str()};
        }
        return prependProcess(text, polynomial, solveQuadratic(a, b, c));
    }
    if (kind == InputKind::Cubic) {
        ensureUnivariateDegree(polynomial, 3);
        if (polynomial.degree(0) < 3)
            return solveOneEquation(
                text, polynomial.degree(0) == 2 ? InputKind::Quadratic : InputKind::Linear, angleUnit);
        return solveHigherDegreeEquation(text, polynomial, 3);
    }
    if (kind == InputKind::Quartic) {
        ensureUnivariateDegree(polynomial, 4);
        if (polynomial.degree(0) < 4) {
            const InputKind lower = polynomial.degree(0) == 3   ? InputKind::Cubic
                                    : polynomial.degree(0) == 2 ? InputKind::Quadratic
                                                                : InputKind::Linear;
            return solveOneEquation(text, lower, angleUnit);
        }
        return solveHigherDegreeEquation(text, polynomial, 4);
    }
    throw ParseError("所选题型与输入原式不匹配");
}

std::array<double, 4> extractLinear(const ParsedRelation& parsed, int variableCount) {
    if (parsed.relation != TokenType::Equal)
        throw ParseError("方程组中的每一行都必须使用等号 =");
    const RationalExpression difference = add(parsed.left, parsed.right, -1.0);
    if (!difference.denominator.isConstant())
        throw ParseError("当前方程组不支持未知数出现在分母中");
    const Polynomial& polynomial = difference.numerator;
    if (polynomial.totalDegree() > 1)
        throw ParseError("方程组中的每个方程都必须是一次方程");
    for (int variable = variableCount; variable < 3; ++variable) {
        if (polynomial.usesVariable(variable))
            throw ParseError("方程组中出现了多余的未知数");
    }
    return {polynomial.coefficient(1, 0, 0), polynomial.coefficient(0, 1, 0), polynomial.coefficient(0, 0, 1),
            -polynomial.coefficient(0, 0, 0)};
}

std::wstring signedTerm(std::int64_t coefficient, const wchar_t* variable, bool first) {
    const bool negative = coefficient < 0;
    const std::int64_t magnitude = negative ? -coefficient : coefficient;
    std::wstring result;
    if (first) {
        if (negative)
            result += L"-";
    } else {
        result += negative ? L" - " : L" + ";
    }
    if (magnitude != 1 || variable[0] == L'\0')
        result += std::to_wstring(magnitude);
    result += variable;
    return result;
}

std::wstring equation2Text(const std::array<std::int64_t, 4>& row) {
    return signedTerm(row[0], L"x", true) + signedTerm(row[1], L"y", false) + L" = " +
           std::to_wstring(row[3]);
}

std::optional<std::array<ExactFraction, 3>>
exactSolve3(const std::array<std::array<double, 4>, 3>& equations) {
    std::array<std::array<ExactFraction, 4>, 3> matrix;
    for (int row = 0; row < 3; ++row) {
        for (int column = 0; column < 4; ++column) {
            const auto value = rationalize(equations[row][column]);
            if (!value)
                return std::nullopt;
            matrix[row][column] = *value;
        }
    }
    int pivotRow = 0;
    for (int column = 0; column < 3 && pivotRow < 3; ++column) {
        int found = pivotRow;
        while (found < 3 && matrix[found][column].isZero())
            ++found;
        if (found == 3)
            continue;
        if (found != pivotRow)
            std::swap(matrix[found], matrix[pivotRow]);
        const ExactFraction pivot = matrix[pivotRow][column];
        for (int j = column; j < 4; ++j)
            matrix[pivotRow][j] = matrix[pivotRow][j] / pivot;
        for (int row = 0; row < 3; ++row) {
            if (row == pivotRow || matrix[row][column].isZero())
                continue;
            const ExactFraction factor = matrix[row][column];
            for (int j = column; j < 4; ++j) {
                matrix[row][j] = matrix[row][j] - factor * matrix[pivotRow][j];
            }
        }
        ++pivotRow;
    }
    if (pivotRow != 3)
        return std::nullopt;
    return std::array<ExactFraction, 3>{matrix[0][3], matrix[1][3], matrix[2][3]};
}

std::wstring additionalMethods2(const std::array<std::array<double, 3>, 2>& equations, const ExactFraction& x,
                                const ExactFraction& y) {
    const auto first = toIntegerEquation({equations[0][0], equations[0][1], 0.0, equations[0][2]});
    const auto second = toIntegerEquation({equations[1][0], equations[1][1], 0.0, equations[1][2]});
    if (!first || !second)
        return L"";
    const auto& p = first->values;
    const auto& q = second->values;
    const std::int64_t d = p[0] * q[1] - q[0] * p[1];
    const std::int64_t dx = p[3] * q[1] - q[3] * p[1];

    std::wostringstream out;
    out << L"\r\n\r\n方法二：加减消元法\r\n"
        << L"将 (1) 乘以 " << q[1] << L"，将 (2) 乘以 " << p[1] << L"，两式相减以消去 y：\r\n"
        << L"    " << d << L"x = " << dx << L"\r\n"
        << L"    x = " << x.text() << L"\r\n"
        << L"把 x = " << x.text() << L" 代回原方程，得到 y = " << y.text() << L"。\r\n\r\n"
        << L"方法三：代入消元法\r\n";

    const auto& source = p[0] != 0 ? p : q;
    const auto& target = p[0] != 0 ? q : p;
    out << L"由 " << equation2Text(source) << L" 解出：\r\n"
        << L"    x = (" << source[3] << L" - " << source[1] << L"y)/" << source[0] << L"\r\n"
        << L"代入另一个方程 " << equation2Text(target) << L"，解得：\r\n"
        << L"    y = " << y.text() << L"\r\n"
        << L"再代回上式，x = " << x.text() << L"。\r\n"
        << L"两种消元方法所得结果一致。";
    return out.str();
}

std::wstring substitutionMethod3(const std::array<std::array<double, 4>, 3>& equations,
                                 const std::array<ExactFraction, 3>& solution) {
    int pivot = 0;
    while (pivot < 3 && std::abs(equations[pivot][0]) < kEpsilon)
        ++pivot;
    if (pivot == 3)
        return L"";
    const auto& p = equations[pivot];
    std::vector<std::array<ExactFraction, 3>> reduced;
    for (int row = 0; row < 3; ++row) {
        if (row == pivot)
            continue;
        std::array<ExactFraction, 3> converted;
        const auto a = rationalize(equations[row][0]);
        const auto b = rationalize(equations[row][1]);
        const auto c = rationalize(equations[row][2]);
        const auto d = rationalize(equations[row][3]);
        const auto pa = rationalize(p[0]);
        const auto pb = rationalize(p[1]);
        const auto pc = rationalize(p[2]);
        const auto pd = rationalize(p[3]);
        if (!a || !b || !c || !d || !pa || !pb || !pc || !pd)
            return L"";
        converted[0] = *b - (*a * *pb / *pa);
        converted[1] = *c - (*a * *pc / *pa);
        converted[2] = *d - (*a * *pd / *pa);
        reduced.push_back(converted);
    }
    std::wostringstream out;
    out << L"\r\n\r\n方法二：代入消元法\r\n"
        << L"从第 " << pivot + 1 << L" 个方程解出 x：\r\n"
        << L"    x = (" << formatNumber(p[3]) << L" - " << formatNumber(p[1]) << L"y - " << formatNumber(p[2])
        << L"z)/" << formatNumber(p[0]) << L"\r\n"
        << L"分别代入另外两个方程，得到关于 y、z 的二元方程组：\r\n";
    for (const auto& row : reduced) {
        out << L"    " << row[0].text() << L"y + " << row[1].text() << L"z = " << row[2].text() << L"\r\n";
    }
    out << L"解这个二元方程组：y = " << solution[1].text() << L"，z = " << solution[2].text()
        << L"。\r\n再代回 x 的表达式：x = " << solution[0].text() << L"。\r\n"
        << L"代入消元法与加减消元法所得结果一致。";
    return out.str();
}

SolveResult solveSystem(const std::vector<std::wstring>& lines, int variables, AngleUnit angleUnit) {
    if (static_cast<int>(lines.size()) != variables) {
        throw ParseError(variables == 2 ? "二元方程组需要输入两行方程" : "三元方程组需要输入三行方程");
    }
    std::wstring prefix = L"输入方程组：\r\n";
    for (const auto& line : lines)
        prefix += L"    " + line + L"\r\n";
    prefix += L"\r\n解析各行并移项后，得到标准形式。\r\n\r\n";

    if (variables == 2) {
        std::array<std::array<double, 3>, 2> equations{};
        for (int i = 0; i < 2; ++i) {
            const auto values = extractLinear(Parser(lines[i], angleUnit).parseRelation(), 2);
            equations[i] = {values[0], values[1], values[3]};
        }
        SolveResult result = solveLinear2(equations);
        result.process = prefix + result.process;
        const auto first = toIntegerEquation({equations[0][0], equations[0][1], 0.0, equations[0][2]});
        const auto second = toIntegerEquation({equations[1][0], equations[1][1], 0.0, equations[1][2]});
        if (first && second) {
            const auto& p = first->values;
            const auto& q = second->values;
            const std::int64_t d = p[0] * q[1] - q[0] * p[1];
            if (d != 0) {
                const ExactFraction x(p[3] * q[1] - q[3] * p[1], d);
                const ExactFraction y(p[0] * q[3] - q[0] * p[3], d);
                result.summary = L"唯一解：x = " + x.text() + L"，y = " + y.text();
                result.process += additionalMethods2(equations, x, y);
            }
        }
        return result;
    }
    std::array<std::array<double, 4>, 3> equations{};
    for (int i = 0; i < 3; ++i) {
        const auto values = extractLinear(Parser(lines[i], angleUnit).parseRelation(), 3);
        equations[i] = {values[0], values[1], values[2], values[3]};
    }
    SolveResult result = solveLinear3(equations);
    result.process = prefix + L"方法一：加减消元法（高斯消元）\r\n" + result.process;
    const auto exact = exactSolve3(equations);
    if (exact) {
        result.summary = L"唯一解：x = " + (*exact)[0].text() + L"，y = " + (*exact)[1].text() + L"，z = " +
                         (*exact)[2].text();
        result.process += substitutionMethod3(equations, *exact);
    }
    return result;
}

SolveResult solveTextInequality(const std::wstring& text, AngleUnit angleUnit) {
    Parser parser(text, angleUnit);
    const ParsedRelation parsed = parser.parseRelation();
    if (parser.fractionalScale() > 1) {
        throw ParseError("分数指数不等式超出当前一元一次不等式的支持范围");
    }
    if (parsed.relation == TokenType::Equal)
        throw ParseError("一元一次不等式需要使用 <、<=、> 或 >=");
    const RationalExpression difference = add(parsed.left, parsed.right, -1.0);
    if (!difference.denominator.isConstant())
        throw ParseError("当前仅支持分母为常数的一元一次不等式");
    ensureUnivariateDegree(difference.numerator, 1);
    double denominator = difference.denominator.constantValue();
    if (std::abs(denominator) < kEpsilon)
        throw ParseError("分母不能为 0");
    Relation relation = toRelation(parsed.relation);
    if (denominator < 0)
        relation = reverseRelation(relation);
    const double scale = std::abs(denominator);
    const double a = difference.numerator.coefficient(1) / scale;
    const double b = difference.numerator.coefficient(0) / scale;
    SolveResult result = solveInequality(a, b, relation, 0.0);
    const auto exactA = rationalize(a);
    const auto exactB = rationalize(b);
    if (exactA && exactB && !exactA->isZero()) {
        const ExactFraction boundary = ExactFraction(-exactB->numerator, exactB->denominator) / *exactA;
        const Relation finalRelation = exactA->numerator < 0 ? reverseRelation(relation) : relation;
        result.summary = L"解集：x " + relationSymbol(finalRelation) + L" " + boundary.text();
        result.process += L"\r\n\r\n将边界值约分，得到最简分数：x " + relationSymbol(finalRelation) + L" " +
                          boundary.text();
    }
    result.process = L"输入原式：" + displayRootNotation(text) + L"\r\n解析、移项并合并同类项后：\r\n\r\n" +
                     result.process;
    return result;
}

struct TrigCall {
    TokenType function = TokenType::Sin;
    std::wstring name;
    std::wstring argument;
};

std::wstring trimText(std::wstring text) {
    const size_t begin = text.find_first_not_of(L" \t\r\n");
    if (begin == std::wstring::npos)
        return L"";
    const size_t end = text.find_last_not_of(L" \t\r\n");
    return text.substr(begin, end - begin + 1);
}

std::optional<TrigCall> parseWholeTrigCall(const std::wstring& source) {
    const std::wstring text = trimText(source);
    struct FunctionInfo {
        const wchar_t* name;
        TokenType type;
    };
    constexpr std::array<FunctionInfo, 4> functions{{{L"sin", TokenType::Sin},
                                                     {L"cos", TokenType::Cos},
                                                     {L"tan", TokenType::Tan},
                                                     {L"cot", TokenType::Cot}}};
    for (const auto& function : functions) {
        const size_t length = std::wcslen(function.name);
        if (text.size() <= length)
            continue;
        bool matches = true;
        for (size_t i = 0; i < length; ++i) {
            if (std::towlower(text[i]) != function.name[i]) {
                matches = false;
                break;
            }
        }
        if (!matches)
            continue;
        size_t opening = length;
        while (opening < text.size() && std::iswspace(text[opening]))
            ++opening;
        if (opening >= text.size() || text[opening] != L'(')
            continue;
        int depth = 0;
        size_t closing = std::wstring::npos;
        for (size_t i = opening; i < text.size(); ++i) {
            if (text[i] == L'(')
                ++depth;
            else if (text[i] == L')' && --depth == 0) {
                closing = i;
                break;
            }
        }
        if (closing == std::wstring::npos || !trimText(text.substr(closing + 1)).empty())
            continue;
        return TrigCall{function.type, function.name, text.substr(opening + 1, closing - opening - 1)};
    }
    return std::nullopt;
}

std::optional<double> constantExpressionValue(const std::wstring& expression, AngleUnit angleUnit) {
    const ParsedRelation parsed = Parser(expression + L"=0", angleUnit).parseRelation();
    if (!parsed.left.numerator.isConstant() || !parsed.left.denominator.isConstant())
        return std::nullopt;
    const double denominator = parsed.left.denominator.constantValue();
    if (std::abs(denominator) < kEpsilon)
        throw ParseError("计算过程中出现除以 0");
    return parsed.left.numerator.constantValue() / denominator;
}

std::optional<std::pair<double, double>> linearArgument(const std::wstring& expression, AngleUnit angleUnit) {
    const ParsedRelation parsed = Parser(expression + L"=0", angleUnit).parseRelation();
    if (!parsed.left.denominator.isConstant())
        return std::nullopt;
    if (parsed.left.numerator.usesVariable(1) || parsed.left.numerator.usesVariable(2) ||
        parsed.left.numerator.degree(0) > 1)
        return std::nullopt;
    const double denominator = parsed.left.denominator.constantValue();
    if (std::abs(denominator) < kEpsilon)
        throw ParseError("计算过程中出现除以 0");
    return std::pair<double, double>{parsed.left.numerator.coefficient(1) / denominator,
                                     parsed.left.numerator.coefficient(0) / denominator};
}

std::optional<double> trigValue(TokenType function, double angle) {
    if (function == TokenType::Sin)
        return std::sin(angle);
    if (function == TokenType::Cos)
        return std::cos(angle);
    if (function == TokenType::Tan) {
        if (std::abs(std::cos(angle)) < 1e-12)
            return std::nullopt;
        return std::tan(angle);
    }
    if (std::abs(std::sin(angle)) < 1e-12)
        return std::nullopt;
    return std::cos(angle) / std::sin(angle);
}

std::optional<ExactFraction> angleAsPiFraction(double angle) {
    const auto fraction = rationalize(angle / kPi);
    if (!fraction || fraction->denominator > 24)
        return std::nullopt;
    if (std::abs(fraction->value() * kPi - angle) > 1e-10)
        return std::nullopt;
    return fraction;
}

std::wstring piFractionText(const ExactFraction& value) {
    if (value.numerator == 0)
        return L"0";
    const bool negative = value.numerator < 0;
    const std::int64_t numerator = negative ? -value.numerator : value.numerator;
    std::wstring result = negative ? L"-" : L"";
    if (numerator != 1)
        result += std::to_wstring(numerator);
    result += L"π";
    if (value.denominator != 1)
        result += L"/" + std::to_wstring(value.denominator);
    return result;
}

std::wstring trigFamilyText(const std::wstring& angleText, int periodPi, double argumentA, double argumentB) {
    std::wstring numerator = angleText == L"0" ? L"" : angleText;
    if (std::abs(argumentB) >= kEpsilon) {
        const std::wstring value = symbolicNumber(std::abs(argumentB));
        if (numerator.empty())
            numerator = argumentB > 0 ? L"-" + value : value;
        else
            numerator += argumentB > 0 ? L" - " + value : L" + " + value;
    }
    const std::wstring periodic = periodPi == 1 ? L"kπ" : std::to_wstring(periodPi) + L"kπ";
    if (numerator.empty())
        numerator = periodic;
    else
        numerator += L" + " + periodic;
    if (std::abs(argumentA - 1.0) < kEpsilon)
        return numerator;
    return L"(" + numerator + L")/" + symbolicNumber(argumentA);
}

std::wstring degreeFractionText(const ExactFraction& value) {
    return value.text() + L"°";
}

std::wstring trigFamilyDegrees(const std::wstring& angleText, int periodDegrees, double argumentA,
                               double argumentB) {
    std::wstring numerator = angleText == L"0°" ? L"" : angleText;
    if (std::abs(argumentB) >= kEpsilon) {
        const std::wstring value = symbolicNumber(std::abs(argumentB)) + L"°";
        if (numerator.empty())
            numerator = argumentB > 0 ? L"-" + value : value;
        else
            numerator += argumentB > 0 ? L" - " + value : L" + " + value;
    }
    const std::wstring periodic = std::to_wstring(periodDegrees) + L"k°";
    if (numerator.empty())
        numerator = periodic;
    else
        numerator += L" + " + periodic;
    if (std::abs(argumentA - 1.0) < kEpsilon)
        return numerator;
    return L"(" + numerator + L")/" + symbolicNumber(argumentA);
}

std::optional<SolveResult> solveTrigEquation(const std::wstring& text, AngleUnit angleUnit) {
    const size_t equals = text.find(L'=');
    if (equals == std::wstring::npos || text.find(L'=', equals + 1) != std::wstring::npos)
        return std::nullopt;
    const std::wstring left = text.substr(0, equals);
    const std::wstring right = text.substr(equals + 1);
    auto call = parseWholeTrigCall(left);
    std::wstring constantSide = right;
    if (!call) {
        call = parseWholeTrigCall(right);
        constantSide = left;
    }
    if (!call)
        return std::nullopt;
    const auto targetValue = constantExpressionValue(constantSide, angleUnit);
    const auto argument = linearArgument(call->argument, angleUnit);
    if (!targetValue || !argument) {
        throw ParseError("含未知数的三角方程目前支持 sin(ax+b)=c、cos(ax+b)=c、tan(ax+b)=c 或 cot(ax+b)=c");
    }
    const double target = *targetValue;
    const double a = argument->first;
    const double b = argument->second;
    const bool degreeMode = angleUnit == AngleUnit::Degrees;
    std::wostringstream process;
    process << L"输入原式：" << displayRootNotation(text) << L"\r\n\r\n"
            << (degreeMode ? L"三角函数按角度制计算。令 u = " : L"三角函数按弧度制计算。令 u = ")
            << polynomialText(Polynomial{{{{1, 0, 0}, a}, {{0, 0, 0}, b}}}) << L"。\r\n";

    if (std::abs(a) < kEpsilon) {
        const double angle = degreeMode ? b * kPi / 180.0 : b;
        const auto value = trigValue(call->function, angle);
        if (!value)
            return SolveResult{L"结论：方程无实数解", process.str() + L"该角度处函数无定义。"};
        if (std::abs(*value - target) <= 1e-10 * std::max({1.0, std::abs(*value), std::abs(target)})) {
            return SolveResult{L"结论：有无穷多个解，x ∈ ℝ",
                               process.str() + L"等式恒成立，因此任意实数 x 都是解。"};
        }
        return SolveResult{L"结论：方程无实数解", process.str() + L"等式恒不成立。"};
    }

    double alpha = 0.0;
    std::wstring inverseName;
    if (call->function == TokenType::Sin || call->function == TokenType::Cos) {
        if (target < -1.0 - 1e-12 || target > 1.0 + 1e-12) {
            process << L"因为 " << call->name << L" 的值域是 [-1, 1]，所以方程无实数解。";
            return SolveResult{L"结论：方程无实数解", process.str()};
        }
        const double clamped = std::clamp(target, -1.0, 1.0);
        if (call->function == TokenType::Sin) {
            alpha = std::asin(clamped);
            inverseName = L"arcsin";
        } else {
            alpha = std::acos(clamped);
            inverseName = L"arccos";
        }
    } else if (call->function == TokenType::Tan) {
        alpha = std::atan(target);
        inverseName = L"arctan";
    } else {
        alpha = std::atan2(1.0, target);
        inverseName = L"arccot";
    }

    const auto exactAlpha = degreeMode ? std::optional<ExactFraction>{} : angleAsPiFraction(alpha);
    std::optional<ExactFraction> exactDegreeAlpha;
    if (degreeMode) {
        const double degrees = alpha * 180.0 / kPi;
        const auto candidate = rationalize(degrees);
        if (candidate && candidate->denominator <= 360 && std::abs(candidate->value() - degrees) <= 1e-10)
            exactDegreeAlpha = candidate;
    }
    const std::wstring alphaText = degreeMode
                                       ? (exactDegreeAlpha ? degreeFractionText(*exactDegreeAlpha) : L"α")
                                       : (exactAlpha ? piFractionText(*exactAlpha) : L"α");
    process << L"令 α = " << inverseName << L"(" << symbolicNumber(target) << L")";
    if (degreeMode && exactDegreeAlpha)
        process << L" = " << alphaText << L"。\r\n";
    else if (!degreeMode && exactAlpha)
        process << L" = " << alphaText << L"。\r\n";
    else if (degreeMode)
        process << L" ≈ " << approximateNumber(alpha * 180.0 / kPi) << L"°。\r\n";
    else
        process << L" ≈ " << approximateNumber(alpha) << L"。\r\n";

    std::vector<std::wstring> families;
    if (call->function == TokenType::Sin) {
        if (std::abs(target) < 1e-12) {
            families.push_back(degreeMode ? trigFamilyDegrees(L"0°", 180, a, b)
                                          : trigFamilyText(L"0", 1, a, b));
            process << (degreeMode ? L"sin u = 0，所以 u = 180k°。\r\n" : L"sin u = 0，所以 u = kπ。\r\n");
        } else {
            families.push_back(degreeMode ? trigFamilyDegrees(alphaText, 360, a, b)
                                          : trigFamilyText(alphaText, 2, a, b));
            std::wstring secondAngle = degreeMode ? L"180° - α" : L"π - α";
            if (degreeMode && exactDegreeAlpha)
                secondAngle = degreeFractionText(ExactFraction(180) - *exactDegreeAlpha);
            else if (!degreeMode && exactAlpha)
                secondAngle = piFractionText(ExactFraction(1) - *exactAlpha);
            families.push_back(degreeMode ? trigFamilyDegrees(secondAngle, 360, a, b)
                                          : trigFamilyText(secondAngle, 2, a, b));
            process << (degreeMode ? L"sin u = sin α，所以 u = α + 360k° 或 u = 180° - α + 360k°。\r\n"
                                   : L"sin u = sin α，所以 u = α + 2kπ 或 u = π - α + 2kπ。\r\n");
            if (std::abs(std::abs(target) - 1.0) < 1e-12)
                families.resize(1);
        }
    } else if (call->function == TokenType::Cos) {
        families.push_back(degreeMode ? trigFamilyDegrees(alphaText, 360, a, b)
                                      : trigFamilyText(alphaText, 2, a, b));
        if (std::abs(std::abs(target) - 1.0) >= 1e-12) {
            std::wstring negativeAlpha = L"-α";
            if (degreeMode && exactDegreeAlpha) {
                negativeAlpha = degreeFractionText(
                    ExactFraction(-exactDegreeAlpha->numerator, exactDegreeAlpha->denominator));
            } else if (!degreeMode && exactAlpha) {
                negativeAlpha =
                    piFractionText(ExactFraction(-exactAlpha->numerator, exactAlpha->denominator));
            }
            families.push_back(degreeMode ? trigFamilyDegrees(negativeAlpha, 360, a, b)
                                          : trigFamilyText(negativeAlpha, 2, a, b));
        }
        process << (degreeMode ? L"cos u = cos α，所以 u = 360k° ± α。\r\n"
                               : L"cos u = cos α，所以 u = 2kπ ± α。\r\n");
    } else {
        families.push_back(degreeMode ? trigFamilyDegrees(alphaText, 180, a, b)
                                      : trigFamilyText(alphaText, 1, a, b));
        process << call->name << L" u = " << symbolicNumber(target)
                << (degreeMode ? L"，所以 u = α + 180k°。\r\n" : L"，所以 u = α + kπ。\r\n");
    }

    std::wstring summary = L"通解：";
    for (size_t i = 0; i < families.size(); ++i) {
        if (i)
            summary += L"，或 ";
        summary += L"x = " + families[i];
    }
    summary += L"；k ∈ ℤ";
    process << L"解出 x，得到上述通解，其中 k ∈ ℤ。";
    return SolveResult{summary, process.str()};
}

InputKind detectKind(const std::vector<std::wstring>& lines, AngleUnit angleUnit) {
    if (lines.size() == 2) {
        int maximumDegree = 0;
        for (const auto& line : lines) {
            Parser parser(line, angleUnit);
            const ParsedRelation relation = parser.parseRelation();
            if (relation.relation != TokenType::Equal)
                throw ParseError("方程组中的每一行都必须使用等号 =");
            const RationalExpression difference = add(relation.left, relation.right, -1.0);
            maximumDegree = std::max(maximumDegree, difference.numerator.totalDegree());
        }
        return maximumDegree > 1 ? InputKind::Quadratic2 : InputKind::Linear2;
    }
    if (lines.size() == 3)
        return InputKind::Linear3;
    if (lines.size() != 1)
        throw ParseError("请输入一个方程，或输入两行/三行方程组");
    const ParsedRelation parsed = Parser(lines.front(), angleUnit).parseRelation();
    if (parsed.relation != TokenType::Equal)
        return InputKind::Inequality;
    const RationalExpression difference = add(parsed.left, parsed.right, -1.0);
    if (difference.denominator.usesVariable(0) || difference.denominator.usesVariable(1) ||
        difference.denominator.usesVariable(2))
        return InputKind::Fraction;
    if (difference.numerator.usesVariable(1) || difference.numerator.usesVariable(2)) {
        throw ParseError("含 y 或 z 的方程组请每个方程输入一行");
    }
    const int degree = difference.numerator.degree(0);
    if (degree >= 4)
        return InputKind::Quartic;
    if (degree == 3)
        return InputKind::Cubic;
    return degree == 2 ? InputKind::Quadratic : InputKind::Linear;
}

} // namespace

// 公共入口只在原始输入阶段校验括号层级；内部等价变形生成的括号不重复校验。
SolveResult solveText(const std::wstring& text, InputKind kind, AngleUnit angleUnit) {
    validateGrouping(text);
    std::wstring normalized;
    normalized.reserve(text.size());
    std::array<wchar_t, 3> variables{L'x', L'y', L'z'};
    std::vector<wchar_t> variableKeys;
    constexpr std::array<wchar_t, 3> canonical{L'x', L'y', L'z'};
    for (size_t sourceIndex = 0; sourceIndex < text.size();) {
        const wchar_t ch = text[sourceIndex];
        auto functionAt = [&](const wchar_t* name) -> bool {
            const size_t length = std::wcslen(name);
            if (sourceIndex + length > text.size())
                return false;
            for (size_t offset = 0; offset < length; ++offset) {
                if (std::towlower(text[sourceIndex + offset]) != name[offset])
                    return false;
            }
            size_t after = sourceIndex + length;
            while (after < text.size() && std::iswspace(text[after]))
                ++after;
            return after < text.size() && isOpeningGrouping(text[after]);
        };
        const wchar_t* functionName =
            functionAt(L"sin")
                ? L"sin"
                : (functionAt(L"cos")
                       ? L"cos"
                       : (functionAt(L"tan") ? L"tan" : (functionAt(L"cot") ? L"cot" : nullptr)));
        if (functionName) {
            normalized += functionName;
            sourceIndex += std::wcslen(functionName);
            continue;
        }
        if (ch == L'π') {
            normalized += ch;
            ++sourceIndex;
            continue;
        }
        if ((ch == L'p' || ch == L'P') && sourceIndex + 1 < text.size() &&
            (text[sourceIndex + 1] == L'i' || text[sourceIndex + 1] == L'I')) {
            normalized += L"pi";
            sourceIndex += 2;
            continue;
        }
        if (ch == L'e' || ch == L'E') {
            normalized += L'e';
            ++sourceIndex;
            continue;
        }
        if (ch == L'i' || ch == L'I') {
            normalized += L'i';
            ++sourceIndex;
            continue;
        }
        if (!((ch >= L'a' && ch <= L'z') || (ch >= L'A' && ch <= L'Z'))) {
            normalized += ch;
            ++sourceIndex;
            continue;
        }
        const wchar_t key = static_cast<wchar_t>(std::towlower(ch));
        auto found = std::find(variableKeys.begin(), variableKeys.end(), key);
        size_t index = 0;
        if (found == variableKeys.end()) {
            if (variableKeys.size() >= canonical.size())
                throw ParseError("最多支持三个不同的未知数");
            index = variableKeys.size();
            variableKeys.push_back(key);
            variables[index] = ch;
        } else {
            index = static_cast<size_t>(found - variableKeys.begin());
        }
        normalized += canonical[index];
        ++sourceIndex;
    }

    const std::vector<std::wstring> lines = splitLines(normalized);
    if (lines.empty())
        throw ParseError("请输入需要求解的原方程或不等式");
    SolveResult result;
    if (hasImaginaryUnitToken(normalized))
        result = solveComplexText(normalized, displayRootNotation(text));
    if (!result.usesImaginaryUnit && lines.size() == 1) {
        if (const auto trigResult = solveTrigEquation(lines.front(), angleUnit))
            result = *trigResult;
    }
    if (result.summary.empty() && result.process.empty() && lines.size() == 1 &&
        lines.front().find(L'|') != std::wstring::npos) {
        if (!absoluteSegments(lines.front(), angleUnit).empty())
            result = solveAbsoluteEquation(lines.front(), angleUnit);
    }
    if (result.summary.empty() && result.process.empty()) {
        if (kind == InputKind::Trigonometric) {
            throw ParseError("三角方程请输入 sin(ax+b)=c、cos(ax+b)=c、tan(ax+b)=c 或 cot(ax+b)=c");
        }
        if (kind == InputKind::Auto)
            kind = detectKind(lines, angleUnit);
        if (kind == InputKind::Linear2)
            result = solveSystem(lines, 2, angleUnit);
        else if (kind == InputKind::Quadratic2)
            result = solveQuadraticSystem(lines, angleUnit);
        else if (kind == InputKind::Linear3)
            result = solveSystem(lines, 3, angleUnit);
        else {
            if (lines.size() != 1)
                throw ParseError("该题型只需要输入一行原式");
            result = kind == InputKind::Inequality ? solveTextInequality(lines.front(), angleUnit)
                                                   : solveOneEquation(lines.front(), kind, angleUnit);
        }
    }
    result.variables = variables;
    result.variableCount = static_cast<int>(variableKeys.size());
    result.normalizedInput = normalized;
    return result;
}

std::wstring applyVariableNames(const std::wstring& text, const SolveResult& result) {
    std::wstring output;
    output.reserve(text.size() + 8);
    const auto isAsciiLetter = [](wchar_t ch) {
        return (ch >= L'a' && ch <= L'z') || (ch >= L'A' && ch <= L'Z');
    };
    for (size_t i = 0; i < text.size(); ++i) {
        const wchar_t ch = text[i];
        if (ch == L'ₓ' && result.variableCount > 0) {
            output += L"_";
            output += result.variables[0];
            continue;
        }
        if (ch == L'ᵧ' && result.variableCount > 1) {
            output += L"_";
            output += result.variables[1];
            continue;
        }
        int variable = ch == L'x' ? 0 : (ch == L'y' ? 1 : (ch == L'z' ? 2 : -1));
        if (variable >= 0 && variable < result.variableCount) {
            const bool previousLetter = i > 0 && isAsciiLetter(text[i - 1]);
            const bool nextLetter = i + 1 < text.size() && isAsciiLetter(text[i + 1]);
            if (!previousLetter && !nextLetter) {
                output += result.variables[static_cast<size_t>(variable)];
                continue;
            }
        }
        output += ch;
    }
    return output;
}

} // namespace equation
