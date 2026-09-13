// 线性方程、线性方程组、二次方程和一次不等式的系数级求解实现。
// 文本解析与精确符号恢复由 expression_parser.cpp 负责。

#include "solver.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <iomanip>
#include <limits>
#include <numeric>
#include <sstream>

namespace equation {
namespace {

constexpr double kEpsilon = 1e-10;

bool isZero(double value) {
    return std::abs(value) < kEpsilon;
}

int realRootPriority(double value) {
    const double tolerance = 1e-8 * std::clamp(std::abs(value), 1.0, 10000.0);
    if (std::abs(value - std::round(value)) <= tolerance)
        return 0;
    return value > 0.0 ? 1 : 2;
}

std::wstring relationText(Relation relation) {
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

bool compare(double left, Relation relation, double right) {
    switch (relation) {
    case Relation::Less:
        return left < right;
    case Relation::LessEqual:
        return left <= right || isZero(left - right);
    case Relation::Greater:
        return left > right;
    case Relation::GreaterEqual:
        return left >= right || isZero(left - right);
    }
    return false;
}

std::wstring matrixText(const std::array<std::array<double, 4>, 3>& matrix) {
    std::wostringstream out;
    for (const auto& row : matrix) {
        out << L"    [ " << std::setw(9) << formatNumber(row[0]) << L"  " << std::setw(9)
            << formatNumber(row[1]) << L"  " << std::setw(9) << formatNumber(row[2]) << L"  |  "
            << std::setw(9) << formatNumber(row[3]) << L" ]\r\n";
    }
    return out.str();
}

} // namespace

// 优先恢复由二进制浮点产生的小分数，再回退到紧凑小数。
std::wstring formatNumber(double value) {
    if (isZero(value)) {
        return L"0";
    }

    const double nearestInteger = std::round(value);
    if (std::abs(value - nearestInteger) < 1e-9 && std::abs(nearestInteger) < 1e15) {
        std::wostringstream integerOut;
        integerOut << std::fixed << std::setprecision(0) << nearestInteger;
        return integerOut.str();
    }

    // Values produced from exact decimal/fraction input often arrive here as
    // repeating binary floating-point numbers.  Recover a small exact fraction
    // before falling back to decimal formatting.
    constexpr std::int64_t maxDenominator = 10000;
    constexpr long double fractionTolerance = 1e-12L;
    const long double wideValue = static_cast<long double>(value);
    for (std::int64_t denominator = 2; denominator <= maxDenominator; ++denominator) {
        const long double numeratorWide = std::round(wideValue * denominator);
        if (std::abs(numeratorWide) > static_cast<long double>(std::numeric_limits<std::int64_t>::max()))
            break;
        const auto numerator = static_cast<std::int64_t>(numeratorWide);
        const long double approximation = static_cast<long double>(numerator) / denominator;
        if (std::abs(approximation - wideValue) <= fractionTolerance * std::max(1.0L, std::abs(wideValue))) {
            const std::int64_t divisor = std::gcd(numerator < 0 ? -numerator : numerator, denominator);
            return std::to_wstring(numerator / divisor) + L"/" + std::to_wstring(denominator / divisor);
        }
    }

    std::wostringstream out;
    out << std::fixed << std::setprecision(8) << value;
    std::wstring text = out.str();
    while (!text.empty() && text.back() == L'0') {
        text.pop_back();
    }
    if (!text.empty() && text.back() == L'.') {
        text.pop_back();
    }
    return text;
}

SolveResult solveLinear(double a, double b, double c) {
    std::wostringstream process;
    process << L"原方程：" << formatNumber(a) << L"x + " << formatNumber(b) << L" = " << formatNumber(c)
            << L"\r\n\r\n";

    if (isZero(a)) {
        process << L"因为 x 的系数 a = 0，方程化为：\r\n"
                << L"    " << formatNumber(b) << L" = " << formatNumber(c) << L"\r\n";
        if (isZero(b - c)) {
            process << L"等式恒成立，因此任意实数 x 都满足方程。";
            return {L"结论：有无穷多个解，x ∈ ℝ", process.str()};
        }
        process << L"等式不成立，因此不存在满足条件的 x。";
        return {L"结论：方程无解", process.str()};
    }

    const double right = c - b;
    const double x = right / a;
    process << L"第 1 步：两边同时减去 " << formatNumber(b) << L"：\r\n"
            << L"    " << formatNumber(a) << L"x = " << formatNumber(c) << L" - " << formatNumber(b) << L" = "
            << formatNumber(right) << L"\r\n\r\n"
            << L"第 2 步：两边同时除以 " << formatNumber(a) << L"：\r\n"
            << L"    x = " << formatNumber(right) << L" ÷ " << formatNumber(a) << L" = " << formatNumber(x)
            << L"\r\n\r\n"
            << L"检验：" << formatNumber(a) << L" × " << formatNumber(x) << L" + " << formatNumber(b)
            << L" = " << formatNumber(c) << L"，成立。";
    return {L"唯一解：x = " + formatNumber(x), process.str()};
}

SolveResult solveLinear2(const std::array<std::array<double, 3>, 2>& e) {
    const double a1 = e[0][0], b1 = e[0][1], c1 = e[0][2];
    const double a2 = e[1][0], b2 = e[1][1], c2 = e[1][2];
    const double d = a1 * b2 - a2 * b1;
    const double dx = c1 * b2 - c2 * b1;
    const double dy = a1 * c2 - a2 * c1;

    std::wostringstream process;
    process << L"原方程组：\r\n"
            << L"    " << formatNumber(a1) << L"x + " << formatNumber(b1) << L"y = " << formatNumber(c1)
            << L"\r\n"
            << L"    " << formatNumber(a2) << L"x + " << formatNumber(b2) << L"y = " << formatNumber(c2)
            << L"\r\n\r\n"
            << L"使用克拉默法则。先计算系数行列式：\r\n"
            << L"    D = a₁b₂ - a₂b₁\r\n"
            << L"      = " << formatNumber(a1) << L" × " << formatNumber(b2) << L" - " << formatNumber(a2)
            << L" × " << formatNumber(b1) << L" = " << formatNumber(d) << L"\r\n\r\n"
            << L"    Dₓ = c₁b₂ - c₂b₁ = " << formatNumber(c1) << L" × " << formatNumber(b2) << L" - "
            << formatNumber(c2) << L" × " << formatNumber(b1) << L" = " << formatNumber(dx) << L"\r\n"
            << L"    Dᵧ = a₁c₂ - a₂c₁ = " << formatNumber(a1) << L" × " << formatNumber(c2) << L" - "
            << formatNumber(a2) << L" × " << formatNumber(c1) << L" = " << formatNumber(dy) << L"\r\n\r\n";

    if (!isZero(d)) {
        const double x = dx / d;
        const double y = dy / d;
        process << L"因为 D ≠ 0，方程组有唯一解：\r\n"
                << L"    x = Dₓ / D = " << formatNumber(dx) << L" / " << formatNumber(d) << L" = "
                << formatNumber(x) << L"\r\n"
                << L"    y = Dᵧ / D = " << formatNumber(dy) << L" / " << formatNumber(d) << L" = "
                << formatNumber(y);
        return {L"唯一解：x = " + formatNumber(x) + L"，y = " + formatNumber(y), process.str()};
    }

    const bool allCoefficientsZero = isZero(a1) && isZero(b1) && isZero(a2) && isZero(b2);
    if (allCoefficientsZero && (!isZero(c1) || !isZero(c2))) {
        process << L"所有未知数的系数都为 0，但右侧常数不全为 0，出现 0 = 非零常数，" << L"因此方程组无解。";
        return {L"结论：方程组无解", process.str()};
    }

    if (!isZero(dx) || !isZero(dy)) {
        process << L"D = 0，但 Dₓ、Dᵧ 不全为 0，两个方程互相矛盾，因此无解。";
        return {L"结论：方程组无解", process.str()};
    }
    process << L"D = Dₓ = Dᵧ = 0，两个方程线性相关，因此有无穷多个解。";
    return {L"结论：方程组有无穷多个解", process.str()};
}

SolveResult solveQuadratic(double a, double b, double c) {
    std::wostringstream process;
    process << L"原方程：" << formatNumber(a) << L"x² + " << formatNumber(b) << L"x + " << formatNumber(c)
            << L" = 0\r\n\r\n";

    if (isZero(a)) {
        process << L"因为 a = 0，该方程退化为一元一次方程。\r\n\r\n";
        SolveResult linear = solveLinear(b, c, 0.0);
        process << linear.process;
        return {L"退化后求解；" + linear.summary, process.str()};
    }

    const double delta = b * b - 4.0 * a * c;
    process << L"第 1 步：计算判别式 Δ：\r\n"
            << L"    Δ = b² - 4ac\r\n"
            << L"      = (" << formatNumber(b) << L")² - 4 × " << formatNumber(a) << L" × " << formatNumber(c)
            << L"\r\n"
            << L"      = " << formatNumber(delta) << L"\r\n\r\n";

    if (delta > kEpsilon) {
        const double rootDelta = std::sqrt(delta);
        double x1 = (-b + rootDelta) / (2.0 * a);
        double x2 = (-b - rootDelta) / (2.0 * a);
        const wchar_t* x1Operator = L"+";
        const wchar_t* x2Operator = L"-";
        if (realRootPriority(x2) < realRootPriority(x1)) {
            std::swap(x1, x2);
            std::swap(x1Operator, x2Operator);
        }
        process << L"因为 Δ > 0，方程有两个不相等的实数根。\r\n"
                << L"第 2 步：代入求根公式 x = (-b ± √Δ) / 2a：\r\n"
                << L"    x₁ = (" << formatNumber(-b) << L" " << x1Operator << L" √" << formatNumber(delta)
                << L") / " << formatNumber(2 * a) << L" = " << formatNumber(x1) << L"\r\n"
                << L"    x₂ = (" << formatNumber(-b) << L" " << x2Operator << L" √" << formatNumber(delta)
                << L") / " << formatNumber(2 * a) << L" = " << formatNumber(x2);
        return {L"两个实根：x₁ = " + formatNumber(x1) + L"，x₂ = " + formatNumber(x2), process.str()};
    }

    if (isZero(delta)) {
        const double x = -b / (2.0 * a);
        process << L"因为 Δ = 0，方程有两个相等的实数根：\r\n"
                << L"    x₁ = x₂ = -b / 2a = " << formatNumber(-b) << L" / " << formatNumber(2 * a) << L" = "
                << formatNumber(x);
        return {L"两个相等实根：x₁ = x₂ = " + formatNumber(x), process.str()};
    }

    const double realPart = -b / (2.0 * a);
    const double imaginaryPart = std::abs(std::sqrt(-delta) / (2.0 * a));
    process << L"因为 Δ < 0，方程没有实数根，但有一对共轭复数根：\r\n"
            << L"    x = -b / 2a ± (√(-Δ) / |2a|)i\r\n"
            << L"      = " << formatNumber(realPart) << L" ± " << formatNumber(imaginaryPart) << L"i";
    return {L"复数根：x₁ = " + formatNumber(realPart) + L" + " + formatNumber(imaginaryPart) + L"i，x₂ = " +
                formatNumber(realPart) + L" - " + formatNumber(imaginaryPart) + L"i",
            process.str()};
}

SolveResult solveLinear3(const std::array<std::array<double, 4>, 3>& equations) {
    // 高斯-约旦消元同时作用于系数矩阵和增广列，并记录每一次初等行变换。
    auto matrix = equations;
    std::wostringstream process;
    process << L"使用高斯消元法。写出原方程组的增广矩阵：\r\n" << matrixText(matrix) << L"\r\n";

    int pivotRow = 0;
    std::array<int, 3> pivotColumns{-1, -1, -1};
    for (int col = 0; col < 3 && pivotRow < 3; ++col) {
        int bestRow = pivotRow;
        for (int row = pivotRow + 1; row < 3; ++row) {
            if (std::abs(matrix[row][col]) > std::abs(matrix[bestRow][col])) {
                bestRow = row;
            }
        }
        if (isZero(matrix[bestRow][col])) {
            continue;
        }

        if (bestRow != pivotRow) {
            std::swap(matrix[bestRow], matrix[pivotRow]);
            process << L"交换第 " << bestRow + 1 << L" 行与第 " << pivotRow + 1 << L" 行。\r\n";
        }

        const double pivot = matrix[pivotRow][col];
        for (int j = col; j < 4; ++j) {
            matrix[pivotRow][j] /= pivot;
            if (isZero(matrix[pivotRow][j]))
                matrix[pivotRow][j] = 0.0;
        }
        process << L"第 " << pivotRow + 1 << L" 行除以主元 " << formatNumber(pivot) << L"；";

        for (int row = 0; row < 3; ++row) {
            if (row == pivotRow || isZero(matrix[row][col]))
                continue;
            const double factor = matrix[row][col];
            for (int j = col; j < 4; ++j) {
                matrix[row][j] -= factor * matrix[pivotRow][j];
                if (isZero(matrix[row][j]))
                    matrix[row][j] = 0.0;
            }
        }
        process << L"再消去该列其余元素：\r\n" << matrixText(matrix) << L"\r\n";
        pivotColumns[pivotRow] = col;
        ++pivotRow;
    }

    int rankA = 0;
    int rankAugmented = 0;
    for (const auto& row : matrix) {
        const bool coefficientNonZero = !isZero(row[0]) || !isZero(row[1]) || !isZero(row[2]);
        if (coefficientNonZero)
            ++rankA;
        if (coefficientNonZero || !isZero(row[3]))
            ++rankAugmented;
    }
    process << L"消元完成：系数矩阵秩 r(A) = " << rankA << L"，增广矩阵秩 r(A|b) = " << rankAugmented
            << L"。\r\n\r\n";

    if (rankA < rankAugmented) {
        process << L"两矩阵的秩不同，出现形如 0 = 非零常数 的矛盾行，所以方程组无解。";
        return {L"结论：三元一次方程组无解", process.str()};
    }
    if (rankA < 3) {
        process << L"两矩阵的秩相同但小于未知数个数 3，因此存在自由变量，方程组有无穷多个解。";
        return {L"结论：三元一次方程组有无穷多个解", process.str()};
    }

    std::array<double, 3> solution{};
    for (int row = 0; row < 3; ++row) {
        if (pivotColumns[row] >= 0) {
            solution[pivotColumns[row]] = matrix[row][3];
        }
    }
    process << L"r(A) = r(A|b) = 3，方程组有唯一解。由最简矩阵直接读出：\r\n"
            << L"    x = " << formatNumber(solution[0]) << L"\r\n"
            << L"    y = " << formatNumber(solution[1]) << L"\r\n"
            << L"    z = " << formatNumber(solution[2]);
    return {L"唯一解：x = " + formatNumber(solution[0]) + L"，y = " + formatNumber(solution[1]) + L"，z = " +
                formatNumber(solution[2]),
            process.str()};
}

SolveResult solveInequality(double a, double b, Relation relation, double c) {
    std::wostringstream process;
    process << L"原不等式：" << formatNumber(a) << L"x + " << formatNumber(b) << L" "
            << relationText(relation) << L" " << formatNumber(c) << L"\r\n\r\n";

    if (isZero(a)) {
        const bool trueForAll = compare(b, relation, c);
        process << L"因为 x 的系数 a = 0，不等式化为：\r\n"
                << L"    " << formatNumber(b) << L" " << relationText(relation) << L" " << formatNumber(c)
                << L"\r\n";
        if (trueForAll) {
            process << L"这个不等式恒成立，所以任意实数 x 都是解。";
            return {L"解集：x ∈ ℝ", process.str()};
        }
        process << L"这个不等式恒不成立，所以解集为空。";
        return {L"解集：∅（无解）", process.str()};
    }

    const double right = c - b;
    const Relation finalRelation = a < 0 ? reverseRelation(relation) : relation;
    const double boundary = right / a;
    process << L"第 1 步：两边同时减去 " << formatNumber(b) << L"：\r\n"
            << L"    " << formatNumber(a) << L"x " << relationText(relation) << L" " << formatNumber(c)
            << L" - " << formatNumber(b) << L"\r\n"
            << L"    " << formatNumber(a) << L"x " << relationText(relation) << L" " << formatNumber(right)
            << L"\r\n\r\n"
            << L"第 2 步：两边同时除以 " << formatNumber(a) << L"。";
    if (a < 0) {
        process << L"因为除数是负数，不等号必须反向：\r\n";
    } else {
        process << L"因为除数是正数，不等号方向不变：\r\n";
    }
    process << L"    x " << relationText(finalRelation) << L" " << formatNumber(right) << L" / "
            << formatNumber(a) << L"\r\n"
            << L"    x " << relationText(finalRelation) << L" " << formatNumber(boundary);
    return {L"解集：x " + relationText(finalRelation) + L" " + formatNumber(boundary), process.str()};
}

} // namespace equation
