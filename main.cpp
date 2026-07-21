// Win32 桌面界面入口：负责三语言、本地主题、历史记录、解法分页、数学结果和图像窗口。
// 数学解析与求解分别委托给 expression_parser、solver、math_view 和 graph_view。

#include "edit_scrollbar.h"
#include "expression_parser.h"
#include "graph_view.h"
#include "math_view.h"

#include <commctrl.h>
#include <dwmapi.h>
#include <gdiplus.h>
#include <uxtheme.h>
#include <windows.h>

#include <algorithm>
#include <array>
#include <cerrno>
#include <complex>
#include <cstdint>
#include <cstdlib>
#include <cwctype>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <limits>
#include <numeric>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

namespace {

constexpr wchar_t kWindowClass[] = L"EquationSolverWindow";
constexpr wchar_t kLogWindowClass[] = L"EquationSolverLogWindow";
constexpr wchar_t kGraphWindowClass[] = L"EquationSolverGraphWindow";
constexpr double kPi = 3.141592653589793238462643383279502884;
constexpr double kEuler = 2.718281828459045235360287471352662498;
constexpr int kTypeComboId = 100;
constexpr int kSolveButtonId = 101;
constexpr int kClearButtonId = 102;
constexpr int kExampleButtonId = 103;
constexpr int kEquationInputId = 104;
constexpr int kThemeButtonId = 105;
constexpr int kLanguageComboId = 106;
constexpr int kHistoryButtonId = 107;
constexpr int kLogClearButtonId = 108;
constexpr int kGraphButtonId = 109;
constexpr int kInputFrameId = 110;
constexpr int kAngleUnitButtonId = 111;
constexpr int kMethodButtonBaseId = 200;
constexpr int kAppIconId = 101;
constexpr DWORD kImmersiveDarkModeAttribute = 20;
constexpr DWORD kWindowCornerPreferenceAttribute = 33;
constexpr DWORD kCaptionColorAttribute = 35;
constexpr DWORD kCaptionTextColorAttribute = 36;

enum class Language { Simplified, English, Traditional };

struct TypeInfo {
    const wchar_t* simplified;
    const wchar_t* english;
    const wchar_t* hintSimplified;
    const wchar_t* hintEnglish;
    const wchar_t* example;
};

constexpr std::array<TypeInfo, 8> kTypes{
    {{L"自动识别", L"Auto detect", L"自动识别方程、不等式或多行方程组",
      L"Automatically detect equations, inequalities, or systems", L"1/(x-1)+1/(x+1)=1"},
     {L"一元一次方程", L"Linear equation", L"支持括号、数值分数和省略乘号",
      L"Supports parentheses, fractions, and implicit multiplication", L"2(x-1)+5=11"},
     {L"二元一次方程组", L"2-variable system", L"每个方程输入一行，也可用分号分隔",
      L"Enter one equation per line, or separate them with semicolons", L"2x+y=7\r\nx-y=2"},
     {L"一元二次方程", L"Quadratic equation", L"使用 ^2 或 ² 表示平方；支持实根和复数根",
      L"Use ^2 or ²; real and complex roots are supported", L"x^2-2x-1=0"},
     {L"三元一次方程组", L"3-variable system", L"输入三行含 x、y、z 的一次方程",
      L"Enter three linear equations containing x, y, and z", L"x+y+z=6\r\n2x-y+z=3\r\nx+2y-z=2"},
     {L"一元一次不等式", L"Linear inequality", L"支持 <、<=、>、>=、≤、≥", L"Supports <, <=, >, >=, ≤, and ≥",
      L"-2x+3<=7"},
     {L"分式方程", L"Rational equation", L"未知数可位于分母；自动列定义域并验根",
      L"Variables may appear in denominators; domain and roots are checked", L"1/(x-1)+1/(x+1)=1"},
     {L"三角方程", L"Trigonometric equation", L"支持弧度制和角度制以及 sin、cos、tan、cot 的周期通解",
      L"Supports radians, degrees, and periodic solutions for sin, cos, tan, and cot", L"sin(x)=1/2"}}};

void replaceAll(std::wstring& text, const std::wstring& from, const std::wstring& to) {
    if (from.empty())
        return;
    size_t position = 0;
    while ((position = text.find(from, position)) != std::wstring::npos) {
        text.replace(position, from.size(), to);
        position += to.size();
    }
}

std::wstring multiplicationForDisplay(std::wstring text) {
    std::replace(text.begin(), text.end(), L'*', L'×');
    return text;
}

std::wstring toTraditional(const std::wstring& text) {
    if (text.empty())
        return text;
    std::wstring result(text.size(), L'\0');
    const int count =
        LCMapStringEx(L"zh-TW", LCMAP_TRADITIONAL_CHINESE, text.c_str(), static_cast<int>(text.size()),
                      result.data(), static_cast<int>(result.size()), nullptr, nullptr, 0);
    if (count <= 0)
        return text;
    result.resize(static_cast<size_t>(count));
    return result;
}

std::wstring toEnglish(std::wstring text) {
    // 求解过程含动态数学值，因此按“最长固定短语优先”替换，以保留公式内容。
    static const std::pair<const wchar_t*, const wchar_t*> replacements[] = {
        // Complete sentences and long phrases must be translated before their
        // shorter components.  This keeps dynamic mathematical values intact
        // while avoiding mixed output such as "row 2 步".
        {L"把 i 作为虚数单位（i² = -1），对复系数增广矩阵进行高斯消元",
         L"Treat i as the imaginary unit (i² = -1) and apply Gaussian elimination to the complex augmented "
         L"matrix"},
        {L"把 i 作为虚数单位（i² = -1），移项、通分并合并同类项",
         L"Treat i as the imaginary unit (i² = -1), rearrange, use a common denominator, and combine like "
         L"terms"},
        {L"把 i 作为虚数单位（i² = -1），高斯消元后出现矛盾行",
         L"Treat i as the imaginary unit (i² = -1). Gaussian elimination produces a contradictory row"},
        {L"把 i 作为虚数单位（i² = -1），高斯消元后存在自由变量",
         L"Treat i as the imaginary unit (i² = -1). Free variables remain after Gaussian elimination"},
        {L"输入原方程组", L"Original system"},
        {L"这是复数范围内的一元一次方程，两边除以 x 的系数",
         L"This is a linear equation over the complex numbers; divide both sides by the coefficient of x"},
        {L"在复数范围内计算判别式并使用求根公式",
         L"Calculate the discriminant over the complex numbers and apply the quadratic formula"},
        {L"复数没有大小关系，含 i 的原式不能使用不等号",
         L"Complex numbers are unordered, so an expression containing i cannot use an inequality sign"},
        {L"含 i 的一元方程目前支持一次或二次方程",
         L"Equations containing i currently support univariate linear and quadratic equations"},
        {L"含 i 的单个方程只能包含一个未知数", L"A single equation containing i can have only one unknown"},
        {L"含 i 的线性方程组需要输入两行或三行方程",
         L"A linear system containing i requires two or three equation lines"},
        {L"含 i 的方程组暂不支持未知数位于分母",
         L"Systems containing i do not currently support an unknown in a denominator"},
        {L"含 i 的方程组中的每个方程都必须是一次方程",
         L"Every equation in a system containing i must be linear"},
        {L"含 i 的方程目前支持整数指数幂", L"Equations containing i currently support integer exponents"},
        {L"含 i 的方程指数绝对值不能超过 8",
         L"For an equation containing i, the exponent magnitude cannot exceed 8"},
        {L"含 i 的方程化简后次数不能超过 8",
         L"After simplification, an equation containing i cannot have degree greater than 8"},
        {L"含 i 的方程中包含无法识别的字符", L"The equation containing i includes an unrecognized character"},
        {L"消元完成后直接读出", L"Read the solution directly after elimination"},
        {L"任意复数均为解", L"every complex number is a solution"},
        {L"方程组有无穷多个复数解", L"The system has infinitely many complex solutions"},
        {L"两个复数根：", L"Two complex roots: "},
        {L"两矩阵的秩相同但小于未知数个数 3，因此存在自由变量，方程组有无穷多个解",
         L"The two matrices have the same rank, but it is less than the three unknowns, so free variables "
         L"exist and the system has infinitely many solutions"},
        {L"两矩阵的秩不同，出现形如 0 = 非零常数 的矛盾行，所以方程组无解",
         L"The matrix ranks differ, producing a contradictory row of the form 0 = nonzero constant, so the "
         L"system has no solution"},
        {L"所有未知数的系数都为 0，但右侧常数不全为 0，出现 0 = 非零常数，因此方程组无解",
         L"All variable coefficients are zero while the constants on the right are not all zero, producing 0 "
         L"= a nonzero constant; therefore the system has no solution"},
        {L"分别代入另外两个方程，得到关于 y、z 的二元方程组",
         L"Substitute into the other two equations to obtain a two-variable system in y and z"},
        {L"判别式 Δ = 4(1 + K²)，代入求根公式并保留根式",
         L"The discriminant is Δ = 4(1 + K²). Apply the quadratic formula and keep the radical form"},
        {L"第 2 步：方程两边同乘公共分母并合并同类项",
         L"Step 2: multiply both sides by the common denominator and combine like terms"},
        {L"分母中不含未知数，直接移项并合并同类项",
         L"The denominator contains no variable; rearrange the equation and combine like terms directly"},
        {L"去分母后等式恒成立，定义域内的所有实数都是解",
         L"After clearing the denominators, the identity always holds; every real number in the domain is a "
         L"solution"},
        {L"公共分母 = 0，不满足定义域，是增根，舍去",
         L"the common denominator is zero, so the value is outside the domain and is rejected as an "
         L"extraneous root"},
        {L"公共分母 ≠ 0，原方程成立，保留",
         L"the common denominator is nonzero, so the original equation holds; keep this root"},
        {L"这个不等式恒不成立，所以解集为空",
         L"This inequality is always false, so its solution set is empty"},
        {L"这个不等式恒成立，所以任意实数 x 都是解",
         L"This inequality is always true, so every real number x is a solution"},
        {L"因为 Δ < 0，方程没有实数根，但有一对共轭复数根",
         L"Because Δ < 0, the equation has no real roots but has a conjugate pair of complex roots"},
        {L"因为 Δ > 0，方程有两个不相等的实数根", L"Because Δ > 0, the equation has two distinct real roots"},
        {L"因为 Δ = 0，方程有两个相等的实数根", L"Because Δ = 0, the equation has two equal real roots"},
        {L"等式恒成立，因此任意实数 x 都满足方程",
         L"The equality is an identity, so every real number x satisfies the equation"},
        {L"等式不成立，因此不存在满足条件的 x",
         L"The equality is false, so no value of x satisfies the equation"},
        {L"因为除数是负数，不等号必须反向",
         L"Because the divisor is negative, the inequality sign must be reversed"},
        {L"因为除数是正数，不等号方向不变",
         L"Because the divisor is positive, the inequality sign remains unchanged"},
        {L"D = Dₓ = Dᵧ = 0，两个方程线性相关，因此有无穷多个解",
         L"D = Dₓ = Dᵧ = 0. The two equations are linearly dependent, so there are infinitely many "
         L"solutions"},
        {L"D = 0，但 Dₓ、Dᵧ 不全为 0，两个方程互相矛盾，因此无解",
         L"D = 0, but Dₓ and Dᵧ are not both zero. The two equations contradict each other, so there is no "
         L"solution"},
        {L"代入消元法与加减消元法所得结果一致",
         L"The substitution and elimination methods give the same result"},
        {L"两种消元方法所得结果一致", L"Both elimination methods give the same result"},
        {L"将所有系数化为最简整数比，使用求根公式",
         L"Reduce all coefficients to the simplest integer ratio and apply the quadratic formula"},
        {L"使用高斯消元法。写出原方程组的增广矩阵",
         L"Use Gaussian elimination. Write the augmented matrix of the original system"},
        {L"使用克拉默法则。先计算系数行列式",
         L"Use Cramer's rule. First calculate the coefficient determinant"},
        {L"解析各行并移项后，得到标准形式",
         L"Parse every row and rearrange each equation into standard form"},
        {L"解析、移项并合并同类项后", L"After parsing, rearranging, and combining like terms"},
        {L"解析、移项并合并同类项", L"Parse, rearrange, and combine like terms"},
        {L"移项、通分并合并同类项", L"Rearrange, use a common denominator, and combine like terms"},
        {L"化简根式后，代入 x = (-b ± √Δ)/(2a)",
         L"After simplifying the radical, substitute into x = (-b ± √Δ)/(2a)"},
        {L"代入求根公式并约分", L"Substitute into the quadratic formula and reduce the fractions"},
        {L"解一元一次方程，得到候选根", L"Solve the linear equation to obtain the candidate root"},
        {L"将边界值约分，得到最简分数", L"Reduce the boundary value to its simplest fraction"},
        {L"将系数化为最简整数比后", L"After reducing the coefficients to the simplest integer ratio"},
        {L"因为 x 的系数 a = 0，方程化为", L"Because the coefficient a of x is zero, the equation becomes"},
        {L"因为 x 的系数 a = 0，不等式化为",
         L"Because the coefficient a of x is zero, the inequality becomes"},
        {L"因为 a = 0，该方程退化为一元一次方程",
         L"Because a = 0, this equation reduces to a linear equation"},
        {L"因为 D ≠ 0，方程组有唯一解", L"Because D ≠ 0, the system has a unique solution"},
        {L"消元完成：系数矩阵秩 r(A) = ", L"Elimination is complete: coefficient-matrix rank r(A) = "},
        {L"，增广矩阵秩 r(A|b) = ", L", augmented-matrix rank r(A|b) = "},
        {L"r(A) = r(A|b) = 3，方程组有唯一解。由最简矩阵直接读出",
         L"r(A) = r(A|b) = 3, so the system has a unique solution. Read it directly from the reduced matrix"},
        {L"再消去该列其余元素", L"Then eliminate the remaining entries in this column"},
        {L"方程没有实数候选根", L"The equation has no real candidate roots"},
        {L"定义域内的所有实数", L"all real numbers in the domain"},
        {L"所有分母均不能为 0", L"every denominator must be nonzero"},
        {L"结果保留最简分数和根式", L"The result is kept in simplified fraction and radical form"},
        {L"等式恒成立，但仍需满足分数指数及分母的定义域",
         L"The equality is an identity, but the domain restrictions from fractional exponents and "
         L"denominators still apply"},
        {L"等式不成立，因此原方程无解", L"The equality is false, so the original equation has no solution"},
        {L"换元方程没有满足条件的实数根",
         L"The substituted equation has no real root satisfying the conditions"},
        {L"换元后，原方程化为关于 t 的方程",
         L"After substitution, the original equation becomes an equation in t"},
        {L"换元后得到一般高次方程。用导数划分单调区间，再以二分法求出全部实根",
         L"After substitution, this is a general higher-degree equation. Use the derivative to divide it "
         L"into monotonic intervals, then find every real root by bisection"},
        {L"以下结果为经过原式验证的数值近似",
         L"The following numerical approximations have been checked in the original equation"},
        {L"因为是偶次根，还必须满足 t ≥ 0", L"Because this is an even root, t must also satisfy t ≥ 0"},
        {L"含分数指数，令 t = ", L"For the fractional exponent, let t = "},
        {L"，则 x = t^", L", so x = t^"},
        {L"检验换元根 t = ", L"Check the substituted root t = "},
        {L"不满足 t ≥ 0，舍去", L"it does not satisfy t ≥ 0, so reject it"},
        {L"使原式分母为 0，舍去", L"it makes an original denominator zero, so reject it"},
        {L"满足定义域，由 x = t^", L"it satisfies the domain; from x = t^"},
        {L" 得 x = ", L", obtain x = "},
        {L" 得 x ≈ ", L", obtain x ≈ "},
        {L"（与已有解相同）", L" (the same as an existing solution)"},
        {L"实数解：", L"Real solutions: "},
        {L"数值解：", L"Numerical solution: "},
        {L"根据绝对值内部表达式的正负，分情况去掉绝对值符号",
         L"Split into cases according to the sign of each expression inside an absolute value"},
        {L"满足本分支条件，保留", L"it satisfies the conditions for this case; keep it"},
        {L"不满足本分支条件，舍去", L"it does not satisfy the conditions for this case; reject it"},
        {L"去掉绝对值后", L"After removing the absolute value"},
        {L"该分支无解", L"This case has no solution"},
        {L"验根 x ", L"Check x "},
        {L"情况 ", L"Case "},

        // Parser errors for exponent input.
        {L"指数格式不正确，请使用如 ^-2、^(1/2) 或 ^(-2/3) 的格式",
         L"Invalid exponent format. Use forms such as ^-2, ^(1/2), or ^(-2/3)"},
        {L"含未知数的三角方程目前支持 sin(ax+b)=c、cos(ax+b)=c、tan(ax+b)=c 或 cot(ax+b)=c",
         L"Trigonometric equations containing a variable currently support sin(ax+b)=c, cos(ax+b)=c, "
         L"tan(ax+b)=c, or cot(ax+b)=c"},
        {L"三角方程请输入 sin(ax+b)=c、cos(ax+b)=c、tan(ax+b)=c 或 cot(ax+b)=c",
         L"Enter a trigonometric equation as sin(ax+b)=c, cos(ax+b)=c, tan(ax+b)=c, or cot(ax+b)=c"},
        {L"三角函数按角度制计算。令 u = ", L"Trigonometric functions are evaluated in degrees. Let u = "},
        {L"三角函数按弧度制计算。令 u = ", L"Trigonometric functions are evaluated in radians. Let u = "},
        {L"令 α = ", L"Let α = "},
        {L"因为 ", L"Because "},
        {L"sin u = sin α，所以 u = α + 2kπ 或 u = π - α + 2kπ",
         L"sin u = sin α, so u = α + 2kπ or u = π - α + 2kπ"},
        {L"sin u = sin α，所以 u = α + 360k° 或 u = 180° - α + 360k°",
         L"sin u = sin α, so u = α + 360k° or u = 180° - α + 360k°"},
        {L"cos u = cos α，所以 u = 360k° ± α", L"cos u = cos α, so u = 360k° ± α"},
        {L"cos u = cos α，所以 u = 2kπ ± α", L"cos u = cos α, so u = 2kπ ± α"},
        {L"sin u = 0，所以 u = 180k°", L"sin u = 0, so u = 180k°"},
        {L"sin u = 0，所以 u = kπ", L"sin u = 0, so u = kπ"},
        {L"解出 x，得到上述通解，其中 k ∈ ℤ", L"Solving for x gives the general solution above, where k ∈ ℤ"},
        {L"，或 ", L", or "},
        {L" 的值域是 [-1, 1]，所以方程无实数解", L" has range [-1, 1], the equation has no real solution"},
        {L"，所以 u = α + kπ", L", so u = α + kπ"},
        {L"，所以 u = α + 180k°", L", so u = α + 180k°"},
        {L"三角函数参数中出现除以 0", L"Division by zero occurs in the trigonometric-function argument"},
        {L"三角函数后需要使用括号", L"A trigonometric function must be followed by brackets"},
        {L"三角函数缺少右括号", L"The trigonometric function is missing its closing bracket"},
        {L"tan 在该角度无定义", L"tan is undefined at this angle"},
        {L"cot 在该角度无定义", L"cot is undefined at this angle"},
        {L"该角度处函数无定义", L"The function is undefined at this angle"},
        {L"等式恒不成立", L"The equality is never true"},
        {L"括号嵌套顺序错误：只能大括号套中括号、中括号套小括号，同种括号不能互相嵌套",
         L"Invalid bracket nesting: braces may contain square brackets, square brackets may contain "
         L"parentheses, and brackets of the same kind cannot be nested"},
        {L"括号不匹配", L"Mismatched brackets"},
        {L"含未知数的表达式不能直接取阶乘",
         L"An expression containing an unknown cannot be used directly with factorial"},
        {L"阶乘只能用于非负整数", L"Factorial can only be applied to a non-negative integer"},
        {L"阶乘参数不能超过 170", L"The factorial argument cannot exceed 170"},
        {L"角度单位已切换，请重新求解", L"The angle unit has changed; solve the equation again"},
        {L"指数的绝对值不能超过 32，分母不能超过 16",
         L"The absolute exponent numerator cannot exceed 32, and its denominator cannot exceed 16"},
        {L"分数指数的公分母不能超过 16", L"The common denominator of fractional exponents cannot exceed 16"},
        {L"分数指数不等式超出当前一元一次不等式的支持范围",
         L"Fractional-exponent inequalities are outside the scope of the current linear-inequality solver"},
        {L"分数指数目前支持数值或单项式；含加减法的底数请先换元化简",
         L"Fractional exponents currently support numeric values or monomials; simplify bases containing "
         L"addition or subtraction first"},
        {L"该分数指数无法化为当前支持的单项式形式",
         L"This fractional exponent cannot be reduced to a supported monomial form"},
        {L"指数运算结果超出可处理范围", L"The exponentiation result is outside the supported range"},
        {L"换元后的方程次数不能超过 32", L"The substituted equation cannot have degree greater than 32"},
        {L"换元后超过二次的方程目前需要是 a·t^n + b = 0 的形式",
         L"After substitution, equations above degree two must currently have the form a·t^n + b = 0"},
        {L"这是单项幂方程，直接开 ", L"This is a monomial power equation; take the "},
        {L" 次方求出 t", L"th root directly to solve for t"},
        {L"偶数次根的底数不能为负数", L"The base of an even root cannot be negative"},
        {L"0 的 0 次幂没有定义", L"0 to the zeroth power is undefined"},
        {L"0 不能取零次幂或负数指数幂", L"Zero cannot be raised to the zeroth or a negative power"},
        {L"0 不能取负数指数幂", L"Zero cannot be raised to a negative power"},
        {L"绝对值符号 || 中间不能为空", L"The absolute-value bars || cannot have empty content"},
        {L"绝对值内部暂不支持含未知数的分数指数",
         L"Fractional powers containing a variable are not currently supported inside an absolute value"},
        {L"绝对值缺少右侧竖线 |", L"The absolute value is missing its closing vertical bar |"},
        {L"一条方程最多支持 4 组绝对值", L"An equation can contain at most four absolute-value groups"},
        {L"原式中没有完整的绝对值符号 | |",
         L"The expression has no complete pair of absolute-value bars | |"},
        {L"当前绝对值求解支持方程，暂不支持绝对值不等式",
         L"Absolute-value equations are supported, but absolute-value inequalities are not yet supported"},
        {L"去掉绝对值后的方程次数不能超过 16",
         L"After removing absolute values, the equation degree cannot exceed 16"},
        {L"该绝对值方程含有恒等分支，暂无法用有限个根表示解集",
         L"This absolute-value equation contains an identity case that cannot be represented by finitely "
         L"many roots"},
        {L"根式缺少右侧双引号 \"", L"The radical is missing its closing double quote \""},
        {L"双引号根式中的被开方数需要是整数、小数、分数或数值绝对值",
         L"The radicand between double quotes must be an integer, decimal, fraction, or numeric absolute "
         L"value"},
        {L"双引号根式中的被开方数需要是整数、小数或分数",
         L"The radicand between double quotes must be an integer, decimal, or fraction"},

        // Numbered steps, rows, and equations are kept explicit so ordinary
        // Chinese characters are never replaced out of context.
        {L"第 1 步：确定定义域", L"Step 1: determine the domain"},
        {L"第 1 步：计算判别式", L"Step 1: calculate the discriminant"},
        {L"第 2 步：代入求根公式", L"Step 2: apply the quadratic formula"},
        {L"第 1 步：两边同时减去", L"Step 1: subtract from both sides"},
        {L"第 2 步：两边同时除以", L"Step 2: divide both sides by"},
        {L"从第 1 个方程解出 x", L"From equation 1, solve for x"},
        {L"从第 2 个方程解出 x", L"From equation 2, solve for x"},
        {L"从第 3 个方程解出 x", L"From equation 3, solve for x"},
        {L"交换第 1 行与第 2 行", L"Swap row 1 with row 2"},
        {L"交换第 1 行与第 3 行", L"Swap row 1 with row 3"},
        {L"交换第 2 行与第 1 行", L"Swap row 2 with row 1"},
        {L"交换第 2 行与第 3 行", L"Swap row 2 with row 3"},
        {L"交换第 3 行与第 1 行", L"Swap row 3 with row 1"},
        {L"交换第 3 行与第 2 行", L"Swap row 3 with row 2"},
        {L"第 1 行除以主元 ", L"Divide row 1 by pivot "},
        {L"第 2 行除以主元 ", L"Divide row 2 by pivot "},
        {L"第 3 行除以主元 ", L"Divide row 3 by pivot "},

        // Method descriptions with dynamic coefficients or equations.
        {L"将 (1) 乘以 ", L"Multiply (1) by "},
        {L"，将 (2) 乘以 ", L", and multiply (2) by "},
        {L"，两式相减以消去 y", L"; subtract the equations to eliminate y"},
        {L"把 x = ", L"Substitute x = "},
        {L" 代回原方程，得到 y = ", L" back into the original equation to obtain y = "},
        {L"代入另一个方程 ", L"Substitute into the other equation "},
        {L"，解得", L"; solving gives"},
        {L"再代回上式，x = ", L"Substitute back into the expression to obtain x = "},
        {L"解这个二元方程组：", L"Solve this two-variable system: "},
        {L"再代回 x 的表达式：", L"Substitute back into the expression for x: "},
        {L"由 ", L"From "},
        {L" 解出：", L", solve for:"},

        // Rational-equation wording with symbolic values inserted between the
        // prefix and suffix.
        {L"令 K = ", L"Let K = "},
        {L"，去分母后的方程精确写为", L". After clearing the denominators, the exact equation is"},
        {L"验根：x = ", L"Root check: x = "},
        {L" 时，", L": "},

        {L"唯一解：", L"Unique solution: "},
        {L"两个相等实根：", L"Two equal real roots: "},
        {L"两个实根：", L"Two real roots: "},
        {L"复数根：", L"Complex roots: "},
        {L"解集：", L"Solution set: "},
        {L"结论：", L"Conclusion: "},
        {L"方法一：加减消元法（高斯消元）", L"Method 1: elimination (Gaussian elimination)"},
        {L"方法二：加减消元法", L"Method 2: addition/subtraction elimination"},
        {L"方法二：代入消元法", L"Method 2: substitution"},
        {L"方法三：代入消元法", L"Method 3: substitution"},
        {L"输入原方程 / 方程组", L"Equation / system"},
        {L"输入方程组", L"Input system"},
        {L"输入原式", L"Original input"},
        {L"原方程组", L"Original system"},
        {L"原不等式", L"Original inequality"},
        {L"原方程", L"Original equation"},
        {L"求解结果", L"Result"},
        {L"三角方程", L"Trigonometric equation"},
        {L"通解", L"General solution"},
        {L"近似值：", L"Approx.: "},
        {L"数值：", L"Values: "},
        {L"唯一解", L"Unique solution"},
        {L"两个相等实根", L"Two equal real roots"},
        {L"两个实根", L"Two real roots"},
        {L"复数根", L"Complex roots"},
        {L"解集", L"Solution set"},
        {L"结论", L"Conclusion"},
        {L"三元一次方程组无解", L"the three-variable system has no solution"},
        {L"三元一次方程组有无穷多个解", L"the three-variable system has infinitely many solutions"},
        {L"方程组有无穷多个解", L"The system has infinitely many solutions"},
        {L"方程组无解", L"The system has no solution"},
        {L"分式方程无实数解", L"The rational equation has no real solution"},
        {L"方程无实数解", L"The equation has no real solution"},
        {L"三元一次方程组", L"3-variable linear system"},
        {L"二元一次方程组", L"2-variable linear system"},
        {L"一元二次方程", L"Quadratic equation"},
        {L"一元一次不等式", L"Linear inequality"},
        {L"一元一次方程", L"Linear equation"},
        {L"分式方程", L"Rational equation"},
        {L"绝对值方程", L"Absolute-value equation"},
        {L"指数方程", L"Exponential equation"},
        {L"退化后求解", L"Solve the reduced equation"},
        {L"有无穷多个解", L"infinitely many solutions"},
        {L"方程无解", L"the equation has no solution"},
        {L"解：", L"Solutions: "},
        {L"检验：", L"Check: "},
        {L"成立", L"valid"},
        {L"无解", L"no solution"},
        {L"验根", L"Check the root"},
        {L"不满足定义域，是增根，舍去", L"outside the domain; reject this extraneous root"},
        {L"原方程成立，保留", L"the original equation holds; keep it"},
        {L"判别式", L"Discriminant"},
        {L"求根公式", L"quadratic formula"},
        {L"移项并合并同类项", L"Rearrange and combine like terms"},
        {L"解得", L"Solving gives"},
        {L"代入另一个方程", L"Substitute into the other equation"},
        {L"代回原方程", L"substitute back into the original equation"},
        {L"再代回上式", L"substitute back into the expression"},
        {L"解出", L"solve for"},
        {L"两式相减以消去", L"subtract the equations to eliminate"},
        {L"代回原方程", L"substitute back into the original equation"},
        {L"不存在满足条件的 x", L"no value of x satisfies the condition"},
        {L"不满足定义域，是增根，舍去", L"outside the domain; reject this extraneous root"},
        {L"原方程成立，保留", L"the original equation holds; keep this root"},
        {L"，", L", "},
        {L"。", L"."},
        {L"：", L":"},
        {L"；", L"; "}};
    for (const auto& [from, to] : replacements)
        replaceAll(text, from, to);

    // Safety net: English mode must never display untranslated CJK text.  The
    // dictionary above covers every solver path; this fallback only guards new
    // messages added in the future, preserving mathematical symbols and values.
    std::wstring clean;
    clean.reserve(text.size());
    bool inUntranslatedRun = false;
    for (const wchar_t ch : text) {
        const bool isCjk = (ch >= 0x3400 && ch <= 0x9FFF) || (ch >= 0xF900 && ch <= 0xFAFF);
        if (isCjk) {
            if (!inUntranslatedRun)
                clean += L"[untranslated]";
            inUntranslatedRun = true;
        } else {
            clean += ch;
            inUntranslatedRun = false;
        }
    }
    text = std::move(clean);
    return text;
}

// 只把最终符号答案换算为近似值，不参与原方程的求解决策。
class NumericMathParser {
  public:
    NumericMathParser(std::wstring text, std::complex<double> kValue = {},
                      equation::AngleUnit angleUnit = equation::AngleUnit::Radians)
        : text_(std::move(text)), kValue_(kValue), angleUnit_(angleUnit) {
        std::replace(text_.begin(), text_.end(), L'！', L'!');
    }

    std::optional<std::complex<double>> parse() {
        position_ = 0;
        valid_ = true;
        const auto value = parseExpression();
        skipSpaces();
        if (!valid_ || position_ != text_.size())
            return std::nullopt;
        return value;
    }

  private:
    using Complex = std::complex<double>;

    void skipSpaces() {
        while (position_ < text_.size() && std::iswspace(text_[position_]))
            ++position_;
    }

    bool consume(wchar_t ch) {
        skipSpaces();
        if (position_ < text_.size() && text_[position_] == ch) {
            ++position_;
            return true;
        }
        return false;
    }

    bool startsPrimary() {
        skipSpaces();
        if (position_ >= text_.size())
            return false;
        const wchar_t ch = text_[position_];
        return std::iswdigit(ch) || ch == L'.' || ch == L'(' || ch == L'{' || ch == L'[' || ch == L'【' ||
               ch == L'√' || ch == L'∛' || ch == L'K' || ch == L'i' || ch == L'p' || ch == L'P' ||
               ch == L'π' || ch == L'e' || ch == L'E' || ch == L's' || ch == L'S' || ch == L'c' ||
               ch == L'C' || ch == L't' || ch == L'T';
    }

    Complex parseExpression() {
        Complex value = parseTerm();
        while (valid_) {
            if (consume(L'+'))
                value += parseTerm();
            else if (consume(L'-'))
                value -= parseTerm();
            else
                break;
        }
        return value;
    }

    Complex parseTerm() {
        Complex value = parseUnary();
        while (valid_) {
            if (consume(L'*') || consume(L'×') || consume(L'·'))
                value *= parseUnary();
            else if (consume(L'/'))
                value /= parseUnary();
            else if (startsPrimary())
                value *= parseUnary();
            else
                break;
        }
        return value;
    }

    Complex parseUnary() {
        if (consume(L'+'))
            return parseUnary();
        if (consume(L'-'))
            return -parseUnary();
        Complex value = parsePrimary();
        while (consume(L'!')) {
            if (std::abs(value.imag()) >= 1e-12 || value.real() < 0.0 || value.real() > 170.0 ||
                std::abs(value.real() - std::round(value.real())) >= 1e-10) {
                valid_ = false;
                return {};
            }
            double factorial = 1.0;
            for (int factor = 2; factor <= static_cast<int>(std::round(value.real())); ++factor)
                factorial *= factor;
            value = factorial;
        }
        while (consume(L'²'))
            value *= value;
        return value;
    }

    Complex parsePrimary() {
        skipSpaces();
        if (position_ >= text_.size()) {
            valid_ = false;
            return {};
        }
        const wchar_t ch = text_[position_];
        if (std::iswdigit(ch) || ch == L'.') {
            wchar_t* end = nullptr;
            const double value = std::wcstod(text_.c_str() + position_, &end);
            if (end == text_.c_str() + position_) {
                valid_ = false;
                return {};
            }
            position_ = static_cast<size_t>(end - text_.c_str());
            return value;
        }
        auto functionAt = [&](const wchar_t* name) {
            for (size_t offset = 0; offset < 3; ++offset) {
                if (position_ + offset >= text_.size() ||
                    std::towlower(text_[position_ + offset]) != name[offset])
                    return false;
            }
            size_t after = position_ + 3;
            while (after < text_.size() && std::iswspace(text_[after]))
                ++after;
            return after < text_.size() && (text_[after] == L'(' || text_[after] == L'[' ||
                                            text_[after] == L'{' || text_[after] == L'【');
        };
        const wchar_t* function =
            functionAt(L"sin")
                ? L"sin"
                : (functionAt(L"cos")
                       ? L"cos"
                       : (functionAt(L"tan") ? L"tan" : (functionAt(L"cot") ? L"cot" : nullptr)));
        if (function) {
            position_ += 3;
            skipSpaces();
            if (position_ >= text_.size()) {
                valid_ = false;
                return {};
            }
            const wchar_t opening = text_[position_++];
            const wchar_t closing =
                opening == L'(' ? L')' : (opening == L'[' ? L']' : (opening == L'{' ? L'}' : L'】'));
            const Complex inputAngle = parseExpression();
            if (!consume(closing)) {
                valid_ = false;
                return {};
            }
            const Complex angle =
                angleUnit_ == equation::AngleUnit::Degrees ? inputAngle * (kPi / 180.0) : inputAngle;
            if (function[0] == L's')
                return std::sin(angle);
            if (function[0] == L'c' && function[2] == L's')
                return std::cos(angle);
            if (function[0] == L't')
                return std::tan(angle);
            const Complex sine = std::sin(angle);
            if (std::abs(sine) < 1e-12) {
                valid_ = false;
                return {};
            }
            return std::cos(angle) / sine;
        }
        if (ch == L'π' || ((ch == L'p' || ch == L'P') && position_ + 1 < text_.size() &&
                           (text_[position_ + 1] == L'i' || text_[position_ + 1] == L'I'))) {
            position_ += ch == L'π' ? 1 : 2;
            return kPi;
        }
        if (ch == L'e' || ch == L'E') {
            ++position_;
            return kEuler;
        }
        if (ch == L'K') {
            ++position_;
            return kValue_;
        }
        if (ch == L'i') {
            ++position_;
            return {0.0, 1.0};
        }
        if (ch == L'(' || ch == L'{' || ch == L'[' || ch == L'【') {
            ++position_;
            const wchar_t closing = ch == L'(' ? L')' : (ch == L'{' ? L'}' : (ch == L'[' ? L']' : L'】'));
            Complex value = parseExpression();
            if (!consume(closing))
                valid_ = false;
            return value;
        }
        if (ch == L'√' || ch == L'∛') {
            ++position_;
            int degree = ch == L'∛' ? 3 : 2;
            if (consume(L'[')) {
                skipSpaces();
                wchar_t* end = nullptr;
                const long parsedDegree = std::wcstol(text_.c_str() + position_, &end, 10);
                if (end == text_.c_str() + position_ || parsedDegree < 2 || parsedDegree > 32) {
                    valid_ = false;
                    return {};
                }
                position_ = static_cast<size_t>(end - text_.c_str());
                if (!consume(L']')) {
                    valid_ = false;
                    return {};
                }
                degree = static_cast<int>(parsedDegree);
            }
            Complex radicand = parsePrimary();
            if (std::abs(radicand.imag()) < 1e-12 && radicand.real() < 0.0 && degree % 2 == 1) {
                return -std::pow(-radicand.real(), 1.0 / degree);
            }
            return std::pow(radicand, 1.0 / degree);
        }
        valid_ = false;
        return {};
    }

    std::wstring text_;
    size_t position_ = 0;
    bool valid_ = true;
    Complex kValue_{};
    equation::AngleUnit angleUnit_ = equation::AngleUnit::Radians;
};

std::wstring twoDecimal(double value) {
    if (std::abs(value) < 0.0005)
        value = 0.0;
    std::wostringstream out;
    out << std::fixed << std::setprecision(2) << value;
    return out.str();
}

std::optional<std::wstring> terminatingDecimal(std::wstring expression) {
    const auto trim = [](std::wstring text) {
        const size_t begin = text.find_first_not_of(L" \t\r\n");
        if (begin == std::wstring::npos)
            return std::wstring{};
        const size_t end = text.find_last_not_of(L" \t\r\n");
        return text.substr(begin, end - begin + 1);
    };
    expression = trim(std::move(expression));
    while (expression.size() >= 2 && expression.front() == L'(' && expression.back() == L')') {
        expression = trim(expression.substr(1, expression.size() - 2));
    }
    auto parseInteger = [&](const std::wstring& text) -> std::optional<std::int64_t> {
        const std::wstring value = trim(text);
        if (value.empty())
            return std::nullopt;
        errno = 0;
        wchar_t* end = nullptr;
        const long long parsed = std::wcstoll(value.c_str(), &end, 10);
        if (errno == ERANGE || end == value.c_str() || !end || *end != L'\0')
            return std::nullopt;
        return static_cast<std::int64_t>(parsed);
    };

    const size_t slash = expression.find(L'/');
    if (slash == std::wstring::npos) {
        const auto integer = parseInteger(expression);
        return integer ? std::optional<std::wstring>(std::to_wstring(*integer)) : std::nullopt;
    }
    if (expression.find(L'/', slash + 1) != std::wstring::npos)
        return std::nullopt;
    const auto numeratorValue = parseInteger(expression.substr(0, slash));
    const auto denominatorValue = parseInteger(expression.substr(slash + 1));
    if (!numeratorValue || !denominatorValue || *denominatorValue == 0)
        return std::nullopt;

    const bool negative = (*numeratorValue < 0) != (*denominatorValue < 0);
    auto magnitude = [](std::int64_t value) -> std::uint64_t {
        return value < 0 ? static_cast<std::uint64_t>(-(value + 1)) + 1 : static_cast<std::uint64_t>(value);
    };
    std::uint64_t numerator = magnitude(*numeratorValue);
    std::uint64_t denominator = magnitude(*denominatorValue);
    const std::uint64_t divisor = std::gcd(numerator, denominator);
    numerator /= divisor;
    denominator /= divisor;
    std::uint64_t remainingDenominator = denominator;
    while (remainingDenominator % 2 == 0)
        remainingDenominator /= 2;
    while (remainingDenominator % 5 == 0)
        remainingDenominator /= 5;
    if (remainingDenominator != 1)
        return std::nullopt;

    const std::uint64_t integerPart = numerator / denominator;
    std::uint64_t remainder = numerator % denominator;
    std::wstring result = negative && numerator != 0 ? L"-" : L"";
    result += std::to_wstring(integerPart);
    if (remainder == 0)
        return result;
    result += L'.';
    while (remainder != 0) {
#if defined(__GNUC__)
        __extension__ using WideUnsigned = unsigned __int128;
        const WideUnsigned scaled = static_cast<WideUnsigned>(remainder) * 10;
#else
        if (remainder > std::numeric_limits<std::uint64_t>::max() / 10)
            return std::nullopt;
        const std::uint64_t scaled = remainder * 10;
#endif
        result += static_cast<wchar_t>(L'0' + static_cast<int>(scaled / denominator));
        remainder = static_cast<std::uint64_t>(scaled % denominator);
    }
    return result;
}

std::wstring approximationForSummary(const std::wstring& summary,
                                     equation::AngleUnit angleUnit = equation::AngleUnit::Radians) {
    if (summary.find(L'≈') != std::wstring::npos ||
        (summary.find(L'/') == std::wstring::npos && summary.find(L'√') == std::wstring::npos &&
         summary.find(L'∛') == std::wstring::npos))
        return L"";

    std::complex<double> kValue{};
    const size_t kPosition = summary.find(L"K = ");
    if (kPosition != std::wstring::npos) {
        const size_t begin = kPosition + 4;
        const size_t end = summary.find_first_of(L"；;,，", begin);
        const auto parsed = NumericMathParser(summary.substr(begin, end - begin), {}, angleUnit).parse();
        if (parsed)
            kValue = *parsed;
    }

    struct Item {
        size_t position;
        std::wstring label;
        std::wstring expression;
        std::complex<double> value;
    };
    std::vector<Item> items;
    std::set<size_t> usedPositions;
    const std::array<const wchar_t*, 7> labels{L"x₁", L"x₂", L"x1", L"x2", L"x", L"y", L"z"};
    for (const wchar_t* rawLabel : labels) {
        const std::wstring label(rawLabel);
        const std::wstring pattern = label + L" = ";
        size_t search = 0;
        while ((search = summary.find(pattern, search)) != std::wstring::npos) {
            const size_t begin = search + pattern.size();
            const size_t end = summary.find_first_of(L"，,；;\r\n", begin);
            const std::wstring expression =
                summary.substr(begin, (end == std::wstring::npos ? summary.size() : end) - begin);
            if (expression.find(L'=') == std::wstring::npos) {
                const auto parsed = NumericMathParser(expression, kValue, angleUnit).parse();
                if (parsed && usedPositions.insert(search).second) {
                    items.push_back({search, label, expression, *parsed});
                }
            }
            search = begin;
        }
    }
    std::sort(items.begin(), items.end(),
              [](const Item& left, const Item& right) { return left.position < right.position; });

    if (items.empty()) {
        const size_t relation = summary.find_first_of(L"<>≤≥");
        if (relation != std::wstring::npos) {
            const auto parsed = NumericMathParser(summary.substr(relation + 1), kValue, angleUnit).parse();
            if (parsed) {
                const std::wstring expression = summary.substr(relation + 1);
                items.push_back({relation, L"x", expression, *parsed});
            }
        }
    }
    if (items.empty())
        return L"";

    std::wstring result = L"数值：";
    for (size_t i = 0; i < items.size(); ++i) {
        if (i)
            result += L"，";
        const auto exactDecimal =
            std::abs(items[i].value.imag()) < 0.0005 ? terminatingDecimal(items[i].expression) : std::nullopt;
        result += items[i].label;
        if (exactDecimal) {
            result += L" = " + *exactDecimal;
        } else {
            result += L" ≈ " + twoDecimal(items[i].value.real());
        }
        if (!exactDecimal && std::abs(items[i].value.imag()) >= 0.0005) {
            result += items[i].value.imag() >= 0 ? L" + " : L" - ";
            result += twoDecimal(std::abs(items[i].value.imag())) + L"i";
        }
    }
    return result;
}

std::vector<std::pair<std::wstring, double>> realValuesForGraph(const std::wstring& summary,
                                                                equation::AngleUnit angleUnit) {
    std::complex<double> kValue{};
    const size_t kPosition = summary.find(L"K = ");
    if (kPosition != std::wstring::npos) {
        const size_t begin = kPosition + 4;
        const size_t end = summary.find_first_of(L"；;,，", begin);
        const auto parsed = NumericMathParser(summary.substr(begin, end - begin), {}, angleUnit).parse();
        if (parsed)
            kValue = *parsed;
    }
    struct Item {
        size_t position;
        std::wstring label;
        double value;
    };
    std::vector<Item> items;
    std::set<size_t> used;
    const std::array<const wchar_t*, 7> labels{L"x₁", L"x₂", L"x1", L"x2", L"x", L"y", L"z"};
    for (const wchar_t* rawLabel : labels) {
        const std::wstring label(rawLabel);
        for (const std::wstring relation : {L" = ", L" ≈ "}) {
            const std::wstring pattern = label + relation;
            size_t search = 0;
            while ((search = summary.find(pattern, search)) != std::wstring::npos) {
                const size_t begin = search + pattern.size();
                const size_t end = summary.find_first_of(L"，,；;\r\n", begin);
                const std::wstring expression =
                    summary.substr(begin, (end == std::wstring::npos ? summary.size() : end) - begin);
                if (expression.find(L'=') == std::wstring::npos) {
                    const auto parsed = NumericMathParser(expression, kValue, angleUnit).parse();
                    if (parsed && std::abs(parsed->imag()) < 1e-8 && used.insert(search).second) {
                        items.push_back({search, label, parsed->real()});
                    }
                }
                search = begin;
            }
        }
    }
    std::sort(items.begin(), items.end(),
              [](const Item& left, const Item& right) { return left.position < right.position; });
    std::vector<std::pair<std::wstring, double>> result;
    for (const auto& item : items)
        result.push_back({item.label, item.value});
    return result;
}

struct HistoryEntry {
    std::wstring time;
    std::wstring type;
    std::wstring equation;
};

std::string wideToUtf8(const std::wstring& text) {
    if (text.empty())
        return {};
    const int size = WideCharToMultiByte(CP_UTF8, 0, text.c_str(), static_cast<int>(text.size()), nullptr, 0,
                                         nullptr, nullptr);
    std::string result(static_cast<size_t>(std::max(0, size)), '\0');
    if (size > 0)
        WideCharToMultiByte(CP_UTF8, 0, text.c_str(), static_cast<int>(text.size()), result.data(), size,
                            nullptr, nullptr);
    return result;
}

std::wstring utf8ToWide(const std::string& text) {
    if (text.empty())
        return {};
    const int size = MultiByteToWideChar(CP_UTF8, 0, text.c_str(), static_cast<int>(text.size()), nullptr, 0);
    std::wstring result(static_cast<size_t>(std::max(0, size)), L'\0');
    if (size > 0)
        MultiByteToWideChar(CP_UTF8, 0, text.c_str(), static_cast<int>(text.size()), result.data(), size);
    return result;
}

std::string escapeHistoryField(const std::wstring& text) {
    const std::string utf8 = wideToUtf8(text);
    std::string result;
    result.reserve(utf8.size());
    for (char ch : utf8) {
        if (ch == '\\')
            result += "\\\\";
        else if (ch == '\t')
            result += "\\t";
        else if (ch == '\r')
            result += "\\r";
        else if (ch == '\n')
            result += "\\n";
        else
            result += ch;
    }
    return result;
}

std::wstring unescapeHistoryField(const std::string& text) {
    std::string result;
    result.reserve(text.size());
    for (size_t i = 0; i < text.size(); ++i) {
        if (text[i] != '\\' || i + 1 >= text.size()) {
            result += text[i];
            continue;
        }
        const char next = text[++i];
        if (next == 't')
            result += '\t';
        else if (next == 'r')
            result += '\r';
        else if (next == 'n')
            result += '\n';
        else
            result += next;
    }
    return utf8ToWide(result);
}

std::filesystem::path historyFilePath() {
    std::wstring base(32768, L'\0');
    const DWORD count =
        GetEnvironmentVariableW(L"LOCALAPPDATA", base.data(), static_cast<DWORD>(base.size()));
    if (count == 0 || count >= base.size()) {
        base.resize(MAX_PATH, L'\0');
        const DWORD temporaryCount = GetTempPathW(static_cast<DWORD>(base.size()), base.data());
        base.resize(temporaryCount > 0 && temporaryCount < base.size() ? temporaryCount : 0);
    } else {
        base.resize(count);
    }
    return std::filesystem::path(base) / L"EquationSolver" / L"history.tsv";
}

std::vector<HistoryEntry> loadHistory() {
    std::vector<HistoryEntry> result;
    try {
        std::ifstream input(historyFilePath(), std::ios::binary);
        std::string line;
        while (std::getline(input, line)) {
            const size_t first = line.find('\t');
            const size_t second = first == std::string::npos ? std::string::npos : line.find('\t', first + 1);
            if (first == std::string::npos || second == std::string::npos)
                continue;
            result.push_back({unescapeHistoryField(line.substr(0, first)),
                              unescapeHistoryField(line.substr(first + 1, second - first - 1)),
                              unescapeHistoryField(line.substr(second + 1))});
        }
    } catch (...) {
        return {};
    }
    if (result.size() > 100)
        result.erase(result.begin(), result.end() - 100);
    return result;
}

void saveHistory(const std::vector<HistoryEntry>& history) {
    try {
        const auto path = historyFilePath();
        std::filesystem::create_directories(path.parent_path());
        std::ofstream output(path, std::ios::binary | std::ios::trunc);
        for (const auto& entry : history) {
            output << escapeHistoryField(entry.time) << '\t' << escapeHistoryField(entry.type) << '\t'
                   << escapeHistoryField(entry.equation) << '\n';
        }
    } catch (...) {
        // History persistence is non-critical; solving remains available.
    }
}

std::wstring currentTimestamp() {
    SYSTEMTIME time{};
    GetLocalTime(&time);
    std::wostringstream out;
    out << std::setfill(L'0') << std::setw(4) << time.wYear << L"-" << std::setw(2) << time.wMonth << L"-"
        << std::setw(2) << time.wDay << L" " << std::setw(2) << time.wHour << L":" << std::setw(2)
        << time.wMinute << L":" << std::setw(2) << time.wSecond;
    return out.str();
}

class SolverApp {
  public:
    explicit SolverApp(HINSTANCE instance) : instance_(instance) {}

    bool create(int showCommand) {
        if (!mathview::registerClass(instance_) || !editscroll::registerClass(instance_) ||
            !graphview::registerClass(instance_))
            return false;
        const HICON icon = LoadIconW(instance_, MAKEINTRESOURCEW(kAppIconId));
        WNDCLASSEXW wc{};
        wc.cbSize = sizeof(wc);
        wc.style = CS_HREDRAW | CS_VREDRAW;
        wc.lpfnWndProc = WindowProc;
        wc.hInstance = instance_;
        wc.hCursor = LoadCursorW(nullptr, IDC_ARROW);
        wc.hIcon = icon ? icon : LoadIconW(nullptr, IDI_APPLICATION);
        wc.hIconSm = wc.hIcon;
        wc.lpszClassName = kWindowClass;
        if (!RegisterClassExW(&wc) && GetLastError() != ERROR_CLASS_ALREADY_EXISTS)
            return false;
        WNDCLASSEXW logClass{};
        logClass.cbSize = sizeof(logClass);
        logClass.style = CS_HREDRAW | CS_VREDRAW;
        logClass.lpfnWndProc = LogWindowProc;
        logClass.hInstance = instance_;
        logClass.hCursor = LoadCursorW(nullptr, IDC_ARROW);
        logClass.hIcon = wc.hIcon;
        logClass.hIconSm = wc.hIconSm;
        logClass.lpszClassName = kLogWindowClass;
        if (!RegisterClassExW(&logClass) && GetLastError() != ERROR_CLASS_ALREADY_EXISTS)
            return false;
        WNDCLASSEXW graphClass = logClass;
        graphClass.lpfnWndProc = GraphWindowProc;
        graphClass.lpszClassName = kGraphWindowClass;
        if (!RegisterClassExW(&graphClass) && GetLastError() != ERROR_CLASS_ALREADY_EXISTS)
            return false;
        window_ = CreateWindowExW(0, kWindowClass, L"方程求解器", WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN,
                                  CW_USEDEFAULT, CW_USEDEFAULT, 1080, 720, nullptr, nullptr, instance_, this);
        if (!window_)
            return false;
        RECT rect{};
        GetWindowRect(window_, &rect);
        SetWindowPos(window_, nullptr, (GetSystemMetrics(SM_CXSCREEN) - (rect.right - rect.left)) / 2,
                     (GetSystemMetrics(SM_CYSCREEN) - (rect.bottom - rect.top)) / 2, 0, 0,
                     SWP_NOSIZE | SWP_NOZORDER);
        ShowWindow(window_, showCommand);
        UpdateWindow(window_);
        return true;
    }

  private:
    static LRESULT CALLBACK WindowProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam) {
        SolverApp* app = reinterpret_cast<SolverApp*>(GetWindowLongPtrW(hwnd, GWLP_USERDATA));
        if (message == WM_NCCREATE) {
            const auto* create = reinterpret_cast<CREATESTRUCTW*>(lParam);
            app = static_cast<SolverApp*>(create->lpCreateParams);
            app->window_ = hwnd;
            SetWindowLongPtrW(hwnd, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(app));
        }
        return app ? app->handleMessage(message, wParam, lParam)
                   : DefWindowProcW(hwnd, message, wParam, lParam);
    }

    static LRESULT CALLBACK LogWindowProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam) {
        SolverApp* app = reinterpret_cast<SolverApp*>(GetWindowLongPtrW(hwnd, GWLP_USERDATA));
        if (message == WM_NCCREATE) {
            const auto* create = reinterpret_cast<CREATESTRUCTW*>(lParam);
            app = static_cast<SolverApp*>(create->lpCreateParams);
            SetWindowLongPtrW(hwnd, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(app));
        }
        return app ? app->handleLogMessage(hwnd, message, wParam, lParam)
                   : DefWindowProcW(hwnd, message, wParam, lParam);
    }

    static LRESULT CALLBACK GraphWindowProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam) {
        SolverApp* app = reinterpret_cast<SolverApp*>(GetWindowLongPtrW(hwnd, GWLP_USERDATA));
        if (message == WM_NCCREATE) {
            const auto* create = reinterpret_cast<CREATESTRUCTW*>(lParam);
            app = static_cast<SolverApp*>(create->lpCreateParams);
            SetWindowLongPtrW(hwnd, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(app));
        }
        return app ? app->handleGraphMessage(hwnd, message, wParam, lParam)
                   : DefWindowProcW(hwnd, message, wParam, lParam);
    }

    HWND makeControl(const wchar_t* className, const wchar_t* text, DWORD style, int id = 0,
                     DWORD extendedStyle = 0) {
        HWND control =
            CreateWindowExW(extendedStyle, className, text, WS_CHILD | WS_VISIBLE | style, 0, 0, 100, 30,
                            window_, reinterpret_cast<HMENU>(static_cast<INT_PTR>(id)), instance_, nullptr);
        SendMessageW(control, WM_SETFONT, reinterpret_cast<WPARAM>(normalFont_), TRUE);
        return control;
    }

    std::wstring localize(const wchar_t* simplified, const wchar_t* english) const {
        if (language_ == Language::English)
            return english;
        if (language_ == Language::Traditional)
            return toTraditional(simplified);
        return simplified;
    }

    std::wstring localizeOutput(const std::wstring& simplified) const {
        if (language_ == Language::English)
            return toEnglish(simplified);
        if (language_ == Language::Traditional)
            return toTraditional(simplified);
        return simplified;
    }

    std::wstring localizedSolverOutput(const std::wstring& simplified) const {
        return equation::applyVariableNames(localizeOutput(simplified), lastVariableContext_);
    }

    void createControls() {
        normalFont_ =
            CreateFontW(-17, 0, 0, 0, FW_NORMAL, FALSE, FALSE, FALSE, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS,
                        CLIP_DEFAULT_PRECIS, CLEARTYPE_QUALITY, DEFAULT_PITCH, L"Microsoft YaHei UI");
        titleFont_ =
            CreateFontW(-27, 0, 0, 0, FW_BOLD, FALSE, FALSE, FALSE, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS,
                        CLIP_DEFAULT_PRECIS, CLEARTYPE_QUALITY, DEFAULT_PITCH, L"Microsoft YaHei UI");
        smallFont_ =
            CreateFontW(-15, 0, 0, 0, FW_NORMAL, FALSE, FALSE, FALSE, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS,
                        CLIP_DEFAULT_PRECIS, CLEARTYPE_QUALITY, DEFAULT_PITCH, L"Microsoft YaHei UI");

        title_ = makeControl(L"STATIC", L"方程求解器", SS_LEFT);
        SendMessageW(title_, WM_SETFONT, reinterpret_cast<WPARAM>(titleFont_), TRUE);
        subtitle_ = makeControl(L"STATIC", L"直接输入原方程，自动给出答案、定义域和逐步推导", SS_LEFT);
        SendMessageW(subtitle_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        languageCombo_ = makeControl(L"BUTTON", L"简体中文", BS_OWNERDRAW | WS_TABSTOP, kLanguageComboId);
        SendMessageW(languageCombo_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        themeButton_ = makeControl(L"BUTTON", L"", BS_OWNERDRAW | WS_TABSTOP, kThemeButtonId);

        typeLabel_ = makeControl(L"STATIC", L"题型", SS_LEFT);
        typeCombo_ = makeControl(L"BUTTON", L"自动识别", BS_OWNERDRAW | WS_TABSTOP, kTypeComboId);
        angleUnitButton_ = makeControl(L"BUTTON", L"弧度制", BS_OWNERDRAW | WS_TABSTOP, kAngleUnitButtonId);
        SendMessageW(typeLabel_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        SendMessageW(typeCombo_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        SendMessageW(angleUnitButton_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        hint_ = makeControl(L"STATIC", L"", SS_LEFT);
        SendMessageW(hint_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        inputLabel_ = makeControl(L"STATIC", L"输入原方程 / 方程组", SS_LEFT);
        inputFrame_ = makeControl(L"BUTTON", L"", BS_OWNERDRAW, kInputFrameId);
        EnableWindow(inputFrame_, FALSE);
        input_ = makeControl(L"EDIT", kTypes[0].example,
                             ES_MULTILINE | ES_AUTOVSCROLL | ES_WANTRETURN | WS_TABSTOP, kEquationInputId);
        inputScrollbar_ = editscroll::create(instance_, window_, input_);
        SendMessageW(input_, EM_SETLIMITTEXT, 4096, 0);
        syntaxTitle_ = makeControl(L"STATIC", L"输入说明", SS_LEFT);
        syntax_ = makeControl(mathview::kClassName, L"",
                              WS_TABSTOP | mathview::kVerticalScroll | mathview::kPlainText);
        SendMessageW(syntax_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);

        exampleButton_ = makeControl(L"BUTTON", L"填入示例", BS_OWNERDRAW | WS_TABSTOP, kExampleButtonId);
        clearButton_ = makeControl(L"BUTTON", L"清空", BS_OWNERDRAW | WS_TABSTOP, kClearButtonId);
        solveButton_ = makeControl(L"BUTTON", L"开始求解", BS_OWNERDRAW | WS_TABSTOP, kSolveButtonId);
        resultTitle_ = makeControl(L"STATIC", L"求解结果", SS_LEFT);
        graphButton_ = makeControl(L"BUTTON", L"图像", BS_OWNERDRAW | WS_TABSTOP, kGraphButtonId);
        EnableWindow(graphButton_, FALSE);
        historyButton_ = makeControl(L"BUTTON", L"求解日志", BS_OWNERDRAW | WS_TABSTOP, kHistoryButtonId);
        SendMessageW(graphButton_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        SendMessageW(historyButton_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        resultSummary_ =
            makeControl(mathview::kClassName, L"等待求解……", WS_TABSTOP | mathview::kTwoLineSummary);
        approximation_ = makeControl(L"STATIC", L"", SS_LEFT);
        SendMessageW(approximation_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        ShowWindow(approximation_, SW_HIDE);
        methodLabel_ = makeControl(L"STATIC", L"解法", SS_LEFT);
        SendMessageW(methodLabel_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        ShowWindow(methodLabel_, SW_HIDE);
        for (int i = 0; i < 3; ++i) {
            methodButtons_[i] =
                makeControl(L"BUTTON", L"", BS_OWNERDRAW | WS_TABSTOP, kMethodButtonBaseId + i);
            ShowWindow(methodButtons_[i], SW_HIDE);
        }
        process_ = makeControl(mathview::kClassName, L"输入原式后点击“开始求解”。",
                               WS_TABSTOP | mathview::kVerticalScroll);

        lastSummaryZh_ = L"等待求解……";
        lastApproximationZh_.clear();
        lastVariableContext_ = {};
        lastSolvedInput_.clear();
        methodSectionsZh_ = {L"输入原式后点击“开始求解”。"};
        history_ = loadHistory();
        updateLanguage();
        applyTheme();
        layoutControls();
        SetFocus(input_);
    }

    std::wstring controlText(HWND control) const {
        const int length = GetWindowTextLengthW(control);
        std::wstring text(static_cast<size_t>(length) + 1, L'\0');
        GetWindowTextW(control, text.data(), length + 1);
        text.resize(static_cast<size_t>(length));
        return text;
    }

    std::wstring historyTypeForInput(const std::wstring& text) const {
        const std::wstring& classified = lastSolvedInput_.empty() ? text : lastSolvedInput_;
        if (currentType_ > 0 && currentType_ < static_cast<int>(kTypes.size())) {
            return kTypes[static_cast<size_t>(currentType_)].simplified;
        }
        int lineCount = 1;
        for (wchar_t ch : classified)
            if (ch == L'\n' || ch == L';' || ch == L'；')
                ++lineCount;
        if (classified.find(L"sin") != std::wstring::npos || classified.find(L"cos") != std::wstring::npos ||
            classified.find(L"tan") != std::wstring::npos || classified.find(L"cot") != std::wstring::npos) {
            return L"三角方程";
        }
        if (lineCount >= 3)
            return L"三元一次方程组";
        if (lineCount == 2)
            return L"二元一次方程组";
        if (classified.find_first_of(L"<>≤≥") != std::wstring::npos)
            return L"一元一次不等式";
        if (classified.find(L'|') != std::wstring::npos)
            return L"绝对值方程";
        if (classified.find(L"/x") != std::wstring::npos || classified.find(L"/(x") != std::wstring::npos ||
            classified.find(L"／x") != std::wstring::npos)
            return L"分式方程";
        if (classified.find(L"^2") != std::wstring::npos || classified.find(L'²') != std::wstring::npos) {
            return L"一元二次方程";
        }
        if (classified.find(L'^') != std::wstring::npos)
            return L"指数方程";
        return L"一元一次方程";
    }

    void addHistoryEntry(const std::wstring& equation) {
        history_.push_back({currentTimestamp(), historyTypeForInput(equation), equation});
        if (history_.size() > 100)
            history_.erase(history_.begin(), history_.end() - 100);
        saveHistory(history_);
        updateHistoryWindow();
    }

    std::wstring displayedHistory() const {
        if (history_.empty())
            return localize(L"暂无求解日志。", L"No solution history yet.");
        std::wstring text;
        for (auto it = history_.rbegin(); it != history_.rend(); ++it) {
            text += L"[" + it->time + L"]  " + localizeOutput(it->type) + L"\r\n";
            text += it->equation + L"\r\n\r\n";
        }
        return text;
    }

    void updateHistoryWindow() {
        if (!logEdit_)
            return;
        const std::wstring text = displayedHistory();
        SetWindowTextW(logEdit_, text.c_str());
    }

    void applyLogTheme() {
        if (!logWindow_)
            return;
        const BOOL enabled = darkMode_ ? TRUE : FALSE;
        DwmSetWindowAttribute(logWindow_, kImmersiveDarkModeAttribute, &enabled, sizeof(enabled));
        const COLORREF captionColor = darkMode_ ? RGB(15, 17, 24) : RGB(255, 255, 255);
        const COLORREF captionTextColor = darkMode_ ? RGB(245, 247, 250) : RGB(25, 28, 34);
        DwmSetWindowAttribute(logWindow_, kCaptionColorAttribute, &captionColor, sizeof(captionColor));
        DwmSetWindowAttribute(logWindow_, kCaptionTextColorAttribute, &captionTextColor,
                              sizeof(captionTextColor));
        const DWORD roundPreference = 2;
        DwmSetWindowAttribute(logWindow_, kWindowCornerPreferenceAttribute, &roundPreference,
                              sizeof(roundPreference));
        const wchar_t* explorer = darkMode_ ? L"DarkMode_Explorer" : L"Explorer";
        SetWindowTheme(logWindow_, explorer, nullptr);
        mathview::setDarkMode(logEdit_, darkMode_);
        SetWindowTheme(logClearButton_, explorer, nullptr);
        RedrawWindow(logWindow_, nullptr, nullptr,
                     RDW_INVALIDATE | RDW_ERASE | RDW_FRAME | RDW_ALLCHILDREN | RDW_UPDATENOW);
    }

    void showHistoryWindow() {
        if (logWindow_) {
            ShowWindow(logWindow_, SW_RESTORE);
            SetForegroundWindow(logWindow_);
            updateHistoryWindow();
            return;
        }
        constexpr int logWidth = 680;
        constexpr int logHeight = 560;
        RECT ownerRect{};
        GetWindowRect(window_, &ownerRect);
        const int logX = ownerRect.left + ((ownerRect.right - ownerRect.left) - logWidth) / 2;
        const int logY = ownerRect.top + ((ownerRect.bottom - ownerRect.top) - logHeight) / 2;
        logWindow_ = CreateWindowExW(
            0, kLogWindowClass, localize(L"求解日志（最近 100 条）", L"Solution History (Last 100)").c_str(),
            WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN, logX, logY, logWidth, logHeight, window_, nullptr,
            instance_, this);
        if (!logWindow_)
            return;
        logEdit_ = CreateWindowExW(0, mathview::kClassName, L"",
                                   WS_CHILD | WS_VISIBLE | WS_TABSTOP | mathview::kVerticalScroll, 16, 16,
                                   632, 440, logWindow_, nullptr, instance_, nullptr);
        logClearButton_ = CreateWindowExW(
            0, L"BUTTON", localize(L"清空日志", L"Clear History").c_str(),
            WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_OWNERDRAW, 16, 470, 130, 36, logWindow_,
            reinterpret_cast<HMENU>(static_cast<INT_PTR>(kLogClearButtonId)), instance_, nullptr);
        SendMessageW(logEdit_, WM_SETFONT, reinterpret_cast<WPARAM>(normalFont_), TRUE);
        SendMessageW(logClearButton_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        updateHistoryWindow();
        applyLogTheme();
        ShowWindow(logWindow_, SW_SHOW);
        UpdateWindow(logWindow_);
    }

    std::vector<std::pair<double, double>> graphMarkers() const {
        std::vector<std::pair<double, double>> markers;
        if (lastGraphSystem2_) {
            std::optional<double> x;
            std::optional<double> y;
            for (const auto& [label, value] : lastGraphValues_) {
                if (label == L"x")
                    x = value;
                else if (label == L"y")
                    y = value;
            }
            if (x && y)
                markers.push_back({*x, *y});
            return markers;
        }
        for (const auto& [label, value] : lastGraphValues_) {
            if (label.empty() || label.front() != L'x')
                continue;
            bool duplicate = false;
            for (const auto& marker : markers) {
                if (std::abs(marker.first - value) < 1e-8) {
                    duplicate = true;
                    break;
                }
            }
            if (!duplicate)
                markers.push_back({value, std::numeric_limits<double>::quiet_NaN()});
        }
        return markers;
    }

    void updateGraphView() {
        if (!graphCanvas_)
            return;
        graphview::GraphRequest request;
        request.input = lastSolvedInput_;
        request.system2 = lastGraphSystem2_;
        request.darkMode = darkMode_;
        request.degreeMode = angleUnit_ == equation::AngleUnit::Degrees;
        request.leftLegend =
            lastGraphSystem2_ ? localize(L"方程 1", L"Equation 1") : localize(L"等号左侧", L"Left side");
        request.rightLegend =
            lastGraphSystem2_ ? localize(L"方程 2", L"Equation 2") : localize(L"等号右侧", L"Right side");
        request.intersectionLabel = localize(L"实数解 / 交点", L"Real solution / intersection");
        if (lastVariableContext_.variableCount > 0) {
            request.xAxisLabel.assign(1, lastVariableContext_.variables[0]);
        }
        if (lastGraphSystem2_ && lastVariableContext_.variableCount > 1) {
            request.yAxisLabel.assign(1, lastVariableContext_.variables[1]);
        }
        request.markers = graphMarkers();
        graphview::setData(graphCanvas_, request);
    }

    void applyGraphTheme() {
        if (!graphWindow_)
            return;
        const BOOL enabled = darkMode_ ? TRUE : FALSE;
        DwmSetWindowAttribute(graphWindow_, kImmersiveDarkModeAttribute, &enabled, sizeof(enabled));
        const COLORREF captionColor = darkMode_ ? RGB(15, 17, 24) : RGB(255, 255, 255);
        const COLORREF captionTextColor = darkMode_ ? RGB(245, 247, 250) : RGB(25, 28, 34);
        DwmSetWindowAttribute(graphWindow_, kCaptionColorAttribute, &captionColor, sizeof(captionColor));
        DwmSetWindowAttribute(graphWindow_, kCaptionTextColorAttribute, &captionTextColor,
                              sizeof(captionTextColor));
        const DWORD roundPreference = 2;
        DwmSetWindowAttribute(graphWindow_, kWindowCornerPreferenceAttribute, &roundPreference,
                              sizeof(roundPreference));
        graphview::setDarkMode(graphCanvas_, darkMode_);
        RedrawWindow(graphWindow_, nullptr, nullptr,
                     RDW_INVALIDATE | RDW_ERASE | RDW_FRAME | RDW_ALLCHILDREN | RDW_UPDATENOW);
    }

    void showGraphWindow() {
        if (!graphEnabled_)
            return;
        if (graphWindow_) {
            updateGraphView();
            ShowWindow(graphWindow_, SW_RESTORE);
            SetForegroundWindow(graphWindow_);
            return;
        }
        constexpr int graphWidth = 860;
        constexpr int graphHeight = 640;
        RECT owner{};
        GetWindowRect(window_, &owner);
        const int x = owner.left + ((owner.right - owner.left) - graphWidth) / 2;
        const int y = owner.top + ((owner.bottom - owner.top) - graphHeight) / 2;
        graphWindow_ = CreateWindowExW(0, kGraphWindowClass, localize(L"方程图像", L"Equation Graph").c_str(),
                                       WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN, x, y, graphWidth, graphHeight,
                                       window_, nullptr, instance_, this);
        if (!graphWindow_)
            return;
        graphCanvas_ =
            CreateWindowExW(0, graphview::kClassName, L"", WS_CHILD | WS_VISIBLE | WS_TABSTOP, 14, 14,
                            graphWidth - 56, graphHeight - 78, graphWindow_, nullptr, instance_, nullptr);
        SendMessageW(graphCanvas_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        RECT client{};
        GetClientRect(graphWindow_, &client);
        MoveWindow(graphCanvas_, 14, 14, client.right - 28, client.bottom - 28, TRUE);
        updateGraphView();
        applyGraphTheme();
        ShowWindow(graphWindow_, SW_SHOW);
        UpdateWindow(graphWindow_);
    }

    void rebuildTypeCombo() {
        const auto& type = kTypes[static_cast<size_t>(currentType_)];
        const std::wstring name = language_ == Language::English       ? type.english
                                  : language_ == Language::Traditional ? toTraditional(type.simplified)
                                                                       : type.simplified;
        SetWindowTextW(typeCombo_, name.c_str());
        InvalidateRect(typeCombo_, nullptr, TRUE);
    }

    void showTypeMenu() {
        HMENU menu = CreatePopupMenu();
        for (size_t i = 0; i < kTypes.size(); ++i) {
            const auto& type = kTypes[i];
            const std::wstring name = language_ == Language::English       ? type.english
                                      : language_ == Language::Traditional ? toTraditional(type.simplified)
                                                                           : type.simplified;
            AppendMenuW(menu, MF_STRING | (static_cast<int>(i) == currentType_ ? MF_CHECKED : 0),
                        400 + static_cast<UINT>(i), name.c_str());
        }
        RECT rect{};
        GetWindowRect(typeCombo_, &rect);
        const UINT command = TrackPopupMenu(menu, TPM_LEFTALIGN | TPM_TOPALIGN | TPM_RETURNCMD, rect.left,
                                            rect.bottom, 0, window_, nullptr);
        DestroyMenu(menu);
        if (command >= 400 && command < 400 + kTypes.size())
            chooseType(static_cast<int>(command - 400));
    }

    void showLanguageMenu() {
        const std::array<const wchar_t*, 3> names{L"简体中文", L"English", L"繁體中文"};
        HMENU menu = CreatePopupMenu();
        for (size_t i = 0; i < names.size(); ++i) {
            AppendMenuW(menu,
                        MF_STRING | (static_cast<int>(i) == static_cast<int>(language_) ? MF_CHECKED : 0),
                        420 + static_cast<UINT>(i), names[i]);
        }
        RECT rect{};
        GetWindowRect(languageCombo_, &rect);
        const UINT command = TrackPopupMenu(menu, TPM_LEFTALIGN | TPM_TOPALIGN | TPM_RETURNCMD, rect.left,
                                            rect.bottom, 0, window_, nullptr);
        DestroyMenu(menu);
        if (command >= 420 && command < 423) {
            language_ = static_cast<Language>(command - 420);
            updateLanguage();
            layoutControls();
        }
    }

    void showAngleUnitMenu() {
        const std::array<std::wstring, 2> names{localize(L"弧度制", L"Radians"),
                                                localize(L"角度制", L"Degrees")};
        HMENU menu = CreatePopupMenu();
        const int selected = angleUnit_ == equation::AngleUnit::Radians ? 0 : 1;
        for (size_t i = 0; i < names.size(); ++i) {
            AppendMenuW(menu, MF_STRING | (static_cast<int>(i) == selected ? MF_CHECKED : 0),
                        430 + static_cast<UINT>(i), names[i].c_str());
        }
        RECT rect{};
        GetWindowRect(angleUnitButton_, &rect);
        const UINT command = TrackPopupMenu(menu, TPM_LEFTALIGN | TPM_TOPALIGN | TPM_RETURNCMD, rect.left,
                                            rect.bottom, 0, window_, nullptr);
        DestroyMenu(menu);
        if (command < 430 || command >= 432)
            return;
        const equation::AngleUnit selectedUnit =
            command == 430 ? equation::AngleUnit::Radians : equation::AngleUnit::Degrees;
        if (selectedUnit == angleUnit_)
            return;
        angleUnit_ = selectedUnit;
        lastSummaryZh_ = L"等待求解……";
        lastApproximationZh_.clear();
        lastVariableContext_ = {};
        methodSectionsZh_ = {L"角度单位已切换，请重新求解。"};
        selectedMethod_ = 0;
        graphEnabled_ = false;
        EnableWindow(graphButton_, FALSE);
        if (graphWindow_)
            DestroyWindow(graphWindow_);
        updateLanguage();
        layoutControls();
    }

    void updateLanguage() {
        SetWindowTextW(window_, localize(L"方程求解器", L"Equation Solver").c_str());
        SetWindowTextW(title_, localize(L"方程求解器", L"Equation Solver").c_str());
        SetWindowTextW(
            subtitle_,
            localize(L"直接输入原方程，自动给出答案、定义域和逐步推导",
                     L"Enter an equation directly to get an exact answer and step-by-step solution")
                .c_str());
        SetWindowTextW(typeLabel_, localize(L"题型", L"Type").c_str());
        SetWindowTextW(inputLabel_, localize(L"输入原方程 / 方程组", L"Equation / system").c_str());
        SetWindowTextW(syntaxTitle_, localize(L"输入说明", L"Input syntax").c_str());
        SetWindowTextW(exampleButton_, localize(L"填入示例", L"Example").c_str());
        SetWindowTextW(clearButton_, localize(L"清空", L"Clear").c_str());
        SetWindowTextW(solveButton_, localize(L"开始求解", L"Solve").c_str());
        SetWindowTextW(resultTitle_, localize(L"求解结果", L"Result").c_str());
        SetWindowTextW(graphButton_, localize(L"图像", L"Graph").c_str());
        SetWindowTextW(historyButton_, localize(L"求解日志", L"History").c_str());
        SetWindowTextW(methodLabel_, localize(L"解法", L"Methods").c_str());
        SetWindowTextW(angleUnitButton_,
                       (angleUnit_ == equation::AngleUnit::Radians ? localize(L"弧度制", L"Radians")
                                                                   : localize(L"角度制", L"Degrees"))
                           .c_str());
        const std::array<const wchar_t*, 3> languageNames{L"简体中文", L"English", L"繁體中文"};
        SetWindowTextW(languageCombo_, languageNames[static_cast<size_t>(language_)]);
        std::wstring syntax = localize(
            L"• 未知数可用任意英文字母（e 除外）：a、t、u 等\r\n• 常数：pi 或 π 表示圆周率，e "
            L"表示自然常数\r\n• 三角函数：支持弧度制和角度制，可在题型旁切换\r\n• 阶乘：5! 或 5！\r\n• "
            L"括号层级：{ [ ( ) ] "
            L"}；同种括号不能嵌套\r\n• 可省略乘号：2a、3(a+1)\r\n• 平方与平方根：a^2；\"100\"\r\n• "
            L"n 次根：\"n\"被开方数\"；其他指数幂：a^-2、a^(1/2)、a^(-2/3)\r\n• "
            L"绝对值：|a|；根号内也可用，如 "
            L"\"3\"|-2|\"\r\n• "
            L"方程组每行一个方程",
            L"• Any English letter except e can be a variable: a, t, u, etc.\r\n• Constants: pi or π is the "
            L"circle constant; e is Euler's number\r\n• Trigonometric functions: switch between radians and "
            L"degrees beside the type selector\r\n• Factorial: 5! or 5！\r\n• Bracket order: { [ ( ) ] }; "
            L"brackets of the same kind cannot be "
            L"nested\r\n• Multiplication may be omitted: 2a, 3(a+1)\r\n• Square and square root: a^2; "
            L"\"100\"\r\n• "
            L"nth root: \"n\"radicand\"; other exponents: a^-2, a^(1/2), a^(-2/3)\r\n• Absolute value: |a|; "
            L"also in "
            L"roots, e.g. \"3\"|-2|\"\r\n• Enter one equation per line for a system");
        replaceAll(syntax, L"（e 除外）", L"（e、i 除外）");
        replaceAll(syntax, L"表示自然常数\r\n", L"表示自然常数，i 表示虚数单位（i² = -1）\r\n");
        replaceAll(syntax, L"except e can", L"except e and i can");
        replaceAll(syntax, L"e is Euler's number\r\n",
                   L"e is Euler's number; i is the imaginary unit (i² = -1)\r\n");
        SetWindowTextW(syntax_, syntax.c_str());
        rebuildTypeCombo();
        const auto& type = kTypes[currentType_];
        SetWindowTextW(hint_, localize(type.hintSimplified, type.hintEnglish).c_str());
        refreshDisplayedResult();
        if (logWindow_) {
            SetWindowTextW(logWindow_,
                           localize(L"求解日志（最近 100 条）", L"Solution History (Last 100)").c_str());
            SetWindowTextW(logClearButton_, localize(L"清空日志", L"Clear History").c_str());
            updateHistoryWindow();
            applyLogTheme();
        }
        if (graphWindow_) {
            SetWindowTextW(graphWindow_, localize(L"方程图像", L"Equation Graph").c_str());
            updateGraphView();
            applyGraphTheme();
        }
        RedrawWindow(window_, nullptr, nullptr, RDW_INVALIDATE | RDW_ERASE | RDW_ALLCHILDREN | RDW_UPDATENOW);
    }

    void chooseType(int index) {
        if (index < 0 || index >= static_cast<int>(kTypes.size()))
            return;
        currentType_ = index;
        const auto& type = kTypes[index];
        SetWindowTextW(hint_, localize(type.hintSimplified, type.hintEnglish).c_str());
        SetWindowTextW(input_, type.example);
        lastSummaryZh_ = L"等待求解……";
        lastApproximationZh_.clear();
        lastVariableContext_ = {};
        lastSolvedInput_.clear();
        methodSectionsZh_ = {L"输入原式后点击“开始求解”。"};
        selectedMethod_ = 0;
        graphEnabled_ = false;
        EnableWindow(graphButton_, FALSE);
        refreshDisplayedResult();
        layoutControls();
        SetFocus(input_);
        SendMessageW(input_, EM_SETSEL, 0, -1);
    }

    void splitMethods(const std::wstring& process) {
        methodSectionsZh_.clear();
        std::vector<size_t> positions{0};
        const size_t second = process.find(L"方法二：");
        const size_t third = process.find(L"方法三：");
        if (second != std::wstring::npos)
            positions.push_back(second);
        if (third != std::wstring::npos)
            positions.push_back(third);
        std::sort(positions.begin(), positions.end());
        for (size_t i = 0; i < positions.size(); ++i) {
            const size_t end = i + 1 < positions.size() ? positions[i + 1] : process.size();
            methodSectionsZh_.push_back(process.substr(positions[i], end - positions[i]));
        }
        if (methodSectionsZh_.empty())
            methodSectionsZh_.push_back(process);
        selectedMethod_ = 0;
    }

    void refreshDisplayedResult() {
        SetWindowTextW(resultSummary_, localizedSolverOutput(lastSummaryZh_).c_str());
        SetWindowTextW(approximation_, localizedSolverOutput(lastApproximationZh_).c_str());
        ShowWindow(approximation_, lastApproximationZh_.empty() ? SW_HIDE : SW_SHOW);
        if (methodSectionsZh_.empty())
            methodSectionsZh_.push_back(L"");
        selectedMethod_ = std::clamp(selectedMethod_, 0, static_cast<int>(methodSectionsZh_.size()) - 1);
        const std::wstring displayedProcess =
            multiplicationForDisplay(localizedSolverOutput(methodSectionsZh_[selectedMethod_]));
        SetWindowTextW(process_, displayedProcess.c_str());
        const bool multiple = methodSectionsZh_.size() > 1;
        ShowWindow(methodLabel_, multiple ? SW_SHOW : SW_HIDE);
        for (int i = 0; i < 3; ++i) {
            ShowWindow(methodButtons_[i],
                       multiple && i < static_cast<int>(methodSectionsZh_.size()) ? SW_SHOW : SW_HIDE);
            InvalidateRect(methodButtons_[i], nullptr, TRUE);
        }
    }

    void solve() {
        // 成功时集中刷新结果、解法、日志和图像；输入异常只更新错误区域。
        const std::wstring text = controlText(input_);
        try {
            const auto kind = static_cast<equation::InputKind>(currentType_);
            const equation::SolveResult result = equation::solveText(text, kind, angleUnit_);
            lastSummaryZh_ = result.summary;
            lastApproximationZh_ = approximationForSummary(result.summary, angleUnit_);
            lastVariableContext_ = result;
            splitMethods(result.process);
            int equationLines = 1;
            for (wchar_t ch : text)
                if (ch == L'\n' || ch == L';' || ch == L'；')
                    ++equationLines;
            const bool inequality = text.find_first_of(L"<>≤≥") != std::wstring::npos;
            const bool noRealSolution = result.summary.find(L"无解") != std::wstring::npos ||
                                        result.summary.find(L"无实数") != std::wstring::npos ||
                                        result.summary.find(L"没有实数") != std::wstring::npos ||
                                        result.summary.find(L"复数根") != std::wstring::npos ||
                                        result.summary.find(L"空集") != std::wstring::npos;
            graphEnabled_ = !result.usesImaginaryUnit && !inequality && equationLines <= 2 && !noRealSolution;
            lastSolvedInput_ = result.normalizedInput.empty() ? text : result.normalizedInput;
            lastGraphSystem2_ = equationLines == 2;
            lastGraphValues_ = realValuesForGraph(result.summary, angleUnit_);
            EnableWindow(graphButton_, graphEnabled_ ? TRUE : FALSE);
            if (graphEnabled_)
                updateGraphView();
            else if (graphWindow_)
                DestroyWindow(graphWindow_);
            refreshDisplayedResult();
            layoutControls();
            addHistoryEntry(text);
        } catch (const equation::ParseError& error) {
            const int needed = MultiByteToWideChar(CP_UTF8, 0, error.what(), -1, nullptr, 0);
            std::wstring message = L"输入格式不正确";
            if (needed > 0) {
                message.assign(static_cast<size_t>(needed), L'\0');
                MultiByteToWideChar(CP_UTF8, 0, error.what(), -1, message.data(), needed);
                message.resize(static_cast<size_t>(needed - 1));
            }
            lastSummaryZh_ = L"无法求解：请检查输入格式";
            lastApproximationZh_.clear();
            lastVariableContext_ = {};
            graphEnabled_ = false;
            EnableWindow(graphButton_, FALSE);
            if (graphWindow_)
                DestroyWindow(graphWindow_);
            methodSectionsZh_ = {message};
            selectedMethod_ = 0;
            refreshDisplayedResult();
            const std::wstring localized = localizeOutput(message);
            MessageBoxW(window_, localized.c_str(), localize(L"输入有误", L"Invalid input").c_str(),
                        MB_OK | MB_ICONWARNING);
            SetFocus(input_);
        } catch (const std::exception&) {
            MessageBoxW(window_,
                        localize(L"求解过程中发生了未预期的错误。", L"An unexpected error occurred.").c_str(),
                        localize(L"错误", L"Error").c_str(), MB_OK | MB_ICONERROR);
        }
    }

    void clearInput() {
        SetWindowTextW(input_, L"");
        lastSummaryZh_ = L"等待输入……";
        lastApproximationZh_.clear();
        lastVariableContext_ = {};
        graphEnabled_ = false;
        EnableWindow(graphButton_, FALSE);
        if (graphWindow_)
            DestroyWindow(graphWindow_);
        methodSectionsZh_ = {L"请输入原方程、不等式或方程组。"};
        selectedMethod_ = 0;
        refreshDisplayedResult();
        layoutControls();
        SetFocus(input_);
    }

    void recreateBrushes() {
        if (backgroundBrush_)
            DeleteObject(backgroundBrush_);
        if (panelBrush_)
            DeleteObject(panelBrush_);
        backgroundBrush_ = CreateSolidBrush(darkMode_ ? RGB(25, 27, 36) : RGB(244, 245, 247));
        panelBrush_ = CreateSolidBrush(darkMode_ ? RGB(36, 39, 51) : RGB(255, 255, 255));
    }

    void applyTheme() {
        recreateBrushes();
        const BOOL enabled = darkMode_ ? TRUE : FALSE;
        DwmSetWindowAttribute(window_, kImmersiveDarkModeAttribute, &enabled, sizeof(enabled));
        const COLORREF captionColor = darkMode_ ? RGB(15, 17, 24) : RGB(255, 255, 255);
        const COLORREF captionTextColor = darkMode_ ? RGB(245, 247, 250) : RGB(25, 28, 34);
        DwmSetWindowAttribute(window_, kCaptionColorAttribute, &captionColor, sizeof(captionColor));
        DwmSetWindowAttribute(window_, kCaptionTextColorAttribute, &captionTextColor,
                              sizeof(captionTextColor));
        const DWORD roundPreference = 2;
        DwmSetWindowAttribute(window_, kWindowCornerPreferenceAttribute, &roundPreference,
                              sizeof(roundPreference));
        const wchar_t* explorer = darkMode_ ? L"DarkMode_Explorer" : L"Explorer";
        SetWindowTheme(window_, explorer, nullptr);
        const std::array<HWND, 11> themed{typeCombo_,   angleUnitButton_, languageCombo_, inputFrame_,
                                          input_,       exampleButton_,   clearButton_,   solveButton_,
                                          graphButton_, historyButton_,   themeButton_};
        for (HWND control : themed)
            SetWindowTheme(control, explorer, nullptr);
        SetWindowTheme(typeCombo_, darkMode_ ? L"DarkMode_CFD" : L"CFD", nullptr);
        SetWindowTheme(angleUnitButton_, darkMode_ ? L"DarkMode_CFD" : L"CFD", nullptr);
        SetWindowTheme(languageCombo_, darkMode_ ? L"DarkMode_CFD" : L"CFD", nullptr);
        editscroll::setDarkMode(inputScrollbar_, darkMode_);
        mathview::setDarkMode(syntax_, darkMode_);
        mathview::setDarkMode(resultSummary_, darkMode_);
        mathview::setDarkMode(process_, darkMode_);
        applyLogTheme();
        applyGraphTheme();
        InvalidateRect(window_, nullptr, TRUE);
        for (HWND control : themed)
            InvalidateRect(control, nullptr, TRUE);
        for (HWND button : methodButtons_)
            InvalidateRect(button, nullptr, TRUE);
        RedrawWindow(window_, nullptr, nullptr,
                     RDW_INVALIDATE | RDW_ERASE | RDW_FRAME | RDW_ALLCHILDREN | RDW_UPDATENOW);
        SetWindowPos(window_, nullptr, 0, 0, 0, 0,
                     SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE | SWP_FRAMECHANGED);
        DwmFlush();
    }

    void roundControl(HWND control, int radius) {
        if (!control || !IsWindowVisible(control))
            return;
        RECT rect{};
        GetClientRect(control, &rect);
        HRGN region = CreateRoundRectRgn(0, 0, rect.right + 1, rect.bottom + 1, radius, radius);
        SetWindowRgn(control, region, TRUE);
    }

    std::wstring splitSummaryBeforeSecondRoot(const std::wstring& summary) const {
        const wchar_t variable =
            lastVariableContext_.variableCount > 0 ? lastVariableContext_.variables[0] : L'x';
        size_t position = summary.find(std::wstring(1, variable) + L"₂");
        if (position == std::wstring::npos)
            position = summary.find(std::wstring(1, variable) + L"2");
        if (position == std::wstring::npos)
            return summary;
        size_t firstEnd = position;
        while (firstEnd > 0) {
            const wchar_t ch = summary[firstEnd - 1];
            if (std::iswspace(ch) || ch == L'，' || ch == L',' || ch == L';' || ch == L'；')
                --firstEnd;
            else
                break;
        }
        return summary.substr(0, firstEnd) + L"\r\n" + summary.substr(position);
    }

    void layoutControls() {
        if (!window_ || !title_)
            return;
        RECT client{};
        GetClientRect(window_, &client);
        const int width = static_cast<int>(client.right);
        const int height = static_cast<int>(client.bottom);
        const int margin = 28;
        SetWindowTextW(resultSummary_, localizedSolverOutput(lastSummaryZh_).c_str());
        const int naturalSummaryWidth = mathview::preferredWidth(resultSummary_);
        const int regularLeftWidth = std::clamp(width * 38 / 100, 350, 460);
        const int regularRightWidth = width - (margin + regularLeftWidth + 24) - margin;
        const int leftWidth = naturalSummaryWidth > regularRightWidth ? 350 : regularLeftWidth;
        const int rightX = margin + leftWidth + 24;
        const int rightWidth = std::max(320, width - rightX - margin);

        MoveWindow(title_, margin, 20, 520, 38, TRUE);
        MoveWindow(subtitle_, margin, 58, std::max(280, width - 2 * margin - 245), 23, TRUE);
        MoveWindow(languageCombo_, width - margin - 210, 24, 138, 34, TRUE);
        MoveWindow(themeButton_, width - margin - 62, 26, 62, 30, TRUE);
        MoveWindow(typeLabel_, margin, 101, 70, 25, TRUE);
        MoveWindow(typeCombo_, margin + 72, 96, std::max(130, leftWidth - 218), 34, TRUE);
        MoveWindow(angleUnitButton_, margin + leftWidth - 134, 96, 134, 34, TRUE);
        MoveWindow(hint_, margin, 136, leftWidth, 24, TRUE);
        MoveWindow(inputLabel_, margin, 171, leftWidth, 24, TRUE);
        MoveWindow(inputFrame_, margin, 198, leftWidth, 156, TRUE);
        MoveWindow(input_, margin + 8, 205, leftWidth - 32, 142, TRUE);
        MoveWindow(inputScrollbar_, margin + leftWidth - 24, 205, 16, 142, TRUE);
        MoveWindow(syntaxTitle_, margin, 376, leftWidth, 24, TRUE);
        const int buttonY = height - 62;
        MoveWindow(syntax_, margin, 403, leftWidth, std::max(122, buttonY - 415), TRUE);
        MoveWindow(exampleButton_, margin, buttonY, 116, 36, TRUE);
        MoveWindow(clearButton_, margin + 128, buttonY, 90, 36, TRUE);
        MoveWindow(solveButton_, margin + 232, buttonY, std::max(118, leftWidth - 232), 36, TRUE);

        MoveWindow(resultTitle_, rightX, 101, std::max(90, rightWidth - 240), 27, TRUE);
        MoveWindow(graphButton_, rightX + rightWidth - 226, 94, 100, 34, TRUE);
        MoveWindow(historyButton_, rightX + rightWidth - 118, 94, 118, 34, TRUE);
        std::wstring displayedSummary = localizedSolverOutput(lastSummaryZh_);
        bool twoLineSummary = naturalSummaryWidth > rightWidth;
        if (twoLineSummary) {
            displayedSummary = splitSummaryBeforeSecondRoot(displayedSummary);
            SetWindowTextW(resultSummary_, displayedSummary.c_str());
        }
        const int formattedWidth = mathview::preferredWidth(resultSummary_);
        const int summaryWidth = twoLineSummary ? rightWidth : std::clamp(formattedWidth, 250, rightWidth);
        const int summaryHeight = std::max(62, mathview::requiredHeight(resultSummary_, summaryWidth, 2));
        MoveWindow(resultSummary_, rightX, 135, summaryWidth, summaryHeight, TRUE);
        const bool multiple = methodSectionsZh_.size() > 1;
        int processTop = 135 + summaryHeight + 8;
        if (!lastApproximationZh_.empty()) {
            MoveWindow(approximation_, rightX + 4, processTop, rightWidth - 8, 24, TRUE);
            processTop += 28;
        }
        if (multiple) {
            MoveWindow(methodLabel_, rightX + 4, processTop + 5, 62, 36, TRUE);
            for (int i = 0; i < 3; ++i)
                MoveWindow(methodButtons_[i], rightX + 68 + i * 52, processTop, 42, 42, TRUE);
            processTop += 50;
        }
        MoveWindow(process_, rightX, processTop, rightWidth, std::max(150, height - processTop - 26), TRUE);

        for (HWND control :
             {typeCombo_, angleUnitButton_, languageCombo_, inputFrame_, exampleButton_, clearButton_,
              solveButton_, graphButton_, historyButton_, resultSummary_, process_})
            roundControl(control, 18);
    }

    void drawOwnerButton(const DRAWITEMSTRUCT& item) {
        HDC dc = item.hDC;
        RECT rect = item.rcItem;
        SetBkMode(dc, TRANSPARENT);
        const Gdiplus::Color parentColor(255, darkMode_ ? 25 : 244, darkMode_ ? 27 : 245,
                                         darkMode_ ? 36 : 247);
        Gdiplus::Graphics graphics(dc);
        graphics.SetSmoothingMode(Gdiplus::SmoothingModeAntiAlias);
        Gdiplus::SolidBrush parentBrush(parentColor);
        graphics.FillRectangle(&parentBrush, 0, 0, rect.right, rect.bottom);
        if (item.CtlID == kThemeButtonId) {
            const Gdiplus::Color trackColor =
                darkMode_ ? Gdiplus::Color(255, 22, 134, 255) : Gdiplus::Color(255, 194, 198, 204);
            Gdiplus::SolidBrush trackBrush(trackColor);
            Gdiplus::GraphicsPath path;
            const Gdiplus::REAL diameterPath = static_cast<Gdiplus::REAL>(rect.bottom);
            path.AddArc(0.0f, 0.0f, diameterPath, diameterPath, 90.0f, 180.0f);
            path.AddArc(static_cast<Gdiplus::REAL>(rect.right - rect.bottom), 0.0f, diameterPath,
                        diameterPath, 270.0f, 180.0f);
            path.CloseFigure();
            graphics.FillPath(&trackBrush, &path);
            const int diameter = rect.bottom - 6;
            const int left = darkMode_ ? rect.right - diameter - 3 : 3;
            Gdiplus::SolidBrush knobBrush(Gdiplus::Color(255, 255, 255, 255));
            graphics.FillEllipse(&knobBrush, left, 3, diameter, diameter);
            SetTextColor(dc, darkMode_ ? RGB(22, 134, 255) : RGB(105, 110, 118));
            RECT glyph{left, 3, left + diameter, 3 + diameter};
            DrawTextW(dc, darkMode_ ? L"☾" : L"☀", -1, &glyph, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
            return;
        }
        if (item.CtlID < kMethodButtonBaseId || item.CtlID >= kMethodButtonBaseId + 3) {
            const bool inputFrame = item.CtlID == kInputFrameId;
            const bool optionBox = item.CtlID == kTypeComboId || item.CtlID == kAngleUnitButtonId ||
                                   item.CtlID == kLanguageComboId;
            const bool disabled = !inputFrame && (item.itemState & ODS_DISABLED) != 0;
            const bool pressed = (item.itemState & ODS_SELECTED) != 0;
            const bool focused = (item.itemState & ODS_FOCUS) != 0;
            const bool primary = item.CtlID == kSolveButtonId;
            COLORREF fill = primary ? RGB(63, 104, 238) : (darkMode_ ? RGB(45, 49, 63) : RGB(255, 255, 255));
            COLORREF border =
                primary ? RGB(63, 104, 238) : (darkMode_ ? RGB(73, 79, 94) : RGB(205, 211, 220));
            COLORREF foreground =
                primary ? RGB(255, 255, 255) : (darkMode_ ? RGB(232, 236, 242) : RGB(30, 42, 56));
            if (pressed)
                fill = primary ? RGB(48, 86, 211) : (darkMode_ ? RGB(57, 62, 78) : RGB(235, 239, 246));
            if (inputFrame) {
                fill = darkMode_ ? RGB(36, 39, 51) : RGB(255, 255, 255);
                border = darkMode_ ? RGB(70, 76, 87) : RGB(205, 211, 220);
            }
            if (disabled) {
                fill = darkMode_ ? RGB(38, 41, 51) : RGB(235, 237, 241);
                border = darkMode_ ? RGB(55, 59, 70) : RGB(220, 223, 229);
                foreground = darkMode_ ? RGB(105, 111, 124) : RGB(145, 151, 162);
            } else if (focused) {
                border = RGB(63, 104, 238);
            }
            const Gdiplus::REAL left = 1.0f;
            const Gdiplus::REAL top = 1.0f;
            const Gdiplus::REAL right = static_cast<Gdiplus::REAL>(rect.right - 2);
            const Gdiplus::REAL bottom = static_cast<Gdiplus::REAL>(rect.bottom - 2);
            const Gdiplus::REAL diameter = 18.0f;
            Gdiplus::GraphicsPath path;
            path.AddArc(left, top, diameter, diameter, 180.0f, 90.0f);
            path.AddArc(right - diameter, top, diameter, diameter, 270.0f, 90.0f);
            path.AddArc(right - diameter, bottom - diameter, diameter, diameter, 0.0f, 90.0f);
            path.AddArc(left, bottom - diameter, diameter, diameter, 90.0f, 90.0f);
            path.CloseFigure();
            Gdiplus::SolidBrush fillBrush(
                Gdiplus::Color(255, GetRValue(fill), GetGValue(fill), GetBValue(fill)));
            graphics.FillPath(&fillBrush, &path);
            Gdiplus::Pen borderPen(
                Gdiplus::Color(255, GetRValue(border), GetGValue(border), GetBValue(border)),
                focused ? 1.6f : 1.0f);
            graphics.DrawPath(&borderPen, &path);
            wchar_t label[128]{};
            GetWindowTextW(item.hwndItem, label, static_cast<int>(std::size(label)));
            SetTextColor(dc, foreground);
            if (optionBox) {
                RECT textRect{rect.left + 12, rect.top, rect.right - 34, rect.bottom};
                DrawTextW(dc, label, -1, &textRect, DT_LEFT | DT_VCENTER | DT_SINGLELINE | DT_END_ELLIPSIS);
                RECT arrowRect{rect.right - 30, rect.top, rect.right - 8, rect.bottom};
                DrawTextW(dc, L"▾", -1, &arrowRect, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
            } else if (!inputFrame) {
                DrawTextW(dc, label, -1, &rect, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
            }
            return;
        }
        const int index = static_cast<int>(item.CtlID) - kMethodButtonBaseId;
        const bool selected = index == selectedMethod_;
        const COLORREF fill =
            selected ? RGB(63, 104, 238) : (darkMode_ ? RGB(45, 49, 63) : RGB(238, 240, 245));
        const COLORREF foreground =
            selected ? RGB(255, 255, 255) : (darkMode_ ? RGB(232, 236, 242) : RGB(30, 34, 42));
        Gdiplus::SolidBrush circleBrush(
            Gdiplus::Color(255, GetRValue(fill), GetGValue(fill), GetBValue(fill)));
        graphics.FillEllipse(&circleBrush, 1.5f, 1.5f, static_cast<Gdiplus::REAL>(rect.right - 3),
                             static_cast<Gdiplus::REAL>(rect.bottom - 3));
        SetTextColor(dc, foreground);
        const std::wstring number = std::to_wstring(index + 1);
        DrawTextW(dc, number.c_str(), -1, &rect, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
    }

    LRESULT handleGraphMessage(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam) {
        switch (message) {
        case WM_SIZE: {
            if (!graphCanvas_)
                return 0;
            RECT client{};
            GetClientRect(hwnd, &client);
            MoveWindow(graphCanvas_, 14, 14, std::max(320L, client.right - 28),
                       std::max(240L, client.bottom - 28), TRUE);
            return 0;
        }
        case WM_GETMINMAXINFO: {
            auto* info = reinterpret_cast<MINMAXINFO*>(lParam);
            info->ptMinTrackSize = {640, 460};
            return 0;
        }
        case WM_ERASEBKGND: {
            RECT client{};
            GetClientRect(hwnd, &client);
            FillRect(reinterpret_cast<HDC>(wParam), &client, backgroundBrush_);
            return 1;
        }
        case WM_CLOSE:
            DestroyWindow(hwnd);
            return 0;
        case WM_DESTROY:
            graphWindow_ = nullptr;
            graphCanvas_ = nullptr;
            return 0;
        }
        return DefWindowProcW(hwnd, message, wParam, lParam);
    }

    LRESULT handleLogMessage(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam) {
        switch (message) {
        case WM_SIZE: {
            if (!logEdit_ || !logClearButton_)
                return 0;
            RECT client{};
            GetClientRect(hwnd, &client);
            const int width = std::max(320, static_cast<int>(client.right));
            const int height = std::max(240, static_cast<int>(client.bottom));
            MoveWindow(logEdit_, 16, 16, width - 32, height - 82, TRUE);
            MoveWindow(logClearButton_, 16, height - 52, 138, 36, TRUE);
            roundControl(logEdit_, 18);
            roundControl(logClearButton_, 18);
            return 0;
        }
        case WM_ERASEBKGND: {
            RECT client{};
            GetClientRect(hwnd, &client);
            FillRect(reinterpret_cast<HDC>(wParam), &client, backgroundBrush_);
            return 1;
        }
        case WM_CTLCOLOREDIT:
        case WM_CTLCOLORSTATIC: {
            HDC dc = reinterpret_cast<HDC>(wParam);
            SetTextColor(dc, darkMode_ ? RGB(232, 236, 242) : RGB(30, 42, 56));
            SetBkColor(dc, darkMode_ ? RGB(36, 39, 51) : RGB(255, 255, 255));
            return reinterpret_cast<LRESULT>(panelBrush_);
        }
        case WM_DRAWITEM:
            drawOwnerButton(*reinterpret_cast<DRAWITEMSTRUCT*>(lParam));
            return TRUE;
        case WM_COMMAND:
            if (LOWORD(wParam) == kLogClearButtonId && HIWORD(wParam) == BN_CLICKED) {
                const int answer = MessageBoxW(
                    hwnd, localize(L"确定清空全部求解日志吗？", L"Clear all solution history?").c_str(),
                    localize(L"清空日志", L"Clear History").c_str(),
                    MB_YESNO | MB_ICONQUESTION | MB_DEFBUTTON2);
                if (answer == IDYES) {
                    history_.clear();
                    saveHistory(history_);
                    updateHistoryWindow();
                }
            }
            return 0;
        case WM_CLOSE:
            DestroyWindow(hwnd);
            return 0;
        case WM_DESTROY:
            logWindow_ = nullptr;
            logEdit_ = nullptr;
            logClearButton_ = nullptr;
            return 0;
        }
        return DefWindowProcW(hwnd, message, wParam, lParam);
    }

    LRESULT handleMessage(UINT message, WPARAM wParam, LPARAM lParam) {
        switch (message) {
        case WM_CREATE:
            createControls();
            return 0;
        case WM_SIZE:
            layoutControls();
            return 0;
        case WM_ERASEBKGND: {
            RECT client{};
            GetClientRect(window_, &client);
            FillRect(reinterpret_cast<HDC>(wParam), &client, backgroundBrush_);
            return 1;
        }
        case WM_GETMINMAXINFO: {
            auto* info = reinterpret_cast<MINMAXINFO*>(lParam);
            info->ptMinTrackSize = {920, 650};
            return 0;
        }
        case WM_DRAWITEM:
            drawOwnerButton(*reinterpret_cast<DRAWITEMSTRUCT*>(lParam));
            return TRUE;
        case WM_COMMAND: {
            const int id = LOWORD(wParam);
            const int notification = HIWORD(wParam);
            if (id == kTypeComboId && notification == BN_CLICKED) {
                showTypeMenu();
            } else if (id == kAngleUnitButtonId && notification == BN_CLICKED) {
                showAngleUnitMenu();
            } else if (id == kLanguageComboId && notification == BN_CLICKED) {
                showLanguageMenu();
            } else if (id == kSolveButtonId && notification == BN_CLICKED) {
                solve();
            } else if (id == kClearButtonId && notification == BN_CLICKED) {
                clearInput();
            } else if (id == kExampleButtonId && notification == BN_CLICKED) {
                SetWindowTextW(input_, kTypes[currentType_].example);
                SetFocus(input_);
                SendMessageW(input_, EM_SETSEL, 0, -1);
            } else if (id == kHistoryButtonId && notification == BN_CLICKED) {
                showHistoryWindow();
            } else if (id == kGraphButtonId && notification == BN_CLICKED) {
                showGraphWindow();
            } else if (id == kThemeButtonId && notification == BN_CLICKED) {
                darkMode_ = !darkMode_;
                applyTheme();
            } else if (id >= kMethodButtonBaseId && id < kMethodButtonBaseId + 3 &&
                       notification == BN_CLICKED) {
                const int index = id - kMethodButtonBaseId;
                if (index < static_cast<int>(methodSectionsZh_.size())) {
                    selectedMethod_ = index;
                    refreshDisplayedResult();
                }
            }
            return 0;
        }
        case WM_CTLCOLOREDIT:
        case WM_CTLCOLORLISTBOX: {
            HDC dc = reinterpret_cast<HDC>(wParam);
            SetTextColor(dc, darkMode_ ? RGB(232, 236, 242) : RGB(30, 42, 56));
            SetBkColor(dc, darkMode_ ? RGB(36, 39, 51) : RGB(255, 255, 255));
            return reinterpret_cast<LRESULT>(panelBrush_);
        }
        case WM_CTLCOLORSTATIC: {
            HDC dc = reinterpret_cast<HDC>(wParam);
            HWND control = reinterpret_cast<HWND>(lParam);
            SetBkMode(dc, TRANSPARENT);
            const bool muted = control == subtitle_ || control == hint_ || control == syntax_ ||
                               control == methodLabel_ || control == approximation_;
            SetTextColor(dc, muted ? (darkMode_ ? RGB(165, 172, 187) : RGB(92, 105, 121))
                                   : (darkMode_ ? RGB(242, 244, 248) : RGB(30, 42, 56)));
            return reinterpret_cast<LRESULT>(backgroundBrush_);
        }
        case WM_DESTROY:
            DeleteObject(normalFont_);
            DeleteObject(titleFont_);
            DeleteObject(smallFont_);
            DeleteObject(panelBrush_);
            DeleteObject(backgroundBrush_);
            PostQuitMessage(0);
            return 0;
        }
        return DefWindowProcW(window_, message, wParam, lParam);
    }

    HINSTANCE instance_{};
    HWND window_{};
    HFONT normalFont_{};
    HFONT titleFont_{};
    HFONT smallFont_{};
    HBRUSH panelBrush_{};
    HBRUSH backgroundBrush_{};
    bool darkMode_ = false;
    Language language_ = Language::Simplified;
    HWND title_{};
    HWND subtitle_{};
    HWND languageCombo_{};
    HWND themeButton_{};
    HWND typeLabel_{};
    HWND typeCombo_{};
    HWND angleUnitButton_{};
    HWND hint_{};
    HWND inputLabel_{};
    HWND inputFrame_{};
    HWND input_{};
    HWND inputScrollbar_{};
    HWND syntaxTitle_{};
    HWND syntax_{};
    HWND exampleButton_{};
    HWND clearButton_{};
    HWND solveButton_{};
    HWND resultTitle_{};
    HWND graphButton_{};
    HWND historyButton_{};
    HWND resultSummary_{};
    HWND approximation_{};
    HWND methodLabel_{};
    std::array<HWND, 3> methodButtons_{};
    HWND process_{};
    HWND logWindow_{};
    HWND logEdit_{};
    HWND logClearButton_{};
    HWND graphWindow_{};
    HWND graphCanvas_{};
    int currentType_ = 0;
    equation::AngleUnit angleUnit_ = equation::AngleUnit::Radians;
    int selectedMethod_ = 0;
    std::wstring lastSummaryZh_;
    std::wstring lastApproximationZh_;
    std::vector<std::wstring> methodSectionsZh_;
    std::vector<HistoryEntry> history_;
    equation::SolveResult lastVariableContext_;
    bool graphEnabled_ = false;
    bool lastGraphSystem2_ = false;
    std::wstring lastSolvedInput_;
    std::vector<std::pair<std::wstring, double>> lastGraphValues_;
};

} // namespace

int WINAPI wWinMain(HINSTANCE instance, HINSTANCE, PWSTR, int showCommand) {
    SetProcessDpiAwarenessContext(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2);
    Gdiplus::GdiplusStartupInput gdiplusInput;
    ULONG_PTR gdiplusToken = 0;
    if (Gdiplus::GdiplusStartup(&gdiplusToken, &gdiplusInput, nullptr) != Gdiplus::Ok)
        return 1;
    INITCOMMONCONTROLSEX controls{sizeof(controls), ICC_STANDARD_CLASSES};
    InitCommonControlsEx(&controls);
    SolverApp app(instance);
    if (!app.create(showCommand)) {
        MessageBoxW(nullptr, L"程序窗口创建失败。", L"错误", MB_OK | MB_ICONERROR);
        Gdiplus::GdiplusShutdown(gdiplusToken);
        return 1;
    }
    MSG message{};
    while (GetMessageW(&message, nullptr, 0, 0) > 0) {
        TranslateMessage(&message);
        DispatchMessageW(&message);
    }
    const int exitCode = static_cast<int>(message.wParam);
    Gdiplus::GdiplusShutdown(gdiplusToken);
    return exitCode;
}
