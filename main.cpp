// Win32 桌面界面入口：负责三语言、本地主题、历史记录、解法分页、数学结果和图像窗口。
// 数学解析与求解分别委托给 expression_parser、solver、math_view 和 graph_view。

#include "expression_parser.h"
#include "graph_view.h"
#include "history_store.h"
#include "math_view.h"

#include <commctrl.h>
#include <commdlg.h>
#include <dwmapi.h>
#include <gdiplus.h>
#include <uxtheme.h>
#include <windows.h>
#include <windowsx.h>

#include <algorithm>
#include <array>
#include <cerrno>
#include <cmath>
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
constexpr wchar_t kDropdownWindowClass[] = L"EquationSolverDropdownWindow";
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
constexpr int kLogEditId = 112;
constexpr int kDropdownListId = 113;
constexpr int kLogImportId = 115;
constexpr int kLogExportId = 116;
constexpr int kMethodButtonBaseId = 200;
constexpr int kDropdownScrollbarWidth = 16;
constexpr int kDropdownArrowHeight = 16;
constexpr int kDropdownMinimumThumbHeight = 28;
constexpr int kAppIconId = 101;
constexpr DWORD kImmersiveDarkModeAttribute = 20;
constexpr DWORD kWindowCornerPreferenceAttribute = 33;
constexpr DWORD kCaptionColorAttribute = 35;
constexpr DWORD kCaptionTextColorAttribute = 36;
constexpr UINT kDropdownCommitMessage = WM_APP + 101;
constexpr UINT_PTR kEquationInputSubclassId = 0x45514E49;
constexpr int kDesignWindowWidth = 1080;
constexpr int kDesignWindowHeight = 720;
constexpr int kReferenceWorkWidth = 1600;
constexpr int kReferenceWorkHeight = 900;

double displayScaleForWorkArea(int workWidth, int workHeight, UINT dpi) {
    const double dpiScale = std::max(0.5, static_cast<double>(dpi) / 96.0);
    const double resolutionScale = std::min(static_cast<double>(workWidth) / kReferenceWorkWidth,
                                            static_cast<double>(workHeight) / kReferenceWorkHeight);
    const double availableScale =
        std::min(static_cast<double>(std::max(1, workWidth - 32)) / kDesignWindowWidth,
                 static_cast<double>(std::max(1, workHeight - 32)) / kDesignWindowHeight);
    return std::clamp(std::min(std::max(dpiScale, resolutionScale), availableScale), 0.5, 1.75);
}

wchar_t matchingRightGrouping(wchar_t opening) {
    switch (opening) {
    case L'(':
        return L')';
    case L'（':
        return L'）';
    case L'[':
        return L']';
    case L'［':
        return L'］';
    case L'【':
        return L'】';
    case L'{':
        return L'}';
    case L'｛':
        return L'｝';
    default:
        return L'\0';
    }
}

bool isRightGrouping(wchar_t value) {
    return value == L')' || value == L'）' || value == L']' || value == L'］' || value == L'】' ||
           value == L'}' || value == L'｝';
}

std::wstring windowText(HWND window) {
    const int length = GetWindowTextLengthW(window);
    std::wstring text(static_cast<size_t>(length) + 1, L'\0');
    GetWindowTextW(window, text.data(), length + 1);
    text.resize(static_cast<size_t>(length));
    return text;
}

LRESULT CALLBACK equationInputSubclassProc(HWND edit, UINT message, WPARAM wParam, LPARAM lParam,
                                           UINT_PTR subclassId, DWORD_PTR) {
    if (message == WM_CHAR) {
        DWORD selectionStart = 0;
        DWORD selectionEnd = 0;
        SendMessageW(edit, EM_GETSEL, reinterpret_cast<WPARAM>(&selectionStart),
                     reinterpret_cast<LPARAM>(&selectionEnd));
        const wchar_t typed = static_cast<wchar_t>(wParam);
        const std::wstring current = windowText(edit);

        if (const wchar_t closing = matchingRightGrouping(typed)) {
            std::wstring replacement;
            replacement += typed;
            if (selectionEnd > selectionStart && selectionEnd <= current.size())
                replacement += current.substr(selectionStart, selectionEnd - selectionStart);
            replacement += closing;
            SendMessageW(edit, EM_REPLACESEL, TRUE, reinterpret_cast<LPARAM>(replacement.c_str()));
            const DWORD caret = selectionEnd > selectionStart
                                    ? selectionStart + static_cast<DWORD>(replacement.size())
                                    : selectionStart + 1;
            SendMessageW(edit, EM_SETSEL, caret, caret);
            return 0;
        }

        if (isRightGrouping(typed) && selectionStart == selectionEnd && selectionStart < current.size() &&
            current[selectionStart] == typed) {
            SendMessageW(edit, EM_SETSEL, selectionStart + 1, selectionStart + 1);
            return 0;
        }

        if (typed == L'\b' && selectionStart == selectionEnd && selectionStart > 0 &&
            selectionStart < current.size() &&
            matchingRightGrouping(current[selectionStart - 1]) == current[selectionStart]) {
            SendMessageW(edit, EM_SETSEL, selectionStart - 1, selectionStart + 1);
            SendMessageW(edit, EM_REPLACESEL, TRUE, reinterpret_cast<LPARAM>(L""));
            return 0;
        }
    } else if (message == WM_NCDESTROY) {
        RemoveWindowSubclass(edit, equationInputSubclassProc, subclassId);
    }
    return DefSubclassProc(edit, message, wParam, lParam);
}

enum class Language { Simplified, English, Traditional };
enum class DropdownKind { None, Type, Language, AngleUnit };

struct TypeInfo {
    const wchar_t* simplified;
    const wchar_t* english;
    const wchar_t* hintSimplified;
    const wchar_t* hintEnglish;
    const wchar_t* example;
};

constexpr std::array<TypeInfo, 12> kTypes{
    {{L"自动识别", L"Auto detect", L"自动识别方程、不等式或多行方程组",
      L"Automatically detect equations, inequalities, or systems", L"1/(x-1)+1/(x+1)=1"},
     {L"一元一次方程", L"Linear equation", L"支持括号、数值分数和省略乘号",
      L"Supports parentheses, fractions, and implicit multiplication", L"2(x-1)+5=11"},
     {L"二元一次方程组", L"2-variable system", L"每个方程输入一行，也可用分号分隔",
      L"Enter one equation per line, or separate them with semicolons", L"2x+y=7\r\nx-y=2"},
     {L"一元二次方程", L"Quadratic equation", L"使用 ^2 或 ² 表示平方；支持实根和复数根",
      L"Use ^2 or ²; real and complex roots are supported", L"x^2-2x-1=0"},
     {L"二元多项式方程组", L"2-variable polynomial system", L"输入两行总次数不超过 4 的方程，求全部实数解",
      L"Enter two equations of total degree at most 4 to find every real solution", L"x^2+y^2=5\r\nx-y=1"},
     {L"一元三次方程", L"Cubic equation", L"支持因式分解、综合除法以及实根和复数根",
      L"Supports factorization, synthetic division, and real or complex roots", L"x^3-6x^2+11x-6=0"},
     {L"一元四次方程", L"Quartic equation", L"支持双二次换元、因式分解以及费拉里精确公式",
      L"Supports biquadratic substitution, factorization, and Ferrari's exact formula", L"x^4-5x^2+5=0"},
     {L"三元一次方程组", L"3-variable system", L"输入三行含 x、y、z 的一次方程",
      L"Enter three linear equations containing x, y, and z", L"x+y+z=6\r\n2x-y+z=3\r\nx+2y-z=2"},
     {L"一元一次不等式", L"Linear inequality", L"支持 <、<=、>、>=、≤、≥", L"Supports <, <=, >, >=, ≤, and ≥",
      L"-2x+3<=7"},
     {L"分式方程", L"Rational equation", L"未知数可位于分母；自动列定义域并验根",
      L"Variables may appear in denominators; domain and roots are checked", L"1/(x-1)+1/(x+1)=1"},
     {L"三角方程", L"Trigonometric equation", L"支持弧度制和角度制以及 sin、cos、tan、cot 的周期通解",
      L"Supports radians, degrees, and periodic solutions for sin, cos, tan, and cot", L"sin(x)=1/2"},
     {L"关系式求解", L"Rearrange relation", L"支持各类关系；给出显式表达式或精确隐式解集，不显示过程",
      L"All relation types: explicit forms or exact implicit solution sets, without steps", L"a^2+b^2=1"}}};
static_assert(kTypes.size() == static_cast<size_t>(equation::InputKind::Relation) + 1);

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
    static constexpr std::array<wchar_t, 10> superscriptDigits{L'⁰', L'¹', L'²', L'³', L'⁴',
                                                               L'⁵', L'⁶', L'⁷', L'⁸', L'⁹'};
    std::wstring result;
    result.reserve(text.size());
    for (size_t index = 0; index < text.size();) {
        if (text[index] != L'^') {
            result += text[index++];
            continue;
        }
        size_t cursor = index + 1;
        const bool negative = cursor < text.size() && text[cursor] == L'-';
        if (negative)
            ++cursor;
        const size_t digitsBegin = cursor;
        while (cursor < text.size() && std::iswdigit(text[cursor]))
            ++cursor;
        if (cursor == digitsBegin) {
            result += text[index++];
            continue;
        }
        if (negative)
            result += L'⁻';
        for (size_t digit = digitsBegin; digit < cursor; ++digit)
            result += superscriptDigits[static_cast<size_t>(text[digit] - L'0')];
        index = cursor;
    }
    return result;
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
        {L"结论：定义域为空，无解", L"No solution: the domain is empty"},
        {L"关系式过长，请缩短输入", L"The relation is too long; shorten the input"},
        {L"请输入需要求解的关系式", L"Enter a relation to solve"},
        {L"根号内的表达式无法解析", L"The expression inside the radical cannot be parsed"},
        {L"根指数须为 2 至 64 的整数", L"The root index must be an integer from 2 to 64"},
        {L"关系式中包含无法识别的表达式", L"The relation contains an unrecognized expression"},
        {L"复数不能比较大小，含 i 的关系式请使用等号",
         L"Complex numbers are unordered; use equality for a relation containing i"},
        {L"请输入有效数字", L"Enter a valid number"},
        {L"绝对值符号不匹配", L"Absolute-value delimiters do not match"},
        {L"隐式解集", L"implicit solution set"},
        {L" 有定义", L" is defined"},
        {L"关系式求解只需输入一行等式", L"Enter a single equality to rearrange a relation"},
        {L"关系式求解只支持等号 =", L"Rearranging a relation requires an equals sign (=)"},
        {L"关系式求解目前只支持一次等式", L"Rearranging relations currently supports linear equalities"},
        {L"关系式求解暂不支持未知数位于分母",
         L"Rearranging relations does not yet support variables in denominators"},
        {L"关系式求解暂不支持分数指数", L"Rearranging relations does not yet support fractional exponents"},
        {L"关系式求解暂不支持虚数系数", L"Rearranging relations does not yet support complex coefficients"},
        {L"结论：等式恒成立", L"The equality is always true"},
        {L"关系式求解", L"Rearrange relation"},
        // Complete sentences and long phrases must be translated before their
        // shorter components.  This keeps dynamic mathematical values intact
        // while avoiding mixed output such as "row 2 步".
        {L"把 i 作为虚数单位（i² = -1），对复系数增广矩阵进行高斯消元",
         L"Treat i as the imaginary unit (i² = -1) and apply Gaussian elimination to the complex augmented "
         L"matrix"},
        {L"答案过长，仅显示近似值：", L"The exact answer is too long; showing approximate values only: "},
        {L"目标：先找到一个容易计算的根，就能把三次或四次方程降低一次。",
         L"Goal: first find a root that is easy to calculate, which reduces a cubic or quartic equation by "
         L"one degree."},
        {L"有理根定理：候选根可写成 ±p/q，p 是常数项的因数，q 是最高次项系数的因数。",
         L"The rational root theorem says that a candidate root has the form ±p/q, where p is a factor of "
         L"the constant term and q is a factor of the leading coefficient."},
        {L"乘积等于 0 时，至少一个因式等于 0。现在只需解最后的低次方程：",
         L"When a product equals 0, at least one factor equals 0. Now solve only the final lower-degree "
         L"equation:"},
        {L"综合除法是“除以 x - r”的简写算法。只写系数，每次按“落下、相乘、相加”计算。",
         L"Synthetic division is a shorthand for division by x - r. Write only the coefficients and repeat "
         L"the three actions: bring down, multiply, and add."},
        {L"这是一元二次方程，先算 Δ = b² - 4ac，再使用 x = (-b ± √Δ)/(2a)。",
         L"This is a quadratic equation. First calculate Δ = b² - 4ac, then use x = (-b ± √Δ)/(2a)."},
        {L"这是一元一次方程，把常数项移到右边，再除以 x 的系数。",
         L"This is a linear equation. Move the constant term to the right, then divide by the coefficient of "
         L"x."},
        {L"目标：把四次式拆成两个二次式，然后用熟悉的二次方程求根公式。",
         L"Goal: split the quartic into two quadratics, then use the familiar quadratic formula."},
        {L"为了把它配成两个二次因式，引入辅助数 m；m 不是原方程的答案。",
         L"Introduce an auxiliary number m so the expression can be written as two quadratic factors; m is "
         L"not an answer to the original equation."},
        {L"因为 x⁴ = (x²)²，先把 x² 看成一个整体，四次方程就会变成熟悉的二次方程。",
         L"Because x⁴ = (x²)², treat x² as one quantity so the quartic becomes a familiar quadratic."},
        {L"思路：先用平移换元去掉二次项，再把新未知数拆成两个数的和。",
         L"Idea: first remove the quadratic term by a translation, then split the new unknown into a sum of "
         L"two numbers."},
        {L"从绝对值较小的候选数开始代入，取 x = ",
         L"Start with candidates of smaller absolute value and take x = "},
        {L"代入后结果为 0，所以 x = ", L"The substituted result is 0, so x = "},
        {L" 是根。根据因式定理，P", L" is a root. By the factor theorem, P"},
        {L" 含有因式 ", L" contains the factor "},
        {L"除去这个因式，得到低一次的多项式：",
         L"After removing this factor, the polynomial of one lower degree is:"},
        {L"原方程因此可写成：", L"Therefore the original equation can be written as:"},
        {L"合并前面找到的根与低次方程的根，就得到原方程的全部根。",
         L"Combine the roots found above with the roots of the lower-degree equation to obtain all roots of "
         L"the original equation."},
        {L"合并综合除法得到的根与最后低次方程的根，即为原方程的全部根。",
         L"Combine the roots from synthetic division with the roots of the final lower-degree equation to "
         L"obtain all roots of the original equation."},
        {L"原系数（按降幂排列）：", L"Original coefficients (in descending powers): "},
        {L"先落下首项系数 ", L"First bring down the leading coefficient "},
        {L"余数为 0，说明可以整除。商的系数为：",
         L"The remainder is 0, so the division is exact. The quotient coefficients are: "},
        {L"第 1 步：对当前多项式 P", L"Step 1: find a rational root of the current polynomial P"},
        {L"第 2 步：对当前多项式 P", L"Step 2: find a rational root of the current polynomial P"},
        {L"第 3 步：对当前多项式 P", L"Step 3: find a rational root of the current polynomial P"},
        {L"步：对当前多项式 P", L"Step: find a rational root of the current polynomial P"},
        {L" 寻找有理根。", L"."},
        {L"p 可取：", L"Possible p values: "},
        {L"；q 可取：", L"; possible q values: "},
        {L"解得：", L"Solutions: "},
        {L"因此：", L"Therefore: "},
        {L"降次后得到：", L"After reducing the degree, obtain: "},
        {L"（余数）", L" (remainder)"},
        {L"把 i 作为虚数单位（i² = -1），移项、通分并合并同类项",
         L"Treat i as the imaginary unit (i² = -1), rearrange, use a common denominator, and combine like "
         L"terms"},
        {L"把 i 作为虚数单位（i² = -1），高斯消元后出现矛盾行",
         L"Treat i as the imaginary unit (i² = -1). Gaussian elimination produces a contradictory row"},
        {L"把 i 作为虚数单位（i² = -1），高斯消元后存在自由变量",
         L"Treat i as the imaginary unit (i² = -1). Free variables remain after Gaussian elimination"},
        {L"输入原方程组", L"Original system"},
        {L"二元多项式方程组需要输入两行方程",
         L"A two-variable polynomial system requires exactly two equation lines"},
        {L"二元多项式方程组暂不支持分数指数",
         L"Fractional exponents are not currently supported in a two-variable polynomial system"},
        {L"二元多项式方程组暂不支持未知数位于分母",
         L"Variables in denominators are not currently supported in a two-variable polynomial system"},
        {L"二元多项式方程组只能包含两个未知数",
         L"A two-variable polynomial system can contain only two unknowns"},
        {L"二元多项式方程组必须包含两个未知数",
         L"A two-variable polynomial system must contain two unknowns"},
        {L"二元多项式方程组目前支持每个未知数次数及总次数不超过 4",
         L"A two-variable polynomial system currently supports degree at most 4 in each variable and in "
         L"total"},
        {L"方程组中的每一行都必须使用等号 =", L"Every line in a system must use the equals sign ="},
        {L"化简后两个方程均为一次方程，转入二元一次方程组求解。",
         L"After simplification both equations are linear, so solve them as a two-variable linear system."},
        {L"消元所得结式恒为 0，两个方程存在公共因式或公共曲线。",
         L"The resultant is identically zero, so the equations share a factor or a common curve."},
        {L"消元后出现整条竖直公共解集，因此实数解有无穷多个。",
         L"Elimination produces an entire vertical common solution set, so there are infinitely many real "
         L"solutions."},
        {L"该结式没有产生同时满足两个原方程的实数坐标。",
         L"The resultant produces no real coordinates satisfying both original equations."},
        {L"把两个方程都看成关于 y 的多项式，建立 Sylvester 矩阵并计算结式以消去 y：",
         L"Regard both equations as polynomials in y, build the Sylvester matrix, and calculate the "
         L"resultant "
         L"to eliminate y:"},
        {L"求出结式的全部实根 x；每个 x 都要回代原方程，排除结式引入的伪根。",
         L"Find every real root x of the resultant; substitute each one into the original equations to "
         L"reject "
         L"extraneous roots introduced by elimination."},
        {L"先把两个方程展开并写成左边为多项式、右边为 0 的标准式：",
         L"Expand both equations and write them in standard form with a polynomial on the left and 0 on the "
         L"right:"},
        {L"对结式得到的每个 x，优先代入关于 y 次数较低的方程求出全部实数 y；再将每个候选坐标同时代入 "
         L"(1)、(2)，只有两式残差均在允许范围内才保留。",
         L"For each x from the resultant, substitute first into the equation of lower degree in y to obtain "
         L"all "
         L"real y values. Keep a candidate only when substitution into both (1) and (2) gives acceptable "
         L"residuals."},
        {L"将两式按相应二次项系数配倍相减，消去一个二次项；再把降次关系代回任一原方程。",
         L"Scale and subtract the equations to eliminate a quadratic term, then substitute the reduced "
         L"relation into either original equation."},
        {L"先把两个方程写成左边为多项式、右边为 0 的标准式：",
         L"First write both equations in standard form with a polynomial on the left and 0 on the right:"},
        {L"这就是少一个二次项的降次关系。把它与 (1) 联立，先求一个未知数，再代入求另一个。",
         L"This reduced relation has one fewer quadratic term. Combine it with (1), solve for one unknown, "
         L"then substitute to find the other."},
        {L"选择只在一个方程中出现的二次项，从另一个方程解出较简单的关系并代入。",
         L"Choose a quadratic term that occurs in only one equation, obtain a simpler relation from the "
         L"other equation, and substitute it."},
        {L"全部候选点均已完成回代筛选，所得坐标与结式法一致。",
         L"Every candidate point has been checked by substitution; the coordinates agree with the "
         L"resultant method."},
        {L"代入降次关系及 (1)，解得 y ", L"substitute into the reduced relation and (1) to obtain y "},
        {L"第 1 组：取 x ", L"Solution 1: take x "},
        {L"第 2 组：取 x ", L"Solution 2: take x "},
        {L"第 3 组：取 x ", L"Solution 3: take x "},
        {L"第 4 组：取 x ", L"Solution 4: take x "},
        {L"第 5 组：取 x ", L"Solution 5: take x "},
        {L"第 6 组：取 x ", L"Solution 6: take x "},
        {L"组：取 x ", L": take x "},
        {L"用 (", L"Use ("},
        {L"，消去 ", L" to eliminate "},
        {L" 项", L" term"},
        {L"所得坐标与结式法一致。", L"The resulting coordinates agree with the resultant method."},
        {L"组解：代入两个原方程，左右两边均相等。",
         L": substituting into both original equations makes their two sides equal."},
        {L"方法一：结式消元法", L"Method 1: elimination by resultant"},
        {L"方法二：加减消元与代入法", L"Method 2: addition/subtraction elimination and substitution"},
        {L"方法二：逐根回代与双式验算法", L"Method 2: root-by-root substitution and verification"},
        {L"回代次数较低的方程，解得 y ", L"substitute into the lower-degree equation to obtain y "},
        {L"结论：二元多项式方程组无实数解",
         L"Conclusion: the two-variable polynomial system has no real solution"},
        {L"结论：方程组有无穷多个实数解", L"Conclusion: the system has infinitely many real solutions"},
        {L"方程最高次项系数不能为 0", L"The leading coefficient cannot be zero"},
        {L"该分支为三次或四次方程，使用精确代数公式求根并保留分数与根式。",
         L"This branch is cubic or quartic, so solve it with an exact algebraic formula and retain "
         L"fractions and radicals."},
        {L"换元后得到三次或四次方程，使用精确代数公式求根，并保留分数与根式。",
         L"The substitution produces a cubic or quartic equation. Solve it with an exact algebraic "
         L"formula and retain fractions and radicals."},
        {L"令 t = x²，原方程化为关于 t 的一元二次方程：",
         L"Let t = x². The original equation becomes a quadratic equation in t:"},
        {L"再由 x² = t 求出上述各根。", L"Then solve x² = t to obtain the roots above."},
        {L"把四次多项式看作 x² 的二次式，写成两个关于 x² 的因式；令每个因式为 0，所得根与换元法一致。",
         L"Treat the quartic as a quadratic in x² and factor it into two expressions in x². Setting each "
         L"factor to zero gives the same roots as substitution."},
        {L"检验得到有理根 x = ", L"Testing gives the rational root x = "},
        {L"继续分解剩余低次多项式，得到结果中的全部根。",
         L"Continue factoring the remaining lower-degree polynomial to obtain every root in the result."},
        {L"用每个有理根作综合除法降低次数，再对剩余的一次或二次方程使用移项或求根公式。",
         L"Use synthetic division for each rational root, then solve the remaining linear or quadratic "
         L"equation by rearrangement or the quadratic formula."},
        {L"按降幂顺序写出系数，并对每个有理根执行“首项落下、乘根、与下一项相加”：",
         L"Write the coefficients in descending powers. For each rational root, bring down the leading "
         L"coefficient, multiply by the root, and add to the next coefficient:"},
        {L"第 1 次综合除法，使用根 r = ", L"Synthetic-division step 1, using root r = "},
        {L"第 2 次综合除法，使用根 r = ", L"Synthetic-division step 2, using root r = "},
        {L"第 3 次综合除法，使用根 r = ", L"Synthetic-division step 3, using root r = "},
        {L"次综合除法，使用根 r = ", L" synthetic-division step, using root r = "},
        {L"降次后得到 ", L"After reducing the degree, obtain "},
        {L"对这个二次方程计算判别式并使用求根公式；",
         L"Calculate the discriminant of this quadratic and apply the quadratic formula;"},
        {L"对这个一次方程移项并除以 x 的系数；",
         L"Rearrange this linear equation and divide by the coefficient of x;"},
        {L"合并综合除法得到的根与低次方程的根，即为原方程的全部根。",
         L"Combine the roots from synthetic division with those of the lower-degree equation to obtain all "
         L"roots of the original equation."},
        {L"使用 Durand-Kerner 数值迭代同时求出多项式的全部实根和复数根。",
         L"Use Durand-Kerner numerical iteration to find all real and complex roots simultaneously."},
        {L"将各根代回标准多项式验算，残差均在允许误差内。",
         L"Substitute every root into the standard polynomial; each residual is within tolerance."},
        {L"方法一：换元法", L"Method 1: substitution"},
        {L"方法一：开立方与单位根法", L"Method 1: cube roots and roots of unity"},
        {L"方法一：卡尔达诺公式", L"Method 1: Cardano's formula"},
        {L"方法一：三角形式的卡尔达诺公式", L"Method 1: trigonometric form of Cardano's formula"},
        {L"方法一：平移换元与双二次换元法", L"Method 1: translation and biquadratic substitution"},
        {L"方法一：费拉里法", L"Method 1: Ferrari's method"},
        {L"方法二：因式分解法", L"Method 2: factorization"},
        {L"方法二：降次与复数单位根法", L"Method 2: depressed cubic and complex roots of unity"},
        {L"方法二：三倍角公式", L"Method 2: triple-angle identity"},
        {L"方法二：平移后因式分解法", L"Method 2: factorization after translation"},
        {L"方法二：配方并分解为两个二次因式",
         L"Method 2: completing the square and factoring into two quadratics"},
        {L"，原方程化为 y⁴ + py² + r = 0。", L", so the original equation becomes y⁴ + py² + r = 0."},
        {L"再令 t = y²，精确求解 t² + pt + r = 0，并对每个 t 保留平方根形式。",
         L"Then let t = y², solve t² + pt + r = 0 exactly, and retain the square-root form for each t."},
        {L"把降幂四次式看作 y² 的二次式，分解为两个关于 y² 的因式；逐个令因式为 "
         L"0，所得四个精确根与方法一一致。",
         L"Treat the depressed quartic as a quadratic in y² and factor it into two expressions in y². "
         L"Setting each factor to zero gives the same four exact roots as Method 1."},
        {L"对每个 t 解 y² = t，最后由 x = y + (", L"For each t, solve y² = t, then use x = y + ("},
        {L") 还原并保留精确根式。", L") to recover x while retaining exact radicals."},
        {L"根据上面两个 t 根，降幂四次式可直接写成：",
         L"Using the two t-roots above, write the depressed quartic directly as:"},
        {L"分别令两个因式为 0：", L"Set each factor equal to 0:"},
        {L"每个等式各取正、负平方根，再由 x = y + (",
         L"Take the positive and negative square roots in each equation, then use x = y + ("},
        {L") 还原，所得四个精确根与方法一逐一相同。",
         L") to recover x. The four exact roots agree individually with Method 1."},
        {L"，原方程化为 y⁴ + py² + qy + r = 0。",
         L", so the original equation becomes y⁴ + py² + qy + r = 0."},
        {L"令 m 满足预解三次方程：", L"Let m satisfy the resolvent cubic:"},
        {L"取正实根 m = ", L"Choose the positive real root m = "},
        {L"，再令 s = √m = ", L", then let s = √m = "},
        {L"，代入结果中的四个精确根式。",
         L" and substitute it into the four exact radical expressions shown in the result."},
        {L"把四次式配成两个二次式后，两个判别式分别为：",
         L"After completing the quartic into two quadratics, their discriminants are:"},
        {L"代入二次方程求根公式，并由 x = y + (", L"Apply the quadratic formula, then use x = y + ("},
        {L") 还原，得到结果中的四个精确根式。",
         L") to recover the four exact radical roots shown in the result."},
        {L"设 y⁴ + py² + qy + r = (y² + sy + t)(y² - sy + u)。",
         L"Set y⁴ + py² + qy + r = (y² + sy + t)(y² - sy + u)."},
        {L"比较 y²、y 和常数项系数，有：",
         L"Comparing the coefficients of y², y, and the constant term gives:"},
        {L"因此原式分解为：", L"Therefore the expression factors as:"},
        {L"分别解两个二次方程：", L"Solve the two quadratic equations separately:"},
        {L"最后由 x = y + (", L"Finally, use x = y + ("},
        {L") 还原，四个根与费拉里法一致。", L") to recover x; the four roots agree with Ferrari's method."},
        {L"由 s = ", L"From s = "},
        {L" 解得：", L", obtain:"},
        {L"把降幂四次式分解为 (y² + sy + t)(y² - sy + "
         L"u)，分别使用一元二次方程求根公式，所得四个根与费拉里法一致。",
         L"Factor the depressed quartic as (y² + sy + t)(y² - sy + u), then apply the quadratic formula "
         L"to both factors. The four roots agree with Ferrari's method."},
        {L"，得到降幂三次方程 u³ + pu + q = 0。", L", producing the depressed cubic u³ + pu + q = 0."},
        {L"，代入卡尔达诺公式得到结果中的三个精确根。",
         L". Substitution into Cardano's formula gives the three exact roots shown in the result."},
        {L"先消去二次项，再把 A、B 分别乘以三个三次单位根组合；所得一实根和一对共轭复根与方法一一致。",
         L"First remove the quadratic term, then combine A and B with the three cube roots of unity. The "
         L"resulting real root and conjugate complex pair agree with Method 1."},
        {L"沿用降幂方程 ", L"Continue with the depressed cubic "},
        {L"并取上面算出的 A、B。", L"and use the A and B calculated above."},
        {L"令 ω = (-1 + √3i)/2，则 ω³ = 1 且 1 + ω + ω² = 0。",
         L"Let ω = (-1 + √3i)/2; then ω³ = 1 and 1 + ω + ω² = 0."},
        {L"三组配对必须保持乘积 AB = -p/3：", L"The three pairings must preserve the product AB = -p/3:"},
        {L"最后由 x = u + (", L"Finally, use x = u + ("},
        {L") 分别还原，得到方法一中的一实根和一对共轭复根。",
         L") to recover each root, giving the real root and conjugate complex pair from Method 1."},
        {L"三个根均为实数。令 θ = acos(", L"All three roots are real. Let θ = acos("},
        {L")，使用结果中保留的精确余弦形式求根。",
         L") and use the exact cosine forms retained in the result."},
        {L"把降幂三次方程与 cos(3θ) = 4cos³θ - 3cosθ 对照，分别取相差 2π/3 的三个角，得到相同的三个实根。",
         L"Compare the depressed cubic with cos(3θ) = 4cos³θ - 3cosθ and use three angles separated by "
         L"2π/3 to obtain the same three real roots."},
        {L"在降幂方程中令 u = 2√(-p/3)·cos φ，代入并提取公因子后得到：",
         L"In the depressed cubic, let u = 2√(-p/3)·cos φ. Substitute and factor to obtain:"},
        {L"利用 cos(3φ) = 4cos³φ - 3cosφ，取 θ = acos(", L"Using cos(3φ) = 4cos³φ - 3cosφ, let θ = acos("},
        {L"把三个 φ 依次代回 u，再由 x = u + (",
         L"Substitute the three values of φ into u, then use x = u + ("},
        {L") 还原，得到与方法一相同的三个实根。", L") to recover the same three real roots as Method 1."},
        {L"判别量 D = ", L"Discriminant quantity D = "},
        {L"令 x = u ", L"Let x = u "},
        {L"令 x = y ", L"Let x = y "},
        {L"设 A = ", L"Let A = "},
        {L"，B = ", L", B = "},
        {L"结合三个三次单位根得到全部三个根。",
         L"combine the three cube roots of unity to obtain all three roots."},
        {L"先移项并除以三次项系数，得到 x³ = ",
         L"First rearrange and divide by the cubic coefficient to obtain x³ = "},
        {L"，则 r³ = ", L", so r³ = "},
        {L"三个三次单位根为 1、(-1 + √3i)/2、(-1 - √3i)/2，",
         L"The three cube roots of unity are 1, (-1 + √3i)/2, and (-1 - √3i)/2; "},
        {L"分别乘以 r 得到全部三个根。", L"multiply each by r to obtain all three roots."},
        {L"把方程写成 x³ - r³ = 0，并使用立方差公式：",
         L"Write the equation as x³ - r³ = 0 and apply the difference-of-cubes identity:"},
        {L"第一个因式给出 x = ", L"The first factor gives x = "},
        {L"第二个因式的判别式为 Δ = -3r²，代入求根公式：",
         L"The second factor has discriminant Δ = -3r²; apply the quadratic formula:"},
        {L"化简后得到与方法一相同的一对共轭复根。",
         L"After simplification, this gives the same conjugate complex pair as Method 1."},
        {L"由方法一算得的两个 t 根，四次多项式可写为：",
         L"Using the two t-roots from Method 1, write the quartic as:"},
        {L"根据零乘积性质，分别解：", L"By the zero-product property, solve:"},
        {L"对非负 t 取 x = ±√t；对负 t 取 x = ±√(-t)i，",
         L"For nonnegative t, take x = ±√t; for negative t, take x = ±√(-t)i; "},
        {L"化简后所得根与换元法一致。",
         L"after simplification, the roots agree with the substitution method."},
        {L"利用 x³ - r³ = (x - r)(x² + rx + r²) 因式分解，再对二次因式使用求根公式，得到相同结果。",
         L"Factor with x³ - r³ = (x - r)(x² + rx + r²), then apply the quadratic formula to the quadratic "
         L"factor to obtain the same result."},
        {L"令 r = ", L"Let r = "},
        {L"。由 x³ = r³，", L". From x³ = r³, "},
        {L"方法一：有理根定理与因式分解法", L"Method 1: rational root theorem and factorization"},
        {L"方法二：综合除法与低次方程公式", L"Method 2: synthetic division and lower-degree formulas"},
        {L"消去 y 得 R(x) = ", L"Eliminating y gives R(x) = "},
        {L"提取因式", L"and extract the factor"},
        {L"验算第 ", L"Check solution "},
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
        {L"二元多项式方程组", L"2-variable polynomial system"},
        {L"二元二次方程组", L"2-variable quadratic system"},
        {L"二元一次方程组", L"2-variable linear system"},
        {L"一元四次方程", L"Quartic equation"},
        {L"一元三次方程", L"Cubic equation"},
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
               ch == L'C' || ch == L't' || ch == L'T' || ch == L'a' || ch == L'A';
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
        if (consume(L'^')) {
            const Complex exponent = parseUnary();
            value = std::pow(value, exponent);
        }
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
        auto functionAt = [&](const wchar_t* name, size_t length) {
            for (size_t offset = 0; offset < length; ++offset) {
                if (position_ + offset >= text_.size() ||
                    std::towlower(text_[position_ + offset]) != name[offset])
                    return false;
            }
            size_t after = position_ + length;
            while (after < text_.size() && std::iswspace(text_[after]))
                ++after;
            return after < text_.size() && (text_[after] == L'(' || text_[after] == L'[' ||
                                            text_[after] == L'{' || text_[after] == L'【');
        };
        const wchar_t* function =
            functionAt(L"acos", 4)
                ? L"acos"
                : (functionAt(L"sin", 3)
                       ? L"sin"
                       : (functionAt(L"cos", 3)
                              ? L"cos"
                              : (functionAt(L"tan", 3) ? L"tan"
                                                       : (functionAt(L"cot", 3) ? L"cot" : nullptr))));
        if (function) {
            position_ += function[0] == L'a' ? 4 : 3;
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
            if (function[0] == L'a')
                return std::acos(inputAngle);
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

std::wstring threeDecimal(double value) {
    if (std::abs(value) < 0.0005)
        value = 0.0;
    std::wostringstream out;
    out << std::fixed << std::setprecision(3) << value;
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

std::wstring solutionSubscript(size_t value) {
    static constexpr std::array<wchar_t, 10> digits{L'₀', L'₁', L'₂', L'₃', L'₄',
                                                    L'₅', L'₆', L'₇', L'₈', L'₉'};
    std::wstring result;
    do {
        result.insert(result.begin(), digits[value % 10]);
        value /= 10;
    } while (value > 0);
    return result;
}

std::vector<std::wstring> solutionValueLabels() {
    std::vector<std::wstring> labels;
    labels.reserve(67);
    for (const wchar_t variable : {L'x', L'y'}) {
        for (size_t index = 1; index <= 16; ++index)
            labels.push_back(std::wstring(1, variable) + solutionSubscript(index));
    }
    for (const wchar_t variable : {L'x', L'y'}) {
        for (size_t index = 1; index <= 16; ++index)
            labels.push_back(std::wstring(1, variable) + std::to_wstring(index));
    }
    labels.insert(labels.end(), {L"x", L"y", L"z"});
    return labels;
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
    for (const std::wstring& label : solutionValueLabels()) {
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
            result += L" ≈ " + threeDecimal(items[i].value.real());
        }
        if (!exactDecimal && std::abs(items[i].value.imag()) >= 0.0005) {
            result += items[i].value.imag() >= 0 ? L" + " : L" - ";
            result += threeDecimal(std::abs(items[i].value.imag())) + L"i";
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
    for (const std::wstring& label : solutionValueLabels()) {
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

using HistoryEntry = history::Entry;

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
        if (!mathview::registerClass(instance_) || !graphview::registerClass(instance_))
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
        WNDCLASSEXW dropdownClass = logClass;
        dropdownClass.lpfnWndProc = DropdownWindowProc;
        dropdownClass.hIcon = nullptr;
        dropdownClass.hIconSm = nullptr;
        dropdownClass.lpszClassName = kDropdownWindowClass;
        if (!RegisterClassExW(&dropdownClass) && GetLastError() != ERROR_CLASS_ALREADY_EXISTS)
            return false;

        const HMONITOR monitor = MonitorFromPoint(POINT{0, 0}, MONITOR_DEFAULTTOPRIMARY);
        MONITORINFO monitorInfo{};
        monitorInfo.cbSize = sizeof(monitorInfo);
        GetMonitorInfoW(monitor, &monitorInfo);
        updateDisplayScale(monitor, GetDpiForSystem(), false);
        const int workWidth = static_cast<int>(monitorInfo.rcWork.right - monitorInfo.rcWork.left);
        const int workHeight = static_cast<int>(monitorInfo.rcWork.bottom - monitorInfo.rcWork.top);
        const int windowWidth = std::min(scaled(kDesignWindowWidth), workWidth);
        const int windowHeight = std::min(scaled(kDesignWindowHeight), workHeight);
        const int windowX =
            monitorInfo.rcWork.left + (monitorInfo.rcWork.right - monitorInfo.rcWork.left - windowWidth) / 2;
        const int windowY =
            monitorInfo.rcWork.top + (monitorInfo.rcWork.bottom - monitorInfo.rcWork.top - windowHeight) / 2;
        window_ =
            CreateWindowExW(0, kWindowClass, L"方程求解器", WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN, windowX,
                            windowY, windowWidth, windowHeight, nullptr, nullptr, instance_, this);
        if (!window_)
            return false;
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

    static LRESULT CALLBACK DropdownWindowProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam) {
        SolverApp* app = reinterpret_cast<SolverApp*>(GetWindowLongPtrW(hwnd, GWLP_USERDATA));
        if (message == WM_NCCREATE) {
            const auto* create = reinterpret_cast<CREATESTRUCTW*>(lParam);
            app = static_cast<SolverApp*>(create->lpCreateParams);
            SetWindowLongPtrW(hwnd, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(app));
        }
        return app ? app->handleDropdownMessage(hwnd, message, wParam, lParam)
                   : DefWindowProcW(hwnd, message, wParam, lParam);
    }

    static LRESULT CALLBACK DropdownListProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam,
                                             UINT_PTR subclassId, DWORD_PTR referenceData) {
        auto* app = reinterpret_cast<SolverApp*>(referenceData);
        if (!app)
            return DefSubclassProc(hwnd, message, wParam, lParam);
        if (message == WM_MOUSEMOVE) {
            const DWORD hit = static_cast<DWORD>(SendMessageW(hwnd, LB_ITEMFROMPOINT, 0, lParam));
            if (HIWORD(hit) == 0) {
                const int index = LOWORD(hit);
                if (index != app->dropdownHoverIndex_) {
                    app->dropdownHoverIndex_ = index;
                    SendMessageW(hwnd, LB_SETCURSEL, index, 0);
                    InvalidateRect(hwnd, nullptr, FALSE);
                }
            }
            TRACKMOUSEEVENT tracking{sizeof(tracking), TME_LEAVE, hwnd, 0};
            TrackMouseEvent(&tracking);
        } else if (message == WM_MOUSEWHEEL) {
            const int lines = -GET_WHEEL_DELTA_WPARAM(wParam) / WHEEL_DELTA * 3;
            app->scrollDropdownBy(lines);
            return 0;
        } else if (message == WM_LBUTTONUP) {
            const LRESULT result = DefSubclassProc(hwnd, message, wParam, lParam);
            const int index = static_cast<int>(SendMessageW(hwnd, LB_GETCURSEL, 0, 0));
            if (index >= 0)
                PostMessageW(GetParent(hwnd), kDropdownCommitMessage, index, 0);
            return result;
        } else if (message == WM_KEYDOWN) {
            if (wParam == VK_RETURN) {
                const int index = static_cast<int>(SendMessageW(hwnd, LB_GETCURSEL, 0, 0));
                if (index >= 0)
                    PostMessageW(GetParent(hwnd), kDropdownCommitMessage, index, 0);
                return 0;
            }
            if (wParam == VK_ESCAPE) {
                app->closeDropdown();
                return 0;
            }
            if (wParam == VK_UP || wParam == VK_DOWN || wParam == VK_PRIOR || wParam == VK_NEXT ||
                wParam == VK_HOME || wParam == VK_END) {
                const LRESULT result = DefSubclassProc(hwnd, message, wParam, lParam);
                InvalidateRect(GetParent(hwnd), nullptr, FALSE);
                return result;
            }
        } else if (message == WM_NCDESTROY) {
            RemoveWindowSubclass(hwnd, DropdownListProc, subclassId);
        }
        return DefSubclassProc(hwnd, message, wParam, lParam);
    }

    int scaled(int value) const {
        return std::max(value > 0 ? 1 : 0, static_cast<int>(std::lround(value * uiScale_)));
    }

    Gdiplus::REAL scaledReal(double value) const {
        return static_cast<Gdiplus::REAL>(value * uiScale_);
    }

    bool updateDisplayScale(HMONITOR monitor, UINT dpi, bool refreshFonts) {
        MONITORINFO info{};
        info.cbSize = sizeof(info);
        if (!GetMonitorInfoW(monitor, &info))
            return false;
        const double nextScale = displayScaleForWorkArea(
            info.rcWork.right - info.rcWork.left, info.rcWork.bottom - info.rcWork.top, dpi ? dpi : 96);
        const bool changed = std::abs(nextScale - uiScale_) > 0.005 || currentDpi_ != dpi;
        uiScale_ = nextScale;
        currentDpi_ = dpi ? dpi : 96;
        if (changed && refreshFonts && normalFont_) {
            closeDropdown();
            recreateFonts();
            for (const auto& secondary :
                 {std::pair<HWND, SIZE>{logWindow_, SIZE{scaled(680), scaled(560)}},
                  std::pair<HWND, SIZE>{graphWindow_, SIZE{scaled(860), scaled(640)}}}) {
                if (!secondary.first)
                    continue;
                RECT rect{};
                GetWindowRect(secondary.first, &rect);
                const int centerX = rect.left + (rect.right - rect.left) / 2;
                const int centerY = rect.top + (rect.bottom - rect.top) / 2;
                SetWindowPos(secondary.first, nullptr, centerX - secondary.second.cx / 2,
                             centerY - secondary.second.cy / 2, secondary.second.cx, secondary.second.cy,
                             SWP_NOZORDER | SWP_NOACTIVATE);
            }
        }
        return changed;
    }

    void adaptWindowToMonitor(HMONITOR monitor, UINT dpi, const RECT& referenceRect) {
        updateDisplayScale(monitor, dpi, true);
        MONITORINFO info{};
        info.cbSize = sizeof(info);
        if (!GetMonitorInfoW(monitor, &info))
            return;
        const int workWidth = info.rcWork.right - info.rcWork.left;
        const int workHeight = info.rcWork.bottom - info.rcWork.top;
        const int windowWidth = std::min(scaled(kDesignWindowWidth), workWidth);
        const int windowHeight = std::min(scaled(kDesignWindowHeight), workHeight);
        const int centerX = referenceRect.left + (referenceRect.right - referenceRect.left) / 2;
        const int centerY = referenceRect.top + (referenceRect.bottom - referenceRect.top) / 2;
        const int workLeft = static_cast<int>(info.rcWork.left);
        const int workTop = static_cast<int>(info.rcWork.top);
        const int workRight = static_cast<int>(info.rcWork.right);
        const int workBottom = static_cast<int>(info.rcWork.bottom);
        const int windowX =
            std::clamp(centerX - windowWidth / 2, workLeft, std::max(workLeft, workRight - windowWidth));
        const int windowY =
            std::clamp(centerY - windowHeight / 2, workTop, std::max(workTop, workBottom - windowHeight));
        SetWindowPos(window_, nullptr, windowX, windowY, windowWidth, windowHeight,
                     SWP_NOZORDER | SWP_NOACTIVATE);
        layoutControls();
    }

    HFONT createAppFont(int designHeight, int minimumHeight, int weight) const {
        const int height = std::max(minimumHeight, scaled(designHeight));
        return CreateFontW(-height, 0, 0, 0, weight, FALSE, FALSE, FALSE, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS,
                           CLIP_DEFAULT_PRECIS, CLEARTYPE_QUALITY, DEFAULT_PITCH, L"Microsoft YaHei UI");
    }

    void applyFontsToControls() {
        for (HWND control : {title_,
                             subtitle_,
                             languageCombo_,
                             themeButton_,
                             typeLabel_,
                             typeCombo_,
                             angleUnitButton_,
                             hint_,
                             inputLabel_,
                             inputFrame_,
                             input_,
                             syntaxTitle_,
                             syntax_,
                             exampleButton_,
                             clearButton_,
                             solveButton_,
                             resultTitle_,
                             graphButton_,
                             historyButton_,
                             resultSummary_,
                             approximation_,
                             methodLabel_,
                             methodButtons_[0],
                             methodButtons_[1],
                             methodButtons_[2],
                             process_}) {
            if (control)
                SendMessageW(control, WM_SETFONT, reinterpret_cast<WPARAM>(normalFont_), TRUE);
        }
        if (title_)
            SendMessageW(title_, WM_SETFONT, reinterpret_cast<WPARAM>(titleFont_), TRUE);
        for (HWND control : {subtitle_, languageCombo_, typeLabel_, typeCombo_, angleUnitButton_, hint_,
                             syntax_, graphButton_, historyButton_, approximation_, methodLabel_}) {
            if (control)
                SendMessageW(control, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        }
        if (logEdit_)
            SendMessageW(logEdit_, WM_SETFONT, reinterpret_cast<WPARAM>(normalFont_), TRUE);
        if (logClearButton_)
            SendMessageW(logClearButton_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        for (HWND control : {logImportButton_, logExportButton_})
            if (control)
                SendMessageW(control, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        if (graphCanvas_)
            SendMessageW(graphCanvas_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        if (dropdownList_)
            SendMessageW(dropdownList_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
    }

    void recreateFonts() {
        HFONT previousNormal = normalFont_;
        HFONT previousTitle = titleFont_;
        HFONT previousSmall = smallFont_;
        normalFont_ = createAppFont(17, 12, FW_NORMAL);
        titleFont_ = createAppFont(27, 19, FW_BOLD);
        smallFont_ = createAppFont(15, 11, FW_NORMAL);
        applyFontsToControls();
        if (previousNormal)
            DeleteObject(previousNormal);
        if (previousTitle)
            DeleteObject(previousTitle);
        if (previousSmall)
            DeleteObject(previousSmall);
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
        return multiplicationForDisplay(
            equation::applyVariableNames(localizeOutput(simplified), lastVariableContext_));
    }

    void createControls() {
        recreateFonts();

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
        SendMessageW(input_, EM_SETLIMITTEXT, 4096, 0);
        SetWindowSubclass(input_, equationInputSubclassProc, kEquationInputSubclassId, 0);
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
        initializeHistory();
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
        if (lastVariableContext_.isRelation)
            return L"关系式求解";
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
        if (lineCount == 2) {
            const bool quadratic = classified.find(L"^2") != std::wstring::npos ||
                                   classified.find(L'²') != std::wstring::npos ||
                                   classified.find(L"xy") != std::wstring::npos ||
                                   classified.find(L"yx") != std::wstring::npos ||
                                   classified.find(L"x*y") != std::wstring::npos ||
                                   classified.find(L"y*x") != std::wstring::npos;
            return quadratic ? L"二元多项式方程组" : L"二元一次方程组";
        }
        if (classified.find_first_of(L"<>≤≥") != std::wstring::npos)
            return L"一元一次不等式";
        if (classified.find(L'|') != std::wstring::npos)
            return L"绝对值方程";
        if (classified.find(L"/x") != std::wstring::npos || classified.find(L"/(x") != std::wstring::npos ||
            classified.find(L"／x") != std::wstring::npos)
            return L"分式方程";
        if (classified.find(L"^4") != std::wstring::npos || classified.find(L'⁴') != std::wstring::npos)
            return L"一元四次方程";
        if (classified.find(L"^3") != std::wstring::npos || classified.find(L'³') != std::wstring::npos)
            return L"一元三次方程";
        if (classified.find(L"^2") != std::wstring::npos || classified.find(L'²') != std::wstring::npos) {
            return L"一元二次方程";
        }
        if (classified.find(L'^') != std::wstring::npos)
            return L"指数方程";
        return L"一元一次方程";
    }

    void initializeHistory() {
        try {
            historyStore_.emplace(history::defaultDirectory());
            const auto startup = historyStore_->initialize(history::deviceFingerprint());
            history_ = historyStore_->load();
            historyReady_ = true;
            historyWasReset_ = startup != history::Startup::ExistingDevice;
        } catch (...) {
            historyReady_ = false;
        }
    }

    bool persistHistory(const std::vector<HistoryEntry>& entries) {
        if (!historyReady_ || !historyStore_)
            return false;
        try {
            historyStore_->save(entries);
            return true;
        } catch (...) {
            historyReady_ = false;
            updateHistoryWindow();
            MessageBoxW(
                logWindow_ ? logWindow_ : window_,
                localize(L"日志未能保存。请检查数据目录权限或设备标记；本次会话的记录可通过导出另存。",
                         L"History could not be saved. Check the data folder permissions or device marker. "
                         L"You can export this session's records.")
                    .c_str(),
                localize(L"日志保存失败", L"History save failed").c_str(), MB_OK | MB_ICONWARNING);
            return false;
        }
    }

    void addHistoryEntry(const std::wstring& equation) {
        history_ = history::merge(history_, {{currentTimestamp(), historyTypeForInput(equation), equation}});
        persistHistory(history_);
        updateHistoryWindow();
    }

    std::optional<std::filesystem::path> chooseHistoryFile(bool exporting) {
        std::wstring filename(32768, L'\0');
        if (exporting) {
            std::wstring timestamp = currentTimestamp();
            std::replace(timestamp.begin(), timestamp.end(), L':', L'-');
            std::replace(timestamp.begin(), timestamp.end(), L' ', L'_');
            const std::wstring initial = L"EquationSolver-history-" + timestamp + L".tsv";
            std::copy(initial.begin(), initial.end(), filename.begin());
        }
        std::wstring filter = localize(L"方程求解器日志 (*.tsv)", L"Equation Solver history (*.tsv)");
        filter.push_back(L'\0');
        filter += L"*.tsv";
        filter.push_back(L'\0');
        filter += localize(L"所有文件 (*.*)", L"All files (*.*)");
        filter.push_back(L'\0');
        filter += L"*.*";
        filter.append(2, L'\0');
        const std::wstring title = exporting ? localize(L"导出求解日志", L"Export solution history")
                                             : localize(L"导入求解日志", L"Import solution history");
        OPENFILENAMEW dialog{};
        dialog.lStructSize = sizeof(dialog);
        dialog.hwndOwner = logWindow_;
        dialog.lpstrFilter = filter.c_str();
        dialog.nFilterIndex = 1;
        dialog.lpstrFile = filename.data();
        dialog.nMaxFile = static_cast<DWORD>(filename.size());
        dialog.lpstrTitle = title.c_str();
        dialog.lpstrDefExt = L"tsv";
        dialog.Flags = OFN_EXPLORER | OFN_NOCHANGEDIR | OFN_PATHMUSTEXIST |
                       (exporting ? OFN_OVERWRITEPROMPT : OFN_FILEMUSTEXIST | OFN_HIDEREADONLY);
        const BOOL selected = exporting ? GetSaveFileNameW(&dialog) : GetOpenFileNameW(&dialog);
        if (selected)
            return std::filesystem::path(filename.c_str());
        if (CommDlgExtendedError())
            MessageBoxW(logWindow_,
                        localize(L"无法打开文件选择窗口，请重试。",
                                 L"Could not open the file dialog. Please try again.")
                            .c_str(),
                        title.c_str(), MB_OK | MB_ICONERROR);
        return std::nullopt;
    }

    void transferHistory(bool exporting) {
        const auto path = chooseHistoryFile(exporting);
        if (!path)
            return;
        try {
            if (exporting) {
                if (historyStore_)
                    historyStore_->exportTo(*path, history_);
                else
                    history::writeBackup(*path, history_);
                MessageBoxW(
                    logWindow_,
                    localize(L"日志已导出，包含方程、题型和原始求解时间。",
                             L"History exported, including equations, types, and original solve times.")
                        .c_str(),
                    localize(L"导出完成", L"Export complete").c_str(), MB_OK | MB_ICONINFORMATION);
            } else {
                // 完整校验后再合并；先保存成功，再更新界面，防止半导入。
                const auto incoming = history::readBackup(*path);
                const auto merged = history::merge(history_, incoming);
                if (!persistHistory(merged))
                    return;
                history_ = merged;
                historyWasReset_ = false;
                updateHistoryWindow();
                const std::wstring message =
                    localize(L"导入完成，已合并并去除重复记录，保留时间最新的 100 条。当前记录数：",
                             L"Import complete. Records were merged and duplicates removed; the latest 100 "
                             L"are kept. Current count: ") +
                    std::to_wstring(history_.size());
                MessageBoxW(logWindow_, message.c_str(), localize(L"导入完成", L"Import complete").c_str(),
                            MB_OK | MB_ICONINFORMATION);
            }
        } catch (...) {
            MessageBoxW(
                logWindow_,
                (exporting ? localize(L"导出失败。请选择程序数据目录以外的位置，并检查文件是否可写。",
                                      L"Export failed. Choose a location outside the app data folder and "
                                      L"check that the file is writable.")
                           : localize(L"导入失败：文件无法读取、格式不正确或超过大小限制（8 MB / 10000 "
                                      L"条）。现有日志未修改。",
                                      L"Import failed: the file is unreadable, invalid, or exceeds the limit "
                                      L"(8 MB / 10,000 entries). Existing history was not changed."))
                    .c_str(),
                localize(L"日志导入和导出", L"History import / export").c_str(), MB_OK | MB_ICONERROR);
        }
    }

    std::wstring displayedHistory() {
        historyLineTargets_.clear();
        historyLineGroups_.clear();
        std::vector<std::wstring> lines;
        const auto appendLine = [&](const std::wstring& line, const std::wstring& equation) {
            lines.push_back(line);
            historyLineTargets_.push_back(equation);
        };
        if (!historyReady_) {
            appendLine(
                localize(L"本机识别或日志读取失败，已暂停日志读写。请检查设备标记和数据目录；当前会话记录仍可"
                         L"导出。",
                         L"Device verification or history loading failed. History storage is paused. Check "
                         L"the device marker and data folder; this session can still be exported."),
                L"");
            appendLine(L"", L"");
        } else if (historyWasReset_) {
            appendLine(localize(L"未找到匹配的本机运行标记，旧使用记录已清空。可通过“导入日志”恢复备份。",
                                L"No matching local run marker was found. Previous usage records were "
                                L"cleared. Use Import history to restore a backup."),
                       L"");
            appendLine(L"", L"");
        }
        if (history_.empty()) {
            appendLine(localize(L"暂无求解日志。", L"No solution history yet."), L"");
        }
        for (auto it = history_.rbegin(); it != history_.rend(); ++it) {
            const int firstLine = static_cast<int>(lines.size());
            appendLine(L"[" + it->time + L"]  " + localizeOutput(it->type), it->equation);
            std::wstring equationLine;
            for (wchar_t ch : it->equation) {
                if (ch == L'\r')
                    continue;
                if (ch == L'\n') {
                    appendLine(equationLine, it->equation);
                    equationLine.clear();
                } else {
                    equationLine += ch;
                }
            }
            appendLine(equationLine, it->equation);
            historyLineGroups_.push_back({firstLine, static_cast<int>(lines.size()) - 1});
            appendLine(L"", L"");
        }
        std::wstring text;
        for (size_t index = 0; index < lines.size(); ++index) {
            if (index)
                text += L"\r\n";
            text += lines[index];
        }
        return text;
    }

    void updateHistoryWindow() {
        if (!logEdit_)
            return;
        const std::wstring text = displayedHistory();
        SetWindowTextW(logEdit_, text.c_str());
        mathview::setClickableLineGroups(logEdit_, historyLineGroups_);
        EnableWindow(logClearButton_, historyReady_);
        EnableWindow(logImportButton_, historyReady_);
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
        SetWindowTheme(logImportButton_, explorer, nullptr);
        SetWindowTheme(logExportButton_, explorer, nullptr);
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
        const int logWidth = scaled(680);
        const int logHeight = scaled(560);
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
        logEdit_ = CreateWindowExW(
            0, mathview::kClassName, L"",
            WS_CHILD | WS_VISIBLE | WS_TABSTOP | mathview::kVerticalScroll | mathview::kClickableLines,
            scaled(16), scaled(16), scaled(632), scaled(440), logWindow_,
            reinterpret_cast<HMENU>(static_cast<INT_PTR>(kLogEditId)), instance_, nullptr);
        logClearButton_ = CreateWindowExW(0, L"BUTTON", localize(L"清空日志", L"Clear History").c_str(),
                                          WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_OWNERDRAW, scaled(16),
                                          scaled(470), scaled(130), scaled(36), logWindow_,
                                          reinterpret_cast<HMENU>(static_cast<INT_PTR>(kLogClearButtonId)),
                                          instance_, nullptr);
        logImportButton_ =
            CreateWindowExW(0, L"BUTTON", localize(L"导入日志", L"Import history").c_str(),
                            WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_OWNERDRAW, 0, 0, 0, 0, logWindow_,
                            reinterpret_cast<HMENU>(static_cast<INT_PTR>(kLogImportId)), instance_, nullptr);
        logExportButton_ =
            CreateWindowExW(0, L"BUTTON", localize(L"导出日志", L"Export history").c_str(),
                            WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_OWNERDRAW, 0, 0, 0, 0, logWindow_,
                            reinterpret_cast<HMENU>(static_cast<INT_PTR>(kLogExportId)), instance_, nullptr);
        SendMessageW(logEdit_, WM_SETFONT, reinterpret_cast<WPARAM>(normalFont_), TRUE);
        SendMessageW(logClearButton_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        SendMessageW(logImportButton_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        SendMessageW(logExportButton_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        SendMessageW(logWindow_, WM_SIZE, 0, 0);
        updateHistoryWindow();
        applyLogTheme();
        ShowWindow(logWindow_, SW_SHOW);
        UpdateWindow(logWindow_);
    }

    std::vector<std::pair<double, double>> graphMarkers() const {
        std::vector<std::pair<double, double>> markers;
        if (lastGraphSystem2_) {
            std::vector<std::wstring> suffixes{L""};
            for (size_t index = 1; index <= 16; ++index)
                suffixes.push_back(solutionSubscript(index));
            for (size_t index = 1; index <= 16; ++index)
                suffixes.push_back(std::to_wstring(index));
            for (const std::wstring& suffix : suffixes) {
                std::optional<double> x;
                std::optional<double> y;
                const std::wstring xLabel = L"x" + suffix;
                const std::wstring yLabel = L"y" + suffix;
                for (const auto& [label, value] : lastGraphValues_) {
                    if (label == xLabel)
                        x = value;
                    else if (label == yLabel)
                        y = value;
                }
                if (!x || !y)
                    continue;
                const bool duplicate = std::any_of(markers.begin(), markers.end(), [&](const auto& marker) {
                    return std::abs(marker.first - *x) < 1e-8 && std::abs(marker.second - *y) < 1e-8;
                });
                if (!duplicate)
                    markers.push_back({*x, *y});
            }
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
        const int graphWidth = scaled(860);
        const int graphHeight = scaled(640);
        RECT owner{};
        GetWindowRect(window_, &owner);
        const int x = owner.left + ((owner.right - owner.left) - graphWidth) / 2;
        const int y = owner.top + ((owner.bottom - owner.top) - graphHeight) / 2;
        graphWindow_ = CreateWindowExW(0, kGraphWindowClass, localize(L"方程图像", L"Equation Graph").c_str(),
                                       WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN, x, y, graphWidth, graphHeight,
                                       window_, nullptr, instance_, this);
        if (!graphWindow_)
            return;
        graphCanvas_ = CreateWindowExW(0, graphview::kClassName, L"", WS_CHILD | WS_VISIBLE | WS_TABSTOP,
                                       scaled(14), scaled(14), graphWidth - scaled(56),
                                       graphHeight - scaled(78), graphWindow_, nullptr, instance_, nullptr);
        SendMessageW(graphCanvas_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        RECT client{};
        GetClientRect(graphWindow_, &client);
        MoveWindow(graphCanvas_, scaled(14), scaled(14), client.right - scaled(28),
                   client.bottom - scaled(28), TRUE);
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

    void closeDropdown() {
        if (dropdownWindow_)
            DestroyWindow(dropdownWindow_);
    }

    void showDropdown(HWND anchor, DropdownKind kind, const std::vector<std::wstring>& items, int selected) {
        closeDropdown();
        if (!anchor || items.empty())
            return;
        RECT anchorRect{};
        GetWindowRect(anchor, &anchorRect);
        const int anchorWidth = anchorRect.right - anchorRect.left;
        const int width = std::max(anchorWidth, kind == DropdownKind::Type ? scaled(210) : anchorWidth);
        const int rowHeight = scaled(36);
        const int visibleRows = std::min(8, static_cast<int>(items.size()));
        const int height = visibleRows * rowHeight + scaled(4);
        MONITORINFO monitor{};
        monitor.cbSize = sizeof(monitor);
        GetMonitorInfoW(MonitorFromRect(&anchorRect, MONITOR_DEFAULTTONEAREST), &monitor);
        int x = std::clamp(anchorRect.left, monitor.rcWork.left,
                           std::max(monitor.rcWork.left, monitor.rcWork.right - width));
        int y = anchorRect.bottom + scaled(2);
        if (y + height > monitor.rcWork.bottom)
            y = anchorRect.top - height - scaled(2);

        dropdownKind_ = kind;
        dropdownItems_ = items;
        dropdownSelectedIndex_ = std::clamp(selected, 0, static_cast<int>(items.size()) - 1);
        dropdownHoverIndex_ = dropdownSelectedIndex_;
        dropdownAnchor_ = anchor;
        // 弹出框归属按钮所在窗口，复用题型选择框的绘制和交互。
        const HWND owner = GetAncestor(anchor, GA_ROOT);
        dropdownWindow_ =
            CreateWindowExW(WS_EX_TOOLWINDOW | WS_EX_TOPMOST, kDropdownWindowClass, L"",
                            WS_POPUP | WS_CLIPCHILDREN, x, y, width, height, owner, nullptr, instance_, this);
        if (!dropdownWindow_) {
            dropdownKind_ = DropdownKind::None;
            dropdownAnchor_ = nullptr;
            return;
        }
        dropdownVisibleRows_ = visibleRows;
        const int scrollbarWidth =
            static_cast<int>(items.size()) > visibleRows ? scaled(kDropdownScrollbarWidth) : 0;
        dropdownList_ = CreateWindowExW(
            0, L"LISTBOX", L"",
            WS_CHILD | WS_VISIBLE | LBS_OWNERDRAWFIXED | LBS_HASSTRINGS | LBS_NOINTEGRALHEIGHT, scaled(2),
            scaled(2), width - scaled(4) - scrollbarWidth, height - scaled(4), dropdownWindow_,
            reinterpret_cast<HMENU>(static_cast<INT_PTR>(kDropdownListId)), instance_, nullptr);
        SendMessageW(dropdownList_, WM_SETFONT, reinterpret_cast<WPARAM>(smallFont_), TRUE);
        SendMessageW(dropdownList_, LB_SETITEMHEIGHT, 0, rowHeight);
        for (const auto& item : items)
            SendMessageW(dropdownList_, LB_ADDSTRING, 0, reinterpret_cast<LPARAM>(item.c_str()));
        SendMessageW(dropdownList_, LB_SETCURSEL, dropdownSelectedIndex_, 0);
        SendMessageW(dropdownList_, LB_SETTOPINDEX, std::max(0, dropdownSelectedIndex_ - visibleRows / 2), 0);
        SetWindowSubclass(dropdownList_, DropdownListProc, 1, reinterpret_cast<DWORD_PTR>(this));
        HRGN region = CreateRoundRectRgn(0, 0, width + 1, height + 1, scaled(10), scaled(10));
        SetWindowRgn(dropdownWindow_, region, TRUE);
        ShowWindow(dropdownWindow_, SW_SHOWNORMAL);
        SetFocus(dropdownList_);
        InvalidateRect(anchor, nullptr, TRUE);
    }

    int dropdownMaxTopIndex() const {
        return std::max(0, static_cast<int>(dropdownItems_.size()) - dropdownVisibleRows_);
    }

    void scrollDropdownTo(int topIndex) {
        if (!dropdownList_)
            return;
        const int clamped = std::clamp(topIndex, 0, dropdownMaxTopIndex());
        SendMessageW(dropdownList_, LB_SETTOPINDEX, clamped, 0);
        InvalidateRect(dropdownList_, nullptr, FALSE);
        if (dropdownWindow_)
            InvalidateRect(dropdownWindow_, nullptr, FALSE);
    }

    void scrollDropdownBy(int lines) {
        if (!dropdownList_)
            return;
        const int topIndex = static_cast<int>(SendMessageW(dropdownList_, LB_GETTOPINDEX, 0, 0));
        scrollDropdownTo(topIndex + lines);
    }

    RECT dropdownScrollbarRect(HWND hwnd) const {
        RECT client{};
        GetClientRect(hwnd, &client);
        return {client.right - scaled(kDropdownScrollbarWidth) - scaled(2), scaled(2),
                client.right - scaled(2), client.bottom - scaled(2)};
    }

    RECT dropdownThumbRect(HWND hwnd) const {
        const RECT bar = dropdownScrollbarRect(hwnd);
        const int arrowHeight = scaled(kDropdownArrowHeight);
        const int trackHeight = std::max(1, static_cast<int>(bar.bottom - bar.top) - 2 * arrowHeight);
        const int maximum = dropdownMaxTopIndex();
        const int thumbHeight = maximum == 0
                                    ? trackHeight
                                    : std::max(scaled(kDropdownMinimumThumbHeight),
                                               trackHeight * dropdownVisibleRows_ /
                                                   std::max(1, static_cast<int>(dropdownItems_.size())));
        const int travel = std::max(0, trackHeight - thumbHeight);
        const int topIndex =
            dropdownList_ ? static_cast<int>(SendMessageW(dropdownList_, LB_GETTOPINDEX, 0, 0)) : 0;
        const int thumbTop = bar.top + arrowHeight + (maximum == 0 ? 0 : travel * topIndex / maximum);
        return {bar.right - scaled(12), thumbTop, bar.right - scaled(4), thumbTop + thumbHeight};
    }

    void drawDropdownScrollbar(HWND hwnd, HDC dc) const {
        if (dropdownMaxTopIndex() == 0)
            return;
        const RECT bar = dropdownScrollbarRect(hwnd);
        HBRUSH trackBrush = CreateSolidBrush(darkMode_ ? RGB(27, 30, 36) : RGB(250, 250, 250));
        FillRect(dc, &bar, trackBrush);
        DeleteObject(trackBrush);

        const COLORREF accent = darkMode_ ? RGB(155, 160, 168) : RGB(105, 108, 112);
        HPEN pen = CreatePen(PS_SOLID, 1, accent);
        HBRUSH brush = CreateSolidBrush(accent);
        HGDIOBJ oldPen = SelectObject(dc, pen);
        HGDIOBJ oldBrush = SelectObject(dc, brush);
        const int centerX = bar.left + (bar.right - bar.left) / 2;
        POINT up[]{{centerX, bar.top + scaled(4)},
                   {centerX - scaled(4), bar.top + scaled(10)},
                   {centerX + scaled(4), bar.top + scaled(10)}};
        POINT down[]{{centerX - scaled(4), bar.bottom - scaled(10)},
                     {centerX + scaled(4), bar.bottom - scaled(10)},
                     {centerX, bar.bottom - scaled(4)}};
        Polygon(dc, up, 3);
        Polygon(dc, down, 3);
        const RECT thumb = dropdownThumbRect(hwnd);
        RoundRect(dc, thumb.left, thumb.top, thumb.right, thumb.bottom, scaled(8), scaled(8));
        SelectObject(dc, oldBrush);
        SelectObject(dc, oldPen);
        DeleteObject(brush);
        DeleteObject(pen);
    }

    bool dropdownItemEnabled(int index) const {
        return index >= 0 && index < static_cast<int>(dropdownItems_.size());
    }

    void commitDropdownSelection(int index) {
        if (!dropdownItemEnabled(index))
            return;
        const DropdownKind kind = dropdownKind_;
        closeDropdown();
        if (kind == DropdownKind::Type) {
            chooseType(index);
            return;
        }
        if (kind == DropdownKind::Language) {
            if (index < 0 || index >= 3)
                return;
            language_ = static_cast<Language>(index);
            updateLanguage();
            layoutControls();
            return;
        }
        if (kind != DropdownKind::AngleUnit || index < 0 || index >= 2)
            return;
        const equation::AngleUnit selectedUnit =
            index == 0 ? equation::AngleUnit::Radians : equation::AngleUnit::Degrees;
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

    void showTypeMenu() {
        std::vector<std::wstring> names;
        names.reserve(kTypes.size());
        for (const auto& type : kTypes) {
            names.push_back(language_ == Language::English       ? type.english
                            : language_ == Language::Traditional ? toTraditional(type.simplified)
                                                                 : type.simplified);
        }
        showDropdown(typeCombo_, DropdownKind::Type, names, currentType_);
    }

    void showLanguageMenu() {
        showDropdown(languageCombo_, DropdownKind::Language, {L"简体中文", L"English", L"繁體中文"},
                     static_cast<int>(language_));
    }

    void showAngleUnitMenu() {
        showDropdown(angleUnitButton_, DropdownKind::AngleUnit,
                     {localize(L"弧度制", L"Radians"), localize(L"角度制", L"Degrees")},
                     angleUnit_ == equation::AngleUnit::Radians ? 0 : 1);
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
            L"}；同种括号不能嵌套；输入左括号会自动补全右括号\r\n• 可省略乘号：2a、3(a+1)\r\n• "
            L"平方与平方根：a^2；\"100\"\r\n• "
            L"n 次根：\"n\"被开方数\"；其他指数幂：a^-2、a^(1/2)、a^(-2/3)\r\n• "
            L"绝对值：|a|；根号内也可用，如 "
            L"\"3\"|-2|\"\r\n• "
            L"高次方程：a^3、a^4；二元多项式方程组支持总次数不超过 4\r\n• 方程组每行一个方程",
            L"• Any English letter except e can be a variable: a, t, u, etc.\r\n• Constants: pi or π is the "
            L"circle constant; e is Euler's number\r\n• Trigonometric functions: switch between radians and "
            L"degrees beside the type selector\r\n• Factorial: 5! or 5！\r\n• Bracket order: { [ ( ) ] }; "
            L"brackets of the same kind cannot be nested; typing a left bracket automatically inserts its "
            L"right bracket\r\n• Multiplication may be omitted: 2a, 3(a+1)\r\n• Square and square root: a^2; "
            L"\"100\"\r\n• "
            L"nth root: \"n\"radicand\"; other exponents: a^-2, a^(1/2), a^(-2/3)\r\n• Absolute value: |a|; "
            L"also in "
            L"roots, e.g. \"3\"|-2|\"\r\n• Higher-degree equations: a^3, a^4; 2-variable polynomial systems "
            L"support total degree at most 4\r\n• Enter one equation per line for a system");
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
            SetWindowTextW(logImportButton_, localize(L"导入日志", L"Import history").c_str());
            SetWindowTextW(logExportButton_, localize(L"导出日志", L"Export history").c_str());
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
        // The custom popup is not a native combo box, so changing currentType_ does not update
        // the button caption automatically. Refresh it as part of the same selection commit.
        rebuildTypeCombo();
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
        const std::wstring displayedProcess = localizedSolverOutput(methodSectionsZh_[selectedMethod_]);
        SetWindowTextW(process_, displayedProcess.c_str());
        ShowWindow(process_, lastVariableContext_.isRelation ? SW_HIDE : SW_SHOW);
        const bool multiple = methodSectionsZh_.size() > 1;
        ShowWindow(methodLabel_, multiple ? SW_SHOW : SW_HIDE);
        for (int i = 0; i < 3; ++i) {
            ShowWindow(methodButtons_[i],
                       multiple && i < static_cast<int>(methodSectionsZh_.size()) ? SW_SHOW : SW_HIDE);
            InvalidateRect(methodButtons_[i], nullptr, TRUE);
        }
    }

    bool answerIsTooLong(const std::wstring& summary) {
        SetWindowTextW(resultSummary_, localizedSolverOutput(summary).c_str());
        RECT client{};
        GetClientRect(window_, &client);
        const int margin = scaled(28);
        const int leftWidth = std::clamp(static_cast<int>(client.right) * 38 / 100, scaled(350), scaled(460));
        const int rightX = margin + leftWidth + scaled(24);
        const int rightWidth = std::max(scaled(320), static_cast<int>(client.right) - rightX - margin);
        const int naturalWidth = mathview::preferredWidth(resultSummary_);
        // The result card is intentionally limited to two readable lines. Longer exact formulae
        // become difficult to follow and can be clipped, so the compact numeric form is clearer.
        return summary.size() > 220 || naturalWidth > rightWidth * 2;
    }

    static std::wstring longAnswerApproximation(std::wstring approximation) {
        const std::wstring prefix = L"数值：";
        if (approximation.rfind(prefix, 0) == 0)
            approximation.erase(0, prefix.size());
        return L"答案过长，仅显示近似值：" + approximation;
    }

    void solve() {
        // 成功时集中刷新结果、解法、日志和图像；输入异常只更新错误区域。
        const std::wstring text = controlText(input_);
        try {
            const auto kind = static_cast<equation::InputKind>(currentType_);
            const equation::SolveResult result = equation::solveText(text, kind, angleUnit_);
            lastSummaryZh_ = result.summary;
            lastApproximationZh_ =
                result.isRelation ? L"" : approximationForSummary(result.summary, angleUnit_);
            lastVariableContext_ = result;
            if (!result.isRelation && answerIsTooLong(result.summary)) {
                if (!lastApproximationZh_.empty()) {
                    lastSummaryZh_ = longAnswerApproximation(lastApproximationZh_);
                    lastApproximationZh_.clear();
                } else if (result.summary.find(L'≈') != std::wstring::npos) {
                    lastSummaryZh_ = L"答案过长，仅显示近似值：\r\n" + result.summary;
                }
            }
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
            lastSolvedInput_ = result.normalizedInput.empty() ? text : result.normalizedInput;
            lastGraphSystem2_ = equationLines == 2;
            lastGraphValues_ = result.isRelation ? std::vector<std::pair<std::wstring, double>>{}
                                                 : realValuesForGraph(result.summary, angleUnit_);
            graphEnabled_ = !result.isRelation && !result.usesImaginaryUnit && !inequality &&
                            equationLines <= 2 && !noRealSolution && !lastGraphValues_.empty() &&
                            (!lastGraphSystem2_ || !graphMarkers().empty());
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
        closeDropdown();
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
        const int margin = scaled(28);
        SetWindowTextW(resultSummary_, localizedSolverOutput(lastSummaryZh_).c_str());
        const int naturalSummaryWidth = mathview::preferredWidth(resultSummary_);
        const int regularLeftWidth = std::clamp(width * 38 / 100, scaled(350), scaled(460));
        // Keep the two-column divider stable while the answer changes.  The result card below still
        // grows to the available right-column width and wraps long exact answers, but it must not
        // move the input controls or leave stale scrollbar/button fragments at their former positions.
        const int leftWidth = regularLeftWidth;
        const int rightX = margin + leftWidth + scaled(24);
        const int rightWidth = std::max(scaled(320), width - rightX - margin);

        MoveWindow(title_, margin, scaled(20), scaled(520), scaled(38), TRUE);
        MoveWindow(subtitle_, margin, scaled(58), std::max(scaled(280), width - 2 * margin - scaled(245)),
                   scaled(23), TRUE);
        MoveWindow(languageCombo_, width - margin - scaled(210), scaled(24), scaled(138), scaled(34), TRUE);
        MoveWindow(themeButton_, width - margin - scaled(62), scaled(26), scaled(62), scaled(30), TRUE);
        MoveWindow(typeLabel_, margin, scaled(101), scaled(70), scaled(25), TRUE);
        MoveWindow(typeCombo_, margin + scaled(72), scaled(96),
                   std::max(scaled(130), leftWidth - scaled(218)), scaled(34), TRUE);
        MoveWindow(angleUnitButton_, margin + leftWidth - scaled(134), scaled(96), scaled(134), scaled(34),
                   TRUE);
        MoveWindow(hint_, margin, scaled(136), leftWidth, scaled(24), TRUE);
        MoveWindow(inputLabel_, margin, scaled(171), leftWidth, scaled(24), TRUE);
        MoveWindow(inputFrame_, margin, scaled(198), leftWidth, scaled(156), TRUE);
        MoveWindow(input_, margin + scaled(8), scaled(205), leftWidth - scaled(16), scaled(142), TRUE);
        MoveWindow(syntaxTitle_, margin, scaled(376), leftWidth, scaled(24), TRUE);
        const int buttonY = height - scaled(62);
        MoveWindow(syntax_, margin, scaled(403), leftWidth, std::max(scaled(122), buttonY - scaled(415)),
                   TRUE);
        MoveWindow(exampleButton_, margin, buttonY, scaled(116), scaled(36), TRUE);
        MoveWindow(clearButton_, margin + scaled(128), buttonY, scaled(90), scaled(36), TRUE);
        MoveWindow(solveButton_, margin + scaled(232), buttonY,
                   std::max(scaled(118), leftWidth - scaled(232)), scaled(36), TRUE);

        MoveWindow(resultTitle_, rightX, scaled(101), std::max(scaled(90), rightWidth - scaled(240)),
                   scaled(27), TRUE);
        MoveWindow(graphButton_, rightX + rightWidth - scaled(226), scaled(94), scaled(100), scaled(34),
                   TRUE);
        MoveWindow(historyButton_, rightX + rightWidth - scaled(118), scaled(94), scaled(118), scaled(34),
                   TRUE);
        std::wstring displayedSummary = localizedSolverOutput(lastSummaryZh_);
        bool twoLineSummary = naturalSummaryWidth > rightWidth;
        if (twoLineSummary) {
            displayedSummary = splitSummaryBeforeSecondRoot(displayedSummary);
            SetWindowTextW(resultSummary_, displayedSummary.c_str());
        }
        const bool expandedExactSummary = displayedSummary.find(L'∛') != std::wstring::npos ||
                                          displayedSummary.find(L"acos(") != std::wstring::npos;
        const wchar_t displayedVariable =
            lastVariableContext_.variableCount > 0 ? lastVariableContext_.variables[0] : L'x';
        const bool multiPointSystem =
            lastGraphSystem2_ &&
            displayedSummary.find(std::wstring(1, displayedVariable) + L"₃") != std::wstring::npos;
        const int maxSummaryLines =
            lastVariableContext_.isRelation ? 64 : (expandedExactSummary || multiPointSystem ? 6 : 2);
        mathview::setMaxVisualLines(resultSummary_, maxSummaryLines);
        const int formattedWidth = mathview::preferredWidth(resultSummary_);
        const int summaryWidth = twoLineSummary || lastVariableContext_.isRelation
                                     ? rightWidth
                                     : std::clamp(formattedWidth, scaled(250), rightWidth);
        int summaryHeight =
            std::max(scaled(62), mathview::requiredHeight(resultSummary_, summaryWidth, maxSummaryLines));
        if (lastVariableContext_.isRelation && summaryHeight > height - scaled(161)) {
            // 含参数的超长答案没有单一数值近似；明确改用引用当前输入的精确解集。
            SetWindowTextW(resultSummary_,
                           localize(L"关系式结果过长。精确隐式解集：满足输入原式及其定义域的全部解。",
                                    L"The relation result is too long. Exact implicit solution set: all "
                                    L"values satisfying the input relations and their domains.")
                               .c_str());
            summaryHeight =
                std::max(scaled(62), mathview::requiredHeight(resultSummary_, summaryWidth, maxSummaryLines));
        }
        MoveWindow(resultSummary_, rightX, scaled(135), summaryWidth, summaryHeight, TRUE);
        const bool multiple = methodSectionsZh_.size() > 1;
        int processTop = scaled(135) + summaryHeight + scaled(8);
        if (!lastApproximationZh_.empty()) {
            MoveWindow(approximation_, rightX + scaled(4), processTop, rightWidth - scaled(8), scaled(24),
                       TRUE);
            processTop += scaled(28);
        }
        if (multiple) {
            MoveWindow(methodLabel_, rightX + scaled(4), processTop + scaled(5), scaled(62), scaled(36),
                       TRUE);
            for (int i = 0; i < 3; ++i)
                MoveWindow(methodButtons_[i], rightX + scaled(68 + i * 52), processTop, scaled(42),
                           scaled(42), TRUE);
            processTop += scaled(50);
        }
        MoveWindow(process_, rightX, processTop, rightWidth,
                   std::max(scaled(150), height - processTop - scaled(26)), TRUE);

        for (HWND control :
             {typeCombo_, angleUnitButton_, languageCombo_, inputFrame_, exampleButton_, clearButton_,
              solveButton_, graphButton_, historyButton_, resultSummary_, process_})
            roundControl(control, scaled(18));

        // MoveWindow redraws each child, but it does not reliably erase every pixel that belonged to
        // the child's old rounded region.  Redrawing the parent and all children prevents ghost
        // scrollbars and detached drop-down arrows after any layout update.
        RedrawWindow(window_, nullptr, nullptr, RDW_INVALIDATE | RDW_ERASE | RDW_ALLCHILDREN);
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
            const int diameter = rect.bottom - scaled(6);
            const int left = darkMode_ ? rect.right - diameter - scaled(3) : scaled(3);
            Gdiplus::SolidBrush knobBrush(Gdiplus::Color(255, 255, 255, 255));
            graphics.FillEllipse(&knobBrush, left, scaled(3), diameter, diameter);
            SetTextColor(dc, darkMode_ ? RGB(22, 134, 255) : RGB(105, 110, 118));
            RECT glyph{left, scaled(3), left + diameter, scaled(3) + diameter};
            DrawTextW(dc, darkMode_ ? L"☾" : L"☀", -1, &glyph, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
            return;
        }
        if (item.CtlID < kMethodButtonBaseId || item.CtlID >= kMethodButtonBaseId + 3) {
            const bool inputFrame = item.CtlID == kInputFrameId;
            const bool optionBox = item.CtlID == kTypeComboId || item.CtlID == kAngleUnitButtonId ||
                                   item.CtlID == kLanguageComboId;
            const bool optionExpanded = optionBox && dropdownWindow_ && dropdownAnchor_ == item.hwndItem;
            const bool disabled = !inputFrame && (item.itemState & ODS_DISABLED) != 0;
            const bool pressed = (item.itemState & ODS_SELECTED) != 0;
            const bool focused = (item.itemState & ODS_FOCUS) != 0;
            const bool primary = item.CtlID == kSolveButtonId;
            COLORREF fill = primary ? RGB(63, 104, 238) : (darkMode_ ? RGB(45, 49, 63) : RGB(255, 255, 255));
            COLORREF border =
                primary ? RGB(63, 104, 238) : (darkMode_ ? RGB(73, 79, 94) : RGB(205, 211, 220));
            COLORREF foreground =
                primary ? RGB(255, 255, 255) : (darkMode_ ? RGB(232, 236, 242) : RGB(30, 42, 56));
            if (optionBox)
                fill = darkMode_ ? RGB(39, 43, 55) : RGB(255, 255, 255);
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
            } else if (focused || optionExpanded) {
                border = RGB(63, 104, 238);
            }
            const Gdiplus::REAL left = scaledReal(1.0);
            const Gdiplus::REAL top = scaledReal(1.0);
            const Gdiplus::REAL right = static_cast<Gdiplus::REAL>(rect.right) - scaledReal(2.0);
            const Gdiplus::REAL bottom = static_cast<Gdiplus::REAL>(rect.bottom) - scaledReal(2.0);
            const Gdiplus::REAL diameter = optionBox ? scaledReal(8.0) : scaledReal(18.0);
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
                (focused || optionExpanded) ? 1.8f : 1.0f);
            graphics.DrawPath(&borderPen, &path);
            wchar_t label[128]{};
            GetWindowTextW(item.hwndItem, label, static_cast<int>(std::size(label)));
            SetTextColor(dc, foreground);
            if (optionBox) {
                RECT textRect{rect.left + scaled(12), rect.top, rect.right - scaled(34), rect.bottom};
                DrawTextW(dc, label, -1, &textRect, DT_LEFT | DT_VCENTER | DT_SINGLELINE | DT_END_ELLIPSIS);
                RECT arrowRect{rect.right - scaled(30), rect.top, rect.right - scaled(8), rect.bottom};
                const Gdiplus::REAL centerX =
                    static_cast<Gdiplus::REAL>(arrowRect.left + arrowRect.right) / 2.0f;
                const Gdiplus::REAL centerY =
                    static_cast<Gdiplus::REAL>(arrowRect.top + arrowRect.bottom) / 2.0f;
                const Gdiplus::REAL halfWidth = scaledReal(5.0);
                const Gdiplus::REAL halfHeight = scaledReal(3.5);
                Gdiplus::PointF arrow[3];
                if (optionExpanded) {
                    arrow[0] = {centerX, centerY - halfHeight};
                    arrow[1] = {centerX - halfWidth, centerY + halfHeight};
                    arrow[2] = {centerX + halfWidth, centerY + halfHeight};
                } else {
                    arrow[0] = {centerX - halfWidth, centerY - halfHeight};
                    arrow[1] = {centerX + halfWidth, centerY - halfHeight};
                    arrow[2] = {centerX, centerY + halfHeight};
                }
                Gdiplus::SolidBrush arrowBrush(
                    Gdiplus::Color(255, GetRValue(foreground), GetGValue(foreground), GetBValue(foreground)));
                graphics.FillPolygon(&arrowBrush, arrow, 3);
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
        graphics.FillEllipse(&circleBrush, scaledReal(1.5), scaledReal(1.5),
                             static_cast<Gdiplus::REAL>(rect.right) - scaledReal(3.0),
                             static_cast<Gdiplus::REAL>(rect.bottom) - scaledReal(3.0));
        SetTextColor(dc, foreground);
        const std::wstring number = std::to_wstring(index + 1);
        DrawTextW(dc, number.c_str(), -1, &rect, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
    }

    LRESULT handleDropdownMessage(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam) {
        switch (message) {
        case kDropdownCommitMessage:
            commitDropdownSelection(static_cast<int>(wParam));
            return 0;
        case WM_MEASUREITEM: {
            auto* measure = reinterpret_cast<MEASUREITEMSTRUCT*>(lParam);
            if (measure && measure->CtlID == kDropdownListId) {
                measure->itemHeight = static_cast<UINT>(scaled(36));
                return TRUE;
            }
            break;
        }
        case WM_DRAWITEM: {
            auto* item = reinterpret_cast<DRAWITEMSTRUCT*>(lParam);
            if (!item || item->CtlID != kDropdownListId || item->itemID == static_cast<UINT>(-1))
                return FALSE;
            const bool enabled = dropdownItemEnabled(static_cast<int>(item->itemID));
            const bool selected = enabled && (item->itemState & ODS_SELECTED) != 0;
            const COLORREF background = selected ? (darkMode_ ? RGB(59, 79, 116) : RGB(224, 235, 255))
                                                 : (darkMode_ ? RGB(57, 59, 70) : RGB(255, 255, 255));
            HBRUSH brush = CreateSolidBrush(background);
            FillRect(item->hDC, &item->rcItem, brush);
            DeleteObject(brush);
            wchar_t label[256]{};
            SendMessageW(item->hwndItem, LB_GETTEXT, item->itemID, reinterpret_cast<LPARAM>(label));
            SetBkMode(item->hDC, TRANSPARENT);
            SetTextColor(item->hDC, enabled ? (darkMode_ ? RGB(242, 244, 248) : RGB(27, 36, 49))
                                            : (darkMode_ ? RGB(145, 151, 162) : RGB(130, 137, 148)));
            RECT textRect = item->rcItem;
            textRect.left += scaled(14);
            textRect.right -= scaled(10);
            DrawTextW(item->hDC, label, -1, &textRect,
                      DT_LEFT | DT_VCENTER | DT_SINGLELINE | DT_END_ELLIPSIS);
            return TRUE;
        }
        case WM_CTLCOLORLISTBOX: {
            HDC dc = reinterpret_cast<HDC>(wParam);
            SetBkColor(dc, darkMode_ ? RGB(57, 59, 70) : RGB(255, 255, 255));
            SetTextColor(dc, darkMode_ ? RGB(242, 244, 248) : RGB(27, 36, 49));
            return reinterpret_cast<LRESULT>(panelBrush_);
        }
        case WM_ERASEBKGND: {
            RECT client{};
            GetClientRect(hwnd, &client);
            HBRUSH brush = CreateSolidBrush(darkMode_ ? RGB(57, 59, 70) : RGB(255, 255, 255));
            FillRect(reinterpret_cast<HDC>(wParam), &client, brush);
            DeleteObject(brush);
            return 1;
        }
        case WM_PAINT: {
            PAINTSTRUCT paint{};
            HDC dc = BeginPaint(hwnd, &paint);
            RECT client{};
            GetClientRect(hwnd, &client);
            HBRUSH background = CreateSolidBrush(darkMode_ ? RGB(57, 59, 70) : RGB(255, 255, 255));
            FillRect(dc, &client, background);
            DeleteObject(background);
            drawDropdownScrollbar(hwnd, dc);
            HPEN border = CreatePen(PS_SOLID, std::max(1, scaled(2)), RGB(39, 126, 255));
            HGDIOBJ oldPen = SelectObject(dc, border);
            HGDIOBJ oldBrush = SelectObject(dc, GetStockObject(NULL_BRUSH));
            RoundRect(dc, 0, 0, client.right, client.bottom, scaled(10), scaled(10));
            SelectObject(dc, oldBrush);
            SelectObject(dc, oldPen);
            DeleteObject(border);
            EndPaint(hwnd, &paint);
            return 0;
        }
        case WM_MOUSEWHEEL: {
            const int lines = -GET_WHEEL_DELTA_WPARAM(wParam) / WHEEL_DELTA * 3;
            scrollDropdownBy(lines);
            return 0;
        }
        case WM_LBUTTONDOWN: {
            if (dropdownMaxTopIndex() == 0)
                return 0;
            const RECT bar = dropdownScrollbarRect(hwnd);
            POINT point{GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam)};
            if (!PtInRect(&bar, point))
                return 0;
            const int topIndex = static_cast<int>(SendMessageW(dropdownList_, LB_GETTOPINDEX, 0, 0));
            const RECT thumb = dropdownThumbRect(hwnd);
            const int arrowHeight = scaled(kDropdownArrowHeight);
            if (point.y < bar.top + arrowHeight)
                scrollDropdownTo(topIndex - 1);
            else if (point.y >= bar.bottom - arrowHeight)
                scrollDropdownTo(topIndex + 1);
            else if (PtInRect(&thumb, point)) {
                dropdownScrollbarDragging_ = true;
                dropdownScrollbarDragOffset_ = point.y - thumb.top;
                SetCapture(hwnd);
            } else if (point.y < thumb.top)
                scrollDropdownTo(topIndex - dropdownVisibleRows_);
            else
                scrollDropdownTo(topIndex + dropdownVisibleRows_);
            return 0;
        }
        case WM_MOUSEMOVE:
            if (dropdownScrollbarDragging_) {
                const RECT bar = dropdownScrollbarRect(hwnd);
                const RECT thumb = dropdownThumbRect(hwnd);
                const int arrowHeight = scaled(kDropdownArrowHeight);
                const int trackHeight = std::max(1, static_cast<int>(bar.bottom - bar.top) - 2 * arrowHeight);
                const int travel = std::max(1, trackHeight - static_cast<int>(thumb.bottom - thumb.top));
                const int mousePosition = static_cast<int>(GET_Y_LPARAM(lParam));
                const int position = std::clamp(mousePosition - dropdownScrollbarDragOffset_ -
                                                    static_cast<int>(bar.top) - arrowHeight,
                                                0, travel);
                scrollDropdownTo(position * dropdownMaxTopIndex() / travel);
            }
            return 0;
        case WM_LBUTTONUP:
            if (dropdownScrollbarDragging_) {
                dropdownScrollbarDragging_ = false;
                ReleaseCapture();
            }
            return 0;
        case WM_ACTIVATE:
            if (LOWORD(wParam) == WA_INACTIVE)
                PostMessageW(hwnd, WM_CLOSE, 0, 0);
            return 0;
        case WM_CLOSE:
            DestroyWindow(hwnd);
            return 0;
        case WM_DESTROY: {
            HWND anchor = dropdownAnchor_;
            dropdownWindow_ = nullptr;
            dropdownList_ = nullptr;
            dropdownAnchor_ = nullptr;
            dropdownKind_ = DropdownKind::None;
            dropdownItems_.clear();
            dropdownHoverIndex_ = -1;
            dropdownVisibleRows_ = 0;
            dropdownScrollbarDragging_ = false;
            if (anchor)
                InvalidateRect(anchor, nullptr, TRUE);
            return 0;
        }
        }
        return DefWindowProcW(hwnd, message, wParam, lParam);
    }

    LRESULT handleGraphMessage(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam) {
        switch (message) {
        case WM_SIZE: {
            if (!graphCanvas_)
                return 0;
            RECT client{};
            GetClientRect(hwnd, &client);
            MoveWindow(graphCanvas_, scaled(14), scaled(14),
                       std::max(static_cast<LONG>(scaled(320)), client.right - scaled(28)),
                       std::max(static_cast<LONG>(scaled(240)), client.bottom - scaled(28)), TRUE);
            return 0;
        }
        case WM_GETMINMAXINFO: {
            auto* info = reinterpret_cast<MINMAXINFO*>(lParam);
            info->ptMinTrackSize = {scaled(640), scaled(460)};
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
            const int width = std::max(scaled(320), static_cast<int>(client.right));
            const int height = std::max(scaled(240), static_cast<int>(client.bottom));
            MoveWindow(logEdit_, scaled(16), scaled(16), width - scaled(32), height - scaled(82), TRUE);
            MoveWindow(logClearButton_, scaled(16), height - scaled(52), scaled(138), scaled(36), TRUE);
            MoveWindow(logImportButton_, width - scaled(292), height - scaled(52), scaled(132), scaled(36),
                       TRUE);
            MoveWindow(logExportButton_, width - scaled(148), height - scaled(52), scaled(132), scaled(36),
                       TRUE);
            roundControl(logEdit_, scaled(18));
            roundControl(logClearButton_, scaled(18));
            roundControl(logImportButton_, scaled(18));
            roundControl(logExportButton_, scaled(18));
            return 0;
        }
        case WM_GETMINMAXINFO: {
            auto* info = reinterpret_cast<MINMAXINFO*>(lParam);
            info->ptMinTrackSize = {scaled(540), scaled(420)};
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
            if (LOWORD(wParam) == kLogExportId || LOWORD(wParam) == kLogImportId) {
                if (LOWORD(wParam) == kLogExportId || historyReady_)
                    transferHistory(LOWORD(wParam) == kLogExportId);
                return 0;
            }
            if (LOWORD(wParam) == kLogEditId && HIWORD(wParam) == mathview::kLineClickedNotification) {
                const int line = mathview::lastClickedLine(logEdit_);
                if (line >= 0 && line < static_cast<int>(historyLineTargets_.size()) &&
                    !historyLineTargets_[static_cast<size_t>(line)].empty()) {
                    const std::wstring& equation = historyLineTargets_[static_cast<size_t>(line)];
                    SetWindowTextW(input_, equation.c_str());
                    SendMessageW(input_, EM_SETSEL, equation.size(), equation.size());
                    ShowWindow(hwnd, SW_HIDE);
                    ShowWindow(window_, SW_RESTORE);
                    SetForegroundWindow(window_);
                    SetFocus(input_);
                }
                return 0;
            }
            if (LOWORD(wParam) == kLogClearButtonId && HIWORD(wParam) == BN_CLICKED) {
                const int answer = MessageBoxW(
                    hwnd, localize(L"确定清空全部求解日志吗？", L"Clear all solution history?").c_str(),
                    localize(L"清空日志", L"Clear History").c_str(),
                    MB_YESNO | MB_ICONQUESTION | MB_DEFBUTTON2);
                if (answer == IDYES && persistHistory({})) {
                    history_.clear();
                    historyWasReset_ = false;
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
            logImportButton_ = nullptr;
            logExportButton_ = nullptr;
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
            closeDropdown();
            layoutControls();
            return 0;
        case WM_DPICHANGED: {
            const RECT suggested = *reinterpret_cast<RECT*>(lParam);
            const HMONITOR monitor = MonitorFromRect(&suggested, MONITOR_DEFAULTTONEAREST);
            const UINT dpi = HIWORD(wParam) ? HIWORD(wParam) : 96;
            if (IsZoomed(window_)) {
                updateDisplayScale(monitor, dpi, true);
                SetWindowPos(window_, nullptr, suggested.left, suggested.top,
                             suggested.right - suggested.left, suggested.bottom - suggested.top,
                             SWP_NOZORDER | SWP_NOACTIVATE);
                layoutControls();
            } else {
                adaptWindowToMonitor(monitor, dpi, suggested);
            }
            return 0;
        }
        case WM_DISPLAYCHANGE: {
            RECT current{};
            GetWindowRect(window_, &current);
            const HMONITOR monitor = MonitorFromWindow(window_, MONITOR_DEFAULTTONEAREST);
            if (IsZoomed(window_)) {
                updateDisplayScale(monitor, GetDpiForWindow(window_), true);
                layoutControls();
            } else {
                adaptWindowToMonitor(monitor, GetDpiForWindow(window_), current);
            }
            return 0;
        }
        case WM_SETTINGCHANGE:
            if (wParam == SPI_SETWORKAREA) {
                RECT current{};
                GetWindowRect(window_, &current);
                const HMONITOR monitor = MonitorFromWindow(window_, MONITOR_DEFAULTTONEAREST);
                if (!IsZoomed(window_))
                    adaptWindowToMonitor(monitor, GetDpiForWindow(window_), current);
            }
            break;
        case WM_ERASEBKGND: {
            RECT client{};
            GetClientRect(window_, &client);
            FillRect(reinterpret_cast<HDC>(wParam), &client, backgroundBrush_);
            return 1;
        }
        case WM_GETMINMAXINFO: {
            auto* info = reinterpret_cast<MINMAXINFO*>(lParam);
            info->ptMinTrackSize = {scaled(920), scaled(650)};
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
            closeDropdown();
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
    double uiScale_ = 1.0;
    UINT currentDpi_ = 96;
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
    HWND logImportButton_{};
    HWND logExportButton_{};
    HWND dropdownWindow_{};
    HWND dropdownList_{};
    HWND dropdownAnchor_{};
    DropdownKind dropdownKind_ = DropdownKind::None;
    std::vector<std::wstring> dropdownItems_;
    int dropdownSelectedIndex_ = -1;
    int dropdownHoverIndex_ = -1;
    int dropdownVisibleRows_ = 0;
    bool dropdownScrollbarDragging_ = false;
    int dropdownScrollbarDragOffset_ = 0;
    HWND graphWindow_{};
    HWND graphCanvas_{};
    int currentType_ = 0;
    equation::AngleUnit angleUnit_ = equation::AngleUnit::Radians;
    int selectedMethod_ = 0;
    std::wstring lastSummaryZh_;
    std::wstring lastApproximationZh_;
    std::vector<std::wstring> methodSectionsZh_;
    std::vector<HistoryEntry> history_;
    std::optional<history::Store> historyStore_;
    bool historyReady_ = false;
    bool historyWasReset_ = false;
    std::vector<std::wstring> historyLineTargets_;
    std::vector<std::pair<int, int>> historyLineGroups_;
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
