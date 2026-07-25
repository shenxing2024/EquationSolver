// 轻量表达式求值器与 Win32/GDI 绘图控件。求值失败使用 NaN 断开曲线，
// 因而分母为零以及 tan/cot 无定义点不会被当作普通坐标绘制。

#include "graph_view.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <cwctype>
#include <iomanip>
#include <limits>
#include <numeric>
#include <optional>
#include <sstream>
#include <vector>

namespace graphview {
namespace {

constexpr UINT kSetData = WM_APP + 61;
constexpr UINT kSetDarkMode = WM_APP + 62;
constexpr double kInvalid = std::numeric_limits<double>::quiet_NaN();
constexpr double kPi = 3.141592653589793238462643383279502884;
constexpr double kEuler = 2.718281828459045235360287471352662498;

double factorialValue(double value) {
    if (!std::isfinite(value) || value < 0.0 || value > 170.0 || std::abs(value - std::round(value)) >= 1e-10)
        return kInvalid;
    double result = 1.0;
    for (int factor = 2; factor <= static_cast<int>(std::round(value)); ++factor)
        result *= factor;
    return result;
}

std::wstring normalize(std::wstring text) {
    for (wchar_t& ch : text) {
        if (ch == L'×' || ch == L'·')
            ch = L'*';
        else if (ch == L'÷' || ch == L'／')
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

double realPower(double base, double exponent) {
    if (base >= 0.0)
        return std::pow(base, exponent);
    for (int denominator = 1; denominator <= 31; ++denominator) {
        const int numerator = static_cast<int>(std::llround(exponent * denominator));
        if (std::abs(exponent - static_cast<double>(numerator) / denominator) < 1e-10) {
            const int divisor = std::gcd(std::abs(numerator), denominator);
            const int reducedNumerator = numerator / divisor;
            const int reducedDenominator = denominator / divisor;
            if ((reducedDenominator & 1) == 0)
                return kInvalid;
            const double magnitude =
                std::pow(-base, static_cast<double>(reducedNumerator) / reducedDenominator);
            return (std::abs(reducedNumerator) & 1) ? -magnitude : magnitude;
        }
    }
    return kInvalid;
}

class NumericParser {
  public:
    NumericParser(std::wstring text, double x, double y, bool degreeMode)
        : text_(normalize(std::move(text))), x_(x), y_(y), degreeMode_(degreeMode) {}

    std::optional<double> parse() {
        position_ = 0;
        valid_ = true;
        const double result = expression();
        spaces();
        if (!valid_ || position_ != text_.size() || !std::isfinite(result))
            return std::nullopt;
        return result;
    }

  private:
    void spaces() {
        while (position_ < text_.size() && std::iswspace(text_[position_]))
            ++position_;
    }
    bool consume(wchar_t ch) {
        spaces();
        if (position_ < text_.size() && text_[position_] == ch) {
            ++position_;
            return true;
        }
        return false;
    }
    bool startsPrimary() {
        spaces();
        if (position_ >= text_.size())
            return false;
        const wchar_t ch = text_[position_];
        return std::iswdigit(ch) || ch == L'.' || ch == L'x' || ch == L'X' || ch == L'y' || ch == L'Y' ||
               ch == L'p' || ch == L'P' || ch == L'π' || ch == L'e' || ch == L'E' || ch == L's' ||
               ch == L'S' || ch == L'c' || ch == L'C' || ch == L't' || ch == L'T' || ch == L'(' ||
               ch == L'|' || ch == L'"';
    }
    double expression() {
        double value = term();
        while (valid_) {
            if (consume(L'+'))
                value += term();
            else if (consume(L'-'))
                value -= term();
            else
                break;
        }
        return value;
    }
    double term() {
        double value = unary();
        while (valid_) {
            if (consume(L'*'))
                value *= unary();
            else if (consume(L'/')) {
                const double divisor = unary();
                if (std::abs(divisor) < 1e-12) {
                    valid_ = false;
                    return kInvalid;
                }
                value /= divisor;
            } else if (startsPrimary())
                value *= unary();
            else
                break;
        }
        return value;
    }
    double unary() {
        if (consume(L'+'))
            return unary();
        if (consume(L'-'))
            return -unary();
        double value = primary();
        while (consume(L'!')) {
            value = factorialValue(value);
            if (!std::isfinite(value)) {
                valid_ = false;
                return kInvalid;
            }
        }
        if (consume(L'^'))
            value = realPower(value, unary());
        return value;
    }
    double primary() {
        spaces();
        if (position_ >= text_.size()) {
            valid_ = false;
            return kInvalid;
        }
        const wchar_t ch = text_[position_];
        if (std::iswdigit(ch) || ch == L'.') {
            wchar_t* end = nullptr;
            const double value = std::wcstod(text_.c_str() + position_, &end);
            if (end == text_.c_str() + position_) {
                valid_ = false;
                return kInvalid;
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
            return after < text_.size() && text_[after] == L'(';
        };
        const wchar_t* function =
            functionAt(L"sin")
                ? L"sin"
                : (functionAt(L"cos")
                       ? L"cos"
                       : (functionAt(L"tan") ? L"tan" : (functionAt(L"cot") ? L"cot" : nullptr)));
        if (function) {
            position_ += 3;
            if (!consume(L'(')) {
                valid_ = false;
                return kInvalid;
            }
            const double inputAngle = expression();
            if (!consume(L')')) {
                valid_ = false;
                return kInvalid;
            }
            const double angle = degreeMode_ ? inputAngle * kPi / 180.0 : inputAngle;
            if (function[0] == L's')
                return std::sin(angle);
            if (function[0] == L'c' && function[1] == L'o' && function[2] == L's')
                return std::cos(angle);
            if (function[0] == L't') {
                if (std::abs(std::cos(angle)) < 1e-12)
                    return kInvalid;
                return std::tan(angle);
            }
            if (std::abs(std::sin(angle)) < 1e-12)
                return kInvalid;
            return std::cos(angle) / std::sin(angle);
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
        if (ch == L'x' || ch == L'X') {
            ++position_;
            return x_;
        }
        if (ch == L'y' || ch == L'Y') {
            ++position_;
            return y_;
        }
        if (consume(L'(')) {
            const double value = expression();
            if (!consume(L')'))
                valid_ = false;
            return value;
        }
        if (ch == L'|') {
            const size_t close = text_.find(L'|', position_ + 1);
            if (close == std::wstring::npos) {
                valid_ = false;
                return kInvalid;
            }
            const auto value =
                NumericParser(text_.substr(position_ + 1, close - position_ - 1), x_, y_, degreeMode_)
                    .parse();
            position_ = close + 1;
            if (!value) {
                valid_ = false;
                return kInvalid;
            }
            return std::abs(*value);
        }
        if (ch == L'"')
            return rootExpression();
        valid_ = false;
        return kInvalid;
    }
    double rootExpression() {
        const size_t opening = position_++;
        const size_t firstClose = text_.find(L'"', position_);
        if (firstClose == std::wstring::npos) {
            valid_ = false;
            return kInvalid;
        }
        const std::wstring first = text_.substr(position_, firstClose - position_);
        double degreeValue = 0.0;
        const auto parsedDegree = NumericParser(first, x_, y_, degreeMode_).parse();
        const size_t secondClose = text_.find(L'"', firstClose + 1);
        if (parsedDegree && secondClose != std::wstring::npos && *parsedDegree >= 2.0 &&
            *parsedDegree <= 32.0 && std::abs(*parsedDegree - std::round(*parsedDegree)) < 1e-10) {
            degreeValue = std::round(*parsedDegree);
            const std::wstring radicandText = text_.substr(firstClose + 1, secondClose - firstClose - 1);
            const auto radicand = NumericParser(radicandText, x_, y_, degreeMode_).parse();
            position_ = secondClose + 1;
            if (!radicand) {
                valid_ = false;
                return kInvalid;
            }
            return realPower(*radicand, 1.0 / degreeValue);
        }
        (void)opening;
        const auto radicand = NumericParser(first, x_, y_, degreeMode_).parse();
        position_ = firstClose + 1;
        if (!radicand || *radicand < 0.0) {
            valid_ = false;
            return kInvalid;
        }
        return std::sqrt(*radicand);
    }

    std::wstring text_;
    size_t position_ = 0;
    bool valid_ = true;
    double x_ = 0.0;
    double y_ = 0.0;
    bool degreeMode_ = false;
};

struct RelationParts {
    std::wstring left;
    std::wstring right;
};

std::optional<RelationParts> splitEquation(const std::wstring& input) {
    const std::wstring text = normalize(input);
    const size_t equals = text.find(L'=');
    if (equals == std::wstring::npos)
        return std::nullopt;
    return RelationParts{text.substr(0, equals), text.substr(equals + 1)};
}

std::vector<std::wstring> lines(const std::wstring& text) {
    std::vector<std::wstring> result;
    std::wstring current;
    for (wchar_t ch : text) {
        if (ch == L'\r')
            continue;
        if (ch == L'\n' || ch == L';' || ch == L'；') {
            if (!current.empty())
                result.push_back(current);
            current.clear();
        } else
            current += ch;
    }
    if (!current.empty())
        result.push_back(current);
    return result;
}

double evaluate(const std::wstring& expression, double x, double y = 0.0, bool degreeMode = false) {
    const auto result = NumericParser(expression, x, y, degreeMode).parse();
    return result ? *result : kInvalid;
}

struct State {
    GraphRequest request;
    HFONT font = nullptr;
};

COLORREF color(bool dark, COLORREF light, COLORREF darkValue) {
    return dark ? darkValue : light;
}

void drawText(HDC dc, const std::wstring& text, int x, int y, COLORREF foreground) {
    SetBkMode(dc, TRANSPARENT);
    SetTextColor(dc, foreground);
    TextOutW(dc, x, y, text.c_str(), static_cast<int>(text.size()));
}

std::wstring shortNumber(double value) {
    if (std::abs(value) < 1e-10)
        value = 0.0;
    std::wostringstream out;
    out << std::fixed << std::setprecision(std::abs(value) < 10 ? 2 : 1) << value;
    std::wstring text = out.str();
    while (text.size() > 1 && text.back() == L'0')
        text.pop_back();
    if (!text.empty() && text.back() == L'.')
        text.pop_back();
    return text;
}

void paint(HWND window, State& state) {
    // 先确定自适应坐标范围，再采样等号两侧；方程组则绘制两条隐式曲线。
    PAINTSTRUCT ps{};
    HDC dc = BeginPaint(window, &ps);
    RECT client{};
    GetClientRect(window, &client);
    HDC memory = CreateCompatibleDC(dc);
    HBITMAP bitmap = CreateCompatibleBitmap(dc, std::max(1L, client.right), std::max(1L, client.bottom));
    HGDIOBJ oldBitmap = SelectObject(memory, bitmap);
    HFONT font = state.font ? state.font : reinterpret_cast<HFONT>(GetStockObject(DEFAULT_GUI_FONT));
    HGDIOBJ oldFont = SelectObject(memory, font);
    const bool dark = state.request.darkMode;
    HBRUSH background = CreateSolidBrush(color(dark, RGB(255, 255, 255), RGB(31, 35, 42)));
    FillRect(memory, &client, background);
    DeleteObject(background);

    RECT plot{62, 52, std::max(180L, client.right - 30), std::max(160L, client.bottom - 48)};
    double maxRootX = 0.0, maxRootY = 0.0;
    for (const auto& marker : state.request.markers) {
        if (std::isfinite(marker.first))
            maxRootX = std::max(maxRootX, std::abs(marker.first));
        if (std::isfinite(marker.second))
            maxRootY = std::max(maxRootY, std::abs(marker.second));
    }
    double xExtent = std::max(state.request.degreeMode ? 360.0 : 5.0, std::ceil(maxRootX * 1.35 + 1.0));
    double yExtent = std::max(5.0, std::ceil(maxRootY * 1.35 + 1.0));
    const auto inputLines = lines(state.request.input);
    std::vector<RelationParts> equations;
    for (const auto& line : inputLines)
        if (auto parts = splitEquation(line))
            equations.push_back(*parts);

    if (!state.request.system2 && !equations.empty()) {
        std::vector<double> magnitudes;
        for (int i = 0; i <= 200; ++i) {
            const double x = -xExtent + 2.0 * xExtent * i / 200.0;
            for (const auto* expression : {&equations[0].left, &equations[0].right}) {
                const double value = evaluate(*expression, x, 0.0, state.request.degreeMode);
                if (std::isfinite(value) && std::abs(value) < 1e7)
                    magnitudes.push_back(std::abs(value));
            }
        }
        if (!magnitudes.empty()) {
            std::sort(magnitudes.begin(), magnitudes.end());
            const double robust = magnitudes[static_cast<size_t>(magnitudes.size() * 0.85)];
            yExtent = std::max(yExtent, std::min(10000.0, std::max(5.0, std::ceil(robust * 1.2))));
        }
    }

    const auto px = [&](double x) {
        return plot.left + static_cast<int>((x + xExtent) / (2 * xExtent) * (plot.right - plot.left));
    };
    const auto py = [&](double y) {
        return plot.bottom - static_cast<int>((y + yExtent) / (2 * yExtent) * (plot.bottom - plot.top));
    };
    const COLORREF grid = color(dark, RGB(228, 232, 239), RGB(56, 61, 73));
    const COLORREF axis = color(dark, RGB(90, 99, 112), RGB(178, 185, 198));
    HPEN gridPen = CreatePen(PS_SOLID, 1, grid);
    HGDIOBJ oldPen = SelectObject(memory, gridPen);
    for (int i = -5; i <= 5; ++i) {
        const int x = plot.left + (i + 5) * (plot.right - plot.left) / 10;
        const int y = plot.top + (i + 5) * (plot.bottom - plot.top) / 10;
        MoveToEx(memory, x, plot.top, nullptr);
        LineTo(memory, x, plot.bottom);
        MoveToEx(memory, plot.left, y, nullptr);
        LineTo(memory, plot.right, y);
    }
    SelectObject(memory, oldPen);
    DeleteObject(gridPen);
    HPEN axisPen = CreatePen(PS_SOLID, 2, axis);
    oldPen = SelectObject(memory, axisPen);
    if (0 >= -xExtent && 0 <= xExtent) {
        MoveToEx(memory, px(0), plot.top, nullptr);
        LineTo(memory, px(0), plot.bottom);
    }
    if (0 >= -yExtent && 0 <= yExtent) {
        MoveToEx(memory, plot.left, py(0), nullptr);
        LineTo(memory, plot.right, py(0));
    }
    SelectObject(memory, oldPen);
    DeleteObject(axisPen);
    const COLORREF textColor = color(dark, RGB(45, 54, 66), RGB(232, 236, 242));
    drawText(memory, state.request.xAxisLabel, plot.right - 12, py(0) + 6, textColor);
    drawText(memory, state.request.yAxisLabel, px(0) + 7, plot.top + 2, textColor);
    drawText(memory, shortNumber(-xExtent), plot.left, plot.bottom + 8, textColor);
    drawText(memory, shortNumber(xExtent), plot.right - 34, plot.bottom + 8, textColor);
    drawText(memory, shortNumber(yExtent), plot.left - 48, plot.top - 6, textColor);

    const COLORREF curveColors[] = {RGB(63, 104, 238), RGB(245, 139, 50), RGB(20, 171, 118)};
    auto drawFunction = [&](const std::wstring& expression, COLORREF curve) {
        HPEN pen = CreatePen(PS_SOLID, 2, curve);
        HGDIOBJ saved = SelectObject(memory, pen);
        bool previousValid = false;
        POINT previous{};
        double previousValue = 0.0;
        for (int pixel = plot.left; pixel <= plot.right; ++pixel) {
            const double x =
                -xExtent + 2.0 * xExtent * (pixel - plot.left) / std::max(1L, plot.right - plot.left);
            const double y = evaluate(expression, x, 0.0, state.request.degreeMode);
            const bool valid = std::isfinite(y) && std::abs(y) <= yExtent * 1.25;
            const POINT current{pixel, valid ? py(y) : 0};
            if (valid && previousValid && std::abs(y - previousValue) < yExtent * 0.55) {
                MoveToEx(memory, previous.x, previous.y, nullptr);
                LineTo(memory, current.x, current.y);
            }
            previousValid = valid;
            previous = current;
            previousValue = y;
        }
        SelectObject(memory, saved);
        DeleteObject(pen);
    };

    if (state.request.system2) {
        for (size_t index = 0; index < equations.size() && index < 3; ++index) {
            const auto& eq = equations[index];
            const auto f = [&](double x, double y) {
                return evaluate(eq.left, x, y, state.request.degreeMode) -
                       evaluate(eq.right, x, y, state.request.degreeMode);
            };
            HPEN pen = CreatePen(PS_SOLID, 2, curveColors[index]);
            HGDIOBJ saved = SelectObject(memory, pen);

            // 用 marching squares 绘制 f(x,y)=0 的隐式曲线。线性方程、圆、椭圆、
            // 双曲线和一般二次曲线都走同一条路径，避免把二次方程误画成直线。
            const int columns = std::clamp<int>((plot.right - plot.left) / 3, 80, 260);
            const int rows = std::clamp<int>((plot.bottom - plot.top) / 3, 60, 200);
            std::vector<double> samples(static_cast<size_t>(columns + 1) * (rows + 1), kInvalid);
            const auto sampleIndex = [&](int column, int row) {
                return static_cast<size_t>(row) * (columns + 1) + column;
            };
            for (int row = 0; row <= rows; ++row) {
                const double y = yExtent - 2.0 * yExtent * row / rows;
                for (int column = 0; column <= columns; ++column) {
                    const double x = -xExtent + 2.0 * xExtent * column / columns;
                    samples[sampleIndex(column, row)] = f(x, y);
                }
            }

            struct ContourPoint {
                double x = 0.0;
                double y = 0.0;
            };
            const auto crossing = [](double first, double second) {
                if (!std::isfinite(first) || !std::isfinite(second))
                    return false;
                if (std::abs(first) < 1e-12 || std::abs(second) < 1e-12)
                    return true;
                return std::signbit(first) != std::signbit(second);
            };
            const auto interpolate = [](ContourPoint firstPoint, ContourPoint secondPoint, double first,
                                        double second) {
                double ratio = std::abs(first - second) < 1e-20 ? 0.5 : first / (first - second);
                ratio = std::clamp(ratio, 0.0, 1.0);
                return ContourPoint{firstPoint.x + (secondPoint.x - firstPoint.x) * ratio,
                                    firstPoint.y + (secondPoint.y - firstPoint.y) * ratio};
            };
            for (int row = 0; row < rows; ++row) {
                const double top = yExtent - 2.0 * yExtent * row / rows;
                const double bottom = yExtent - 2.0 * yExtent * (row + 1) / rows;
                for (int column = 0; column < columns; ++column) {
                    const double left = -xExtent + 2.0 * xExtent * column / columns;
                    const double right = -xExtent + 2.0 * xExtent * (column + 1) / columns;
                    const std::array<ContourPoint, 4> corners{
                        ContourPoint{left, top}, ContourPoint{right, top},
                        ContourPoint{right, bottom}, ContourPoint{left, bottom}};
                    const std::array<double, 4> values{
                        samples[sampleIndex(column, row)], samples[sampleIndex(column + 1, row)],
                        samples[sampleIndex(column + 1, row + 1)], samples[sampleIndex(column, row + 1)]};
                    std::vector<ContourPoint> intersections;
                    for (int edge = 0; edge < 4; ++edge) {
                        const int next = (edge + 1) % 4;
                        if (crossing(values[edge], values[next]))
                            intersections.push_back(
                                interpolate(corners[edge], corners[next], values[edge], values[next]));
                    }
                    if (intersections.size() < 2)
                        continue;
                    for (size_t point = 0; point + 1 < intersections.size(); point += 2) {
                        MoveToEx(memory, px(intersections[point].x), py(intersections[point].y), nullptr);
                        LineTo(memory, px(intersections[point + 1].x), py(intersections[point + 1].y));
                    }
                }
            }
            SelectObject(memory, saved);
            DeleteObject(pen);
        }
    } else if (!equations.empty()) {
        drawFunction(equations[0].left, curveColors[0]);
        drawFunction(equations[0].right, curveColors[1]);
    }

    for (const auto& marker : state.request.markers) {
        double markerY = marker.second;
        if (!std::isfinite(markerY) && !equations.empty())
            markerY = evaluate(equations[0].left, marker.first, 0.0, state.request.degreeMode);
        if (!std::isfinite(marker.first) || !std::isfinite(markerY))
            continue;
        const int x = px(marker.first), y = py(markerY);
        HBRUSH dot = CreateSolidBrush(RGB(224, 68, 84));
        HGDIOBJ saved = SelectObject(memory, dot);
        Ellipse(memory, x - 5, y - 5, x + 6, y + 6);
        SelectObject(memory, saved);
        DeleteObject(dot);
        drawText(memory, L"(" + shortNumber(marker.first) + L", " + shortNumber(markerY) + L")", x + 8,
                 y - 20, textColor);
    }

    drawText(memory, L"● " + state.request.leftLegend, 64, 16, curveColors[0]);
    drawText(memory, L"● " + state.request.rightLegend, 250, 16, curveColors[1]);
    if (!state.request.markers.empty())
        drawText(memory, L"● " + state.request.intersectionLabel, 440, 16, RGB(224, 68, 84));

    BitBlt(dc, 0, 0, client.right, client.bottom, memory, 0, 0, SRCCOPY);
    SelectObject(memory, oldFont);
    SelectObject(memory, oldBitmap);
    DeleteObject(bitmap);
    DeleteDC(memory);
    EndPaint(window, &ps);
}

LRESULT CALLBACK windowProc(HWND window, UINT message, WPARAM wParam, LPARAM lParam) {
    auto* state = reinterpret_cast<State*>(GetWindowLongPtrW(window, GWLP_USERDATA));
    if (message == WM_CREATE) {
        state = new State();
        SetWindowLongPtrW(window, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(state));
        return 0;
    }
    if (!state)
        return DefWindowProcW(window, message, wParam, lParam);
    switch (message) {
    case kSetData:
        state->request = *reinterpret_cast<const GraphRequest*>(lParam);
        InvalidateRect(window, nullptr, TRUE);
        return 0;
    case kSetDarkMode:
        state->request.darkMode = wParam != 0;
        InvalidateRect(window, nullptr, TRUE);
        return 0;
    case WM_SETFONT:
        state->font = reinterpret_cast<HFONT>(wParam);
        InvalidateRect(window, nullptr, TRUE);
        return 0;
    case WM_ERASEBKGND:
        return 1;
    case WM_PAINT:
        paint(window, *state);
        return 0;
    case WM_SIZE:
        InvalidateRect(window, nullptr, TRUE);
        return 0;
    case WM_NCDESTROY:
        delete state;
        SetWindowLongPtrW(window, GWLP_USERDATA, 0);
        break;
    }
    return DefWindowProcW(window, message, wParam, lParam);
}

} // namespace

bool registerClass(HINSTANCE instance) {
    WNDCLASSEXW wc{};
    wc.cbSize = sizeof(wc);
    wc.style = CS_HREDRAW | CS_VREDRAW;
    wc.lpfnWndProc = windowProc;
    wc.hInstance = instance;
    wc.hCursor = LoadCursorW(nullptr, IDC_CROSS);
    wc.lpszClassName = kClassName;
    return RegisterClassExW(&wc) || GetLastError() == ERROR_CLASS_ALREADY_EXISTS;
}

void setData(HWND window, const GraphRequest& request) {
    SendMessageW(window, kSetData, 0, reinterpret_cast<LPARAM>(&request));
}

void setDarkMode(HWND window, bool darkMode) {
    SendMessageW(window, kSetDarkMode, darkMode, 0);
}

} // namespace graphview
