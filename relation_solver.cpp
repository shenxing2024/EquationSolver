#include "relation_solver.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cwctype>
#include <limits>
#include <map>
#include <memory>
#include <numeric>
#include <optional>
#include <set>
#include <sstream>

namespace equation {
namespace {

// 有限小数从原文转换为有理数；整个符号计算过程不把参数系数变成 double。
struct Rational {
    std::int64_t n = 0, d = 1;
    Rational(std::int64_t numerator = 0, std::int64_t denominator = 1) : n(numerator), d(denominator) {
        if (!d)
            throw ParseError("分母不能为 0");
        if (d < 0) {
            n = -n;
            d = -d;
        }
        const auto g = std::gcd(n, d);
        n /= g;
        d /= g;
    }
};

std::optional<Rational> arithmetic(Rational a, Rational b, bool product) {
    // 留足余量，避免后续取负数及乘加溢出；超限时保留原始算式。
    constexpr auto limit = std::numeric_limits<std::int64_t>::max() / 4;
    if (product) {
        const auto g1 = std::gcd(a.n, b.d), g2 = std::gcd(b.n, a.d);
        a.n /= g1;
        b.d /= g1;
        b.n /= g2;
        a.d /= g2;
        if (std::abs(static_cast<long double>(a.n) * b.n) > limit ||
            static_cast<long double>(a.d) * b.d > limit)
            return std::nullopt;
        return Rational(a.n * b.n, a.d * b.d);
    }
    const auto g = std::gcd(a.d, b.d);
    if (std::abs(static_cast<long double>(a.n) * (b.d / g)) > limit ||
        std::abs(static_cast<long double>(b.n) * (a.d / g)) > limit ||
        static_cast<long double>(a.d) * (b.d / g) > limit)
        return std::nullopt;
    const auto left = a.n * (b.d / g), right = b.n * (a.d / g);
    if (std::abs(static_cast<long double>(left) + right) > limit)
        return std::nullopt;
    return Rational(left + right, a.d * (b.d / g));
}

enum class Kind { Number, Symbol, Add, Multiply, Divide, Power, Negate, Function, Root };
struct Node;
using E = std::shared_ptr<const Node>;
struct Node {
    Kind kind;
    std::wstring name;
    std::optional<Rational> number;
    E a, b;
};

E node(Kind kind, E a = {}, E b = {}, std::wstring name = L"") {
    return std::make_shared<Node>(Node{kind, std::move(name), {}, std::move(a), std::move(b)});
}
E number(Rational r) {
    return std::make_shared<Node>(Node{Kind::Number, L"", r, {}, {}});
}
E integer(std::int64_t n) {
    return number(Rational(n));
}
E symbol(std::wstring name) {
    return node(Kind::Symbol, {}, {}, std::move(name));
}
bool is(E e, std::int64_t n) {
    return e->number && e->number->n == n && e->number->d == 1;
}
std::wstring text(E e);
E neg(E e);
E add(E a, E b);
E mul(E a, E b);
E divide(E a, E b);
E power(E a, E b);

std::wstring key(E e) {
    if (!e)
        return L"";
    if (e->number)
        return std::to_wstring(e->number->n) + L"/" + std::to_wstring(e->number->d);
    return std::to_wstring(static_cast<int>(e->kind)) + L":" + e->name + L"[" + key(e->a) + L"][" +
           key(e->b) + L"]";
}
bool same(E a, E b) {
    return a == b || key(a) == key(b);
}
bool contains(E e, wchar_t variable) {
    if (!e)
        return false;
    return (e->kind == Kind::Symbol && e->name == std::wstring(1, variable)) || contains(e->a, variable) ||
           contains(e->b, variable);
}
bool hasSymbol(E e, const std::wstring& name) {
    if (!e)
        return false;
    return (e->kind == Kind::Symbol && e->name == name) || hasSymbol(e->a, name) || hasSymbol(e->b, name);
}

E neg(E e) {
    if (e->number)
        return number(Rational(-e->number->n, e->number->d));
    if (e->kind == Kind::Negate)
        return e->a;
    return node(Kind::Negate, e);
}

// 把加法合并为“系数×同一表达式”，能精确识别 b-b、ab-ba 等抵消。
E add(E a, E b) {
    std::vector<std::pair<E, Rational>> items;
    const auto collect = [&](auto&& self, E e, Rational scale) -> void {
        if (e->kind == Kind::Add) {
            self(self, e->a, scale);
            self(self, e->b, scale);
        } else if (e->kind == Kind::Negate)
            self(self, e->a, Rational(-scale.n, scale.d));
        else if (e->number) {
            if (const auto r = arithmetic(*e->number, scale, true))
                items.push_back({integer(1), *r});
            else
                items.push_back({e, scale});
        } else if (e->kind == Kind::Multiply && e->a->number) {
            if (const auto r = arithmetic(*e->a->number, scale, true))
                items.push_back({e->b, *r});
            else
                items.push_back({e, scale});
        } else
            items.push_back({e, scale});
    };
    collect(collect, a, Rational(1));
    collect(collect, b, Rational(1));
    std::vector<std::pair<E, Rational>> merged;
    for (const auto& item : items) {
        auto found = std::find_if(merged.begin(), merged.end(),
                                  [&](const auto& value) { return same(value.first, item.first); });
        if (found != merged.end()) {
            if (const auto r = arithmetic(found->second, item.second, false))
                found->second = *r;
            else
                merged.push_back(item);
        } else
            merged.push_back(item);
    }
    E result;
    for (const auto& item : merged) {
        if (!item.second.n)
            continue;
        E term = is(item.first, 1)                 ? number(item.second)
                 : item.second.n == item.second.d  ? item.first
                 : item.second.n == -item.second.d ? neg(item.first)
                                                   : mul(number(item.second), item.first);
        result = result ? node(Kind::Add, result, term) : term;
    }
    return result ? result : integer(0);
}
E sub(E a, E b) {
    return add(a, neg(b));
}

E mul(E a, E b) {
    if (is(a, 0) || is(b, 0))
        return integer(0);
    if (is(a, 1))
        return b;
    if (is(b, 1))
        return a;
    if (is(a, -1))
        return neg(b);
    if (is(b, -1))
        return neg(a);
    if (a->kind == Kind::Negate)
        return neg(mul(a->a, b));
    if (b->kind == Kind::Negate)
        return neg(mul(a, b->a));
    if (a->number && b->number) {
        if (const auto r = arithmetic(*a->number, *b->number, true))
            return number(*r);
    }
    if (b->number && !a->number)
        std::swap(a, b);
    if (a->number && a->number->n < 0)
        return neg(mul(neg(a), b));
    if (a->number && b->kind == Kind::Multiply && b->a->number) {
        if (const auto r = arithmetic(*a->number, *b->a->number, true))
            return mul(number(*r), b->b);
    }
    if (!a->number && !b->number && key(a) > key(b))
        std::swap(a, b);
    return node(Kind::Multiply, a, b);
}

E divide(E a, E b) {
    if (is(b, 0))
        return node(Kind::Divide, a, b); // 原始定义域由独立的约束列表保存。
    if (is(a, 0))
        return integer(0);
    if (is(b, 1))
        return a;
    if (is(b, -1))
        return neg(a);
    if (b->kind == Kind::Negate)
        return neg(divide(a, b->a));
    if (b->number && b->number->n < 0)
        return neg(divide(a, neg(b)));
    if (same(a, b))
        return integer(1);
    if (a->number && b->number && b->number->n) {
        if (const auto r = arithmetic(*a->number, Rational(b->number->d, b->number->n), true))
            return number(*r);
    }
    if (a->kind == Kind::Negate)
        return neg(divide(a->a, b));
    return node(Kind::Divide, a, b);
}

E power(E a, E b) {
    if (is(b, 0))
        return integer(1);
    if (is(b, 1))
        return a;
    if (is(a, 1))
        return a;
    if (a->number && b->number && b->number->d == 1 && std::abs(b->number->n) <= 16) {
        Rational result(1);
        bool exact = true;
        for (int i = 0; i < std::abs(b->number->n); ++i) {
            const auto next = arithmetic(result, *a->number, true);
            if (!next) {
                exact = false;
                break;
            }
            result = *next;
        }
        if (exact && (b->number->n >= 0 || result.n))
            return number(b->number->n < 0 ? Rational(result.d, result.n) : result);
    }
    return node(Kind::Power, a, b);
}

E function(const std::wstring& name, E a) {
    if (name == L"ln" && is(a, 1))
        return integer(0);
    if (name == L"ln" && a->kind == Kind::Symbol && a->name == L"e")
        return integer(1);
    if (name == L"exp" && is(a, 0))
        return integer(1);
    if (name == L"abs" && a->number)
        return number(Rational(std::abs(a->number->n), a->number->d));
    return node(Kind::Function, a, {}, name);
}
E root(E a, int degree) {
    if (degree == 1)
        return a;
    if (degree % 2 && a->number && a->number->n < 0)
        return neg(root(neg(a), degree));
    if (a->number && (a->number->n >= 0 || degree % 2)) {
        const auto exactRoot = [&](std::int64_t value) -> std::optional<std::int64_t> {
            const auto rounded = static_cast<std::int64_t>(
                std::round(std::pow(static_cast<long double>(value), 1.0L / degree)));
            Rational result(1);
            for (int i = 0; i < degree; ++i) {
                const auto next = arithmetic(result, Rational(rounded), true);
                if (!next)
                    return std::nullopt;
                result = *next;
            }
            return result.n == value ? std::optional<std::int64_t>(rounded) : std::nullopt;
        };
        const auto n = exactRoot(std::abs(a->number->n)), d = exactRoot(a->number->d);
        if (n && d)
            return number(Rational(a->number->n < 0 ? -*n : *n, *d));
    }
    if (degree % 2 && a->kind == Kind::Negate)
        return neg(root(a->a, degree));
    return node(Kind::Root, a, integer(degree));
}

int precedence(E e) {
    if (e->kind == Kind::Add)
        return 1;
    if (e->kind == Kind::Multiply || e->kind == Kind::Divide || (e->number && e->number->d != 1))
        return 2;
    if (e->kind == Kind::Negate || (e->number && e->number->n < 0))
        return 3;
    return 4;
}
std::wstring grouped(E e, int level) {
    const auto value = text(e);
    return precedence(e) < level ? L"(" + value + L")" : value;
}
std::wstring text(E e) {
    if (e->number)
        return std::to_wstring(e->number->n) +
               (e->number->d == 1 ? L"" : L"/" + std::to_wstring(e->number->d));
    switch (e->kind) {
    case Kind::Symbol:
        return e->name;
    case Kind::Number:
        return e->name;
    case Kind::Add: {
        if (e->b->kind == Kind::Negate)
            return text(e->a) + L" - " + grouped(e->b->a, 2);
        if (e->b->number && e->b->number->n < 0)
            return text(e->a) + L" - " + text(neg(e->b));
        return text(e->a) + L" + " + text(e->b);
    }
    case Kind::Negate:
        return L"-" + grouped(e->a, 4);
    case Kind::Multiply:
        return grouped(e->a, 3) + L"·" + grouped(e->b, 3);
    case Kind::Divide:
        return grouped(e->a, 2) + L"/" + grouped(e->b, 3);
    case Kind::Power:
        return grouped(e->a, 4) + L"^" +
               (e->b->number && e->b->number->d == 1 ? text(e->b) : L"(" + text(e->b) + L")");
    case Kind::Root: {
        const int degree = static_cast<int>(e->b->number->n);
        return (degree == 2   ? L"√"
                : degree == 3 ? L"∛"
                              : L"√[" + std::to_wstring(degree) + L"]") +
               L"{" + text(e->a) + L"}";
    }
    case Kind::Function:
        if (e->name == L"abs")
            return L"|" + text(e->a) + L"|";
        if (e->name == L"factorial")
            return grouped(e->a, 4) + L"!";
        return e->name + L"(" + text(e->a) + L")";
    }
    return L"?";
}

E replace(E e, wchar_t variable, E value) {
    if (!e || !contains(e, variable))
        return e;
    if (e->kind == Kind::Symbol && e->name == std::wstring(1, variable))
        return value;
    E a = replace(e->a, variable, value), b = replace(e->b, variable, value);
    switch (e->kind) {
    case Kind::Add:
        return add(a, b);
    case Kind::Multiply:
        return mul(a, b);
    case Kind::Divide:
        return divide(a, b);
    case Kind::Power:
        return power(a, b);
    case Kind::Negate:
        return neg(a);
    case Kind::Root:
        return root(a, static_cast<int>(b->number->n));
    case Kind::Function:
        return function(e->name, a);
    default:
        return e;
    }
}

enum class Op { Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual, Integer, Defined };
struct Condition {
    E a;
    Op op;
    E b;
};
std::wstring opText(Op op) {
    switch (op) {
    case Op::Equal:
        return L" = ";
    case Op::NotEqual:
        return L" ≠ ";
    case Op::Less:
        return L" < ";
    case Op::LessEqual:
        return L" ≤ ";
    case Op::Greater:
        return L" > ";
    case Op::GreaterEqual:
        return L" ≥ ";
    default:
        return L"";
    }
}
std::wstring text(const Condition& c) {
    if (c.op == Op::Integer)
        return text(c.a) + L" ∈ ℤ";
    if (c.op == Op::Defined)
        return text(c.a) + L" 有定义";
    return text(c.a) + opText(c.op) + text(c.b);
}
Op reversed(Op op) {
    if (op == Op::Less)
        return Op::Greater;
    if (op == Op::LessEqual)
        return Op::GreaterEqual;
    if (op == Op::Greater)
        return Op::Less;
    if (op == Op::GreaterEqual)
        return Op::LessEqual;
    return op;
}
std::optional<bool> truth(const Condition& c) {
    if (c.op == Op::Defined)
        return std::nullopt;
    if (c.op == Op::Integer) {
        if (c.a->number)
            return c.a->number->d == 1;
        return std::nullopt;
    }
    if (same(c.a, c.b))
        return c.op == Op::Equal || c.op == Op::LessEqual || c.op == Op::GreaterEqual;
    if (c.a->number && c.b->number) {
        const Rational a = *c.a->number, b = *c.b->number;
        const bool equal = a.n == b.n && a.d == b.d;
        if (c.op == Op::Equal)
            return equal;
        if (c.op == Op::NotEqual)
            return !equal;
        const long double av = static_cast<long double>(a.n) / a.d, bv = static_cast<long double>(b.n) / b.d;
        if (std::abs(av - bv) <=
            std::numeric_limits<long double>::epsilon() * std::max({1.0L, std::abs(av), std::abs(bv)}) * 4)
            return std::nullopt;
        if (c.op == Op::Less || c.op == Op::LessEqual)
            return av < bv;
        return av > bv;
    }
    if ((c.op == Op::NotEqual || c.op == Op::Equal) && is(c.b, 0)) {
        if (c.a->kind == Kind::Symbol && (c.a->name == L"e" || c.a->name == L"π" || c.a->name == L"i"))
            return c.op == Op::NotEqual;
        if (c.a->kind == Kind::Function && c.a->name == L"exp")
            return c.op == Op::NotEqual;
    }
    return std::nullopt;
}
bool simplifyConditions(std::vector<Condition>& conditions) {
    std::vector<Condition> result;
    std::set<std::wstring> seen;
    for (const auto& condition : conditions) {
        const auto known = truth(condition);
        if (known && !*known)
            return false;
        if (known)
            continue;
        if (seen.insert(text(condition)).second)
            result.push_back(condition);
    }
    conditions = std::move(result);
    return true;
}

struct Equation {
    E a, b;
    Op op = Op::Equal;
};
class Reader {
  public:
    explicit Reader(std::wstring source, bool complex = false)
        : source_(std::move(source)), complex_(complex) {
        for (auto& ch : source_) {
            if (ch == L'×' || ch == L'·')
                ch = L'*';
            if (ch == L'÷')
                ch = L'/';
            if (ch == L'−' || ch == L'–' || ch == L'—')
                ch = L'-';
            if (ch == L'“' || ch == L'”' || ch == L'＂')
                ch = L'"';
            if (ch == L'＝')
                ch = L'=';
            if (ch == L'！')
                ch = L'!';
            if (ch == L'（' || ch == L'[' || ch == L'［' || ch == L'【' || ch == L'{' || ch == L'｛')
                ch = L'(';
            if (ch == L'）' || ch == L']' || ch == L'］' || ch == L'】' || ch == L'}' || ch == L'｝')
                ch = L')';
        }
        if (source_.size() > 32768)
            throw ParseError("关系式过长，请缩短输入");
    }
    std::vector<Condition> domains;
    Equation equation() {
        Equation result;
        result.a = sum();
        if (take(L'=')) {
            result.op = Op::Equal;
            take(L'=');
        } else if (take(L'≠'))
            result.op = Op::NotEqual;
        else if (take(L'≤'))
            result.op = Op::LessEqual;
        else if (take(L'≥'))
            result.op = Op::GreaterEqual;
        else if (take(L'<'))
            result.op = take(L'=') ? Op::LessEqual : Op::Less;
        else if (take(L'>'))
            result.op = take(L'=') ? Op::GreaterEqual : Op::Greater;
        else
            throw ParseError("原式中需要有等号或不等号");
        result.b = sum();
        if (peek())
            throw ParseError("关系符右侧还有无法解析的内容");
        if (complex_ && result.op != Op::Equal && result.op != Op::NotEqual)
            throw ParseError("复数不能比较大小，含 i 的关系式请使用等号");
        return result;
    }
    E expression() {
        E e = sum();
        if (peek())
            throw ParseError("根号内的表达式无法解析");
        return e;
    }

  private:
    std::wstring source_;
    size_t at_ = 0;
    bool pipe_ = false, complex_ = false;
    int depth_ = 0, count_ = 0;
    wchar_t peek() {
        while (at_ < source_.size() && std::iswspace(source_[at_]))
            ++at_;
        return at_ < source_.size() ? source_[at_] : 0;
    }
    bool take(wchar_t ch) {
        if (peek() != ch)
            return false;
        ++at_;
        return true;
    }
    E sum() {
        E e = product();
        while (true) {
            if (take(L'+'))
                e = add(e, product());
            else if (take(L'-'))
                e = sub(e, product());
            else
                return e;
        }
    }
    bool starts(wchar_t ch) {
        return ch == L'(' || ch == L'"' || ch == L'π' || ch == L'√' || ch == L'∛' || (ch == L'|' && !pipe_) ||
               ch == L'.' || (ch >= L'0' && ch <= L'9') || (ch >= L'a' && ch <= L'z');
    }
    E product() {
        E e = unary();
        while (true) {
            if (take(L'*'))
                e = mul(e, unary());
            else if (take(L'/')) {
                E denominator = unary();
                domains.push_back({denominator, Op::NotEqual, integer(0)});
                e = divide(e, denominator);
            } else if (starts(peek()))
                e = mul(e, unary());
            else
                return e;
        }
    }
    E unary() {
        if (++depth_ > 128)
            throw ParseError("关系式过长，请缩短输入");
        struct Guard {
            int& depth;
            ~Guard() {
                --depth;
            }
        } guard{depth_};
        if (take(L'+'))
            return unary();
        if (take(L'-'))
            return neg(unary());
        return exponent();
    }
    E exponent() {
        E e = primary();
        while (take(L'!')) {
            domains.push_back({e, Op::Integer, {}});
            domains.push_back({e, Op::GreaterEqual, integer(0)});
            e = function(L"factorial", e);
        }
        E p;
        if (take(L'^'))
            p = unary();
        else if (take(L'²'))
            p = integer(2);
        else if (take(L'³'))
            p = integer(3);
        else if (take(L'⁴'))
            p = integer(4);
        if (!p)
            return e;
        if (p->number) {
            if (p->number->n < 0)
                domains.push_back({e, Op::NotEqual, integer(0)});
            if (!complex_ && p->number->d % 2 == 0)
                domains.push_back({e, Op::GreaterEqual, integer(0)});
        } else if (!complex_)
            domains.push_back({power(e, p), Op::Defined, {}});
        return power(e, p);
    }
    E decimal() {
        std::wstring digits;
        bool dot = false;
        std::int64_t n = 0, d = 1;
        bool exact = true;
        while (true) {
            const wchar_t ch = peek();
            if (ch == L'.' && !dot) {
                dot = true;
                digits += ch;
                ++at_;
                continue;
            }
            if (ch < L'0' || ch > L'9')
                break;
            digits += ch;
            ++at_;
            if (n > (std::numeric_limits<std::int64_t>::max() / 4 - (ch - L'0')) / 10 ||
                (dot && d > std::numeric_limits<std::int64_t>::max() / 40))
                exact = false;
            if (exact) {
                n = n * 10 + ch - L'0';
                if (dot)
                    d *= 10;
            }
        }
        if (digits.empty() || digits == L".")
            throw ParseError("请输入有效数字");
        return exact ? number(Rational(n, d)) : node(Kind::Number, {}, {}, digits);
    }
    E rooted(E argument, int degree) {
        if (!complex_ && degree % 2 == 0)
            domains.push_back({argument, Op::GreaterEqual, integer(0)});
        return root(argument, degree);
    }
    E primary() {
        if (++depth_ > 128 || ++count_ > 2048)
            throw ParseError("关系式过长，请缩短输入");
        struct Guard {
            int& depth;
            ~Guard() {
                --depth;
            }
        } guard{depth_};
        const wchar_t ch = peek();
        if (take(L'(')) {
            E e = sum();
            if (!take(L')'))
                throw ParseError("括号不匹配");
            return e;
        }
        if (take(L'|')) {
            const bool previous = pipe_;
            pipe_ = true;
            E e = sum();
            if (!take(L'|'))
                throw ParseError("绝对值符号不匹配");
            pipe_ = previous;
            return function(L"abs", e);
        }
        if (take(L'"')) {
            const size_t closing = source_.find(L'"', at_);
            if (closing == std::wstring::npos)
                throw ParseError("根号引号不匹配");
            std::wstring body = source_.substr(at_, closing - at_);
            at_ = closing + 1;
            int degree = 2;
            const size_t next = source_.find(L'"', at_);
            if (!body.empty() && body.size() <= 2 &&
                body.find_first_not_of(L"0123456789") == std::wstring::npos && next != std::wstring::npos &&
                source_.substr(at_, next - at_).find_first_of(L"=<>≤≥") == std::wstring::npos) {
                degree = std::stoi(body);
                if (degree < 2 || degree > 64)
                    throw ParseError("根指数须为 2 至 64 的整数");
                body = source_.substr(at_, next - at_);
                at_ = next + 1;
            }
            Reader inside(body, complex_);
            E e = inside.expression();
            domains.insert(domains.end(), inside.domains.begin(), inside.domains.end());
            return rooted(e, degree);
        }
        if (take(L'√'))
            return rooted(primary(), 2);
        if (take(L'∛'))
            return rooted(primary(), 3);
        if ((ch >= L'0' && ch <= L'9') || ch == L'.')
            return decimal();
        if (take(L'π'))
            return symbol(L"π");
        if (ch >= L'a' && ch <= L'z') {
            for (const auto* name : {L"factorial", L"sqrt", L"cbrt", L"asin", L"acos", L"atan", L"acot",
                                     L"exp", L"log", L"sin", L"cos", L"tan", L"cot", L"abs", L"ln"}) {
                const std::wstring word = name;
                if (source_.compare(at_, word.size(), word) != 0)
                    continue;
                size_t after = at_ + word.size();
                while (after < source_.size() && std::iswspace(source_[after]))
                    ++after;
                if (after >= source_.size() || source_[after] != L'(')
                    continue;
                at_ = after + 1;
                E e = sum();
                if (!take(L')'))
                    throw ParseError("括号不匹配");
                if (word == L"sqrt" || word == L"cbrt")
                    return rooted(e, word == L"sqrt" ? 2 : 3);
                if (!complex_) {
                    if (word == L"ln" || word == L"log")
                        domains.push_back({e, Op::Greater, integer(0)});
                    if (word == L"asin" || word == L"acos") {
                        domains.push_back({e, Op::GreaterEqual, integer(-1)});
                        domains.push_back({e, Op::LessEqual, integer(1)});
                    }
                }
                if (complex_ && (word == L"ln" || word == L"log"))
                    domains.push_back({e, Op::NotEqual, integer(0)});
                if (word == L"tan" || word == L"cot")
                    domains.push_back(
                        {function(word == L"tan" ? L"cos" : L"sin", e), Op::NotEqual, integer(0)});
                if (word == L"factorial") {
                    domains.push_back({e, Op::Integer, {}});
                    domains.push_back({e, Op::GreaterEqual, integer(0)});
                }
                return function(word, e);
            }
            if (source_.compare(at_, 2, L"pi") == 0) {
                at_ += 2;
                return symbol(L"π");
            }
            ++at_;
            if (ch == L'x' || ch == L'y' || ch == L'z' || ch == L'e' || ch == L'i')
                return symbol(std::wstring(1, ch));
        }
        throw ParseError("关系式中包含无法识别的表达式");
    }
};

// 把有理式保留为分子/分母；所有原始分母条件在 Reader::domains 中独立保存。
struct Fraction {
    E n, d;
};
Fraction fraction(E e) {
    if (e->kind == Kind::Add) {
        auto a = fraction(e->a), b = fraction(e->b);
        return {add(mul(a.n, b.d), mul(b.n, a.d)), mul(a.d, b.d)};
    }
    if (e->kind == Kind::Negate) {
        auto a = fraction(e->a);
        return {neg(a.n), a.d};
    }
    if (e->kind == Kind::Multiply || e->kind == Kind::Divide) {
        auto a = fraction(e->a), b = fraction(e->b);
        if (e->kind == Kind::Divide)
            std::swap(b.n, b.d);
        return {mul(a.n, b.n), mul(a.d, b.d)};
    }
    return {e, integer(1)};
}
using Polynomial = std::vector<E>;
void trim(Polynomial& p) {
    while (p.size() > 1 && is(p.back(), 0))
        p.pop_back();
}
std::optional<Polynomial> polynomial(E e, wchar_t target) {
    if (!contains(e, target))
        return Polynomial{e};
    if (e->kind == Kind::Symbol)
        return Polynomial{integer(0), integer(1)};
    if (e->kind == Kind::Negate) {
        auto p = polynomial(e->a, target);
        if (!p)
            return {};
        for (auto& value : *p)
            value = neg(value);
        return p;
    }
    if (e->kind == Kind::Add || e->kind == Kind::Multiply) {
        auto a = polynomial(e->a, target), b = polynomial(e->b, target);
        if (!a || !b)
            return {};
        Polynomial result(e->kind == Kind::Add ? std::max(a->size(), b->size()) : a->size() + b->size() - 1,
                          integer(0));
        if (result.size() > 5)
            return {};
        if (e->kind == Kind::Add) {
            for (size_t i = 0; i < a->size(); ++i)
                result[i] = add(result[i], (*a)[i]);
            for (size_t i = 0; i < b->size(); ++i)
                result[i] = add(result[i], (*b)[i]);
        } else {
            for (size_t i = 0; i < a->size(); ++i)
                for (size_t j = 0; j < b->size(); ++j)
                    result[i + j] = add(result[i + j], mul((*a)[i], (*b)[j]));
        }
        trim(result);
        return result;
    }
    if (e->kind == Kind::Divide && !contains(e->b, target)) {
        auto p = polynomial(e->a, target);
        if (!p)
            return {};
        for (auto& value : *p)
            value = divide(value, e->b);
        return p;
    }
    if (e->kind == Kind::Power && e->b->number && e->b->number->d == 1 && e->b->number->n >= 0 &&
        e->b->number->n <= 4) {
        E expanded = integer(1);
        for (int i = 0; i < e->b->number->n; ++i)
            expanded = mul(expanded, e->a);
        return polynomial(expanded, target);
    }
    return {};
}

struct Case {
    E value;
    std::vector<Condition> conditions;
    bool any = false;
};
using Cases = std::vector<Case>;
void append(Cases& destination, Cases values, const std::vector<Condition>& extra = {}) {
    for (auto& value : values) {
        value.conditions.insert(value.conditions.end(), extra.begin(), extra.end());
        if (simplifyConditions(value.conditions))
            destination.push_back(std::move(value));
    }
}

Cases polynomialRoots(Polynomial p, bool complex) {
    trim(p);
    if (p.size() == 1)
        return {{{}, {{p[0], Op::Equal, integer(0)}}, true}};
    if (p.size() > 3)
        return {};
    const E a = p.back();
    Cases result;
    if (p.size() == 2) {
        append(result, {{divide(neg(p[0]), a), {}, false}}, {{a, Op::NotEqual, integer(0)}});
    } else {
        E delta = sub(power(p[1], integer(2)), mul(integer(4), mul(a, p[0])));
        std::vector<Condition> conditions{{a, Op::NotEqual, integer(0)}};
        if (!complex)
            conditions.push_back({delta, Op::GreaterEqual, integer(0)});
        if (is(p[1], 0)) {
            E radicand = divide(neg(p[0]), a);
            conditions = {{a, Op::NotEqual, integer(0)}};
            if (!complex)
                conditions.push_back({radicand, Op::GreaterEqual, integer(0)});
            append(result, {{root(radicand, 2), {}, false}, {neg(root(radicand, 2)), {}, false}}, conditions);
        } else {
            E divisor = mul(integer(2), a);
            append(result,
                   {{divide(add(neg(p[1]), root(delta, 2)), divisor), {}, false},
                    {divide(sub(neg(p[1]), root(delta, 2)), divisor), {}, false}},
                   conditions);
        }
    }
    p.pop_back();
    append(result, polynomialRoots(p, complex), {{a, Op::Equal, integer(0)}});
    return result;
}

class Isolator {
  public:
    Isolator(wchar_t target, AngleUnit unit, bool complex)
        : target_(target), unit_(unit), complex_(complex) {}
    std::optional<Cases> solve(E left, E right, int depth = 0) {
        if (depth > 32 || ++visits_ > 128)
            return {};
        bool l = contains(left, target_), r = contains(right, target_);
        if (!l && r) {
            std::swap(left, right);
            std::swap(l, r);
        }
        if (!l && !r)
            return Cases{{{}, {{left, Op::Equal, right}}, true}};
        if (left->kind == Kind::Symbol && l && !r)
            return Cases{{right, {}, false}};
        if (!r) {
            const bool a = left->a && contains(left->a, target_), b = left->b && contains(left->b, target_);
            if (left->kind == Kind::Negate)
                return solve(left->a, neg(right), depth + 1);
            if (left->kind == Kind::Add && a != b)
                return solve(a ? left->a : left->b, sub(right, a ? left->b : left->a), depth + 1);
            if (left->kind == Kind::Multiply && a != b) {
                E factor = a ? left->b : left->a;
                auto normal = solve(a ? left->a : left->b, divide(right, factor), depth + 1);
                if (!normal)
                    return {};
                Cases result;
                append(result, *normal, {{factor, Op::NotEqual, integer(0)}});
                append(result, {{{}, {}, true}},
                       {{factor, Op::Equal, integer(0)}, {right, Op::Equal, integer(0)}});
                return result;
            }
            if (left->kind == Kind::Divide && a != b) {
                if (a)
                    return solve(left->a, mul(right, left->b), depth + 1);
                auto normal = solve(left->b, divide(left->a, right), depth + 1);
                if (!normal)
                    return {};
                Cases result;
                append(result, *normal, {{right, Op::NotEqual, integer(0)}});
                append(result, {{{}, {}, true}},
                       {{right, Op::Equal, integer(0)}, {left->a, Op::Equal, integer(0)}});
                return result;
            }
            if (left->kind == Kind::Root && a) {
                if (complex_)
                    return {}; // 复数主值根的值域不能用实数的平方/立方规则替代。
                const int degree = static_cast<int>(left->b->number->n);
                auto result = solve(left->a, power(right, integer(degree)), depth + 1);
                if (result && !complex_ && degree % 2 == 0)
                    for (auto& value : *result)
                        value.conditions.push_back({right, Op::GreaterEqual, integer(0)});
                return result;
            }
            if (left->kind == Kind::Power && a && !b && left->b->number && !complex_) {
                const Rational exponent = *left->b->number;
                if (!exponent.n)
                    return Cases{{{}, {{right, Op::Equal, integer(1)}}, true}};
                if (std::abs(exponent.n) > 64 || exponent.d > 64)
                    return {};
                E desired = exponent.n < 0 ? divide(integer(1), right) : right;
                const int magnitude = static_cast<int>(std::abs(exponent.n));
                E candidate = power(root(desired, magnitude), integer(exponent.d));
                std::vector<Condition> conditions;
                if (exponent.n < 0)
                    conditions.push_back({right, Op::NotEqual, integer(0)});
                if (magnitude % 2 == 0 || exponent.d % 2 == 0)
                    conditions.push_back({right, Op::GreaterEqual, integer(0)});
                auto one = solve(left->a, candidate, depth + 1);
                if (!one)
                    return {};
                Cases result;
                append(result, *one, conditions);
                if (magnitude % 2 == 0 && exponent.d % 2 != 0) {
                    auto two = solve(left->a, neg(candidate), depth + 1);
                    if (!two)
                        return {};
                    append(result, *two, conditions);
                }
                return result;
            }
            if (left->kind == Kind::Power && !a && b && !complex_) {
                const bool knownBase =
                    (left->a->number && left->a->number->n > 0 && !is(left->a, 1)) ||
                    (left->a->kind == Kind::Symbol && (left->a->name == L"e" || left->a->name == L"π"));
                if (knownBase) {
                    auto result =
                        solve(left->b, divide(function(L"ln", right), function(L"ln", left->a)), depth + 1);
                    if (result)
                        for (auto& value : *result)
                            value.conditions.push_back({right, Op::Greater, integer(0)});
                    return result;
                }
            }
            if (left->kind == Kind::Function && a && !complex_) {
                const auto& name = left->name;
                if (name == L"abs") {
                    auto plus = solve(left->a, right, depth + 1),
                         minus = solve(left->a, neg(right), depth + 1);
                    if (!plus || !minus)
                        return {};
                    Cases result;
                    append(result, *plus, {{right, Op::GreaterEqual, integer(0)}});
                    append(result, *minus, {{right, Op::GreaterEqual, integer(0)}});
                    return result;
                }
                if (name == L"ln" || name == L"log")
                    return solve(left->a, power(name == L"ln" ? symbol(L"e") : integer(10), right),
                                 depth + 1);
                if (name == L"exp") {
                    auto result = solve(left->a, function(L"ln", right), depth + 1);
                    if (result)
                        for (auto& value : *result)
                            value.conditions.push_back({right, Op::Greater, integer(0)});
                    return result;
                }
                E pi = unit_ == AngleUnit::Degrees ? integer(180) : symbol(L"π");
                E period = mul(symbol(L"κ"), name == L"sin" || name == L"cos" ? mul(integer(2), pi) : pi);
                const auto inverse = [&](const std::wstring& f) {
                    E result = function(f, right);
                    return unit_ == AngleUnit::Degrees ? mul(divide(integer(180), symbol(L"π")), result)
                                                       : result;
                };
                std::vector<E> angles;
                std::vector<Condition> conditions{{symbol(L"κ"), Op::Integer, {}}};
                if (name == L"sin") {
                    angles = {add(inverse(L"asin"), period), add(sub(pi, inverse(L"asin")), period)};
                } else if (name == L"cos") {
                    angles = {add(inverse(L"acos"), period), add(neg(inverse(L"acos")), period)};
                } else if (name == L"tan")
                    angles = {add(inverse(L"atan"), period)};
                else if (name == L"cot")
                    angles = {add(sub(divide(pi, integer(2)), inverse(L"atan")), period)};
                if (!angles.empty()) {
                    if (name == L"sin" || name == L"cos") {
                        conditions.push_back({right, Op::GreaterEqual, integer(-1)});
                        conditions.push_back({right, Op::LessEqual, integer(1)});
                    }
                    Cases result;
                    for (E angle : angles) {
                        auto values = solve(left->a, angle, depth + 1);
                        if (!values)
                            return {};
                        append(result, *values, conditions);
                    }
                    return result;
                }
                if (name == L"asin" || name == L"acos" || name == L"atan" || name == L"acot") {
                    // 反三角函数必须保留主值范围，否则反解会引入多余解。
                    E boundary = divide(symbol(L"π"), integer(2));
                    std::vector<Condition> range;
                    if (name == L"acos" || name == L"acot")
                        range = {{right, name == L"acos" ? Op::GreaterEqual : Op::Greater, integer(0)},
                                 {right, name == L"acos" ? Op::LessEqual : Op::Less, symbol(L"π")}};
                    else
                        range = {{right, name == L"asin" ? Op::GreaterEqual : Op::Greater, neg(boundary)},
                                 {right, name == L"asin" ? Op::LessEqual : Op::Less, boundary}};
                    E forwardAngle =
                        unit_ == AngleUnit::Degrees ? mul(divide(integer(180), symbol(L"π")), right) : right;
                    auto values = solve(left->a, function(name.substr(1), forwardAngle), depth + 1);
                    if (!values)
                        return {};
                    Cases result;
                    append(result, *values, range);
                    return result;
                }
            }
        }
        const auto f = fraction(sub(left, right));
        auto p = polynomial(f.n, target_);
        if (p && p->size() <= 3)
            return polynomialRoots(*p, complex_);
        // 双二次式以 t=变量² 求解，保留外层平方根及实数条件。
        if (p && p->size() == 5 && is((*p)[1], 0) && is((*p)[3], 0) && !complex_) {
            Cases result;
            for (const auto& t : polynomialRoots({(*p)[0], (*p)[2], (*p)[4]}, false)) {
                if (t.any)
                    return {};
                std::vector<Condition> conditions = t.conditions;
                conditions.push_back({t.value, Op::GreaterEqual, integer(0)});
                append(result, {{root(t.value, 2), {}, false}, {neg(root(t.value, 2)), {}, false}},
                       conditions);
            }
            return result;
        }
        return {};
    }

  private:
    wchar_t target_;
    AngleUnit unit_;
    bool complex_;
    int visits_ = 0;
};

std::vector<std::wstring> linesOf(const std::wstring& input) {
    std::vector<std::wstring> result;
    std::wstring current;
    for (wchar_t ch : input) {
        if (ch == L'\n' || ch == L';' || ch == L'；') {
            if (current.find_first_not_of(L" \t\r") != std::wstring::npos)
                result.push_back(current);
            current.clear();
        } else
            current += ch;
    }
    if (current.find_first_not_of(L" \t\r") != std::wstring::npos)
        result.push_back(current);
    return result;
}
std::wstring constraintsText(const std::vector<Condition>& conditions) {
    std::wstring result;
    for (const auto& condition : conditions) {
        if (!result.empty())
            result += L"，";
        result += text(condition);
    }
    return result;
}

std::wstring implicit(wchar_t target, const std::vector<Equation>& equations, std::vector<Condition> domains,
                      bool complex) {
    E placeholder = symbol(L"λ");
    for (const auto& equation : equations)
        domains.push_back({replace(equation.a, target, placeholder), equation.op,
                           replace(equation.b, target, placeholder)});
    for (auto& condition : domains) {
        condition.a = replace(condition.a, target, placeholder);
        condition.b = replace(condition.b, target, placeholder);
    }
    if (!simplifyConditions(domains))
        return std::wstring(1, target) + L" ∈ ∅";
    return std::wstring(1, target) + L" ∈ {λ ∈ " + (complex ? L"ℂ" : L"ℝ") + L" | " +
           constraintsText(domains) + L"}（隐式解集）";
}

std::optional<std::wstring> inequality(wchar_t target, const Equation& equation,
                                       const std::vector<Condition>& domains) {
    // 有理不等式不能直接乘含字母的分母；保留为隐式解集，避免漏掉符号分支。
    E difference = sub(equation.a, equation.b);
    if (fraction(difference).d && !is(fraction(difference).d, 1))
        return {};
    auto p = polynomial(difference, target);
    if (!p || p->size() > 2)
        return {};
    E a = p->size() == 2 ? (*p)[1] : integer(0), b = (*p)[0];
    std::wstring result;
    auto emit = [&](std::wstring answer, std::vector<Condition> conditions) {
        conditions.insert(conditions.end(), domains.begin(), domains.end());
        if (!simplifyConditions(conditions))
            return;
        if (!result.empty())
            result += L"；";
        result += answer;
        if (!conditions.empty())
            result += L"（" + constraintsText(conditions) + L"）";
    };
    E value = divide(neg(b), a);
    emit(std::wstring(1, target) + opText(equation.op) + text(value), {{a, Op::Greater, integer(0)}});
    emit(std::wstring(1, target) + opText(reversed(equation.op)) + text(value), {{a, Op::Less, integer(0)}});
    emit(std::wstring(1, target) + L" ∈ ℝ", {{a, Op::Equal, integer(0)}, {b, equation.op, integer(0)}});
    return result.empty() ? std::wstring(1, target) + L" ∈ ∅" : result;
}

} // namespace

SolveResult solveGeneralRelation(const std::wstring& canonicalInput, int variableCount, AngleUnit unit) {
    const auto lines = linesOf(canonicalInput);
    // 单字母题目和可确定的方程组继续使用原求解器的完整精确算法，仅去掉过程。
    if (variableCount <= 1 || (lines.size() > 1 && lines.size() == static_cast<size_t>(variableCount))) {
        try {
            auto numerical = solveText(canonicalInput, InputKind::Auto, unit);
            if (!numerical.isRelation) {
                numerical.process.clear();
                numerical.isRelation = true;
                return numerical;
            }
        } catch (const ParseError&) { /* 交给符号解析器；保持已输入关系的精确意义。 */
        }
    }
    bool complex = false;
    std::vector<Equation> equations;
    std::vector<Condition> domains;
    for (const auto& line : lines) {
        Reader reader(line, complex);
        equations.push_back(reader.equation());
        domains.insert(domains.end(), reader.domains.begin(), reader.domains.end());
        complex = complex || hasSymbol(equations.back().a, L"i") || hasSymbol(equations.back().b, L"i");
    }
    if (complex) {
        equations.clear();
        domains.clear();
        for (const auto& line : lines) {
            Reader reader(line, true);
            equations.push_back(reader.equation());
            domains.insert(domains.end(), reader.domains.begin(), reader.domains.end());
        }
    }
    if (equations.empty())
        throw ParseError("请输入需要求解的关系式");
    SolveResult result;
    result.isRelation = true;
    result.usesImaginaryUnit = complex;
    if (!simplifyConditions(domains)) {
        result.summary = L"结论：定义域为空，无解";
        return result;
    }
    if (!variableCount) {
        std::vector<Condition> conditions = domains;
        for (const auto& equation : equations)
            conditions.push_back({equation.a, equation.op, equation.b});
        result.summary = simplifyConditions(conditions) ? L"结论：等式恒成立" : L"结论：方程无解";
        if (!conditions.empty())
            result.summary = constraintsText(conditions);
        return result;
    }
    for (int index = 0; index < variableCount; ++index) {
        const wchar_t target = L'x' + index;
        std::wstring answer;
        size_t selected = 0;
        while (selected < equations.size() && !contains(equations[selected].a, target) &&
               !contains(equations[selected].b, target))
            ++selected;
        if (selected == equations.size())
            selected = 0;
        const Equation& equation = equations[selected];
        if (equations.size() == 1 && equation.op != Op::Equal && !complex)
            answer = inequality(target, equation, domains).value_or(L"");
        else if (equation.op == Op::Equal) {
            auto cases = Isolator(target, unit, complex).solve(equation.a, equation.b);
            if (cases && cases->size() <= 12) {
                std::set<std::wstring> seen;
                for (auto& item : *cases) {
                    E replacement = item.any ? symbol(std::wstring(1, target)) : item.value;
                    auto conditions = item.conditions;
                    conditions.insert(conditions.end(), domains.begin(), domains.end());
                    for (size_t row = 0; row < equations.size(); ++row)
                        if (row != selected)
                            conditions.push_back({equations[row].a, equations[row].op, equations[row].b});
                    for (auto& condition : conditions) {
                        condition.a = replace(condition.a, target, replacement);
                        condition.b = replace(condition.b, target, replacement);
                    }
                    if (!simplifyConditions(conditions))
                        continue;
                    std::wstring entry = std::wstring(1, target) + (item.any ? (complex ? L" ∈ ℂ" : L" ∈ ℝ")
                                                                             : L" = " + text(item.value));
                    if (!conditions.empty())
                        entry += L"（" + constraintsText(conditions) + L"）";
                    if (seen.insert(entry).second) {
                        if (!answer.empty())
                            answer += L"；";
                        answer += entry;
                    }
                }
                if (answer.empty())
                    answer = std::wstring(1, target) + L" ∈ ∅";
            }
        }
        // 通用组合关系或过长的分支使用精确隐式解集；没有伪造显式公式。
        if (answer.empty() || answer.size() > 520)
            answer = implicit(target, equations, domains, complex);
        if (!result.summary.empty())
            result.summary += L"\r\n";
        result.summary += answer;
    }
    // 多个字母的分支太长时，用一个联合解集保留全部条件，避免重复原式三次。
    if (result.summary.size() > 720) {
        const wchar_t placeholders[] = {L'λ', L'μ', L'ν'};
        std::wstring names, placeholdersText;
        std::vector<Condition> joint = domains;
        for (const auto& equation : equations)
            joint.push_back({equation.a, equation.op, equation.b});
        for (int index = 0; index < variableCount; ++index) {
            if (index) {
                names += L", ";
                placeholdersText += L", ";
            }
            names += static_cast<wchar_t>(L'x' + index);
            placeholdersText += placeholders[index];
            for (auto& condition : joint) {
                condition.a = replace(condition.a, static_cast<wchar_t>(L'x' + index),
                                      symbol(std::wstring(1, placeholders[index])));
                condition.b = replace(condition.b, static_cast<wchar_t>(L'x' + index),
                                      symbol(std::wstring(1, placeholders[index])));
            }
        }
        if (!simplifyConditions(joint))
            result.summary = L"结论：方程无解";
        else
            result.summary = L"(" + names + L") ∈ {(" + placeholdersText + L") ∈ " + (complex ? L"ℂ" : L"ℝ") +
                             L"^" + std::to_wstring(variableCount) + L" | " + constraintsText(joint) +
                             L"}（隐式解集）";
    }
    return result;
}

} // namespace equation
