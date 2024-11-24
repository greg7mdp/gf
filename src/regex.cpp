#include <string>
#include <string_view>
#include <vector>
#include <set>
#include <iostream>

#include <ctre.hpp>

// --------------------------------------------------------------------------------
template <ctll::fixed_string... Strings>
constexpr auto fs_concat() noexcept {
   constexpr std::size_t len = (Strings.size() + ...);
   std::array<char32_t, len> arr{};
   std::size_t i = 0;
   auto append = [&](auto const& s) mutable {
      for (auto c : s)
         arr[i++] = c;
   };
   (append(Strings), ...);

   return ctll::fixed_string<len>(arr);
}

// --------------------------------------------------------------------------------
template <ctll::fixed_string fs>
constexpr auto fs_repeat() noexcept {
   return fs_concat<"(", fs, ")*">();
}

// --------------------------------------------------------------------------------
std::set<std::string_view> extract_debuggable_expressions(std::string_view code) {
    std::set<std::string_view> expressions;

    // Basic patterns
    // --------------
    static constexpr auto identifier  = ctll::fixed_string{"[a-zA-Z_][a-zA-Z0-9_]*"};
    static constexpr auto ns_delim    = ctll::fixed_string{"::"};
    static constexpr auto call_params = ctll::fixed_string{R"(\s*\([^()]*\))"};
    static constexpr auto bracket_op  = ctll::fixed_string{R"(\s*\[[^\[\]]+\])"};
    static constexpr auto dereference = ctll::fixed_string{R"((?:(?:\.|\->)[a-zA-Z0-9_]*)*)"};

    // Namespaced identifier pattern (includes non-namespaced as well)
    // ---------------------------------------------------------------
    static constexpr auto namespaced_id = fs_concat<identifier, fs_repeat<fs_concat<ns_delim, identifier>()>()>();

    // Function calls with parameters: foo(arg1, arg2) or ns::foo(arg1, arg2)
    // ----------------------------------------------------------------------
    static constexpr auto function_call = fs_concat<namespaced_id, call_params>();

    // Parenthesized expressions: (x + y)
    // ----------------------------------
    static constexpr auto parenthesized = ctll::fixed_string{
        R"(\(([^()]+)\))"
    };

    // Variable names and member access: foo, foo.bar, foo->bar, ns::foo, ns::foo->bar
    // -------------------------------------------------------------------------------
    static constexpr auto variables = fs_concat<namespaced_id, dereference>();

    // Array access: arr[idx] or ns::arr[idx]
    // --------------------------------------
    static constexpr auto array_access = fs_concat<namespaced_id, bracket_op>();

    // Basic binary expressions: a + b, x * y, ns::x + y etc.
    // ------------------------------------------------------
    static constexpr auto oper        = ctll::fixed_string{R"(\s*[+\-*/%&|^]\s*)"};
    static constexpr auto binary_expr = fs_concat<namespaced_id, oper, namespaced_id>();

    // Find all matches for each pattern
    // ---------------------------------
    for (auto match : ctre::search_all<function_call>(code)) {
        expressions.insert(match.get<0>());
    }

    for (auto match : ctre::search_all<parenthesized>(code)) {
        expressions.insert(match.get<1>());
    }

    for (auto match : ctre::search_all<variables>(code)) {
        expressions.insert(match.get<0>());
    }

    for (auto match : ctre::search_all<array_access>(code)) {
       expressions.insert(match.get<0>());
    }

    for (auto match : ctre::search_all<binary_expr>(code)) {
        expressions.insert(match.get<0>());
    }

    return expressions;
}

#if 0
int main() {
    std::string code = "aa::bb::result = aa::foo(x + y) + a::b->x + arr[idx]->member.value * 2;";
    auto expressions = extract_debuggable_expressions(code);

    for (const auto& expr : expressions) {
        std::cout << "Debuggable expression: " << expr << '\n';
    }
}
#endif