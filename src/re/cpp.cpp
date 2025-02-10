#include <re/re.hpp>

#include <algorithm>
#include <iostream>

#include <ctre.hpp>
using namespace ctre::literals;

template <size_t N>
using fs_t = ctll::fixed_string<N>; // we can use `fs_t` instead of `ctll::fixed_string` only with clang++-19 or g++-11

// --------------------------------------------------------------------------------
template <ctll::fixed_string... Strings>
constexpr auto fs_concat() noexcept {
   constexpr std::size_t     len = (Strings.size() + ...);
   std::array<char32_t, len> arr{};
   std::size_t               i      = 0;
   auto                      append = [&](auto const& s) mutable {
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
[[maybe_unused]] static constexpr auto function_call = fs_concat<namespaced_id, call_params>();

// Parenthesized expressions: (x + y)
// ----------------------------------
static constexpr auto parenthesized = ctll::fixed_string{R"(\(([^(),]+)\))"};

// Variable names and member access: foo, foo.bar, foo->bar, ns::foo, ns::foo->bar
// -------------------------------------------------------------------------------
static constexpr auto variables = fs_concat<namespaced_id, dereference>();

// Array access: arr[idx] or ns::arr[idx]
// --------------------------------------
static constexpr auto array_access = fs_concat<variables, bracket_op>();

// Basic binary expressions: a + b, x * y, ns::x + y etc.
// ------------------------------------------------------
static constexpr auto oper        = ctll::fixed_string{R"(\s*[+\-*/%&|^]\s*)"};
static constexpr auto binary_expr = fs_concat<namespaced_id, oper, namespaced_id>();

// Spaces and open parenthesis
// ---------------------------
static constexpr auto spaces_par = ctll::fixed_string{R"(\s*\()"};

// --------------------------------------------------------------------------------
template <ctll::fixed_string pattern, size_t capture_index>
void collect_matches(std::vector<std::string_view>& expressions, const std::string_view& code) {
   for (auto match : ctre::search_all<pattern>(code)) {
      expressions.push_back(match.template get<capture_index>());
   }
}

// --------------------------------------------------------------------------------
std::vector<std::string_view> regexp::cpp::extract_debuggable_expressions(std::string_view code, expr_flags e) const {
   std::vector<std::string_view> expressions;
   expressions.reserve(32);

   // Find all matches for each pattern
   // ---------------------------------
   if (e & allow_function_calls)
      collect_matches<function_call, 0>(expressions, code); // don't execute function calls which may have side effects
   collect_matches<parenthesized, 1>(expressions, code);
   collect_matches<variables, 0>(expressions, code);
   collect_matches<array_access, 0>(expressions, code);
   collect_matches<binary_expr, 0>(expressions, code);

   // remove duplicates
   // -----------------
   std::sort(expressions.begin(), expressions.end());
   auto newEnd = std::unique(expressions.begin(), expressions.end());
   expressions.erase(newEnd, expressions.end());

   // sort by size
   // ------------
   std::stable_sort(expressions.begin(), expressions.end(),
                    [](std::string_view a, std::string_view b) { return a.size() < b.size(); });

   return expressions;
}

// --------------------------------------------------------------------------------
std::optional<regexp::bounds> regexp::cpp::find_symbol_at_pos(std::string_view code, size_t pos) const {
   bool match_ident = false;
   std::vector<std::string_view> matches;
   matches.reserve(4);
   auto c = code[pos];
   if (c == '(' || c == ')') {
      collect_matches<function_call, 0>(matches, code);
      collect_matches<parenthesized, 0>(matches, code);
   } else  if (c == '[' || c == ']') {
      collect_matches<array_access, 0>(matches, code);
   } else {
      match_ident = true;
      collect_matches<variables, 0>(matches, code);
   }
   for (const auto& v : matches) {
      size_t start = v.data() - code.data();
      if (start <= pos && start + v.length() >= pos) {
         auto rest = code.substr(start + v.length());
         if (match_ident && static_cast<bool>(ctre::starts_with<spaces_par>(rest))) {
            // if we parsed an variable reference followed by an open parenthesis, it is likely a function call
            auto par = ctre::match<spaces_par>(rest);
            return find_symbol_at_pos(code, start + v.length() + par.get<0>().size()); // will parse function calls
         }
         return regexp::bounds{ start, start + v.length() };
      }
   }
   return {};
}

#if 0
// compile with: `cd ../src; g++-12 -std=c++20 -I../deps/include regex.cpp`
// ------------------------------------------------------------------------
int main() {
    std::string code = "aa::bb::result = aa::foo::bar(x + y) + a::b::c->x + arr[idx]->member.value * 2;";
    auto expressions = regex::extract_debuggable_expressions(code);

    for (const auto& expr : expressions) {
        std::cout << "Debuggable expression: " << expr << '\n';
    }

    std::string input;
    while (true) {
        std::cout << "Enter text (or 'quit' to exit): ";
        getline(std::cin, input);
        if (input == "quit") 
            break;

        std::cout << "match: " << regex::match_stack_or_breakpoint_output(input) << '\n';
    }

    return 0;
}
#endif