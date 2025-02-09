#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include <re/re.hpp>
#include <re/cpp.cpp>

namespace rng   = std::ranges;
namespace views = rng::views;

regexp::cpp cpp;

TEST_CASE("extract_debuggable_expressions") {

   std::string code = "aa::bb::result = aa::foo::bar(x + y) + a::b::c->x + arr[idx]->member.value * 2;";
   auto e = cpp.extract_debuggable_expressions(code);

   for (auto expected : { "x", "y", "arr", "arr[idx]", "idx", "a::b::c->x", "aa::bb::result" }) {
      CHECK(std::ranges::any_of(e, [&](auto sv){ return sv == expected; }));
   };

   for (const auto& expr : e) {
      std::cout << "Debuggable expression: " << expr << '\n';
   }
}
