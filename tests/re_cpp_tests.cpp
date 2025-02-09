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

   CHECK(std::ranges::any_of(e, [](auto sv){ return sv == "x"; }));
   CHECK(std::ranges::any_of(e, [](auto sv){ return sv == "y"; }));
   CHECK(std::ranges::any_of(e, [](auto sv){ return sv == "arr"; }));
   CHECK(std::ranges::any_of(e, [](auto sv){ return sv == "idx"; }));
   CHECK(std::ranges::any_of(e, [](auto sv){ return sv == "a::b::c->x"; }));
   CHECK(std::ranges::any_of(e, [](auto sv){ return sv == "aa::bb::result"; }));
   for (const auto& expr : e) {
      std::cout << "Debuggable expression: " << expr << '\n';
   }
}
