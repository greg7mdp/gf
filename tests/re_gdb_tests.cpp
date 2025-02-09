#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include <re/re.hpp>
#include <re/gdb.cpp>  // include cpp file as not a library

namespace rng   = std::ranges;
namespace views = rng::views;

regexp::gdb gdb;

TEST_CASE("match_stack_or_breakpoint_output") {
   CHECK(gdb.match_stack_or_breakpoint_output("Num "));
   CHECK(gdb.match_stack_or_breakpoint_output("#2 "));
}
