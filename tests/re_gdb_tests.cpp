#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include <re/re.hpp>
#include <re/gdb.cpp>  // include cpp file as not a library
#include <ranges>

namespace rng   = std::ranges;
namespace views = rng::views;

regexp::gdb_impl gdb;

TEST_CASE("match_stack_or_breakpoint_output") {
   CHECK(gdb.matches("Num ", debug_look_for::stack_or_breakpoint_output));
   CHECK(gdb.matches("#2 ", debug_look_for::stack_or_breakpoint_output));
}
