#include <re/re.hpp>

#include <algorithm>
#include <iostream>

#include <ctre.hpp>
using namespace ctre::literals;

// --------------------------------------------------------------------------------
bool regexp::gdb::match_stack_or_breakpoint_output(std::string_view s) constc {
   return static_cast<bool>(ctre::starts_with<"(Num|#[0-9]+)[ ]+">(s));
}

// --------------------------------------------------------------------------------
bool regexp::gdb::evaluation_error(std::string_view s) const {
   return static_cast<bool>(ctre::starts_with<"(A syntax error|No symbol|Attempt to|cannot resolve)">(s));
}
