#include "re.hpp"

#include <algorithm>
#include <iostream>

#include <ctre.hpp>
using namespace ctre::literals;

// --------------------------------------------------------------------------------
bool regexp::gdb::match_stack_or_breakpoint_output(std::string_view s) {
   return static_cast<bool>(ctre::starts_with<"(Num|#[0-9]+)[ ]+">(s));
}
