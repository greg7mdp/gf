#include <re/re.hpp>

#include <algorithm>
#include <iostream>
#include <cassert>

#include <ctre.hpp>
using namespace ctre::literals;
using namespace regexp;

// --------------------------------------------------------------------------------
bool gdb_impl::matches(std::string_view s, debug_look_for lf) const {
   switch(lf) {
   case debug_look_for::stack_or_breakpoint_output:
      return static_cast<bool>(ctre::starts_with<"(Num|#[0-9]+)[ ]+">(s));

   case debug_look_for::evaluation_error:
      return static_cast<bool>(ctre::starts_with<"(A syntax error|No symbol|Attempt to|cannot resolve)">(s));
      
   default:
      assert(0);
   }
   return false;
}

// --------------------------------------------------------------------------------
one_res gdb_impl::find_1(std::string_view sv, debug_look_for lf) const {
   switch(lf) {

   default:
      assert(0);
   }
   return {};
}