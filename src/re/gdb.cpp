#include <re/re.hpp>

#include <algorithm>
#include <iostream>
#include <cassert>

#include <ctre.hpp>
using namespace ctre::literals;
using namespace regexp;

// --------------------------------------------------------------------------------
bool gdb_impl::matches(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   switch(lf) {
   case debug_look_for::stack_or_breakpoint_output:
      return static_cast<bool>(ctre::starts_with<"(Num|#[0-9]+)[ ]+">(s.substr(pos)));

   case debug_look_for::evaluation_error:
      return static_cast<bool>(ctre::starts_with<"(A syntax error|No symbol|Attempt to|cannot resolve)">(s.substr(pos)));
      
   default:
      assert(0);
   }
   return false;
}

// --------------------------------------------------------------------------------
one_res gdb_impl::find_1(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   switch(lf) {

   default:
      assert(0);
   }
   return {};
}

// --------------------------------------------------------------------------------
two_res gdb_impl::find_2(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   switch(lf) {

   default:
      assert(0);
   }
   return {};
}

// --------------------------------------------------------------------------------
three_res gdb_impl::find_3(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   switch(lf) {

   default:
      assert(0);
   }
   return {};
}

// --------------------------------------------------------------------------------
four_res gdb_impl::find_4(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   switch(lf) {

   default:
      assert(0);
   }
   return {};
}

// --------------------------------------------------------------------------------
ones gdb_impl::find_1s(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   switch(lf) {

   default:
      assert(0);
   }
   return {};
}

// --------------------------------------------------------------------------------
twos gdb_impl::find_2s(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   switch(lf) {

   default:
      assert(0);
   }
   return {};
}

// --------------------------------------------------------------------------------
threes gdb_impl::find_3s(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   switch(lf) {

   default:
      assert(0);
   }
   return {};
}

// --------------------------------------------------------------------------------
fours gdb_impl::find_4s(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   fours res;

   switch(lf) {
   case debug_look_for::thread_info:
      res.reserve(16);
      for (auto match : ctre::multiline_search_all<R"(^(\*| ) ([0-9]+) *Thread.*[0-9]\) ("[a-zA-Z_0-9]+").*(0x.*)$)">(s)) {
         auto [m, sel, id, name, frame] = match;
         res.push_back(four_tuple{sel, id, name, frame});
      }
      return res;

   default:
      assert(0);
   }
   return res;
}
