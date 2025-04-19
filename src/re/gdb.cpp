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
opt_res_t<1> gdb_impl::find_1(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   switch(lf) {

   default:
      assert(0);
   }
   return {};
}

// --------------------------------------------------------------------------------
opt_res_t<2> gdb_impl::find_2(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   switch(lf) {

   default:
      assert(0);
   }
   return {};
}

// --------------------------------------------------------------------------------
opt_res_t<3> gdb_impl::find_3(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   switch(lf) {

   default:
      assert(0);
   }
   return {};
}

// --------------------------------------------------------------------------------
opt_res_t<4> gdb_impl::find_4(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   switch(lf) {

   default:
      assert(0);
   }
   return {};
}

// --------------------------------------------------------------------------------
vec_res_t<1> gdb_impl::find_1s(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   switch(lf) {

   default:
      assert(0);
   }
   return {};
}

// --------------------------------------------------------------------------------
vec_res_t<2> gdb_impl::find_2s(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   switch(lf) {

   default:
      assert(0);
   }
   return {};
}

// --------------------------------------------------------------------------------
vec_res_t<3> gdb_impl::find_3s(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   switch(lf) {

   default:
      assert(0);
   }
   return {};
}

// --------------------------------------------------------------------------------
vec_res_t<4> gdb_impl::find_4s(std::string_view s, debug_look_for lf, size_t pos /* = 0 */) const {
   vec_res_t<4> res;

   switch(lf) {
   case debug_look_for::thread_info:
      // ---------------------------------- gdb output --------------------------------------------
      //   Id   Target Id                                        Frame
      // * 1    Thread 0x7ffff78eb780 (LWP 1103966) "gf"         0x00007ffff7718bcf in poll () from /lib/x86_64-linux-gnu/libc.so.6
      //   3    Thread 0x7ffff3200640 (LWP 1103968) "gdb_thread" 0x00007ffff771b63d in select () from /lib/x86_64-linux-gnu/libc.so.6
      // (gdb)
      // ------------------------------------------------------------------------------------------
      res.reserve(8);
      for (auto match : ctre::multiline_search_all<R"(^(\*| ) ([0-9]+) *Thread.*[0-9]\) ("[a-zA-Z_0-9]+").*(0x.*)$)">(s)) {
         auto [m, sel, id, name, frame] = match;
         res.push_back(tuples_t<4>{sel, id, name, frame});
      }
      return res;

   default:
      assert(0);
   }
   return res;
}
