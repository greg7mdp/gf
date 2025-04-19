#pragma once

#include <string_view>
#include <vector>
#include <optional>
#include <cstdint>

namespace regexp {

struct bounds { size_t start, end; };

enum expr_flags {
   allow_function_calls     = 1 << 0,
   avoid_constant_litterals = 1 << 1
};

enum class code_look_for {
   selectable_expression        // an expression to select when double clicking (compat. with `add_watch`)
};

enum class debug_look_for {
   stack_or_breakpoint_output,  // debugger output when hitting a breakpoint or stack frame change
   evaluation_error,            // debugger output when evaluating an expression fails
   thread_info                  // `info threads` debugger output -> (id, thread_name, stack_frame)
};

using sv_t = std::string_view;

using two_tuple   = std::tuple<sv_t, sv_t>;
using three_tuple = std::tuple<sv_t, sv_t, sv_t>;
using four_tuple  = std::tuple<sv_t, sv_t, sv_t, sv_t>;

using one_res   = std::optional<sv_t>;
using two_res   = std::optional<two_tuple>;
using three_res = std::optional<three_tuple>;
using four_res  = std::optional<four_tuple>;

using ones   = std::vector<sv_t>;
using twos   = std::vector<two_tuple>;
using threes = std::vector<three_tuple>;
using fours  = std::vector<four_tuple>;

// --------
// debugger
// --------
struct debugger_base {
   virtual ~debugger_base() {}
   virtual bool      matches(sv_t s, debug_look_for lf, size_t pos = 0) const = 0;

   virtual one_res   find_1(sv_t s, debug_look_for lf, size_t pos = 0) const  = 0;
   virtual two_res   find_2(sv_t s, debug_look_for lf, size_t pos = 0) const  = 0;
   virtual three_res find_3(sv_t s, debug_look_for lf, size_t pos = 0) const  = 0;
   virtual four_res  find_4(sv_t s, debug_look_for lf, size_t pos = 0) const  = 0;

   virtual ones   find_1s(sv_t s, debug_look_for lf, size_t pos = 0) const  = 0;
   virtual twos   find_2s(sv_t s, debug_look_for lf, size_t pos = 0) const  = 0;
   virtual threes find_3s(sv_t s, debug_look_for lf, size_t pos = 0) const  = 0;
   virtual fours  find_4s(sv_t s, debug_look_for lf, size_t pos = 0) const  = 0;
};

struct gdb_impl : public debugger_base {
   bool      matches(sv_t s, debug_look_for lf, size_t pos = 0) const final;

   one_res   find_1(sv_t s, debug_look_for lf, size_t pos = 0) const final;
   two_res   find_2(sv_t s, debug_look_for lf, size_t pos = 0) const final;
   three_res find_3(sv_t s, debug_look_for lf, size_t pos = 0) const final;
   four_res  find_4(sv_t s, debug_look_for lf, size_t pos = 0) const final;

   ones   find_1s(sv_t s, debug_look_for lf, size_t pos = 0) const final;
   twos   find_2s(sv_t s, debug_look_for lf, size_t pos = 0) const final;
   threes find_3s(sv_t s, debug_look_for lf, size_t pos = 0) const final;
   fours  find_4s(sv_t s, debug_look_for lf, size_t pos = 0) const final;
};

// --------
// language
// --------
struct language_base {
   virtual ~language_base() {}
   virtual std::vector<sv_t> debuggable_expressions(sv_t code, uint32_t e = 0) const    = 0;
   virtual std::optional<bounds> find_at_pos(sv_t code, code_look_for lf, size_t pos) const = 0;

   virtual bool      matches(sv_t s, code_look_for lf, size_t pos = 0) const = 0;
   virtual one_res   find_1(sv_t s, code_look_for lf, size_t pos = 0) const  = 0;
   virtual two_res   find_2(sv_t s, code_look_for lf, size_t pos = 0) const  = 0;
   virtual three_res find_3(sv_t s, code_look_for lf, size_t pos = 0) const  = 0;
};

struct cpp_impl : public language_base {
   std::vector<sv_t>     debuggable_expressions(sv_t code, uint32_t e = 0) const final;
   std::optional<bounds> find_at_pos(sv_t code, code_look_for lf, size_t pos) const final;

   bool      matches(sv_t s, code_look_for lf, size_t pos = 0) const final;
   one_res   find_1(sv_t s, code_look_for lf, size_t pos = 0) const final;
   two_res   find_2(sv_t s, code_look_for lf, size_t pos = 0) const final;
   three_res find_3(sv_t s, code_look_for lf, size_t pos = 0) const final;
};

// ----
// misc
// ----
struct misc {

};


}