#pragma once

#include <string_view>
#include <vector>
#include <optional>
#include <cstdint>

#include <boost/mp11.hpp>

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

template <size_t N>
using tuples_t = boost::mp11::mp_repeat_c<std::tuple<sv_t>, N>; // tuple<sv_t, sv_t, ...>

template <size_t N>
using opt_res_t = std::optional<tuples_t<N>>;                  // optional<tuple<sv_t, sv_t, ...>>

template <size_t N>
using vec_res_t = std::vector<tuples_t<N>>;                    // vector<tuple<sv_t, sv_t, ...>>

// --------
// debugger
// --------
struct debugger_base {
   virtual ~debugger_base() {}

   virtual bool          matches(sv_t s, debug_look_for lf, size_t pos = 0) const = 0;

   virtual opt_res_t<1> find_1(sv_t s, debug_look_for lf, size_t pos = 0) const = 0;
   virtual opt_res_t<2> find_2(sv_t s, debug_look_for lf, size_t pos = 0) const = 0;
   virtual opt_res_t<3> find_3(sv_t s, debug_look_for lf, size_t pos = 0) const = 0;
   virtual opt_res_t<4> find_4(sv_t s, debug_look_for lf, size_t pos = 0) const = 0;

   virtual vec_res_t<1> find_1s(sv_t s, debug_look_for lf, size_t pos = 0) const = 0;
   virtual vec_res_t<2> find_2s(sv_t s, debug_look_for lf, size_t pos = 0) const = 0;
   virtual vec_res_t<3> find_3s(sv_t s, debug_look_for lf, size_t pos = 0) const = 0;
   virtual vec_res_t<4> find_4s(sv_t s, debug_look_for lf, size_t pos = 0) const = 0;
};

struct gdb_impl : public debugger_base {
   bool         matches(sv_t s, debug_look_for lf, size_t pos = 0) const final;

   opt_res_t<1> find_1(sv_t s, debug_look_for lf, size_t pos = 0) const final;
   opt_res_t<2> find_2(sv_t s, debug_look_for lf, size_t pos = 0) const final;
   opt_res_t<3> find_3(sv_t s, debug_look_for lf, size_t pos = 0) const final;
   opt_res_t<4> find_4(sv_t s, debug_look_for lf, size_t pos = 0) const final;

   vec_res_t<1> find_1s(sv_t s, debug_look_for lf, size_t pos = 0) const final;
   vec_res_t<2> find_2s(sv_t s, debug_look_for lf, size_t pos = 0) const final;
   vec_res_t<3> find_3s(sv_t s, debug_look_for lf, size_t pos = 0) const final;
   vec_res_t<4> find_4s(sv_t s, debug_look_for lf, size_t pos = 0) const final;
};

// --------
// language
// --------
struct language_base {
   virtual ~language_base() {}
   
   virtual std::vector<sv_t>     debuggable_expressions(sv_t code, uint32_t e = 0) const    = 0;
   virtual std::optional<bounds> find_at_pos(sv_t code, code_look_for lf, size_t pos) const = 0;

   virtual bool         matches(sv_t s, code_look_for lf, size_t pos = 0) const = 0;
   virtual opt_res_t<1> find_1(sv_t s, code_look_for lf, size_t pos = 0) const  = 0;
   virtual opt_res_t<2> find_2(sv_t s, code_look_for lf, size_t pos = 0) const  = 0;
   virtual opt_res_t<3> find_3(sv_t s, code_look_for lf, size_t pos = 0) const  = 0;
};

struct cpp_impl : public language_base {
   std::vector<sv_t>     debuggable_expressions(sv_t code, uint32_t e = 0) const final;
   std::optional<bounds> find_at_pos(sv_t code, code_look_for lf, size_t pos) const final;

   bool         matches(sv_t s, code_look_for lf, size_t pos = 0) const final;
   opt_res_t<1> find_1(sv_t s, code_look_for lf, size_t pos = 0) const final;
   opt_res_t<2> find_2(sv_t s, code_look_for lf, size_t pos = 0) const final;
   opt_res_t<3> find_3(sv_t s, code_look_for lf, size_t pos = 0) const final;
};

// ----
// misc
// ----
struct misc {

};


}