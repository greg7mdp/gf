#pragma once

#include <string_view>
#include <vector>
#include <optional>
#include <cstdint>

namespace regexp {

struct bounds { size_t start, end; };

enum expr_flags {
   allow_function_calls = 1 << 0
};

enum class code_look_for {
   selectable_expression        // an expression to select when double clicking (compat. with `add_watch`)
};

enum class debug_look_for {
   stack_or_breakpoint_output,  // debugger output when hitting a breakpoint or stack frame change
   evaluation_error             // debugger output when evaluating an expression fails
};

using one_res = std::optional<std::string_view>;
using two_res = std::optional<std::pair<std::string_view, std::string_view>>;

// --------
// debugger
// --------
struct debugger_base {
   virtual ~debugger_base() {}
   virtual bool    matches(std::string_view s, debug_look_for lf) const = 0;
   virtual one_res find_1(std::string_view s, debug_look_for lf) const  = 0;
};

struct gdb_impl : public debugger_base {
   bool    matches(std::string_view s, debug_look_for lf) const final;
   one_res find_1(std::string_view s, debug_look_for lf) const final;
};

// --------
// language
// --------
struct language_base {
   virtual ~language_base() {}
   virtual std::vector<std::string_view> debuggable_expressions(std::string_view code, uint32_t e = 0) const    = 0;
   virtual std::optional<bounds>         find_at_pos(std::string_view code, code_look_for lf, size_t pos) const = 0;

   virtual bool    matches(std::string_view s, code_look_for lf) const = 0;
   virtual one_res find_1(std::string_view s, code_look_for lf) const  = 0;
};

struct cpp_impl : public language_base {
   std::vector<std::string_view> debuggable_expressions(std::string_view code, uint32_t e = 0) const final;
   std::optional<bounds>         find_at_pos(std::string_view code, code_look_for lf, size_t pos) const final;

   bool    matches(std::string_view s, code_look_for lf) const final;
   one_res find_1(std::string_view s, code_look_for lf) const final;
};

// ----
// misc
// ----
struct misc {

};


}