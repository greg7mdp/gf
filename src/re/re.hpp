#pragma once

#include <string_view>
#include <vector>
#include <optional>

namespace regexp {

struct bounds { size_t start, end; };

enum expr_flags {
   expr_flags_none = 0,
   allow_function_calls = 1
};

// --------
// debugger
// --------
struct debugger_base {
   virtual ~debugger_base() {}
   virtual bool match_stack_or_breakpoint_output(std::string_view s) const = 0;
   virtual bool evaluation_error(std::string_view s) const = 0;
};

struct gdb : public debugger_base {
   bool match_stack_or_breakpoint_output(std::string_view s) const final;
   bool evaluation_error(std::string_view s) const final;
};

// --------
// language
// --------
struct language_base {
   virtual ~language_base() {}
   virtual std::vector<std::string_view> extract_debuggable_expressions(std::string_view code,
                                                                        expr_flags       e = expr_flags_none) const = 0;
   virtual std::optional<bounds>         find_symbol_at_pos(std::string_view code, size_t pos) const = 0;
};

struct cpp : public language_base {
   std::vector<std::string_view> extract_debuggable_expressions(std::string_view code,
                                                                expr_flags       e = expr_flags_none) const final;
   std::optional<bounds>         find_symbol_at_pos(std::string_view code, size_t pos) const final;
};

// ----
// misc
// ----
struct misc {

};


}