#pragma once

#include <string_view>
#include <vector>

namespace regexp {

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

// language
// --------
struct language_base {
   virtual ~language_base() {}
   virtual std::vector<std::string_view> extract_debuggable_expressions(std::string_view code) const = 0;
};

// specific support
// ----------------
struct cpp : public language_base {
   std::vector<std::string_view> extract_debuggable_expressions(std::string_view code) const final;
};

}