#pragma once

#include <string>
#include <string_view>
#include <vector>

namespace regexp {

// base functionality
// ------------------
struct debugger_base {
   virtual ~debugger_base() {}
   virtual bool match_stack_or_breakpoint_output(std::string_view s) = 0;
};

struct language_base {
   virtual ~language_base() {}
   virtual std::vector<std::string_view> extract_debuggable_expressions(std::string_view code) = 0;
};

// specific support
// ----------------
struct gdb : public debugger_base {
   bool match_stack_or_breakpoint_output(std::string_view s) final;
};

struct cpp : public language_base {
   std::vector<std::string_view> extract_debuggable_expressions(std::string_view code) final;
};

}