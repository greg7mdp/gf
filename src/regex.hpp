#pragma once

#include <string>
#include <string_view>
#include <vector>

struct regex {
   static bool match_stack_or_breakpoint_output(std::string_view s);

   static std::vector<std::string_view> extract_debuggable_expressions(std::string_view code);
};
