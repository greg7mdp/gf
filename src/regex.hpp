#pragma once

#include <string>
#include <string_view>
#include <vector>

struct regex {
   static std::vector<std::string_view> extract_debuggable_expressions(std::string_view code);
};
