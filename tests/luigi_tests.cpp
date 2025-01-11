#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include <luigi.hpp>

#include <tuple>
#include <ranges>
#include <algorithm>

namespace rng   = std::ranges;
namespace views = rng::views;

TEST_CASE(".ini file reading") {
   const char* ini_str = R"(
   [commands]
      Set breakpoint=b mem_visualizer.cpp:118
      break in rbtree_best_fit=b rbtree_best_fit.hpp:1245

   [shortcuts]
      Ctrl+I=print i
      Ctrl+Shift+F10=reverse-next
      Ctrl+Shift+F11=reverse-step

   [ui]
      scale=1
   )";

   INI_Parser ini_file(ini_str);
   
   auto ui  = ini_file | views::filter([](auto &t) { return t._section == "ui" && !t._key.empty(); });
   CHECK(rng::count_if(ui, [](auto &) { return true; }) == 1);

   auto shortcuts  = ini_file | views::filter([](auto &t) { return t._section == "shortcuts" && !t._key.empty(); });
   CHECK(rng::count_if(shortcuts, [](auto &) { return true; }) == 3);

   auto first_command =  rng::find_if(ini_file, [](auto &t) { return t._section == "commands" && !t._key.empty(); });
   CHECK(first_command != rng::end(ini_file));
   auto [section, key, value] = *first_command;
   CHECK(section == "commands");
   CHECK(key == "Set breakpoint");
   CHECK(value == "b mem_visualizer.cpp:118");
   
}