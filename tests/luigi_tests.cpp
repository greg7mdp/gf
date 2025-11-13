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

TEST_CASE("INI_File::insert_in_section - duplicate removal bug") {
   // This test verifies the fix for the bug where removing duplicates
   // would cause the rest of the file to be appended, duplicating all
   // subsequent sections. The bug was in line 2406 of luigi.hpp where
   // std::string_view was constructed without a size parameter.

   // Create a temporary test file
   fs::path test_file = fs::temp_directory_path() / "test_ini_bug.ini";

   SUBCASE("Removing duplicate should not duplicate subsequent sections") {
      // Create initial file with two sections
      {
         std::ofstream ofs(test_file);
         ofs << "[program]\n"
             << "./examples/calc\n"
             << "./gf\n"
             << "./examples/gf_testprog\n"
             << "\n"
             << "[ui_layout]\n"
             << "layout1\n"
             << "layout2\n";
      }

      // Insert "./gf\n" at the beginning (it already exists, so should just move it)
      INI_File{test_file}.insert_in_section("[program]\n", "./gf\n", 0);

      // Read the result
      std::ifstream ifs(test_file);
      std::string result((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());

      // Verify the structure
      CHECK(result.find("[program]\n") != std::string::npos);
      CHECK(result.find("[ui_layout]\n") != std::string::npos);

      // Count how many times [ui_layout] appears - should be exactly once
      size_t count = 0;
      size_t pos = 0;
      while ((pos = result.find("[ui_layout]", pos)) != std::string::npos) {
         count++;
         pos += 11; // length of "[ui_layout]"
      }
      CHECK(count == 1);

      // Verify layout1 and layout2 appear exactly once each
      CHECK(std::count(result.begin(), result.end(), '1') == 1);
      CHECK(std::count(result.begin(), result.end(), '2') == 1);

      // Verify ./gf is at the beginning of [program] section
      size_t program_pos = result.find("[program]\n");
      size_t gf_pos = result.find("./gf\n", program_pos);
      size_t calc_pos = result.find("./examples/calc\n", program_pos);
      CHECK(gf_pos < calc_pos); // ./gf should come before ./examples/calc
   }

   SUBCASE("Removing duplicate from middle of section") {
      // Create initial file
      {
         std::ofstream ofs(test_file);
         ofs << "[program]\n"
             << "./first\n"
             << "./middle\n"
             << "./last\n"
             << "\n"
             << "[other]\n"
             << "content1\n"
             << "content2\n";
      }

      // Move "./middle\n" to the beginning
      INI_File{test_file}.insert_in_section("[program]\n", "./middle\n", 0);

      // Read the result
      std::ifstream ifs(test_file);
      std::string result((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());

      // Verify [other] section appears exactly once
      size_t count = 0;
      size_t pos = 0;
      while ((pos = result.find("[other]", pos)) != std::string::npos) {
         count++;
         pos += 7;
      }
      CHECK(count == 1);

      // Verify content1 and content2 appear exactly once
      CHECK(result.find("content1") != std::string::npos);
      CHECK(result.find("content2") != std::string::npos);
      size_t first_content1 = result.find("content1");
      size_t second_content1 = result.find("content1", first_content1 + 8);
      CHECK(second_content1 == std::string::npos); // Should not find it again

      // Verify order in [program] section: middle, first, last
      size_t program_pos = result.find("[program]\n");
      size_t middle_pos = result.find("./middle\n", program_pos);
      size_t first_pos = result.find("./first\n", program_pos);
      size_t last_pos = result.find("./last\n", program_pos);
      CHECK(middle_pos < first_pos);
      CHECK(first_pos < last_pos);
   }

   SUBCASE("Inserting new entry should not duplicate subsequent sections") {
      // Create initial file
      {
         std::ofstream ofs(test_file);
         ofs << "[program]\n"
             << "./existing\n"
             << "\n"
             << "[settings]\n"
             << "key=value\n";
      }

      // Insert a new entry that doesn't exist
      INI_File{test_file}.insert_in_section("[program]\n", "./new\n", 0);

      // Read the result
      std::ifstream ifs(test_file);
      std::string result((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());

      // Verify [settings] section appears exactly once
      size_t count = 0;
      size_t pos = 0;
      while ((pos = result.find("[settings]", pos)) != std::string::npos) {
         count++;
         pos += 10;
      }
      CHECK(count == 1);

      // Verify the new entry is there
      CHECK(result.find("./new\n") != std::string::npos);
      CHECK(result.find("./existing\n") != std::string::npos);
   }

   SUBCASE("Multiple operations should not cause cumulative duplication") {
      // Create initial file
      {
         std::ofstream ofs(test_file);
         ofs << "[program]\n"
             << "./a\n"
             << "./b\n"
             << "./c\n"
             << "\n"
             << "[ui_layout]\n"
             << "layout_line\n";
      }

      // Perform multiple insert operations (simulating multiple program runs)
      INI_File{test_file}.insert_in_section("[program]\n", "./b\n", 0);
      INI_File{test_file}.insert_in_section("[program]\n", "./c\n", 0);
      INI_File{test_file}.insert_in_section("[program]\n", "./a\n", 0);

      // Read the result
      std::ifstream ifs(test_file);
      std::string result((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());

      // Count [ui_layout] sections - should still be exactly one
      size_t count = 0;
      size_t pos = 0;
      while ((pos = result.find("[ui_layout]", pos)) != std::string::npos) {
         count++;
         pos += 11;
      }
      CHECK(count == 1);

      // Verify layout_line appears exactly once
      count = 0;
      pos = 0;
      while ((pos = result.find("layout_line", pos)) != std::string::npos) {
         count++;
         pos += 11;
      }
      CHECK(count == 1);
   }

   // Clean up
   fs::remove(test_file);
}