#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include <clangd.hpp>

// Helper to read file contents
std::string read_file(const fs::path& path) {
   std::ifstream file(path);
   if (!file) {
      throw std::runtime_error("Failed to open file: " + path.string());
   }
   return std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
}

// Helper to find line number of a pattern in file content
std::optional<size_t> find_line_with_pattern(const std::string& content, const std::string& pattern) {
   std::istringstream stream(content);
   std::string line;
   size_t line_num = 0;

   while (std::getline(stream, line)) {
      if (line.find(pattern) != std::string::npos) {
         return line_num;
      }
      line_num++;
   }
   return std::nullopt;
}

// Helper to find column position of pattern in a specific line
size_t find_column_in_line(const std::string& content, size_t line_num, const std::string& pattern) {
   std::istringstream stream(content);
   std::string line;
   size_t current_line = 0;

   while (std::getline(stream, line)) {
      if (current_line == line_num) {
         size_t pos = line.find(pattern);
         return (pos != std::string::npos) ? pos : 0;
      }
      current_line++;
   }
   return 0;
}

// Helper to wait for async callback with timeout
template<typename Func>
bool wait_for_callback(Func&& check, std::chrono::milliseconds timeout = std::chrono::milliseconds(5000)) {
   auto start = std::chrono::steady_clock::now();
   while (!check()) {
      if (std::chrono::steady_clock::now() - start > timeout) {
         return false;
      }
      std::this_thread::sleep_for(std::chrono::milliseconds(10));
   }
   return true;
}

TEST_CASE("ClangdClient basic functionality") {
   // Use simple test file with stable line numbers
   fs::path test_file = fs::current_path() / ".." / "tests" / "reference_files" / "test_input.cpp";
   fs::path root_path = fs::current_path() / "..";

   REQUIRE(fs::exists(test_file));

   std::string test_content = read_file(test_file);
   REQUIRE(!test_content.empty());

   // Find where target_function is called (should be in caller function)
   auto call_line = find_line_with_pattern(test_content, "target_function();");
   REQUIRE(call_line.has_value());
   size_t call_column = find_column_in_line(test_content, *call_line, "target_function");

   // Find where target_function is defined
   auto def_line = find_line_with_pattern(test_content, "void target_function()");
   REQUIRE(def_line.has_value());

   // Track callback execution
   bool callback_executed = false;
   std::string result_file;
   UICode::code_pos_pair_t result_pos;

   // Create client with a simple message forwarder
   ClangdClient client;

   SUBCASE("Start clangd") {
      bool started = client.start(root_path.string(),
         [&](std::function<void(const json&)> callback, const json& message) {
            // Execute callback immediately (we're in test, no UI thread needed)
            callback(message.value("result", json()));
         });

      REQUIRE(started);

      // Wait for initialization
      REQUIRE(wait_for_callback([&]() { return client.is_initialized(); }));
      CHECK(client.is_initialized());
   }

   SUBCASE("Open document and goto definition") {
      bool started = client.start(root_path.string(),
         [&](std::function<void(const json&)> callback, const json& message) {
            callback(message.value("result", json()));
         });

      REQUIRE(started);
      REQUIRE(wait_for_callback([&]() { return client.is_initialized(); }));

      // Open test file
      client.open_document(test_file.string(), test_content);

      // Give clangd time to parse the document
      std::this_thread::sleep_for(std::chrono::milliseconds(500));

      // Request definition of target_function from the call site
      UICode::code_pos_t pos{*call_line, call_column};

      client.goto_definition(test_file.string(), pos,
         [&](const std::string& file, UICode::code_pos_pair_t pos_pair) {
            result_file = file;
            result_pos = pos_pair;
            callback_executed = true;
         });

      // Wait for callback with timeout
      REQUIRE(wait_for_callback([&]() { return callback_executed; }));

      CHECK(callback_executed);
      CHECK(!result_file.empty());
      CHECK(result_file.find("test_input.cpp") != std::string::npos);

      // Should jump to definition line
      CHECK(result_pos[0].line == *def_line);
   }

   SUBCASE("Query before initialization should not crash") {
      ClangdClient uninit_client;

      // This should not crash, just return early
      uninit_client.open_document("/tmp/test.cpp", "int main() {}");

      bool callback_called = false;
      uninit_client.goto_definition("/tmp/test.cpp", UICode::code_pos_t{0, 0},
         [&](const std::string&, UICode::code_pos_pair_t) {
            callback_called = true;
         });

      // Wait a bit to ensure callback is not called
      std::this_thread::sleep_for(std::chrono::milliseconds(100));
      CHECK(!callback_called);
   }

   SUBCASE("Multiple goto_definition requests") {
      bool started = client.start(root_path.string(),
         [&](std::function<void(const json&)> callback, const json& message) {
            callback(message.value("result", json()));
         });

      REQUIRE(started);
      REQUIRE(wait_for_callback([&]() { return client.is_initialized(); }));

      client.open_document(test_file.string(), test_content);
      std::this_thread::sleep_for(std::chrono::milliseconds(500));

      int callbacks_received = 0;

      // Make two requests for the same target_function to test multiple concurrent requests
      client.goto_definition(test_file.string(), UICode::code_pos_t{*call_line, call_column},
         [&](const std::string&, UICode::code_pos_pair_t) {
            callbacks_received++;
         });

      client.goto_definition(test_file.string(), UICode::code_pos_t{*call_line, call_column},
         [&](const std::string&, UICode::code_pos_pair_t) {
            callbacks_received++;
         });

      // Both callbacks should be executed
      REQUIRE(wait_for_callback([&]() { return callbacks_received == 2; }));
      CHECK(callbacks_received == 2);
   }
}
