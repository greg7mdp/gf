#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include <clangd.hpp>

static std::string clangd_path = "clangd";

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
   std::string        line;
   size_t             line_num = 0;

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
   std::string        line;
   size_t             current_line = 0;

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
template <typename Func>
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
   std::atomic<bool>       callback_executed = false;
   std::string             result_file;
   UICode::code_pos_pair_t result_pos;

   // Create client with a simple message forwarder
   ClangdClient client;

   SUBCASE("Start clangd") {
      bool started = client.start(
         root_path.string(), clangd_path,
         [&](std::function<void(const json&)> callback, const json& message) {
            // Execute callback immediately (we're in test, no UI thread needed)
            callback(message.value("result", json()));
         },
         [](const std::string& method, const json& params) {
            // Handle notifications (optional)
            (void)method;
            (void)params;
         });

      REQUIRE(started);

      // Wait for initialization
      REQUIRE(wait_for_callback([&]() { return client.is_initialized(); }));
      CHECK(client.is_initialized());
   }

   SUBCASE("Open document and goto definition") {
      bool started = client.start(root_path.string(), clangd_path,
                                  [&](std::function<void(const json&)> callback, const json& message) {
                                     callback(message.value("result", json()));
                                  });

      REQUIRE(started);
      REQUIRE(wait_for_callback([&]() { return client.is_initialized(); }));

      // Open test file
      client.open_document(test_file.string(), test_content);

      // Request definition of target_function from the call site
      UICode::code_pos_t pos{*call_line, call_column};

      client.goto_definition(test_file.string(), pos, [&](const std::string& file, UICode::code_pos_pair_t pos_pair) {
         result_file       = file;
         result_pos        = pos_pair;
         callback_executed = true;
      });

      // Wait for callback with timeout
      REQUIRE(wait_for_callback([&]() { return callback_executed.load(); }));

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
                                    [&](const std::string&, UICode::code_pos_pair_t) { callback_called = true; });

      // Wait a bit to ensure callback is not called
      std::this_thread::sleep_for(std::chrono::milliseconds(50));
      CHECK(!callback_called);
   }

   SUBCASE("Get semantic tokens") {
      bool started = client.start(root_path.string(), clangd_path,
                                  [&](std::function<void(const json&)> callback, const json& message) {
                                     callback(message.value("result", json()));
                                  });

      REQUIRE(started);
      REQUIRE(wait_for_callback([&]() { return client.is_initialized(); }));

      // Check that token types were populated during initialization
      CHECK(!client._token_types.empty());
      CHECK(!client._token_modifiers.empty());

      // Open test file
      client.open_document(test_file.string(), test_content);

      // Request semantic tokens
      std::atomic<bool>     tokens_received = false;
      std::vector<uint32_t> token_data;

      client.get_semantic_tokens(test_file.string(), [&](const std::vector<uint32_t>& data) {
         token_data      = data;
         tokens_received = true;
      });

      // Wait for callback with timeout
      REQUIRE(wait_for_callback([&]() { return tokens_received.load(); }));

      CHECK(tokens_received);
      CHECK(!token_data.empty());

      // Data should be in groups of 5 (deltaLine, deltaStart, length, tokenType, tokenModifiers)
      CHECK(token_data.size() % 5 == 0);

      // Verify we can decode at least one token
      if (token_data.size() >= 5) {
         uint32_t delta_line      = token_data[0];
         uint32_t delta_start     = token_data[1];
         uint32_t length          = token_data[2];
         uint32_t token_type_idx  = token_data[3];
         uint32_t token_modifiers = token_data[4];

         // Token type index should be valid
         CHECK(token_type_idx < client._token_types.size());

         // Length should be reasonable (not zero, not huge)
         CHECK(length > 0);
         CHECK(length < 1000);

         // Line and column should be non-negative (deltas can be 0 but not negative)
         CHECK(delta_line >= 0);
         CHECK(delta_start >= 0);

         // Token modifiers is a bitset, so any value is valid
         (void)token_modifiers;
      }
   }

   SUBCASE("Semantic tokens for unopened file") {
      bool started = client.start(root_path.string(), clangd_path,
                                  [&](std::function<void(const json&)> callback, const json& message) {
                                     callback(message.value("result", json()));
                                  });

      REQUIRE(started);
      REQUIRE(wait_for_callback([&]() { return client.is_initialized(); }));

      std::atomic<bool>     tokens_received = false;
      std::vector<uint32_t> token_data;

      // Request tokens for a file that hasn't been opened
      client.get_semantic_tokens(test_file.string(), [&](const std::vector<uint32_t>& data) {
         token_data      = data;
         tokens_received = true;
      });

      // Should still get a callback, possibly with empty data
      REQUIRE(wait_for_callback([&]() { return tokens_received.load(); }));
      CHECK(tokens_received);

      // Data might be empty or might have tokens if clangd can find the file
      if (!token_data.empty()) {
         CHECK(token_data.size() % 5 == 0);
      }
   }

   SUBCASE("Verify token type mapping") {
      bool started = client.start(root_path.string(), clangd_path,
                                  [&](std::function<void(const json&)> callback, const json& message) {
                                     callback(message.value("result", json()));
                                  });

      REQUIRE(started);
      REQUIRE(wait_for_callback([&]() { return client.is_initialized(); }));

      // Verify that token types are mapped to valid sem_tok_t values
      for (const auto& token_type : client._token_types) {
         // All token types should be valid enum values
         CHECK(static_cast<int>(token_type) >= 0);
         CHECK(static_cast<int>(token_type) <= static_cast<int>(sem_tok_t::bracket));
      }
   }

   SUBCASE("Verify type tokens are present") {
      bool started = client.start(root_path.string(), clangd_path,
                                  [&](std::function<void(const json&)> callback, const json& message) {
                                     callback(message.value("result", json()));
                                  });

      REQUIRE(started);
      REQUIRE(wait_for_callback([&]() { return client.is_initialized(); }));

      // Open test file
      client.open_document(test_file.string(), test_content);

      // Request semantic tokens
      std::atomic<bool>     tokens_received = false;
      std::vector<uint32_t> token_data;

      client.get_semantic_tokens(test_file.string(), [&](const std::vector<uint32_t>& data) {
         token_data      = data;
         tokens_received = true;
      });

      // Wait for callback with timeout
      REQUIRE(wait_for_callback([&]() { return tokens_received.load(); }));

      CHECK(tokens_received);
      CHECK(!token_data.empty());

      // Look for type tokens specifically (MyStruct, std::string)
      bool found_type_token = false;
      uint32_t current_line = 0;

      for (size_t i = 0; i < token_data.size(); i += 5) {
         uint32_t delta_line     = token_data[i];
         uint32_t delta_start    = token_data[i + 1];
         uint32_t length         = token_data[i + 2];
         uint32_t token_type_idx = token_data[i + 3];

         if (delta_line > 0) {
            current_line += delta_line;
         }

         if (token_type_idx < client._token_types.size()) {
            auto token_type = client._token_types[token_type_idx];

            // Print all tokens for debugging
            std::print(std::cerr, "Line {}: type={} ({}), length={}, start={}\n",
                      current_line, static_cast<int>(token_type),
                      token_type == sem_tok_t::type ? "TYPE" :
                      token_type == sem_tok_t::function ? "FUNCTION" :
                      token_type == sem_tok_t::variable ? "VARIABLE" : "OTHER",
                      length, delta_start);

            if (token_type == sem_tok_t::type) {
               found_type_token = true;
            }
         }
      }

      // We expect to find at least one type token (MyStruct or std::string)
      CHECK(found_type_token);
   }

   SUBCASE("Handle notifications") {
      std::vector<std::string> received_notifications;
      std::atomic<bool>        received_notification = false;

      bool started = client.start(
         root_path.string(), clangd_path,
         [&](std::function<void(const json&)> callback, const json& message) {
            callback(message.value("result", json()));
         },
         [&](const std::string& method, const json& params) {
            // Record notifications received
            received_notifications.push_back(method);
            received_notification = true;
            (void)params;
         });

      REQUIRE(started);
      REQUIRE(wait_for_callback([&]() { return client.is_initialized(); }));

      // Open a document - this may trigger notifications like textDocument/publishDiagnostics
      client.open_document(test_file.string(), test_content);

      // Give clangd time to process and potentially send notifications
      std::this_thread::sleep_for(std::chrono::milliseconds(1000));

      // We might receive notifications (diagnostics, etc.) but it's not guaranteed
      // Just verify the callback mechanism works if notifications are sent
      if (received_notification) {
         CHECK(!received_notifications.empty());
         // Common notifications from clangd:
         // - textDocument/publishDiagnostics
         // - $/progress (for indexing)
         for (const auto& notif : received_notifications) {
            CLANGD_LOG("Test received notification: {}\n", notif);
         }
      }
   }
}
