#pragma once

#include "luigi.hpp"

#include <mutex>
#include <thread>
#include <sys/wait.h>
#include <fcntl.h>

#include <nlohmann/json.hpp>
using json = nlohmann::json;

// #define CLANGD_DEBUG_LOGGING 1

#ifdef CLANGD_DEBUG_LOGGING
   #define CLANGD_LOG(...) std::print(std::cerr, __VA_ARGS__)
#else
   #define CLANGD_LOG(...) ((void)0)
#endif

// --------------------------------------------------------------------------------------------
inline void set_thread_name(const char* name) {
#if defined(__linux__) || defined(__FreeBSD__)
   pthread_setname_np(pthread_self(), name);
#elif defined(__APPLE__)
   pthread_setname_np(name);
#endif
}

// --------------------------------------------------------------------------------------------
// Map clangd token type string to sem_tok_t
// --------------------------------------------------------------------------------------------
static inline sem_tok_t MapTokenType(const std::string& token_type) {
   if (token_type == "comment")
      return sem_tok_t::comment;
   if (token_type == "string")
      return sem_tok_t::string;
   if (token_type == "number")
      return sem_tok_t::number;
   if (token_type == "operator")
      return sem_tok_t::oper;
   if (token_type == "macro")
      return sem_tok_t::preprocessor;
   if (token_type == "keyword" || token_type == "modifier")
      return sem_tok_t::keyword;
   if (token_type == "type" || token_type == "class" || token_type == "struct" || token_type == "enum" ||
       token_type == "interface" || token_type == "typeParameter" || token_type == "namespace")
      return sem_tok_t::type;
   if (token_type == "variable" || token_type == "parameter" || token_type == "property" || token_type == "enumMember")
      return sem_tok_t::variable;
   if (token_type == "function" || token_type == "method")
      return sem_tok_t::function;

   return sem_tok_t::def;
}

// --------------------------------------------------------------------------------------------
// Clangd LSP Client
// --------------------------------------------------------------------------------------------
struct ClangdResponse {
   std::function<void(const json&)> callback;
   json                             result;
};

class ClangdClient {
   using fwd_resp_t        = std::function<void(std::function<void(const json&)> callback, const json& message)>;
   using notify_callback_t = std::function<void(const std::string& method, const json& params)>;

private:
   pid_t             _clangd_pid          = -1;
   int               _pipe_to_clangd[2]   = {-1, -1};
   int               _pipe_from_clangd[2] = {-1, -1};
   int               _next_request_id     = 1;
   std::atomic<bool> _initialized         = false;
   std::string       _root_path;
   std::thread       _clangd_thread;
   std::atomic<bool> _kill_thread = false;
   std::mutex        _pending_mutex;
   fwd_resp_t        _fwd_response;
   notify_callback_t _notify_callback;

   std::unordered_map<int, std::function<void(const json&)>> _pending_requests;

   void send_request(const std::string& method, const json& params, std::function<void(const json&)> callback) {
      int request_id = _next_request_id++;

      json request = {
         {"jsonrpc", "2.0"     },
         {"id",      request_id},
         {"method",  method    },
         {"params",  params    }
      };

      std::string content = request.dump();
      std::string message = std::format("Content-Length: {}\r\n\r\n{}", content.size(), content);

      if (_pipe_to_clangd[1] != -1) {
         (void)!write(_pipe_to_clangd[1], message.data(), message.size());
         std::lock_guard<std::mutex> lock(_pending_mutex);
         _pending_requests[request_id] = callback;
      }
   }

   void send_notification(const std::string& method, const json& params) {
      json notification = {
         {"jsonrpc", "2.0" },
         {"method",  method},
         {"params",  params}
      };

      std::string content = notification.dump();
      std::string message = std::format("Content-Length: {}\r\n\r\n{}", content.size(), content);

      if (_pipe_to_clangd[1] != -1) {
         (void)!write(_pipe_to_clangd[1], message.data(), message.size());
      }
   }

public:
   std::vector<sem_tok_t> _token_types;
   std::vector<std::string>       _token_modifiers;

   ClangdClient() = default;

   ~ClangdClient() { shutdown(); }

   bool start(const std::string& root_path, const std::string& clangd_path, fwd_resp_t forward_clang_response,
              notify_callback_t notification_callback = nullptr) {
      _fwd_response    = std::move(forward_clang_response);
      _notify_callback = std::move(notification_callback);
      _root_path       = root_path;

      // Create pipes
      if (pipe(_pipe_to_clangd) == -1 || pipe(_pipe_from_clangd) == -1) {
         CLANGD_LOG("Failed to create pipes for clangd\n");
         return false;
      }

      _clangd_pid = fork();

      if (_clangd_pid == -1) {
         CLANGD_LOG("Failed to fork for clangd\n");
         return false;
      }

      if (_clangd_pid == 0) {
         // Child process - run clangd
         dup2(_pipe_to_clangd[0], STDIN_FILENO);
         dup2(_pipe_from_clangd[1], STDOUT_FILENO);

         close(_pipe_to_clangd[0]);
         close(_pipe_to_clangd[1]);
         close(_pipe_from_clangd[0]);
         close(_pipe_from_clangd[1]);

         // Save original stderr in case exec fails
         int stderr_backup = dup(STDERR_FILENO);

         // Redirect stderr to /dev/null to suppress clangd's logging
         int dev_null = open("/dev/null", O_WRONLY);
         if (dev_null != -1) {
            dup2(dev_null, STDERR_FILENO);
            close(dev_null);
         }

         execlp(clangd_path.c_str(), clangd_path.c_str(), "--background-index", nullptr);

         // If we get here, exec failed - restore stderr and print error
         if (stderr_backup != -1) {
            dup2(stderr_backup, STDERR_FILENO);
            close(stderr_backup);
         }
         std::print(std::cerr, "Failed to execute clangd at path: {}\n", clangd_path);

         _exit(1);
      }

      // Parent process
      close(_pipe_to_clangd[0]);
      close(_pipe_from_clangd[1]);

      // Start the thread to read responses
      _clangd_thread = std::thread([this]() { clangd_thread_fn(); });

      // Initialize LSP
      initialize();

      return true;
   }

   void initialize() {
      json params = {
         {"processId",    getpid()                       },
         {"rootUri",      "file://" + _root_path         },
         {"capabilities",
          {{"textDocument",
            {{"definition", {{"linkSupport", true}}},
             {"semanticTokens",
              {{"requests", {{"full", true}}},
               {"tokenTypes",
                {"namespace", "type",     "class",    "enum",       "interface", "struct",  "typeParameter",
                 "parameter", "variable", "property", "enumMember", "function",  "method",  "macro",
                 "keyword",   "modifier", "comment",  "string",     "number",    "operator"}},
               {"tokenModifiers",
                {"declaration", "definition", "readonly", "static", "deprecated", "abstract", "async", "modification",
                 "documentation", "defaultLibrary"}}}}}}}}
      };

      send_request("initialize", params, [this](const json& result) {
         _initialized = true;

         // Store semantic token legend if available
         if (result.contains("capabilities") && result["capabilities"].contains("semanticTokensProvider")) {
            auto provider = result["capabilities"]["semanticTokensProvider"];
            if (provider.contains("legend")) {
               auto token_type_strings = provider["legend"]["tokenTypes"].get<std::vector<std::string>>();
               _token_modifiers        = provider["legend"]["tokenModifiers"].get<std::vector<std::string>>();

               // Map string token types to sem_tok_t enum
               _token_types.reserve(token_type_strings.size());
               for (const auto& type_str : token_type_strings) {
                  _token_types.push_back(MapTokenType(type_str));
               }

               CLANGD_LOG("Semantic tokens supported with {} types and {} modifiers\n", _token_types.size(),
                          _token_modifiers.size());
            }
         }

         send_notification("initialized", json::object());
      });
   }

   void open_document(const std::string& file_path, std::string_view content) {
      if (!_initialized)
         return;

      json params = {
         {"textDocument", {{"uri", "file://" + file_path}, {"languageId", "cpp"}, {"version", 1}, {"text", content}}}
      };

      send_notification("textDocument/didOpen", params);
   }

   void goto_definition(const std::string& file_path, const UICode::code_pos_t& pos,
                        std::function<void(const std::string&, UICode::code_pos_pair_t)> callback) {
      if (!_initialized)
         return;

      json params = {
         {"textDocument", {{"uri", "file://" + file_path}}               },
         {"position",     {{"line", pos.line}, {"character", pos.offset}}}
      };

      send_request("textDocument/definition", params, [callback](const json& result) {
         CLANGD_LOG("clangd definition response: {}\n", result.dump());

         if (result.is_null() || result.empty()) {
            CLANGD_LOG("clangd returned null or empty result\n");
            return;
         }

         // Handle both single location and array of locations
         json location = result.is_array() ? result[0] : result;

         if (location.contains("uri") && location.contains("range")) {
            std::string uri = location["uri"];
            if (uri.starts_with("file://")) {
               uri = uri.substr(7);
            }
            UICode::code_pos_pair_t pos{
               UICode::code_pos_t{location["range"]["start"]["line"], location["range"]["start"]["character"]},
               UICode::code_pos_t{location["range"]["end"]["line"],   location["range"]["end"]["character"]  }
            };
            CLANGD_LOG("Jumping to: {} line {} col {}\n", uri, pos[0].line, pos[0].offset);
            callback(uri, pos);
         } else {
            CLANGD_LOG("Location doesn't contain uri or range\n");
         }
      });
   }

   void get_semantic_tokens(const std::string& file_path, std::function<void(const std::vector<uint32_t>&)> callback) {
      if (!_initialized)
         return;

      json params = {
         {"textDocument", {{"uri", "file://" + file_path}}}
      };

      send_request("textDocument/semanticTokens/full", params, [this, callback](const json& result) {
         CLANGD_LOG("clangd semantic tokens response received\n");

         if (result.is_null() || result.empty() || !result.contains("data")) {
            CLANGD_LOG("clangd returned null or empty semantic tokens\n");
            callback({});
            return;
         }

         // Extract the data array
         std::vector<uint32_t> data = result["data"].get<std::vector<uint32_t>>();
         CLANGD_LOG("Received {} semantic token values\n", data.size());
         callback(data);
      });
   }

   void clangd_thread_fn() {
      set_thread_name("clangd_thread");

      int pipe_from_clangd = _pipe_from_clangd[0];

      fd_set  readfds;
      timeval timeout{0, 10000}; // Wait for 10 ms

      std::string buffer;

      while (true) {
         // Wait for data to be available
         FD_ZERO(&readfds);
         FD_SET(pipe_from_clangd, &readfds);
         int result = select(pipe_from_clangd + 1, &readfds, nullptr, nullptr, &timeout);

         if (result == -1) {
            CLANGD_LOG("clangd_thread: select error: {}\n", errno);
            break;
         }

         if (result == 0) { // timeout
            if (_kill_thread)
               break;
            continue;
         }

         if (!FD_ISSET(pipe_from_clangd, &readfds))
            continue;

         // Data is available for reading
         char    chunk[4096];
         ssize_t bytes_read = read(pipe_from_clangd, chunk, sizeof(chunk));

         if (_kill_thread)
            break;

         if (bytes_read <= 0) {
            std::this_thread::sleep_for(std::chrono::microseconds{10000});
            continue;
         }

         buffer.append(chunk, bytes_read);

         // Process complete messages
         while (true) {
            // Look for Content-Length header
            auto header_end = buffer.find("\r\n\r\n");
            if (header_end == std::string::npos)
               break;

            auto content_length_pos = buffer.find("Content-Length: ");
            if (content_length_pos == std::string::npos)
               break;

            content_length_pos += 16; // strlen("Content-Length: ")
            auto content_length_end = buffer.find("\r\n", content_length_pos);
            if (content_length_end == std::string::npos)
               break;

            size_t content_length =
               std::stoull(buffer.substr(content_length_pos, content_length_end - content_length_pos));
            size_t message_start = header_end + 4;

            if (buffer.size() < message_start + content_length)
               break;

            std::string message_content = buffer.substr(message_start, content_length);
            buffer.erase(0, message_start + content_length);

            // Parse JSON and handle response or notification
            try {
               json message = json::parse(message_content);
               CLANGD_LOG("Received clangd message: {}\n", message.dump().substr(0, 200));

               if (message.contains("id") && !message["id"].is_null()) {
                  // This is a response to a request
                  int id = message["id"];
                  CLANGD_LOG("Message has id: {}\n", id);

                  std::function<void(const json&)> callback;
                  {
                     std::lock_guard<std::mutex> lock(_pending_mutex);
                     if (_pending_requests.contains(id)) {
                        CLANGD_LOG("Found pending request for id {}, posting to main thread\n", id);
                        callback = std::move(_pending_requests[id]);
                        _pending_requests.erase(id);
                     } else {
                        CLANGD_LOG("No pending request found for id {}\n", id);
                     }
                  }

                  // Post to main thread
                  if (callback)
                     _fwd_response(std::move(callback), message);
               } else if (message.contains("method")) {
                  // This is a notification from the server
                  std::string method = message["method"];
                  json        params = message.contains("params") ? message["params"] : json::object();

                  CLANGD_LOG("Received notification: {}\n", method);

                  // Forward notification to callback if provided
                  if (_notify_callback) {
                     _notify_callback(method, params);
                  }
               }
            } catch (const std::exception& e) {
               CLANGD_LOG("Failed to parse clangd response: {}\n", e.what());
            }
         }
      }
   }

   void shutdown() {
      if (_clangd_pid != -1) {
         if (_initialized) {
            send_request("shutdown", json::object(), [](const json&) {});
            send_notification("exit", json::object());
         }

         _kill_thread = true;
         if (_clangd_thread.joinable()) {
            _clangd_thread.join();
         }

         close(_pipe_to_clangd[1]);
         close(_pipe_from_clangd[0]);

         waitpid(_clangd_pid, nullptr, 0);
         _clangd_pid = -1;
      }
   }

   bool is_initialized() const { return _initialized.load(); }
};