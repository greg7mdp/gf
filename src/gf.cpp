// TODO Disassembly window:
// 	- Setting/clearing/showing breakpoints.
// 	- Jump/run to line.
// 	- Shift+F10: run to next instruction (for skipping past loops).
// 	- Split source and disassembly view.

// TODO Inspect line mode:
// 	- Jump/run to selected line.
// 	- How to show overloaded variables correctly when moving lines?

// TODO More data visualization tools in the data window.

#include <algorithm>
#include <cassert>
#include <condition_variable>
#include <csignal>
#include <cstdarg>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <ctime>
#include <ctype.h>
#include <dirent.h>
#include <fcntl.h>
#include <fstream>
#include <memory>
#include <mutex>
#include <poll.h>
#include <queue>
#include <ranges>
#include <semaphore.h>
#include <sys/wait.h>
#include <spawn.h>
#include <sys/stat.h>
#include <thread>
#include <unordered_map>
#include <vector>
#include <iostream>
#include <format>

namespace views        = std::views;
namespace rng          = std::ranges;
static const auto npos = std::string::npos;

using namespace std;

#include <ctre.hpp>
using namespace ctre::literals;

#include "luigi.hpp"
#include <re/re.hpp>

// ---------------------------------------------------------------------------------------------
//                              Generic Data structures
// ---------------------------------------------------------------------------------------------
template <typename T>
class SPSCQueue {
public:
   SPSCQueue()
      : _quit(false) {}

   void push(T&& item) {
      std::unique_lock lock(_mutex);
      _queue.push(std::move(item));
      _cv.notify_one();
   }

   bool pop(std::optional<T>& value) {
      std::unique_lock lock(_mutex);
      bool             res = _cv.wait_for(lock, std::chrono::seconds(15), [this] { return !_queue.empty() || _quit; });

      if (!res || _quit) { // !res means we hit the timeout
         value = std::optional<T>{};
         return false;
      }

      value = std::move(_queue.front());
      _queue.pop();
      return true;
   }

   size_t size() const {
      std::unique_lock lock(_mutex);
      return _queue.size();
   }

   void signal_quit() {
      _quit = true;
      _cv.notify_one();
   }

   bool is_quitting() { return _quit; }

private:
   std::queue<T>           _queue;
   mutable std::mutex      _mutex;
   std::condition_variable _cv;
   std::atomic<bool>       _quit;
};

// ---------------------------------------------------------------------------------------------
//                              Utilities
// ---------------------------------------------------------------------------------------------
static inline bool resize_to_lf(std::string& s, char c = '\n') {
   auto end = s.find_first_of(c);
   if (end != npos) {
      s.resize(end);
      return true;
   }
   return false;
}

template <typename T>
T sv_atoi_impl(string_view str, size_t offset = 0) {
   auto sz = str.size();
   assert(offset < sz);
   size_t i = offset;

   while (i < sz && std::isspace(static_cast<unsigned char>(str[i])))
      ++i;

   if (i == sz)
      return 0;

   bool negative = false;

   if (str[i] == '+')
      ++i;
   else if (str[i] == '-') {
      ++i;
      negative = true;
   }

   T result = 0;
   for (; i < sz && std::isdigit(static_cast<unsigned char>(str[i])); ++i) {
      int digit = str[i] - '0';
      result *= 10;
      result -= digit; // calculate in negatives to support INT_MIN, LONG_MIN,..
   }

   return negative ? result : -result;
}

char* mk_cstring(std::string_view sv) {
   auto sz = sv.size();
   auto s  = (char*)malloc(sz + 1);
   for (size_t i = 0; i < sz; ++i)
      s[i] = sv[i];
   s[sz] = 0;
   return s;
}

static inline int sv_atoi(string_view str, size_t offset = 0) { return sv_atoi_impl<int>(str, offset); }

[[maybe_unused]] static inline long sv_atol(string_view str, size_t offset = 0) {
   return sv_atoi_impl<long>(str, offset);
}

[[maybe_unused]] static inline long long sv_atoll(string_view str, size_t offset = 0) {
   return sv_atoi_impl<long long>(str, offset);
}

[[maybe_unused]] static inline uint64_t sv_atoul(string_view str, size_t offset = 0) {
   return sv_atoi_impl<uint64_t>(str, offset);
}

void print(const std::string_view str) { std::cout << str; }

// Variadic template for print with format arguments
template <typename... Args>
void print(std::format_string<Args...> fmt, Args&&... args) {
   std::cout << std::format(fmt, std::forward<Args>(args)...);
}

// ---------------------------------------------------------------------------------------------
//                              Data structures
// ---------------------------------------------------------------------------------------------
struct InterfaceCommand {
   const char* _label = nullptr;
   UIShortcut  _shortcut;
};

struct InterfaceWindow {
   UIElement* (*_create)(UIElement* parent)            = nullptr;
   void (*_update)(const char* data, UIElement* el)    = nullptr;
   void (*_focus)(UIElement* el)                       = nullptr;
   UIElement* _el                                      = nullptr;
   bool       _queued_update                           = false;
   bool       _always_update                           = false;
   void (*_config)(string_view key, string_view value) = nullptr;
};

struct InterfaceDataViewer {
   const char* _add_button_label = nullptr;
   void (*_add_button_callback)();
};

// --------------------------------------------------------------------------------------------
struct Command {
   std::string _key;
   std::string _value;
};

// --------------------------------------------------------------------------------------------
struct ReceiveMessageType {
   UIMessage                                         _msg;
   std::function<void(std::unique_ptr<std::string>)> _callback;
};

// --------------------------------------------------------------------------------------------
//                      Config (mostly read from `gf2_config.ini` file)
// --------------------------------------------------------------------------------------------
struct GF_Config {
   std::string _layout_string = "v(75,h(50,Source,v(50,t(Exe,Breakpoints,Commands,Struct),t(Stack,Files,Thread,"
                                "CmdSearch))),h(40,Console,t(Watch,Locals,Registers,Data,Log,Prof,Memory,View)))";

   // executable window
   // -----------------
   std::string _exe_path;
   std::string _exe_args;
   bool        _exe_ask_dir = true;

   // misc
   // ----
   const char*     _control_pipe_path = nullptr;
   const char*     _vim_server_name   = "GVIM";
   const char*     _log_pipe_path     = nullptr;
   vector<Command> _preset_commands;
   char            _global_config_path[PATH_MAX];
   char            _local_config_dir[PATH_MAX];
   char            _local_config_path[PATH_MAX];
   int             _code_font_size      = 13;
   int             _interface_font_size = 11;
   int             _window_width        = -1;
   int             _window_height       = -1;
   float           _ui_scale            = 1;
   bool            _maximize;
   bool            _selectable_source        = true;
   bool            _restore_watch_window     = false;
   bool            _confirm_command_connect  = true;
   bool            _confirm_command_kill     = true;
   int             _backtrace_count_limit    = 50;
   bool            _grab_focus_on_breakpoint = true;
};

GF_Config gfc;

// --------------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------------
struct WatchWindow;

struct Context {
// ========== Debugger config ======================
#if defined(__OpenBSD__)
   const char* _gdb_path = "egdb";
#else
   const char* _gdb_path = "gdb";
#endif
   std::string _initial_gdb_command = "set prompt (gdb) ";
   bool        _first_update        = true;
   UICode*     _log_window          = nullptr; // if sent, log all debugger output there
   ui_handle   _prev_focus_win      = 0;
   UI*         _ui; // non-owning pointer

   unique_ptr<regexp::debugger_base> _dbg_re;
   unique_ptr<regexp::language_base> _lang_re;

   // ========== Debugger interaction ======================
   int               _pipe_to_gdb     = 0;
   pid_t             _gdb_pid         = 0;
   std::atomic<bool> _kill_gdb_thread = false;
   std::thread       _gdb_thread; // reads gdb output and pushes it to queue (we wait on queue in DebuggerSend
   std::atomic<bool> _evaluate_mode =
      false; // when true, means we sent a command to gdb and are waiting for the response
   char**                 _gdb_argv = nullptr;
   int                    _gdb_argc = 0;
   SPSCQueue<std::string> _evaluate_result_queue;
   std::atomic<bool>      _program_running = true;

   std::unordered_map<std::string, InterfaceWindow> _interface_windows;
   vector<InterfaceCommand>                         _interface_commands;
   vector<InterfaceDataViewer>                      _interface_data_viewers;

   Context();

   // make private
   void send_to_gdb(string_view sv) const {
      char newline = '\n';
      write(_pipe_to_gdb, sv.data(), sv.size());
      write(_pipe_to_gdb, &newline, 1);
   }

   void interrupt_gdb(size_t wait_time_us = 20 * 1000) {
      if (_program_running) {
         kill(_gdb_pid, SIGINT);
         std::this_thread::sleep_for(std::chrono::microseconds{wait_time_us});
         _program_running = false;
      }
   }

   void kill_gdb_thread() {
      print(std::cerr, "killing gdb thread.\n");
      _kill_gdb_thread = true;
      _gdb_thread.join();
      _kill_gdb_thread = false;
   }

   void kill_gdb() {
      kill_gdb_thread();
      print(std::cerr, "killing gdb process {}.\n", _gdb_pid);
      kill(_gdb_pid, SIGKILL);
   }

   void restore_focus() {
      if (_prev_focus_win) {
         _ui->set_focus(_prev_focus_win);
         _prev_focus_win = 0;
      }
   }

   void grab_focus(UIWindow* win) {
      if (win && gfc._grab_focus_on_breakpoint) {
         _prev_focus_win = _ui->get_focus();
         win->grab_focus(); // grab focus when breakpoint is hit!
      }
   }

   void           debugger_thread_fn();
   UIConfig       load_settings(bool earlyPass);
   void           add_builtin_windows_and_commands();
   void           register_extensions();
   void           show_menu(UIButton* self);
   void           create_layout(UIElement* parent, const char*& current);
   void           generate_layout_string(UIElement* e, std::string& sb);
   bool           copy_layout_to_clipboard();
   void           additional_setup();
   UIElement*     switch_to_window_and_focus(string_view name);
   unique_ptr<UI> gf_main(int argc, char** argv);
};

Context ctx;

vector<ReceiveMessageType> receiveMessageTypes;
WatchWindow*               firstWatchWindow = nullptr;
UIMessage                  msgReceivedData;
UIMessage                  msgReceivedLog;
UIMessage                  msgReceivedControl;
UIMessage                  msgReceivedNext = (UIMessage)(UIMessage::USER_PLUS_1);

char previousLocation[256];

// ---------------------------------------------------
// User interface:
// ---------------------------------------------------
struct SourceWindow;

UIWindow*     s_main_window    = nullptr;
UISwitcher*   s_main_switcher  = nullptr;
UICode*       s_display_code   = nullptr;
SourceWindow* s_source_window  = nullptr;
UICode*       s_display_output = nullptr;
UITextbox*    s_input_textbox  = nullptr;
UISpacer*     s_trafficlight   = nullptr;
UIMDIClient*  s_data_window    = nullptr;
UIPanel*      s_data_tab       = nullptr;

// ---------------------------------------------------
// StackWindow
// ---------------------------------------------------

struct StackEntry {
   char     _function[64];
   char     _location[sizeof(previousLocation)];
   uint64_t _address;
   int      _id;
};

struct StackWindow {
private:
   vector<StackEntry> _stack;
   size_t             _selected;
   bool               _has_changed;

public:
   void clear() { _stack.clear(); }
   void append(const StackEntry& entry) { _stack.push_back(entry); }
   bool has_selection() const { return _selected < _stack.size(); }
   bool changed() const { return _has_changed; }
   void set_selected(size_t i) { _selected = i; }
   void set_changed(bool b) { _has_changed = b; }

   vector<StackEntry>& stack() { return _stack; }

   const StackEntry& operator[](size_t i) const { return _stack[i]; }
   StackEntry&       operator[](size_t i) { return _stack[i]; }
   StackEntry&       current() { return _stack[_selected]; }

   void set_frame(UIElement* el, int index);

   int _table_message_proc(UITable* table, UIMessage msg, int di, void* dp);

   static int StackWindowMessage(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<StackWindow*>(el->_cp)->_table_message_proc(static_cast<UITable*>(el), msg, di, dp);
   }

   static UIElement* Create(UIElement* parent);
   static void       Update(const char*, UIElement* table);
};

StackWindow* sw = nullptr;

// Python code:

const char* pythonCode = R"(py

import gdb.types

def _gf_hook_string(basic_type):
    hook_string = str(basic_type)
    template_start = hook_string.find('<')
    if template_start != -1: hook_string = hook_string[0:template_start]
    return hook_string

def _gf_basic_type(value):
    basic_type = gdb.types.get_basic_type(value.type)
    if basic_type.code == gdb.TYPE_CODE_PTR:
        basic_type = gdb.types.get_basic_type(basic_type.target())
    return basic_type

def _gf_value(expression):
    try:
        value = gdb.parse_and_eval(expression[0])
        for index in expression[1:]:
            if isinstance(index, str) and index[0] == '[':
                value = gf_hooks[_gf_hook_string(_gf_basic_type(value))](value, index)
            else: value = value[index]
        return value
    except gdb.error:
        print('??')
        return None

def gf_typeof(expression):
    value = _gf_value(expression)
    if value == None: return
    print(value.type)

def gf_valueof(expression, format):
    value = _gf_value(expression)
    if value == None: return
    result = ''
    while True:
        basic_type = gdb.types.get_basic_type(value.type)
        if basic_type.code != gdb.TYPE_CODE_PTR: break
        try:
            result = result + '(' + str(value) + ') '
            value = value.dereference()
        except: break
    try:
        if format[0] != ' ': result = result + value.format_string(max_elements=10,max_depth=3,format=format)[0:200]
        else: result = result + value.format_string(max_elements=10,max_depth=3)[0:200]
    except:
        result = result + '??'
    print(result)

def gf_addressof(expression):
    value = _gf_value(expression)
    if value == None: return
    print(value.address)

def __gf_fields_recurse(type):
    if type.code == gdb.TYPE_CODE_STRUCT or type.code == gdb.TYPE_CODE_UNION:
        for field in gdb.types.deep_items(type):
            if field[1].is_base_class: __gf_fields_recurse(field[1].type)
            else: print(field[0])
    elif type.code == gdb.TYPE_CODE_ARRAY:
        print('(array) %d' % (type.range()[1]+1))

def _gf_fields_recurse(value):
    basic_type = _gf_basic_type(value)
    __gf_fields_recurse(basic_type)

def gf_fields(expression):
    value = _gf_value(expression)
    if value == None: return
    basic_type = _gf_basic_type(value)
    hook_string = _gf_hook_string(basic_type)
    try: gf_hooks[hook_string](value, None)
    except: __gf_fields_recurse(basic_type)

def gf_locals():
    try:
        frame = gdb.selected_frame()
        block = frame.block()
    except:
        return
    names = set()
    while block and not (block.is_global or block.is_static):
        for symbol in block:
            if (symbol.is_argument or symbol.is_variable or symbol.is_constant):
                names.add(symbol.name)
        block = block.superblock
    for name in names:
        print(name)

end
)";

// Forward declarations:

static bool DisplaySetPosition(const char* file, std::optional<size_t> line, bool useGDBToGetFullPath);
void        WatchAddExpression(string_view string);
bool        CommandInspectLine();

// ------------------------------------------------------
// Utilities:
// ------------------------------------------------------

inline uint64_t Hash(const uint8_t* key, size_t keyBytes) {
   uint64_t hash = 0xCBF29CE484222325;
   for (uintptr_t i = 0; i < keyBytes; i++)
      hash = (hash ^ key[i]) * 0x100000001B3;
   return hash;
}

int ModifiedRowMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::PAINT) {
      static_cast<UIPainter*>(dp)->draw_border(el->_bounds, el->theme().selected, UIRectangle(2));
   }

   return 0;
}

int TrafficLightMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::PAINT) {
      static_cast<UIPainter*>(dp)->draw_rectangle(el->_bounds,
                                                  ctx._program_running ? el->theme().accent1 : el->theme().accent2,
                                                  el->theme().border, UIRectangle(1));
   }

   return 0;
}

int SourceFindEndOfBlock() {
   auto currentLine = s_display_code->current_line();

   if (!currentLine)
      return -1;

   int tabs = 0;

   auto line = s_display_code->line(*currentLine);
   for (size_t i = 0; i < line.size(); i++) {
      if (isspace(line[i]))
         tabs++;
      else
         break;
   }

   size_t num_lines = s_display_code->num_lines();
   for (size_t j = *currentLine + 1; j < num_lines; j++) {
      int t = 0;

      auto line = s_display_code->line(j);
      if (line.empty())
         continue;

      for (size_t i = 0; i < line.size() - 1; i++) {
         if (isspace(line[i]))
            t++;
         else
            break;
      }

      if (t < tabs && line[t] == '}') {
         return j + 1;
      }
   }

   return -1;
}

bool SourceFindOuterFunctionCall(const char** start, const char** end) {
   int  num_lines   = (int)s_display_code->num_lines();
   auto currentLine = s_display_code->current_line();

   if (!currentLine)
      return false;

   size_t offset = s_display_code->line_offset(*currentLine);
   bool   found  = false;

   // Look forwards for the end of the call ");".

   size_t num_chars = s_display_code->size();
   while (offset < num_chars - 1) {
      if ((*s_display_code)[offset] == ')' && (*s_display_code)[offset + 1] == ';') {
         found = true;
         break;
      } else if ((*s_display_code)[offset] == ';' || (*s_display_code)[offset] == '{') {
         break;
      }

      offset++;
   }

   if (!found)
      return false;

   // Look backwards for the matching bracket.

   int level = 0;

   while (offset > 0) {
      if ((*s_display_code)[offset] == ')') {
         level++;
      } else if ((*s_display_code)[offset] == '(') {
         level--;
         if (level == 0)
            break;
      }

      offset--;
   }

   if (level)
      return false;

   *start = *end = &(*s_display_code)[offset];
   found         = false;
   offset--;

   // Look backwards for the start of the function name.
   // TODO Support function pointers.

   while (offset > 0) {
      char c = (*s_display_code)[offset];

      if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == ' ' || (c >= '0' && c <= '9')) {
         // Part of the function name.
         offset--;
      } else {
         *start = &(*s_display_code)[offset + 1];
         found  = true;
         break;
      }
   }

   return found;
}

UIMessage ReceiveMessageRegister(std::function<void(std::unique_ptr<std::string>)> callback) {
   receiveMessageTypes.push_back({._msg = msgReceivedNext, ._callback = std::move(callback)});
   msgReceivedNext = (UIMessage)((uint32_t)msgReceivedNext + 1);
   return receiveMessageTypes.back()._msg;
}

void Context::debugger_thread_fn() {
   int outputPipe[2];
   int inputPipe[2];

   pipe(outputPipe);
   pipe(inputPipe);

#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__APPLE__)
   print(std::cerr, "Using fork\n");
   gdbPID = fork();

   if (gdbPID == 0) {
      setsid();
      dup2(inputPipe[0], 0);    // inputPipe[0]  == stdin
      dup2(outputPipe[1], 1);   // outputPipe[1] == stdout
      dup2(outputPipe[1], 2);   // outputPipe[1] == stderr
      execvp(gdbPath, gdbArgv); // execute gdb with arguments gdbArgv
      print(std::cerr, "Error: Couldn't execute gdb.\n");
      exit(EXIT_FAILURE);

   } else if (gdbPID < 0) {
      print(std::cerr, "Error: Couldn't fork.\n");
      exit(EXIT_FAILURE);
   }
#else
   print(std::cerr, "Using spawn\n");
   posix_spawn_file_actions_t actions = {};
   posix_spawn_file_actions_init(&actions);
   posix_spawn_file_actions_adddup2(&actions, inputPipe[0], 0);  // inputPipe[0]  == stdin
   posix_spawn_file_actions_adddup2(&actions, outputPipe[1], 1); // outputPipe[1] == stdout
   posix_spawn_file_actions_adddup2(&actions, outputPipe[1], 2); // outputPipe[1] == stderr

   posix_spawnattr_t attrs = {};
   posix_spawnattr_init(&attrs);
   posix_spawnattr_setflags(&attrs, POSIX_SPAWN_SETSID);

   posix_spawnp(&_gdb_pid, _gdb_path, &actions, &attrs, _gdb_argv, environ);

   posix_spawn_file_actions_destroy(&actions);
   posix_spawnattr_destroy(&attrs);
#endif

   _pipe_to_gdb = inputPipe[1];

   send_to_gdb(_initial_gdb_command);

   int pipeFromGdb = outputPipe[0];

   fd_set  readfds;
   timeval timeout{0, 10000}; // Wait for 10 ms

   std::string catBuffer;
   while (true) {

      // wait for some data to be available on pipeFromGdb, so we can
      // check for killGdbThread being set
      // ------------------------------------------------------------
      FD_ZERO(&readfds);
      FD_SET(pipeFromGdb, &readfds);
      int result = select(pipeFromGdb + 1, &readfds, nullptr, nullptr, &timeout);

      if (result == -1) { // "Error in select"
         std::cout << "error: " << errno << '\n';
         break;
      } else if (result == 0) { // timeout
         if (_kill_gdb_thread)
            break;
      } else if (FD_ISSET(pipeFromGdb, &readfds)) { // Data is available for reading
         char buffer[512 + 1];
         int  count = read(pipeFromGdb, buffer, 512);
         if (_kill_gdb_thread)
            break;
         if (count <= 0) {
            std::this_thread::sleep_for(std::chrono::microseconds{10000});
            continue;
         }
         buffer[count] = 0;

         // if `logWindow` is set, copy all received output from gdb there as soon
         // as we receive it, even it it is not complete.
         // ----------------------------------------------------------------------
         if (_log_window && !_evaluate_mode)
            s_main_window->post_message(msgReceivedLog, new std::string(buffer));

         if (catBuffer.size() + count + 1 > catBuffer.capacity())
            catBuffer.reserve(catBuffer.capacity() * 2);
         catBuffer.insert(catBuffer.end(), buffer, buffer + count);

         // wait till we get the prompt again so we know we received the complete output
         // ----------------------------------------------------------------------------
         // if (catBuffer.contains("(gdb) ") && !catBuffer.contains("\n(gdb) "))
         //   print("================ got catBuffer=\"{}\"\n", catBuffer);

         // just checking for `"(gdb) "` fails when I'm debugging `gf` itself, as a string containing `"(gdb) "`
         // is passed to `MsgReceivedData` sometimes, and therefore appears on the stack trace.
         // => Be more strict with what markes the end of the debugger output.
         if (!(catBuffer.contains("\n(gdb) ") || catBuffer.contains(">(gdb) ") || catBuffer == "(gdb) ")) {
            continue;
         }
         // print("================ got ({}) catBuffer=\"{}\"\n", evaluateMode, catBuffer);

         // Notify the main thread we have data.
         // ------------------------------------
         if (_evaluate_mode) {
            _evaluate_result_queue.push(std::move(catBuffer));
            _evaluate_mode = false;
         } else {
            s_main_window->post_message(msgReceivedData, new std::string(std::move(catBuffer)));
         }

         catBuffer = std::string{};
      }
   }

   return;
}

void DebuggerStartThread() {
   ctx._gdb_thread = std::thread([]() { ctx.debugger_thread_fn(); });
}

// can be called by: SourceWindowUpdate -> EvaluateExpresion -> EvaluateCommand
// synchronous means we will wait for the debugger output
std::optional<std::string> DebuggerSend(string_view command, bool echo, bool synchronous) {
   std::optional<std::string> res;
   ctx.interrupt_gdb();

   if (synchronous)
      ctx._evaluate_mode = true;

   ctx._program_running = true;

   if (s_trafficlight)
      s_trafficlight->repaint(nullptr);

   // std::cout << "sending: \"" << command << "\"\n";

   if (echo && s_display_output) {
      s_display_output->insert_content(command, false);
      s_display_output->refresh();
   }

   ctx.send_to_gdb(command);

   if (synchronous) {
      bool quit = !ctx._evaluate_result_queue.pop(res);
      if (!res) {
         print("Hit timeout on command \"{}\"\n", command);
         res = std::string{}; // in synchronous mode we always return a (possibly empty) string
      } else {
         ctx._program_running = false;
         if (!quit && s_trafficlight)
            s_trafficlight->repaint(nullptr);
      }
   }
   // print("{} ==> {}\n", command, res ? *res : "???"s);
   return res;
}

std::string EvaluateCommand(string_view command, bool echo = false) {
   auto res = *std::move(DebuggerSend(command, echo, true));
   // print("{} ==> {}\n", command, res);
   return res;
}

std::string EvaluateExpression(string_view expression, string_view format = {}) {
   auto cmd = std::format("p{} {}", format, expression);
   auto res = EvaluateCommand(cmd);
   auto eq  = res.find_first_of('=');
   if (eq != npos) {
      res.erase(0, eq);  // remove characters up to '='
      resize_to_lf(res); // terminate string at '\n'
   }
   return res;
}

void* ControlPipeThread(void*) {
   while (true) {
      FILE* file = fopen(gfc._control_pipe_path, "rb");
      auto  s    = new std::string;
      s->resize(256);
      (*s)[fread(s->data(), 1, 255, file)] = 0;
      s_main_window->post_message(msgReceivedControl, s);
      fclose(file);
   }

   return nullptr;
}

void DebuggerGetStack() {
   auto res = EvaluateCommand(std::format("bt {}", gfc._backtrace_count_limit));
   if (res.empty())
      return;

   sw->clear();

   const char* position = res.c_str();

   while (*position == '#') {
      const char* next = position;

      while (true) {
         next = strchr(next + 1, '\n');
         if (!next || next[1] == '#')
            break;
      }

      if (!next)
         next = position + strlen(position);

      StackEntry entry = {};

      entry._id = strtoul(position + 1, (char**)&position, 0);

      while (*position == ' ' && position < next)
         position++;
      bool hasAddress = *position == '0';

      if (hasAddress) {
         entry._address = strtoul(position, (char**)&position, 0);
         position += 4;
      }

      while (*position == ' ' && position < next)
         position++;
      const char* functionName = position;
      position                 = strchr(position, ' ');
      if (!position || position >= next)
         break;
      std_format_to_n(entry._function, sizeof(entry._function), "{}",
                      std::string_view{functionName, (size_t)(position - functionName)});

      const char* file = strstr(position, " at ");

      if (file && file < next) {
         file += 4;
         const char* end = file;
         while (*end != '\n' && end < next)
            end++;
         std_format_to_n(entry._location, sizeof(entry._location), "{}", std::string_view{file, (size_t)(end - file)});
      }

      sw->append(entry);

      if (!(*next))
         break;
      position = next + 1;
   }
}

struct TabCompleter {
   bool _last_key_was_tab;

private:
   int _consecutive_tab_count;
   int _last_tab_bytes;

public:
   void run(UITextbox* textbox, bool lastKeyWasTab, bool addPrintPrefix) {
      auto text   = textbox->text();
      auto buffer = std::format("complete {}{}", addPrintPrefix ? "p " : "",
                                text.substr(0, lastKeyWasTab ? (size_t)_last_tab_bytes : text.size()));
      for (int i = 0; buffer[i]; i++)
         if (buffer[i] == '\\')
            buffer[i] = ' ';
      auto res = EvaluateCommand(buffer);
      if (res.empty())
         return;

      const char* start = res.c_str();
      const char* end   = strchr(start, '\n');

      if (!lastKeyWasTab) {
         _consecutive_tab_count = 0;
         _last_tab_bytes        = text.size();
      }

      while (start && end && memcmp(start + (addPrintPrefix ? 2 : 0), text.data(), _last_tab_bytes)) {
         start = end + 1;
         end   = strchr(start, '\n');
      }

      for (int i = 0; end && i < _consecutive_tab_count; i++) {
         start = end + 1;
         end   = strchr(start, '\n');
      }

      if (!end) {
         _consecutive_tab_count = 0;
         start                  = res.c_str();
         end                    = strchr(start, '\n');
      }

      _last_key_was_tab = true;
      _consecutive_tab_count++;

      if (end) {
         if (addPrintPrefix)
            start += 2;
         textbox->clear(false);
         textbox->replace_text({start, static_cast<size_t>(end - start)}, false);
         textbox->refresh();
      }
   }
};

// ---------------------------------------------------
// Source display:
// ---------------------------------------------------

struct SourceWindow {
   static UIFont* s_code_font;

   int                    _auto_print_expression_line;
   int                    _auto_print_result_line;
   std::array<char, 1024> _auto_print_expression;
   std::array<char, 1024> _auto_print_result;

   char   _current_file[PATH_MAX];
   char   _current_file_full[PATH_MAX];
   time_t _current_file_read_time;
   bool   _showing_disassembly;

private:
   int _current_end_of_block;
   int _last_cursor_x;
   int _last_cursor_y;

   int _if_condition_evaluation;
   int _if_condition_line;
   int _if_condition_from;
   int _if_condition_to;

   struct InspectResult {
      std::string _expression;
      std::string _value;
   };

   vector<InspectResult> _inspect_results;
   bool                  _no_inspect_results;
   bool                  _in_inspect_line_mode;
   int                   _inspect_mode_restore_line;
   UIRectangle           _display_current_line_bounds;
   const char*           _disassembly_command = "disas /s";

   int _code_message_proc(UICode* code, UIMessage msg, int di, void* dp);
   int _line_message_proc(UIElement* el, UIMessage msg, int di, void* dp);

   void _update(const char* data, UICode* el);

public:
   std::array<char, 1024>& auto_print_result() { return _auto_print_result; }

   bool display_set_position(const char* file, std::optional<size_t> line, bool useGDBToGetFullPath);
   void display_set_position_from_stack();
   void disassembly_load();
   void disassembly_update_line();
   bool toggle_disassembly();
   void draw_inspect_line_mode_overlay(UIPainter* painter);
   void inspect_current_line();
   void exit_inspect_line_mode(UIElement* el);
   bool inspect_line();
   bool set_disassembly_mode();

   static int DisplayCodeMessage(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<SourceWindow*>(el->_cp)->_code_message_proc(static_cast<UICode*>(el), msg, di, dp);
   }

   static int InspectLineModeMessage(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<SourceWindow*>(el->_cp)->_line_message_proc(el, msg, di, dp);
   }

   static UIElement* Create(UIElement* parent) {
      s_source_window = new SourceWindow;
      s_display_code  = &parent->add_code(gfc._selectable_source ? UICode::SELECTABLE : 0)
                           .set_font(s_code_font)
                           .set_cp(s_source_window)
                           .set_user_proc(SourceWindow::DisplayCodeMessage);
      return s_display_code;
   }

   static void Update(const char* data, UIElement* el) {
      static_cast<SourceWindow*>(el->_cp)->_update(data, static_cast<UICode*>(el));
   }
};

UIFont* SourceWindow::s_code_font = nullptr;

static bool DisplaySetPosition(const char* file, std::optional<size_t> line, bool useGDBToGetFullPath) {
   return s_source_window->display_set_position(file, line, useGDBToGetFullPath);
}

// ---------------------------------------------------
// Breakpoints:
// ---------------------------------------------------
std::optional<std::string> DebuggerSend(string_view command, bool echo, bool synchronous);

struct BreakpointMgr {
private:
   struct Breakpoint {
      int      _number = 0;
      char     _file[PATH_MAX];
      char     _full_path[PATH_MAX];
      int      _line       = 0;
      int      _hit        = 0;
      bool     _watchpoint = false;
      bool     _enabled    = false;
      bool     _multiple   = false;
      char     _condition[128];
      uint64_t _condition_hash = 0;

      bool match_path(const char* p) const { return 0 == strcmp(_full_path, p); }
      bool match(int line, const char* path) const { return _line == line && match_path(path); }
   };

   friend struct BreakpointsWindow;

   vector<Breakpoint> _breakpoints; // current debugger breakpoints

public:
   size_t num_breakpoints() const { return _breakpoints.size(); }

   template <class F>
   void for_all_matching_breakpoints(int line, const char* path, F&& f) {
      for (size_t i = 0; i < _breakpoints.size(); i++) {
         if (_breakpoints[i].match(line, path)) {
            std::forward<F>(f)(i, _breakpoints[i]);
         }
      }
   }

   void command(int index, const char* action) {
      Breakpoint* breakpoint = &_breakpoints[index];
      (void)DebuggerSend(std::format("{} {}", action, breakpoint->_number), true, false);
   }

   void toggle_breakpoint(int line = 0) {
      if (s_source_window->_showing_disassembly) {
         // TODO.
         return;
      }

      if (!line) {
         auto currentLine = s_display_code->current_line();
         if (!currentLine)
            return;
         line = *currentLine + 1; // gdb line numbers are 1-indexed
      }

      for (const auto& bp : _breakpoints) {
         if (bp.match(line, s_source_window->_current_file_full)) {
            (void)DebuggerSend(std::format("clear {}:{}", s_source_window->_current_file, line), true, false);
            return;
         }
      }

      (void)DebuggerSend(std::format("b {}:{}", s_source_window->_current_file, line), true, false);
   }

   void update_breakpoint_from_gdb() {
      auto eval_res = EvaluateCommand("info break");
      _breakpoints.clear();

      const char* position = eval_res.c_str();

      while (true) {
         while (true) {
            position = strchr(position, '\n');
            if (!position || isdigit(position[1]))
               break;
            position++;
         }

         if (!position)
            break;

         const char* next = position;

         int number = sv_atoi(position);

         const char* enabledString = strstr(next + 1, " y ");
         bool        enabled       = enabledString && enabledString < strchr(next + 1, '\n');

         while (true) {
            next = strchr(next + 1, '\n');
            if (!next || isdigit(next[1]))
               break;
         }

         if (!next)
            next = position + strlen(position);

         const char* file = strstr(position, " at ");
         if (file)
            file += 4;

         Breakpoint breakpoint = {};
         breakpoint._number    = number;
         breakpoint._enabled   = enabled;

         bool recognised = true;

         const char* condition = strstr(position, "stop only if ");

         if (condition && condition < next) {
            const char* end = strchr(condition, '\n');
            condition += 13;
            std_format_to_n(breakpoint._condition, sizeof(breakpoint._condition), "{}",
                            std::string_view{condition, (size_t)(end - condition)});
            breakpoint._condition_hash = Hash((const uint8_t*)condition, end - condition);
         }

         const char* hitCountNeedle = "breakpoint already hit";
         const char* hitCount       = strstr(position, hitCountNeedle);
         if (hitCount)
            hitCount += strlen(hitCountNeedle);

         if (hitCount && hitCount < next) {
            breakpoint._hit = sv_atoi(hitCount);
         }

         if (file && file < next) {
            const char* end = strchr(file, ':');

            if (end && isdigit(end[1])) {
               if (file[0] == '.' && file[1] == '/')
                  file += 2;
               std_format_to_n(breakpoint._file, sizeof(breakpoint._file), "{}",
                               std::string_view{file, (size_t)(end - file)});
               breakpoint._line = sv_atoi(end, 1);
            } else
               recognised = false;
         } else
            recognised = false;

         if (recognised) {
            realpath(breakpoint._file, breakpoint._full_path);

            for (auto& bp : _breakpoints) {
               if (bp.match(breakpoint._line, breakpoint._full_path) &&
                   bp._condition_hash == breakpoint._condition_hash) {
                  bp._multiple = breakpoint._multiple = true;
                  break;
               }
            }
            if (!breakpoint._multiple)
               _breakpoints.push_back(std::move(breakpoint));
         } else if (strstr(position, "watchpoint") != 0) {
            // we have a watchpoint
            const char* address = strstr(position, enabled ? " y  " : " n  ");
            if (address) {
               address += 2;
               while (*address == ' ')
                  address++;
               if (!isspace(*address)) {
                  const char* end = strchr(address, '\n');
                  if (end) {
                     breakpoint._watchpoint = true;
                     snprintf(breakpoint._file, sizeof(breakpoint._file), "%.*s", (int)(end - address), address);
                     _breakpoints.push_back(std::move(breakpoint));
                  }
               }
            }
         }

         position = next;
      }
   }
};

BreakpointMgr s_breakpoint_mgr; // singletom

// ------------------------------------------------------
// Commands:
// ------------------------------------------------------

std::optional<std::string> CommandParseInternal(string_view command, bool synchronous) {
   std::optional<std::string> res;
   if (command == "gf-step") {
      if (!ctx._program_running)
         res = DebuggerSend(s_source_window->_showing_disassembly ? "stepi" : "s", true, synchronous);
   } else if (command == "gf-next") {
      if (!ctx._program_running)
         res = DebuggerSend(s_source_window->_showing_disassembly ? "nexti" : "n", true, synchronous);
   } else if (command == "gf-step-out-of-block") {
      int line = SourceFindEndOfBlock();

      if (line != -1) {
         (void)DebuggerSend(std::format("until {}", line), true, synchronous);
      }
   } else if (command == "gf-step-into-outer") {
      const char *start, *end;
      bool        found = SourceFindOuterFunctionCall(&start, &end);

      if (found) {
         res = DebuggerSend(std::format("advance {}", string_view(start, (int)(end - start))), true, synchronous);
      } else {
         return CommandParseInternal("gf-step", synchronous);
      }
   } else if (command == "gf-restart-gdb") {
      ctx._first_update = true;
      ctx.kill_gdb();
      DebuggerStartThread();
   } else if (command == "gf-get-pwd") {
      auto        res    = EvaluateCommand("info source");
      const char* needle = "Compilation directory is ";
      const char* pwd    = strstr(res.c_str(), needle);

      if (pwd) {
         pwd += strlen(needle);
         char* end = (char*)strchr(pwd, '\n');
         if (end)
            *end = 0;

         if (!chdir(pwd)) {
            if (s_display_output) {
               s_display_output->insert_content(std::format("New working directory: {}", pwd), false);
               s_display_output->refresh();
            }
         }
         return {};
      }

      s_main_window->show_dialog(0, "Couldn't get the working directory.\n%f%B", "OK");
   } else if (command.starts_with("gf-switch-to ")) {
      ctx.switch_to_window_and_focus(command.substr(13));
   } else if (command.starts_with("gf-command ")) {
      for (const auto& cmd : gfc._preset_commands) {
         if (!cmd._key.starts_with(command.substr(11)))
            continue;
         char* copy     = strdup(cmd._value.c_str());
         char* position = copy;

         while (true) {
            char* end = strchr(position, ';');
            if (end)
               *end = 0;
            char* async = strchr(position, '&');
            if (async && !async[1])
               *async = 0;
            else
               async = nullptr;
            if (synchronous)
               async = nullptr; // Trim the '&' character, but run synchronously anyway.
            res = CommandParseInternal(position, !async);
            if (s_display_output && res)
               s_display_output->insert_content(*res, false);
            if (end)
               position = end + 1;
            else
               break;
         }

         if (s_display_output)
            s_display_output->refresh();
         free(copy);
         break;
      }
   } else if (command == "gf-inspect-line") {
      CommandInspectLine();
   } else if (command == "target remote :1234" && gfc._confirm_command_connect &&
              s_main_window->show_dialog(0, "Connect to remote target?\n%f%B%C", "Connect", "Cancel") == "Cancel") {
   } else if (command == "kill" && gfc._confirm_command_kill &&
              s_main_window->show_dialog(0, "Kill debugging target?\n%f%B%C", "Kill", "Cancel") == "Cancel") {
   } else {
      res = DebuggerSend(command, true, synchronous);
   }

   return res;
}

static void CommandSendToGDB(string_view s) { (void)CommandParseInternal(s, false); }

static void CommandDeleteBreakpoint(int index) { s_breakpoint_mgr.command(index, "delete"); }

static void CommandDisableBreakpoint(int index) { s_breakpoint_mgr.command(index, "disable"); }

static void CommandEnableBreakpoint(int index) { s_breakpoint_mgr.command(index, "enable"); }

static bool CommandSyncWithGvim() {
   char buffer[1024];
   std_format_to_n(buffer, sizeof(buffer), "vim --servername {} --remote-expr \"execute(\\\"ls\\\")\" | grep %%",
                   gfc._vim_server_name);
   FILE* file = popen(buffer, "r");
   if (!file)
      return false;
   buffer[fread(buffer, 1, 1023, file)] = 0;
   pclose(file);
   char* name = strchr(buffer, '"');
   if (!name)
      return false;
   char* nameEnd = strchr(++name, '"');
   if (!nameEnd)
      return false;
   *nameEnd   = 0;
   char* line = nameEnd + 1;
   while (!isdigit(*line) && *line)
      line++;
   if (!line)
      return false;
   int  lineNumber = sv_atoi(line);
   char buffer2[PATH_MAX];

   if (name[0] != '/' && name[0] != '~') {
      char buffer[1024];
      std_format_to_n(buffer, sizeof(buffer), "vim --servername {} --remote-expr \"execute(\\\"pwd\\\")\" | grep '/'",
                      gfc._vim_server_name);
      FILE* file = popen(buffer, "r");
      if (!file)
         return false;
      buffer[fread(buffer, 1, 1023, file)] = 0;
      pclose(file);
      if (!strchr(buffer, '\n'))
         return false;
      *strchr(buffer, '\n') = 0;
      std_format_to_n(buffer2, sizeof(buffer2), "{}/{}", buffer, name);
   } else {
      strcpy(buffer2, name);
   }

   DisplaySetPosition(buffer2, lineNumber - 1, false); // lines in vi are 1-based
   return true;
}

static void CommandCustom(string_view command) {

   if (command.starts_with("shell ")) {
      // TODO Move this into CommandParseInternal?

      if (s_display_output)
         s_display_output->insert_content(std::format("Running shell command \"{}\"...\n", command), false);
      int         start  = time(nullptr);
      int         result = system(std::format("{} > .output.gf 2>&1", command).c_str());
      std::string output = LoadFile(".output.gf");
      size_t      bytes  = output.size();
      unlink(".output.gf");

      std::string copy;
      copy.resize_and_overwrite(output.size() + 1, [](char*, size_t sz) { return sz; });

      uintptr_t j = 0;

      for (size_t i = 0; i <= bytes;) {
         if ((uint8_t)output[i] == 0xE2 && (uint8_t)output[i + 1] == 0x80 &&
             ((uint8_t)output[i + 2] == 0x98 || (uint8_t)output[i + 2] == 0x99)) {
            copy[j++] = '\'';
            i += 3;
         } else {
            copy[j++] = output[i++];
         }
      }

      if (s_display_output) {
         s_display_output->insert_content({copy.data(), j}, false);
         s_display_output->insert_content(
            std::format("(exit code: {}; time: {}s)\n", result, (int)(time(nullptr) - start)), false);
         s_display_output->refresh();
      }
   } else {
      (void)CommandParseInternal(command, false);
   }
}

// ------------------------------------------------------
// Settings:
// ------------------------------------------------------

const char* themeItems[] = {
   "panel1",         "panel2",       "selected",         "border",        "text",           "textDisabled",
   "textSelected",   "buttonNormal", "buttonHovered",    "buttonPressed", "buttonDisabled", "textboxNormal",
   "textboxFocused", "codeFocused",  "codeBackground",   "codeDefault",   "codeComment",    "codeString",
   "codeNumber",     "codeOperator", "codePreprocessor", "accent1",       "accent2",
};

static void SettingsAddTrustedFolder() {
   std::string config         = LoadFile(gfc._global_config_path);
   const char* section_string = "\n[trusted_folders]\n";
   auto        insert_pos     = config.find(section_string);

   if (insert_pos == std::string::npos) {
      config += section_string;
      insert_pos = config.size();
   } else {
      insert_pos += strlen(section_string);
   }

   std::ofstream ofs(gfc._global_config_path, std::ofstream::out | std::ofstream::binary);
   if (!ofs)
      print(std::cerr, "Error: Could not modify the global config file!\n");
   else {
      ofs << config.substr(0, insert_pos);
      ofs << gfc._local_config_dir << '\n';
      ofs << config.substr(insert_pos, config.size());
   }
}

// ------------------------------------------------------------------------------
// load settings (from both global and local config files) in two passes:
//
// earlypass:
//   +  "ui"
//   +  "commands"
//   +  "trusted_folders"
//   +  "vim"
//   +  "pipe"
//   +  "executable"
//
// !earlypass:
//   +  "shortcuts"
//   +  "gdb"
//   +  "theme"
// ------------------------------------------------------------------------------
UIConfig Context::load_settings(bool earlyPass) {
   bool        currentFolderIsTrusted = false;
   static bool cwdConfigNotTrusted    = false;
   UIConfig    ui_config;

   // load global config first (from ~/.config/gf2_config.ini), and then local config
   for (int i = 0; i < 2; i++) {
      const auto config = LoadFile(i ? gfc._local_config_path : gfc._global_config_path);

      if (earlyPass && i && !currentFolderIsTrusted && !config.empty()) {
         print(std::cerr, "Would you like to load the config file .project.gf from your current directory?\n");
         print(std::cerr, "You have not loaded this config file before.\n");
         print(std::cerr, "(Y) - Yes, and add it to the list of trusted files\n");
         print(std::cerr, "(N) - No\n");
         char c = 'n';
         fread(&c, 1, 1, stdin);

         if (c != 'y' && c != 'Y') {
            cwdConfigNotTrusted = true;
            break;
         } else {
            SettingsAddTrustedFolder();
         }
      } else if (!earlyPass && cwdConfigNotTrusted && i) {
         break;
      }

      INI_Parser config_view(config);

      for (auto parse_res : config_view) {
         auto [section, key, value] = parse_res;
         if (section.empty())
            break;

         if (!key.empty() && key[0] == '#')
            continue;

         if (section == "shortcuts" && !key.empty() && !earlyPass) {
            UIShortcut shortcut;

            std::string k(key);
            std::transform(k.begin(), k.end(), k.begin(), [](unsigned char c) { return std::tolower(c); });

            shortcut.ctrl   = k.contains("ctrl+");
            shortcut.shift  = k.contains("shift+");
            shortcut.alt    = k.contains("alt+");
            shortcut.invoke = [cmd = std::string(value)]() {
               CommandCustom(cmd);
               return true;
            };

            const char* codeStart = k.c_str();

            for (int i = 0; k[i]; i++) {
               if (k[i] == '+')
                  codeStart = k.c_str() + i + 1;
            }

            for (int i = 0; i < 26; i++) {
               if (codeStart[0] == 'a' + i && codeStart[1] == 0) {
                  shortcut.code = UIKeycode((int)UIKeycode::A + i);
               }
            }

            for (int i = 0; i < 10; i++) {
               if (codeStart[0] == '0' + i && codeStart[1] == 0) {
                  shortcut.code = UIKeycode((int)UIKeycode::ZERO + i);
               }
            }

            for (int i = 1; i <= 12; i++) {
               if (codeStart[0] == 'f' && isdigit(codeStart[1]) && sv_atoi(codeStart, 1) == i) {
                  shortcut.code = UI_KEYCODE_FKEY(i);
               }
            }

            if ((int)shortcut.code == 0) {
               print(std::cerr, "Warning: Could not register shortcut for '{}'.\n", key);
            } else {
               s_main_window->register_shortcut(std::move(shortcut));
            }
         } else if (section == "ui" && !key.empty() && earlyPass) {
            if (key == "font_size") {
               gfc._interface_font_size = gfc._code_font_size = sv_atoi(value);
            } else {
               // clang-format off
               parse_res.parse_str  ("font_path", ui_config.font_path) ||
               parse_res.parse_int  ("font_size_code", gfc._code_font_size) ||
               parse_res.parse_int  ("font_size_interface", gfc._interface_font_size) ||
               parse_res.parse_float("scale", gfc._ui_scale) ||
               parse_res.parse_str  ("layout", gfc._layout_string) ||
               parse_res.parse_bool ("maximize", gfc._maximize) ||
               parse_res.parse_bool ("restore_watch_window", gfc._restore_watch_window) ||
               parse_res.parse_bool ("selectable_source", gfc._selectable_source) ||
               parse_res.parse_int  ("window_width", gfc._window_width) ||
               parse_res.parse_int  ("window_height", gfc._window_height) ||
               parse_res.parse_bool ("grab_focus_on_breakpoint", gfc._grab_focus_on_breakpoint);
               // clang-format on
            }
         } else if (section == "gdb" && !key.empty() && !earlyPass) {
            if (key == "argument") {
               ctx._gdb_argc++;
               ctx._gdb_argv                    = (char**)realloc(ctx._gdb_argv, sizeof(char*) * (ctx._gdb_argc + 1));
               ctx._gdb_argv[ctx._gdb_argc - 1] = mk_cstring(value);
               ctx._gdb_argv[ctx._gdb_argc]     = nullptr;
            } else if (key == "arguments") {
               char buffer[2048];
               auto sz  = value.size();
               auto val = value.data();

               for (size_t i = 0; i < sz; i++) {
                  if (isspace(val[i])) {
                     continue;
                  }

                  size_t argumentStart = 0;
                  size_t argumentEnd   = 0;

                  if (val[i] == '\"') {
                     i++;
                     argumentStart = i;
                     for (; i < sz && val[i] != '\"'; i++)
                        ;
                     argumentEnd = i;
                     i++;
                  } else if (val[i] == '\'') {
                     i++;
                     argumentStart = i;
                     for (; i < sz && val[i] != '\''; i++)
                        ;
                     argumentEnd = i;
                     i++;
                  } else {
                     argumentStart = i;
                     i++;
                     for (; i < sz && (val[i] != '\'' && val[i] != '\"' && !isspace(val[i])); i++)
                        ;
                     argumentEnd = i;
                  }

                  std_format_to_n(buffer, sizeof(buffer), "{}",
                                  std::string_view{&val[argumentStart], (size_t)(argumentEnd - argumentStart)});

                  ctx._gdb_argc++; // 0 is for the program name
                  ctx._gdb_argv = (char**)realloc(ctx._gdb_argv, sizeof(char*) * (ctx._gdb_argc + 1));
                  ctx._gdb_argv[ctx._gdb_argc - 1] = strdup(buffer);
                  ctx._gdb_argv[ctx._gdb_argc]     = nullptr;
               }
            } else if (key == "path") {
               char* path       = mk_cstring(value);
               ctx._gdb_path    = path;
               ctx._gdb_argv[0] = path;
            } else if (key == "log_all_output" && sv_atoi(value)) {
               if (auto it = _interface_windows.find("Log"); it != _interface_windows.end()) {
                  const auto& [name, window] = *it;
                  ctx._log_window            = static_cast<UICode*>(window._el);
               }
               if (!ctx._log_window) {
                  print(std::cerr, "Warning: gdb.log_all_output was enabled, "
                                   "but your layout does not have a 'Log' window.\n");
               }
            } else {
               // clang-format off
               parse_res.parse_bool("confirm_command_kill", gfc._confirm_command_kill) ||
               parse_res.parse_bool("confirm_command_connect", gfc._confirm_command_connect) ||
               parse_res.parse_int ("backtrace_count_limit", gfc._backtrace_count_limit);
               // clang-format on
            }
         } else if (section == "commands" && earlyPass && !key.empty() && !value.empty()) {
            gfc._preset_commands.push_back(Command{._key = std::string(key), ._value = std::string(value)});
         } else if (section == "trusted_folders" && earlyPass && !key.empty()) {
            if (key == gfc._local_config_dir)
               currentFolderIsTrusted = true;
         } else if (section == "theme" && !earlyPass && !key.empty() && !value.empty()) {
            for (uintptr_t i = 0; i < sizeof(themeItems) / sizeof(themeItems[0]); i++) {
               if (key != themeItems[i])
                  continue;
               std::from_chars(value.data(), value.data() + value.size(), ((uint32_t*)&ui_config._theme)[i], 16);
               ui_config._has_theme = true;
            }
            if (key == "predefined") {
               if (auto itr = ui_themes.find(std::string(value)); itr != ui_themes.end()) {
                  ui_config._theme     = itr->second;
                  ui_config._has_theme = true;
               }
            }
         } else if (section == "vim" && earlyPass && key == "server_name") {
            gfc._vim_server_name = mk_cstring(value);
         } else if (section == "pipe" && earlyPass && key == "log") {
            gfc._log_pipe_path = mk_cstring(value);
            mkfifo(gfc._log_pipe_path, 6 + 6 * 8 + 6 * 64);
         } else if (section == "pipe" && earlyPass && key == "control") {
            gfc._control_pipe_path = mk_cstring(value);
            mkfifo(gfc._control_pipe_path, 6 + 6 * 8 + 6 * 64);
            pthread_t thread;
            pthread_create(&thread, nullptr, ControlPipeThread, nullptr);
         } else if (section == "executable" && !key.empty() && earlyPass) {
            // clang-format off
            parse_res.parse_str ("path", gfc._exe_path) ||
            parse_res.parse_str ("arguments", gfc._exe_args) ||
            parse_res.parse_bool("ask_directory", gfc._exe_ask_dir);
            // clang-format on
         } else if (earlyPass && !section.empty() && !key.empty() && !value.empty()) {
            if (auto it = _interface_windows.find(std::string(section)); it != _interface_windows.end()) {
               const auto& [name, window] = *it;
               if (window._config) {
                  window._config(key, value);
               }
            }
         }
      }
   }
   return ui_config;
}

// ------------------------------------------------------
// Debug windows:
// ------------------------------------------------------

// ---------------------------------------------------
// Source display:
// ---------------------------------------------------

// --------------------------------
// `line`, if present, is `0-based`
// --------------------------------
bool SourceWindow::display_set_position(const char* file, std::optional<size_t> line, bool useGDBToGetFullPath) {
   if (_showing_disassembly) {
      return false;
   }

   char        buffer[4096];
   const char* originalFile = file;

   if (file && file[0] == '~') {
      std_format_to_n(buffer, sizeof(buffer), "{}/{}", getenv("HOME"), 1 + file);
      file = buffer;
   } else if (file && file[0] != '/' && useGDBToGetFullPath) {
      auto        res = EvaluateCommand("info source");
      const char* f   = strstr(res.c_str(), "Located in ");

      if (f) {
         f += 11;
         const char* end = strchr(f, '\n');

         if (end) {
            std_format_to_n(buffer, sizeof(buffer), "{}", std::string_view{f, (size_t)(end - f)});
            file = buffer;
         }
      }
   }

   bool reloadFile = false;

   if (file) {
      if (strcmp(_current_file, file)) {
         reloadFile = true;
      }

      struct stat buf;

      if (!stat(file, &buf) && buf.st_mtime != _current_file_read_time) {
         reloadFile = true;
      }

      _current_file_read_time = buf.st_mtime;
   }

   bool changed = false;

   if (reloadFile) {
      std_format_to_n(_current_file, 4096, "{}", file);
      realpath(_current_file, _current_file_full);

      s_main_window->set_name(_current_file_full);

      s_display_code->load_file(file,
                                std::format("The file '{}' (from '{}') could not be loaded.", file, originalFile));

      changed               = true;
      _auto_print_result[0] = 0;
   }

   auto currentLine = s_display_code->current_line();
   if (line && (!currentLine || currentLine != line)) {
      s_display_code->set_current_line(*line);
      s_display_code->set_focus_line(*line);
      changed = true;
   }

   _current_end_of_block = SourceFindEndOfBlock();
   s_display_code->refresh();

   return changed;
}

void SourceWindow::display_set_position_from_stack() {
   if (sw->has_selection()) {
      char  location[sizeof(previousLocation)];
      auto& current = sw->current();
      strcpy(previousLocation, current._location);
      strcpy(location, current._location);
      char*                 line = strchr(location, ':');
      std::optional<size_t> position;
      if (line) {
         *line    = 0;
         position = sv_atoul(line + 1) - 1; // lines in gdb are 1-based
      }
      display_set_position(location, position, true);
   }
}

void SourceWindow::disassembly_load() {
   auto res = EvaluateCommand(_disassembly_command);

   if (!res.contains("Dump of assembler code for function")) {
      res = EvaluateCommand("disas $pc,+1000");
   }

   const char* end = strstr(res.c_str(), "End of assembler dump.");

   if (!end) {
      print("Disassembly failed. GDB output:\n{}\n", res);
      return;
   }

   const char* start = strstr(res.c_str(), ":\n");

   if (!start) {
      print("Disassembly failed. GDB output:\n{}\n", res);
      return;
   }

   start += 2;

   if (start >= end) {
      print("Disassembly failed. GDB output:\n{}\n", res);
      return;
   }

   char* pointer = (char*)strstr(start, "=> ");

   if (pointer) {
      pointer[0] = ' ';
      pointer[1] = ' ';
   }

   s_display_code->insert_content({start, static_cast<size_t>(end - start)}, true);
}

void SourceWindow::disassembly_update_line() {
   auto        res     = EvaluateCommand("p $pc");
   const char* address = strstr(res.c_str(), "0x");

   if (address) {
      uint64_t a = strtoul(address, nullptr, 0);

      for (int i = 0; i < 2; i++) {
         // Look for the line in the disassembly.

         bool found = false;

         size_t num_lines = s_display_code->num_lines();
         for (size_t i = 0; i < num_lines; i++) {
            uint64_t b = sv_atoul(s_display_code->line(i), 3);

            if (a == b) {
               s_display_code->set_focus_line(i);
               _auto_print_expression_line = i;
               found                       = true;
               break;
            }
         }

         if (!found) {
            // Reload the disassembly.
            disassembly_load();
         } else {
            break;
         }
      }

      s_display_code->refresh();
   }
}

bool SourceWindow::toggle_disassembly() {
   _showing_disassembly      = !_showing_disassembly;
   _auto_print_result_line   = 0;
   _auto_print_expression[0] = 0;
   s_display_code->_flags ^= UICode::NO_MARGIN;

   if (_showing_disassembly) {
      s_display_code->insert_content("Disassembly could not be loaded.\nPress Ctrl+D to return to source view.", true);
      s_display_code->set_tab_columns(8);
      disassembly_load();
      disassembly_update_line();
   } else {
      s_display_code->set_current_line({});
      _current_end_of_block   = -1;
      _current_file[0]        = 0;
      _current_file_read_time = 0;
      display_set_position_from_stack();
      s_display_code->set_tab_columns(4);
   }

   s_display_code->refresh();
   return true;
}

bool SourceWindow::set_disassembly_mode() {
   auto newMode = s_main_window->show_dialog(0, "Select the disassembly mode:\n%b\n%b\n%b", "Disassembly only",
                                             "With source", "Source centric");

   if (newMode == "Disassembly only")
      _disassembly_command = "disas";
   else if (newMode == "With source")
      _disassembly_command = "disas /s";
   else if (newMode == "Source centric")
      _disassembly_command = "disas /m";

   if (_showing_disassembly) {
      toggle_disassembly();
      toggle_disassembly();
   }
   return true;
}

bool CommandSetDisassemblyMode() { return s_source_window->set_disassembly_mode(); }

void SourceWindow::draw_inspect_line_mode_overlay(UIPainter* painter) {
   const auto& theme       = painter->theme();
   auto        active_font = painter->active_font();

   const char* instructions = "(Press Esc to exit inspect line mode.)";
   int         width        = (strlen(instructions) + 8) * active_font->_glyph_width;

   for (const auto& ir : _inspect_results) {
      int w = (ir._expression.size() + ir._value.size() + 8) * active_font->_glyph_width;
      if (w > width)
         width = w;
   }

   int  xOffset     = 0;
   auto currentLine = s_display_code->current_line();
   if (!currentLine)
      return;

   std::string_view cur_line = s_display_code->line(*currentLine);
   for (auto c : cur_line) {
      if (c == '\t' || c == ' ') {
         xOffset += (c == '\t' ? 4 : 1) * active_font->_glyph_width;
      } else {
         break;
      }
   }

   int         lineHeight = painter->ui()->string_height();
   UIRectangle bounds     = _display_current_line_bounds +
                        UIRectangle(xOffset, 0, lineHeight, 8 + lineHeight * (_inspect_results.size() / 2 + 1));
   bounds.r = bounds.l + width;
   painter->draw_block(bounds + UIRectangle(3), theme.border);
   painter->draw_rectangle(bounds, theme.codeBackground, theme.border, UIRectangle(2));
   UIRectangle line = bounds + UIRectangle(4, -4, 4, 0);
   line.b           = line.t + lineHeight;
   std::string buffer;

   size_t index = 0;
   for (const auto& ir : _inspect_results) {
      if (_no_inspect_results) {
         buffer = ir._expression;
      } else if (index < 9) {
         buffer = std::format("[{}] {} {}", index + 1, ir._expression, ir._value);
      } else {
         buffer = std::format("    {} {}", ir._expression, ir._value);
      }

      painter->draw_string(line, buffer, _no_inspect_results ? theme.codeOperator : theme.codeString, UIAlign::left,
                           nullptr);
      line = line + UIRectangle(0, lineHeight);
      ++index;
   }

   painter->draw_string(line, instructions, theme.codeNumber, UIAlign::right, nullptr);
}

void CommandDeleteAllBreakpointsOnLine(int line) {
   s_breakpoint_mgr.for_all_matching_breakpoints(line, s_source_window->_current_file_full,
                                                 [](int line, auto&) { CommandDeleteBreakpoint(line); });
}

void CommandDisableAllBreakpointsOnLine(int line) {
   s_breakpoint_mgr.for_all_matching_breakpoints(line, s_source_window->_current_file_full,
                                                 [](int line, auto&) { CommandDisableBreakpoint(line); });
}

void CommandEnableAllBreakpointsOnLine(int line) {
   s_breakpoint_mgr.for_all_matching_breakpoints(line, s_source_window->_current_file_full,
                                                 [](int line, auto&) { CommandEnableBreakpoint(line); });
}

int SourceWindow::_code_message_proc(UICode* code, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::CLICKED && !_showing_disassembly) {
      int result = code->hittest(code->_window->cursor_pos());

      if (result < 0 && code->left_down_in_margin()) {
         int line = -result;
         s_breakpoint_mgr.toggle_breakpoint(line);
      } else if (result > 0 && !code->left_down_in_margin()) {
         int line = result;

         if (code->_window->_ctrl) {
            (void)DebuggerSend(std::format("until {}", line), true, false);
         } else if (code->_window->_alt || code->_window->_shift) {
            EvaluateCommand(std::format("tbreak {}", line));
            (void)DebuggerSend(std::format("jump {}", line), true, false);
         }
      }
   } else if (msg == UIMessage::LEFT_DBLCLICK && !code->empty()) {
      int hitTest = code->hittest(code->cursor_pos());

      if (hitTest > 0) {
         auto cursor_pt = code->cursor_pos();
         auto pos       = code->code_pos_from_point(cursor_pt);
         auto line      = code->line(pos.line);

         auto bounds = ctx._lang_re->find_symbol_at_pos(line, pos.offset);
         if (bounds) {
            auto [start, end] = *bounds;
            code->set_selection(2, pos.line, start); // anchor
            code->set_selection(3, pos.line, end);   // caret
            code->update_selection();
            code->refresh();
            return 1;
         }
         return 0;
      }
   } else if (msg == UIMessage::RIGHT_DOWN && !_showing_disassembly) {
      int result = code->hittest(code->cursor_pos());

      bool atLeastOneBreakpointEnabled = false;

      s_breakpoint_mgr.for_all_matching_breakpoints(-result, _current_file_full, [&](int line, auto& bp) {
         if (bp._enabled)
            atLeastOneBreakpointEnabled = true;
      });

      s_breakpoint_mgr.for_all_matching_breakpoints(-result, _current_file_full, [&](int line, auto&) {
         UIMenu& menu = code->ui()->create_menu(code->_window, UIMenu::NO_SCROLL).add_item(0, "Delete", [=](UIButton&) {
            CommandDeleteAllBreakpointsOnLine(-result);
         });
         if (atLeastOneBreakpointEnabled)
            menu.add_item(0, "Disable", [=](UIButton&) { CommandDisableAllBreakpointsOnLine(-result); });
         else
            menu.add_item(0, "Enable", [=](UIButton&) { CommandEnableAllBreakpointsOnLine(-result); });
         menu.show();
      });
   } else if (msg == UIMessage::CODE_GET_MARGIN_COLOR && !_showing_disassembly) {
      const auto& theme       = code->theme();
      int         num_enabled = 0, num_disabled = 0;

      s_breakpoint_mgr.for_all_matching_breakpoints(di, _current_file_full, [&](int line, auto& bp) {
         if (bp._enabled)
            ++num_enabled;
         else
            ++num_disabled;
      });

      if (num_disabled) {
         return (((theme.accent1 & 0xFF0000) >> 1) & 0xFF0000) | (((theme.accent1 & 0xFF00) >> 1) & 0xFF00) |
                ((theme.accent1 & 0xFF) >> 1);
      }
      return num_enabled ? theme.accent1 : 0;
   } else if (msg == UIMessage::PAINT) {
      code->_class_proc(code, msg, di, dp);

      if (_in_inspect_line_mode) {
         UIFont* previousFont = code->font()->activate();
         draw_inspect_line_mode_overlay((UIPainter*)dp);
         previousFont->activate();
      }

      return 1;
   } else if (msg == UIMessage::CODE_DECORATE_LINE) {
      auto&               theme       = code->theme();
      auto                active_font = code->active_font();
      UICodeDecorateLine* m           = (UICodeDecorateLine*)dp;
      auto                currentLine = s_display_code->current_line();

      if (currentLine && m->index == (int)*currentLine) {
         _display_current_line_bounds = m->bounds;
      }

      if (m->index == _auto_print_result_line) {
         UIRectangle rectangle =
            UIRectangle(m->x + active_font->_glyph_width, m->bounds.r, m->y, m->y + code->ui()->string_height());
         m->painter->draw_string(rectangle, _auto_print_result.data(), theme.codeComment, UIAlign::left, nullptr);
      }

      if (code->hittest(code->cursor_pos()) == m->index && code->is_hovered() &&
          (code->_window->_ctrl || code->_window->_alt || code->_window->_shift) &&
          !code->_window->textbox_modified_flag()) {
         m->painter->draw_border(m->bounds, code->_window->_ctrl ? theme.selected : theme.codeOperator, UIRectangle(2));
         m->painter->draw_string(m->bounds, code->_window->_ctrl ? "=> run until " : "=> skip to ", theme.text,
                                 UIAlign::right, nullptr);
      } else if (m->index == _current_end_of_block) {
         m->painter->draw_string(m->bounds, "[Shift+F10]", theme.codeComment, UIAlign::right, nullptr);
      }

      if (m->index == _if_condition_line && _if_condition_evaluation) {
         int columnFrom = code->byte_to_column(_if_condition_line, _if_condition_from);
         int columnTo   = code->byte_to_column(_if_condition_line, _if_condition_to);
         m->painter->draw_block(UIRectangle(m->bounds.l + columnFrom * active_font->_glyph_width,
                                            m->bounds.l + columnTo * active_font->_glyph_width, m->bounds.b - 2,
                                            m->bounds.b),
                                _if_condition_evaluation == 2 ? theme.accent2 : theme.accent1);
      }
   } else if (msg == UIMessage::MOUSE_MOVE || msg == UIMessage::UPDATE) {
      auto pos = code->cursor_pos();
      if (pos.x != _last_cursor_x || pos.y != _last_cursor_y) {
         _last_cursor_x = pos.x;
         _last_cursor_y = pos.y;
         code->_window->set_textbox_modified_flag(false);
      }

      code->refresh();
   }

   return 0;
}

void SourceWindow::_update(const char* data, UICode* el) {
   bool changedSourceLine = false;

   const char* line = data;

   while (*line) {
      if (line[0] == '\n' || line == data) {
         int i = line == data ? 0 : 1, number = 0;

         while (line[i]) {
            if (line[i] == '\t') {
               break;
            } else if (isdigit(line[i])) {
               number = number * 10 + line[i] - '0';
               i++;
            } else {
               goto tryNext;
            }
         }

         if (!line[i])
            break;
         if (number)
            changedSourceLine = true;
      tryNext:;
         line += i + 1;
      } else {
         line++;
      }
   }

   if (!sw->changed() && changedSourceLine)
      sw->set_selected(0);
   sw->set_changed(false);

   if (changedSourceLine && sw->has_selection() && strcmp(sw->current()._location, previousLocation)) {
      display_set_position_from_stack();
   }

   auto currentLine = s_display_code->current_line();
   if (changedSourceLine && currentLine) {
      // If there is an auto-print expression from the previous line, evaluate it.
#if 0
      if (autoPrintExpression[0]) {
         auto        res    = EvaluateCommand(std::format("p {}", autoPrintExpression));
         const char* result = strchr(res.c_str(), '=');

         if (result) {
            autoPrintResultLine = autoPrintExpressionLine;
            std_format_to_n(autoPrintResult, sizeof(autoPrintResult), "{}", result);
            char* end = strchr(autoPrintResult, '\n');
            if (end)
               *end = 0;
         } else {
            autoPrintResult[0] = 0;
         }

         autoPrintExpression[0] = 0;
      }
#endif

      // Parse the new source line.
      std::string_view text_sv  = s_display_code->line(*currentLine);
      size_t           bytes    = text_sv.size();
      const char*      text     = &text_sv[0];
      uintptr_t        position = 0;

      while (position < bytes) {
         if (text[position] != '\t')
            break;
         else
            position++;
      }

      uintptr_t expressionStart = position;

      {
         // Try to parse a type name.

         uintptr_t position2 = position;

         while (position2 < bytes) {
            char c = text[position2];
            if (!UI::is_alnum_or_underscore(c))
               break;
            else
               position2++;
         }

         if (position2 == bytes)
            goto noTypeName;
         if (text[position2] != ' ')
            goto noTypeName;
         position2++;

         while (position2 < bytes) {
            if (text[position2] != '*')
               break;
            else
               position2++;
         }

         if (position2 == bytes)
            goto noTypeName;
         if (!UI::is_alnum_or_underscore(text[position2]))
            goto noTypeName;

         position = expressionStart = position2;
      noTypeName:;
      }

      while (position < bytes) {
         char c = text[position];
         if (!UI::is_alnum_or_underscore(c) && c != '[' && c != ']' && c != ' ' && c != '.' && c != '-' && c != '>')
            break;
         else
            position++;
      }

      uintptr_t expressionEnd = position;

      while (position < bytes) {
         if (text[position] != ' ')
            break;
         else
            position++;
      }

      if (position != bytes && text[position] == '=') {
         std_format_to_n(&_auto_print_expression[0], sizeof(_auto_print_expression), "{}",
                         std::string_view{text + expressionStart, expressionEnd - expressionStart});
      }

      _auto_print_expression_line = *currentLine + 1;

      // Try to evaluate simple if conditions.

      _if_condition_evaluation = 0;

      for (uintptr_t i = 0, phase = 0, expressionStart = 0, depth = 0; i < bytes; i++) {
         if (phase == 0) {
            if (text[i] == ' ' || text[i] == '\t' || text[i] == '}') {
            } else if (i < bytes - 4 && text[i] == 'e' && text[i + 1] == 'l' && text[i + 2] == 's' &&
                       text[i + 3] == 'e' && text[i + 4] == ' ') {
               i += 4;
            } else if (i < bytes - 2 && text[i] == 'i' && text[i + 1] == 'f' &&
                       (text[i + 2] == ' ' || text[i + 2] == '(')) {
               phase = 1;
            } else {
               break;
            }
         } else if (phase == 1) {
            if (text[i] == '(') {
               phase           = 2;
               expressionStart = i + 1;
            }
         } else if (phase == 2) {
            if (text[i] == '(') {
               if ((i > 3 && text[i - 3] == '|' && text[i - 2] == '|' && text[i - 1] == ' ') ||
                   (i > 3 && text[i - 3] == '&' && text[i - 2] == '&' && text[i - 1] == ' ') ||
                   (i > 2 && text[i - 2] == '|' && text[i - 1] == '|') ||
                   (i > 2 && text[i - 2] == '&' && text[i - 1] == '&') ||
                   (i > 2 && text[i - 2] == '!' && text[i - 1] == ' ') || (i > 1 && text[i - 1] == '!') ||
                   (i > 6 && text[i - 6] == 's' && text[i - 5] == 't' && text[i - 4] == 'r' && text[i - 3] == 'c' &&
                    text[i - 2] == 'm' && text[i - 1] == 'p')) {
                  depth++;
               } else {
                  // Don't evaluate function calls.
                  break;
               }
            } else if (i > 1 && i < bytes - 1 && text[i] == '=' && text[i - 1] != '>' && text[i - 1] != '<' &&
                       text[i - 1] != '=' && text[i + 1] != '=') {
               // Don't evaluate assignments.
               break;
            } else if (text[i] == ')' && depth) {
               depth--;
            } else if (text[i] == ')' && !depth) {
               (void)expressionStart;
#if 0
               auto res = EvaluateExpression(string_view{&text[expressionStart], i - expressionStart});

               if (res == "= true") {
                  ifConditionEvaluation = 2;
                  ifConditionFrom = expressionStart, ifConditionTo = i;
                  ifConditionLine = *currentLine;
               } else if (res == "= false") {
                  ifConditionEvaluation = 1;
                  ifConditionFrom = expressionStart, ifConditionTo = i;
                  ifConditionLine = *currentLine;
               }
#endif
               break;
            }
         }
      }
   }

   el->refresh();
}

bool InspectIsTokenCharacter(char c) { return isalpha(c) || c == '_'; }

void SourceWindow::inspect_current_line() {
   _inspect_results.clear();
   auto currentLine = s_display_code->current_line();
   if (!currentLine)
      return;

   auto code = s_display_code->line(*currentLine);

   auto expressions = ctx._lang_re->extract_debuggable_expressions(code);
   for (auto e : expressions) {
      auto res = EvaluateExpression(e);
      // std::cout << "eval(\"" << e << "\") -> " << res << '\n';

      if (ctx._dbg_re->evaluation_error(res))
         continue;

      if (0 == memcmp(res.c_str(), "= {", 3) && !strchr(res.c_str() + 3, '='))
         continue;

      _inspect_results.emplace_back(std::string{e}, res);
   }

   if (!_inspect_results.size()) {
      _inspect_results.emplace_back("No expressions to display.", "");
   } else {
      _no_inspect_results = false;
   }
}

void SourceWindow::exit_inspect_line_mode(UIElement* el) {
   el->destroy();
   s_input_textbox->focus();
   _in_inspect_line_mode = false;
   s_display_code->set_current_line(_inspect_mode_restore_line);
   s_display_code->set_focus_line(_inspect_mode_restore_line);
   s_display_code->refresh();
}

int SourceWindow::_line_message_proc(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::UPDATE && !el->is_focused()) {
      exit_inspect_line_mode(el);
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if ((m->text == "`") || m->code == UIKeycode::ESCAPE) {
         exit_inspect_line_mode(el);
      } else if (m->code >= UI_KEYCODE_DIGIT('1') && m->code <= UI_KEYCODE_DIGIT('9')) {
         int index = ((int)m->code - (int)UI_KEYCODE_DIGIT('1'));

         if (index < (int)_inspect_results.size()) {
            exit_inspect_line_mode(el);
            WatchAddExpression(_inspect_results[index]._expression);
         }
      } else {
         auto currentLine = s_display_code->current_line();
         if (!currentLine)
            return 0;
         if ((m->code == UIKeycode::UP && *currentLine != 0) ||
             (m->code == UIKeycode::DOWN && *currentLine + 1 < s_display_code->num_lines())) {
            *currentLine += m->code == UIKeycode::UP ? -1 : 1;
            s_display_code->set_current_line(*currentLine);
            s_display_code->set_focus_line(*currentLine);
            inspect_current_line();
            s_display_code->refresh();
         }
      }

      return 1;
   }

   return 0;
}

bool SourceWindow::inspect_line() {
   auto currentLine = s_display_code->current_line();
   if (!currentLine)
      return false;

   _inspect_mode_restore_line = *currentLine;
   _in_inspect_line_mode      = true;
   inspect_current_line();
   s_display_code->refresh();

   // Create an element to receive key input messages.
   s_main_window->add_element(0, InspectLineModeMessage, 0).set_cp(s_display_code->_cp).focus();
   return true;
}

bool CommandInspectLine() { return static_cast<SourceWindow*>(s_display_code->_cp)->inspect_line(); }

// ---------------------------------------------------/
// Data viewers:
// ---------------------------------------------------/

struct AutoUpdateViewer {
   UIElement* _el;
   void (*_callback)(UIElement*);
};

vector<AutoUpdateViewer> autoUpdateViewers;
bool                     autoUpdateViewersQueued;

bool DataViewerRemoveFromAutoUpdateList(UIElement* el) {
   if (auto it = rng::find_if(autoUpdateViewers, [&](const auto& auv) { return auv._el == el; });
       it != rng::end(autoUpdateViewers)) {
      autoUpdateViewers.erase(it);
      return true;
   }

   return false;
}

int DataViewerAutoUpdateButtonMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::CLICKED) {
      el->_flags ^= UIButton::CHECKED;

      if (el->_flags & UIButton::CHECKED) {
         AutoUpdateViewer v = {._el = el->_parent, ._callback = (void (*)(UIElement*))el->_cp};
         autoUpdateViewers.push_back(v);
      } else {
         [[maybe_unused]] bool found = DataViewerRemoveFromAutoUpdateList(el->_parent);
         assert(found);
      }
   }

   return 0;
}

void DataViewersUpdateAll() {
   if (~s_data_tab->_flags & UIElement::hide_flag) {
      for (const auto& auv : autoUpdateViewers) {
         auv._callback(auv._el);
      }
   } else if (!autoUpdateViewers.empty()) {
      autoUpdateViewersQueued = true;
   }
}

// ---------------------------------------------------/
// Bitmap viewer:
// ---------------------------------------------------/

struct BitmapViewer {
   std::string     _pointer;
   std::string     _width;
   std::string     _height;
   std::string     _stride;
   int             _parsed_width  = 0;
   int             _parsed_height = 0;
   UIButton*       _auto_toggle   = nullptr;
   UIImageDisplay* _display       = nullptr;
   UIPanel*        _label_panel   = nullptr;
   UILabel*        _label         = nullptr;
};

int BitmapViewerWindowMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   BitmapViewer* viewer = (BitmapViewer*)el->_cp;
   if (msg == UIMessage::DESTROY) {
      DataViewerRemoveFromAutoUpdateList(el);
      delete viewer;
      el->_cp = nullptr;
   } else if (msg == UIMessage::GET_WIDTH) {
      int fit = viewer->_parsed_width + 40;
      return fit > 300 ? fit : 300;
   } else if (msg == UIMessage::GET_HEIGHT) {
      int fit = viewer->_parsed_height + 40;
      return fit > 100 ? fit : 100;
   }

   return 0;
}

void BitmapViewerUpdate(std::string pointerString, std::string widthString, std::string heightString,
                        std::string strideString, UIElement* owner = nullptr);

int BitmapViewerRefreshMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::CLICKED) {
      BitmapViewer* bitmap = (BitmapViewer*)el->_parent->_cp;
      BitmapViewerUpdate(bitmap->_pointer, bitmap->_width, bitmap->_height, bitmap->_stride, el->_parent);
   }

   return 0;
}

const char* BitmapViewerGetBits(std::string pointerString, std::string widthString, std::string heightString,
                                std::string strideString, uint32_t** _bits, int* _width, int* _height, int* _stride) {
   auto widthResult = EvaluateExpression(widthString);
   if (widthResult.empty()) {
      return "Could not evaluate width.";
   }
   int  width        = sv_atoi(widthResult, 1);
   auto heightResult = EvaluateExpression(heightString);
   if (heightResult.empty()) {
      return "Could not evaluate height.";
   }
   int  height        = sv_atoi(heightResult, 1);
   int  stride        = width * 4;
   auto pointerResult = EvaluateExpression(pointerString, "/x");
   if (pointerResult.empty()) {
      return "Could not evaluate pointer.";
   }
   char _pointerResult[1024];
   std_format_to_n(_pointerResult, sizeof(_pointerResult), "{}", pointerResult);
   auto pr = strstr(_pointerResult, " 0x");
   if (!pr) {
      return "Pointer to image bits does not look like an address!";
   }
   ++pr;

   if (!strideString.empty()) {
      auto strideResult = EvaluateExpression(strideString);
      if (strideResult.empty()) {
         return "Could not evaluate stride.";
      }
      stride = sv_atoi(strideResult, 1);
   }

   uint32_t* bits = (uint32_t*)malloc(stride * height * 4); // TODO Is this multiply by 4 necessary?! And the one below.

   char bitmapPath[PATH_MAX];
   realpath(".bitmap.gf", bitmapPath);

   auto res = EvaluateCommand(std::format("dump binary memory {} ({}) ({}+{})", bitmapPath, pr, pr, stride * height));

   FILE* f = fopen(bitmapPath, "rb");

   if (f) {
      fread(bits, 1, stride * height * 4, f); // TODO Is this multiply by 4 necessary?!
      fclose(f);
      unlink(bitmapPath);
   }

   if (!f || res.contains("access")) {
      return "Could not read the image bits!";
   }

   *_bits = bits, *_width = width, *_height = height, *_stride = stride;
   return nullptr;
}

int BitmapViewerDisplayMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::RIGHT_UP) {
      el->ui()
         ->create_menu(el->_window, UIMenu::NO_SCROLL)
         .add_item(0, "Save to file...",
                   [el](UIButton&) {
                      static char* path = nullptr;
                      auto result       = s_main_window->show_dialog(0, "Save to file       \nPath:\n%t\n%f%B%C", &path,
                                                                     "Save", "Cancel");
                      if (result != "Save")
                         return;

                      UIImageDisplay* display = (UIImageDisplay*)el;
                      FILE*           f       = fopen(path, "wb");
                      print(f, "P6\n{} {}\n255\n", display->_width, display->_height);

                      for (size_t i = 0; i < display->_width * display->_height; i++) {
                         uint8_t pixel[3] = {(uint8_t)(display->_bits[i] >> 16), (uint8_t)(display->_bits[i] >> 8),
                                             (uint8_t)display->_bits[i]};
                         fwrite(pixel, 1, 3, f);
                      }

                      fclose(f);
                   })
         .show();
   }

   return 0;
}

void BitmapViewerAutoUpdateCallback(UIElement* el) {
   BitmapViewer* bitmap = (BitmapViewer*)el->_cp;
   BitmapViewerUpdate(bitmap->_pointer, bitmap->_width, bitmap->_height, bitmap->_stride, el);
}

void BitmapViewerUpdate(std::string pointerString, std::string widthString, std::string heightString,
                        std::string strideString, UIElement* owner) {
   uint32_t*   bits  = nullptr;
   int         width = 0, height = 0, stride = 0;
   const char* error =
      BitmapViewerGetBits(pointerString, widthString, heightString, strideString, &bits, &width, &height, &stride);

   if (!owner) {
      BitmapViewer* bitmap = new BitmapViewer;

      bitmap->_pointer = std::move(pointerString);
      bitmap->_width   = std::move(widthString);
      bitmap->_height  = std::move(heightString);
      bitmap->_stride  = std::move(strideString);

      UIMDIChild* window = &s_data_window->add_mdichild(UIMDIChild::CLOSE_BUTTON, UIRectangle(0), "Bitmap")
                               .set_user_proc(BitmapViewerWindowMessage)
                               .set_cp(bitmap);
      bitmap->_auto_toggle = &window->add_button(UIButton::SMALL | UIElement::non_client_flag, "Auto")
                                 .set_cp((void*)BitmapViewerAutoUpdateCallback)
                                 .set_user_proc(DataViewerAutoUpdateButtonMessage);
      window->add_button(UIButton::SMALL | UIElement::non_client_flag, "Refresh")
         .set_user_proc(BitmapViewerRefreshMessage);
      owner = window;

      UIPanel* panel = &owner->add_panel(UIPanel::EXPAND);
      bitmap->_display =
         &panel->add_imagedisplay(UIImageDisplay::INTERACTIVE | UIElement::v_fill, bits, width, height, stride)
             .set_user_proc(BitmapViewerDisplayMessage);
      bitmap->_label_panel = &panel->add_panel(UIPanel::COLOR_1 | UIElement::v_fill);
      bitmap->_label       = &bitmap->_label_panel->add_label(UIElement::h_fill, {});
   }

   BitmapViewer* bitmap  = (BitmapViewer*)owner->_cp;
   bitmap->_parsed_width = width, bitmap->_parsed_height = height;
   bitmap->_display->set_content(bits, width, height, stride);
   if (error)
      bitmap->_label->set_label(error);
   if (error)
      bitmap->_label_panel->_flags &= ~UIElement::hide_flag, bitmap->_display->_flags |= UIElement::hide_flag;
   else
      bitmap->_label_panel->_flags |= UIElement::hide_flag, bitmap->_display->_flags &= ~UIElement::hide_flag;
   bitmap->_label_panel->_parent->refresh();
   owner->refresh();
   s_data_window->refresh();

   free(bits);
}

void BitmapAddDialog() {
   static char *pointer = nullptr, *width = nullptr, *height = nullptr, *stride = nullptr;

   auto result = s_main_window->show_dialog(0,
                                            "Add bitmap\n\n%l\n\nPointer to bits: (32bpp, RR GG BB "
                                            "AA)\n%t\nWidth:\n%t\nHeight:\n%t\nStride: (optional)\n%t\n\n%l\n\n%f%B%C",
                                            &pointer, &width, &height, &stride, "Add", "Cancel");

   if (result == "Add") {
      BitmapViewerUpdate(pointer ?: "", width ?: "", height ?: "", (stride && stride[0]) ? stride : "");
   }
}

// ---------------------------------------------------/
// Console:
// ---------------------------------------------------/

struct ConsoleWindow {
private:
   vector<unique_ptr<char[]>> _command_history;
   size_t                     _command_history_index = 0;
   FILE*                      _command_log           = nullptr;

   bool previous_command() {
      if (_command_history_index < _command_history.size()) {
         s_input_textbox->clear(false);
         s_input_textbox->replace_text(_command_history[_command_history_index].get(), false);
         if (_command_history_index < _command_history.size() - 1)
            _command_history_index++;
         s_input_textbox->refresh();
      }
      return true;
   }

   bool next_command() {
      s_input_textbox->clear(false);

      if (_command_history_index > 0) {
         _command_history_index--;
         s_input_textbox->replace_text(_command_history[_command_history_index].get(), false);
      }

      s_input_textbox->refresh();
      return true;
   }

   bool clear_output() {
      s_display_output->clear();
      s_display_output->refresh();
      return true;
   }

   int _textbox_message_proc(UITextbox* textbox, UIMessage msg, int di, void* dp) {
      if (msg == UIMessage::KEY_TYPED) {
         UIKeyTyped* m = (UIKeyTyped*)dp;

         static TabCompleter tabCompleter  = {};
         bool                lastKeyWasTab = tabCompleter._last_key_was_tab;
         tabCompleter._last_key_was_tab    = false;

         std::string_view cur_text = textbox->text();
         auto             sz       = cur_text.size();

         if (!m->text.empty() && !textbox->_window->_ctrl && !textbox->_window->_alt && m->text[0] == '`' && !sz) {
            textbox->set_reject_next_key(true);
         } else if (m->code == UIKeycode::ENTER && !textbox->_window->_shift) {
            if (!sz) {
               if (_command_history.size()) {
                  CommandSendToGDB(_command_history[0].get());
               }

               return 1;
            }

            auto buffer = std::format("{}", textbox->text());
            if (_command_log)
               print(_command_log, "{}\n", buffer);
            CommandSendToGDB(buffer);


            unique_ptr<char[]> string = std::make_unique<char[]>(sz + 1);
            memcpy(string.get(), cur_text.data(), sz);
            string[sz] = 0;
            _command_history.insert(_command_history.cbegin(), std::move(string));
            _command_history_index = 0;

            if (_command_history.size() > 500) {
               _command_history.pop_back();
            }

            textbox->clear(false);
            textbox->refresh();

            return 1;
         } else if (m->code == UIKeycode::TAB && sz && !textbox->_window->_shift) {
            tabCompleter.run(textbox, lastKeyWasTab, false);
            return 1;
         } else if (m->code == UIKeycode::UP) {
            auto currentLine = s_display_code->current_line();
            if (textbox->_window->_shift) {
               if (currentLine && *currentLine > 0) {
                  DisplaySetPosition(nullptr, *currentLine - 1, false);
               }
            } else {
               previous_command();
            }
         } else if (m->code == UIKeycode::DOWN) {
            auto currentLine = s_display_code->current_line();
            if (textbox->_window->_shift) {
               if (currentLine && *currentLine + 1 < s_display_code->num_lines()) {
                  DisplaySetPosition(nullptr, *currentLine + 1, false);
               }
            } else {
               next_command();
            }
         }
      }

      return 0;
   }

public:
   static int TextboxInputMessage(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<ConsoleWindow*>(el->_cp)->_textbox_message_proc(static_cast<UITextbox*>(el), msg, di, dp);
   }

   static UIElement* Create(UIElement* parent) {
      auto     w       = new ConsoleWindow;
      UIPanel* panel2  = &parent->add_panel(UIPanel::EXPAND);
      s_display_output = &panel2->add_code(UICode::NO_MARGIN | UIElement::v_fill | UICode::SELECTABLE);
      UIPanel* panel3  = &panel2->add_panel(UIPanel::HORIZONTAL | UIPanel::EXPAND | UIPanel::COLOR_1)
                            .set_border(UIRectangle(5))
                            .set_gap(5);
      s_trafficlight = &panel3->add_spacer(0, 30, 30).set_user_proc(TrafficLightMessage);
      panel3->add_button(0, "Menu").on_click([](UIButton& buttonMenu) { ctx.show_menu(&buttonMenu); });
      s_input_textbox = &panel3->add_textbox(UIElement::h_fill).set_user_proc(TextboxInputMessage).set_cp(w).focus();
      return panel2;
   }
};

// ---------------------------------------------------/
// Watch window:
// ---------------------------------------------------/
struct Watch;
using WatchVector = vector<shared_ptr<Watch>>;

struct WatchWindow;
static WatchWindow* WatchGetFocused();

struct Watch : public std::enable_shared_from_this<Watch> {
private:
   bool        _open             = false;
   bool        _has_fields       = false;
   bool        _loaded_fields    = false;
   bool        _is_array         = false;
   bool        _is_dynamic_array = false;
   uint8_t     _depth            = 0;
   char        _format           = 0;
   uintptr_t   _array_index      = 0;
   std::string _key;
   std::string _value;
   std::string _type;
   WatchVector _fields;
   Watch*      _parent       = nullptr;
   uint64_t    _update_index = 0;

public:
   friend struct WatchWindow;
   static constexpr int WATCH_ARRAY_MAX_FIELDS = 10000000;

   char&              format() { return _format; }
   const std::string& key() const { return _key; };

   std::string evaluate(std::string_view function) const {
      char      buffer[4096];
      uintptr_t position = 0;

      position += std_format_to_n(buffer + position, sizeof(buffer) - position, "py {}([", function);

      const Watch* stack[32];
      int          stackCount = 0;
      stack[0]                = this;

      while (stack[stackCount]) {
         stack[stackCount + 1] = stack[stackCount]->_parent;
         stackCount++;
         if (stackCount == 32)
            break;
      }

      bool first = true;

      while (stackCount) {
         stackCount--;

         if (!first) {
            position += std_format_to_n(buffer + position, sizeof(buffer) - position, ",");
         } else {
            first = false;
         }

         auto w = stack[stackCount];
         if (!w->_key.empty()) {
            position += std_format_to_n(buffer + position, sizeof(buffer) - position, "'{}'", w->_key);
         } else if (w->_parent && w->_parent->_is_dynamic_array) {
            position += std_format_to_n(buffer + position, sizeof(buffer) - position, "'[{}]'", w->_array_index);
         } else {
            position += std_format_to_n(buffer + position, sizeof(buffer) - position, "{}", w->_array_index);
         }
      }

      position += std_format_to_n(buffer + position, sizeof(buffer) - position, "]");

      if (function == "gf_valueof") {
         position += std_format_to_n(buffer + position, sizeof(buffer) - position, ",'{:c}'", _format ?: ' ');
      }

      position += std_format_to_n(buffer + position, sizeof(buffer) - position, ")");
      return EvaluateCommand(buffer);
   }

   bool has_fields() const {
      auto res = evaluate("gf_fields");

      if (res.contains("(array)") || res.contains("(d_arr)")) {
         return true;
      }

      if (auto pos = res.find_first_of('\n'); pos != string::npos) {
         // trim string to first `\n` and return true if remaining isn't gdb prompt
         // -----------------------------------------------------------------------
         res.resize(pos);
         return !res.contains("(gdb)\n");
      }
      return false;
   }

   void add_fields(WatchWindow* w);

   void insert_field_rows(WatchVector* array) {
      for (const auto& field : _fields) {
         array->push_back(field);
         if (field->_open)
            field->insert_field_rows(array);
      }
   }

   std::string get_address() {
      auto res = evaluate("gf_addressof");

      if (strstr(res.c_str(), "??")) {
         s_main_window->show_dialog(0, "Couldn't get the address of the variable.\n%f%B", "OK");
         return {};
      }

      auto end = res.find_first_of(' ');
      if (end == npos) {
         s_main_window->show_dialog(0, "Couldn't get the address of the variable.\n%f%B", "OK");
         return {};
      }
      res.resize(end);
      resize_to_lf(res);
      return res;
   }

   void save_as(FILE* file, int indent, int indexInParentArray) {
      print(file, "{:.{}}", "\t\t\t\t\t\t\t\t\t\t\t\t\t\t", indent);

      if (indexInParentArray == -1) {
         print(file, "{} = ", _key);
      } else {
         print(file, "[{}] = ", indexInParentArray);
      }

      if (_open) {
         print(file, "\n");

         for (size_t i = 0; i < _fields.size(); i++)
            _fields[i]->save_as(file, indent + 1, _is_array ? i : -1);
      } else {
         auto res = evaluate("gf_valueof");
         if (!res.empty()) {
            resize_to_lf(res);
            print(file, "{}\n", res);
         }
      }
   }

   void clear(bool fieldsOnly) {
      _loaded_fields = false;
      _fields.clear();

      if (!fieldsOnly) {
         _key.clear();
      }
   }
};

static int WatchTextboxMessage(UIElement* el, UIMessage msg, int di, void* dp);

struct WatchWindow : public UIElement {
   enum WatchWindowMode {
      WATCH_NORMAL,
      WATCH_LOCALS,
   };

private:
   WatchVector     _rows;
   WatchVector     _base_expressions;
   WatchVector     _dynamic_arrays;
   UITextbox*      _textbox;
   std::string     _last_local_list;
   size_t          _selected_row;
   int             _extra_rows;
   WatchWindowMode _mode;
   uint64_t        _update_index;
   bool            _waiting_for_format_character;

public:
   friend struct Watch;

   WatchWindow(UIElement* parent, uint32_t flags, const char* name)
      : UIElement(parent, flags, WatchWindow::WatchWindowMessage, name)
      , _textbox(nullptr)
      , _selected_row(0)
      , _mode(WATCH_NORMAL)
      , _update_index(0)
      , _waiting_for_format_character(false) {
      _cp = this; // may be needed, see WatchGetFocused()
   }

   size_t last_row() const {
      size_t res = _rows.size() + _extra_rows;
      return res ? res - 1 : res;
   }

   const WatchVector& base_expressions() const { return _base_expressions; }

   void destroy_textbox() {
      if (!_textbox)
         return;
      _textbox->destroy();
      _textbox = nullptr;
      focus();
   }

   bool add_watch() {
      if (_textbox)
         return false;
      _selected_row = _rows.size();
      create_textbox_for_row(false);
      return true;
   }

   void free_watch(const shared_ptr<Watch>& watch, bool fieldsOnly) {
      if (watch->_is_dynamic_array) {
         if (auto it = rng::find(_dynamic_arrays, watch); it != rng::end(_dynamic_arrays))
            _dynamic_arrays.erase(it);
      }
      watch->clear(fieldsOnly);
   }

   std::optional<shared_ptr<Watch>> get_selected_watch() const {
      if (_textbox || _selected_row > _rows.size() || _rows.empty())
         return {};
      return _rows[_selected_row == _rows.size() ? _selected_row - 1 : _selected_row];
   }

   void delete_expression(bool fieldsOnly = false) {
      destroy_textbox();
      if (_selected_row == _rows.size())
         return;
      int end = _selected_row + 1;

      for (; end < (int)_rows.size(); end++) {
         if (_rows[_selected_row]->_depth >= _rows[end]->_depth) {
            break;
         }
      }

      shared_ptr<Watch> watch = _rows[_selected_row]; // no reference as we want to hold the pointer

      if (!fieldsOnly) {
         if (auto it = rng::find(_base_expressions, watch); it != rng::end(_base_expressions))
            _base_expressions.erase(it);
      }

      if (fieldsOnly)
         _selected_row++;
      _rows.erase(_rows.cbegin() + _selected_row, _rows.cbegin() + end);
      free_watch(watch, fieldsOnly);
      if (!fieldsOnly)
         watch.reset();
   }

   void ensure_row_visible(size_t index) {
      if (_selected_row > _rows.size())
         _selected_row = _rows.size();
      UIScrollBar* scroll    = ((UIPanel*)_parent)->scrollbar();
      int          rowHeight = (int)(ui_size::textbox_height * _window->scale());
      int          start = index * rowHeight, end = (index + 1) * rowHeight, height = _parent->_bounds.height();
      bool         unchanged = false;
      if (end >= scroll->position() + height)
         scroll->position() = end - height;
      else if (start <= scroll->position())
         scroll->position() = start;
      else
         unchanged = true;
      if (!unchanged)
         _parent->refresh();
   }

   void insert_field_rows(const shared_ptr<Watch>& watch, size_t position, bool ensureLastVisible) {
      WatchVector array = {};
      watch->insert_field_rows(&array);
      _rows.insert(_rows.cbegin() + position, array.cbegin(), array.cend());
      if (ensureLastVisible)
         ensure_row_visible(position + array.size() - 1);
      array.clear();
   }

   void add_expression(string_view string = {}) {
      if (string.empty() && _textbox && _textbox->text().empty()) {
         destroy_textbox();
         return;
      }

      auto watch = make_shared<Watch>();

      if (!string.empty())
         watch->_key = string;
      else
         watch->_key = _textbox->text();

      delete_expression(); // Deletes textbox.
      _rows.insert(_rows.cbegin() + _selected_row, watch);
      _base_expressions.push_back(watch);
      _selected_row++;

      auto res = watch->evaluate("gf_typeof");

      if (!res.contains("??")) {
         resize_to_lf(res);
         watch->_type       = std::move(res);
         watch->_has_fields = watch->has_fields();
      }
   }

   void append_expression(string_view string) {
      _selected_row = _rows.size();
      add_expression(string);
      if (_selected_row)
         _selected_row--;
      ensure_row_visible(_selected_row);
      _parent->refresh();
      refresh();
   }

   void create_textbox_for_row(bool addExistingText) {
      int         rowHeight = (int)(ui_size::textbox_height * _window->scale());
      UIRectangle row       = _bounds;
      row.t += _selected_row * rowHeight, row.b = row.t + rowHeight;
      _textbox = &add_textbox(0).set_user_proc(WatchTextboxMessage).set_cp(this);
      _textbox->move(row, true);
      _textbox->focus();

      if (addExistingText) {
         _textbox->replace_text(_rows[_selected_row]->_key, false);
      }
   }

   bool add_entry_for_address() {
      if (_mode == WATCH_NORMAL && _selected_row == _rows.size())
         return false;
      const auto& watch = _rows[_selected_row];
      auto        res   = watch->get_address();
      if (res.empty())
         return false;

      if (_mode != WATCH_NORMAL) {
         ctx.switch_to_window_and_focus("Watch");
         auto w = WatchGetFocused();
         assert(w != nullptr);
         return w->add_entry_for_address2(res);
      }
      return add_entry_for_address2(res);
   }

   bool add_entry_for_address2(std::string& res) {
      auto        address = res;
      const auto& watch   = _rows[_selected_row];
      res                 = watch->evaluate("gf_typeof");
      if (res.empty() || res.contains("??"))
         return false;
      resize_to_lf(res);

      auto buffer = std::format("({}*){}", res, address);
      add_expression(buffer);
      ensure_row_visible(_selected_row);
      _parent->refresh();
      refresh();
      return true;
   }

   bool view_source_at_address() {
      if (_mode == WATCH_NORMAL && _selected_row == _rows.size())
         return false;
      char* position = (char*)_rows[_selected_row]->_value.c_str();
      while (*position && !isdigit(*position))
         position++;
      if (!(*position))
         return false;
      uint64_t value = strtoul(position, nullptr, 0);
      auto     res   = EvaluateCommand(std::format("info line * 0x{:x}", value));
      position       = (char*)res.c_str();

      if (res.contains("No line number")) {
         resize_to_lf(res);
         s_main_window->show_dialog(0, "%s\n%f%B", res.c_str(), "OK");
         return false;
      }

      while (*position && !isdigit(*position))
         position++;
      if (!(*position))
         return false;
      size_t line = strtol(position, &position, 0);
      while (*position && *position != '"')
         position++;
      if (!(*position))
         return false;
      char* file = position + 1;
      char* end  = strchr(file, '"');
      if (!end)
         return false;
      *end = 0;
      s_source_window->display_set_position(file, line - 1, false);
      return true;
   }

   void copy_value_to_clipboard() {
      if (_mode == WATCH_NORMAL && _selected_row == _rows.size())
         return;

      const shared_ptr<Watch>& watch = _rows[_selected_row];

      auto res = watch->evaluate("gf_valueof");
      if (!res.empty()) {
         resize_to_lf(res);
         _window->write_clipboard_text(strdup(res.c_str()), sel_target_t::clipboard);
      }
   }

   void update() {
      if (_mode == WatchWindow::WATCH_LOCALS) {
         auto res = EvaluateCommand("py gf_locals()");

         bool newFrame = (_last_local_list.empty() || _last_local_list != res);

         if (newFrame) {
            _last_local_list = res;

            char*         buffer = strdup(res.c_str());
            char*         s      = buffer;
            char*         end;
            vector<char*> expressions = {};

            // we get a list of variables separated by `\n` characters, followed by the prompt
            // extract all the variable names into `expressions`.
            // we could use a regex here
            // -------------------------------------------------------------------------------
            while ((end = strchr(s, '\n')) != nullptr) {
               *end = '\0';
               if (strstr(s, "(gdb)"))
                  break;
               expressions.push_back(s);
               s = end + 1;
            }

            if (expressions.size() > 0) {
               for (size_t watchIndex = 0; watchIndex < _base_expressions.size(); watchIndex++) {
                  const shared_ptr<Watch>& watch   = _base_expressions[watchIndex];
                  bool                     matched = false;

                  if (auto it = rng::find_if(expressions, [&](char* e) { return watch->_key == e; });
                      it != rng::end(expressions)) {
                     expressions.erase(it);
                     matched = true;
                  }

                  if (!matched) {
                     [[maybe_unused]] bool found = false;
                     for (size_t rowIndex = 0; rowIndex < _rows.size(); rowIndex++) {
                        if (_rows[rowIndex] == watch) {
                           _selected_row = rowIndex;
                           delete_expression();
                           watchIndex--;
                           found = true;
                           break;
                        }
                     }
                     assert(found);
                  }
               }

               // Add the remaining (new) variables.
               for (auto exp : expressions) {
                  _selected_row = _rows.size();
                  add_expression(exp);
               }

               _selected_row = _rows.size();
            }

            free(buffer);
            expressions.clear();
         }
      }

      for (size_t i = 0; i < _base_expressions.size(); i++) {
         const shared_ptr<Watch>& watch = _base_expressions[i];
         auto                     res   = watch->evaluate("gf_typeof");
         resize_to_lf(res);

         if (res != watch->_type && res != "??") {
            watch->_type = std::move(res);

            for (size_t j = 0; j < _rows.size(); j++) {
               if (_rows[j] == watch) {
                  _selected_row = j;
                  add_expression(watch->_key);
                  _selected_row = _rows.size(), i--;
                  break;
               }
            }
         }
      }

      for (size_t i = 0; i < _dynamic_arrays.size(); i++) {
         const shared_ptr<Watch>& watch = _dynamic_arrays[i];
         auto                     res   = watch->evaluate("gf_fields");
         if (res.empty() || !res.contains("(d_arr)"))
            continue;
         int count = sv_atoi(res, 7);

         count        = std::clamp(count, 0, Watch::WATCH_ARRAY_MAX_FIELDS);
         int oldCount = watch->_fields.size();

         if (oldCount != count) {
            size_t index = (size_t)-1;

            for (size_t i = 0; i < _rows.size(); i++) {
               if (_rows[i] == watch) {
                  index = i;
                  break;
               }
            }

            assert(index != (size_t)-1);
            _selected_row = index;
            delete_expression(true);
            watch->_open = true;
            watch->add_fields(this);
            insert_field_rows(watch, index + 1, false);
         }
      }

      _update_index++;
      _parent->refresh();
      refresh();
   }

   void WatchChangeLoggerCreate();

   void save_as() {
      if (_selected_row == _rows.size())
         return;

      char* filePath = nullptr;
      auto  result   = s_main_window->show_dialog(0, "Path:            \n%t\n%f%B%C", &filePath, "Save", "Cancel");

      if (result == "Cancel") {
         free(filePath);
         return;
      }

      FILE* f = fopen(filePath, "wb");
      free(filePath);

      if (!f) {
         s_main_window->show_dialog(0, "Could not open the file for writing!\n%f%B", "OK");
         return;
      }

      _rows[_selected_row]->save_as(f, 0, -1);
      fclose(f);
   }

   int _class_message_proc(UIMessage msg, int di, void* dp);

   static int WatchWindowMessage(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<WatchWindow*>(el)->_class_message_proc(msg, di, dp);
   }

   static int WatchPanelMessage(UIElement* el, UIMessage msg, int di, void* dp) {
      WatchWindow* window = (WatchWindow*)el->_cp;
      if (msg == UIMessage::LEFT_DOWN) {
         window->focus();
         window->repaint(nullptr);
      }

      return 0;
   }

   static UIElement* _Create(UIElement* parent, const char* name, WatchWindowMode mode) {
      UIPanel*     panel = &parent->add_panel(UIPanel::SCROLL | UIPanel::COLOR_1);
      WatchWindow* w     = new WatchWindow(panel, UIElement::h_fill | UIElement::tab_stop_flag, name);
      panel->set_user_proc(WatchPanelMessage).set_cp(w);

      w->_mode = mode;
      if (mode == WatchWindow::WATCH_NORMAL) {
         w->_extra_rows = 1;
         if (!firstWatchWindow)
            firstWatchWindow = w;
      }
      return panel;
   }

   static UIElement* CreateLocalsWindow(UIElement* parent) {
      return WatchWindow::_Create(parent, "Locals", WatchWindow::WATCH_LOCALS);
   }

   static UIElement* Create(UIElement* parent) {
      return WatchWindow::_Create(parent, "Watch", WatchWindow::WATCH_NORMAL);
   }

   static void Update(const char*, UIElement* el) { static_cast<WatchWindow*>(el->_cp)->update(); }

   static void Focus(UIElement* el) { static_cast<WatchWindow*>(el)->focus(); }
};

void Watch::add_fields(WatchWindow* w) {
   if (_loaded_fields) {
      return;
   }

   _loaded_fields = true;

   auto res = evaluate("gf_fields");

   if (res.contains("(array)") || res.contains("(d_arr)")) {
      int count = sv_atoi(res, 7);

      count = std::clamp(count, 0, Watch::WATCH_ARRAY_MAX_FIELDS);

      _is_array         = true;
      bool hasSubFields = false;

      if (res.contains("(d_arr)")) {
         _is_dynamic_array = true;
         w->_dynamic_arrays.push_back(shared_from_this());
      }

      for (int i = 0; i < count; i++) {
         auto field          = make_shared<Watch>();
         field->_format      = _format;
         field->_array_index = (uintptr_t)i;
         field->_parent      = this;

         _fields.push_back(field);
         if (!i)
            hasSubFields = field->has_fields();
         field->_has_fields = hasSubFields;
         field->_depth      = _depth + 1;
      }
   } else {
      char* start    = (char*)res.c_str();
      char* position = start;

      while (true) {
         // add all fields from `res`. fields are separated by `\n` characters.
         // -------------------------------------------------------------------
         char* end = strchr(position, '\n');
         if (!end)
            break;
         *end = 0;
         if (strstr(position, "(gdb)"))
            break;
         auto field     = make_shared<Watch>();
         field->_depth  = (uint8_t)(_depth + 1);
         field->_key    = position;
         field->_parent = this;

         _fields.emplace_back(field);
         field->_has_fields = field->has_fields();
         position           = end + 1;
      }
   }
}

int WatchWindow::_class_message_proc(UIMessage msg, int di, void* dp) {
   int rowHeight = (int)(ui_size::textbox_height * _window->scale());
   int result    = 0;

   if (msg == UIMessage::PAINT) {
      UIPainter*  painter = (UIPainter*)dp;
      const auto& thm     = theme();

      for (size_t i = (painter->_clip.t - _bounds.t) / rowHeight; i <= last_row(); i++) {
         UIRectangle row = _bounds;
         row.t += i * rowHeight, row.b = row.t + rowHeight;

         UIRectangle rect_intersection = intersection(row, painter->_clip);
         if (!rect_intersection.valid())
            break;

         bool focused = i == _selected_row && is_focused();

         if (focused)
            painter->draw_block(row, thm.selected);
         painter->draw_border(row, thm.border, UIRectangle(0, 1, 0, 1));

         row.l += ui_size::textbox_margin;
         row.r -= ui_size::textbox_margin;

         if (i < _rows.size()) {
            const shared_ptr<Watch>& watch = _rows[i];
            char                     buffer[256];

            if ((watch->_value.empty() || watch->_update_index != _update_index) && !watch->_open) {
               if (!ctx._program_running) {
                  watch->_update_index = _update_index;
                  auto res             = watch->evaluate("gf_valueof");
                  resize_to_lf(res);
                  watch->_value = std::move(res);
               } else {
                  watch->_value = "..";
               }
            }

            char keyIndex[64];

            if (watch->_key.empty()) {
               std_format_to_n(keyIndex, sizeof(keyIndex), "[{}]", watch->_array_index);
            }

            if (focused && _waiting_for_format_character) {
               std_format_to_n(buffer, sizeof(buffer), "Enter format character: (e.g. 'x' for hex)");
            } else {
               std_format_to_n(buffer, sizeof(buffer), "{:.{}}{}{}{}{}", "                                           ",
                               watch->_depth * 3,
                               watch->_open         ? "v "
                               : watch->_has_fields ? "> "
                                                    : "",
                               !watch->_key.empty() ? watch->_key.c_str() : keyIndex, watch->_open ? "" : " = ",
                               watch->_open ? "" : watch->_value.c_str());
            }

            if (focused) {
               painter->draw_string(row, buffer, thm.textSelected, UIAlign::left, nullptr);
            } else {
               painter->draw_string_highlighted(row, buffer, 1, nullptr);
            }
         }
      }
   } else if (msg == UIMessage::GET_HEIGHT) {
      return (last_row() + 1) * rowHeight;
   } else if (msg == UIMessage::LEFT_DOWN) {
      auto act_font = active_font();
      auto pos      = cursor_pos();
      if (pos.y >= _bounds.t) {
         _selected_row = (pos.y - _bounds.t) / rowHeight;

         if (_selected_row < _rows.size()) {
            const shared_ptr<Watch>& watch = _rows[_selected_row];
            int                      x     = (pos.x - _bounds.l) / act_font->_glyph_width;

            if (x >= watch->_depth * 3 - 1 && x <= watch->_depth * 3 + 1 && watch->_has_fields) {
               UIKeyTyped m;
               m.code = watch->_open ? UIKeycode::LEFT : UIKeycode::RIGHT;
               _class_message_proc(UIMessage::KEY_TYPED, 0, &m);
            }
         }
      } else
         _selected_row = 0;

      focus();
      repaint(nullptr);
   } else if (msg == UIMessage::RIGHT_DOWN) {
      auto pos = cursor_pos();
      if (pos.y >= _bounds.t) {
         size_t index = (pos.y - _bounds.t) / rowHeight;

         if (index < _rows.size()) {
            _class_message_proc(UIMessage::LEFT_DOWN, di, dp);
            UIMenu& menu = ui()->create_menu(_window, UIMenu::NO_SCROLL);

            if (_mode == WATCH_NORMAL && !_rows[index]->_parent) {
               menu.add_item(0, "Edit expression", [this](UIButton&) { create_textbox_for_row(true); })
                  .add_item(0, "Delete", [this](UIButton&) {
                     delete_expression();
                     _parent->refresh();
                     refresh();
                  });
            }

            menu.add_item(0, "Copy value to clipboard\tCtrl+C", [this](UIButton&) { copy_value_to_clipboard(); })
               .add_item(0, "Log writes to address...", [this](UIButton&) { WatchChangeLoggerCreate(); })
               .add_item(0, "Break on writes to address", [this](UIButton&) {
                  if (_selected_row == _rows.size())
                     return;
                  auto res = _rows[_selected_row]->get_address();
                  ;
                  if (res.empty())
                     return;

                  auto buffer = std::format("watch * {}", res);
                  (void)DebuggerSend(buffer, true, false);
               });

            if (firstWatchWindow) {
               menu.add_item(0, "Add entry for address\tCtrl+E", [this](UIButton&) { add_entry_for_address(); });
            }

            menu.add_item(0, "View source at address\tCtrl+G", [this](UIButton&) { view_source_at_address(); })
               .add_item(0, "Save as...", [this](UIButton&) { save_as(); })
               .show();
         }
      }
   } else if (msg == UIMessage::UPDATE) {
      repaint(nullptr);
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;
      result        = 1;

      if (_waiting_for_format_character) {
         _rows[_selected_row]->_format = (!m->text.empty() && isalpha(m->text[0])) ? m->text[0] : 0;
         _rows[_selected_row]->_update_index--;

         if (_rows[_selected_row]->_is_array) {
            for (auto& field : _rows[_selected_row]->_fields) {
               field->_format = _rows[_selected_row]->_format;
               field->_update_index--;
            }
         }

         _waiting_for_format_character = false;
      } else if (_mode == WATCH_NORMAL && _selected_row != _rows.size() && !_textbox &&
                 (m->code == UIKeycode::ENTER || m->code == UIKeycode::BACKSPACE ||
                  (m->code == UIKeycode::LEFT && !_rows[_selected_row]->_open)) &&
                 !_rows[_selected_row]->_parent) {
         create_textbox_for_row(true);
      } else if (m->code == UIKeycode::DEL && !_textbox && _selected_row != _rows.size() &&
                 !_rows[_selected_row]->_parent) {
         delete_expression();
      } else if (!m->text.empty() && m->text[0] == '/' && _selected_row != _rows.size()) {
         _waiting_for_format_character = true;
      } else if (!m->text.empty() && m->text[0] == '`') {
         result = 0;
      } else if (_mode == WATCH_NORMAL && !m->text.empty() && m->code != UIKeycode::TAB && !_textbox &&
                 !_window->_ctrl && !_window->_alt &&
                 (_selected_row == _rows.size() || !_rows[_selected_row]->_parent)) {
         create_textbox_for_row(false);
         _textbox->message(msg, di, dp);
      } else if (_mode == WATCH_NORMAL && !m->text.empty() && m->code == UI_KEYCODE_LETTER('V') && !_textbox &&
                 _window->_ctrl && !_window->_alt && !_window->_shift &&
                 (_selected_row == _rows.size() || !_rows[_selected_row]->_parent)) {
         create_textbox_for_row(false);
         _textbox->message(msg, di, dp);
      } else if (m->code == UIKeycode::ENTER && _textbox) {
         add_expression();
      } else if (m->code == UIKeycode::ESCAPE) {
         destroy_textbox();
      } else if (m->code == UIKeycode::UP) {
         if (_window->_shift) {
            auto currentLine = s_display_code->current_line();
            if (currentLine && *currentLine > 0) {
               DisplaySetPosition(nullptr, *currentLine - 1, false);
            }
         } else {
            destroy_textbox();
            if (_selected_row)
               _selected_row--;
         }
      } else if (m->code == UIKeycode::DOWN) {
         if (_window->_shift) {
            auto currentLine = s_display_code->current_line();
            if (currentLine && *currentLine + 1 < s_display_code->num_lines()) {
               DisplaySetPosition(nullptr, *currentLine + 1, false);
            }
         } else {
            destroy_textbox();
            _selected_row++;
         }
      } else if (m->code == UIKeycode::HOME) {
         _selected_row = 0;
      } else if (m->code == UIKeycode::END) {
         _selected_row = last_row();
      } else if (m->code == UIKeycode::RIGHT && !_textbox && _selected_row != _rows.size() &&
                 _rows[_selected_row]->_has_fields && !_rows[_selected_row]->_open) {
         const shared_ptr<Watch>& watch = _rows[_selected_row];
         watch->_open                   = true;
         watch->add_fields(this);
         insert_field_rows(watch, _selected_row + 1, true);
      } else if (m->code == UIKeycode::LEFT && !_textbox && _selected_row != _rows.size() &&
                 _rows[_selected_row]->_has_fields && _rows[_selected_row]->_open) {
         size_t end = _selected_row + 1;

         for (; end < _rows.size(); end++) {
            if (_rows[_selected_row]->_depth >= _rows[end]->_depth) {
               break;
            }
         }

         _rows.erase(_rows.cbegin() + _selected_row + 1, _rows.cbegin() + end);
         _rows[_selected_row]->_open = false;
      } else if (m->code == UIKeycode::LEFT && !_textbox && _selected_row != _rows.size() &&
                 !_rows[_selected_row]->_open) {
         for (size_t i = 0; i < _rows.size(); i++) {
            if (_rows[_selected_row]->_parent == _rows[i].get()) {
               _selected_row = i;
               break;
            }
         }
      } else if (m->code == UI_KEYCODE_LETTER('C') && !_textbox && !_window->_shift && !_window->_alt &&
                 _window->_ctrl) {
         copy_value_to_clipboard();
      } else {
         result = 0;
      }

      ensure_row_visible(_selected_row);
      _parent->refresh();
      refresh();
   } else if (msg == UIMessage::MIDDLE_DOWN) {
      if (_mode == WATCH_NORMAL && !_textbox && !_window->_ctrl && !_window->_alt &&
          (_selected_row == _rows.size() || !_rows[_selected_row]->_parent)) {
         create_textbox_for_row(false);
         _textbox->paste(sel_target_t::primary);
         repaint(nullptr);
      }
      return 1;
   }

   if (_selected_row > last_row()) {
      _selected_row = last_row();
   }

   return result;
}

struct WatchLogEvaluated {
   std::string _result;
};

struct WatchLogEntry {
   std::string               _value;
   std::string               _where;
   vector<WatchLogEvaluated> _evaluated;
   vector<StackEntry>        _trace;
};

struct WatchLogger {
   int                   _id             = 0;
   int                   _selected_entry = 0;
   char                  _columns[256]   = {0};
   std::string           _expressions_to_evaluate;
   vector<WatchLogEntry> _entries;
   UITable*              _table = nullptr;
   UITable*              _trace = nullptr;
};

vector<WatchLogger*> watchLoggers;

int WatchTextboxMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::UPDATE) {
      if (!el->is_focused()) {
         el->destroy();
         ((WatchWindow*)el->_cp)->destroy_textbox(); // use _cp here!
      }
   } else if (msg == UIMessage::KEY_TYPED) {
      UITextbox*  textbox = (UITextbox*)el;
      UIKeyTyped* m       = (UIKeyTyped*)dp;

      static TabCompleter tabCompleter  = {};
      bool                lastKeyWasTab = tabCompleter._last_key_was_tab;
      tabCompleter._last_key_was_tab    = false;

      if (m->code == UIKeycode::TAB && textbox->text().size() && !el->_window->_shift) {
         tabCompleter.run(textbox, lastKeyWasTab, true);
         return 1;
      }
   }

   return 0;
}

void WatchAddExpression(string_view sv) {
   UIElement* el = ctx.switch_to_window_and_focus("Watch");
   static_cast<WatchWindow*>(el->_cp)->append_expression(sv);
}

int WatchLoggerWindowMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::DESTROY) {
      if (el->_cp) {
         WatchLogger* logger = (WatchLogger*)el->_cp;

         if (auto it = rng::find(watchLoggers, logger); it != rng::end(watchLoggers))
            watchLoggers.erase(it);

         EvaluateCommand(std::format("delete {}", logger->_id));
         delete logger;
      }
   } else if (msg == UIMessage::GET_WIDTH || msg == UIMessage::GET_HEIGHT) {
      return el->_window->scale() * 400;
   }

   return 0;
}

void WatchLoggerTraceSelectFrame(UIElement* el, int index, WatchLogger* logger) {
   if (index == -1) {
      return;
   }

   StackEntry* entry = &logger->_entries[logger->_selected_entry]._trace[index];
   char        location[sizeof(entry->_location)];
   strcpy(location, entry->_location);
   char* colon = strchr(location, ':');

   if (colon) {
      *colon = 0;
      DisplaySetPosition(location, sv_atoul(colon + 1) - 1, false);
      el->refresh();
   }
}

int WatchLoggerTableMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   WatchLogger* logger = (WatchLogger*)el->_cp;

   if (msg == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m     = (UITableGetItem*)dp;
      WatchLogEntry*  entry = &logger->_entries[m->_row];
      m->_is_selected       = (int)m->_row == logger->_selected_entry;

      if (m->_column == 0) {
         return m->format_to("{}", entry->_value);
      } else if (m->_column == 1) {
         return m->format_to("{}", entry->_where);
      } else {
         if (m->_column - 2 < entry->_evaluated.size()) {
            return m->format_to("{}", entry->_evaluated[m->_column - 2]._result);
         } else {
            return 0;
         }
      }
   } else if (msg == UIMessage::LEFT_DOWN || msg == UIMessage::MOUSE_DRAG) {
      int index = ((UITable*)el)->hittest(el->cursor_pos());

      if (index != -1 && logger->_selected_entry != index) {
         logger->_selected_entry = index;
         logger->_trace->set_num_items(logger->_entries[index]._trace.size());
         WatchLoggerTraceSelectFrame(logger->_trace, 0, logger);
         logger->_trace->resize_columns();
         logger->_trace->refresh();
         el->refresh();
      }
   }

   return 0;
}

int WatchLoggerTraceMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   WatchLogger* logger = (WatchLogger*)el->_cp;

   if (msg == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m     = (UITableGetItem*)dp;
      StackEntry*     entry = &logger->_entries[logger->_selected_entry]._trace[m->_row];

      if (m->_column == 0) {
         return m->format_to("{}", entry->_id);
      } else if (m->_column == 1) {
         return m->format_to("{}", entry->_function);
      } else if (m->_column == 2) {
         return m->format_to("{}", entry->_location);
      } else if (m->_column == 3) {
         return m->format_to("0x{:X}", entry->_address);
      }
   } else if (msg == UIMessage::LEFT_DOWN || msg == UIMessage::MOUSE_DRAG) {
      int index = ((UITable*)el)->hittest(el->cursor_pos());
      WatchLoggerTraceSelectFrame(el, index, logger);
   }

   return 0;
}

void WatchLoggerResizeColumns(WatchLogger* logger) {
   logger->_table->resize_columns();
   logger->_table->refresh();
}

void WatchWindow::WatchChangeLoggerCreate() {
   if (_selected_row == _rows.size()) {
      return;
   }

   if (!s_data_tab) {
      s_main_window->show_dialog(0, "The data window is not open.\nThe watch log cannot be created.\n%f%B", "OK");
      return;
   }

   auto res = _rows[_selected_row]->get_address();
   if (res.empty()) {
      return;
   }

   char* expressionsToEvaluate = nullptr;

   auto result = s_main_window->show_dialog(
      0, "-- Watch logger settings --\nExpressions to evaluate (separate with semicolons):\n%t\n\n%l\n\n%f%B%C",
      &expressionsToEvaluate, "Start", "Cancel");

   if (result == "Cancel" || expressionsToEvaluate == nullptr) {
      free(expressionsToEvaluate);
      return;
   }

   UIMDIChild* child =
      &s_data_window->add_mdichild(UIMDIChild::CLOSE_BUTTON, UIRectangle(0), std::format("Log {}", res));

   res                = EvaluateCommand(std::format("watch * {}", res));
   const char* number = strstr(res.c_str(), "point ");

   if (!number) {
      s_main_window->show_dialog(0, "Couldn't set the watchpoint.\n%f%B", "OK");
      return;
   }

   WatchLogger* logger = new WatchLogger;

   child->add_button(UIButton::SMALL | UIElement::non_client_flag, "Resize columns").on_click([logger](UIButton&) {
      WatchLoggerResizeColumns(logger);
   });

   uintptr_t position = 0;
   position += std_format_to_n(logger->_columns + position, sizeof(logger->_columns) - position, "New value\tWhere");

   if (expressionsToEvaluate) {
      uintptr_t start = 0;

      for (uintptr_t i = 0; true; i++) {
         if (expressionsToEvaluate[i] == ';' || !expressionsToEvaluate[i]) {
            position += std_format_to_n(logger->_columns + position, sizeof(logger->_columns) - position, "\t{}",
                                        std::string_view{expressionsToEvaluate + start, (size_t)(i - start)});
            start = i + 1;
         }

         if (!expressionsToEvaluate[i]) {
            break;
         }
      }
   }

   UISplitPane* panel = &child->add_splitpane(0, 0.5f);
   UITable*     table = &panel->add_table(UIElement::h_fill | UIElement::v_fill, logger->_columns);
   UITable*     trace = &panel->add_table(UIElement::h_fill | UIElement::v_fill, "Index\tFunction\tLocation\tAddress");

   logger->_id                      = sv_atoi(number, 6);
   logger->_table                   = table;
   logger->_trace                   = trace;
   logger->_selected_entry          = -1;
   logger->_expressions_to_evaluate = expressionsToEvaluate;
   child->set_user_proc(WatchLoggerWindowMessage).set_cp(logger);
   table->set_user_proc(WatchLoggerTableMessage).set_cp(logger);
   trace->set_user_proc(WatchLoggerTraceMessage).set_cp(logger);
   watchLoggers.push_back(logger);
   s_data_window->refresh();
   WatchLoggerResizeColumns(logger);

   s_main_window->show_dialog(0, "The log has been setup in the data window.\n%f%B", "OK");
   return;
}

// right click on variable and "Log writes to address..."
// ---------------------------------------------------
// parse something like:
// "\nHardware watchpoint 16: * 0x7fffffffd85c\n\nOld value = 1\nNew value = 2\000main (argc=1, argv=0x7fffffffd988)\n
// at /home/greg/github/greg/gf/examples/gf_testprog.cpp:33\00033\t   int res = a.x + c.y[1] - fi"
// --------------------------------------------------------------------------------------------------
bool WatchLoggerUpdate(std::string _data) {
   char* data             = &_data[0];
   char* stringWatchpoint = strstr(data, "watchpoint ");
   if (!stringWatchpoint)
      return false;
   char* stringAddressStart = strstr(data, ": * ");
   if (!stringAddressStart)
      return false;
   int   id    = sv_atoi(stringWatchpoint, 11);
   char* value = strstr(data, "\nNew value = ");
   if (!value)
      return false;
   value += 13;
   char* afterValue = strchr(value, '\n');
   if (!afterValue)
      return false;
   char* where = strstr(afterValue, " at ");
   if (!where)
      return false;
   where += 4;
   char* afterWhere = strchr(where, '\n');
   if (!afterWhere)
      return false;
   WatchLogger* logger = nullptr;

   for (const auto& wl : watchLoggers) {
      if (wl->_id == id) {
         logger = wl;
         break;
      }
   }

   if (!logger)
      return false;

   *afterValue         = 0;
   *afterWhere         = 0;
   WatchLogEntry entry = {};

   const char* expressionsToEvaluate = logger->_expressions_to_evaluate.c_str();

   if (expressionsToEvaluate) {
      uintptr_t start = 0;

      for (uintptr_t i = 0; true; i++) {
         if (expressionsToEvaluate[i] == ';' || !expressionsToEvaluate[i]) {

            auto res = EvaluateExpression(string_view(expressionsToEvaluate + start, i - start));
            start    = i + 1;
            WatchLogEvaluated evaluated;
            const char*       start = strstr(res.c_str(), " = ");
            if (start)
               evaluated._result = start + 3;
            else
               evaluated._result = std::move(res);
            entry._evaluated.push_back(std::move(evaluated));
         }

         if (!expressionsToEvaluate[i]) {
            break;
         }
      }
   }

   if (strlen(value) >= sizeof(entry._value))
      value[sizeof(entry._value) - 1] = 0;
   if (strlen(where) >= sizeof(entry._where))
      where[sizeof(entry._where) - 1] = 0;
   entry._value = value;
   entry._where = where;

   std::swap(entry._trace, sw->stack());
   DebuggerGetStack();
   std::swap(entry._trace, sw->stack());

   logger->_entries.push_back(entry);
   ++logger->_table->num_items();
   logger->_table->refresh();
   (void)DebuggerSend("c", false, false);
   return true;
}

WatchWindow* WatchGetFocused() {
   return s_main_window->focused()->_class_proc == WatchWindow::WatchWindowMessage
             ? (WatchWindow*)s_main_window->focused()->_cp
             : nullptr;
}

bool CommandWatchAddEntryForAddress() {
   if (auto w = WatchGetFocused())
      return w->add_entry_for_address();
   return false;
}

bool CommandWatchViewSourceAtAddress() {
   if (auto w = WatchGetFocused())
      return w->view_source_at_address();
   return false;
}

bool CommandAddWatch() {
   if (auto el = ctx.switch_to_window_and_focus("Watch"))
      return static_cast<WatchWindow*>(el)->add_watch();
   return false;
}

// ---------------------------------------------------/
// Stack window:
// ---------------------------------------------------/

void StackWindow::set_frame(UIElement* el, int index) {
   if (index >= 0 && index < (int)((UITable*)el)->num_items()) {
      _has_changed = true;
      if (_selected != (size_t)index) {
         (void)DebuggerSend(std::format("frame {}", index), false, false);
         _selected = index;
         el->repaint(nullptr);
      } else {
         s_display_code->set_current_line({}); // force the update in DisplayPosition as we may have scrolled away
         s_source_window->display_set_position_from_stack();
      }
   }
}

int StackWindow::_table_message_proc(UITable* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m       = (UITableGetItem*)dp;
      m->_is_selected         = (size_t)m->_row == _selected;
      const StackEntry& entry = _stack[m->_row];

      if (m->_column == 0) {
         return m->format_to("{}", entry._id);
      } else if (m->_column == 1) {
         return m->format_to("{}", entry._function);
      } else if (m->_column == 2) {
         return m->format_to("{}", entry._location);
      } else if (m->_column == 3) {
         return m->format_to("0x{:X}", entry._address);
      }
   } else if (msg == UIMessage::LEFT_DOWN || msg == UIMessage::MOUSE_DRAG) {
      set_frame(el, ((UITable*)el)->hittest(el->cursor_pos()));
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if (m->code == UIKeycode::UP || m->code == UIKeycode::DOWN) {
         set_frame(el, _selected + (m->code == UIKeycode::UP ? -1 : 1));
         // TODO Scroll the row into view if necessary.
         return 1;
      }
   }

   return 0;
}

UIElement* StackWindow::Create(UIElement* parent) {
   sw = new StackWindow;
   return &parent->add_table(0, "Index\tFunction\tLocation\tAddress")
              .set_cp(sw)
              .set_user_proc(StackWindow::StackWindowMessage);
}

void StackWindow::Update(const char*, UIElement* el) {
   UITable&     table = *(UITable*)el;
   StackWindow* sw    = static_cast<StackWindow*>(el->_cp);
   table.set_num_items(sw->_stack.size()).resize_columns().refresh();
}

// ---------------------------------------------------/
// Breakpoints window:
// ---------------------------------------------------/

struct BreakpointsWindow {
private:
   using Breakpoint = BreakpointMgr::Breakpoint;

   vector<int>         _selected;
   int                 _anchor = 0;
   vector<Breakpoint>& _breakpoints;

public:
   BreakpointsWindow(vector<Breakpoint>& bp)
      : _breakpoints(bp) {}

   void for_all_selected_breakpoints(string_view action) const {
      for (auto selected : _selected) {
         for (const auto& breakpoint : _breakpoints) {
            if (breakpoint._number == selected) {
               (void)DebuggerSend(std::format("{} {}", action, selected), true, false);
               break;
            }
         }
      }
   }

   void delete_selected_breakpoints() const { for_all_selected_breakpoints("delete"); }

   void disable_selected_breakpoints() const { for_all_selected_breakpoints("disable"); }

   void enable_selected_breakpoints() const { for_all_selected_breakpoints("enable"); }

   int _table_message_proc(UITable* table, UIMessage msg, int di, void* dp);

   static int BreakpointsWindowMessage(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<BreakpointsWindow*>(el->_cp)->_table_message_proc(static_cast<UITable*>(el), msg, di, dp);
   }

   static UIElement* Create(UIElement* parent) {
      return &parent->add_table(0, "File\tLine\tEnabled\tCondition\tHit")
                 .set_cp(new BreakpointsWindow(s_breakpoint_mgr._breakpoints))
                 .set_user_proc(BreakpointsWindow::BreakpointsWindowMessage);
   }

   static void Update(const char*, UIElement* el) {
      UITable*                            table = (UITable*)el;
      [[maybe_unused]] BreakpointsWindow* bw    = static_cast<BreakpointsWindow*>(el->_cp);
      table->set_num_items(s_breakpoint_mgr.num_breakpoints());
      table->resize_columns();
      table->refresh();
   }
};

int BreakpointsWindow::_table_message_proc(UITable* uitable, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m     = (UITableGetItem*)dp;
      Breakpoint*     entry = &_breakpoints[m->_row];
      m->_is_selected       = rng::find(_selected, entry->_number) != rng::end(_selected);

      if (m->_column == 0) {
         return m->format_to("{}", entry->_file);
      } else if (m->_column == 1) {
         if (entry->_watchpoint)
            return m->format_to("watch {}", entry->_number);
         else
            return m->format_to("{}", entry->_line);
      } else if (m->_column == 2) {
         return m->format_to("{}", entry->_enabled ? "yes" : "no");
      } else if (m->_column == 3) {
         return m->format_to("{}", entry->_condition);
      } else if (m->_column == 4) {
         if (entry->_hit > 0) {
            return m->format_to("{}", entry->_hit);
         }
      }
   } else if (msg == UIMessage::RIGHT_DOWN) {
      int index = ((UITable*)uitable)->hittest(uitable->cursor_pos());

      if (index != -1) {
         Breakpoint* entry = &_breakpoints[index];

         bool found = rng::find(_selected, entry->_number) != rng::end(_selected);
         if (_selected.size() <= 1 || !found) {
            if (!uitable->_window->_ctrl)
               _selected.clear();
            _selected.push_back(entry->_number);
         }

         UIMenu& menu = uitable->ui()->create_menu(uitable->_window, UIMenu::NO_SCROLL);

         if (_selected.size() > 1) {
            bool atLeastOneBreakpointDisabled = false;
            bool atLeastOneBreakpointEnabled  = false;


            for (auto selected : _selected) {
               for (const auto& breakpoint : _breakpoints) {
                  if (breakpoint._number == selected) {
                     if (breakpoint._enabled)
                        atLeastOneBreakpointEnabled = true;
                     else
                        atLeastOneBreakpointDisabled = true;
                  }
               }
            }

            menu.add_item(0, "Delete", [this](UIButton&) { delete_selected_breakpoints(); });

            if (atLeastOneBreakpointDisabled)
               menu.add_item(0, "Enable", [this](UIButton&) { enable_selected_breakpoints(); });

            if (atLeastOneBreakpointEnabled)
               menu.add_item(0, "Disable", [this](UIButton&) { disable_selected_breakpoints(); });
         } else {
            menu.add_item(0, "Delete", [index](UIButton&) { CommandDeleteBreakpoint(index); });

            if (_breakpoints[index]._enabled)
               menu.add_item(0, "Disable", [index](UIButton&) { CommandDisableBreakpoint(index); });
            else
               menu.add_item(0, "Enable", [index](UIButton&) { CommandEnableBreakpoint(index); });
         }

         menu.show();
      }
   } else if (msg == UIMessage::LEFT_DOWN) {
      int index = ((UITable*)uitable)->hittest(uitable->cursor_pos());

      if (index != -1) {
         Breakpoint* entry = &_breakpoints[index];

         if (!uitable->_window->_shift)
            _anchor = entry->_number;
         if (!uitable->_window->_ctrl)
            _selected.clear();

         uintptr_t from = 0, to = 0;

         for (size_t i = 0; i < _breakpoints.size(); i++) {
            if (_breakpoints[i]._number == entry->_number) {
               from = i;
            }
            if (_breakpoints[i]._number == _anchor) {
               to = i;
            }
         }

         if (from > to) {
            uintptr_t temp = from;
            from = to, to = temp;
         }

         for (uintptr_t i = from; i <= to; i++) {
            if (uitable->_window->_ctrl && !uitable->_window->_shift) {
               if (auto it = rng::find(_selected, _breakpoints[i]._number); it != rng::end(_selected))
                  _selected.erase(it);
            } else {
               _selected.push_back(_breakpoints[i]._number);
            }
         }

         if (!entry->_watchpoint && rng::find(_selected, entry->_number) != rng::end(_selected)) {
            DisplaySetPosition(entry->_file, entry->_line - 1, false);
         }
      } else if (!uitable->_window->_ctrl && !uitable->_window->_shift) {
         _selected.clear();
      }
      uitable->focus();
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if (m->code == UIKeycode::DEL && _selected.size() > 0) {
         delete_selected_breakpoints();
      }
   }

   return 0;
}

// ---------------------------------------------------/
// Data window:
// ---------------------------------------------------/

struct DataWindow {
private:
   UIButton* _fill_window_button;

   bool toggle_fill_data_tab() {
      if (!s_data_tab)
         return false;
      static UIElement *oldParent, *oldBefore;
      _fill_window_button->_flags ^= UIButton::CHECKED;

      if (s_main_switcher->_active == s_data_tab) {
         s_main_switcher->switch_to(s_main_switcher->_children[0]);
         s_data_tab->change_parent(oldParent, oldBefore);
      } else {
         s_data_tab->message(UIMessage::TAB_SELECTED, 0, 0);
         oldParent = s_data_tab->_parent;
         oldBefore = s_data_tab->change_parent(s_main_switcher, nullptr);
         s_main_switcher->switch_to(s_data_tab);
      }
      return true;
   }

public:
   static int DataTabMessage(UIElement* el, UIMessage msg, int di, void* dp) {
      if (msg == UIMessage::TAB_SELECTED && autoUpdateViewersQueued) {
         // If we've switched to the data tab, we may need to update the bitmap viewers.

         for (const auto& auw : autoUpdateViewers)
            auw._callback(auw._el);

         autoUpdateViewersQueued = false;
      }

      return 0;
   }

   static UIElement* Create(UIElement* parent) {
      auto w = new DataWindow;

      s_data_tab      = &parent->add_panel(UIPanel::EXPAND);
      UIPanel* panel5 = &s_data_tab->add_panel(UIPanel::COLOR_1 | UIPanel::HORIZONTAL | UIPanel::SMALL_SPACING);

      w->_fill_window_button =
         &panel5->add_button(UIButton::SMALL, "Fill window").on_click([w](UIButton&) { w->toggle_fill_data_tab(); });

      for (const auto& idw : ctx._interface_data_viewers) {
         panel5->add_button(UIButton::SMALL, idw._add_button_label).on_click([&](UIButton&) {
            idw._add_button_callback();
         });
      }

      s_data_window = &s_data_tab->add_mdiclient(UIElement::v_fill).set_cp(w);
      s_data_tab->set_user_proc(DataTabMessage);
      return s_data_tab;
   }
};

// ---------------------------------------------------/
// Struct window:
// ---------------------------------------------------/

struct StructWindow {
private:
   UICode*    _display = nullptr;
   UITextbox* _textbox = nullptr;

public:
   int _textbox_message_proc(UITextbox* textbox, UIMessage msg, int di, void* dp) {
      if (msg == UIMessage::KEY_TYPED) {
         UIKeyTyped* m = (UIKeyTyped*)dp;

         if (m->code == UIKeycode::ENTER) {
            auto  res = EvaluateCommand(std::format("ptype /o {}", _textbox->text()));
            char* end = (char*)strstr(res.c_str(), "\n(gdb)");
            if (end)
               *end = 0;
            _display->insert_content(res, true);
            _textbox->clear(false);
            _display->refresh();
            textbox->refresh();
            return 1;
         }
      }
      return 0;
   }

   static int TextboxStructNameMessage(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<StructWindow*>(el->_cp)->_textbox_message_proc(static_cast<UITextbox*>(el), msg, di, dp);
   }

   static UIElement* Create(UIElement* parent) {
      StructWindow* window = new StructWindow;
      UIPanel*      panel  = &parent->add_panel(UIPanel::COLOR_1 | UIPanel::EXPAND);
      window->_textbox     = &panel->add_textbox(0).set_user_proc(TextboxStructNameMessage).set_cp(window);
      window->_display     = &panel->add_code(UIElement::v_fill | UICode::NO_MARGIN | UICode::SELECTABLE)
                             .insert_content("Type the name of a struct to view its layout.", false);
      return panel;
   }
};

// ---------------------------------------------------/
// Files window:
// ---------------------------------------------------/
struct FilesWindow {
   char     _directory[PATH_MAX];
   UIPanel* _panel = nullptr;
   UILabel* _path  = nullptr;

   const char* directory() const { return _directory; }

   mode_t get_mode(UIButton* button, size_t* oldLength) {
      const char* name = button->label().data();
      *oldLength       = strlen(_directory);
      strcat(_directory, "/");
      strcat(_directory, name);
      struct stat s;
      stat(_directory, &s);
      return s.st_mode;
   }

   void update_path() {
      char copy[PATH_MAX];
      realpath(_directory, copy); // resolve dir into `copy`
      strcpy(_directory, copy);   // copy resolved path into `_directory`
   }

   bool populate_panel() {
      size_t         oldLength;
      DIR*           directory = opendir(_directory);
      struct dirent* entry;
      if (!directory)
         return false;
      vector<std::string> names = {};
      while ((entry = readdir(directory)))
         names.push_back(entry->d_name);
      closedir(directory);
      _panel->destroy_descendents();

      std::sort(names.begin(), names.end());

      for (auto name : names) {
         if (name[0] != '.' || name[1] != 0) {
            UIButton* button = &_panel->add_button(0, name)
                                   .clear_flag(UIElement::tab_stop_flag)
                                   .set_cp(this)
                                   .set_user_proc(FilesButtonMessage);

            if (S_ISDIR(get_mode(button, &oldLength))) {
               button->set_flag(UIButton::CHECKED);
            }

            _directory[oldLength] = 0;
         }
      }

      _panel->refresh();

      char path[PATH_MAX];
      realpath(_directory, path);
      _path->set_label(path);

      return true;
   }

   void navigate_to_cwd() {
      getcwd(_directory, sizeof(_directory));
      populate_panel();
   }

   void navigate_to_active_file() {
      std_format_to_n(_directory, sizeof(_directory), "{}", s_source_window->_current_file_full);
      int p = strlen(_directory);
      while (p--) {
         if (_directory[p] == '/') {
            _directory[p] = 0;
            break;
         }
      }
      populate_panel();
   }

   int _button_message_proc(UIButton* button, UIMessage msg, int di, void* dp) {
      if (msg == UIMessage::CLICKED) {
         size_t oldLength;
         mode_t mode = get_mode(button, &oldLength);

         if (S_ISDIR(mode)) {
            if (populate_panel()) {
               update_path();
               return 0;
            }
         } else if (S_ISREG(mode)) {
            DisplaySetPosition(directory(), 0, false);
         }

         _directory[oldLength] = 0;
      } else if (msg == UIMessage::PAINT) {
         UIPainter*  painter = (UIPainter*)dp;
         int         i       = button->is_pressed() + button->is_hovered();
         const auto& theme   = button->theme();
         if (i)
            painter->draw_block(button->_bounds, i == 2 ? theme.buttonPressed : theme.buttonHovered);
         painter->draw_string(button->_bounds + UIRectangle(ui_size::button_padding, 0, 0, 0), button->label(),
                              button->_flags & UIButton::CHECKED ? theme.codeNumber : theme.codeDefault, UIAlign::left,
                              nullptr);
         return 1;
      }

      return 0;
   }

   static int FilesButtonMessage(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<FilesWindow*>(el->_cp)->_button_message_proc(static_cast<UIButton*>(el), msg, di, dp);
   }

   static UIElement* Create(UIElement* parent) {
      FilesWindow* window    = new FilesWindow;
      UIPanel*     container = &parent->add_panel(UIPanel::EXPAND);
      window->_panel = &container->add_panel(UIPanel::COLOR_1 | UIPanel::EXPAND | UIPanel::SCROLL | UIElement::v_fill)
                           .set_gap(-1)
                           .set_border(UIRectangle(1))
                           .set_cp(window);
      UIPanel* row = &container->add_panel(UIPanel::COLOR_2 | UIPanel::HORIZONTAL | UIPanel::SMALL_SPACING);

      row->add_button(UIButton::SMALL, "-> cwd").on_click([window](UIButton&) { window->navigate_to_cwd(); });
      row->add_button(UIButton::SMALL, "-> active file").on_click([window](UIButton&) {
         window->navigate_to_active_file();
      });

      window->_path = &row->add_label(UIElement::h_fill, "");
      window->navigate_to_cwd();
      return container;
   }
};

// ---------------------------------------------------/
// Registers window:
// ---------------------------------------------------/
struct RegistersWindow {
private:
   struct RegisterData {
      char _string[128];
   };

   vector<RegisterData> _reg_data;

public:
   void update(UIElement* panel) {
      auto res = EvaluateCommand("info registers");

      if (res.empty() || res.contains("The program has no registers now.") ||
          res.contains("The current thread has terminated")) {
         return;
      }

      panel->destroy_descendents();
      const char*          position     = res.c_str();
      vector<RegisterData> new_reg_data = {};
      bool                 anyChanges   = false;

      while (*position != '(') {
         const char* nameStart = position;
         while (isspace(*nameStart))
            nameStart++;
         const char* nameEnd = position = strchr(nameStart, ' ');
         if (!nameEnd)
            break;
         const char* format1Start = position;
         while (isspace(*format1Start))
            format1Start++;
         const char* format1End = position = strchr(format1Start, ' ');
         if (!format1End)
            break;
         const char* format2Start = position;
         while (isspace(*format2Start))
            format2Start++;
         const char* format2End = position = strchr(format2Start, '\n');
         if (!format2End)
            break;

         const char* stringStart = nameStart;
         const char* stringEnd   = format2End;

         RegisterData data;
         std_format_to_n(data._string, sizeof(data._string), "{}",
                         std::string_view{stringStart, (size_t)(stringEnd - stringStart)});
         bool modified = false;

         if (_reg_data.size() > new_reg_data.size()) {
            RegisterData* old = &_reg_data[new_reg_data.size()];

            if (strcmp(old->_string, data._string)) {
               modified = true;
            }
         }

         new_reg_data.push_back(data);

         UIPanel* row = &panel->add_panel(UIPanel::HORIZONTAL | UIElement::h_fill);
         if (modified)
            row->set_user_proc(ModifiedRowMessage);
         row->add_label(0, {stringStart, static_cast<size_t>(stringEnd - stringStart)});

         bool isPC = false;
         if (nameEnd == nameStart + 3 && 0 == memcmp(nameStart, "rip", 3))
            isPC = true;
         if (nameEnd == nameStart + 3 && 0 == memcmp(nameStart, "eip", 3))
            isPC = true;
         if (nameEnd == nameStart + 2 && 0 == memcmp(nameStart, "ip", 2))
            isPC = true;

         auto  sw        = static_cast<SourceWindow*>(s_display_code->_cp);
         auto& ap_result = sw->auto_print_result();

         if (modified && s_source_window->_showing_disassembly && !isPC) {
            if (!anyChanges) {
               ap_result[0]                = 0;
               sw->_auto_print_result_line = sw->_auto_print_expression_line;
               anyChanges                  = true;
            } else {
               int position = strlen(ap_result.data());
               std_format_to_n(&ap_result[position], sizeof(ap_result) - position, ", ");
            }

            int position = strlen(ap_result.data());
            std_format_to_n(&ap_result[position], sizeof(ap_result) - position, "{}={}",
                            std::string_view{nameStart, (size_t)(nameEnd - nameStart)},
                            std::string_view{format1Start, (size_t)(format1End - format1Start)});
         }
      }

      panel->refresh();
      _reg_data.clear();
      _reg_data = new_reg_data;
   }

   static void Update(const char*, UIElement* el) {
      RegistersWindow* rw = static_cast<RegistersWindow*>(el->_cp);
      rw->update(el);
   }

   static UIElement* Create(UIElement* parent) {
      auto window = new RegistersWindow;
      return &parent->add_panel(UIPanel::SMALL_SPACING | UIPanel::COLOR_1 | UIPanel::SCROLL).set_cp(window);
   }
};

// ---------------------------------------------------/
// Commands window:
// ---------------------------------------------------/

struct CommandsWindow {

   static UIElement* Create(UIElement* parent) {
      UIPanel* panel =
         &parent->add_panel(UIPanel::COLOR_1 | UIPanel::SMALL_SPACING | UIPanel::EXPAND | UIPanel::SCROLL);
      if (!gfc._preset_commands.size())
         panel->add_label(0, "No preset commands found in config file!");

      for (const auto& cmd : gfc._preset_commands) {
         panel->add_button(0, cmd._key).on_click([command = std::format("gf-command {}", cmd._key)](UIButton&) {
            CommandSendToGDB(command);
         });
      }

      return panel;
   }
};

// ---------------------------------------------------/
// Log window:
// ---------------------------------------------------/

struct LogWindow {
   static void* _thread_fn(void* context) {
      if (!gfc._log_pipe_path) {
         print(std::cerr, "Warning: The log pipe path has not been set in the configuration file!\n");
         return nullptr;
      }

      int file = open(gfc._log_pipe_path, O_RDONLY | O_NONBLOCK);

      if (file == -1) {
         print(std::cerr, "Warning: Could not open the log pipe!\n");
         return nullptr;
      }

      struct pollfd p = {.fd = file, .events = POLLIN};

      while (true) {
         poll(&p, 1, 10000);

         if (p.revents & POLLHUP) {
            struct timespec t = {.tv_nsec = 10000000};
            nanosleep(&t, 0);
         }

         while (true) {
            char input[16384];
            int  length = read(file, input, sizeof(input) - 1);
            if (length <= 0)
               break;
            input[length] = 0;

            std::string* s = new std::string;
            s->resize(sizeof(context) + length + 1);
            memcpy(s->data(), &context, sizeof(context));
            strcpy(s->data() + sizeof(context), input);
            s_main_window->post_message(msgReceivedLog, s);
         }
      }
   }

   static UIElement* Create(UIElement* parent) {
      UICode*   code = &parent->add_code(UICode::SELECTABLE);
      pthread_t thread;
      pthread_create(&thread, nullptr, LogWindow::_thread_fn, code);
      return code;
   }
};

// ---------------------------------------------------/
// Thread window:
// ---------------------------------------------------/

struct ThreadsWindow {
private:
   struct Thread {
      char _frame[127];
      bool _active = false;
      int  _id     = 0;
   };

   vector<Thread> _threads;

public:
   int _table_message_proc(UITable* uitable, UIMessage msg, int di, void* dp) {
      if (msg == UIMessage::TABLE_GET_ITEM) {
         UITableGetItem* m = (UITableGetItem*)dp;
         m->_is_selected   = _threads[m->_row]._active;

         if (m->_column == 0) {
            return m->format_to("{}", _threads[m->_row]._id);
         } else if (m->_column == 1) {
            return m->format_to("{}", _threads[m->_row]._frame);
         }
      } else if (msg == UIMessage::LEFT_DOWN) {
         int index = uitable->hittest(uitable->cursor_pos());

         if (index != -1) {
            (void)DebuggerSend(std::format("thread {}", _threads[index]._id), true, false);
         }
      }

      return 0;
   }

   static int ThreadsWindowMessage(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<ThreadsWindow*>(el->_cp)->_table_message_proc(static_cast<UITable*>(el), msg, di, dp);
   }

   void update(UITable* table) {
      _threads.clear();
      auto res = EvaluateCommand("info threads");
      if (res.empty())
         return;

      char* position = (char*)res.c_str();
      for (int i = 0; position[i]; i++) {
         if (position[i] == '\n' && position[i + 1] == ' ' && position[i + 2] == ' ' && position[i + 3] == ' ') {
            memmove(position + i, position + i + 3, strlen(position) - 3 - i + 1);
         }
      }

      while (true) {
         position = strchr(position, '\n');
         if (!position)
            break;
         Thread thread = {};
         if (position[1] == '*')
            thread._active = true;
         thread._id = sv_atoi(position, 2);
         position   = strchr(position + 1, '"');
         if (!position)
            break;
         position = strchr(position + 1, '"');
         if (!position)
            break;
         position++;
         char* end = strchr(position, '\n');
         if (end - position >= (ptrdiff_t)sizeof(thread._frame))
            end = position + sizeof(thread._frame) - 1;
         memcpy(thread._frame, position, end - position);
         thread._frame[end - position] = 0;
         _threads.push_back(thread);
      }

      table->set_num_items(_threads.size());
      table->resize_columns();
      table->refresh();
   }

   static UIElement* Create(UIElement* parent) {
      return &parent->add_table(0, "ID\tFrame")
                 .set_cp(new ThreadsWindow)
                 .set_user_proc(ThreadsWindow::ThreadsWindowMessage);
   }

   static void Update(const char*, UIElement* table) {
      ThreadsWindow* window = static_cast<ThreadsWindow*>(table->_cp);
      window->update(static_cast<UITable*>(table));
   }
};

// ---------------------------------------------------/
// Executable window:
// ---------------------------------------------------/

struct ExecutableWindow {
private:
   UITextbox* _path      = nullptr;
   UITextbox* _arguments = nullptr;
   bool       _should_ask;

public:
   void start_or_run(bool pause) {
      auto res = EvaluateCommand(std::format("file \"{}\"", _path->text()));

      if (res.contains("No such file or directory.")) {
         s_main_window->show_dialog(0, "The executable path is invalid.\n%f%B", "OK");
         return;
      }

      (void)EvaluateCommand(std::format("start {}", _arguments->text()));

      if (_should_ask) {
         CommandParseInternal("gf-get-pwd", true);
      }

      if (!pause) {
         (void)CommandParseInternal("run", false);
      } else {
         DebuggerGetStack();
         s_source_window->display_set_position_from_stack();
      }
   }

   void save() {
      FILE* f = fopen(gfc._local_config_path, "rb");
      if (f) {
         auto result = s_main_window->show_dialog(0, ".project.gf already exists in the current directory.\n%f%B%C",
                                                  "Overwrite", "Cancel");
         if (result != "Overwrite")
            return;
         fclose(f);
      }

      f = fopen(gfc._local_config_path, "wb");
      print(f, "[executable]\npath={}\narguments={}\nask_directory={}\n", _path->text(), _arguments->text(),
            _should_ask ? '1' : '0');
      fclose(f);
      SettingsAddTrustedFolder();
      s_main_window->show_dialog(0, "Saved executable settings!\n%f%B", "OK");
   }

   static UIElement* Create(UIElement* parent) {
      ExecutableWindow* win   = new ExecutableWindow;
      UIPanel*          panel = &parent->add_panel(UIPanel::COLOR_1 | UIPanel::EXPAND);

      panel->add_n(
         [&](auto& p) { p.add_label(0, "Path to executable:"); },
         [&](auto& p) { win->_path = &p.add_textbox(0).replace_text(gfc._exe_path, false); },
         [&](auto& p) { p.add_label(0, "Command line arguments:"); },
         [&](auto& p) { win->_arguments = &p.add_textbox(0).replace_text(gfc._exe_args, false); },
         [&](auto& p) {
            p.add_checkbox(0, "Ask GDB for working directory").set_checked(gfc._exe_ask_dir).track(&win->_should_ask);
         },
         [&](auto& p) {
            p.add_panel(UIPanel::HORIZONTAL)
               .add_n(
                  [&](auto& p) { p.add_button(0, "Run").on_click([win](UIButton&) { win->start_or_run(false); }); },
                  [&](auto& p) { p.add_button(0, "Start").on_click([win](UIButton&) { win->start_or_run(true); }); },
                  [&](auto& p) { p.add_spacer(0, 10, 0); },
                  [&](auto& p) { p.add_button(0, "Save to .project.gf").on_click([win](UIButton&) { win->save(); }); });
         });
      return panel;
   }
};

// ---------------------------------------------------/
// Command search window:
// ---------------------------------------------------/

struct CommandSearchWindow {
private:
   struct GDBCommand {
      char* _name              = nullptr;
      char* _description       = nullptr;
      char* _description_lower = nullptr;
   };

   UICode*            _display = nullptr;
   UITextbox*         _textbox = nullptr;
   vector<GDBCommand> _commands;

public:
   int _textbox_message_proc(UITextbox* textbox, UIMessage msg, int di, void* dp) {
      if (msg == UIMessage::KEY_TYPED) {
         if (!_commands.size()) {
            auto  res = EvaluateCommand("help all");
            char* s   = nullptr;
            if (!res.empty()) {
               s = (char*)res.c_str();
               for (int i = 0; s[i]; i++) {
                  if (s[i] == ',' && s[i + 1] == ' ' && s[i + 2] == '\n') {
                     s[i + 2] = ' ';
                  }
               }
            }

            char* position = s;

            while (position) {
               char* next = strchr(position, '\n');
               if (!next)
                  break;
               char* dash = strstr(position, "--");

               if (dash && dash < next && dash > position + 1) {
                  GDBCommand command = {};
                  command._name      = (char*)calloc(1, dash - 1 - position + 1);

                  for (int i = 0, j = 0; i < dash - 1 - position; i++) {
                     if (position[i] != ' ' || position[i + 1] != ' ') {
                        command._name[j++] = position[i];
                     }
                  }

                  command._description       = (char*)calloc(1, next - (dash + 3) + 1);
                  command._description_lower = (char*)calloc(1, next - (dash + 3) + 1);
                  memcpy(command._description, dash + 3, next - (dash + 3));

                  for (int i = 0; command._description[i]; i++) {
                     command._description_lower[i] = command._description[i] >= 'A' && command._description[i] <= 'Z'
                                                        ? command._description[i] + 'a' - 'A'
                                                        : command._description[i];
                  }

                  _commands.push_back(command);
               }

               position = next + 1;
            }
         }

         char query[4096];
         char buffer[4096];
         bool firstMatch = true;

         std_format_to_n(query, sizeof(query), "{}", _textbox->text());
         for (int i = 0; query[i]; i++) {
            query[i] = query[i] >= 'A' && query[i] <= 'Z' ? query[i] + 'a' - 'A' : query[i];
         }

         for (const auto& cmd : _commands) {
            if (strstr(cmd._description_lower, query)) {
               std_format_to_n(buffer, sizeof(buffer), "{}: {}", cmd._name, cmd._description);
               _display->insert_content(buffer, firstMatch);
               firstMatch = false;
            }
         }

         if (firstMatch) {
            _display->insert_content("(no matches)", firstMatch);
         }

         _display->reset_vscroll();
         _display->refresh();
      }

      return 0;
   }

   static int SearchCommandMessage(UIElement* el, UIMessage msg, int di, void* dp) {
      CommandSearchWindow* window = static_cast<CommandSearchWindow*>(el->_cp);
      return window->_textbox_message_proc(static_cast<UITextbox*>(el), msg, di, dp);
   }

   static UIElement* Create(UIElement* parent) {
      CommandSearchWindow* win = new CommandSearchWindow;

      UIPanel* panel =
         &parent->add_panel(UIPanel::COLOR_1 | UIPanel::EXPAND)
             .add_n(
                [&](auto& p) {
                   win->_textbox =
                      &p.add_textbox(0).set_user_proc(CommandSearchWindow::SearchCommandMessage).set_cp(win);
                },
                [&](auto& p) {
                   win->_display = &p.add_code(UIElement::v_fill | UICode::NO_MARGIN | UICode::SELECTABLE)
                                       .insert_content("Type here to search \nGDB command descriptions.", true);
                });
      return panel;
   }
};

// ----------------------------------------------------------
// Utilities:
// ----------------------------------------------------------

void ThumbnailResize(uint32_t* bits, uint32_t originalWidth, uint32_t originalHeight, uint32_t targetWidth,
                     uint32_t targetHeight) {
   float cx = (float)originalWidth / targetWidth;
   float cy = (float)originalHeight / targetHeight;

   for (uint32_t i = 0; i < originalHeight; i++) {
      uint32_t* output = bits + i * originalWidth;
      uint32_t* input  = output;

      for (uint32_t j = 0; j < targetWidth; j++) {
         uint32_t sumAlpha = 0, sumRed = 0, sumGreen = 0, sumBlue = 0;
         uint32_t count = (uint32_t)((j + 1) * cx) - (uint32_t)(j * cx);

         for (uint32_t k = 0; k < count; k++, input++) {
            uint32_t pixel = *input;
            sumAlpha += (pixel >> 24) & 0xFF;
            sumRed += (pixel >> 16) & 0xFF;
            sumGreen += (pixel >> 8) & 0xFF;
            sumBlue += (pixel >> 0) & 0xFF;
         }

         sumAlpha /= count;
         sumRed /= count;
         sumGreen /= count;
         sumBlue /= count;

         *output = (sumAlpha << 24) | (sumRed << 16) | (sumGreen << 8) | (sumBlue << 0);
         output++;
      }
   }

   for (uint32_t i = 0; i < targetWidth; i++) {
      uint32_t* output = bits + i;
      uint32_t* input  = output;

      for (uint32_t j = 0; j < targetHeight; j++) {
         uint32_t sumAlpha = 0, sumRed = 0, sumGreen = 0, sumBlue = 0;
         uint32_t count = (uint32_t)((j + 1) * cy) - (uint32_t)(j * cy);

         for (uint32_t k = 0; k < count; k++, input += originalWidth) {
            uint32_t pixel = *input;
            sumAlpha += (pixel >> 24) & 0xFF;
            sumRed += (pixel >> 16) & 0xFF;
            sumGreen += (pixel >> 8) & 0xFF;
            sumBlue += (pixel >> 0) & 0xFF;
         }

         sumAlpha /= count;
         sumRed /= count;
         sumGreen /= count;
         sumBlue /= count;

         *output = (sumAlpha << 24) | (sumRed << 16) | (sumGreen << 8) | (sumBlue << 0);
         output += originalWidth;
      }
   }

   for (uint32_t i = 0; i < targetHeight; i++) {
      for (uint32_t j = 0; j < targetWidth; j++) {
         bits[i * targetWidth + j] = bits[i * originalWidth + j];
      }
   }
}

/////////////////////////////////////////////////////
// Profiler:
// ----------------------------------------------------------

// TODO Inclusive/exclusive timing switch.
// TODO Horizontal scale modes (e.g. make all leaf calls equal width).
// TODO Coloring the flame graph based on other parameters?
// TODO Watching expressions during profiled step; highlight entries that modify it.
struct ProfProfilingEntry {
   void*    _this_function = nullptr;
   uint64_t _time_stamp    = 0; // High bit set if exiting the function.
};

struct ProfWindow {
   uint64_t _ticks_per_ms          = 0;
   UIFont*  _font_flame_graph      = nullptr;
   bool     _in_step_over_profiled = false;
};

struct ProfFlameGraphEntry {
   void*       _this_function = nullptr;
   const char* _name          = nullptr;
   double      _start_time    = 0;
   double      _end_time      = 0;
   int         _depth         = 0;
   uint8_t     _color_index   = 0;
};

struct ProfFlameGraphEntryTime {
   // Keep this structure as small as possible!
   float _start = 0;
   float _end   = 0;
   int   _depth = 0;
};

struct ProfSourceFileEntry {
   char _path[256];
};

struct ProfFunctionEntry {
   uint32_t _call_count        = 0;
   int      _line_number       = 0;
   int      _source_file_index = 0;
   double   _total_time        = 0;
   char     _name[64];
};

int ProfFlameGraphMessage(UIElement* el, UIMessage msg, int di, void* dp);

struct ProfFlameGraphReport : public UIElement {
   UIRectangle  _client;
   UIFont*      _font;
   UITable*     _table;
   UIButton*    _switch_view_button;
   UIScrollBar* _v_scroll;
   bool         _showing_table;

   vector<ProfFlameGraphEntry>             _entries;
   vector<ProfFlameGraphEntryTime>         _entry_times;
   vector<ProfFunctionEntry>               _sorted_functions;
   unordered_map<void*, ProfFunctionEntry> _functions;
   vector<ProfSourceFileEntry>             _source_files;

   uint32_t* _thumbnail;
   int       _thumbnail_width;
   int       _thumbnail_height;

   double _total_time;
   double _x_start;
   int    _x_end;

   ProfFlameGraphEntry* _hover;
   ProfFlameGraphEntry* _menu_item;

#define FLAME_GRAPH_DRAG_ZOOM_RANGE (1)
#define FLAME_GRAPH_DRAG_PAN (2)
#define FLAME_GRAPH_DRAG_X_PAN_AND_ZOOM (3)
#define FLAME_GRAPH_DRAG_X_SCROLL (4)
   bool   _drag_started;
   int    _drag_mode;
   double _drag_initial_value, _drag_initial_value2;
   int    _drag_initial_point, _drag_initial_point2;
   int    _drag_current_point;
   double _drag_scroll_rate;

   ProfFlameGraphReport(UIElement* parent, uint32_t flags)
      : UIElement(parent, flags, ProfFlameGraphMessage, "flame graph")
      , _client(0)
      , _font(nullptr)
      , _table(nullptr)
      , _switch_view_button(nullptr)
      , _v_scroll(nullptr)
      , _showing_table(false)
      , _thumbnail(nullptr)
      , _thumbnail_width(0)
      , _thumbnail_height(0)
      , _total_time(0)
      , _x_start(0)
      , _x_end(0)
      , _hover(nullptr)
      , _menu_item(nullptr)
      , _drag_started(false)
      , _drag_mode(0)
      , _drag_initial_value(0)
      , _drag_initial_value2(0)
      , _drag_initial_point(0)
      , _drag_initial_point2(0)
      , _drag_current_point(0)
      , _drag_scroll_rate(0) {}
};

const uint32_t profMainColor        = 0xFFBFC1C3;
const uint32_t profHoverColor       = 0xFFBFC1FF;
const uint32_t profBorderLightColor = 0xFFFFFFFF;
const uint32_t profBorderDarkColor  = 0xFF000000;
const uint32_t profBackgroundColor  = 0xFF505153;
const uint32_t profTextColor        = 0xFF000000;

const uint32_t profEntryColorPalette[] = {
   0xFFE5A0A0, 0xFFDBA0E5, 0xFFA0B5E5, 0xFFA0E5C6, 0xFFC9E5A0, 0xFFE5B1A0, 0xFFE5A0DE, 0xFFA0A4E5, 0xFFA0E5D7,
   0xFFB8E5A0, 0xFFE5C3A0, 0xFFE5A0CD, 0xFFAEA0E5, 0xFFA0E2E5, 0xFFA7E5A0, 0xFFE5D4A0, 0xFFE5A0BC, 0xFFBFA0E5,
   0xFFA0D0E5, 0xFFA0E5AA, 0xFFE5E5A0, 0xFFE5A0AA, 0xFFD0A0E5, 0xFFA0BFE5, 0xFFA0E5BC, 0xFFD4E5A0, 0xFFE5A7A0,
   0xFFE2A0E5, 0xFFA0AEE5, 0xFFA0E5CD, 0xFFC3E5A0, 0xFFE5B8A0, 0xFFE5A0D7, 0xFFA4A0E5, 0xFFA0E5DE, 0xFFB1E5A0,
   0xFFE5C9A0, 0xFFE5A0C6, 0xFFB5A0E5, 0xFFA0DBE5, 0xFFA0E5A0, 0xFFE5DBA0, 0xFFE5A0B5, 0xFFC6A0E5, 0xFFA0C9E5,
   0xFFA0E5B1, 0xFFDEE5A0, 0xFFE5A0A4, 0xFFD7A0E5, 0xFFA0B8E5, 0xFFA0E5C3, 0xFFCDE5A0, 0xFFE5AEA0, 0xFFE5A0E2,
   0xFFA0A7E5, 0xFFA0E5D4, 0xFFBCE5A0, 0xFFE5BFA0, 0xFFE5A0D0, 0xFFAAA0E5, 0xFFA0E5E5, 0xFFAAE5A0, 0xFFE5D0A0,
   0xFFE5A0BF, 0xFFBCA0E5, 0xFFA0D4E5, 0xFFA0E5A7, 0xFFE5E2A0, 0xFFE5A0AE, 0xFFCDA0E5, 0xFFA0C3E5, 0xFFA0E5B8,
   0xFFD7E5A0, 0xFFE5A4A0, 0xFFDEA0E5, 0xFFA0B1E5, 0xFFA0E5C9, 0xFFC6E5A0, 0xFFE5B5A0, 0xFFE5A0DB,
};

const int profZoomBarHeight = 30;
const int profScaleHeight   = 20;
const int profRowHeight     = 30;

#define PROF_MAX_RENDER_THREAD_COUNT (8)
pthread_t profRenderThreads[PROF_MAX_RENDER_THREAD_COUNT];
sem_t     profRenderStartSemaphores[PROF_MAX_RENDER_THREAD_COUNT];
sem_t     profRenderEndSemaphore;
UIPainter* volatile profRenderPainter;
ProfFlameGraphReport* volatile profRenderReport;
int          profRenderThreadCount;
volatile int profRenderThreadIndexAllocator;
volatile int profRenderActiveThreads;

int ProfFlameGraphEntryCompare(const void* _a, const void* _b) {
   ProfFlameGraphEntry* a = (ProfFlameGraphEntry*)_a;
   ProfFlameGraphEntry* b = (ProfFlameGraphEntry*)_b;
   return a->_depth > b->_depth             ? 1
          : a->_depth < b->_depth           ? -1
          : a->_start_time > b->_start_time ? 1
          : a->_start_time < b->_start_time ? -1
                                            : 0;
}

void ProfShowSource(ProfFlameGraphReport* report) {
   ProfFlameGraphEntry* entry = report->_menu_item;
   if (!report->_functions.contains(entry->_this_function)) {
      s_main_window->show_dialog(0, "Source information was not found for this function.\n%f%b", "OK");
      return;
   }
   ProfFunctionEntry& function = report->_functions[entry->_this_function];

   if (!function._name[0]) {
      s_main_window->show_dialog(0, "Source information was not found for this function.\n%f%b", "OK");
      return;
   } else {
      DisplaySetPosition(report->_source_files[function._source_file_index]._path, function._line_number - 1, false);
   }
}

void ProfAddBreakpoint(ProfFlameGraphEntry* entry) { CommandSendToGDB(std::format("b {}", entry->_name)); }

void ProfFillView(ProfFlameGraphReport* report) {
   ProfFlameGraphEntry* entry = report->_menu_item;
   report->_x_start           = entry->_start_time;
   report->_x_end             = entry->_end_time;
   report->repaint(0);
}

void ProfDrawTransparentOverlay(UIPainter* painter, UIRectangle rectangle, uint32_t color) {
   rectangle = intersection(painter->_clip, rectangle);
   if (!rectangle.valid())
      return;

   for (int line = rectangle.t; line < rectangle.b; line++) {
      uint32_t* bits = painter->_bits + line * painter->_width + rectangle.l;

      for (int x = 0; x < rectangle.width(); x++) {
         uint32_t original = bits[x];
         uint32_t m1       = 180;
         uint32_t m2       = 255 - m1;
         uint32_t r2       = m2 * (original & 0x00FF00FF);
         uint32_t g2       = m2 * (original & 0x0000FF00);
         uint32_t r1       = m1 * (color & 0x00FF00FF);
         uint32_t g1       = m1 * (color & 0x0000FF00);
         uint32_t result   = 0xFF000000 | (0x0000FF00 & ((g1 + g2) >> 8)) | (0x00FF00FF & ((r1 + r2) >> 8));
         bits[x]           = result;
      }
   }
}

void* ProfFlameGraphRenderThread(void* _unused) {
   (void)_unused;
   int threadIndex = __sync_fetch_and_add(&profRenderThreadIndexAllocator, 1);

   while (true) {
      sem_wait(&profRenderStartSemaphores[threadIndex]);

      ProfFlameGraphReport* report = profRenderReport;
      UIElement*            el     = report;

      double     zoomX    = (double)report->_client.width() / (report->_x_end - report->_x_start);
      UIPainter  _painter = *profRenderPainter; // Some of the draw functions modify the painter's clip, so make a copy.
      UIPainter* painter  = &_painter;

      int64_t pr = 0, pd = 0;
      float   xStartF = (float)report->_x_start;
      float   xEndF   = (float)report->_x_end;

      size_t startIndex = report->_entries.size() / profRenderThreadIndexAllocator * threadIndex;
      size_t endIndex   = report->_entries.size() / profRenderThreadIndexAllocator * (threadIndex + 1);

      if (profRenderThreadCount == threadIndex + 1) {
         endIndex = report->_entries.size();
      }

      // printf("render on thread %d from %d to %d\n", threadIndex, startIndex, endIndex);

      for (size_t i = startIndex; i < endIndex; i++) {
         ProfFlameGraphEntryTime* time = &report->_entry_times[i];

         if (time->_end < xStartF || time->_start > xEndF) {
            continue;
         }

         int64_t rr = report->_client.l + (int64_t)((time->_end - report->_x_start) * zoomX + 0.999); // RECTANGLE_EARLY

         if (pr == rr && pd == time->_depth) {
            continue;
         }

         ProfFlameGraphEntry* entry = &report->_entries[i];

         // RECTANGLE_OTHER
         int64_t rl = report->_client.l + (int64_t)((time->_start - report->_x_start) * zoomX);
         int64_t rt =
            report->_client.t + time->_depth * profRowHeight + profScaleHeight - report->_v_scroll->position();
         int64_t rb = rt + profRowHeight;

         if (rl <= el->_clip.r && rr >= el->_clip.l && rt <= el->_clip.b && rb >= el->_clip.t) {
            // Carefully convert 64-bit integers to 32-bit integers for UIRectangle,
            // since the rectangle may be really large when zoomed in.
            UIRectangle r;
            r.l = rl < report->_client.l ? report->_client.l : rl;
            r.r = rr > report->_client.r ? report->_client.r : rr;
            r.t = rt < report->_client.t ? report->_client.t : rt;
            r.b = rb > report->_client.b ? report->_client.b : rb;

            painter->draw_block(UIRectangle(r.r - 1, r.r, r.t, r.b - 1), profBorderDarkColor);
            painter->draw_block(UIRectangle(r.l, r.r, r.b - 1, r.b), profBorderDarkColor);
            painter->draw_block(UIRectangle(r.l, r.r - 1, r.t, r.t + 1), profBorderLightColor);
            painter->draw_block(UIRectangle(r.l, r.l + 1, r.t + 1, r.b - 1), profBorderLightColor);

            bool hovered =
               report->_hover && report->_hover->_this_function == entry->_this_function && !report->_drag_mode;
            uint32_t color = hovered ? profHoverColor : profEntryColorPalette[entry->_color_index];
            /// uint32_t color = hovered ? profHoverColor : profMainColor;
            painter->draw_block(UIRectangle(r.l + 1, r.r - 1, r.t + 1, r.b - 1), color);

            if (r.width() > 40) {
               auto string = std::format("{} {:f}ms", entry->_name, entry->_end_time - entry->_start_time);
               painter->draw_string(UIRectangle(r.l + 2, r.r, r.t, r.b), string, profTextColor, UIAlign::left, nullptr);
            }
         }

         pr = rr, pd = entry->_depth;

         float nextDrawTime = 0.99f / zoomX + time->_end;

         for (; i < report->_entries.size(); i++) {
            if (report->_entry_times[i]._end >= nextDrawTime || report->_entry_times[i]._depth != time->_depth) {
               i--;
               break;
            }
         }
      }

      __sync_fetch_and_sub(&profRenderActiveThreads, 1);
      sem_post(&profRenderEndSemaphore);
   }
}

int ProfFlameGraphMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   ProfFlameGraphReport* report = (ProfFlameGraphReport*)el;

   if (msg == UIMessage::PAINT) {
      UIFont* previousFont = report->_font->activate();

      if (report->_x_start < 0)
         report->_x_start = 0;
      if (report->_x_end > report->_total_time)
         report->_x_end = report->_total_time;
      if (report->_x_end < report->_x_start + 1e-7)
         report->_x_end = report->_x_start + 1e-7;

      double zoomX = (double)report->_client.width() / (report->_x_end - report->_x_start);

      if (!profRenderThreadCount) {
         profRenderThreadCount = sysconf(_SC_NPROCESSORS_CONF);
         if (profRenderThreadCount < 1)
            profRenderThreadCount = 1;
         if (profRenderThreadCount > PROF_MAX_RENDER_THREAD_COUNT)
            profRenderThreadCount = PROF_MAX_RENDER_THREAD_COUNT;
         print("Using {} render threads.\n", profRenderThreadCount);

         sem_init(&profRenderEndSemaphore, 0, 0);

         for (int i = 0; i < profRenderThreadCount; i++) {
            sem_init(&profRenderStartSemaphores[i], 0, 0);
            pthread_create(&profRenderThreads[i], nullptr, ProfFlameGraphRenderThread, report);
         }
      }

      UIPainter* painter = (UIPainter*)dp;
      painter->draw_block(report->_client, profBackgroundColor);

      profRenderReport        = report;
      profRenderPainter       = painter;
      profRenderActiveThreads = profRenderThreadCount;
      __sync_synchronize();
      for (int i = 0; i < profRenderThreadCount; i++)
         sem_post(&profRenderStartSemaphores[i]);
      for (int i = 0; i < profRenderThreadCount; i++)
         sem_wait(&profRenderEndSemaphore);
      assert(!__sync_fetch_and_sub(&profRenderActiveThreads, 1));

      {
         UIRectangle r =
            UIRectangle(report->_client.l, report->_client.r, report->_client.t, report->_client.t + profScaleHeight);
         painter->draw_rectangle(r, profMainColor, profBorderDarkColor, UIRectangle(0, 0, 0, 1));

         double increment = 1000.0;
         while (increment > 1e-6 && increment * zoomX > 600.0)
            increment *= 0.1;

         double start = (painter->_clip.l - report->_client.l) / zoomX + report->_x_start;
         start -= fmod(start, increment) + increment;

         for (double i = start; i < report->_total_time; i += increment) {
            UIRectangle r;
            r.t = report->_client.t;
            r.b = r.t + profScaleHeight;
            r.l = report->_client.l + (int)((i - report->_x_start) * zoomX);
            r.r = r.l + (int)(increment * zoomX);
            if (r.l > painter->_clip.r)
               break;
            auto string = std::format("{:.4f}ms", i);
            painter->draw_block(UIRectangle(r.l, r.l + 1, r.t, r.b), profBorderLightColor);
            painter->draw_string(r, string, profTextColor, UIAlign::left, nullptr);
         }
      }

      if (report->_drag_mode == FLAME_GRAPH_DRAG_ZOOM_RANGE) {
         UIRectangle r = report->_client;
         r.l = report->_drag_initial_point, r.r = report->_drag_current_point;
         if (r.l > r.r)
            r.r = report->_drag_initial_point, r.l = report->_drag_current_point;
         painter->draw_invert(r);
      }

      if (report->_thumbnail) {
         UIRectangle zoomBar =
            UIRectangle(report->_client.l, report->_client.r, report->_client.b - profZoomBarHeight, report->_client.b);
         UIRectangle zoomBarThumb = zoomBar;
         zoomBarThumb.l           = zoomBar.l + zoomBar.width() * (report->_x_start / report->_total_time);
         zoomBarThumb.r           = zoomBar.l + zoomBar.width() * (report->_x_end / report->_total_time);
         UIRectangle drawBounds   = intersection(zoomBar, painter->_clip);

         for (int i = drawBounds.t; i < drawBounds.b; i++) {
            for (int j = drawBounds.l; j < drawBounds.r; j++) {
               int si = (i - zoomBar.t) * report->_thumbnail_height / zoomBar.height();
               int sj = (j - zoomBar.l) * report->_thumbnail_width / zoomBar.width();

               if (si >= 0 && si < report->_thumbnail_height && sj >= 0 && sj < report->_thumbnail_width) {
                  painter->_bits[i * painter->_width + j] = report->_thumbnail[si * report->_thumbnail_width + sj];
               }
            }
         }

         painter->draw_border(zoomBar, profBorderDarkColor, UIRectangle(2));
         painter->draw_border(zoomBarThumb, profBorderLightColor, UIRectangle(4));
      }

      if (report->_hover && !report->_drag_mode) {
         const ProfFunctionEntry& function = report->_functions[report->_hover->_this_function];

         char line1[256], line2[256], line3[256];
         std_format_to_n(line1, sizeof(line1), "[{}] {}:{}", report->_hover->_name,
                         function._source_file_index != -1 ? report->_source_files[function._source_file_index]._path
                                                           : "??",
                         function._line_number);
         std_format_to_n(line2, sizeof(line2), "This call: {:f}ms {:.1f}%%",
                         report->_hover->_end_time - report->_hover->_start_time,
                         (report->_hover->_end_time - report->_hover->_start_time) / report->_total_time * 100.0);
         std_format_to_n(line3, sizeof(line3), "Total: {:f}ms in {} calls ({:f}ms avg) {:.1f}%%", function._total_time,
                         function._call_count, function._total_time / function._call_count,
                         function._total_time / report->_total_time * 100.0);

         UI* ui         = el->ui();
         int width      = 0;
         int line1Width = ui->string_width(line1);
         if (width < line1Width)
            width = line1Width;
         int line2Width = ui->string_width(line2);
         if (width < line2Width)
            width = line2Width;
         int line3Width = ui->string_width(line3);
         if (width < line3Width)
            width = line3Width;
         int lineHeight = ui->string_height();
         int height     = 3 * lineHeight;

         auto pos = el->cursor_pos();
         int  x   = pos.x;
         if (x + width > el->_clip.r)
            x = el->_clip.r - width;
         int y = pos.y + 25;
         if (y + height > el->_clip.b)
            y = pos.y - height - 10;
         UIRectangle rectangle = UIRectangle(x, x + width, y, y + height);

         ProfDrawTransparentOverlay(painter, rectangle + ui_rect_1i(-5), 0xFF000000);
         painter->draw_string(UIRectangle(x, x + width, y + lineHeight * 0, y + lineHeight * 1), line1, 0xFFFFFFFF,
                              UIAlign::left, 0);
         painter->draw_string(UIRectangle(x, x + width, y + lineHeight * 1, y + lineHeight * 2), line2, 0xFFFFFFFF,
                              UIAlign::left, 0);
         painter->draw_string(UIRectangle(x, x + width, y + lineHeight * 2, y + lineHeight * 3), line3, 0xFFFFFFFF,
                              UIAlign::left, 0);
      }

      previousFont->activate();
   } else if (msg == UIMessage::MOUSE_MOVE) {
      double               zoomX = (double)report->_client.width() / (report->_x_end - report->_x_start);
      ProfFlameGraphEntry* hover = nullptr;
      auto                 pos   = el->cursor_pos();

      int   depth   = (pos.y - report->_client.t + report->_v_scroll->position() - profScaleHeight) / profRowHeight;
      float xStartF = (float)report->_x_start;
      float xEndF   = (float)report->_x_end;

      for (size_t i = 0; i < report->_entries.size(); i++) {
         ProfFlameGraphEntryTime* time = &report->_entry_times[i];

         if (time->_depth != depth || time->_end < xStartF || time->_start > xEndF) {
            continue;
         }

         int64_t rr = report->_client.l + (int64_t)((time->_end - report->_x_start) * zoomX + 0.999); // RECTANGLE_EARLY
         // RECTANGLE_OTHER
         int64_t rl = report->_client.l + (int64_t)((time->_start - report->_x_start) * zoomX);
         int64_t rt =
            report->_client.t + time->_depth * profRowHeight + profScaleHeight - report->_v_scroll->position();
         int64_t rb = rt + profRowHeight;

         (void)rt;
         (void)rb;

         if (pos.x >= rl && pos.x < rr) {
            hover = &report->_entries[i];
            break;
         }
      }

      if (hover != report->_hover || hover /* to repaint the tooltip */) {
         report->_hover = hover;
         el->repaint(nullptr);
      }
   } else if (msg == UIMessage::UPDATE) {
      if (report->_hover && !el->is_hovered()) {
         report->_hover = nullptr;
         el->repaint(nullptr);
      }
   } else if (msg == UIMessage::LEFT_DOWN) {
      auto pos = el->cursor_pos();
      if (pos.y < report->_client.b - profZoomBarHeight) {
         report->_drag_mode           = FLAME_GRAPH_DRAG_PAN;
         report->_drag_initial_value  = report->_x_start;
         report->_drag_initial_point  = pos.x;
         report->_drag_initial_value2 = report->_v_scroll->position();
         report->_drag_initial_point2 = pos.y;
         el->_window->set_cursor((int)UICursor::hand);
      } else {
         report->_drag_mode          = FLAME_GRAPH_DRAG_X_SCROLL;
         report->_drag_initial_value = report->_x_start;
         report->_drag_initial_point = pos.x;
         report->_drag_scroll_rate   = 1.0;

         if (pos.x < report->_client.l + report->_client.width() * (report->_x_start / report->_total_time) ||
             pos.y >= report->_client.l + report->_client.width() * (report->_x_end / report->_total_time)) {
            report->_drag_scroll_rate = 0.2;
         }
      }
   } else if (msg == UIMessage::MIDDLE_DOWN) {
      auto pos = el->cursor_pos();
      if (pos.y < report->_client.b - profZoomBarHeight) {
         report->_drag_mode           = FLAME_GRAPH_DRAG_X_PAN_AND_ZOOM;
         report->_drag_initial_value  = report->_x_start;
         report->_drag_initial_point  = pos.x;
         report->_drag_initial_point2 = pos.y;
         el->_window->set_cursor((int)UICursor::cross_hair);
      }
   } else if (msg == UIMessage::RIGHT_DOWN) {
      auto pos = el->cursor_pos();
      if (pos.y < report->_client.b - profZoomBarHeight) {
         report->_drag_mode          = FLAME_GRAPH_DRAG_ZOOM_RANGE;
         report->_drag_initial_point = pos.x;
      }
   } else if (msg == UIMessage::LEFT_UP || msg == UIMessage::RIGHT_UP || msg == UIMessage::MIDDLE_UP) {
      if (report->_drag_mode == FLAME_GRAPH_DRAG_ZOOM_RANGE && report->_drag_started) {
         UIRectangle r = report->_client;
         r.l = report->_drag_initial_point, r.r = report->_drag_current_point;
         if (r.l > r.r)
            r.r = report->_drag_initial_point, r.l = report->_drag_current_point;
         double zoomX     = (double)report->_client.width() / (report->_x_end - report->_x_start);
         report->_x_end   = (r.r - report->_client.l) / zoomX + report->_x_start;
         report->_x_start = (r.l - report->_client.l) / zoomX + report->_x_start;
      } else if (!report->_drag_started && msg == UIMessage::RIGHT_UP && report->_hover) {
         report->_menu_item = report->_hover;
         el->ui()
            ->create_menu(el->_window, UIMenu::NO_SCROLL)
            .add_item(0, "Show source", [report](UIButton&) { ProfShowSource(report); })
            .add_item(0, "Add breakpoint", [report](UIButton&) { ProfAddBreakpoint(report->_hover); })
            .add_item(0, "Fill view", [report](UIButton&) { ProfFillView(report); })
            .show();
      } else if (!report->_drag_started && msg == UIMessage::MIDDLE_UP && report->_hover) {
         report->_menu_item = report->_hover;
         ProfFillView(report);
      }

      report->_drag_mode    = 0;
      report->_drag_started = false;
      el->repaint(nullptr);
      el->_window->set_cursor((int)UICursor::arrow);
   } else if (msg == UIMessage::MOUSE_DRAG) {
      report->_drag_started = true;
      auto pos              = el->cursor_pos();

      if (report->_drag_mode == FLAME_GRAPH_DRAG_PAN) {
         double delta     = report->_x_end - report->_x_start;
         report->_x_start = report->_drag_initial_value - (double)(pos.x - report->_drag_initial_point) *
                                                             report->_total_time / report->_client.width() * delta /
                                                             report->_total_time;
         report->_x_end = report->_x_start + delta;
         if (report->_x_start < 0) {
            report->_x_end -= report->_x_start;
            report->_x_start = 0;
         }
         if (report->_x_end > report->_total_time) {
            report->_x_start += report->_total_time - report->_x_end;
            report->_x_end = report->_total_time;
         }
         report->_v_scroll->position() = report->_drag_initial_value2 - (double)(pos.y - report->_drag_initial_point2);
         report->_v_scroll->refresh();
      } else if (report->_drag_mode == FLAME_GRAPH_DRAG_X_SCROLL) {
         double delta     = report->_x_end - report->_x_start;
         report->_x_start = report->_drag_initial_value + (double)(pos.x - report->_drag_initial_point) *
                                                             report->_total_time / report->_client.width() *
                                                             report->_drag_scroll_rate;
         report->_x_end = report->_x_start + delta;
         if (report->_x_start < 0) {
            report->_x_end -= report->_x_start;
            report->_x_start = 0;
         }
         if (report->_x_end > report->_total_time) {
            report->_x_start += report->_total_time - report->_x_end;
            report->_x_end = report->_total_time;
         }
      } else if (report->_drag_mode == FLAME_GRAPH_DRAG_X_PAN_AND_ZOOM) {
         double delta = report->_x_end - report->_x_start;
         report->_x_start += (double)(pos.x - report->_drag_initial_point) * report->_total_time /
                             report->_client.width() * delta / report->_total_time * 3.0;
         report->_x_end = report->_x_start + delta;
         double factor  = powf(1.02, pos.y - report->_drag_initial_point2);
         double mouse   = (double)(pos.x - report->_client.l) / report->_client.width();
#if 0
         mouse = 0.5;
         XWarpPointer(ui->display, None, windowMain->window, 0, 0, 0, 0, report->dragInitialPoint, report->dragInitialPoint2);
#else
         report->_drag_initial_point  = pos.x;
         report->_drag_initial_point2 = pos.y;
#endif
         double newZoom = (report->_x_end - report->_x_start) / report->_total_time * factor;
         report->_x_start += mouse * (report->_x_end - report->_x_start) * (1 - factor);
         report->_x_end = newZoom * report->_total_time + report->_x_start;
      } else if (report->_drag_mode == FLAME_GRAPH_DRAG_ZOOM_RANGE) {
         report->_drag_current_point = pos.x;
      }

      el->repaint(nullptr);
   } else if (msg == UIMessage::MOUSE_WHEEL) {
      auto   pos         = el->cursor_pos();
      int    divisions   = di / 72;
      double factor      = 1;
      double perDivision = 1.2f;
      while (divisions > 0)
         factor *= perDivision, divisions--;
      while (divisions < 0)
         factor /= perDivision, divisions++;
      double mouse   = (double)(pos.x - report->_client.l) / report->_client.width();
      double newZoom = (report->_x_end - report->_x_start) / report->_total_time * factor;
      report->_x_start += mouse * (report->_x_end - report->_x_start) * (1 - factor);
      report->_x_end = newZoom * report->_total_time + report->_x_start;
      el->repaint(nullptr);
      return 1;
   } else if (msg == UIMessage::GET_CURSOR) {
      return report->_drag_mode == FLAME_GRAPH_DRAG_PAN              ? (int)UICursor::hand
             : report->_drag_mode == FLAME_GRAPH_DRAG_X_PAN_AND_ZOOM ? (int)UICursor::cross_hair
                                                                     : (int)UICursor::arrow;
   } else if (msg == UIMessage::LAYOUT) {
      UIRectangle scrollBarBounds = el->_bounds;
      scrollBarBounds.l           = scrollBarBounds.r - ui_size::scroll_bar * el->_window->scale();
      report->_v_scroll->set_page(el->_bounds.height() - profZoomBarHeight);
      report->_v_scroll->move(scrollBarBounds, true);
      report->_client   = el->_bounds;
      report->_client.r = scrollBarBounds.l;
   } else if (msg == UIMessage::SCROLLED) {
      el->refresh();
   } else if (msg == UIMessage::DESTROY) {
      report->_entries.clear();
      report->_functions.clear();
      report->_source_files.clear();
      report->_entry_times.clear();
      free(report->_thumbnail);
   }

   return 0;
}

int ProfReportWindowMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   ProfFlameGraphReport* report = (ProfFlameGraphReport*)el->_cp;

   if (msg == UIMessage::LAYOUT) {
      if (report->_showing_table) {
         report->_flags |= UIElement::hide_flag;
         report->_table->_flags &= ~UIElement::hide_flag;
      } else {
         report->_flags &= ~UIElement::hide_flag;
         report->_table->_flags |= UIElement::hide_flag;
      }
      el->_class_proc(el, msg, di, dp);
      report->_table->move(report->_bounds, false);
      return 1;
   }

   return 0;
}

void ProfSwitchView(ProfFlameGraphReport* report) {
   report->_showing_table = !report->_showing_table;
   report->_switch_view_button->set_label(report->_showing_table ? "Graph view" : "Table view");
   report->_parent->refresh();
}

#define PROF_FUNCTION_COMPARE(a, b)                                 \
   int a(const void* c, const void* d) {                            \
      const ProfFunctionEntry* left  = (const ProfFunctionEntry*)c; \
      const ProfFunctionEntry* right = (const ProfFunctionEntry*)d; \
      return b;                                                     \
   }
#define PROF_COMPARE_NUMBERS(a, b) (a) > (b) ? -1 : (a) < (b) ? 1 : 0

PROF_FUNCTION_COMPARE(ProfFunctionCompareName, strcmp(left->_name, right->_name));
PROF_FUNCTION_COMPARE(ProfFunctionCompareTotalTime, PROF_COMPARE_NUMBERS(left->_total_time, right->_total_time));
PROF_FUNCTION_COMPARE(ProfFunctionCompareCallCount, PROF_COMPARE_NUMBERS(left->_call_count, right->_call_count));
PROF_FUNCTION_COMPARE(ProfFunctionCompareAverage, PROF_COMPARE_NUMBERS(left->_total_time / left->_call_count,
                                                                       right->_total_time / right->_call_count));
PROF_FUNCTION_COMPARE(ProfFunctionComparePercentage, PROF_COMPARE_NUMBERS(left->_total_time, right->_total_time));

int ProfTableMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   ProfFlameGraphReport* report = (ProfFlameGraphReport*)el->_cp;
   UITable*              table  = report->_table;

   if (msg == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem*    m     = (UITableGetItem*)dp;
      ProfFunctionEntry* entry = &report->_sorted_functions[m->_row];

      if (m->_column == 0) {
         return m->format_to("{}", entry->_name);
      } else if (m->_column == 1) {
         return m->format_to("{:f}", entry->_total_time);
      } else if (m->_column == 2) {
         return m->format_to("{}", entry->_call_count);
      } else if (m->_column == 3) {
         return m->format_to("{:f}", entry->_total_time / entry->_call_count);
      } else if (m->_column == 4) {
         return m->format_to("{:f}", entry->_total_time / report->_total_time * 100);
      }
   } else if (msg == UIMessage::LEFT_DOWN) {
      int index = table->header_hittest(el->cursor_pos());

      if (index != -1) {
         if (index == 0) {
            qsort(report->_sorted_functions.data(), report->_sorted_functions.size(), sizeof(ProfFunctionEntry),
                  ProfFunctionCompareName);
         } else if (index == 1) {
            qsort(report->_sorted_functions.data(), report->_sorted_functions.size(), sizeof(ProfFunctionEntry),
                  ProfFunctionCompareTotalTime);
         } else if (index == 2) {
            qsort(report->_sorted_functions.data(), report->_sorted_functions.size(), sizeof(ProfFunctionEntry),
                  ProfFunctionCompareCallCount);
         } else if (index == 3) {
            qsort(report->_sorted_functions.data(), report->_sorted_functions.size(), sizeof(ProfFunctionEntry),
                  ProfFunctionCompareAverage);
         } else if (index == 4) {
            qsort(report->_sorted_functions.data(), report->_sorted_functions.size(), sizeof(ProfFunctionEntry),
                  ProfFunctionComparePercentage);
         }

         el->refresh();
         table->set_column_highlight(index);
      }
   } else if (msg == UIMessage::GET_CURSOR) {
      return table->header_hittest(el->cursor_pos()) == -1 ? (int)UICursor::arrow : (int)UICursor::hand;
   }

   return 0;
}

void ProfLoadProfileData(void* _window) {
   ProfWindow* data = (ProfWindow*)_window;

   auto        res              = EvaluateExpression("gfProfilingTicksPerMs");
   const char* ticksPerMsString = strstr(res.c_str(), "= ");
   data->_ticks_per_ms          = ticksPerMsString ? sv_atoi(ticksPerMsString, 2) : 0;

   if (!ticksPerMsString || !data->_ticks_per_ms) {
      s_main_window->show_dialog(0, "Profile data could not be loaded (1).\nConsult the guide.\n%f%b", "OK");
      return;
   }

   auto pos           = EvaluateExpression("gfProfilingBufferPosition");
   int  rawEntryCount = sv_atoi(strstr(pos.c_str(), "= "), 2);
   print("Reading {} profiling entries...\n", rawEntryCount);

   if (rawEntryCount == 0) {
      return;
   }

   if (rawEntryCount > 10000000) {
      // Show a loading message.
      UIWindow* window = s_main_window;
      UIPainter painter(window);
      char      string[256];
      std_format_to_n(string, sizeof(string), "Loading data... (estimated time: {} seconds)",
                      rawEntryCount / 5000000 + 1);
      painter.draw_block(painter._clip, painter.theme().panel1);
      painter.draw_string(painter._clip, string, painter.theme().text, UIAlign::center, 0);
      window->set_update_region(ui_rect_2s(window->width(), window->height()));
      window->endpaint(nullptr);
      window->set_update_region(painter._clip);
   }

   ProfProfilingEntry* rawEntries = (ProfProfilingEntry*)calloc(sizeof(ProfProfilingEntry), rawEntryCount);

   char path[PATH_MAX];
   realpath(".profile.gf", path);
   char buffer[PATH_MAX * 2];
   std_format_to_n(buffer, sizeof(buffer),
                   "dump binary memory {} (gfProfilingBuffer) (gfProfilingBuffer+gfProfilingBufferPosition)", path);
   (void)EvaluateCommand(buffer);
   FILE* f = fopen(path, "rb");

   if (!f) {
      s_main_window->show_dialog(0, "Profile data could not be loaded (2).\nConsult the guide.\n%f%b", "OK");
      free(rawEntries);
      return;
   }

   fread(rawEntries, 1, sizeof(ProfProfilingEntry) * rawEntryCount, f);
   fclose(f);
   unlink(path);

   print("Got raw profile data.\n");

   unordered_map<void*, ProfFunctionEntry> functions   = {};
   vector<ProfSourceFileEntry>             sourceFiles = {};

   int stackErrorCount = 0;
   int stackDepth      = 0;

   for (int i = 0; i < rawEntryCount; i++) {
      if (rawEntries[i]._time_stamp >> 63) {
         if (stackDepth)
            stackDepth--;
         else
            stackErrorCount++;
      } else {
         stackDepth++;
      }

      if (functions.contains(rawEntries[i]._this_function))
         continue;
      ProfFunctionEntry& function = functions[rawEntries[i]._this_function];

      function._source_file_index = -1;

      std_format_to_n(buffer, sizeof(buffer), "(void *) {:p}", rawEntries[i]._this_function);
      auto cName = EvaluateExpression(buffer);
      if (cName.empty())
         continue;

      if (strchr(cName.c_str(), '<'))
         cName = strchr(cName.c_str(), '<') + 1;
      int length = strlen(cName.c_str());
      if (length > (int)sizeof(function._name) - 1)
         length = sizeof(function._name) - 1;
      memcpy(function._name, cName.c_str(), length);
      function._name[length] = 0;

      int inTemplate = 0;

      for (int j = 0; j < length; j++) {
         if (function._name[j] == '(' && !inTemplate) {
            function._name[j] = 0;
            break;
         } else if (function._name[j] == '<') {
            inTemplate++;
         } else if (function._name[j] == '>') {
            if (inTemplate) {
               inTemplate--;
            } else {
               function._name[j] = 0;
               break;
            }
         }
      }

      std_format_to_n(buffer, sizeof(buffer), "py print(gdb.lookup_global_symbol('{}').symtab.filename)",
                      function._name);
      auto res = EvaluateCommand(buffer);

      if (!res.contains("Traceback (most recent call last):")) {
         resize_to_lf(res);
         ProfSourceFileEntry sourceFile  = {};
         const char*         cSourceFile = res.c_str();
         length                          = strlen(cSourceFile);
         if (length > (int)sizeof(sourceFile._path) - 1)
            length = sizeof(sourceFile._path) - 1;
         memcpy(sourceFile._path, cSourceFile, length);
         sourceFile._path[length] = 0;
         std_format_to_n(buffer, sizeof(buffer), "py print(gdb.lookup_global_symbol('{}').line)", function._name);
         res                   = EvaluateCommand(buffer);
         function._line_number = sv_atoi(res);

         for (size_t i = 0; i < sourceFiles.size(); i++) {
            if (0 == strcmp(sourceFiles[i]._path, sourceFile._path)) {
               function._source_file_index = i;
               break;
            }
         }

         if (function._source_file_index == -1) {
            function._source_file_index = sourceFiles.size();
            sourceFiles.push_back(sourceFile);
         }
      }
   }

   UIMDIChild* window = &s_data_window->add_mdichild(UIMDIChild::CLOSE_BUTTON, ui_rect_2s(800, 600), "Flame graph");
   ProfFlameGraphReport* report = new ProfFlameGraphReport(window, 0);

   report->_switch_view_button = &window->add_button(UIButton::SMALL | UIElement::non_client_flag, "Table view")
                                     .set_cp(report)
                                     .on_click([report](UIButton&) { ProfSwitchView(report); });
   UITable* table = report->_table = &window->add_table(0, "Name\tTime spent (ms)\tCall count\tAverage per call (ms)")
                                         .set_cp(report)
                                         .set_user_proc(ProfTableMessage);

   report->_v_scroll = &report->add_scrollbar(0);
   report->_font     = data->_font_flame_graph;

   window->set_cp(report).set_user_proc(ProfReportWindowMessage);

   report->_functions    = functions;
   functions             = {};
   report->_source_files = sourceFiles;
   sourceFiles           = {};

   vector<ProfFlameGraphEntry> stack = {};

   for (int i = 0; i < stackErrorCount; i++) {
      ProfFlameGraphEntry entry = {};
      entry._name               = "[unknown]";
      entry._start_time         = 0;
      entry._depth              = stack.size();
      stack.push_back(entry);
   }

   for (int i = 0; i < rawEntryCount; i++) {
      if (rawEntries[i]._time_stamp >> 63) {
         if (!stack.size()) {
            continue;
         }

         ProfFlameGraphEntry entry = stack.back();
         entry._end_time           = (double)((rawEntries[i]._time_stamp & 0x7FFFFFFFFFFFFFFFUL) -
                                    (rawEntries[0]._time_stamp & 0x7FFFFFFFFFFFFFFFUL)) /
                           data->_ticks_per_ms;

         if (0 == strcmp(entry._name, "[unknown]")) {
            if (report->_functions.contains(rawEntries[i]._this_function))
               entry._name = report->_functions[rawEntries[i]._this_function]._name;
         }

         entry._this_function = rawEntries[i]._this_function;
         stack.pop_back();
         report->_entries.push_back(entry);
      } else {
         ProfFlameGraphEntry entry = {};
         if (report->_functions.contains(rawEntries[i]._this_function)) {
            ProfFunctionEntry& function = report->_functions[rawEntries[i]._this_function];
            entry._name                 = function._name;
            entry._color_index =
               function._source_file_index % (sizeof(profEntryColorPalette) / sizeof(profEntryColorPalette[0]));
         }

         entry._start_time = (double)(rawEntries[i]._time_stamp - (rawEntries[0]._time_stamp & 0x7FFFFFFFFFFFFFFFUL)) /
                             data->_ticks_per_ms;
         entry._this_function = rawEntries[i]._this_function;
         entry._depth         = stack.size();
         stack.push_back(entry);
      }
   }

   for (const auto& entry : report->_entries) {
      if (entry._end_time > report->_total_time) {
         report->_total_time = entry._end_time;
      }
   }

   while (stack.size()) {
      ProfFlameGraphEntry entry = stack.back();
      entry._end_time           = report->_total_time;
      stack.pop_back();
      report->_entries.push_back(entry);
   }

   if (!report->_total_time) {
      report->_total_time = 1;
   }

   stack.clear();
   report->_x_end = report->_total_time;
   qsort(report->_entries.data(), report->_entries.size(), sizeof(ProfFlameGraphEntry), ProfFlameGraphEntryCompare);

   int maxDepth = 0;

   for (const auto& entry : report->_entries) {
      ProfFlameGraphEntryTime time;
      time._start = entry._start_time;
      time._end   = entry._end_time;
      time._depth = entry._depth;
      report->_entry_times.push_back(time);

      if (entry._depth > maxDepth) {
         maxDepth = entry._depth;
      }

      ProfFunctionEntry& function = report->_functions[entry._this_function];
      function._call_count++;
      function._total_time += entry._end_time - entry._start_time;
   }

   print("Found {} functions over {} source files.\n", report->_functions.size(), report->_source_files.size());

   report->_v_scroll->set_maximum((maxDepth + 2) * 30);

   for (const auto& [k, v] : report->_functions) {
      if (k)
         report->_sorted_functions.push_back(v);
   }

   {
      // Create an image of the graph for the zoom bar.
      uint32_t  width  = 1200;
      uint32_t  height = maxDepth * 30 + 30;
      UIPainter painter(report->ui(), width, height, (uint32_t*)malloc(width * height * 4));

      report->_client = report->_bounds = report->_clip = painter._clip;
      ProfFlameGraphMessage(report, UIMessage::PAINT, 0, &painter);
      int newHeight = 30;
      ThumbnailResize(painter._bits, painter._width, painter._height, painter._width, newHeight);
      report->_thumbnail        = (uint32_t*)realloc(painter._bits, painter._width * newHeight * 4);
      report->_thumbnail_width  = width;
      report->_thumbnail_height = newHeight;
   }

   table->set_num_items(report->_sorted_functions.size());
   qsort(report->_sorted_functions.data(), report->_sorted_functions.size(), sizeof(ProfFunctionEntry),
         ProfFunctionCompareTotalTime);
   table->set_column_highlight(1);
   table->resize_columns();

   free(rawEntries);
}

void ProfStepOverProfiled(ProfWindow* window) {
   (void)EvaluateCommand("call GfProfilingStart()");
   CommandSendToGDB("gf-next");
   window->_in_step_over_profiled = true;
}

void ProfWindowUpdate(const char* data, UIElement* el) {
   ProfWindow* window = (ProfWindow*)el->_cp;

   if (window->_in_step_over_profiled) {
      (void)EvaluateCommand("call GfProfilingStop()");
      ProfLoadProfileData(window);
      ctx.switch_to_window_and_focus("Data");
      s_data_window->refresh();
      window->_in_step_over_profiled = false;
   }
}

UIElement* ProfWindowCreate(UIElement* parent) {
   const int   fontSizeFlameGraph = 8;
   ProfWindow* window             = new ProfWindow;
   UI*         ui                 = parent->ui();
   window->_font_flame_graph      = ui->create_font(ui->default_font_path(), fontSizeFlameGraph);
   UIPanel* panel                 = &parent->add_panel(UIPanel::COLOR_1 | UIPanel::EXPAND).set_cp(window);
   panel->add_button(UIElement::v_fill, "Step over profiled").on_click([window](UIButton&) {
      ProfStepOverProfiled(window);
   });

#ifdef UI_FREETYPE
   // Since we will do multithreaded painting with fontFlameGraph, we need to make sure all its glyphs are ready to go.
   for (uintptr_t i = 0; i < sizeof(window->_font_flame_graph->_glyphs_rendered); i++) {
      UIPainter fakePainter(parent->ui(), 0, 0, nullptr);
      UIFont*   previousFont = window->_font_flame_graph->activate();
      fakePainter.draw_glyph(0, 0, i, 0xFF000000);
      previousFont->activate();
   }
#endif

   return panel;
}

/////////////////////////////////////////////////////
// Memory window:
/////////////////////////////////////////////////////

// TODO Click a pointer to go to that address.
// TODO Click a function pointer to go the source location.
// TODO Better string visualization.
// TODO Moving between watch window and memory window.
// TODO Set data breakpoints.
// TODO Highlight modified bytes.

static int  MemoryWindowMessage(UIElement* el, UIMessage msg, int di, void* dp);
static void MemoryWindowGotoButtonInvoke(void* cp);

struct MemoryWindow : public UIElement {
   UIButton*       _goto_button;
   vector<int16_t> _loaded_bytes;
   uint64_t        _offset;

   MemoryWindow(UIElement* parent)
      : UIElement(parent, 0, MemoryWindowMessage, "memory window")
      , _goto_button(
           &add_button(UIButton::SMALL, "&").on_click([this](UIButton&) { MemoryWindowGotoButtonInvoke(this); })) {}

   int _message_proc(UIMessage msg, int di, void* dp) {
      if (msg == UIMessage::PAINT) {
         const auto& thm     = theme();
         UIPainter*  painter = (UIPainter*)dp;
         painter->draw_block(_bounds, thm.panel1);

         char        buffer[64];
         uint64_t    address   = _offset;
         const int   rowHeight = ui()->string_height();
         UIRectangle row       = _bounds + ui_rect_1i(10);
         size_t      rowCount  = (painter->_clip.b - row.t) / rowHeight;
         row.b                 = row.t + rowHeight;

         {
            std_format_to_n(buffer, sizeof(buffer), "Inspecting memory @{:p}", (void*)_offset);
            painter->draw_string(row, buffer, thm.codeString, UIAlign::left, 0);
            row.t += rowHeight;
            row.b += rowHeight;
            const char* header = "         0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F   0123456789ABCDEF";
            painter->draw_string(row, header, thm.codeComment, UIAlign::left, 0);
            row.t += rowHeight;
            row.b += rowHeight;
         }

         if (rowCount > 0 && rowCount * 16 > _loaded_bytes.size()) {
            _loaded_bytes.clear();

            for (size_t i = 0; i < (size_t)rowCount * 16 / 8; i++) {
               std_format_to_n(buffer, sizeof(buffer), "x/8xb 0x{:x}", _offset + i * 8);
               auto res = EvaluateCommand(buffer);

               bool error = true;

               if (!res.contains("Cannot access memory")) {
                  const char* position = strchr(res.c_str(), ':');

                  if (position) {
                     position++;

                     for (int i = 0; i < 8; i++) {
                        _loaded_bytes.push_back(strtol(position, nullptr, 0));
                     }

                     error = false;
                  }
               }

               if (error) {
                  for (int i = 0; i < 8; i++) {
                     _loaded_bytes.push_back(-1);
                  }
               }
            }
         }

         while (row.t < painter->_clip.b) {
            int position = 0;

            const auto& thm = theme();
            std_format_to_n(buffer, sizeof(buffer), "{:8X} ", (uint32_t)(address & 0xFFFFFFFF));
            painter->draw_string(row, buffer, thm.codeComment, UIAlign::left, 0);
            UIRectangle r          = row + UIRectangle(ui()->string_width(buffer), 0, 0, 0);
            int         glyphWidth = ui()->string_width("a");

            for (int i = 0; i < 16; i++) {
               if (address + i >= _offset + _loaded_bytes.size() || _loaded_bytes[address + i - _offset] < 0) {
                  painter->draw_glyph(r.l + position, r.t, '?', thm.codeOperator);
                  position += glyphWidth;
                  painter->draw_glyph(r.l + position, r.t, '?', thm.codeOperator);
                  position += glyphWidth;
               } else {
                  const char* hexChars = "0123456789ABCDEF";
                  uint8_t     byte     = _loaded_bytes[address + i - _offset];
                  painter->draw_glyph(r.l + position, r.t, hexChars[(byte & 0xF0) >> 4], thm.codeNumber);
                  position += glyphWidth;
                  painter->draw_glyph(r.l + position, r.t, hexChars[(byte & 0x0F) >> 0], thm.codeNumber);
                  position += glyphWidth;

                  if (byte >= 0x20 && byte < 0x7F) {
                     painter->draw_glyph(r.l + (49 + i) * glyphWidth, r.t, byte, thm.codeString);
                  }
               }

               position += glyphWidth;
            }

            row.t += rowHeight;
            row.b += rowHeight;
            address += 0x10;
         }
      } else if (msg == UIMessage::LAYOUT) {
         UIRectangle bounds = _bounds + ui_rect_1i(10);
         _goto_button->move(UIRectangle(bounds.r - _goto_button->message(UIMessage::GET_WIDTH, 0, 0), bounds.r,
                                        bounds.t, bounds.t + _goto_button->message(UIMessage::GET_HEIGHT, 0, 0)),
                            false);
      } else if (msg == UIMessage::MOUSE_WHEEL) {
         _offset += di / 72 * 0x10;
         _loaded_bytes.clear();
         repaint(nullptr);
      }

      return 0;
   }

   static int MemoryWindowMessage(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<MemoryWindow*>(el)->_message_proc(msg, di, dp);
   }
};

void MemoryWindowUpdate(const char* data, UIElement* el) {
   MemoryWindow* window = (MemoryWindow*)el;
   window->_loaded_bytes.clear();
   el->repaint(nullptr);
}

void MemoryWindowGotoButtonInvoke(void* cp) {
   MemoryWindow* window     = (MemoryWindow*)cp;
   char*         expression = nullptr;

   if (s_main_window->show_dialog(0, "Enter address expression:\n%t\n%f%b%b", &expression, "Goto", "Cancel") ==
       "Goto") {
      char buffer[4096];
      std_format_to_n(buffer, sizeof(buffer), "py gf_valueof(['{}'],' ')", expression);
      auto        res    = EvaluateCommand(buffer);
      const char* result = res.c_str();

      if (result && ((*result == '(' && isdigit(result[1])) || isdigit(*result))) {
         if (*result == '(')
            result++;
         uint64_t address = strtol(result, nullptr, 0);

         if (address) {
            window->_loaded_bytes.clear();
            window->_offset = address & ~0xF;
            window->repaint(nullptr);
         } else {
            s_main_window->show_dialog(0, "Cannot access memory at address 0.\n%f%b", "OK");
         }
      } else {
         s_main_window->show_dialog(0, "Expression did not evaluate to an address.\n%f%b", "OK");
      }
   }

   free(expression);
}

UIElement* MemoryWindowCreate(UIElement* parent) { return new MemoryWindow(parent); }

/////////////////////////////////////////////////////
// View window:
/////////////////////////////////////////////////////

static int ViewWindowColorSwatchMessage(UIElement* el, UIMessage msg, int di, void* dp);
static int ViewWindowMatrixGridMessage(UIElement* el, UIMessage msg, int di, void* dp);

struct ViewWindowColorSwatch : public UIElement {
   uint32_t color;

   ViewWindowColorSwatch(UIElement* parent, uint32_t color)
      : UIElement(parent, 0, ViewWindowColorSwatchMessage, "Color swatch")
      , color(color) {}
};

enum class grid_type_t { char_t, float_t, double_t };

struct storage_t {
   virtual ~storage_t()              = default;
   virtual void        read(FILE* f) = 0;
   virtual const char* data() const  = 0;
};

template <class T>
struct storage_impl_t : public storage_t {
   int       w, h;
   vector<T> storage;

   storage_impl_t(int w, int h)
      : w(w)
      , h(h)
      , storage(w * h) {}

   void read(FILE* f) final { fread(storage.data(), 1, w * h * sizeof(T), f); }

   const char* data() const final { return (const char*)storage.data(); }
};

struct ViewWindowMatrixGrid : public UIElement {
   UIScrollBar* hScroll;
   UIScrollBar* vScroll;

   int                   w, h;
   int                   itemCharacters;
   unique_ptr<storage_t> storage;
   grid_type_t           grid_type;

   ViewWindowMatrixGrid(UIElement* parent, int w, int h, char type)
      : UIElement(parent, UIElement::h_fill | UIElement::v_fill, ViewWindowMatrixGridMessage, "Matrix grid")
      , w(w)
      , h(h) {
      hScroll = &add_scrollbar(UIScrollBar::HORIZONTAL);
      vScroll = &add_scrollbar(0);
      hScroll->set_maximum(w * parent->ui()->string_width("A") * itemCharacters);
      vScroll->set_maximum(h * parent->ui()->string_height());

      if (type == 'c') {
         grid_type      = grid_type_t::char_t;
         itemCharacters = 1;
         storage        = make_unique<storage_impl_t<char>>(w, h);
      } else if (type == 'f') {
         grid_type      = grid_type_t::float_t;
         itemCharacters = 14;
         storage        = make_unique<storage_impl_t<float>>(w, h);
      } else {
         grid_type      = grid_type_t::double_t;
         itemCharacters = 14;
         storage        = make_unique<storage_impl_t<double>>(w, h);
      }
   }

   void read(FILE* f) { storage->read(f); }

   const char* data() const { return storage->data(); }
};

int ViewWindowStringMessage(UIElement* el, UIMessage msg, int di, void* dp);

struct ViewWindowString : public UIElement {
   UIScrollBar*       vScroll;
   unique_ptr<char[]> data;
   int                length;

   ViewWindowString(UIElement* parent, unique_ptr<char[]> data, int length)
      : UIElement(parent, UIElement::h_fill | UIElement::v_fill, ViewWindowStringMessage, "String display")
      , vScroll(&add_scrollbar(0))
      , data(std::move(data))
      , length(length) {}
};

int ViewWindowColorSwatchMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_HEIGHT) {
      return el->ui()->string_height();
   } else if (msg == UIMessage::PAINT) {
      const auto& thm     = el->theme();
      uint32_t    color   = ((ViewWindowColorSwatch*)el)->color;
      UIPainter*  painter = (UIPainter*)dp;
      const char* message = "Col: ";

      painter->draw_string(el->_bounds, message, thm.text, UIAlign::left, nullptr);
      UIRectangle swatch =
         UIRectangle(el->_bounds.l + el->ui()->string_width(message), 0, el->_bounds.t + 2, el->_bounds.b - 2);
      swatch.r = swatch.l + 50;
      painter->draw_rectangle(swatch, color, 0xFF000000, UIRectangle(1));
   }

   return 0;
}

double ViewWindowMatrixCalculateDeterminant(double* matrix, int n) {
   if (n == 1) {
      return matrix[0];
   }

   double s = 0;

   for (int i = 0; i < n; i++) {
      double* sub = (double*)malloc(sizeof(double) * (n - 1) * (n - 1));

      for (int j = 0; j < n - 1; j++) {
         for (int k = 0; k < n - 1; k++) {
            sub[j * (n - 1) + k] = matrix[(j + 1) * n + (k < i ? k : k + 1)];
         }
      }

      double x = ViewWindowMatrixCalculateDeterminant(sub, n - 1);
      x *= matrix[0 * n + i];
      if (i & 1)
         s -= x;
      else
         s += x;

      free(sub);
   }

   return s;
}

int ViewWindowMatrixGridMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   ViewWindowMatrixGrid* grid = (ViewWindowMatrixGrid*)el;

   if (msg == UIMessage::PAINT) {
      // TODO Optimise for really large arrays.
      // TODO Calculate eigenvectors/values.
      UI*         ui                 = el->ui();
      const auto& thm                = el->theme();
      auto [glyphWidth, glyphHeight] = ui->string_dims("A");
      UIPainter* painter             = (UIPainter*)dp;

      for (int i = 0; i < grid->h; i++) {
         for (int j = 0; j < grid->w; j++) {
            if (grid->grid_type == grid_type_t::char_t) {
               char c = grid->data()[i * grid->w + j];
               if (!c)
                  continue;
               painter->draw_glyph(el->_bounds.l + j * glyphWidth - grid->hScroll->position(),
                                   el->_bounds.t + i * glyphHeight - grid->vScroll->position(), c, thm.text);
            } else if (grid->grid_type == grid_type_t::float_t || grid->grid_type == grid_type_t::double_t) {
               double f = grid->grid_type == grid_type_t::double_t ? ((double*)grid->data())[i * grid->w + j]
                                                                   : (double)((float*)grid->data())[i * grid->w + j];
               char   buffer[64];
               std_format_to_n(buffer, sizeof(buffer), "{:f}", f);
               UIRectangle rectangle =
                  UIRectangle(j * glyphWidth * 14, (j + 1) * glyphWidth * 14, i * glyphHeight, (i + 1) * glyphHeight);
               UIRectangle offset = UIRectangle(el->_bounds.l - (int)grid->hScroll->position(),
                                                el->_bounds.t - (int)grid->vScroll->position());
               painter->draw_string(rectangle + offset, buffer, thm.text, UIAlign::right, nullptr);
            }
         }
      }

      int scrollBarSize = ui_size::scroll_bar * el->_window->scale();
      painter->draw_block(
         UIRectangle(el->_bounds.r - scrollBarSize, el->_bounds.r, el->_bounds.b - scrollBarSize, el->_bounds.b),
         thm.panel1);
   } else if (msg == UIMessage::LAYOUT) {
      UIRectangle scrollBarBounds = el->_bounds;
      scrollBarBounds.l           = scrollBarBounds.r - ui_size::scroll_bar * el->_window->scale();
      scrollBarBounds.b -= ui_size::scroll_bar * el->_window->scale();
      grid->vScroll->set_page(scrollBarBounds.height());
      grid->vScroll->move(scrollBarBounds, true);
      scrollBarBounds   = el->_bounds;
      scrollBarBounds.t = scrollBarBounds.b - ui_size::scroll_bar * el->_window->scale();
      scrollBarBounds.r -= ui_size::scroll_bar * el->_window->scale();
      grid->hScroll->set_page(scrollBarBounds.width());
      grid->hScroll->move(scrollBarBounds, true);
   } else if (msg == UIMessage::SCROLLED) {
      el->repaint(nullptr);
   }

   return 0;
}

int ViewWindowStringLayout(ViewWindowString* display, UIPainter* painter, int offset) {
   UIRectangle clientBounds = display->_bounds;
   clientBounds.r -= ui_size::scroll_bar * display->_window->scale();
   int x = clientBounds.l, y = clientBounds.t - offset;
   UI* ui = painter->ui();

   auto [glyphWidth, glyphHeight] = ui->string_dims("a");
   const auto& thm                = ui->theme();

   for (int i = 0; i < display->length; i++) {
      if (x + glyphWidth > clientBounds.r) {
         x = clientBounds.l + glyphWidth;
         y += glyphHeight;
         if (painter)
            painter->draw_glyph(clientBounds.l, y, '>', thm.codeComment);
      }

      if (display->data[i] < 0x20 || display->data[i] >= 0x7F) {
         if (display->data[i] == '\n') {
            if (painter)
               painter->draw_glyph(x, y, '\\', thm.codeComment);
            x += glyphWidth;
            if (painter)
               painter->draw_glyph(x, y, 'n', thm.codeComment);
            x = clientBounds.l;
            y += glyphHeight;
         } else if (display->data[i] == '\t') {
            if (painter)
               painter->draw_glyph(x, y, '\\', thm.codeNumber);
            x += glyphWidth;
            if (painter)
               painter->draw_glyph(x, y, 't', thm.codeNumber);
            x += glyphWidth;
         } else {
            const char* hexChars = "0123456789ABCDEF";
            if (painter)
               painter->draw_glyph(x, y, '<', thm.codeNumber);
            x += glyphWidth;
            if (painter)
               painter->draw_glyph(x, y, hexChars[(display->data[i] & 0xF0) >> 4], thm.codeNumber);
            x += glyphWidth;
            if (painter)
               painter->draw_glyph(x, y, hexChars[(display->data[i] & 0x0F) >> 0], thm.codeNumber);
            x += glyphWidth;
            if (painter)
               painter->draw_glyph(x, y, '>', thm.codeNumber);
            x += glyphWidth;
         }
      } else {
         if (painter)
            painter->draw_glyph(x, y, display->data[i], thm.codeDefault);
         x += glyphWidth;
      }
   }

   return y - clientBounds.t + glyphHeight;
}

int ViewWindowStringMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   ViewWindowString* display = (ViewWindowString*)el;

   if (msg == UIMessage::DESTROY) {
      display->data.reset();
   } else if (msg == UIMessage::LAYOUT) {
      UIRectangle scrollBarBounds = el->_bounds;
      scrollBarBounds.l           = scrollBarBounds.r - ui_size::scroll_bar * el->_window->scale();
      UIRectangle clientBounds    = el->_bounds;
      clientBounds.r -= ui_size::scroll_bar * el->_window->scale();
      display->vScroll->set_maximum(ViewWindowStringLayout(display, nullptr, 0));
      display->vScroll->set_page(el->_bounds.height());
      display->vScroll->move(scrollBarBounds, true);
   } else if (msg == UIMessage::PAINT) {
      const auto& thm = el->theme();
      static_cast<UIPainter*>(dp)->draw_block(el->_bounds, thm.codeBackground);
      ViewWindowStringLayout(display, (UIPainter*)dp, display->vScroll->position());
   } else if (msg == UIMessage::MOUSE_WHEEL) {
      return display->vScroll->message(msg, di, dp);
   } else if (msg == UIMessage::SCROLLED) {
      el->repaint(nullptr);
   }

   return 0;
}

void ViewWindowView(void* cp) {
   // Get the selected watch expression.
   UIElement* watchElement = ctx.switch_to_window_and_focus("Watch");
   if (!watchElement)
      return;

   WatchWindow* w     = (WatchWindow*)watchElement->_cp;
   auto         w_opt = w->get_selected_watch();
   if (!w_opt)
      return;

   const shared_ptr<Watch>& watch = *w_opt;

   if (!cp)
      cp = ctx.switch_to_window_and_focus("View");
   if (!cp)
      return;

   // Destroy the previous panel contents.
   UIElement* panel = (UIElement*)cp;
   panel->destroy_descendents();
   panel->add_button(0, "View (Ctrl+Shift+V)").on_click([panel](UIButton&) { ViewWindowView(panel); });

   // Get information about the watch expression.
   char type[256], buffer[256];
   char oldFormat  = watch->format();
   watch->format() = 0;

   auto res = watch->evaluate("gf_typeof");
   resize_to_lf(res);
   std_format_to_n(type, sizeof(type), "{}", res);
   std_format_to_n(buffer, sizeof(buffer), "Type: {}", type);
   panel->add_label(0, buffer);

   res = watch->evaluate("gf_valueof");
   resize_to_lf(res);
   watch->format() = oldFormat;
   // print("valueof: {}\n", ctx.evaluateResult);

   // Create the specific display for the given type.
   if (0 == strcmp(type, "uint8_t") || 0 == strcmp(type, "uint16_t") || 0 == strcmp(type, "uint32_t") ||
       0 == strcmp(type, "uint64_t") || 0 == strcmp(type, "int8_t") || 0 == strcmp(type, "int16_t") ||
       0 == strcmp(type, "int32_t") || 0 == strcmp(type, "int64_t") || 0 == strcmp(type, "unsigned char") ||
       0 == strcmp(type, "unsigned short") || 0 == strcmp(type, "unsigned int") || 0 == strcmp(type, "unsigned") ||
       0 == strcmp(type, "unsigned long") || 0 == strcmp(type, "unsigned long long") || 0 == strcmp(type, "char") ||
       0 == strcmp(type, "short") || 0 == strcmp(type, "int") || 0 == strcmp(type, "long") ||
       0 == strcmp(type, "long long")) {

      int64_t value = sv_atoll(res);

      std_format_to_n(buffer, sizeof(buffer), " 8b: {} {} 0x{:x} '{:c}'", (int8_t)value, (uint8_t)value, (uint8_t)value,
                      (char)value);
      panel->add_label(0, buffer);
      std_format_to_n(buffer, sizeof(buffer), "16b: {} {} 0x{:x}", (int16_t)value, (uint16_t)value, (uint16_t)value);
      panel->add_label(0, buffer);
      std_format_to_n(buffer, sizeof(buffer), "32b: {} {} 0x{:x}", (int32_t)value, (uint32_t)value, (uint32_t)value);
      panel->add_label(0, buffer);
      std_format_to_n(buffer, sizeof(buffer), "64b: %{} {} 0x{:x}", (int64_t)value, (uint64_t)value, (uint64_t)value);
      panel->add_label(0, buffer);

      int p = std_format_to_n(buffer, sizeof(buffer), "Bin: ");

      for (int64_t i = 63; i >= 32; i--) {
         buffer[p++] = (value & ((uint64_t)1 << i)) ? '1' : '0';
         if ((i & 7) == 0)
            buffer[p++] = ' ';
      }

      panel->add_label(0, {buffer, static_cast<size_t>(p)});

      p = std_format_to_n(buffer, sizeof(buffer), "     ");

      for (int64_t i = 31; i >= 0; i--) {
         buffer[p++] = (value & ((uint64_t)1 << i)) ? '1' : '0';
         if ((i & 7) == 0)
            buffer[p++] = ' ';
      }

      panel->add_label(0, {buffer, static_cast<size_t>(p)});

      if (value <= 0xFFFFFFFF) {
         new ViewWindowColorSwatch(panel, (uint32_t)value);
      }
   } else if ((0 == memcmp(type, "char [", 6) && !strstr(type, "][")) || 0 == strcmp(type, "const char *") ||
              0 == strcmp(type, "char *")) {
      print("string '{}'\n", res);
      char address[64];

      if ((res)[0] != '(') {
         res = watch->evaluate("gf_addressof");
         print("addressof '{}'\n", res);
         resize_to_lf(res, ' ');
         resize_to_lf(res);
         std_format_to_n(address, sizeof(address), "{}", res);
      } else {
         char* end = (char*)strchr(res.c_str() + 1, ' ');
         if (!end)
            goto unrecognised;
         *end = 0;
         std_format_to_n(address, sizeof(address), "{}", res.substr(1));
      }

      char tempPath[PATH_MAX];
      realpath(".temp.gf", tempPath);
      auto res = EvaluateExpression(std::format("(size_t)strlen((const char *)({})", address));
      print("'{}' -> '{}'\n", buffer, res);
      const char* lengthString = res.c_str() ? strstr(res.c_str(), "= ") : nullptr;
      size_t      length       = lengthString ? sv_atoi(lengthString, 2) : 0;
      // TODO Preventing errors when calling strlen from crashing the target?

      if (!length) {
         goto unrecognised;
      }

      unique_ptr<char[]> data = make_unique<char[]>(length + 1);

      if (!data) {
         goto unrecognised;
      }

      std_format_to_n(buffer, sizeof(buffer), "dump binary memory {} ({}) ({}+{})", tempPath, address, address, length);
      res = EvaluateCommand(buffer);
      print("'{}' -> '{}'\n", buffer, res);
      FILE* f = fopen(tempPath, "rb");

      if (f) {
         fread(data.get(), 1, length, f);
         fclose(f);
         unlink(tempPath);
         data[length] = 0;
         // print("got '{}'\n", data);
         new ViewWindowString(panel, std::move(data), length);
         std_format_to_n(buffer, sizeof(buffer), "{}+1 bytes", length);
         panel->add_label(UIElement::h_fill, buffer);
      } else {
         goto unrecognised;
      }
   } else if (0 == memcmp(type, "char [", 6) || 0 == memcmp(type, "float [", 7) || 0 == memcmp(type, "double [", 8)) {
      int   itemSize = type[0] == 'c' ? sizeof(char) : type[0] == 'f' ? sizeof(float) : sizeof(double);
      char* p        = strchr(type, '[') + 1;
      int   w        = strtol(p, &p, 0);
      if (memcmp(p, "][", 2))
         goto unrecognised;
      p += 2;
      int h = strtol(p, &p, 0);
      if (strcmp(p, "]"))
         goto unrecognised;
      if (w <= 1 || h <= 1)
         goto unrecognised;
      auto res = watch->get_address();
      if (res.empty())
         goto unrecognised;

      ViewWindowMatrixGrid* grid = new ViewWindowMatrixGrid(panel, w, h, type[0]);

      char tempPath[PATH_MAX];
      realpath(".temp.gf", tempPath);
      char buffer[PATH_MAX * 2];
      std_format_to_n(buffer, sizeof(buffer), "dump binary memory {} ({}) ({}+{})", tempPath, res, res,
                      w * h * itemSize);
      res     = EvaluateCommand(buffer);
      FILE* f = fopen(tempPath, "rb");

      if (f) {
         grid->read(f);
         fclose(f);
         unlink(tempPath);
      }

      if ((grid->grid_type == grid_type_t::float_t || grid->grid_type == grid_type_t::double_t) && w == h && w <= 4 &&
          w >= 2) {
         double matrix[16];

         for (int i = 0; i < w; i++) {
            for (int j = 0; j < w; j++) {
               if (grid->grid_type == grid_type_t::float_t) {
                  matrix[i * w + j] = ((float*)grid->data())[i * w + j];
               } else {
                  matrix[i * w + j] = ((double*)grid->data())[i * w + j];
               }
            }
         }

         double determinant = ViewWindowMatrixCalculateDeterminant(matrix, w);
         std_format_to_n(buffer, sizeof(buffer), "Determinant: {:f}", determinant);
         panel->add_label(0, buffer);
      }
   } else {
   unrecognised:;
      // TODO Custom view.
      // TODO Table view for array of structures.
      panel->add_label(0, "No view available for type.");
   }

   // Relayout the panel.
   panel->refresh();
}

void ViewWindowView() { ViewWindowView(nullptr); }

void ViewWindowUpdate(const char* data, UIElement* el) {}

UIElement* ViewWindowCreate(UIElement* parent) {
   UIPanel* panel = &parent->add_panel(UIPanel::EXPAND | UIPanel::COLOR_1);
   panel->add_button(0, "View (Ctrl+Shift+V)").on_click([panel](UIButton&) { ViewWindowView(panel); });
   panel->add_label(0, "Select a watch expression, then click View.");
   return panel;
}

// ----------------------------------------------------------
// Waveform display:
// ----------------------------------------------------------

int WaveformDisplayMessage(UIElement* el, UIMessage msg, int di, void* dp);
int WaveformDisplayZoomButtonMessage(UIElement* el, UIMessage msg, int di, void* dp);
int WaveformDisplayNormalizeButtonMessage(UIElement* el, UIMessage msg, int di, void* dp);

struct WaveformDisplay : public UIElement {
   float*       samples;
   size_t       sampleCount, channels;
   int          samplesOnScreen, minimumZoom;
   UIScrollBar* scrollBar;
   UIButton *   zoomOut, *zoomIn, *normalize;
   int          dragLastX, dragLastModification;
   float        peak;

   WaveformDisplay(UIElement* parent, uint32_t flags)
      : UIElement(parent, flags, WaveformDisplayMessage, "WaveformDisplay")
      , samples(nullptr)
      , sampleCount(0)
      , channels(0)
      , samplesOnScreen(0)
      , minimumZoom(0)
      , scrollBar(new UIScrollBar(this, UIElement::non_client_flag | UIScrollBar::HORIZONTAL))
      , zoomOut(new UIButton(this, UIButton::SMALL, "-"))
      , zoomIn(new UIButton(this, UIButton::SMALL, "+"))
      , normalize(new UIButton(this, UIButton::SMALL, "Norm"))
      , dragLastX(0)
      , dragLastModification(0)
      , peak(0) {
      zoomOut->set_user_proc(WaveformDisplayZoomButtonMessage);
      zoomIn->set_user_proc(WaveformDisplayZoomButtonMessage);
      normalize->set_user_proc(WaveformDisplayNormalizeButtonMessage);
   }
};

void WaveformDisplayDrawVerticalLineWithTranslucency(UIPainter* painter, UIRectangle rectangle, uint32_t color,
                                                     uint32_t alpha) {
   rectangle = intersection(painter->_clip, rectangle);
   if (!rectangle.valid())
      return;
   uint32_t* bits = painter->_bits + rectangle.t * painter->_width + rectangle.l;

   for (int y = 0; y < rectangle.b - rectangle.t; y++) {
      uint32_t* destination = &bits[y * painter->_width];
      uint32_t  m1 = alpha, m2 = 255 - m1;
      uint32_t  original = *destination;
      uint32_t  r2       = m2 * (original & 0x00FF00FF);
      uint32_t  g2       = m2 * (original & 0x0000FF00);
      uint32_t  r1       = m1 * (color & 0x00FF00FF);
      uint32_t  g1       = m1 * (color & 0x0000FF00);
      uint32_t  result   = 0xFF000000 | (0x0000FF00 & ((g1 + g2) >> 8)) | (0x00FF00FF & ((r1 + r2) >> 8));
      *destination       = result;
   }
}

int WaveformDisplayMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   WaveformDisplay* display = (WaveformDisplay*)el;

   if (display->sampleCount == 0 && msg != UIMessage::DESTROY) {
      return 0;
   }

   if (msg == UIMessage::DESTROY) {
      free(display->samples);
      display->samples = nullptr;
   } else if (msg == UIMessage::LAYOUT) {
      if (display->samplesOnScreen > (int)display->sampleCount) {
         display->samplesOnScreen = display->sampleCount;
      }

      int         scrollBarHeight = ui_size::scroll_bar * el->_window->scale();
      UIRectangle scrollBarBounds = el->_bounds;
      scrollBarBounds.t           = scrollBarBounds.b - scrollBarHeight;
      display->scrollBar->set_maximum(display->sampleCount);
      display->scrollBar->set_page(display->samplesOnScreen);
      display->scrollBar->move(scrollBarBounds, true);

      display->zoomOut->move(UIRectangle(el->_bounds.l + (int)(15 * el->_window->scale()),
                                         el->_bounds.l + (int)(45 * el->_window->scale()),
                                         el->_bounds.t + (int)(15 * el->_window->scale()),
                                         el->_bounds.t + (int)(45 * el->_window->scale())),
                             true);
      display->zoomIn->move(UIRectangle(el->_bounds.l + (int)(45 * el->_window->scale()),
                                        el->_bounds.l + (int)(75 * el->_window->scale()),
                                        el->_bounds.t + (int)(15 * el->_window->scale()),
                                        el->_bounds.t + (int)(45 * el->_window->scale())),
                            true);
      display->normalize->move(UIRectangle(el->_bounds.l + (int)(75 * el->_window->scale()),
                                           el->_bounds.l + (int)(135 * el->_window->scale()),
                                           el->_bounds.t + (int)(15 * el->_window->scale()),
                                           el->_bounds.t + (int)(45 * el->_window->scale())),
                               true);
   } else if (msg == UIMessage::MOUSE_DRAG && el->_window->pressed_button() == 1) {
      auto pos = el->cursor_pos();
      display->scrollBar->position() += display->dragLastModification;
      display->dragLastModification = (pos.x - display->dragLastX) * display->samplesOnScreen / el->_bounds.width();
      display->scrollBar->position() -= display->dragLastModification;
      display->refresh();
   } else if (msg == UIMessage::MOUSE_DRAG && el->_window->pressed_button() == 2) {
      display->repaint(nullptr);
   } else if (msg == UIMessage::MOUSE_MOVE) {
      display->repaint(nullptr);
   } else if (msg == UIMessage::MIDDLE_UP) {
      auto pos = el->cursor_pos();
      int  l = pos.x - el->_bounds.l, r = display->dragLastX - el->_bounds.l;
      if (r < l) {
         int t = l;
         l     = r;
         r     = t;
      }
      float lf = (float)l / el->_bounds.width() * display->samplesOnScreen + display->scrollBar->position();
      float rf = (float)r / el->_bounds.width() * display->samplesOnScreen + display->scrollBar->position();

      if (rf - lf >= display->minimumZoom) {
         display->scrollBar->position() = lf;
         display->samplesOnScreen       = rf - lf;
      }

      display->refresh();
   } else if (msg == UIMessage::LEFT_DOWN || msg == UIMessage::MIDDLE_DOWN) {
      auto pos                      = el->cursor_pos();
      display->dragLastX            = pos.x;
      display->dragLastModification = 0;
   } else if (msg == UIMessage::MOUSE_WHEEL) {
      auto   pos         = el->cursor_pos();
      int    divisions   = di / 72;
      double factor      = 1;
      double perDivision = 1.2f;
      while (divisions > 0)
         factor *= perDivision, divisions--;
      while (divisions < 0)
         factor /= perDivision, divisions++;
      double mouse   = (double)(pos.x - el->_bounds.l) / el->_bounds.width();
      double newZoom = (double)display->samplesOnScreen / display->sampleCount * factor;

      if (newZoom * display->sampleCount >= display->minimumZoom) {
         display->scrollBar->position() += mouse * display->samplesOnScreen * (1 - factor);
         display->samplesOnScreen = newZoom * display->sampleCount;
      }

      display->refresh();
   } else if (msg == UIMessage::SCROLLED) {
      el->repaint(nullptr);
   } else if (msg == UIMessage::PAINT) {
      UIRectangle client = el->_bounds;
      const auto& thm    = el->theme();
      client.b -= display->scrollBar->_bounds.height();

      UIPainter*  painter = (UIPainter*)dp;
      UIRectangle oldClip = painter->_clip;
      painter->_clip      = intersection(client, painter->_clip);
      int ym              = (client.t + client.b) / 2;
      int h2              = (client.b - client.t) / 2;
      int yp              = ym;
      painter->draw_block(painter->_clip, thm.panel1);
      painter->draw_block(UIRectangle(client.l, client.r, ym, ym + 1), 0x707070);

      float yScale =
         (display->normalize->_flags & UIButton::CHECKED) && display->peak > 0.00001f ? 1.0f / display->peak : 1.0f;

      int    sampleOffset = (int)display->scrollBar->position();
      float* samples      = &display->samples[display->channels * sampleOffset];
      int    sampleCount  = display->samplesOnScreen;
      UI_ASSERT(sampleOffset + sampleCount <= (int)display->sampleCount);

      auto pos = el->cursor_pos();

      if (sampleCount > client.width()) {
         uint32_t alpha = 255 - 80 * (display->channels - 1);

         for (size_t channel = 0; channel < display->channels; channel++) {
            for (int32_t x = painter->_clip.l; x < painter->_clip.r; x++) {
               int32_t x0  = x - client.l;
               float   xf0 = (float)x0 / (client.r - client.l);
               float   xf1 = (float)(x0 + 1) / (client.r - client.l);
               float   yf = 0.0f, yt = 0.0f;
               int     i0 = xf0 * sampleCount;
               int     i1 = xf1 * sampleCount;
               int     is = 1 + (i1 - i0) / 1000; // Skip samples if we're zoomed really far out.

               for (int k = i0; k < i1; k += is) {
                  float t = samples[channel + display->channels * (int)k];
                  if (t < 0 && yt < -t)
                     yt = -t;
                  else if (t > 0 && yf < t)
                     yf = t;
               }

               UIRectangle r = UIRectangle(x, x + 1, ym - (int)(yt * h2 * yScale), ym + (int)(yf * h2 * yScale));
               WaveformDisplayDrawVerticalLineWithTranslucency(painter, r, thm.text, alpha);
            }
         }
      } else {
         for (size_t channel = 0; channel < display->channels; channel++) {
            yp = ym + h2 * yScale * samples[channel + 0];

            for (int32_t i = 0; i < sampleCount; i++) {
               int32_t x0 = (int)((float)i / sampleCount * client.width()) + client.l;
               int32_t x1 = (int)((float)(i + 1) / sampleCount * client.width()) + client.l;
               int32_t y  = ym + h2 * yScale * samples[channel + display->channels * (int)i];
               painter->draw_line(x0, yp, x1, y, thm.text);
               yp = y;
            }
         }

         if (sampleCount < client.width() / 4) {
            for (size_t channel = 0; channel < display->channels; channel++) {
               for (int32_t i = 0; i < sampleCount; i++) {
                  int32_t x1 = (int)((float)(i + 1) / sampleCount * client.width()) + client.l;
                  int32_t y  = ym + h2 * yScale * samples[channel + display->channels * (int)i];
                  painter->draw_block(UIRectangle(x1 - 2, x1 + 2, y - 2, y + 2), channel % 2 ? 0xFFFF00FF : 0xFF00FFFF);
               }
            }
         }

         int mouseXSample = (float)(pos.x - client.l) / el->_bounds.width() * display->samplesOnScreen - 0.5f;

         if (mouseXSample >= 0 && mouseXSample < sampleCount && el->_clip.contains(pos) &&
             !display->scrollBar->_clip.contains(pos)) {
            int         stringOffset = 20 * el->_window->scale();
            UIRectangle stringRectangle =
               UIRectangle(client.l + stringOffset, client.r - stringOffset, client.t + stringOffset,
                           client.t + stringOffset + el->ui()->string_height());
            char buffer[100];
            std_format_to_n(buffer, sizeof(buffer), "{}: ", (int)(mouseXSample + display->scrollBar->position()));

            for (size_t channel = 0; channel < display->channels; channel++) {
               char  buffer2[30];
               float sample = samples[channel + display->channels * mouseXSample];
               std_format_to_n(buffer2, sizeof(buffer2), "{}{}{:.5f}", channel ? ", " : "", sample >= 0.0f ? "+" : "",
                               sample);
               if (strlen(buffer) + strlen(buffer2) > sizeof(buffer) - 1)
                  break;
               strcat(buffer, buffer2);
            }

            painter->draw_string(stringRectangle, buffer, thm.text, UIAlign::right, nullptr);

            int32_t x1 = (int)((float)(mouseXSample + 1) / sampleCount * client.width()) + client.l;
            WaveformDisplayDrawVerticalLineWithTranslucency(painter, UIRectangle(x1, x1 + 1, client.t, client.b),
                                                            0xFFFFFF, 100);
         }
      }

      if (el->_window->pressed_button() == 2 && el->_window->pressed()) {
         int l = pos.x, r = display->dragLastX;
         painter->draw_invert(UIRectangle(l > r ? r : l, l > r ? l : r, el->_bounds.t, el->_bounds.r));
      }

      painter->_clip = oldClip;
   }

   return 0;
}

int WaveformDisplayZoomButtonMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   WaveformDisplay* display = (WaveformDisplay*)el->_parent;

   if (msg == UIMessage::CLICKED) {
      if (el == display->zoomOut) {
         display->scrollBar->position() -= display->samplesOnScreen / 2;
         display->samplesOnScreen *= 2;
      } else if (el == display->zoomIn && display->samplesOnScreen >= display->minimumZoom) {
         display->samplesOnScreen /= 2;
         display->scrollBar->position() += display->samplesOnScreen / 2;
      }

      display->refresh();
   }

   return 0;
}

int WaveformDisplayNormalizeButtonMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   WaveformDisplay* display = (WaveformDisplay*)el->_parent;

   if (msg == UIMessage::CLICKED) {
      el->_flags ^= UIButton::CHECKED;
      display->refresh();
   }

   return 0;
}

WaveformDisplay* WaveformDisplayCreate(UIElement* parent, uint32_t flags) { return new WaveformDisplay(parent, flags); }

void WaveformDisplaySetContent(WaveformDisplay* display, float* samples, size_t sampleCount, size_t channels) {
   UI_ASSERT(channels >= 1);
   free(display->samples);
   display->samples = (float*)malloc(sizeof(float) * sampleCount * channels);
   memcpy(display->samples, samples, sizeof(float) * sampleCount * channels);
   display->sampleCount     = sampleCount;
   display->channels        = channels;
   display->samplesOnScreen = sampleCount;
   display->minimumZoom     = 40 > sampleCount ? sampleCount : 40;

   float peak = 0.0f;

   for (uintptr_t i = 0; i < sampleCount * channels; i++) {
      if (samples[i] > peak) {
         peak = samples[i];
      } else if (-samples[i] > peak) {
         peak = -samples[i];
      }
   }

   display->peak = peak;
}

// ----------------------------------------------------------
// Waveform viewer:
// ----------------------------------------------------------

struct WaveformViewer {
   char             pointer[256];
   char             sampleCount[256];
   char             channels[256];
   int              parsedSampleCount = 0;
   int              parsedChannels    = 0;
   UIButton*        autoToggle        = nullptr;
   UIPanel*         labelPanel        = nullptr;
   UILabel*         label             = nullptr;
   WaveformDisplay* display           = nullptr;
};

void WaveformViewerUpdate(const char* pointerString, const char* sampleCountString, const char* channelsString,
                          UIElement* owner);

const char* WaveformViewerGetSamples(const char* pointerString, const char* sampleCountString,
                                     const char* channelsString, float** _samples, int* _sampleCount, int* _channels) {
   auto sampleCountResult = EvaluateExpression(sampleCountString);
   if (sampleCountResult.empty()) {
      return "Could not evaluate sample count.";
   }
   int  sampleCount    = sv_atoi(sampleCountResult, 1);
   auto channelsResult = EvaluateExpression(channelsString);
   if (channelsResult.empty()) {
      return "Could not evaluate channels.";
   }
   int channels = sv_atoi(channelsResult, 1 + 1);
   if (channels < 1 || channels > 8) {
      return "Channels must be between 1 and 8.";
   }
   auto pointerResult = EvaluateExpression(pointerString, "/x");
   if (pointerResult.empty()) {
      return "Could not evaluate pointer.";
   }
   char _pointerResult[1024];
   std_format_to_n(_pointerResult, sizeof(_pointerResult), "{}", pointerResult);
   pointerResult = strstr(_pointerResult, " 0x");
   if (pointerResult.empty()) {
      return "Pointer to sample data does not look like an address!";
   }

   size_t byteCount = sampleCount * channels * 4;
   float* samples   = (float*)malloc(byteCount);

   char transferPath[PATH_MAX];
   realpath(".transfer.gf", transferPath);

   char buffer[PATH_MAX * 2];
   std_format_to_n(buffer, sizeof(buffer), "dump binary memory {} ({}) ({}+{})", transferPath,
                   pointerResult.c_str() + 1, pointerResult.c_str() + 1, byteCount);
   auto res = EvaluateCommand(buffer);

   FILE* f = fopen(transferPath, "rb");

   if (f) {
      fread(samples, 1, byteCount, f);
      fclose(f);
      unlink(transferPath);
   }

   if (!f || res.contains("access")) {
      return "Could not read the waveform samples!";
   }

   *_samples = samples, *_sampleCount = sampleCount, *_channels = channels;
   return nullptr;
}

int WaveformViewerWindowMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::DESTROY) {
      DataViewerRemoveFromAutoUpdateList(el);
      delete (WaveformViewer*)el->_cp;
   } else if (msg == UIMessage::GET_WIDTH) {
      return 300;
   } else if (msg == UIMessage::GET_HEIGHT) {
      return 300;
   }

   return 0;
}

void WaveformViewerAutoUpdateCallback(UIElement* el) {
   WaveformViewer* viewer = (WaveformViewer*)el->_cp;
   WaveformViewerUpdate(viewer->pointer, viewer->sampleCount, viewer->channels, el);
}

int WaveformViewerRefreshMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::CLICKED) {
      WaveformViewerAutoUpdateCallback(el->_parent);
   }

   return 0;
}

void WaveformViewerSaveToFile(WaveformDisplay* display) {
   static char* path   = nullptr;
   auto         result = s_main_window->show_dialog(0, "Save to file       \nPath:\n%t\n%f%b%b%b", &path, "Save",
                                                    "Save and open", "Cancel");
   if (result == "Cancel")
      return;
   FILE* f = fopen(path, "wb");
   if (!f) {
      s_main_window->show_dialog(0, "Unable to open file for writing.\n%f%b", "OK");
      return;
   }
   int32_t i;
   int16_t s;
   fwrite("RIFF", 1, 4, f);
   i = 36 + display->channels * 4 * display->sampleCount;
   fwrite(&i, 1, 4, f); // Total file size, minus 8.
   fwrite("WAVE", 1, 4, f);
   fwrite("fmt ", 1, 4, f);
   i = 16;
   fwrite(&i, sizeof(i), 1, f); // Format chunk size.
   s = 3;
   fwrite(&s, sizeof(s), 1, f); // Format tag (float).
   s = display->channels;
   fwrite(&s, sizeof(s), 1, f); // Channels.
   i = 48000;
   fwrite(&i, sizeof(i), 1, f); // Sample rate (guessed).
   i = 48000 * display->channels * sizeof(float);
   fwrite(&i, sizeof(i), 1, f); // Average bytes per second.
   s = display->channels * sizeof(float);
   fwrite(&s, sizeof(s), 1, f); // Block align.
   s = sizeof(float) * 8;
   fwrite(&s, sizeof(s), 1, f); // Bits per sample.
   fwrite("data", 1, 4, f);
   i = display->channels * sizeof(float) * display->sampleCount;
   fwrite(&i, sizeof(i), 1, f);                                              // Bytes of sample data.
   fwrite(display->samples, display->channels * 4, display->sampleCount, f); // Sample data.
   fclose(f);

   if (result == "Save and open") {
      char buffer[4000];
      std_format_to_n(buffer, sizeof(buffer), "xdg-open \"{}\"", path);
      system(buffer);
   }
}

int WaveformViewerDisplayMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::RIGHT_UP) {
      WaveformDisplay* display = (WaveformDisplay*)el;
      el->ui()
         ->create_menu(el->_window, UIMenu::NO_SCROLL)
         .add_item(0, "Save to .wav...", [display](UIButton&) { WaveformViewerSaveToFile(display); })
         .show();
   }

   return 0;
}

void WaveformViewerUpdate(const char* pointerString, const char* sampleCountString, const char* channelsString,
                          UIElement* owner) {
   float*      samples     = nullptr;
   int         sampleCount = 0, channels = 0;
   const char* error =
      WaveformViewerGetSamples(pointerString, sampleCountString, channelsString, &samples, &sampleCount, &channels);

   if (!owner) {
      WaveformViewer* viewer = new WaveformViewer;
      if (pointerString)
         std_format_to_n(viewer->pointer, sizeof(viewer->pointer), "{}", pointerString);
      if (sampleCountString)
         std_format_to_n(viewer->sampleCount, sizeof(viewer->sampleCount), "{}", sampleCountString);
      if (channelsString)
         std_format_to_n(viewer->channels, sizeof(viewer->channels), "{}", channelsString);

      UIMDIChild* window = &s_data_window->add_mdichild(UIMDIChild::CLOSE_BUTTON, UIRectangle(0), "Waveform")
                               .set_user_proc(WaveformViewerWindowMessage)
                               .set_cp(viewer);
      viewer->autoToggle = &window->add_button(UIButton::SMALL | UIElement::non_client_flag, "Auto")
                               .set_cp((void*)WaveformViewerAutoUpdateCallback)
                               .set_user_proc(DataViewerAutoUpdateButtonMessage);
      window->add_button(UIButton::SMALL | UIElement::non_client_flag, "Refresh")
         .set_user_proc(WaveformViewerRefreshMessage);
      owner = window;

      UIPanel* panel     = &owner->add_panel(UIPanel::EXPAND);
      viewer->labelPanel = &panel->add_panel(UIPanel::COLOR_1 | UIElement::v_fill);
      viewer->label      = &viewer->labelPanel->add_label(UIElement::h_fill, {});
      viewer->display    = WaveformDisplayCreate(panel, UIElement::v_fill);
      viewer->display->set_user_proc(WaveformViewerDisplayMessage);
   }

   WaveformViewer* viewer    = (WaveformViewer*)owner->_cp;
   viewer->parsedSampleCount = sampleCount, viewer->parsedChannels = channels;

   if (error) {
      viewer->label->set_label(error);
      viewer->labelPanel->_flags &= ~UIElement::hide_flag;
      viewer->display->_flags |= UIElement::hide_flag;
   } else {
      viewer->labelPanel->_flags |= UIElement::hide_flag;
      viewer->display->_flags &= ~UIElement::hide_flag;
      WaveformDisplaySetContent(viewer->display, samples, sampleCount, channels);
   }

   viewer->display->refresh();
   viewer->label->refresh();
   viewer->labelPanel->_parent->refresh();
   owner->refresh();
   s_data_window->refresh();

   free(samples);
}

void WaveformAddDialog() {
   static char *pointer = nullptr, *sampleCount = nullptr, *channels = nullptr;

   auto result = s_main_window->show_dialog(
      0,
      "Add waveform\n\n%l\n\nPointer to samples: (float *)\n%t\nSample count (per channel):\n%t\n"
      "Channels (interleaved):\n%t\n\n%l\n\n%f%b%b",
      &pointer, &sampleCount, &channels, "Add", "Cancel");

   if (result == "Add") {
      WaveformViewerUpdate(pointer, sampleCount, channels, nullptr);
   }
}

// ----------------------------------------------------------
// Registration:
// ----------------------------------------------------------

void Context::register_extensions() {
   _interface_windows["Prof"]   = {._create = ProfWindowCreate, ._update = ProfWindowUpdate, ._always_update = true};
   _interface_windows["Memory"] = {MemoryWindowCreate, MemoryWindowUpdate};
   _interface_windows["View"]   = {ViewWindowCreate, ViewWindowUpdate};

   _interface_data_viewers.push_back({"Add waveform...", WaveformAddDialog});
   _interface_commands.push_back({
      ._label = nullptr, ._shortcut{.code = UI_KEYCODE_LETTER('V'), .ctrl = true, .shift = true, .invoke = []() {
                                       ViewWindowView();
                                       return true;
                                    }}
   });
}

#if __has_include("plugins.cpp")
   #include "plugins.cpp"
#endif

// ------------------------------------------------------
// Interface and main:
// ------------------------------------------------------

bool ElementHidden(UIElement* el) {
   while (el) {
      if (el->_flags & UIElement::hide_flag) {
         return true;
      } else {
         el = el->_parent;
      }
   }

   return false;
}

void MsgReceivedData(std::unique_ptr<std::string> input) {
   ctx._program_running = false;

   if (ctx._first_update) {
      (void)EvaluateCommand(pythonCode);

      char path[PATH_MAX];
      std_format_to_n(path, sizeof(path), "{}/.config/gf2_watch.txt", getenv("HOME"));
      std::string s    = LoadFile(path);
      const char* data = s.c_str();

      while (data && gfc._restore_watch_window) {
         const char* end = strchr(data, '\n');
         if (!end)
            break;
         WatchAddExpression(string_view{data, static_cast<size_t>(end - data)});
         data = end + 1;
      }

      ctx._first_update = false;
   }

   if (WatchLoggerUpdate(*input))
      return;
   if (s_source_window->_showing_disassembly)
      s_source_window->disassembly_update_line();

   if (!ctx._dbg_re->match_stack_or_breakpoint_output(*input)) {
      // we don't want to call `DebuggerGetBreakpoints()` upon receiving the result of `DebuggerGetBreakpoints()`,
      // causing an infinite loop!!!
      DebuggerGetStack();
      s_breakpoint_mgr.update_breakpoint_from_gdb();

      ctx.grab_focus(s_input_textbox->_window); // grab focus when breakpoint is hit!
   }

   for (auto& [name, iw] : ctx._interface_windows) {
      InterfaceWindow* window = &iw;
      if (!window->_update || !window->_el)
         continue;
      if (!window->_always_update && ElementHidden(window->_el))
         window->_queued_update = true;
      else
         window->_update(input->c_str(), window->_el);
   }

   DataViewersUpdateAll();

   if (s_display_output) {
      s_display_output->insert_content(*input, false);
      s_display_output->refresh();
   }

   if (s_trafficlight)
      s_trafficlight->repaint(nullptr);
}

void MsgReceivedControl(std::unique_ptr<std::string> input) {
   char* start = &(*input)[0];
   char* end   = strchr(start, '\n');
   if (end)
      *end = 0;

   if (start[0] == 'f' && start[1] == ' ') {
      DisplaySetPosition(start + 2, 0, false);
   } else if (start[0] == 'l' && start[1] == ' ') {
      DisplaySetPosition(nullptr, sv_atoul(start + 2) - 1, false);
   } else if (start[0] == 'c' && start[1] == ' ') {
      (void)CommandParseInternal(start + 2, false);
   }
}

enum invoker_flags { invoker_restore_focus = 1 << 0 };

auto gdb_invoker(string_view cmd, int flags = 0) {
   return [cmd, flags]() {
      CommandSendToGDB(cmd);
      if (flags & invoker_restore_focus)
         ctx.restore_focus(); // restore input focus to the window that had it before `gf` grabbed it
      return true;
   };
}

void Context::add_builtin_windows_and_commands() {
   _interface_windows["Stack"]       = {StackWindow::Create, StackWindow::Update};
   _interface_windows["Source"]      = {SourceWindow::Create, SourceWindow::Update};
   _interface_windows["Breakpoints"] = {BreakpointsWindow::Create, BreakpointsWindow::Update};
   _interface_windows["Registers"]   = {RegistersWindow::Create, RegistersWindow::Update};
   _interface_windows["Watch"]       = {WatchWindow::Create, WatchWindow::Update, WatchWindow::Focus};
   _interface_windows["Locals"]      = {WatchWindow::CreateLocalsWindow, WatchWindow::Update, WatchWindow::Focus};
   _interface_windows["Commands"]    = {CommandsWindow::Create, nullptr};
   _interface_windows["Data"]        = {DataWindow::Create, nullptr};
   _interface_windows["Struct"]      = {StructWindow::Create, nullptr};
   _interface_windows["Files"]       = {FilesWindow::Create, nullptr};
   _interface_windows["Console"]     = {ConsoleWindow::Create, nullptr};
   _interface_windows["Log"]         = {LogWindow::Create, nullptr};
   _interface_windows["Thread"]      = {ThreadsWindow::Create, ThreadsWindow::Update};
   _interface_windows["Exe"]         = {ExecutableWindow::Create, nullptr};
   _interface_windows["CmdSearch"]   = {CommandSearchWindow::Create, nullptr};

   _interface_data_viewers.push_back({"Add bitmap...", BitmapAddDialog});

   _interface_commands.push_back({
      ._label = "Run\tShift+F5", ._shortcut{.code = UI_KEYCODE_FKEY(5), .shift = true, .invoke = gdb_invoker("r")}
   });
   _interface_commands.push_back({
      ._label = "Run paused\tCtrl+F5",
      ._shortcut{.code = UI_KEYCODE_FKEY(5), .ctrl = true, .invoke = gdb_invoker("start")}
   });
   _interface_commands.push_back({
      ._label = "Kill\tF3", ._shortcut{.code = UI_KEYCODE_FKEY(3), .invoke = gdb_invoker("kill")}
   });
   _interface_commands.push_back({
      ._label = "Restart GDB\tCtrl+R",
      ._shortcut{.code = UI_KEYCODE_LETTER('R'), .ctrl = true, .invoke = gdb_invoker("gf-restart-gdb")}
   });
   _interface_commands.push_back({
      ._label = "Connect\tF4", ._shortcut{.code = UI_KEYCODE_FKEY(4), .invoke = gdb_invoker("target remote :1234")}
   });
   _interface_commands.push_back({
      ._label = "Continue\tF5",
      ._shortcut{.code = UI_KEYCODE_FKEY(5), .invoke = gdb_invoker("c", invoker_restore_focus)}
   });
   _interface_commands.push_back({
      ._label = "Step over\tF10", ._shortcut{.code = UI_KEYCODE_FKEY(10), .invoke = gdb_invoker("gf-next")}
   });
   _interface_commands.push_back({
      ._label = "Step out of block\tShift+F10",
      ._shortcut{.code = UI_KEYCODE_FKEY(10), .shift = true, .invoke = gdb_invoker("gf-step-out-of-block")}
   });
   _interface_commands.push_back({
      ._label = "Step in\tF11", ._shortcut{.code = UI_KEYCODE_FKEY(11), .invoke = gdb_invoker("gf-step")}
   });
   _interface_commands.push_back({
      ._label = "Step into outer\tShift+F8",
      ._shortcut{.code = UI_KEYCODE_FKEY(8), .shift = true, .invoke = gdb_invoker("gf-step-into-outer")}
   });
   _interface_commands.push_back({
      ._label = "Step out\tShift+F11",
      ._shortcut{.code = UI_KEYCODE_FKEY(11), .shift = true, .invoke = gdb_invoker("finish")}
   });
   _interface_commands.push_back({
      ._label = "Reverse continue\tCtrl+Shift+F5",
      ._shortcut{.code = UI_KEYCODE_FKEY(5), .ctrl = true, .shift = true, .invoke = gdb_invoker("reverse-continue")}
   });
   _interface_commands.push_back({
      ._label = "Reverse step over\tCtrl+Shift+F10",
      ._shortcut{.code = UI_KEYCODE_FKEY(10), .ctrl = true, .shift = true, .invoke = gdb_invoker("reverse-next")}
   });
   _interface_commands.push_back({
      ._label = "Reverse step in\tCtrl+Shift+F11",
      ._shortcut{.code = UI_KEYCODE_FKEY(11), .ctrl = true, .shift = true, .invoke = gdb_invoker("reverse-step")}
   });
   _interface_commands.push_back({
      ._label = "Break\tF8", ._shortcut{.code = UI_KEYCODE_FKEY(8), .invoke = [&]() {
                                           ctx.interrupt_gdb(0);
                                           return true;
                                        }}
   });
   _interface_commands.push_back({
      ._label = "Toggle breakpoint\tF9", ._shortcut{.code = UI_KEYCODE_FKEY(9), .invoke = []() {
                                                       s_breakpoint_mgr.toggle_breakpoint();
                                                       return true;
                                                    }}
   });
   _interface_commands.push_back({
      ._label = "Sync with gvim\tF2", ._shortcut{.code = UI_KEYCODE_FKEY(2), .invoke = CommandSyncWithGvim}
   });
   _interface_commands.push_back({
      ._label = "Ask GDB for PWD\tCtrl+Shift+P",
      ._shortcut{.code = UI_KEYCODE_LETTER('P'), .ctrl = true, .shift = true, .invoke = gdb_invoker("gf-get-pwd")}
   });
   _interface_commands.push_back({
      ._label = "Set disassembly mode\tCtrl+M",
      ._shortcut{.code = UI_KEYCODE_LETTER('M'), .ctrl = true, .invoke = CommandSetDisassemblyMode}
   });
   _interface_commands.push_back({
      ._label = "Inspect line", ._shortcut{.code = UIKeycode::BACKTICK, .invoke = CommandInspectLine}
   });
   _interface_commands.push_back({
      ._label = nullptr, ._shortcut{.code = UI_KEYCODE_LETTER('G'), .ctrl = true, .invoke = []() {
                                       CommandWatchViewSourceAtAddress();
                                       return true;
                                    }}
   });
#if 0
   _interface_commands.push_back({
      ._label = nullptr,
      ._shortcut{.code = UI_KEYCODE_LETTER('P'), .ctrl = true, .shift = false, .invoke = CommandPreviousCommand}
   });
   _interface_commands.push_back({
      ._label = nullptr,
      ._shortcut{.code = UI_KEYCODE_LETTER('N'), .ctrl = true, .shift = false, .invoke = CommandNextCommand}
   });
#endif
   _interface_commands.push_back(
      {._label = "Copy Layout to Clipboard", ._shortcut{.invoke = [&]() { return ctx.copy_layout_to_clipboard(); }}});
#if 0
   // conflicts with textbox readlime bindings
   // -----------------------------------------
   interfaceCommands.push_back({
      .label = "Toggle disassembly\tCtrl+D",
      .shortcut{.code = UI_KEYCODE_LETTER('D'), .ctrl = true, .invoke = CommandToggleDisassembly}
   });
   interfaceCommands.push_back({.label = "Add watch", .shortcut{.invoke = CommandAddWatch}});
   interfaceCommands.push_back({
      .label = nullptr,
      .shortcut{.code = UI_KEYCODE_LETTER('E'), .ctrl = true, .invoke = []() { CommandWatchAddEntryForAddress(); return true; } }
   });
   interfaceCommands.push_back({
      .label = nullptr, .shortcut{.code = UI_KEYCODE_LETTER('B'), .ctrl = true, .invoke = CommandToggleFillDataTab}
   });
   interfaceCommands.push_back({
      .label = nullptr,
      .shortcut{.code = UI_KEYCODE_LETTER('L'), .ctrl = true, .shift = false, .invoke = CommandClearOutput}
   });
#endif

   msgReceivedData    = ReceiveMessageRegister(MsgReceivedData);
   msgReceivedControl = ReceiveMessageRegister(MsgReceivedControl);

   // received buffer contains debugger output to add to log window
   msgReceivedLog = ReceiveMessageRegister([](std::unique_ptr<std::string> buffer) {
      assert(ctx._log_window);
      ctx._log_window->insert_content(*buffer, false);
      ctx._log_window->refresh();
   });
}

void Context::show_menu(UIButton* self) {
   UIMenu& menu = self->ui()->create_menu((UIElement*)self, UIMenu::PLACE_ABOVE | UIMenu::NO_SCROLL);

   for (const auto& ic : _interface_commands) {
      if (ic._label)
         menu.add_item(0, ic._label, [&](UIButton&) { ic._shortcut.invoke(); });
   }
   menu.show();
}

UIElement* Context::switch_to_window_and_focus(string_view target_name) {
   for (auto& [name, w] : _interface_windows) {
      if (!w._el)
         continue;
      if (target_name != name)
         continue;

      if ((w._el->_flags & UIElement::hide_flag) && w._el->_parent->get_class_proc() == UITabPane::_ClassMessageProc) {
         UITabPane* tabPane = (UITabPane*)w._el->_parent;

         for (uint32_t i = 0; i < tabPane->_children.size(); i++) {
            if (tabPane->_children[i] == w._el) {
               tabPane->set_active(i);
               break;
            }
         }

         tabPane->refresh();
      }

      if (w._focus) {
         w._focus(w._el);
      } else if (w._el->_flags & UIElement::tab_stop_flag) {
         w._el->focus();
      }

      return w._el;
   }

   s_main_window->show_dialog(0, "Couldn't find the window '%s'.\n%f%B", target_name, "OK");
   return nullptr;
}

int MainWindowMessageProc(UIElement*, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::WINDOW_ACTIVATE) {
      DisplaySetPosition(s_source_window->_current_file_full, s_display_code->current_line(), false);
   } else {
      for (const auto& msgtype : receiveMessageTypes) {
         if (msgtype._msg == msg) {
            msgtype._callback(std::unique_ptr<std::string>(static_cast<std::string*>(dp)));
            break;
         }
      }
   }

   return 0;
}

int InterfaceTabPaneMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::LAYOUT) {
      el->_class_proc(el, msg, di, dp);

      for (auto& [name, w] : ctx._interface_windows) {
         if (w._el && (~w._el->_flags & UIElement::hide_flag) && w._queued_update) {
            w._queued_update = false;
            w._update("", w._el);
            w._el->move(w._el->_bounds, false);
         }
      }

      return 1;
   }

   return 0;
}

// `current` is the current position when parsing GF_Config::layout_string
// ------------------------------------------------------------------------
const char* InterfaceLayoutNextToken(const char*& current, const char* expected = nullptr) {
   static char buffer[32];
   char*       out = buffer;

   while (isspace(*current)) {
      ++current;
   }

   char first = *current;

   if (first == 0) {
      *out = 0;
   } else if (first == ',' || first == '(' || first == ')') {
      out[0] = first;
      out[1] = 0;
      current++;
   } else if (isalnum(first)) {
      for (uintptr_t i = 0; i < sizeof(buffer) - 1; i++) {
         if (isalnum(*current)) {
            *out++ = *current++;
         } else {
            break;
         }
      }

      *out = 0;
   } else {
      print(std::cerr, "Error: Invalid character in layout string '{}'.\n", first);
      exit(1);
   }

   if (expected) {
      if (*expected == '#') {
         bool valid = true;

         for (int i = 0; buffer[i]; i++) {
            if (!isdigit(buffer[i]))
               valid = false;
         }

         if (!valid) {
            print(std::cerr, "Error: Expected a number in layout string; got '{}'.\n", buffer);
            exit(1);
         }
      } else if (strcmp(expected, buffer)) {
         print(std::cerr, "Error: Expected '{}' in layout string; got '{}'.\n", expected, buffer);
         exit(1);
      }
   }

   return buffer;
}

void Context::additional_setup() {
   if (s_display_code && firstWatchWindow) {
      s_display_code->add_selection_menu_item("Add watch", [&](std::string_view sel) { WatchAddExpression(sel); });
   }
}

void Context::create_layout(UIElement* parent, const char*& layout_string_current) {
   const char* token = InterfaceLayoutNextToken(layout_string_current);

   if (0 == strcmp("h", token) || 0 == strcmp("v", token)) {
      uint32_t flags = UIElement::v_fill | UIElement::h_fill;
      if (*token == 'v')
         flags |= UIElement::vertical_flag;
      InterfaceLayoutNextToken(layout_string_current, "(");
      UIElement* container =
         &parent->add_splitpane(flags, sv_atoi(InterfaceLayoutNextToken(layout_string_current, "#")) * 0.01f);
      InterfaceLayoutNextToken(layout_string_current, ",");
      create_layout(container, layout_string_current);
      InterfaceLayoutNextToken(layout_string_current, ",");
      create_layout(container, layout_string_current);
      InterfaceLayoutNextToken(layout_string_current, ")");
   } else if (0 == strcmp("t", token)) {
      InterfaceLayoutNextToken(layout_string_current, "(");
      char* copy = strdup(layout_string_current);
      for (uintptr_t i = 0; copy[i]; i++)
         if (copy[i] == ',')
            copy[i] = '\t';
         else if (copy[i] == ')')
            copy[i] = 0;
      UIElement* container =
         &parent->add_tabpane(UIElement::v_fill | UIElement::h_fill, copy).set_user_proc(InterfaceTabPaneMessage);
      free(copy);
      create_layout(container, layout_string_current);

      while (true) {
         token = InterfaceLayoutNextToken(layout_string_current);

         if (0 == strcmp(token, ",")) {
            create_layout(container, layout_string_current);
         } else if (0 == strcmp(token, ")")) {
            break;
         } else {
            print(std::cerr, "Error: Invalid layout string! Expected ',' or ')' in tab container list; got '{}'.\n",
                  token);
            exit(1);
         }
      }
   } else {
      if (auto it = _interface_windows.find(token); it != _interface_windows.end()) {
         auto& [name, w] = *it;
         w._el           = w._create(parent);
      } else {
         print(std::cerr, "Error: Invalid layout string! The window '{}' was not found.\n", token);
         exit(1);
      }
   }
}

void Context::generate_layout_string(UIElement* e, std::string& sb) {
   char buf[32];

   if (strcmp(e->_class_name, "Split Pane") == 0) {
      assert(e->_children.size() == 3);
      if (e->_flags & UIElement::vertical_flag) {
         sb.push_back('v');
      } else {
         sb.push_back('h');
      }
      sb.push_back('(');
      int n = snprintf(buf, sizeof(buf), "%d", (int)std::round(((UISplitPane*)e)->weight() * 100));
      sb.insert(sb.end(), buf, buf + n);
      sb.push_back(',');
      generate_layout_string(e->_children[1], sb);
      sb.push_back(',');
      generate_layout_string(e->_children[2], sb);
      sb.push_back(')');
   } else if (strcmp(e->_class_name, "Tab Pane") == 0) {
      sb += "t(";
      for (size_t i = 0; i < e->_children.size(); ++i) {
         if (i > 0)
            sb.push_back(',');
         generate_layout_string(e->_children[i], sb);
      }
      sb.push_back(')');
   } else {
      for (auto& [name, window] : _interface_windows) {
         if (window._el != nullptr && window._el->_id == e->_id) {
            sb += name;
            return;
         }
      }
      assert(0 && "unreachable");
   }
}

bool Context::copy_layout_to_clipboard() {
   std::string sb;
   sb.reserve(512);
   generate_layout_string(s_main_switcher->_children[0]->_children[0], sb);
   _ui->write_clipboard_text(sb, s_main_window, sel_target_t::clipboard);
   return true;
}

unique_ptr<UI> Context::gf_main(int argc, char** argv) {
   if (argc == 2 && (0 == strcmp(argv[1], "-?") || 0 == strcmp(argv[1], "-h") || 0 == strcmp(argv[1], "--help"))) {
      print(std::cerr,
            "Usage: {} [GDB args]\n\n"
            "GDB args: Pass any GDB arguments here, they will be forwarded to GDB.\n\n"
            "For more information, view the README at https://github.com/nakst/gf/blob/master/README.md.\n",
            argv[0]);
      return {};
   }

   // catch some signals
   // ------------------
   std::signal(SIGINT, [](int) {
      ctx.kill_gdb();
      exit(0);
   });
   std::signal(SIGPIPE, [](int) { print(std::cerr, "SIGPIPE Received - ignored.\n"); });

   // process command arguments and create updated version to pass to gdb
   // -------------------------------------------------------------------
   ctx._gdb_argv    = (char**)malloc(sizeof(char*) * (argc + 1));
   ctx._gdb_argv[0] = (char*)"gdb";
   memcpy(ctx._gdb_argv + 1, argv + 1, sizeof(argv) * argc);
   ctx._gdb_argc = argc;

   if (argc >= 2 && 0 == strcmp(argv[1], "--rr-replay")) {
      ctx._gdb_argv[0] = (char*)"rr";
      ctx._gdb_argv[1] = (char*)"replay";
      ctx._gdb_path    = "rr";
   }

   // load settings and initialize ui
   // -------------------------------
   getcwd(gfc._local_config_dir, sizeof(gfc._local_config_dir));
   std_format_to_n(gfc._global_config_path, sizeof(gfc._global_config_path), "{}/.config/gf2_config.ini",
                   getenv("HOME"));
   std_format_to_n(gfc._local_config_path, sizeof(gfc._local_config_path), "{}/.project.gf", gfc._local_config_dir);

   UIConfig ui_config = ctx.load_settings(true);

   ui_config.default_font_size = gfc._interface_font_size;

   auto ui = UI::initialise(ui_config); // sets `ui.default_font_path`
   ctx._ui = ui.get();

   // ui->_theme = uiThemeDark; // force it for now, overriding `gf2_config.ini` - should remove though!

   // create fonts for interface and code
   // -----------------------------------
   const auto& font_path     = ui->default_font_path();
   SourceWindow::s_code_font = ui->create_font(font_path, gfc._code_font_size);
   ui->create_font(font_path, gfc._interface_font_size)->activate();

   if (gfc._window_width == -1 || gfc._window_height == -1) {
      auto dims  = ui->screen_size();
      auto ratio = (float)dims.x / dims.y;
      if (ratio > 2.5f)
         dims.x /= 2; // superwide or two screens
      gfc._window_width  = (int)((float)dims.x * 0.78f);
      gfc._window_height = (int)((float)dims.y * 0.78f);
   }
   s_main_window =
      &(ui->create_window(0, gfc._maximize ? UIWindow::MAXIMIZE : 0, "gf", gfc._window_width, gfc._window_height)
           .set_scale(gfc._ui_scale)
           .set_user_proc(MainWindowMessageProc));

   for (const auto& ic : _interface_commands) {
      if (!(int)ic._shortcut.code)
         continue;
      s_main_window->register_shortcut(ic._shortcut);
   }

   s_main_switcher                   = &s_main_window->add_switcher(0);
   const char* layout_string_current = gfc._layout_string.c_str();
   create_layout(&s_main_switcher->add_panel(UIPanel::EXPAND), layout_string_current);
   s_main_switcher->switch_to(s_main_switcher->_children[0]);

   if (*InterfaceLayoutNextToken(layout_string_current)) {
      print(std::cerr, "Warning: Layout string has additional text after the end of the top-level entry.\n");
   }

   additional_setup();

   ui_config = ctx.load_settings(false);
   if (ui_config._has_theme)
      ui->theme() = ui_config._theme;

   DebuggerStartThread();
   CommandSyncWithGvim();
   return ui;
}

Context::Context() {
   _dbg_re  = std::make_unique<regexp::gdb>();
   _lang_re = std::make_unique<regexp::cpp>();

   add_builtin_windows_and_commands();
   register_extensions();
}

int main(int argc, char** argv) {
   auto ui_ptr = ctx.gf_main(argc, argv);
   if (!ui_ptr)
      return 1;

   ui_ptr->message_loop();
   ctx.kill_gdb();

   if (gfc._restore_watch_window && firstWatchWindow) {
      std_format_to_n(gfc._global_config_path, sizeof(gfc._global_config_path), "{}/.config/gf2_watch.txt",
                      getenv("HOME"));
      FILE* f = fopen(gfc._global_config_path, "wb");

      if (f) {
         for (const auto& exp : firstWatchWindow->base_expressions()) {
            print(f, "{}\n", exp->key());
         }

         fclose(f);
      } else {
         print(std::cerr, "Warning: Could not save the contents of the watch window; '{}' was not accessible.\n",
               gfc._global_config_path);
      }
   }

   return 0;
}
