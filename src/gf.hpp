#pragma once

#define GF_VERSION_MAJOR 0
#define GF_VERSION_MINOR 8
#define GF_VERSION_PATCH 0

#include "luigi.hpp"
#include "clangd.hpp"
#include "utils.hpp"

#include <algorithm>
#include <cassert>
#include <condition_variable>
#include <cstdarg>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <memory>
#include <mutex>
#include <unistd.h>
#include <queue>
#include <unordered_map>
#include <vector>
#include <iostream>

namespace fs = std::filesystem;

#include <ctre.hpp>
using namespace ctre::literals;

#include <re/re.hpp>
using namespace regexp;

#include <nlohmann/json.hpp>
using json = nlohmann::json;

#define ALLOW_SIDE_EFFECTS 0 // evaluating expressions with side effects in debugger can change program behavior

// ---------------------------------------------------
// ---------------------------------------------------
struct ControlPipe {
   static void* thread_proc(void*);
   static void  on_command(std::unique_ptr<std::string> input);
};

// ---------------------------------------------------
struct AutoUpdateViewer {
   UIElement* _el                = nullptr;
   void (*_callback)(UIElement*) = nullptr;
};

// ---------------------------------------------------
// Source display
// ---------------------------------------------------
struct SourceWindow {
   int                    _auto_print_expression_line = 0;
   int                    _auto_print_result_line     = 0;
   std::array<char, 1024> _auto_print_expression;
   std::array<char, 1024> _auto_print_result;

   UIFont*     _code_font = nullptr;
   std::string _current_file;
   bool        _showing_disassembly = false;

private:
   int _current_end_of_block = 0;
   int _last_cursor_x        = 0;
   int _last_cursor_y        = 0;

   int _if_condition_evaluation = 0;
   int _if_condition_line       = 0;
   int _if_condition_from       = 0;
   int _if_condition_to         = 0;

   struct InspectResult {
      std::string _expression;
      std::string _value;
   };

   std::vector<InspectResult> _inspect_results;
   bool                       _no_inspect_results        = false;
   bool                       _in_inspect_line_mode      = false;
   int                        _inspect_mode_restore_line = 0;
   const char*                _disassembly_command       = "disas /s";
   UIRectangle                _display_current_line_bounds;

   static std::string s_previous_file_loc; // `<file path>:<line>`

   int _code_message_proc(UICode* code, UIMessage msg, int di, void* dp);
   int _line_message_proc(UIElement* el, UIMessage msg, int di, void* dp);

   void _update(const char* data, UICode* el);

public:
   std::array<char, 1024>& auto_print_result() { return _auto_print_result; }

   bool load_file(const std::string_view file, bool useGDBToGetFullPath);
   bool display_set_position(const std::string_view file, std::optional<size_t> line, bool useGDBToGetFullPath);
   void jump_to_position(const std::string_view file, const UICode::code_pos_pair_t& pos, bool useGDBToGetFullPath);
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

   static UIElement* Create(UIElement* parent);

   static void Update(const char* data, UIElement* el) {
      static_cast<SourceWindow*>(el->_cp)->_update(data, static_cast<UICode*>(el));
   }
};

// ---------------------------------------------------
// StackWindow
// ---------------------------------------------------
struct StackEntry {
   std::string _function; // `<function_name>`
   std::string _location; // `<file path>:<line>`
   uint64_t    _address = 0;
   int         _id      = 0;
};

struct StackWindow {
private:
   std::vector<StackEntry> _stack;
   size_t                  _selected    = 0;
   bool                    _has_changed = false;

public:
   void clear() { _stack.clear(); }
   void append(const StackEntry& entry) { _stack.push_back(entry); }
   bool has_selection() const { return _selected < _stack.size(); }
   bool changed() const { return _has_changed; }
   void set_selected(size_t i) { _selected = i; }
   void set_changed(bool b) { _has_changed = b; }

   void update_stack();

   std::vector<StackEntry>& stack() { return _stack; }

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

// ---------------------------------------------------
// WatchLogger
// ---------------------------------------------------
struct WatchLogEvaluated {
   std::string _result;
};

struct WatchLogEntry {
   std::string                    _value;
   std::string                    _where;
   std::vector<WatchLogEvaluated> _evaluated;
   std::vector<StackEntry>        _trace;
};

struct WatchLogger {
   int                        _id             = 0;
   int                        _selected_entry = 0;
   char                       _columns[256]   = {0};
   std::string                _expressions_to_evaluate;
   std::vector<WatchLogEntry> _entries;
   UITable*                   _table = nullptr;
   UITable*                   _trace = nullptr;
};

// ---------------------------------------------------/
// Executable window:
// ---------------------------------------------------/
struct ExecutableWindow {
private:
   enum exe_flags {
      ef_breakpoints_restored = 1 << 0,
      ef_watches_restored     = 1 << 1,
   };

   std::string _current_exe;       // path of current executable once it has started at least once.
   uint32_t    _current_exe_flags; // reset to 0 when executable changes. So we restore stuff only once.
   fs::path    _prog_config_path;  // set to `.ini` config file path for current running program
   fs::path    _prog_history_path; // set to `.hist` history file path for current running program
   bool        _same_prog = false;

   UITextbox* _path      = nullptr;
   UITextbox* _arguments = nullptr;
   bool       _should_ask{false};

public:
   size_t _current_path_index = 0; // current index of `_path` in <prog>.ini in .gf
   size_t _current_arg_index  = 0; // current index of `_arguments` in `get_prog_config_path()`

   static UIElement* Create(UIElement* parent);

   void        start_or_run(bool pause);
   std::string start_info_json();
   std::string get_path() const { return std::string{_path->text()}; }
   std::string get_arguments() const { return std::string{_arguments->text()}; }
   void        update_path(const fs::path& prog_config_path, int incr); // should be -1, 0 or +1
   void        update_args(const fs::path& prog_config_path, int incr,  // should be -1, 0 or +1
                           bool update_exe_path);
   void        restore_watches();
   void        restore_breakpoints();
   void        save_watches();
   void        save_breakpoints();
   void        save_prog_args();

private:
   void maybe_clear_exe_info();
   void maybe_set_exe_info(std::string_view exe_path);
};

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

   std::optional<T> pop() {
      std::unique_lock lock(_mutex);
      bool             res = _cv.wait_for(lock, std::chrono::seconds(15), [this] { return !_queue.empty() || _quit; });

      if (!res || _quit) { // !res means we hit the timeout
         return {};
      }

      auto value = std::move(_queue.front());
      _queue.pop();
      return std::optional<T>{std::move(value)};
   }

   size_t size() const {
      std::unique_lock lock(_mutex);
      return _queue.size();
   }

   void signal_quit() {
      _quit = true;
      _cv.notify_all();
   }

   bool is_quitting() { return _quit; }

private:
   std::queue<T>           _queue;
   mutable std::mutex      _mutex;
   std::condition_variable _cv;
   std::atomic<bool>       _quit;
};

// ---------------------------------------------------------------------------------------------
//                              Data structures
// ---------------------------------------------------------------------------------------------
struct InterfaceCommand {
   const char* _label = nullptr;
   UIShortcut  _shortcut;
};

struct InterfaceWindow {
   UIElement* (*_create)(UIElement* parent)                      = nullptr;
   void (*_update)(const char* data, UIElement* el)              = nullptr;
   void (*_focus)(UIElement* el)                                 = nullptr;
   UIElement* _el                                                = nullptr; // UIElement returned by `_create` fn
   bool       _queued_update                                     = false;
   bool       _always_update                                     = false;
   void (*_config)(std::string_view key, std::string_view value) = nullptr;
};

struct InterfaceDataViewer {
   const char* _add_button_label  = nullptr;
   void (*_add_button_callback)() = nullptr;
};

// --------------------------------------------------------------------------------------------
// Navigation history for go-to-definition / go-back
// --------------------------------------------------------------------------------------------
struct NavLocation {
   std::string             _file;
   UICode::code_pos_pair_t _pos;
};

struct NavigationHistory {
   std::vector<NavLocation> _history;
   size_t                   _current = 0; // index in history, 1-based

   void push(std::string_view file, const UICode::code_pos_pair_t& pos) {
      // Remove any forward history when pushing a new location
      if (_current < _history.size()) {
         _history.resize(_current);
      }
      _history.emplace_back(std::string(file), pos);
      _current = _history.size();
   }

   std::optional<NavLocation> go_back() {
      if (_current > 0) {
         _current--;
         return _history[_current];
      }
      return std::nullopt;
   }

   std::optional<NavLocation> go_forward() {
      if (_current < _history.size()) {
         _current++;
         return _history[_current - 1];
      }
      return std::nullopt;
   }
};

// --------------------------------------------------------------------------------------------
struct UserMessageTypes {
   UIMessage                                         _msg;
   std::function<void(std::unique_ptr<std::string>)> _callback;
};

// --------------------------------------------------------------------------------------------
struct ExeStartInfo {
   std::string _path;
   std::string _args;

   friend std::ostream& operator<<(std::ostream& os, const ExeStartInfo& esi) {
      os << "ExeStartInfo(\"" << esi._path << "\", \"" << esi._args << "\")";
      return os;
   }

   NLOHMANN_DEFINE_TYPE_INTRUSIVE(ExeStartInfo, _path, _args)
};

// --------------------------------------------------------------------------------------------
struct Command {
   std::string _key;
   std::string _value;
};

// --------------------------------------------------------------------------------------------
//                      Config (mostly read from `gf2_config.ini` file)
// --------------------------------------------------------------------------------------------
struct GF_Config {
   std::string _layout_string = "v(75,h(50,Source,v(50,t(Exe,Breakpoints,Commands,Struct),t(Stack,Files,Thread,"
                                "CmdSearch))),h(40,Console,t(Watch,Locals,Registers,Data,Log,Prof,Memory,View)))";

   // executable window
   // -----------------
   ExeStartInfo _exe;
   bool         _ask_dir          = false;
   bool         _allow_exe_config = false;

   // misc
   // ----
   std::unique_ptr<const char[]> _control_pipe_path;
   std::string                   _vim_server_name;
   std::string                   _log_pipe_path;
   std::vector<Command>          _preset_commands;
   fs::path _global_config_path; // ~/.config/gf_config.ini or ~/.config/gf2_config.ini (main config file)
   fs::path _current_directory;  // <cwd>
   fs::path _local_config_dir;   // <path>/.gf where <path> is `cwd` dir or the one above<cwd>
   fs::path _local_config_path;  // <path>/.gf/gf_config.ini (stores overrides to `gf_config.ini`?)
   int      _code_font_size           = 13;
   int      _interface_font_size      = 11;
   int      _window_width             = -1;
   int      _window_height            = -1;
   float    _ui_scale                 = 1;
   bool     _maximize                 = false;
   bool     _selectable_source        = true;
   bool     _restore_watch_window     = true;
   bool     _restore_breakpoints      = true;
   bool     _restore_prog_args        = true;
   bool     _confirm_command_connect  = true;
   bool     _confirm_command_kill     = true;
   int      _backtrace_count_limit    = 50;
   bool     _grab_focus_on_breakpoint = true;

   void init() {
      assert(_current_directory.empty() && _local_config_dir.empty()); // make sure it is called only once

      _current_directory = get_realpath(my_getcwd());

      // ----------------------------
      // Figure out local config dir.
      // start from current dir
      // ----------------------------
      _local_config_dir = _current_directory;

      // go one dir up is current directory starts with "build"
      // ------------------------------------------------------
      if (_local_config_dir.filename().native().starts_with("build"))
         _local_config_dir = _local_config_dir.parent_path();

      // `.gf` is the directory holding the local config files
      // -----------------------------------------------------
      _local_config_dir.append(".gf");
      fs::create_directories(_local_config_dir); // create directory if it doesn't exist

      _global_config_path = std::format("{}/.config/gf_config.ini", getenv("HOME"));
      if (!fs::exists(_global_config_path)) {
         // if `gf_config.ini` not present, also accept `gf2_config.ini`
         _global_config_path = std::format("{}/.config/gf2_config.ini", getenv("HOME"));
      }

      _local_config_path = std::format("{}/gf_config.ini", _local_config_dir.native());
   }

private:
   // return <path>/.gf/<progname>.<extension> where path is `cwd` dir where `gf` was launched,
   // or the one above
   fs::path get_prog_path(std::string_view extension);

public:
   const fs::path& get_local_config_dir() { return _local_config_dir; }

   // <path>/.gf/<progname>.ini
   fs::path get_prog_config_path() { return get_prog_path(".ini"); }

   // <path>/.gf/<progname>.hist
   fs::path get_command_history_path() { return get_prog_path(".hist"); }

   std::vector<fs::path> get_progs() {
      std::vector<fs::path> res;
      fs::path              path{get_local_config_dir()};
      assert(fs::is_directory(path));

      for (const auto& entry : fs::directory_iterator(path)) {
         std::filesystem::path p = entry.path();
         if (std::filesystem::is_regular_file(p)) {
            if (p.filename() == "gf_config.ini")
               continue; // skip local config file

            if (p.extension() == ".ini")
               res.push_back(entry);
         }
      }

      return res;
   }

   UIConfig load_settings(bool earlyPass);
};

// --------------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------------
struct WatchWindow;
struct ConsoleWindow;
struct DataWindow;
struct ProfFlameGraphReport;

using opt_string = std::optional<std::string>;

struct Context {
// ========== Debugger config ======================
#if defined(__OpenBSD__)
   std::string _gdb_path = "egdb";
#else
   std::string _gdb_path = "gdb";
#endif
   std::string _initial_gdb_command = "set prompt (gdb) ";
   bool        _first_update        = true;
   UICode*     _log_window          = nullptr; // if sent, log all debugger output there
   ui_handle   _prev_focus_win      = 0;
   UI*         _ui; // non-owning pointer

   unique_ptr<regexp::debugger_base> _dbg_re;
   unique_ptr<regexp::language_base> _lang_re;

   // ========== Debugger interaction ======================
   using arg_vec = std::vector<std::unique_ptr<const char[]>>;

   int               _pipe_to_gdb     = 0;
   pid_t             _gdb_pid         = 0;
   std::atomic<bool> _kill_gdb_thread = false;
   std::thread       _gdb_thread; // reads gdb output and pushes it to queue (we wait on queue in DebuggerSend
   std::atomic<bool> _evaluate_mode = false; // true means we sent a command to gdb and are waiting for the response
   arg_vec           _gdb_argv;
   SPSCQueue<std::string> _evaluate_result_queue;
   std::atomic<bool>      _program_running = true;
   bool                   _program_started = false; // So we know to restart the program if it has exited

   //  ========== Gui stuff  ======================
   using window_map = std::unordered_map<std::string, InterfaceWindow>;

   window_map                       _interface_windows;
   std::vector<InterfaceCommand>    _interface_commands;
   std::vector<InterfaceDataViewer> _interface_data_viewers;
   NavigationHistory                _nav_history;
   WatchWindow*                     _first_watch_window = nullptr;
   std::vector<AutoUpdateViewer>    _auto_update_viewers;
   bool                             _auto_update_viewers_queued;
   std::vector<WatchLogger*>        _watch_loggers;

   //  ========== clangd  ======================
   ClangdClient _clangd;
   std::string  _clangd_path = "clangd";

   //  ========== Messages  ======================
   std::vector<UserMessageTypes> _user_message_types;
   UIMessage                     _msg_received_data;
   UIMessage                     _msg_received_log;
   UIMessage                     _msg_received_control;
   UIMessage                     _msg_received_clangd;
   UIMessage                     _msg_received_next = (UIMessage::USER_PLUS_1);

   // ========== pointers to interface windows  ======================
   ConsoleWindow*    _console_window    = nullptr;
   UIMDIClient*      _data_mdiclient    = nullptr;
   UIPanel*          _data_tab          = nullptr;
   DataWindow*       _data_window       = nullptr;
   UICode*           _display_code      = nullptr;
   UICode*           _display_output    = nullptr;
   ExecutableWindow* _executable_window = nullptr;
   UITextbox*        _input_textbox     = nullptr;
   UIWindow*         _main_window       = nullptr;
   UISwitcher*       _main_switcher     = nullptr;
   SourceWindow*     _source_window     = nullptr;
   StackWindow*      _stack_window      = nullptr;
   UISpacer*         _trafficlight      = nullptr;

   // ========== profiler stuff (should be std::atomic I believe)  ======================
   volatile int _prof_render_thread_index_allocator;
   ProfFlameGraphReport* volatile _prof_render_report;
   UIPainter* volatile _prof_render_painter;
   volatile int _prof_render_active_threads;


   Context();

   // callback frees the `std::string*` received in `void* dp` by inserting it into a
   // `unique_ptr` in `MainWindowMessageProc`
   // -------------------------------------------------------------------------------
   UIMessage register_user_message(std::function<void(std::unique_ptr<std::string>)> callback) {
      _user_message_types.push_back({._msg = _msg_received_next, ._callback = std::move(callback)});
      _msg_received_next = static_cast<UIMessage>(static_cast<uint32_t>(_msg_received_next) + 1);
      return _user_message_types.back()._msg;
   }

   // make private
   void send_to_gdb(std::string_view sv) const {
      char newline = '\n';
      (void)!write(_pipe_to_gdb, sv.data(), sv.size());
      (void)!write(_pipe_to_gdb, &newline, 1);
   }

   void interrupt_gdb(size_t wait_time_us = 20ull * 1000) {
      if (_program_running) {
         kill(_gdb_pid, SIGINT);
         std::this_thread::sleep_for(std::chrono::microseconds{wait_time_us});
         _program_running = false;
      }
   }

   void kill_gdb_thread() {
      // std::print(std::cerr, "killing gdb thread.\n");
      _kill_gdb_thread = true;
      _gdb_thread.join();
      _kill_gdb_thread = false;
   }

   void kill_gdb() {
      kill_gdb_thread();
      // std::print(std::cerr, "killing gdb process {}.\n", _gdb_pid);
      kill(_gdb_pid, SIGKILL);
   }

   void start_debugger_thread() {
      _gdb_thread = std::thread([this]() { debugger_thread_fn(); });
   }

   std::string    eval_command(std::string_view command, bool echo = false);
   std::string    eval_expression(std::string_view expression, std::string_view format = {});
   opt_string     send_command_to_debugger(std::string_view command, bool echo, bool synchronous);
   opt_string     send_to_gdb_internal(std::string_view command, bool synchronous);
   void           send_to_gdb_async(std::string_view s) { (void)send_to_gdb_internal(s, false); }
   void           shell_or_send_to_gdb_internal(std::string_view command);
   bool           set_disassembly_mode() { return _source_window->set_disassembly_mode(); }
   bool           inspect_line() { return _source_window->inspect_line(); }
   bool           sync_with_gvim();
   bool           goto_definition();
   bool           go_back();
   bool           display_set_position(std::string_view file, std::optional<size_t> line);
   int            source_find_end_of_block();
   bool           source_find_outer_function_call(const char** start, const char** end);
   void           msg_received_data(std::unique_ptr<std::string> input);
   void           save_user_info();
   void           grab_focus(UIWindow* win);
   void           restore_focus();
   void           debugger_thread_fn();
   void           add_builtin_windows_and_commands();
   void           register_extensions();
   void           show_menu(UIButton* self);
   void           create_layout(UIElement* parent, const char*& current);
   void           generate_layout_string(UIElement* e, std::string& sb) const;
   bool           copy_layout_to_clipboard() const;
   void           save_layout() const;
   std::string    read_layout(const fs::path& local_config_path) const;
   void           additional_setup();
   UIElement*     switch_to_window_and_focus(std::string_view name);
   UIElement*     find_window(std::string_view name);
   void           emplace_gdb_args_from_ini_file(std::string_view val);
   ExeStartInfo   emplace_gdb_args_from_command_line(int argc, char** argv);
   unique_ptr<UI> gf_main(int argc, char** argv);
};

// --------------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------------
enum class bp_command { ena, dis, del };

struct BreakpointMgr {
private:
   struct Breakpoint {
      int         _number{0};
      std::string _file;
      std::string _full_path; // realpath of `_file`, computed, do not serialize
      int         _line       = 0;
      int         _hit        = 0; // count of hits - do not serialize
      bool        _watchpoint = false;
      bool        _enabled    = false;
      bool        _multiple   = false;
      std::string _condition;
      uint64_t    _condition_hash = 0; // hash of _condition, computed

      [[nodiscard]] bool match(int line, std::string_view path) const { return _line == line && path == _full_path; }

      void       command(bp_command cmd) const;
      void       toggle() const { command(_enabled ? bp_command::dis : bp_command::ena); }
      Breakpoint update() &&;
      void       set(); // sets that breakpoint in gdb
      void       clear();

      NLOHMANN_DEFINE_TYPE_INTRUSIVE(Breakpoint, _file, _line, _watchpoint, _enabled, _condition)
   };

   friend struct BreakpointsWindow;

   std::vector<Breakpoint> _breakpoints; // current debugger breakpoints

public:
   [[nodiscard]] size_t num_breakpoints() const { return _breakpoints.size(); }

   template <class F>
   void for_all_matching_breakpoints(int line, std::string_view path, F&& f) {
      for (size_t i = 0; i < _breakpoints.size(); i++) {
         if (_breakpoints[i].match(line, path)) {
            std::forward<F>(f)(i, _breakpoints[i]);
         }
      }
   }

   template <class F>
   void for_all_breakpoints(F&& f) {
      for (size_t i = 0; i < _breakpoints.size(); i++) {
         std::forward<F>(f)(i, _breakpoints[i]);
      }
   }

   void delete_all_breakpoints();
   void toggle_breakpoint(int line = 0);
   void restore_breakpoints(const fs::path& ini_path);
   void update_breakpoints_from_gdb();
};