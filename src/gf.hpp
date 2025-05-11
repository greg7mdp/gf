#pragma once

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
#include <filesystem>
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

namespace fs = std::filesystem;

#include "luigi.hpp"

#define ALLOW_SIDE_EFFECTS 0   // evaluating expressions with side effects in debugger can change program behavior

// ---------------------------------------------------
// ---------------------------------------------------
struct ControlPipe {
   static void* thread_proc(void*);
   static void  on_command(std::unique_ptr<std::string> input);
};

// ---------------------------------------------------
// Source display
// ---------------------------------------------------
struct SourceWindow {
   static UIFont* s_code_font;

   int                    _auto_print_expression_line;
   int                    _auto_print_result_line;
   std::array<char, 1024> _auto_print_expression;
   std::array<char, 1024> _auto_print_result;

   std::string _current_file;
   std::string _current_file_full;
   time_t      _current_file_read_time;
   bool        _showing_disassembly;

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

   std::vector<InspectResult> _inspect_results;
   bool                       _no_inspect_results;
   bool                       _in_inspect_line_mode = false;
   int                        _inspect_mode_restore_line;
   UIRectangle                _display_current_line_bounds;
   const char*                _disassembly_command = "disas /s";

   static std::string s_previous_file_loc;   // `<file path>:<line>`

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

   static UIElement* Create(UIElement* parent);

   static void Update(const char* data, UIElement* el) {
      static_cast<SourceWindow*>(el->_cp)->_update(data, static_cast<UICode*>(el));
   }
};

// ---------------------------------------------------
// StackWindow
// ---------------------------------------------------
struct StackEntry {
   std::string _function;   // `<function_name>`
   std::string _location;   // `<file path>:<line>`
   uint64_t    _address;
   int         _id;
};

struct StackWindow {
private:
   std::vector<StackEntry> _stack;
   size_t                  _selected;
   bool                    _has_changed;

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

// ---------------------------------------------------/
// Executable window:
// ---------------------------------------------------/
struct ExecutableWindow {
private:
   UITextbox* _path      = nullptr;
   UITextbox* _arguments = nullptr;
   bool       _should_ask;
   uint32_t   _current_index = 0;   // current index of `_path`/`_arguments` in `get_prog_config_path()`

public:
   void              start_or_run(bool pause);
   void              save();
   static UIElement* Create(UIElement* parent);

   std::string       get_path() const { return std::string{_path->text()}; }
   std::string       get_arguments() const { return std::string{_arguments->text()}; }
};


// ---------------------------------------------------
// ---------------------------------------------------
