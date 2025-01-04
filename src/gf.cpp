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
#include "regex.hpp"

// ---------------------------------------------------------------------------------------------
//                              Generic Data structures
// ---------------------------------------------------------------------------------------------
template <typename T>
class SPSCQueue {
public:
   SPSCQueue()
      : quit_(false) {}

   void push(T&& item) {
      std::unique_lock<std::mutex> lock(mutex_);
      queue_.push(std::move(item));
      cv_.notify_one();
   }

   bool pop(std::optional<T>& value) {
      std::unique_lock<std::mutex> lock(mutex_);
      bool res = cv_.wait_for(lock, std::chrono::seconds(15), [this] { return !queue_.empty() || quit_; });

      if (!res || quit_) { // !res means we hit the timeout
         value = std::optional<T>{};
         return false;
      }

      value = std::move(queue_.front());
      queue_.pop();
      return true;
   }

   size_t size() const {
      std::unique_lock<std::mutex> lock(mutex_);
      return queue_.size();
   }

   void signal_quit() {
      quit_ = true;
      cv_.notify_one();
   }

   bool is_quitting() { return quit_; }

private:
   std::queue<T>           queue_;
   mutable std::mutex      mutex_;
   std::condition_variable cv_;
   std::atomic<bool>       quit_;
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

static inline int sv_atoi(string_view str, size_t offset = 0) {
   return sv_atoi_impl<int>(str, offset);
}

[[maybe_unused]] static inline long sv_atol(string_view str, size_t offset = 0) {
   return sv_atoi_impl<long>(str, offset);
}

[[maybe_unused]] static inline long long sv_atoll(string_view str, size_t offset = 0) {
   return sv_atoi_impl<long long>(str, offset);
}

[[maybe_unused]] static inline uint64_t sv_atoul(string_view str, size_t offset = 0) {
   return sv_atoi_impl<uint64_t>(str, offset);
}

void print(const std::string_view str) {
   std::cout << str;
}

// Variadic template for print with format arguments
template <typename... Args>
void print(std::format_string<Args...> fmt, Args&&... args) {
   std::cout << std::format(fmt, std::forward<Args>(args)...);
}

// ---------------------------------------------------------------------------------------------
//                              Data structures
// ---------------------------------------------------------------------------------------------
struct InterfaceCommand {
   const char* label = nullptr;
   UIShortcut  shortcut;
};

struct InterfaceWindow {
   UIElement* (*create)(UIElement* parent)            = nullptr;
   void (*update)(const char* data, UIElement* el)    = nullptr;
   void (*focus)(UIElement* el)                       = nullptr;
   UIElement* el                                      = nullptr;
   bool       queuedUpdate                            = false;
   bool       alwaysUpdate                            = false;
   void (*config)(string_view key, string_view value) = nullptr;
};

struct InterfaceDataViewer {
   const char* addButtonLabel = nullptr;
   void (*addButtonCallback)();
};

struct INIState {
   char*  buffer       = nullptr;
   char*  section      = nullptr;
   char*  key          = nullptr;
   char*  value        = nullptr;
   size_t bytes        = 0;
   size_t sectionBytes = 0;
   size_t keyBytes     = 0;
   size_t valueBytes   = 0;

   INIState& operator=(const INIState&) = delete;

   INIState& operator=(INIState&&) = default;

   INIState() = default;

   ~INIState() {
      free(buffer);
      free(section);
      free(key);
      free(value);
   }

   INIState(INIState&& o)
      : INIState() {
      std::swap(buffer, o.buffer);
      std::swap(section, o.section);
      std::swap(key, o.key);
      std::swap(value, o.value);
      std::swap(bytes, o.bytes);
      std::swap(sectionBytes, o.sectionBytes);
      std::swap(keyBytes, o.keyBytes);
      std::swap(valueBytes, o.valueBytes);
   }

   // this because INIState is filled with pointers to strings hacked from the result of `LoadFile(configFile)`,
   // and then inserted into a vector (see `presetCommands.push_back(state.clone())`
   // Hard to not leak memory with these shenanigans.
   INIState clone() {
      INIState s = *this;
      if (buffer)
         s.buffer = strdup(buffer);
      if (section)
         s.section = strdup(section);
      if (key)
         s.key = strdup(key);
      if (value)
         s.value = strdup(value);
      return s;
   }

   void clear() { buffer = section = key = value = nullptr; }

private:
   INIState(const INIState&) = default;
};

struct ReceiveMessageType {
   UIMessage                                         msg;
   std::function<void(std::unique_ptr<std::string>)> callback;
};

struct WatchWindow;

struct Context {
// ========== Debugger config ======================
#if defined(__OpenBSD__)
   const char* gdbPath = "egdb";
#else
   const char* gdbPath = "gdb";
#endif
   std::string initialGDBCommand = "set prompt (gdb) ";
   bool        firstUpdate       = true;
   UICode*     logWindow         = nullptr; // if sent, log all debugger output there

   // ========== Debugger interaction ======================
   int               pipeToGDB     = 0;
   pid_t             gdbPID        = 0;
   std::atomic<bool> killGdbThread = false;
   std::thread       gdbThread;            // reads gdb output and pushes it to queue (we wait on queue in DebuggerSend
   std::atomic<bool> evaluateMode = false; // when true, means we sent a command to gdb and are waiting for the response
   char**            gdbArgv      = nullptr;
   int               gdbArgc      = 0;
   SPSCQueue<std::string> evaluateResultQueue;
   std::atomic<bool>      programRunning = true;

   std::unordered_map<std::string, InterfaceWindow> interfaceWindows;
   vector<InterfaceCommand>                         interfaceCommands;
   vector<InterfaceDataViewer>                      interfaceDataViewers;

   Context();

   // make private
   void SendToGdb(string_view sv) const {
      char newline = '\n';
      write(pipeToGDB, sv.data(), sv.size());
      write(pipeToGDB, &newline, 1);
   }

   void InterruptGdb(size_t wait_time_us = 20 * 1000) {
      if (programRunning) {
         kill(gdbPID, SIGINT);
         std::this_thread::sleep_for(std::chrono::microseconds{wait_time_us});
         programRunning = false;
      }
   }

   void KillGdbThread() {
      print(std::cerr, "killing gdb thread.\n");
      killGdbThread = true;
      gdbThread.join();
      killGdbThread = false;
   }

   void KillGdb() {
      KillGdbThread();
      print(std::cerr, "killing gdb process {}.\n", gdbPID);
      kill(gdbPID, SIGKILL);
   }

   void           DebuggerThread();
   UIConfig       SettingsLoad(bool earlyPass);
   void           InterfaceAddBuiltinWindowsAndCommands();
   void           RegisterExtensions();
   void           InterfaceShowMenu(UIButton* self);
   void           InterfaceLayoutCreate(UIElement* parent);
   UIElement*     InterfaceWindowSwitchToAndFocus(string_view name);
   unique_ptr<UI> GfMain(int argc, char** argv);
};

Context ctx;

// --------------------------------------------------------------------------------------------
struct GF_Config {
   const char* layout_string =
      "v(75,h(80,Source,v(50,t(Exe,Breakpoints,Commands,Struct),t(Stack,Files,Thread,CmdSearch)))"
      ",h(65,Console,t(Watch,Locals,Registers,Data)))";

   // executable window
   // -----------------
   std::string exe_path;
   std::string exe_args;
   bool        exe_ask_dir = true;
};

GF_Config gfc;

FILE*                      commandLog      = nullptr;
char                       emptyString     = 0;
const char*                vimServerName   = "GVIM";
const char*                logPipePath     = nullptr;
const char*                controlPipePath = nullptr;
vector<INIState>           presetCommands;
char                       globalConfigPath[PATH_MAX];
char                       localConfigDirectory[PATH_MAX];
char                       localConfigPath[PATH_MAX];
vector<ReceiveMessageType> receiveMessageTypes;
int                        code_font_size      = 13;
int                        interface_font_size = 11;
int                        window_width        = 800;
int                        window_height       = 600;
float                      ui_scale            = 1;
bool                       selectableSource    = true;
bool                       restoreWatchWindow;
WatchWindow*               firstWatchWindow = nullptr;
bool                       maximize;
bool                       confirmCommandConnect = true, confirmCommandKill = true;
int                        backtraceCountLimit = 50;
UIMessage msgReceivedData, msgReceivedLog, msgReceivedControl, msgReceivedNext = (UIMessage)(UIMessage::USER_PLUS_1);

// Current file and line:

char   currentFile[PATH_MAX];
char   currentFileFull[PATH_MAX];
time_t currentFileReadTime;
bool   showingDisassembly;
char   previousLocation[256];

// User interface:

UIWindow*   windowMain   = nullptr;
UISwitcher* switcherMain = nullptr;

UICode*    displayCode   = nullptr;
UICode*    displayOutput = nullptr;
UITextbox* textboxInput  = nullptr;
UISpacer*  trafficLight  = nullptr;

UIMDIClient* dataWindow = nullptr;
UIPanel*     dataTab    = nullptr;

UIFont* code_font = nullptr;

// Breakpoints:

struct Breakpoint {
   int      number = 0;
   char     file[PATH_MAX];
   char     fileFull[PATH_MAX];
   int      line       = 0;
   int      hit        = 0;
   bool     watchpoint = false;
   bool     enabled    = false;
   bool     multiple   = false;
   char     condition[128];
   uint64_t conditionHash = 0;
};

vector<Breakpoint> breakpoints;

// Stack:

struct StackEntry {
   char     function[64];
   char     location[sizeof(previousLocation)];
   uint64_t address;
   int      id;
};

vector<StackEntry> stack;
size_t             stackSelected;
bool               stackChanged;

// Python code:

const char* pythonCode = R"(py

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

bool DisplaySetPosition(const char* file, std::optional<size_t> line, bool useGDBToGetFullPath);
void WatchAddExpression2(string_view string);
int  WatchWindowMessage(UIElement* el, UIMessage msg, int di, void* dp);
bool CommandInspectLine();

// ------------------------------------------------------
// Utilities:
// ------------------------------------------------------

inline uint64_t Hash(const uint8_t* key, size_t keyBytes) {
   uint64_t hash = 0xCBF29CE484222325;
   for (uintptr_t i = 0; i < keyBytes; i++)
      hash = (hash ^ key[i]) * 0x100000001B3;
   return hash;
}

// reads into `destination` until character `c1` found (or we reach the end of `s->buffer`)
#define INI_READ(destination, counter, c1, c2)              \
   s->destination = s->buffer, s->counter = 0;              \
   while (s->bytes && *s->buffer != c1 && *s->buffer != c2) \
      s->counter++, s->buffer++, s->bytes--;                \
   if (s->bytes && *s->buffer == c1)                        \
      s->buffer++, s->bytes--;

bool INIParse(INIState* s) {
   while (s->bytes) {
      char c = *s->buffer;

      if (c == ' ' || c == '\n' || c == '\r') {
         s->buffer++, s->bytes--;
         continue;
      } else if (c == ';') {
         s->valueBytes = 0;
         INI_READ(key, keyBytes, '\n', 0);
      } else if (c == '[') {
         s->keyBytes = s->valueBytes = 0;
         s->buffer++, s->bytes--;
         INI_READ(section, sectionBytes, ']', 0);
      } else {
         INI_READ(key, keyBytes, '=', '\n');
         INI_READ(value, valueBytes, '\n', 0);
      }

      if (s->sectionBytes)
         s->section[s->sectionBytes] = 0;
      else
         s->section = &emptyString;

      if (s->keyBytes)
         s->key[s->keyBytes] = 0;
      else
         s->key = &emptyString;

      if (s->valueBytes)
         s->value[s->valueBytes] = 0;
      else
         s->value = &emptyString;

      return true;
   }

   return false;
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
                                                  ctx.programRunning ? el->theme().accent1 : el->theme().accent2,
                                                  el->theme().border, UIRectangle(1));
   }

   return 0;
}

int SourceFindEndOfBlock() {
   auto currentLine = displayCode->current_line();

   if (!currentLine)
      return -1;

   int tabs = 0;

   auto line = displayCode->line(*currentLine);
   for (size_t i = 0; i < line.size(); i++) {
      if (isspace(line[i]))
         tabs++;
      else
         break;
   }

   size_t num_lines = displayCode->num_lines();
   for (size_t j = *currentLine + 1; j < num_lines; j++) {
      int t = 0;

      auto line = displayCode->line(j);
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
   int  num_lines   = (int)displayCode->num_lines();
   auto currentLine = displayCode->current_line();

   if (!currentLine)
      return false;

   size_t offset = displayCode->line_offset(*currentLine);
   bool   found  = false;

   // Look forwards for the end of the call ");".

   size_t num_chars = displayCode->size();
   while (offset < num_chars - 1) {
      if ((*displayCode)[offset] == ')' && (*displayCode)[offset + 1] == ';') {
         found = true;
         break;
      } else if ((*displayCode)[offset] == ';' || (*displayCode)[offset] == '{') {
         break;
      }

      offset++;
   }

   if (!found)
      return false;

   // Look backwards for the matching bracket.

   int level = 0;

   while (offset > 0) {
      if ((*displayCode)[offset] == ')') {
         level++;
      } else if ((*displayCode)[offset] == '(') {
         level--;
         if (level == 0)
            break;
      }

      offset--;
   }

   if (level)
      return false;

   *start = *end = &(*displayCode)[offset];
   found         = false;
   offset--;

   // Look backwards for the start of the function name.
   // TODO Support function pointers.

   while (offset > 0) {
      char c = (*displayCode)[offset];

      if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == ' ' || (c >= '0' && c <= '9')) {
         // Part of the function name.
         offset--;
      } else {
         *start = &(*displayCode)[offset + 1];
         found  = true;
         break;
      }
   }

   return found;
}

UIMessage ReceiveMessageRegister(std::function<void(std::unique_ptr<std::string>)> callback) {
   receiveMessageTypes.push_back({.msg = msgReceivedNext, .callback = std::move(callback)});
   msgReceivedNext = (UIMessage)((uint32_t)msgReceivedNext + 1);
   return receiveMessageTypes.back().msg;
}

void Context::DebuggerThread() {
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

   posix_spawnp(&gdbPID, gdbPath, &actions, &attrs, gdbArgv, environ);

   posix_spawn_file_actions_destroy(&actions);
   posix_spawnattr_destroy(&attrs);
#endif

   pipeToGDB = inputPipe[1];

   SendToGdb(initialGDBCommand);

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
      int result = select(pipeFromGdb + 1, &readfds, NULL, NULL, &timeout);

      if (result == -1) { // "Error in select"
         std::cout << "error: " << errno << '\n';
         break;
      } else if (result == 0) { // timeout
         if (killGdbThread)
            break;
      } else if (FD_ISSET(pipeFromGdb, &readfds)) { // Data is available for reading
         char buffer[512 + 1];
         int  count = read(pipeFromGdb, buffer, 512);
         if (killGdbThread)
            break;
         if (count <= 0) {
            std::this_thread::sleep_for(std::chrono::microseconds{10000});
            continue;
         }
         buffer[count] = 0;

         // if `logWindow` is set, copy all received output from gdb there as soon
         // as we receive it, even it it is not complete.
         // ----------------------------------------------------------------------
         if (logWindow && !evaluateMode)
            windowMain->post_message(msgReceivedLog, new std::string(buffer));

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
         if (evaluateMode) {
            evaluateResultQueue.push(std::move(catBuffer));
            evaluateMode = false;
         } else {
            windowMain->post_message(msgReceivedData, new std::string(std::move(catBuffer)));
         }

         catBuffer = std::string{};
      }
   }

   return;
}

void DebuggerStartThread() {
   ctx.gdbThread = std::thread([]() { ctx.DebuggerThread(); });
}

// can be called by: SourceWindowUpdate -> EvaluateExpresion -> EvaluateCommand
// synchronous means we will wait for the debugger output
std::optional<std::string> DebuggerSend(string_view command, bool echo, bool synchronous) {
   std::optional<std::string> res;
   ctx.InterruptGdb();

   if (synchronous)
      ctx.evaluateMode = true;

   ctx.programRunning = true;

   if (trafficLight)
      trafficLight->repaint(nullptr);

   // std::cout << "sending: \"" << command << "\"\n";

   if (echo && displayOutput) {
      displayOutput->insert_content(command, false);
      displayOutput->refresh();
   }

   ctx.SendToGdb(command);

   if (synchronous) {
      bool quit = !ctx.evaluateResultQueue.pop(res);
      if (!res) {
         print("Hit timeout on command \"{}\"\n", command);
         res = std::string{}; // in synchronous mode we always return a (possibly empty) string
      } else {
         ctx.programRunning = false;
         if (!quit && trafficLight)
            trafficLight->repaint(nullptr);
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
      FILE* file = fopen(controlPipePath, "rb");
      auto  s    = new std::string;
      s->resize(256);
      (*s)[fread(s->data(), 1, 255, file)] = 0;
      windowMain->post_message(msgReceivedControl, s);
      fclose(file);
   }

   return nullptr;
}

void DebuggerGetStack() {
   auto res = EvaluateCommand(std::format("bt {}", backtraceCountLimit));
   if (res.empty())
      return;

   stack.clear();

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

      entry.id = strtoul(position + 1, (char**)&position, 0);

      while (*position == ' ' && position < next)
         position++;
      bool hasAddress = *position == '0';

      if (hasAddress) {
         entry.address = strtoul(position, (char**)&position, 0);
         position += 4;
      }

      while (*position == ' ' && position < next)
         position++;
      const char* functionName = position;
      position                 = strchr(position, ' ');
      if (!position || position >= next)
         break;
      std_format_to_n(entry.function, sizeof(entry.function), "{}",
                      std::string_view{functionName, (size_t)(position - functionName)});

      const char* file = strstr(position, " at ");

      if (file && file < next) {
         file += 4;
         const char* end = file;
         while (*end != '\n' && end < next)
            end++;
         std_format_to_n(entry.location, sizeof(entry.location), "{}", std::string_view{file, (size_t)(end - file)});
      }

      stack.push_back(entry);

      if (!(*next))
         break;
      position = next + 1;
   }
}

void DebuggerGetBreakpoints() {
   auto eval_res = EvaluateCommand("info break");
   breakpoints.clear();

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
      breakpoint.number     = number;
      breakpoint.enabled    = enabled;

      bool recognised = true;

      const char* condition = strstr(position, "stop only if ");

      if (condition && condition < next) {
         const char* end = strchr(condition, '\n');
         condition += 13;
         std_format_to_n(breakpoint.condition, sizeof(breakpoint.condition), "{}",
                         std::string_view{condition, (size_t)(end - condition)});
         breakpoint.conditionHash = Hash((const uint8_t*)condition, end - condition);
      }

      const char* hitCountNeedle = "breakpoint already hit";
      const char* hitCount       = strstr(position, hitCountNeedle);
      if (hitCount)
         hitCount += strlen(hitCountNeedle);

      if (hitCount && hitCount < next) {
         breakpoint.hit = sv_atoi(hitCount);
      }

      if (file && file < next) {
         const char* end = strchr(file, ':');

         if (end && isdigit(end[1])) {
            if (file[0] == '.' && file[1] == '/')
               file += 2;
            std_format_to_n(breakpoint.file, sizeof(breakpoint.file), "{}",
                            std::string_view{file, (size_t)(end - file)});
            breakpoint.line = sv_atoi(end, 1);
         } else
            recognised = false;
      } else
         recognised = false;

      if (recognised) {
         realpath(breakpoint.file, breakpoint.fileFull);

         for (auto& bp : breakpoints) {
            if (strcmp(bp.fileFull, breakpoint.fileFull) == 0 && bp.conditionHash == breakpoint.conditionHash &&
                bp.line == breakpoint.line) {
               bp.multiple = breakpoint.multiple = true;
               break;
            }
         }
         if (!breakpoint.multiple)
            breakpoints.push_back(std::move(breakpoint));
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
                  breakpoint.watchpoint = true;
                  snprintf(breakpoint.file, sizeof(breakpoint.file), "%.*s", (int)(end - address), address);
                  breakpoints.push_back(std::move(breakpoint));
               }
            }
         }
      }

      position = next;
   }
}

struct TabCompleter {
   bool _lastKeyWasTab;
   int  consecutiveTabCount;
   int  lastTabBytes;
};

void TabCompleterRun(TabCompleter* completer, UITextbox* textbox, bool lastKeyWasTab, bool addPrintPrefix) {
   auto text   = textbox->text();
   auto buffer = std::format("complete {}{}", addPrintPrefix ? "p " : "",
                             text.substr(0, lastKeyWasTab ? (size_t)completer->lastTabBytes : text.size()));
   for (int i = 0; buffer[i]; i++)
      if (buffer[i] == '\\')
         buffer[i] = ' ';
   auto res = EvaluateCommand(buffer);
   if (res.empty())
      return;

   const char* start = res.c_str();
   const char* end   = strchr(start, '\n');

   if (!lastKeyWasTab) {
      completer->consecutiveTabCount = 0;
      completer->lastTabBytes        = text.size();
   }

   while (start && end && memcmp(start + (addPrintPrefix ? 2 : 0), text.data(), completer->lastTabBytes)) {
      start = end + 1;
      end   = strchr(start, '\n');
   }

   for (int i = 0; end && i < completer->consecutiveTabCount; i++) {
      start = end + 1;
      end   = strchr(start, '\n');
   }

   if (!end) {
      completer->consecutiveTabCount = 0;
      start                          = res.c_str();
      end                            = strchr(start, '\n');
   }

   completer->_lastKeyWasTab = true;
   completer->consecutiveTabCount++;

   if (end) {
      if (addPrintPrefix)
         start += 2;
      textbox->clear(false);
      textbox->replace_text({start, static_cast<size_t>(end - start)}, false);
      textbox->refresh();
   }
}

// ------------------------------------------------------
// Commands:
// ------------------------------------------------------

std::optional<std::string> CommandParseInternal(string_view command, bool synchronous) {
   std::optional<std::string> res;
   if (command == "gf-step") {
      if (!ctx.programRunning)
         res = DebuggerSend(showingDisassembly ? "stepi" : "s", true, synchronous);
   } else if (command == "gf-next") {
      if (!ctx.programRunning)
         res = DebuggerSend(showingDisassembly ? "nexti" : "n", true, synchronous);
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
      ctx.firstUpdate = true;
      ctx.KillGdb();
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
            if (displayOutput) {
               displayOutput->insert_content(std::format("New working directory: {}", pwd), false);
               displayOutput->refresh();
            }
         }
         return {};
      }

      windowMain->show_dialog(0, "Couldn't get the working directory.\n%f%B", "OK");
   } else if (command.starts_with("gf-switch-to ")) {
      ctx.InterfaceWindowSwitchToAndFocus(command.substr(13));
   } else if (command.starts_with("gf-command ")) {
      for (const auto& cmd : presetCommands) {
         if (command.substr(11) == cmd.key)
            continue;
         char* copy     = strdup(cmd.value);
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
            if (displayOutput && res)
               displayOutput->insert_content(*res, false);
            if (end)
               position = end + 1;
            else
               break;
         }

         if (displayOutput)
            displayOutput->refresh();
         free(copy);
         break;
      }
   } else if (command == "gf-inspect-line") {
      CommandInspectLine();
   } else if (command == "target remote :1234" && confirmCommandConnect &&
              windowMain->show_dialog(0, "Connect to remote target?\n%f%B%C", "Connect", "Cancel") == "Cancel") {
   } else if (command == "kill" && confirmCommandKill &&
              windowMain->show_dialog(0, "Kill debugging target?\n%f%B%C", "Kill", "Cancel") == "Cancel") {
   } else {
      res = DebuggerSend(command, true, synchronous);
   }

   return res;
}

void CommandSendToGDB(string_view s) {
   (void)CommandParseInternal(s, false);
}

static void BreakpointCommand(int index, const char* action) {
   Breakpoint* breakpoint = &breakpoints[index];
   (void)DebuggerSend(std::format("{} {}", action, breakpoint->number), true, false);
}

static void CommandDeleteBreakpoint(int index) {
   BreakpointCommand(index, "delete");
}

static void CommandDisableBreakpoint(int index) {
   BreakpointCommand(index, "disable");
}

static void CommandEnableBreakpoint(int index) {
   BreakpointCommand(index, "enable");
}

bool CommandSyncWithGvim() {
   char buffer[1024];
   std_format_to_n(buffer, sizeof(buffer), "vim --servername {} --remote-expr \"execute(\\\"ls\\\")\" | grep %%",
                   vimServerName);
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
                      vimServerName);
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

void CommandToggleBreakpoint(int line = 0) {
   if (showingDisassembly) {
      // TODO.
      return;
   }

   if (!line) {
      auto currentLine = displayCode->current_line();
      if (!currentLine)
         return;
      line = *currentLine + 1; // gdb line numbers are 1-indexed
   }

   for (const auto& bp : breakpoints) {
      if (bp.line == line && 0 == strcmp(bp.fileFull, currentFileFull)) {
         (void)DebuggerSend(std::format("clear {}:{}", currentFile, line), true, false);
         return;
      }
   }

   (void)DebuggerSend(std::format("b {}:{}", currentFile, line), true, false);
}

void CommandCustom(string_view command) {

   if (command.starts_with("shell ")) {
      // TODO Move this into CommandParseInternal?

      if (displayOutput)
         displayOutput->insert_content(std::format("Running shell command \"{}\"...\n", command), false);
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

      if (displayOutput) {
         displayOutput->insert_content({copy.data(), j}, false);
         displayOutput->insert_content(
            std::format("(exit code: {}; time: {}s)\n", result, (int)(time(nullptr) - start)), false);
         displayOutput->refresh();
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

void SettingsAddTrustedFolder() {
   std::string config         = LoadFile(globalConfigPath);
   const char* section_string = "\n[trusted_folders]\n";
   auto        insert_pos     = config.find(section_string);

   if (insert_pos == std::string::npos) {
      config += section_string;
      insert_pos = config.size();
   } else {
      insert_pos += strlen(section_string);
   }

   std::ofstream ofs(globalConfigPath, std::ofstream::out | std::ofstream::binary);
   if (!ofs)
      print(std::cerr, "Error: Could not modify the global config file!\n");
   else {
      ofs << config.substr(0, insert_pos);
      ofs << localConfigDirectory << '\n';
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
//   + "shortcuts"
//   +  "gdb"
//   +  "theme"
// ------------------------------------------------------------------------------
UIConfig Context::SettingsLoad(bool earlyPass) {
   bool        currentFolderIsTrusted = false;
   static bool cwdConfigNotTrusted    = false;
   UIConfig    ui_config;

   // load global config first (from ~/.config/gf2_config.ini), and then local config
   for (int i = 0; i < 2; i++) {
      INIState state;
      auto     config = LoadFile(i ? localConfigPath : globalConfigPath);
      state.bytes     = config.size();
      state.buffer    = config[0] ? (char*)config.c_str() : nullptr;

      if (earlyPass && i && !currentFolderIsTrusted && state.buffer) {
         print(std::cerr, "Would you like to load the config file .project.gf from your current directory?\n");
         print(std::cerr, "You have not loaded this config file before.\n");
         print(std::cerr, "(Y) - Yes, and add it to the list of trusted files\n");
         print(std::cerr, "(N) - No\n");
         char c = 'n';
         fread(&c, 1, 1, stdin);

         if (c != 'y') {
            cwdConfigNotTrusted = true;
            break;
         } else {
            SettingsAddTrustedFolder();
         }
      } else if (!earlyPass && cwdConfigNotTrusted && i) {
         break;
      }

      while (INIParse(&state)) {
         if (0 == strcmp(state.section, "shortcuts") && *state.key && !earlyPass) {
            UIShortcut shortcut;

            for (int i = 0; state.key[i]; i++) {
               state.key[i] = tolower(state.key[i]);
            }

            shortcut.ctrl   = strstr(state.key, "ctrl+");
            shortcut.shift  = strstr(state.key, "shift+");
            shortcut.alt    = strstr(state.key, "alt+");
            shortcut.invoke = [cmd = state.value]() {
               CommandCustom(cmd);
               return true;
            };

            const char* codeStart = state.key;

            for (int i = 0; state.key[i]; i++) {
               if (state.key[i] == '+') {
                  codeStart = state.key + i + 1;
               }
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
               print(std::cerr, "Warning: Could not register shortcut for '{}'.\n", state.key);
            } else {
               windowMain->register_shortcut(std::move(shortcut));
            }
         } else if (0 == strcmp(state.section, "ui") && earlyPass) {
            if (0 == strcmp(state.key, "font_path")) {
               ui_config.font_path = state.value;
            } else if (0 == strcmp(state.key, "font_size")) {
               interface_font_size = code_font_size = sv_atoi(state.value);
            } else if (0 == strcmp(state.key, "font_size_code")) {
               code_font_size = sv_atoi(state.value);
            } else if (0 == strcmp(state.key, "font_size_interface")) {
               interface_font_size = sv_atoi(state.value);
            } else if (0 == strcmp(state.key, "scale")) {
               ui_scale = atof(state.value);
            } else if (0 == strcmp(state.key, "layout")) {
               gfc.layout_string = strdup(state.value);
            } else if (0 == strcmp(state.key, "maximize")) {
               maximize = sv_atoi(state.value);
            } else if (0 == strcmp(state.key, "restore_watch_window")) {
               restoreWatchWindow = sv_atoi(state.value);
            } else if (0 == strcmp(state.key, "selectable_source")) {
               selectableSource = sv_atoi(state.value);
            } else if (0 == strcmp(state.key, "window_width")) {
               window_width = sv_atoi(state.value);
            } else if (0 == strcmp(state.key, "window_height")) {
               window_height = sv_atoi(state.value);
            }
         } else if (0 == strcmp(state.section, "gdb") && !earlyPass) {
            if (0 == strcmp(state.key, "argument")) {
               ctx.gdbArgc++;
               ctx.gdbArgv                  = (char**)realloc(ctx.gdbArgv, sizeof(char*) * (ctx.gdbArgc + 1));
               ctx.gdbArgv[ctx.gdbArgc - 1] = strdup(state.value);
               ctx.gdbArgv[ctx.gdbArgc]     = nullptr;
            } else if (0 == strcmp(state.key, "arguments")) {
               char buffer[2048];

               for (size_t i = 0; i < state.valueBytes; i++) {
                  if (isspace(state.value[i])) {
                     continue;
                  }

                  size_t argumentStart = 0;
                  size_t argumentEnd   = 0;

                  if (state.value[i] == '\"') {
                     i++;
                     argumentStart = i;
                     for (; i < state.valueBytes && state.value[i] != '\"'; i++)
                        ;
                     argumentEnd = i;
                     i++;
                  } else if (state.value[i] == '\'') {
                     i++;
                     argumentStart = i;
                     for (; i < state.valueBytes && state.value[i] != '\''; i++)
                        ;
                     argumentEnd = i;
                     i++;
                  } else {
                     argumentStart = i;
                     i++;
                     for (; i < state.valueBytes &&
                            (state.value[i] != '\'' && state.value[i] != '\"' && !isspace(state.value[i]));
                          i++)
                        ;
                     argumentEnd = i;
                  }

                  std_format_to_n(buffer, sizeof(buffer), "{}",
                                  std::string_view{&state.value[argumentStart], (size_t)(argumentEnd - argumentStart)});

                  ctx.gdbArgc++; // 0 is for the program name
                  ctx.gdbArgv                  = (char**)realloc(ctx.gdbArgv, sizeof(char*) * (ctx.gdbArgc + 1));
                  ctx.gdbArgv[ctx.gdbArgc - 1] = strdup(buffer);
                  ctx.gdbArgv[ctx.gdbArgc]     = nullptr;
               }
            } else if (0 == strcmp(state.key, "path")) {
               char* path     = strdup(state.value);
               ctx.gdbPath    = path;
               ctx.gdbArgv[0] = path;
            } else if (0 == strcmp(state.key, "log_all_output") && sv_atoi(state.value)) {
               if (auto it = interfaceWindows.find("Log"); it != interfaceWindows.end()) {
                  const auto& [name, window] = *it;
                  ctx.logWindow              = static_cast<UICode*>(window.el);
               }
               if (!ctx.logWindow) {
                  print(std::cerr, "Warning: gdb.log_all_output was enabled, "
                                   "but your layout does not have a 'Log' window.\n");
               }
            } else if (0 == strcmp(state.key, "confirm_command_kill")) {
               confirmCommandKill = sv_atoi(state.value);
            } else if (0 == strcmp(state.key, "confirm_command_connect")) {
               confirmCommandConnect = sv_atoi(state.value);
            } else if (0 == strcmp(state.key, "backtrace_count_limit")) {
               backtraceCountLimit = sv_atoi(state.value);
            }
         } else if (0 == strcmp(state.section, "commands") && earlyPass && state.keyBytes && state.valueBytes) {
            presetCommands.push_back(state.clone());
         } else if (0 == strcmp(state.section, "trusted_folders") && earlyPass && state.keyBytes) {
            if (0 == strcmp(localConfigDirectory, state.key))
               currentFolderIsTrusted = true;
         } else if (0 == strcmp(state.section, "theme") && !earlyPass && state.keyBytes && state.valueBytes) {
            for (uintptr_t i = 0; i < sizeof(themeItems) / sizeof(themeItems[0]); i++) {
               if (strcmp(state.key, themeItems[i]))
                  continue;
               ((uint32_t*)&ui_config._theme)[i] = strtoul(state.value, nullptr, 16);
               ui_config._has_theme              = true;
            }
         } else if (0 == strcmp(state.section, "vim") && earlyPass && 0 == strcmp(state.key, "server_name")) {
            vimServerName = strdup(state.value);
         } else if (0 == strcmp(state.section, "pipe") && earlyPass && 0 == strcmp(state.key, "log")) {
            logPipePath = strdup(state.value);
            mkfifo(logPipePath, 6 + 6 * 8 + 6 * 64);
         } else if (0 == strcmp(state.section, "pipe") && earlyPass && 0 == strcmp(state.key, "control")) {
            controlPipePath = strdup(state.value);
            mkfifo(controlPipePath, 6 + 6 * 8 + 6 * 64);
            pthread_t thread;
            pthread_create(&thread, nullptr, ControlPipeThread, nullptr);
         } else if (0 == strcmp(state.section, "executable") && earlyPass) {
            if (0 == strcmp(state.key, "path")) {
               gfc.exe_path = state.value;
            } else if (0 == strcmp(state.key, "arguments")) {
               gfc.exe_args = state.value;
            } else if (0 == strcmp(state.key, "ask_directory")) {
               gfc.exe_ask_dir = sv_atoi(state.value);
            }
         } else if (earlyPass && *state.section && *state.key && *state.value) {
            if (auto it = interfaceWindows.find(state.section); it != interfaceWindows.end()) {
               const auto& [name, window] = *it;
               if (window.config) {
                  window.config(state.key, state.value);
               }
            }
         }
      }
      state.clear(); // get rid of the pointers into the readfile buffer
   }
   return ui_config;
}

// ------------------------------------------------------
// Debug windows:
// ------------------------------------------------------

// ---------------------------------------------------
// Source display:
// ---------------------------------------------------

char autoPrintExpression[1024];
char autoPrintResult[1024];
int  autoPrintExpressionLine;
int  autoPrintResultLine;

int currentEndOfBlock;
int lastCursorX, lastCursorY;

int ifConditionEvaluation, ifConditionLine;
int ifConditionFrom, ifConditionTo;

struct InspectResult {
   std::string expression;
   std::string value;
};

vector<InspectResult> inspectResults;
bool                  noInspectResults;
bool                  inInspectLineMode;
int                   inspectModeRestoreLine;
UIRectangle           displayCurrentLineBounds;
const char*           disassemblyCommand = "disas /s";

// --------------------------------
// `line`, if present, is `0-based`
// --------------------------------
bool DisplaySetPosition(const char* file, std::optional<size_t> line, bool useGDBToGetFullPath) {
   if (showingDisassembly) {
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
      if (strcmp(currentFile, file)) {
         reloadFile = true;
      }

      struct stat buf;

      if (!stat(file, &buf) && buf.st_mtime != currentFileReadTime) {
         reloadFile = true;
      }

      currentFileReadTime = buf.st_mtime;
   }

   bool changed = false;

   if (reloadFile) {
      std_format_to_n(currentFile, 4096, "{}", file);
      realpath(currentFile, currentFileFull);

      windowMain->set_name(currentFileFull);

      displayCode->load_file(file, std::format("The file '{}' (from '{}') could not be loaded.", file, originalFile));

      changed            = true;
      autoPrintResult[0] = 0;
   }

   auto currentLine = displayCode->current_line();
   if (line && (!currentLine || currentLine != line)) {
      displayCode->set_current_line(*line);
      displayCode->set_focus_line(*line);
      changed = true;
   }

   currentEndOfBlock = SourceFindEndOfBlock();
   displayCode->refresh();

   return changed;
}

void DisplaySetPositionFromStack() {
   if (stackSelected < stack.size()) {
      char location[sizeof(previousLocation)];
      strcpy(previousLocation, stack[stackSelected].location);
      strcpy(location, stack[stackSelected].location);
      char*                 line = strchr(location, ':');
      std::optional<size_t> position;
      if (line) {
         *line    = 0;
         position = sv_atoul(line + 1) - 1; // lines in gdb are 1-based
      }
      DisplaySetPosition(location, position, true);
   }
}

void DisassemblyLoad() {
   auto res = EvaluateCommand(disassemblyCommand);

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

   displayCode->insert_content({start, static_cast<size_t>(end - start)}, true);
}

void DisassemblyUpdateLine() {
   auto        res     = EvaluateCommand("p $pc");
   const char* address = strstr(res.c_str(), "0x");

   if (address) {
      uint64_t a = strtoul(address, nullptr, 0);

      for (int i = 0; i < 2; i++) {
         // Look for the line in the disassembly.

         bool found = false;

         size_t num_lines = displayCode->num_lines();
         for (size_t i = 0; i < num_lines; i++) {
            uint64_t b = sv_atoul(displayCode->line(i), 3);

            if (a == b) {
               displayCode->set_focus_line(i);
               autoPrintExpressionLine = i;
               found                   = true;
               break;
            }
         }

         if (!found) {
            // Reload the disassembly.
            DisassemblyLoad();
         } else {
            break;
         }
      }

      displayCode->refresh();
   }
}

bool CommandToggleDisassembly() {
   showingDisassembly     = !showingDisassembly;
   autoPrintResultLine    = 0;
   autoPrintExpression[0] = 0;
   displayCode->_flags ^= UICode::NO_MARGIN;

   if (showingDisassembly) {
      displayCode->insert_content("Disassembly could not be loaded.\nPress Ctrl+D to return to source view.", true);
      displayCode->set_tab_columns(8);
      DisassemblyLoad();
      DisassemblyUpdateLine();
   } else {
      displayCode->set_current_line({});
      currentEndOfBlock   = -1;
      currentFile[0]      = 0;
      currentFileReadTime = 0;
      DisplaySetPositionFromStack();
      displayCode->set_tab_columns(4);
   }

   displayCode->refresh();
   return true;
}

bool CommandSetDisassemblyMode() {
   auto newMode = windowMain->show_dialog(0, "Select the disassembly mode:\n%b\n%b\n%b", "Disassembly only",
                                          "With source", "Source centric");

   if (newMode == "Disassembly only")
      disassemblyCommand = "disas";
   else if (newMode == "With source")
      disassemblyCommand = "disas /s";
   else if (newMode == "Source centric")
      disassemblyCommand = "disas /m";

   if (showingDisassembly) {
      CommandToggleDisassembly();
      CommandToggleDisassembly();
   }
   return true;
}

void DisplayCodeDrawInspectLineModeOverlay(UIPainter* painter) {
   auto& theme       = painter->theme();
   auto  active_font = painter->active_font();

   const char* instructions = "(Press Esc to exit inspect line mode.)";
   int         width        = (strlen(instructions) + 8) * active_font->_glyph_width;

   for (const auto& ir : inspectResults) {
      int w = (ir.expression.size() + ir.value.size() + 8) * active_font->_glyph_width;
      if (w > width)
         width = w;
   }

   int  xOffset     = 0;
   auto currentLine = displayCode->current_line();
   if (!currentLine)
      return;

   std::string_view cur_line = displayCode->line(*currentLine);
   for (auto c : cur_line) {
      if (c == '\t' || c == ' ') {
         xOffset += (c == '\t' ? 4 : 1) * active_font->_glyph_width;
      } else {
         break;
      }
   }

   int         lineHeight = painter->ui()->string_height();
   UIRectangle bounds =
      displayCurrentLineBounds + UIRectangle(xOffset, 0, lineHeight, 8 + lineHeight * (inspectResults.size() / 2 + 1));
   bounds.r = bounds.l + width;
   painter->draw_block(bounds + UIRectangle(3), theme.border);
   painter->draw_rectangle(bounds, theme.codeBackground, theme.border, UIRectangle(2));
   UIRectangle line = bounds + UIRectangle(4, -4, 4, 0);
   line.b           = line.t + lineHeight;
   std::string buffer;

   size_t index = 0;
   for (const auto& ir : inspectResults) {
      if (noInspectResults) {
         buffer = ir.expression;
      } else if (index < 9) {
         buffer = std::format("[{}] {} {}", index + 1, ir.expression, ir.value);
      } else {
         buffer = std::format("    {} {}", ir.expression, ir.value);
      }

      painter->draw_string(line, buffer, noInspectResults ? theme.codeOperator : theme.codeString, UIAlign::left, NULL);
      line = line + UIRectangle(0, lineHeight);
      ++index;
   }

   painter->draw_string(line, instructions, theme.codeNumber, UIAlign::right, NULL);
}

template <class F>
void for_all_breakpoints_on_line(int line, F&& f) {
   for (size_t i = 0; i < breakpoints.size(); i++) {
      if (breakpoints[i].line == line && 0 == strcmp(breakpoints[i].fileFull, currentFileFull)) {
         std::forward<F>(f)(i);
      }
   }
}

void CommandDeleteAllBreakpointsOnLine(int line) {
   for_all_breakpoints_on_line(line, [](int line) { CommandDeleteBreakpoint(line); });
}

void CommandDisableAllBreakpointsOnLine(int line) {
   for_all_breakpoints_on_line(line, [](int line) { CommandDisableBreakpoint(line); });
}

void CommandEnableAllBreakpointsOnLine(int line) {
   for_all_breakpoints_on_line(line, [](int line) { CommandEnableBreakpoint(line); });
}

int DisplayCodeMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   UICode* code = (UICode*)el;

   if (msg == UIMessage::CLICKED && !showingDisassembly) {
      int result = code->hittest(el->_window->cursor_pos());

      if (result < 0 && code->left_down_in_margin()) {
         int line = -result;
         CommandToggleBreakpoint(line);
      } else if (result > 0 && !code->left_down_in_margin()) {
         int line = result;

         if (el->_window->_ctrl) {
            (void)DebuggerSend(std::format("until {}", line), true, false);
         } else if (el->_window->_alt || el->_window->_shift) {
            EvaluateCommand(std::format("tbreak {}", line));
            (void)DebuggerSend(std::format("jump {}", line), true, false);
         }
      }
   } else if (msg == UIMessage::RIGHT_DOWN && !showingDisassembly) {
      int result = code->hittest(el->cursor_pos());

      bool atLeastOneBreakpointEnabled = false;

      for (const auto& bp : breakpoints) {
         if (bp.line == -result && 0 == strcmp(bp.fileFull, currentFileFull) && bp.enabled) {
            atLeastOneBreakpointEnabled = true;
            break;
         }
      }

      for (const auto& bp : breakpoints) {
         if (bp.line == -result && 0 == strcmp(bp.fileFull, currentFileFull)) {
            UIMenu& menu = el->ui()->create_menu(el->_window, UIMenu::NO_SCROLL).add_item(0, "Delete", [=](UIButton&) {
               CommandDeleteAllBreakpointsOnLine(-result);
            });
            if (atLeastOneBreakpointEnabled)
               menu.add_item(0, "Disable", [=](UIButton&) { CommandDisableAllBreakpointsOnLine(-result); });
            else
               menu.add_item(0, "Enable", [=](UIButton&) { CommandEnableAllBreakpointsOnLine(-result); });
            menu.show();
         }
      }
   } else if (msg == UIMessage::CODE_GET_MARGIN_COLOR && !showingDisassembly) {
      auto& theme                        = el->theme();
      bool  atLeastOneBreakpointDisabled = false;

      for (const auto& bp : breakpoints) {
         if (bp.line == di && 0 == strcmp(bp.fileFull, currentFileFull)) {
            if (bp.enabled)
               return theme.accent1;
            else
               atLeastOneBreakpointDisabled = true;
         }
      }

      if (atLeastOneBreakpointDisabled) {
         return (((theme.accent1 & 0xFF0000) >> 1) & 0xFF0000) | (((theme.accent1 & 0xFF00) >> 1) & 0xFF00) |
                ((theme.accent1 & 0xFF) >> 1);
      }
   } else if (msg == UIMessage::PAINT) {
      el->_class_proc(el, msg, di, dp);

      if (inInspectLineMode) {
         UIFont* previousFont = code->font()->activate();
         DisplayCodeDrawInspectLineModeOverlay((UIPainter*)dp);
         previousFont->activate();
      }

      return 1;
   } else if (msg == UIMessage::CODE_DECORATE_LINE) {
      auto&               theme       = el->theme();
      auto                active_font = el->active_font();
      UICodeDecorateLine* m           = (UICodeDecorateLine*)dp;
      auto                currentLine = displayCode->current_line();

      if (currentLine && m->index == (int)*currentLine) {
         displayCurrentLineBounds = m->bounds;
      }

      if (m->index == autoPrintResultLine) {
         UIRectangle rectangle =
            UIRectangle(m->x + active_font->_glyph_width, m->bounds.r, m->y, m->y + el->ui()->string_height());
         m->painter->draw_string(rectangle, autoPrintResult, theme.codeComment, UIAlign::left, NULL);
      }

      if (code->hittest(el->cursor_pos()) == m->index && el->is_hovered() &&
          (el->_window->_ctrl || el->_window->_alt || el->_window->_shift) && !el->_window->textbox_modified_flag()) {
         m->painter->draw_border(m->bounds, el->_window->_ctrl ? theme.selected : theme.codeOperator, UIRectangle(2));
         m->painter->draw_string(m->bounds, el->_window->_ctrl ? "=> run until " : "=> skip to ", theme.text,
                                 UIAlign::right, NULL);
      } else if (m->index == currentEndOfBlock) {
         m->painter->draw_string(m->bounds, "[Shift+F10]", theme.codeComment, UIAlign::right, NULL);
      }

      if (m->index == ifConditionLine && ifConditionEvaluation) {
         int columnFrom = code->byte_to_column(ifConditionLine, ifConditionFrom);
         int columnTo   = code->byte_to_column(ifConditionLine, ifConditionTo);
         m->painter->draw_block(UIRectangle(m->bounds.l + columnFrom * active_font->_glyph_width,
                                            m->bounds.l + columnTo * active_font->_glyph_width, m->bounds.b - 2,
                                            m->bounds.b),
                                ifConditionEvaluation == 2 ? theme.accent2 : theme.accent1);
      }
   } else if (msg == UIMessage::MOUSE_MOVE || msg == UIMessage::UPDATE) {
      auto pos = el->cursor_pos();
      if (pos.x != lastCursorX || pos.y != lastCursorY) {
         lastCursorX = pos.x;
         lastCursorY = pos.y;
         el->_window->set_textbox_modified_flag(false);
      }

      el->refresh();
   }

   return 0;
}

UIElement* SourceWindowCreate(UIElement* parent) {
   displayCode = &parent->add_code(selectableSource ? UICode::SELECTABLE : 0)
                     .set_font(code_font)
                     .set_user_proc(DisplayCodeMessage);
   return displayCode;
}

void SourceWindowUpdate(const char* data, UIElement* el) {
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

   if (!stackChanged && changedSourceLine)
      stackSelected = 0;
   stackChanged = false;

   if (changedSourceLine && stackSelected < stack.size() && strcmp(stack[stackSelected].location, previousLocation)) {
      DisplaySetPositionFromStack();
   }

   auto currentLine = displayCode->current_line();
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
      std::string_view text_sv  = displayCode->line(*currentLine);
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
         std_format_to_n(autoPrintExpression, sizeof(autoPrintExpression), "{}",
                         std::string_view{text + expressionStart, expressionEnd - expressionStart});
      }

      autoPrintExpressionLine = *currentLine + 1;

      // Try to evaluate simple if conditions.

      ifConditionEvaluation = 0;

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

bool InspectIsTokenCharacter(char c) {
   return isalpha(c) || c == '_';
}

void InspectCurrentLine() {
   inspectResults.clear();
   auto currentLine = displayCode->current_line();
   if (!currentLine)
      return;

   auto code = displayCode->line(*currentLine);

   auto expressions = regex::extract_debuggable_expressions(code);
   for (auto e : expressions) {
      auto res = EvaluateExpression(e);
      // std::cout << "eval(\"" << e << "\") -> " << res << '\n';

      if (ctre::starts_with<"(A syntax error|No symbol|Attempt to|cannot resolve)">(res))
         continue;

      if (0 == memcmp(res.c_str(), "= {", 3) && !strchr(res.c_str() + 3, '='))
         continue;

      inspectResults.emplace_back(std::string{e}, res);
   }

   if (!inspectResults.size()) {
      inspectResults.emplace_back("No expressions to display.", "");
   } else {
      noInspectResults = false;
   }
}

void InspectLineModeExit(UIElement* el) {
   el->destroy();
   textboxInput->focus();
   inInspectLineMode = false;
   displayCode->set_current_line(inspectModeRestoreLine);
   displayCode->set_focus_line(inspectModeRestoreLine);
   displayCode->refresh();
}

int InspectLineModeMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::UPDATE && !el->is_focused()) {
      InspectLineModeExit(el);
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if ((m->text == "`") || m->code == UIKeycode::ESCAPE) {
         InspectLineModeExit(el);
      } else if (m->code >= UI_KEYCODE_DIGIT('1') && m->code <= UI_KEYCODE_DIGIT('9')) {
         int index = ((int)m->code - (int)UI_KEYCODE_DIGIT('1'));

         if (index < (int)inspectResults.size()) {
            InspectLineModeExit(el);
            WatchAddExpression2(inspectResults[index].expression);
         }
      } else {
         auto currentLine = displayCode->current_line();
         if (!currentLine)
            return 0;
         if ((m->code == UIKeycode::UP && *currentLine != 0) ||
             (m->code == UIKeycode::DOWN && *currentLine + 1 < displayCode->num_lines())) {
            *currentLine += m->code == UIKeycode::UP ? -1 : 1;
            displayCode->set_current_line(*currentLine);
            displayCode->set_focus_line(*currentLine);
            InspectCurrentLine();
            displayCode->refresh();
         }
      }

      return 1;
   }

   return 0;
}

bool CommandInspectLine() {
   auto currentLine = displayCode->current_line();
   if (!currentLine)
      return false;

   inspectModeRestoreLine = *currentLine;
   inInspectLineMode      = true;
   InspectCurrentLine();
   displayCode->refresh();

   // Create an element to receive key input messages.
   windowMain->add_element(0, InspectLineModeMessage, 0).focus();
   return true;
}

// ---------------------------------------------------/
// Data viewers:
// ---------------------------------------------------/

struct AutoUpdateViewer {
   UIElement* el;
   void (*callback)(UIElement*);
};

vector<AutoUpdateViewer> autoUpdateViewers;
bool                     autoUpdateViewersQueued;

bool DataViewerRemoveFromAutoUpdateList(UIElement* el) {
   if (auto it = rng::find_if(autoUpdateViewers, [&](const auto& auv) { return auv.el == el; });
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
         AutoUpdateViewer v = {.el = el->_parent, .callback = (void (*)(UIElement*))el->_cp};
         autoUpdateViewers.push_back(v);
      } else {
         [[maybe_unused]] bool found = DataViewerRemoveFromAutoUpdateList(el->_parent);
         assert(found);
      }
   }

   return 0;
}

void DataViewersUpdateAll() {
   if (~dataTab->_flags & UIElement::hide_flag) {
      for (const auto& auv : autoUpdateViewers) {
         auv.callback(auv.el);
      }
   } else if (!autoUpdateViewers.empty()) {
      autoUpdateViewersQueued = true;
   }
}

// ---------------------------------------------------/
// Bitmap viewer:
// ---------------------------------------------------/

struct BitmapViewer {
   std::string     pointer;
   std::string     width;
   std::string     height;
   std::string     stride;
   int             parsedWidth  = 0;
   int             parsedHeight = 0;
   UIButton*       autoToggle   = nullptr;
   UIImageDisplay* display      = nullptr;
   UIPanel*        labelPanel   = nullptr;
   UILabel*        label        = nullptr;
};

int BitmapViewerWindowMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   BitmapViewer* viewer = (BitmapViewer*)el->_cp;
   if (msg == UIMessage::DESTROY) {
      DataViewerRemoveFromAutoUpdateList(el);
      delete viewer;
      el->_cp = nullptr;
   } else if (msg == UIMessage::GET_WIDTH) {
      int fit = viewer->parsedWidth + 40;
      return fit > 300 ? fit : 300;
   } else if (msg == UIMessage::GET_HEIGHT) {
      int fit = viewer->parsedHeight + 40;
      return fit > 100 ? fit : 100;
   }

   return 0;
}

void BitmapViewerUpdate(std::string pointerString, std::string widthString, std::string heightString,
                        std::string strideString, UIElement* owner = nullptr);

int BitmapViewerRefreshMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::CLICKED) {
      BitmapViewer* bitmap = (BitmapViewer*)el->_parent->_cp;
      BitmapViewerUpdate(bitmap->pointer, bitmap->width, bitmap->height, bitmap->stride, el->_parent);
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
                      static char* path = NULL;
                      auto         result =
                         windowMain->show_dialog(0, "Save to file       \nPath:\n%t\n%f%B%C", &path, "Save", "Cancel");
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
   BitmapViewerUpdate(bitmap->pointer, bitmap->width, bitmap->height, bitmap->stride, el);
}

void BitmapViewerUpdate(std::string pointerString, std::string widthString, std::string heightString,
                        std::string strideString, UIElement* owner) {
   uint32_t*   bits  = nullptr;
   int         width = 0, height = 0, stride = 0;
   const char* error =
      BitmapViewerGetBits(pointerString, widthString, heightString, strideString, &bits, &width, &height, &stride);

   if (!owner) {
      BitmapViewer* bitmap = new BitmapViewer;

      bitmap->pointer = std::move(pointerString);
      bitmap->width   = std::move(widthString);
      bitmap->height  = std::move(heightString);
      bitmap->stride  = std::move(strideString);

      UIMDIChild* window = &dataWindow->add_mdichild(UIMDIChild::CLOSE_BUTTON, UIRectangle(0), "Bitmap")
                               .set_user_proc(BitmapViewerWindowMessage)
                               .set_cp(bitmap);
      bitmap->autoToggle = &window->add_button(UIButton::SMALL | UIElement::non_client_flag, "Auto")
                               .set_cp((void*)BitmapViewerAutoUpdateCallback)
                               .set_user_proc(DataViewerAutoUpdateButtonMessage);
      window->add_button(UIButton::SMALL | UIElement::non_client_flag, "Refresh")
         .set_user_proc(BitmapViewerRefreshMessage);
      owner = window;

      UIPanel* panel = &owner->add_panel(UIPanel::EXPAND);
      bitmap->display =
         &panel->add_imagedisplay(UIImageDisplay::INTERACTIVE | UIElement::v_fill, bits, width, height, stride)
             .set_user_proc(BitmapViewerDisplayMessage);
      bitmap->labelPanel = &panel->add_panel(UIPanel::COLOR_1 | UIElement::v_fill);
      bitmap->label      = &bitmap->labelPanel->add_label(UIElement::h_fill, {});
   }

   BitmapViewer* bitmap = (BitmapViewer*)owner->_cp;
   bitmap->parsedWidth = width, bitmap->parsedHeight = height;
   bitmap->display->set_content(bits, width, height, stride);
   if (error)
      bitmap->label->set_label(error);
   if (error)
      bitmap->labelPanel->_flags &= ~UIElement::hide_flag, bitmap->display->_flags |= UIElement::hide_flag;
   else
      bitmap->labelPanel->_flags |= UIElement::hide_flag, bitmap->display->_flags &= ~UIElement::hide_flag;
   bitmap->labelPanel->_parent->refresh();
   owner->refresh();
   dataWindow->refresh();

   free(bits);
}

void BitmapAddDialog() {
   static char *pointer = nullptr, *width = nullptr, *height = nullptr, *stride = nullptr;

   auto result = windowMain->show_dialog(0,
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

vector<unique_ptr<char[]>> commandHistory;
size_t                     commandHistoryIndex;

bool CommandPreviousCommand() {
   if (commandHistoryIndex < commandHistory.size()) {
      textboxInput->clear(false);
      textboxInput->replace_text(commandHistory[commandHistoryIndex].get(), false);
      if (commandHistoryIndex < commandHistory.size() - 1)
         commandHistoryIndex++;
      textboxInput->refresh();
   }
   return true;
}

bool CommandNextCommand() {
   textboxInput->clear(false);

   if (commandHistoryIndex > 0) {
      commandHistoryIndex--;
      textboxInput->replace_text(commandHistory[commandHistoryIndex].get(), false);
   }

   textboxInput->refresh();
   return true;
}

bool CommandClearOutput() {
   displayOutput->clear();
   displayOutput->refresh();
   return true;
}

int TextboxInputMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   UITextbox* textbox = (UITextbox*)el;

   if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      static TabCompleter tabCompleter  = {};
      bool                lastKeyWasTab = tabCompleter._lastKeyWasTab;
      tabCompleter._lastKeyWasTab       = false;

      std::string_view text = textbox->text();
      auto             sz   = text.size();

      if (m->text.size() && !el->_window->_ctrl && !el->_window->_alt && m->text[0] == '`' && !sz) {
         textbox->set_reject_next_key(true);
      } else if (m->code == UIKeycode::ENTER && !el->_window->_shift) {
         if (!sz) {
            if (commandHistory.size()) {
               CommandSendToGDB(commandHistory[0].get());
            }

            return 1;
         }

         auto buffer = std::format("{}", textbox->text());
         if (commandLog)
            print(commandLog, "{}\n", buffer);
         CommandSendToGDB(buffer);


         unique_ptr<char[]> string = std::make_unique<char[]>(sz + 1);
         memcpy(string.get(), text.data(), sz);
         string[sz] = 0;
         commandHistory.insert(commandHistory.cbegin(), std::move(string));
         commandHistoryIndex = 0;

         if (commandHistory.size() > 500) {
            commandHistory.pop_back();
         }

         textbox->clear(false);
         textbox->refresh();

         return 1;
      } else if (m->code == UIKeycode::TAB && sz && !el->_window->_shift) {
         TabCompleterRun(&tabCompleter, textbox, lastKeyWasTab, false);
         return 1;
      } else if (m->code == UIKeycode::UP) {
         auto currentLine = displayCode->current_line();
         if (el->_window->_shift) {
            if (currentLine && *currentLine > 0) {
               DisplaySetPosition(NULL, *currentLine - 1, false);
            }
         } else {
            CommandPreviousCommand();
         }
      } else if (m->code == UIKeycode::DOWN) {
         auto currentLine = displayCode->current_line();
         if (el->_window->_shift) {
            if (currentLine && *currentLine + 1 < displayCode->num_lines()) {
               DisplaySetPosition(NULL, *currentLine + 1, false);
            }
         } else {
            CommandNextCommand();
         }
      }
   }

   return 0;
}

UIElement* ConsoleWindowCreate(UIElement* parent) {
   UIPanel* panel2 = &parent->add_panel(UIPanel::EXPAND);
   displayOutput   = &panel2->add_code(UICode::NO_MARGIN | UIElement::v_fill | UICode::SELECTABLE);
   UIPanel* panel3 = &panel2->add_panel(UIPanel::HORIZONTAL | UIPanel::EXPAND | UIPanel::COLOR_1)
                         .set_border(UIRectangle(5))
                         .set_gap(5);
   trafficLight = &panel3->add_spacer(0, 30, 30).set_user_proc(TrafficLightMessage);
   panel3->add_button(0, "Menu").on_click([](UIButton& buttonMenu) { ctx.InterfaceShowMenu(&buttonMenu); });
   textboxInput = &panel3->add_textbox(UIElement::h_fill).set_user_proc(TextboxInputMessage).focus();
   return panel2;
}

// ---------------------------------------------------/
// Watch window:
// ---------------------------------------------------/
struct Watch;
using WatchVector = vector<shared_ptr<Watch>>;

struct Watch {
   bool        open = false, hasFields = false, loadedFields = false, isArray = false, isDynamicArray = false;
   uint8_t     depth      = 0;
   char        format     = 0;
   uintptr_t   arrayIndex = 0;
   std::string key;
   std::string value;
   std::string type;
   WatchVector fields;
   Watch*      parent      = nullptr;
   uint64_t    updateIndex = 0;

   static constexpr int WATCH_ARRAY_MAX_FIELDS = 10000000;
};

enum WatchWindowMode {
   WATCH_NORMAL,
   WATCH_LOCALS,
};

struct WatchWindow : public UIElement {
   WatchVector     rows;
   WatchVector     baseExpressions;
   WatchVector     dynamicArrays;
   UITextbox*      textbox;
   std::string     lastLocalList;
   size_t          selectedRow;
   int             extraRows;
   WatchWindowMode mode;
   uint64_t        updateIndex;
   bool            waitingForFormatCharacter;

   WatchWindow(UIElement* parent, uint32_t flags, const char* name);
};

struct WatchLogEvaluated {
   std::string result;
};

struct WatchLogEntry {
   std::string               value;
   std::string               where;
   vector<WatchLogEvaluated> evaluated;
   vector<StackEntry>        trace;
};

struct WatchLogger {
   int                   id            = 0;
   int                   selectedEntry = 0;
   char                  columns[256]  = {0};
   std::string           expressionsToEvaluate;
   vector<WatchLogEntry> entries;
   UITable*              table = nullptr;
   UITable*              trace = nullptr;
};

vector<WatchLogger*> watchLoggers;

size_t WatchLastRow(WatchWindow* w) {
   size_t res = w->rows.size() + w->extraRows;
   return res ? res - 1 : res;
}

int WatchTextboxMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::UPDATE) {
      if (!el->is_focused()) {
         el->destroy();
         ((WatchWindow*)el->_cp)->textbox = nullptr;
      }
   } else if (msg == UIMessage::KEY_TYPED) {
      UITextbox*  textbox = (UITextbox*)el;
      UIKeyTyped* m       = (UIKeyTyped*)dp;

      static TabCompleter tabCompleter  = {};
      bool                lastKeyWasTab = tabCompleter._lastKeyWasTab;
      tabCompleter._lastKeyWasTab       = false;

      if (m->code == UIKeycode::TAB && textbox->text().size() && !el->_window->_shift) {
         TabCompleterRun(&tabCompleter, textbox, lastKeyWasTab, true);
         return 1;
      }
   }

   return 0;
}

void WatchDestroyTextbox(WatchWindow* w) {
   if (!w->textbox)
      return;
   w->textbox->destroy();
   w->textbox = nullptr;
   w->focus();
}

void WatchFree(WatchWindow* w, const shared_ptr<Watch>& watch, bool fieldsOnly = false) {
   if (watch->isDynamicArray) {
      if (auto it = rng::find(w->dynamicArrays, watch); it != rng::end(w->dynamicArrays))
         w->dynamicArrays.erase(it);
   }

   watch->loadedFields = false;
   watch->fields.clear();

   if (!fieldsOnly) {
      watch->key.clear();
   }
}

void WatchDeleteExpression(WatchWindow* w, bool fieldsOnly = false) {
   WatchDestroyTextbox(w);
   if (w->selectedRow == w->rows.size())
      return;
   int end = w->selectedRow + 1;

   for (; end < (int)w->rows.size(); end++) {
      if (w->rows[w->selectedRow]->depth >= w->rows[end]->depth) {
         break;
      }
   }

   shared_ptr<Watch> watch = w->rows[w->selectedRow]; // no reference as we want to hold the pointer

   if (!fieldsOnly) {
      if (auto it = rng::find(w->baseExpressions, watch); it != rng::end(w->baseExpressions))
         w->baseExpressions.erase(it);
   }

   if (fieldsOnly)
      w->selectedRow++;
   w->rows.erase(w->rows.cbegin() + w->selectedRow, w->rows.cbegin() + end);
   WatchFree(w, watch, fieldsOnly);
   if (!fieldsOnly)
      watch.reset();
}

std::string WatchEvaluate(std::string_view function, const shared_ptr<Watch>& watch) {
   char      buffer[4096];
   uintptr_t position = 0;

   position += std_format_to_n(buffer + position, sizeof(buffer) - position, "py {}([", function);

   Watch* stack[32];
   int    stackCount = 0;
   stack[0]          = watch.get();

   while (stack[stackCount]) {
      stack[stackCount + 1] = stack[stackCount]->parent;
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
      if (!w->key.empty()) {
         position += std_format_to_n(buffer + position, sizeof(buffer) - position, "'{}'", w->key);
      } else if (w->parent && w->parent->isDynamicArray) {
         position += std_format_to_n(buffer + position, sizeof(buffer) - position, "'[{}]'", w->arrayIndex);
      } else {
         position += std_format_to_n(buffer + position, sizeof(buffer) - position, "{}", w->arrayIndex);
      }
   }

   position += std_format_to_n(buffer + position, sizeof(buffer) - position, "]");

   if (function == "gf_valueof") {
      position += std_format_to_n(buffer + position, sizeof(buffer) - position, ",'{:c}'", watch->format ?: ' ');
   }

   position += std_format_to_n(buffer + position, sizeof(buffer) - position, ")");
   return EvaluateCommand(buffer);
}

bool WatchHasFields(const shared_ptr<Watch>& watch) {
   auto res = WatchEvaluate("gf_fields", watch);

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

void WatchAddFields(WatchWindow* w, const shared_ptr<Watch>& watch) {
   if (watch->loadedFields) {
      return;
   }

   watch->loadedFields = true;

   auto res = WatchEvaluate("gf_fields", watch);

   if (res.contains("(array)") || res.contains("(d_arr)")) {
      int count = sv_atoi(res, 7);

      count = std::clamp(count, 0, Watch::WATCH_ARRAY_MAX_FIELDS);

      watch->isArray    = true;
      bool hasSubFields = false;

      if (res.contains("(d_arr)")) {
         watch->isDynamicArray = true;
         w->dynamicArrays.push_back(watch);
      }

      for (int i = 0; i < count; i++) {
         auto field        = make_shared<Watch>();
         field->format     = watch->format;
         field->arrayIndex = (uintptr_t)i;
         field->parent     = watch.get();

         watch->fields.push_back(field);
         if (!i)
            hasSubFields = WatchHasFields(field);
         field->hasFields = hasSubFields;
         field->depth     = watch->depth + 1;
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
         auto field    = make_shared<Watch>();
         field->depth  = (uint8_t)(watch->depth + 1);
         field->key    = position;
         field->parent = watch.get();

         watch->fields.emplace_back(field);
         field->hasFields = WatchHasFields(field);
         position         = end + 1;
      }
   }
}

void WatchEnsureRowVisible(WatchWindow* w, size_t index) {
   if (w->selectedRow > w->rows.size())
      w->selectedRow = w->rows.size();
   UIScrollBar* scroll    = ((UIPanel*)w->_parent)->scrollbar();
   int          rowHeight = (int)(ui_size::textbox_height * w->_window->scale());
   int          start = index * rowHeight, end = (index + 1) * rowHeight, height = w->_parent->_bounds.height();
   bool         unchanged = false;
   if (end >= scroll->position() + height)
      scroll->position() = end - height;
   else if (start <= scroll->position())
      scroll->position() = start;
   else
      unchanged = true;
   if (!unchanged)
      w->_parent->refresh();
}

void WatchInsertFieldRows2(const shared_ptr<Watch>& watch, WatchVector* array) {
   for (const auto& field : watch->fields) {
      array->push_back(field);
      if (field->open)
         WatchInsertFieldRows2(field, array);
   }
}

void WatchInsertFieldRows(WatchWindow* w, const shared_ptr<Watch>& watch, size_t position, bool ensureLastVisible) {
   WatchVector array = {};
   WatchInsertFieldRows2(watch, &array);
   w->rows.insert(w->rows.cbegin() + position, array.cbegin(), array.cend());
   if (ensureLastVisible)
      WatchEnsureRowVisible(w, position + array.size() - 1);
   array.clear();
}

void WatchAddExpression(WatchWindow* w, string_view string = {}) {
   if (string.empty() && w->textbox && w->textbox->text().empty()) {
      WatchDestroyTextbox(w);
      return;
   }

   auto watch = make_shared<Watch>();

   if (!string.empty())
      watch->key = string;
   else
      watch->key = w->textbox->text();

   WatchDeleteExpression(w); // Deletes textbox.
   w->rows.insert(w->rows.cbegin() + w->selectedRow, watch);
   w->baseExpressions.push_back(watch);
   w->selectedRow++;

   auto res = WatchEvaluate("gf_typeof", watch);

   if (!res.contains("??")) {
      resize_to_lf(res);
      watch->type      = std::move(res);
      watch->hasFields = WatchHasFields(watch);
   }
}

void WatchAddExpression2(string_view string) {
   UIElement*   el = ctx.InterfaceWindowSwitchToAndFocus("Watch");
   WatchWindow* w  = (WatchWindow*)el->_cp;
   w->selectedRow  = w->rows.size();
   WatchAddExpression(w, string);
   if (w->selectedRow)
      w->selectedRow--;
   WatchEnsureRowVisible(w, w->selectedRow);
   w->_parent->refresh();
   w->refresh();
}

int WatchLoggerWindowMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::DESTROY) {
      if (el->_cp) {
         WatchLogger* logger = (WatchLogger*)el->_cp;

         if (auto it = rng::find(watchLoggers, logger); it != rng::end(watchLoggers))
            watchLoggers.erase(it);

         EvaluateCommand(std::format("delete {}", logger->id));
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

   StackEntry* entry = &logger->entries[logger->selectedEntry].trace[index];
   char        location[sizeof(entry->location)];
   strcpy(location, entry->location);
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
      WatchLogEntry*  entry = &logger->entries[m->_row];
      m->_is_selected       = (int)m->_row == logger->selectedEntry;

      if (m->_column == 0) {
         return m->format_to("{}", entry->value);
      } else if (m->_column == 1) {
         return m->format_to("{}", entry->where);
      } else {
         if (m->_column - 2 < entry->evaluated.size()) {
            return m->format_to("{}", entry->evaluated[m->_column - 2].result);
         } else {
            return 0;
         }
      }
   } else if (msg == UIMessage::LEFT_DOWN || msg == UIMessage::MOUSE_DRAG) {
      int index = ((UITable*)el)->hittest(el->cursor_pos());

      if (index != -1 && logger->selectedEntry != index) {
         logger->selectedEntry = index;
         logger->trace->set_num_items(logger->entries[index].trace.size());
         WatchLoggerTraceSelectFrame(logger->trace, 0, logger);
         logger->trace->resize_columns();
         logger->trace->refresh();
         el->refresh();
      }
   }

   return 0;
}

int WatchLoggerTraceMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   WatchLogger* logger = (WatchLogger*)el->_cp;

   if (msg == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m     = (UITableGetItem*)dp;
      StackEntry*     entry = &logger->entries[logger->selectedEntry].trace[m->_row];

      if (m->_column == 0) {
         return m->format_to("{}", entry->id);
      } else if (m->_column == 1) {
         return m->format_to("{}", entry->function);
      } else if (m->_column == 2) {
         return m->format_to("{}", entry->location);
      } else if (m->_column == 3) {
         return m->format_to("0x{:X}", entry->address);
      }
   } else if (msg == UIMessage::LEFT_DOWN || msg == UIMessage::MOUSE_DRAG) {
      int index = ((UITable*)el)->hittest(el->cursor_pos());
      WatchLoggerTraceSelectFrame(el, index, logger);
   }

   return 0;
}

std::string WatchGetAddress(const shared_ptr<Watch>& watch) {
   auto res = WatchEvaluate("gf_addressof", watch);

   if (strstr(res.c_str(), "??")) {
      windowMain->show_dialog(0, "Couldn't get the address of the variable.\n%f%B", "OK");
      return {};
   }

   auto end = res.find_first_of(' ');
   if (end == npos) {
      windowMain->show_dialog(0, "Couldn't get the address of the variable.\n%f%B", "OK");
      return {};
   }
   res.resize(end);

   resize_to_lf(res);
   return res;
}

void WatchLoggerResizeColumns(WatchLogger* logger) {
   logger->table->resize_columns();
   logger->table->refresh();
}

void WatchChangeLoggerCreate(WatchWindow* w) {
   if (w->selectedRow == w->rows.size()) {
      return;
   }

   if (!dataTab) {
      windowMain->show_dialog(0, "The data window is not open.\nThe watch log cannot be created.\n%f%B", "OK");
      return;
   }

   auto res = WatchGetAddress(w->rows[w->selectedRow]);
   if (res.empty()) {
      return;
   }

   char* expressionsToEvaluate = nullptr;

   auto result = windowMain->show_dialog(
      0, "-- Watch logger settings --\nExpressions to evaluate (separate with semicolons):\n%t\n\n%l\n\n%f%B%C",
      &expressionsToEvaluate, "Start", "Cancel");

   if (result == "Cancel") {
      free(expressionsToEvaluate);
      return;
   }

   UIMDIChild* child = &dataWindow->add_mdichild(UIMDIChild::CLOSE_BUTTON, UIRectangle(0), std::format("Log {}", res));

   res                = EvaluateCommand(std::format("watch * {}", res));
   const char* number = strstr(res.c_str(), "point ");

   if (!number) {
      windowMain->show_dialog(0, "Couldn't set the watchpoint.\n%f%B", "OK");
      return;
   }

   WatchLogger* logger = new WatchLogger;

   child->add_button(UIButton::SMALL | UIElement::non_client_flag, "Resize columns").on_click([logger](UIButton&) {
      WatchLoggerResizeColumns(logger);
   });

   uintptr_t position = 0;
   position += std_format_to_n(logger->columns + position, sizeof(logger->columns) - position, "New value\tWhere");

   if (expressionsToEvaluate) {
      uintptr_t start = 0;

      for (uintptr_t i = 0; true; i++) {
         if (expressionsToEvaluate[i] == ';' || !expressionsToEvaluate[i]) {
            position += std_format_to_n(logger->columns + position, sizeof(logger->columns) - position, "\t{}",
                                        std::string_view{expressionsToEvaluate + start, (size_t)(i - start)});
            start = i + 1;
         }

         if (!expressionsToEvaluate[i]) {
            break;
         }
      }
   }

   UISplitPane* panel = &child->add_splitpane(0, 0.5f);
   UITable*     table = &panel->add_table(UIElement::h_fill | UIElement::v_fill, logger->columns);
   UITable*     trace = &panel->add_table(UIElement::h_fill | UIElement::v_fill, "Index\tFunction\tLocation\tAddress");

   logger->id                    = sv_atoi(number, 6);
   logger->table                 = table;
   logger->trace                 = trace;
   logger->selectedEntry         = -1;
   logger->expressionsToEvaluate = expressionsToEvaluate;
   child->_cp                    = logger;
   table->_cp                    = logger;
   trace->_cp                    = logger;
   child->_user_proc             = WatchLoggerWindowMessage;
   table->_user_proc             = WatchLoggerTableMessage;
   trace->_user_proc             = WatchLoggerTraceMessage;
   watchLoggers.push_back(logger);
   dataWindow->refresh();
   WatchLoggerResizeColumns(logger);

   windowMain->show_dialog(0, "The log has been setup in the data window.\n%f%B", "OK");
   return;
}

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
      if (wl->id == id) {
         logger = wl;
         break;
      }
   }

   if (!logger)
      return false;

   *afterValue         = 0;
   *afterWhere         = 0;
   WatchLogEntry entry = {};

   const char* expressionsToEvaluate = logger->expressionsToEvaluate.c_str();

   if (expressionsToEvaluate) {
      uintptr_t start = 0;

      for (uintptr_t i = 0; true; i++) {
         if (expressionsToEvaluate[i] == ';' || !expressionsToEvaluate[i]) {

            auto res = EvaluateExpression(string_view(expressionsToEvaluate + start, i - start));
            start    = i + 1;
            WatchLogEvaluated evaluated;
            const char*       start = strstr(res.c_str(), " = ");
            if (start)
               evaluated.result = start + 3;
            else
               evaluated.result = std::move(res);
            entry.evaluated.push_back(std::move(evaluated));
         }

         if (!expressionsToEvaluate[i]) {
            break;
         }
      }
   }

   if (strlen(value) >= sizeof(entry.value))
      value[sizeof(entry.value) - 1] = 0;
   if (strlen(where) >= sizeof(entry.where))
      where[sizeof(entry.where) - 1] = 0;
   entry.value                      = value;
   entry.where                      = where;
   vector<StackEntry> previousStack = stack;
   stack                            = {};
   DebuggerGetStack();
   entry.trace = stack;
   stack       = previousStack;
   logger->entries.push_back(entry);
   ++logger->table->num_items();
   logger->table->refresh();
   (void)DebuggerSend("c", false, false);
   return true;
}

void WatchCreateTextboxForRow(WatchWindow* w, bool addExistingText) {
   int         rowHeight = (int)(ui_size::textbox_height * w->_window->scale());
   UIRectangle row       = w->_bounds;
   row.t += w->selectedRow * rowHeight, row.b = row.t + rowHeight;
   w->textbox = &w->add_textbox(0).set_user_proc(WatchTextboxMessage).set_cp(w);
   w->textbox->move(row, true);
   w->textbox->focus();

   if (addExistingText) {
      w->textbox->replace_text(w->rows[w->selectedRow]->key, false);
   }
}

WatchWindow* WatchGetFocused() {
   return windowMain->focused()->_class_proc == WatchWindowMessage ? (WatchWindow*)windowMain->focused()->_cp : NULL;
}

bool CommandWatchAddEntryForAddress(WatchWindow* _w) {
   WatchWindow* w = _w ? _w : WatchGetFocused();
   if (!w)
      return false;
   if (w->mode == WATCH_NORMAL && w->selectedRow == w->rows.size())
      return false;
   const auto& watch = w->rows[w->selectedRow];
   auto        res   = WatchGetAddress(watch);
   if (res.empty())
      return false;

   if (w->mode != WATCH_NORMAL) {
      ctx.InterfaceWindowSwitchToAndFocus("Watch");
      w = WatchGetFocused();
      assert(w != NULL);
   }

   auto address = res;
   res          = WatchEvaluate("gf_typeof", watch);
   if (res.empty() || res.contains("??"))
      return false;
   resize_to_lf(res);

   auto buffer = std::format("({}*){}", res, address);
   WatchAddExpression(w, buffer);
   WatchEnsureRowVisible(w, w->selectedRow);
   w->_parent->refresh();
   w->refresh();
   return true;
}

bool CommandWatchAddEntryForAddress() {
   return CommandWatchAddEntryForAddress(WatchGetFocused());
}

bool CommandWatchViewSourceAtAddress(WatchWindow* _w) {
   WatchWindow* w = _w ? _w : WatchGetFocused();
   if (!w)
      return false;
   if (w->mode == WATCH_NORMAL && w->selectedRow == w->rows.size())
      return false;
   char* position = (char*)w->rows[w->selectedRow]->value.c_str();
   while (*position && !isdigit(*position))
      position++;
   if (!(*position))
      return false;
   uint64_t value = strtoul(position, nullptr, 0);
   auto     res   = EvaluateCommand(std::format("info line * 0x{:x}", value));
   position       = (char*)res.c_str();

   if (res.contains("No line number")) {
      resize_to_lf(res);
      windowMain->show_dialog(0, "%s\n%f%B", res.c_str(), "OK");
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
   DisplaySetPosition(file, line - 1, false);
   return true;
}

bool CommandWatchViewSourceAtAddress() {
   return CommandWatchViewSourceAtAddress(WatchGetFocused());
}

void CommandWatchSaveAsRecurse(FILE* file, const shared_ptr<Watch>& watch, int indent, int indexInParentArray) {
   print(file, "{:.{}}", "\t\t\t\t\t\t\t\t\t\t\t\t\t\t", indent);

   if (indexInParentArray == -1) {
      print(file, "{} = ", watch->key);
   } else {
      print(file, "[{}] = ", indexInParentArray);
   }

   if (watch->open) {
      print(file, "\n");

      for (size_t i = 0; i < watch->fields.size(); i++) {
         CommandWatchSaveAsRecurse(file, watch->fields[i], indent + 1, watch->isArray ? i : -1);
      }
   } else {
      auto res = WatchEvaluate("gf_valueof", watch);
      if (!res.empty()) {
         resize_to_lf(res);
         print(file, "{}\n", res);
      }
   }
}

void CommandWatchSaveAs(WatchWindow* _w) {
   WatchWindow* w = _w ? _w : WatchGetFocused();
   if (!w)
      return;
   if (w->selectedRow == w->rows.size())
      return;

   char* filePath = nullptr;
   auto  result   = windowMain->show_dialog(0, "Path:            \n%t\n%f%B%C", &filePath, "Save", "Cancel");

   if (result == "Cancel") {
      free(filePath);
      return;
   }

   FILE* f = fopen(filePath, "wb");
   free(filePath);

   if (!f) {
      windowMain->show_dialog(0, "Could not open the file for writing!\n%f%B", "OK");
      return;
   }

   CommandWatchSaveAsRecurse(f, w->rows[w->selectedRow], 0, -1);
   fclose(f);
}

void CommandWatchCopyValueToClipboard(WatchWindow* w) {
   if (!w)
      return;
   if (w->mode == WATCH_NORMAL && w->selectedRow == w->rows.size())
      return;

   const shared_ptr<Watch>& watch = w->rows[w->selectedRow];

   auto res = WatchEvaluate("gf_valueof", watch);
   if (!res.empty()) {
      resize_to_lf(res);
      w->_window->write_clipboard_text(strdup(res.c_str()), sel_target_t::clipboard);
   }
}

int WatchWindowMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   WatchWindow* w         = (WatchWindow*)el->_cp;
   int          rowHeight = (int)(ui_size::textbox_height * el->_window->scale());
   int          result    = 0;

   if (msg == UIMessage::PAINT) {
      UIPainter* painter = (UIPainter*)dp;
      auto&      theme   = el->theme();

      for (size_t i = (painter->_clip.t - el->_bounds.t) / rowHeight; i <= WatchLastRow(w); i++) {
         UIRectangle row = el->_bounds;
         row.t += i * rowHeight, row.b = row.t + rowHeight;

         UIRectangle rect_intersection = intersection(row, painter->_clip);
         if (!rect_intersection.valid())
            break;

         bool focused = i == w->selectedRow && el->is_focused();

         if (focused)
            painter->draw_block(row, theme.selected);
         painter->draw_border(row, theme.border, UIRectangle(0, 1, 0, 1));

         row.l += ui_size::textbox_margin;
         row.r -= ui_size::textbox_margin;

         if (i < w->rows.size()) {
            const shared_ptr<Watch>& watch = w->rows[i];
            char                     buffer[256];

            if ((watch->value.empty() || watch->updateIndex != w->updateIndex) && !watch->open) {
               if (!ctx.programRunning) {
                  watch->updateIndex = w->updateIndex;
                  auto res           = WatchEvaluate("gf_valueof", watch);
                  resize_to_lf(res);
                  watch->value = std::move(res);
               } else {
                  watch->value = "..";
               }
            }

            char keyIndex[64];

            if (watch->key.empty()) {
               std_format_to_n(keyIndex, sizeof(keyIndex), "[{}]", watch->arrayIndex);
            }

            if (focused && w->waitingForFormatCharacter) {
               std_format_to_n(buffer, sizeof(buffer), "Enter format character: (e.g. 'x' for hex)");
            } else {
               std_format_to_n(buffer, sizeof(buffer), "{:.{}}{}{}{}{}", "                                           ",
                               watch->depth * 3,
                               watch->open        ? "v "
                               : watch->hasFields ? "> "
                                                  : "",
                               !watch->key.empty() ? watch->key.c_str() : keyIndex, watch->open ? "" : " = ",
                               watch->open ? "" : watch->value.c_str());
            }

            if (focused) {
               painter->draw_string(row, buffer, theme.textSelected, UIAlign::left, nullptr);
            } else {
               painter->draw_string_highlighted(row, buffer, 1, NULL);
            }
         }
      }
   } else if (msg == UIMessage::GET_HEIGHT) {
      return (WatchLastRow(w) + 1) * rowHeight;
   } else if (msg == UIMessage::LEFT_DOWN) {
      auto active_font = el->active_font();
      auto pos         = el->cursor_pos();
      if (pos.y >= el->_bounds.t) {
         w->selectedRow = (pos.y - el->_bounds.t) / rowHeight;

         if (w->selectedRow < w->rows.size()) {
            const shared_ptr<Watch>& watch = w->rows[w->selectedRow];
            int                      x     = (pos.x - el->_bounds.l) / active_font->_glyph_width;

            if (x >= watch->depth * 3 - 1 && x <= watch->depth * 3 + 1 && watch->hasFields) {
               UIKeyTyped m;
               m.code = watch->open ? UIKeycode::LEFT : UIKeycode::RIGHT;
               WatchWindowMessage(el, UIMessage::KEY_TYPED, 0, &m);
            }
         }
      } else
         w->selectedRow = 0;

      el->focus();
      el->repaint(nullptr);
   } else if (msg == UIMessage::RIGHT_DOWN) {
      auto pos = el->cursor_pos();
      if (pos.y >= el->_bounds.t) {
         size_t index = (pos.y - el->_bounds.t) / rowHeight;

         if (index < w->rows.size()) {
            WatchWindowMessage(el, UIMessage::LEFT_DOWN, di, dp);
            UIMenu& menu = el->ui()->create_menu(el->_window, UIMenu::NO_SCROLL);

            if (w->mode == WATCH_NORMAL && !w->rows[index]->parent) {
               menu.add_item(0, "Edit expression", [w](UIButton&) { WatchCreateTextboxForRow(w, true); })
                  .add_item(0, "Delete", [w](UIButton&) {
                     WatchDeleteExpression(w);
                     w->_parent->refresh();
                     w->refresh();
                  });
            }

            menu.add_item(0, "Copy value to clipboard\tCtrl+C", [w](UIButton&) { CommandWatchCopyValueToClipboard(w); })
               .add_item(0, "Log writes to address...", [w](UIButton&) { WatchChangeLoggerCreate(w); })
               .add_item(0, "Break on writes to address", [w](UIButton&) {
                  if (w->selectedRow == w->rows.size())
                     return;
                  auto res = WatchGetAddress(w->rows[w->selectedRow]);
                  if (res.empty())
                     return;

                  auto buffer = std::format("watch * {}", res);
                  (void)DebuggerSend(buffer, true, false);
               });

            if (firstWatchWindow) {
               menu.add_item(0, "Add entry for address\tCtrl+E", [w](UIButton&) { CommandWatchAddEntryForAddress(w); });
            }

            menu.add_item(0, "View source at address\tCtrl+G", [w](UIButton&) { CommandWatchViewSourceAtAddress(w); })
               .add_item(0, "Save as...", [w](UIButton&) { CommandWatchSaveAs(w); })
               .show();
         }
      }
   } else if (msg == UIMessage::UPDATE) {
      el->repaint(nullptr);
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;
      result        = 1;

      if (w->waitingForFormatCharacter) {
         w->rows[w->selectedRow]->format = (!m->text.empty() && isalpha(m->text[0])) ? m->text[0] : 0;
         w->rows[w->selectedRow]->updateIndex--;

         if (w->rows[w->selectedRow]->isArray) {
            for (auto& field : w->rows[w->selectedRow]->fields) {
               field->format = w->rows[w->selectedRow]->format;
               field->updateIndex--;
            }
         }

         w->waitingForFormatCharacter = false;
      } else if (w->mode == WATCH_NORMAL && w->selectedRow != w->rows.size() && !w->textbox &&
                 (m->code == UIKeycode::ENTER || m->code == UIKeycode::BACKSPACE ||
                  (m->code == UIKeycode::LEFT && !w->rows[w->selectedRow]->open)) &&
                 !w->rows[w->selectedRow]->parent) {
         WatchCreateTextboxForRow(w, true);
      } else if (m->code == UIKeycode::DEL && !w->textbox && w->selectedRow != w->rows.size() &&
                 !w->rows[w->selectedRow]->parent) {
         WatchDeleteExpression(w);
      } else if (!m->text.empty() && m->text[0] == '/' && w->selectedRow != w->rows.size()) {
         w->waitingForFormatCharacter = true;
      } else if (!m->text.empty() && m->text[0] == '`') {
         result = 0;
      } else if (w->mode == WATCH_NORMAL && !m->text.empty() && m->code != UIKeycode::TAB && !w->textbox &&
                 !el->_window->_ctrl && !el->_window->_alt &&
                 (w->selectedRow == w->rows.size() || !w->rows[w->selectedRow]->parent)) {
         WatchCreateTextboxForRow(w, false);
         w->textbox->message(msg, di, dp);
      } else if (w->mode == WATCH_NORMAL && !m->text.empty() && m->code == UI_KEYCODE_LETTER('V') && !w->textbox &&
                 el->_window->_ctrl && !el->_window->_alt && !el->_window->_shift &&
                 (w->selectedRow == w->rows.size() || !w->rows[w->selectedRow]->parent)) {
         WatchCreateTextboxForRow(w, false);
         w->textbox->message(msg, di, dp);
      } else if (m->code == UIKeycode::ENTER && w->textbox) {
         WatchAddExpression(w);
      } else if (m->code == UIKeycode::ESCAPE) {
         WatchDestroyTextbox(w);
      } else if (m->code == UIKeycode::UP) {
         if (el->_window->_shift) {
            auto currentLine = displayCode->current_line();
            if (currentLine && *currentLine > 0) {
               DisplaySetPosition(NULL, *currentLine - 1, false);
            }
         } else {
            WatchDestroyTextbox(w);
            if (w->selectedRow)
               w->selectedRow--;
         }
      } else if (m->code == UIKeycode::DOWN) {
         if (el->_window->_shift) {
            auto currentLine = displayCode->current_line();
            if (currentLine && *currentLine + 1 < displayCode->num_lines()) {
               DisplaySetPosition(NULL, *currentLine + 1, false);
            }
         } else {
            WatchDestroyTextbox(w);
            w->selectedRow++;
         }
      } else if (m->code == UIKeycode::HOME) {
         w->selectedRow = 0;
      } else if (m->code == UIKeycode::END) {
         w->selectedRow = WatchLastRow(w);
      } else if (m->code == UIKeycode::RIGHT && !w->textbox && w->selectedRow != w->rows.size() &&
                 w->rows[w->selectedRow]->hasFields && !w->rows[w->selectedRow]->open) {
         const shared_ptr<Watch>& watch = w->rows[w->selectedRow];
         watch->open                    = true;
         WatchAddFields(w, watch);
         WatchInsertFieldRows(w, watch, w->selectedRow + 1, true);
      } else if (m->code == UIKeycode::LEFT && !w->textbox && w->selectedRow != w->rows.size() &&
                 w->rows[w->selectedRow]->hasFields && w->rows[w->selectedRow]->open) {
         size_t end = w->selectedRow + 1;

         for (; end < w->rows.size(); end++) {
            if (w->rows[w->selectedRow]->depth >= w->rows[end]->depth) {
               break;
            }
         }

         w->rows.erase(w->rows.cbegin() + w->selectedRow + 1, w->rows.cbegin() + end);
         w->rows[w->selectedRow]->open = false;
      } else if (m->code == UIKeycode::LEFT && !w->textbox && w->selectedRow != w->rows.size() &&
                 !w->rows[w->selectedRow]->open) {
         for (size_t i = 0; i < w->rows.size(); i++) {
            if (w->rows[w->selectedRow]->parent == w->rows[i].get()) {
               w->selectedRow = i;
               break;
            }
         }
      } else if (m->code == UI_KEYCODE_LETTER('C') && !w->textbox && !el->_window->_shift && !el->_window->_alt &&
                 el->_window->_ctrl) {
         CommandWatchCopyValueToClipboard(w);
      } else {
         result = 0;
      }

      WatchEnsureRowVisible(w, w->selectedRow);
      el->_parent->refresh();
      el->refresh();
   } else if (msg == UIMessage::MIDDLE_DOWN) {
      if (w->mode == WATCH_NORMAL && !w->textbox && !el->_window->_ctrl && !el->_window->_alt &&
          (w->selectedRow == w->rows.size() || !w->rows[w->selectedRow]->parent)) {
         WatchCreateTextboxForRow(w, false);
         w->textbox->paste(sel_target_t::primary);
         el->repaint(NULL);
      }
      return 1;
   }

   if (w->selectedRow > WatchLastRow(w)) {
      w->selectedRow = WatchLastRow(w);
   }

   return result;
}

int WatchPanelMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   WatchWindow* window = (WatchWindow*)el->_cp;
   if (msg == UIMessage::LEFT_DOWN) {
      window->focus();
      window->repaint(nullptr);
   }

   return 0;
}

WatchWindow::WatchWindow(UIElement* parent, uint32_t flags, const char* name)
   : UIElement(parent, flags, WatchWindowMessage, name)
   , textbox(nullptr)
   , selectedRow(0)
   , mode(WATCH_NORMAL)
   , updateIndex(0)
   , waitingForFormatCharacter(false) {
   _cp = this; // todo: shouldn't be needed
}

UIElement* WatchWindowCreate(UIElement* parent) {
   UIPanel*     panel = &parent->add_panel(UIPanel::SCROLL | UIPanel::COLOR_1);
   WatchWindow* w     = new WatchWindow(panel, UIElement::h_fill | UIElement::tab_stop_flag, "Watch");
   panel->_user_proc  = WatchPanelMessage;
   panel->_cp         = w;

   w->mode      = WATCH_NORMAL;
   w->extraRows = 1;
   if (!firstWatchWindow)
      firstWatchWindow = w;
   return panel;
}

UIElement* LocalsWindowCreate(UIElement* parent) {
   UIPanel*     panel = &parent->add_panel(UIPanel::SCROLL | UIPanel::COLOR_1);
   WatchWindow* w     = new WatchWindow(panel, UIElement::h_fill | UIElement::tab_stop_flag, "Locals");
   panel->_user_proc  = WatchPanelMessage;
   panel->_cp         = w;
   w->mode            = WATCH_LOCALS;
   return panel;
}

void WatchWindowUpdate(const char*, UIElement* el) {
   WatchWindow* w = (WatchWindow*)el->_cp;

   if (w->mode == WATCH_LOCALS) {
      auto res = EvaluateCommand("py gf_locals()");

      bool newFrame = (w->lastLocalList.empty() || w->lastLocalList != res);

      if (newFrame) {
         w->lastLocalList = res;

         char*         buffer = strdup(res.c_str());
         char*         s      = buffer;
         char*         end;
         vector<char*> expressions = {};

         // we get a list of variables separated by `\n` characters, followed by the prompt
         // extract all the variable names into `expressions`.
         // we could use a regex here
         // -------------------------------------------------------------------------------
         while ((end = strchr(s, '\n')) != NULL) {
            *end = '\0';
            if (strstr(s, "(gdb)"))
               break;
            expressions.push_back(s);
            s = end + 1;
         }

         if (expressions.size() > 0) {
            for (size_t watchIndex = 0; watchIndex < w->baseExpressions.size(); watchIndex++) {
               const shared_ptr<Watch>& watch   = w->baseExpressions[watchIndex];
               bool                     matched = false;

               if (auto it = rng::find_if(expressions, [&](char* e) { return watch->key == e; });
                   it != rng::end(expressions)) {
                  expressions.erase(it);
                  matched = true;
               }

               if (!matched) {
                  [[maybe_unused]] bool found = false;
                  for (size_t rowIndex = 0; rowIndex < w->rows.size(); rowIndex++) {
                     if (w->rows[rowIndex] == watch) {
                        w->selectedRow = rowIndex;
                        WatchDeleteExpression(w);
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
               w->selectedRow = w->rows.size();
               WatchAddExpression(w, exp);
            }

            w->selectedRow = w->rows.size();
         }

         free(buffer);
         expressions.clear();
      }
   }

   for (size_t i = 0; i < w->baseExpressions.size(); i++) {
      const shared_ptr<Watch>& watch = w->baseExpressions[i];
      auto                     res   = WatchEvaluate("gf_typeof", watch);
      resize_to_lf(res);

      if (res != watch->type && res != "??") {
         watch->type = std::move(res);

         for (size_t j = 0; j < w->rows.size(); j++) {
            if (w->rows[j] == watch) {
               w->selectedRow = j;
               WatchAddExpression(w, watch->key);
               w->selectedRow = w->rows.size(), i--;
               break;
            }
         }
      }
   }

   for (size_t i = 0; i < w->dynamicArrays.size(); i++) {
      const shared_ptr<Watch>& watch = w->dynamicArrays[i];
      auto                     res   = WatchEvaluate("gf_fields", watch);
      if (res.empty() || !res.contains("(d_arr)"))
         continue;
      int count = sv_atoi(res, 7);

      count        = std::clamp(count, 0, Watch::WATCH_ARRAY_MAX_FIELDS);
      int oldCount = watch->fields.size();

      if (oldCount != count) {
         size_t index = (size_t)-1;

         for (size_t i = 0; i < w->rows.size(); i++) {
            if (w->rows[i] == watch) {
               index = i;
               break;
            }
         }

         assert(index != (size_t)-1);
         w->selectedRow = index;
         WatchDeleteExpression(w, true);
         watch->open = true;
         WatchAddFields(w, watch);
         WatchInsertFieldRows(w, watch, index + 1, false);
      }
   }

   w->updateIndex++;
   el->_parent->refresh();
   el->refresh();
}

void WatchWindowFocus(UIElement* el) {
   WatchWindow* w = (WatchWindow*)el;
   w->focus();
}

bool CommandAddWatch() {
   UIElement* el = ctx.InterfaceWindowSwitchToAndFocus("Watch");
   if (!el)
      return false;
   WatchWindow* w = (WatchWindow*)el->_cp;
   if (w->textbox)
      return false;
   w->selectedRow = w->rows.size();
   WatchCreateTextboxForRow(w, false);
   return true;
}

// ---------------------------------------------------/
// Stack window:
// ---------------------------------------------------/

void StackSetFrame(UIElement* el, int index) {
   if (index >= 0 && index < (int)((UITable*)el)->num_items()) {
      stackChanged = true;
      if (stackSelected != (size_t)index) {
         (void)DebuggerSend(std::format("frame {}", index), false, false);
         stackSelected = index;
         el->repaint(nullptr);
      } else {
         displayCode->set_current_line({}); // force the update in DisplayPosition as we may have scrolled away
         DisplaySetPositionFromStack();
      }
   }
}

int TableStackMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m = (UITableGetItem*)dp;
      m->_is_selected   = (size_t)m->_row == stackSelected;
      StackEntry* entry = &stack[m->_row];

      if (m->_column == 0) {
         return m->format_to("{}", entry->id);
      } else if (m->_column == 1) {
         return m->format_to("{}", entry->function);
      } else if (m->_column == 2) {
         return m->format_to("{}", entry->location);
      } else if (m->_column == 3) {
         return m->format_to("0x{:X}", entry->address);
      }
   } else if (msg == UIMessage::LEFT_DOWN || msg == UIMessage::MOUSE_DRAG) {
      StackSetFrame(el, ((UITable*)el)->hittest(el->cursor_pos()));
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if (m->code == UIKeycode::UP || m->code == UIKeycode::DOWN) {
         StackSetFrame(el, stackSelected + (m->code == UIKeycode::UP ? -1 : 1));
         // TODO Scroll the row into view if necessary.
         return 1;
      }
   }

   return 0;
}

UIElement* StackWindowCreate(UIElement* parent) {
   return &parent->add_table(0, "Index\tFunction\tLocation\tAddress").set_user_proc(TableStackMessage);
}

void StackWindowUpdate(const char*, UIElement* _table) {
   UITable& table = *(UITable*)_table;
   table.set_num_items(stack.size()).resize_columns().refresh();
}

// ---------------------------------------------------/
// Breakpoints window:
// ---------------------------------------------------/

struct BreakpointTableData {
   vector<int> selected;
   int         anchor = 0;
};

void for_all_selected_breakpoints(BreakpointTableData* data, string_view action) {
   for (auto selected : data->selected) {
      for (const auto& breakpoint : breakpoints) {
         if (breakpoint.number == selected) {
            (void)DebuggerSend(std::format("{} {}", action, selected), true, false);
            break;
         }
      }
   }
}

void CommandDeleteSelectedBreakpoints(BreakpointTableData* data) {
   for_all_selected_breakpoints(data, "delete");
}

void CommandDisableSelectedBreakpoints(BreakpointTableData* data) {
   for_all_selected_breakpoints(data, "disable");
}

void CommandEnableSelectedBreakpoints(BreakpointTableData* data) {
   for_all_selected_breakpoints(data, "enable");
}

int TableBreakpointsMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   BreakpointTableData* data = (BreakpointTableData*)el->_cp;

   if (msg == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m     = (UITableGetItem*)dp;
      Breakpoint*     entry = &breakpoints[m->_row];
      m->_is_selected       = rng::find(data->selected, entry->number) != rng::end(data->selected);

      if (m->_column == 0) {
         return m->format_to("{}", entry->file);
      } else if (m->_column == 1) {
         if (entry->watchpoint)
            return m->format_to("watch {}", entry->number);
         else
            return m->format_to("{}", entry->line);
      } else if (m->_column == 2) {
         return m->format_to("{}", entry->enabled ? "yes" : "no");
      } else if (m->_column == 3) {
         return m->format_to("{}", entry->condition);
      } else if (m->_column == 4) {
         if (entry->hit > 0) {
            return m->format_to("{}", entry->hit);
         }
      }
   } else if (msg == UIMessage::RIGHT_DOWN) {
      int index = ((UITable*)el)->hittest(el->cursor_pos());

      if (index != -1) {
         Breakpoint* entry = &breakpoints[index];

         bool found = rng::find(data->selected, entry->number) != rng::end(data->selected);
         if (data->selected.size() <= 1 || !found) {
            if (!el->_window->_ctrl)
               data->selected.clear();
            data->selected.push_back(entry->number);
         }

         UIMenu& menu = el->ui()->create_menu(el->_window, UIMenu::NO_SCROLL);

         if (data->selected.size() > 1) {
            bool atLeastOneBreakpointDisabled = false;

            for (auto selected : data->selected) {
               for (const auto& breakpoint : breakpoints) {
                  if (breakpoint.number == selected && !breakpoint.enabled) {
                     atLeastOneBreakpointDisabled = true;
                     goto addMenuItems;
                  }
               }
            }

         addMenuItems:
            menu.add_item(0, "Delete", [data](UIButton&) { CommandDeleteSelectedBreakpoints(data); });

            if (atLeastOneBreakpointDisabled)
               menu.add_item(0, "Enable", [data](UIButton&) { CommandEnableSelectedBreakpoints(data); });
            else
               menu.add_item(0, "Disable", [data](UIButton&) { CommandDisableSelectedBreakpoints(data); });
         } else {
            menu.add_item(0, "Delete", [index](UIButton&) { CommandDeleteBreakpoint(index); });

            if (breakpoints[index].enabled)
               menu.add_item(0, "Enable", [index](UIButton&) { CommandEnableBreakpoint(index); });
         }

         menu.show();
      }
   } else if (msg == UIMessage::LEFT_DOWN) {
      int index = ((UITable*)el)->hittest(el->cursor_pos());

      if (index != -1) {
         Breakpoint* entry = &breakpoints[index];

         if (!el->_window->_shift)
            data->anchor = entry->number;
         if (!el->_window->_ctrl)
            data->selected.clear();

         uintptr_t from = 0, to = 0;

         for (size_t i = 0; i < breakpoints.size(); i++) {
            if (breakpoints[i].number == entry->number) {
               from = i;
            }
            if (breakpoints[i].number == data->anchor) {
               to = i;
            }
         }

         if (from > to) {
            uintptr_t temp = from;
            from = to, to = temp;
         }

         for (uintptr_t i = from; i <= to; i++) {
            if (el->_window->_ctrl && !el->_window->_shift) {
               if (auto it = rng::find(data->selected, breakpoints[i].number); it != rng::end(data->selected))
                  data->selected.erase(it);
            } else {
               data->selected.push_back(breakpoints[i].number);
            }
         }

         if (!entry->watchpoint && rng::find(data->selected, entry->number) != rng::end(data->selected)) {
            DisplaySetPosition(entry->file, entry->line - 1, false);
         }
      } else if (!el->_window->_ctrl && !el->_window->_shift) {
         data->selected.clear();
      }
      el->focus();
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if (m->code == UIKeycode::DEL && data->selected.size() > 0) {
         CommandDeleteSelectedBreakpoints(data);
      }
   }

   return 0;
}

UIElement* BreakpointsWindowCreate(UIElement* parent) {
   return &parent->add_table(0, "File\tLine\tEnabled\tCondition\tHit")
              .set_cp(new BreakpointTableData)
              .set_user_proc(TableBreakpointsMessage);
}

void BreakpointsWindowUpdate(const char*, UIElement* _table) {
   UITable* table = (UITable*)_table;
   table->set_num_items(breakpoints.size());
   table->resize_columns();
   table->refresh();
}

// ---------------------------------------------------/
// Data window:
// ---------------------------------------------------/

UIButton* buttonFillWindow;

int DataTabMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::TAB_SELECTED && autoUpdateViewersQueued) {
      // If we've switched to the data tab, we may need to update the bitmap viewers.

      for (const auto& auw : autoUpdateViewers)
         auw.callback(auw.el);

      autoUpdateViewersQueued = false;
   }

   return 0;
}

bool CommandToggleFillDataTab() {
   if (!dataTab)
      return false;
   static UIElement *oldParent, *oldBefore;
   buttonFillWindow->_flags ^= UIButton::CHECKED;

   if (switcherMain->_active == dataTab) {
      switcherMain->switch_to(switcherMain->_children[0]);
      dataTab->change_parent(oldParent, oldBefore);
   } else {
      dataTab->message(UIMessage::TAB_SELECTED, 0, 0);
      oldParent = dataTab->_parent;
      oldBefore = dataTab->change_parent(switcherMain, NULL);
      switcherMain->switch_to(dataTab);
   }
   return true;
}

UIElement* DataWindowCreate(UIElement* parent) {
   dataTab         = &parent->add_panel(UIPanel::EXPAND);
   UIPanel* panel5 = &dataTab->add_panel(UIPanel::COLOR_1 | UIPanel::HORIZONTAL | UIPanel::SMALL_SPACING);

   panel5->add_button(UIButton::SMALL, "Fill window").on_click([](UIButton&) { CommandToggleFillDataTab(); });
   for (const auto& idw : ctx.interfaceDataViewers) {
      panel5->add_button(UIButton::SMALL, idw.addButtonLabel).on_click([&](UIButton&) { idw.addButtonCallback(); });
   }

   dataTab->add_mdiclient(UIElement::v_fill).set_user_proc(DataTabMessage);
   return dataTab;
}

// ---------------------------------------------------/
// Struct window:
// ---------------------------------------------------/

struct StructWindow {
   UICode*    display = nullptr;
   UITextbox* textbox = nullptr;
};

int TextboxStructNameMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   StructWindow* window = (StructWindow*)el->_cp;

   if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if (m->code == UIKeycode::ENTER) {
         auto  res = EvaluateCommand(std::format("ptype /o {}", window->textbox->text()));
         char* end = (char*)strstr(res.c_str(), "\n(gdb)");
         if (end)
            *end = 0;
         window->display->insert_content(res, true);
         window->textbox->clear(false);
         window->display->refresh();
         el->refresh();
         return 1;
      }
   }

   return 0;
}

UIElement* StructWindowCreate(UIElement* parent) {
   StructWindow* window = new StructWindow;
   UIPanel*      panel  = &parent->add_panel(UIPanel::COLOR_1 | UIPanel::EXPAND);
   window->textbox      = &panel->add_textbox(0).set_user_proc(TextboxStructNameMessage).set_cp(window);
   window->display      = &panel->add_code(UIElement::v_fill | UICode::NO_MARGIN | UICode::SELECTABLE)
                         .insert_content("Type the name of a struct to view its layout.", false);
   return panel;
}

// ---------------------------------------------------/
// Files window:
// ---------------------------------------------------/

struct FilesWindow {
   char     directory[PATH_MAX];
   UIPanel* panel = nullptr;
   UILabel* path  = nullptr;
};

bool FilesPanelPopulate(FilesWindow* window);

mode_t FilesGetMode(FilesWindow* window, UIButton* button, size_t* oldLength) {
   const char* name = button->label().data();
   *oldLength       = strlen(window->directory);
   strcat(window->directory, "/");
   strcat(window->directory, name);
   struct stat s;
   stat(window->directory, &s);
   return s.st_mode;
}

int FilesButtonMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   UIButton* button = (UIButton*)el;

   if (msg == UIMessage::CLICKED) {
      FilesWindow* window = (FilesWindow*)el->_cp;
      size_t       oldLength;
      mode_t       mode = FilesGetMode(window, button, &oldLength);

      if (S_ISDIR(mode)) {
         if (FilesPanelPopulate(window)) {
            char copy[PATH_MAX];
            realpath(window->directory, copy);
            strcpy(window->directory, copy);
            return 0;
         }
      } else if (S_ISREG(mode)) {
         DisplaySetPosition(window->directory, 0, false);
      }

      window->directory[oldLength] = 0;
   } else if (msg == UIMessage::PAINT) {
      UIPainter* painter = (UIPainter*)dp;
      int        i       = el->is_pressed() + el->is_hovered();
      auto&      theme   = el->theme();
      if (i)
         painter->draw_block(el->_bounds, i == 2 ? theme.buttonPressed : theme.buttonHovered);
      painter->draw_string(el->_bounds + UIRectangle(ui_size::button_padding, 0, 0, 0), button->label(),
                           button->_flags & UIButton::CHECKED ? theme.codeNumber : theme.codeDefault, UIAlign::left,
                           NULL);
      return 1;
   }

   return 0;
}

bool FilesPanelPopulate(FilesWindow* window) {
   size_t         oldLength;
   DIR*           directory = opendir(window->directory);
   struct dirent* entry;
   if (!directory)
      return false;
   vector<std::string> names = {};
   while ((entry = readdir(directory)))
      names.push_back(entry->d_name);
   closedir(directory);
   window->panel->destroy_descendents();

   std::sort(names.begin(), names.end());

   for (auto name : names) {
      if (name[0] != '.' || name[1] != 0) {
         UIButton* button = &window->panel->add_button(0, name)
                                .clear_flag(UIElement::tab_stop_flag)
                                .set_cp(window)
                                .set_user_proc(FilesButtonMessage);

         if (S_ISDIR(FilesGetMode(window, button, &oldLength))) {
            button->set_flag(UIButton::CHECKED);
         }

         window->directory[oldLength] = 0;
      }
   }

   window->panel->refresh();

   char path[PATH_MAX];
   realpath(window->directory, path);
   window->path->set_label(path);

   return true;
}

void FilesNavigateToCWD(FilesWindow* window) {
   getcwd(window->directory, sizeof(window->directory));
   FilesPanelPopulate(window);
}

void FilesNavigateToActiveFile(FilesWindow* window) {
   std_format_to_n(window->directory, sizeof(window->directory), "{}", currentFileFull);
   int p = strlen(window->directory);
   while (p--)
      if (window->directory[p] == '/') {
         window->directory[p] = 0;
         break;
      }
   FilesPanelPopulate(window);
}

UIElement* FilesWindowCreate(UIElement* parent) {
   FilesWindow* window    = new FilesWindow;
   UIPanel*     container = &parent->add_panel(UIPanel::EXPAND);
   window->panel = &container->add_panel(UIPanel::COLOR_1 | UIPanel::EXPAND | UIPanel::SCROLL | UIElement::v_fill)
                       .set_gap(-1)
                       .set_border(UIRectangle(1))
                       .set_cp(window);
   UIPanel* row = &container->add_panel(UIPanel::COLOR_2 | UIPanel::HORIZONTAL | UIPanel::SMALL_SPACING);

   row->add_button(UIButton::SMALL, "-> cwd").on_click([window](UIButton&) { FilesNavigateToCWD(window); });
   row->add_button(UIButton::SMALL, "-> active file").on_click([window](UIButton&) {
      FilesNavigateToActiveFile(window);
   });

   window->path = &row->add_label(UIElement::h_fill, "");
   FilesNavigateToCWD(window);
   return container;
}

// ---------------------------------------------------/
// Registers window:
// ---------------------------------------------------/

struct RegisterData {
   char string[128];
};
vector<RegisterData> registerData;

UIElement* RegistersWindowCreate(UIElement* parent) {
   return &parent->add_panel(UIPanel::SMALL_SPACING | UIPanel::COLOR_1 | UIPanel::SCROLL);
}

void RegistersWindowUpdate(const char*, UIElement* panel) {
   auto res = EvaluateCommand("info registers");

   if (res.empty() || res.contains("The program has no registers now.") ||
       res.contains("The current thread has terminated")) {
      return;
   }

   panel->destroy_descendents();
   const char*          position        = res.c_str();
   vector<RegisterData> newRegisterData = {};
   bool                 anyChanges      = false;

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
      std_format_to_n(data.string, sizeof(data.string), "{}",
                      std::string_view{stringStart, (size_t)(stringEnd - stringStart)});
      bool modified = false;

      if (registerData.size() > newRegisterData.size()) {
         RegisterData* old = &registerData[newRegisterData.size()];

         if (strcmp(old->string, data.string)) {
            modified = true;
         }
      }

      newRegisterData.push_back(data);

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

      if (modified && showingDisassembly && !isPC) {
         if (!anyChanges) {
            autoPrintResult[0]  = 0;
            autoPrintResultLine = autoPrintExpressionLine;
            anyChanges          = true;
         } else {
            int position = strlen(autoPrintResult);
            std_format_to_n(autoPrintResult + position, sizeof(autoPrintResult) - position, ", ");
         }

         int position = strlen(autoPrintResult);
         std_format_to_n(autoPrintResult + position, sizeof(autoPrintResult) - position, "{}={}",
                         std::string_view{nameStart, (size_t)(nameEnd - nameStart)},
                         std::string_view{format1Start, (size_t)(format1End - format1Start)});
      }
   }

   panel->refresh();
   registerData.clear();
   registerData = newRegisterData;
}

// ---------------------------------------------------/
// Commands window:
// ---------------------------------------------------/

UIElement* CommandsWindowCreate(UIElement* parent) {
   UIPanel* panel = &parent->add_panel(UIPanel::COLOR_1 | UIPanel::SMALL_SPACING | UIPanel::EXPAND | UIPanel::SCROLL);
   if (!presetCommands.size())
      panel->add_label(0, "No preset commands found in config file!");

   for (const auto& cmd : presetCommands) {
      panel->add_button(0, cmd.key).on_click([command = std::format("gf-command {}", cmd.key)](UIButton&) {
         CommandSendToGDB(command);
      });
   }

   return panel;
}

// ---------------------------------------------------/
// Log window:
// ---------------------------------------------------/

void* LogWindowThread(void* context) {
   if (!logPipePath) {
      print(std::cerr, "Warning: The log pipe path has not been set in the configuration file!\n");
      return nullptr;
   }

   int file = open(logPipePath, O_RDONLY | O_NONBLOCK);

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
         windowMain->post_message(msgReceivedLog, s);
      }
   }
}

void LogReceived(char* buffer) {
   ctx.logWindow->insert_content(buffer, false);
   (*(UIElement**)buffer)->refresh();
   free(buffer);
}

UIElement* LogWindowCreate(UIElement* parent) {
   UICode*   code = &parent->add_code(UICode::SELECTABLE);
   pthread_t thread;
   pthread_create(&thread, nullptr, LogWindowThread, code);
   return code;
}

// ---------------------------------------------------/
// Thread window:
// ---------------------------------------------------/

struct Thread {
   char frame[127];
   bool active = false;
   int  id     = 0;
};

struct ThreadWindow {
   vector<Thread> threads;
};

int ThreadTableMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   ThreadWindow* window = (ThreadWindow*)el->_cp;

   if (msg == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m = (UITableGetItem*)dp;
      m->_is_selected   = window->threads[m->_row].active;

      if (m->_column == 0) {
         return m->format_to("{}", window->threads[m->_row].id);
      } else if (m->_column == 1) {
         return m->format_to("{}", window->threads[m->_row].frame);
      }
   } else if (msg == UIMessage::LEFT_DOWN) {
      int index = ((UITable*)el)->hittest(el->cursor_pos());

      if (index != -1) {
         (void)DebuggerSend(std::format("thread {}", window->threads[index].id), true, false);
      }
   }

   return 0;
}

UIElement* ThreadWindowCreate(UIElement* parent) {
   return &parent->add_table(0, "ID\tFrame").set_cp(new ThreadWindow).set_user_proc(ThreadTableMessage);
}

void ThreadWindowUpdate(const char*, UIElement* _table) {
   ThreadWindow* window = (ThreadWindow*)_table->_cp;
   window->threads.clear();

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
         thread.active = true;
      thread.id = sv_atoi(position, 2);
      position  = strchr(position + 1, '"');
      if (!position)
         break;
      position = strchr(position + 1, '"');
      if (!position)
         break;
      position++;
      char* end = strchr(position, '\n');
      if (end - position >= (ptrdiff_t)sizeof(thread.frame))
         end = position + sizeof(thread.frame) - 1;
      memcpy(thread.frame, position, end - position);
      thread.frame[end - position] = 0;
      window->threads.push_back(thread);
   }

   UITable* table = (UITable*)_table;
   table->set_num_items(window->threads.size());
   table->resize_columns();
   table->refresh();
}

// ---------------------------------------------------/
// Executable window:
// ---------------------------------------------------/

struct ExecutableWindow {
   UITextbox* path      = nullptr;
   UITextbox* arguments = nullptr;
   bool       should_ask;

   void start_or_run(bool pause) {
      auto res = EvaluateCommand(std::format("file \"{}\"", path->text()));

      if (res.contains("No such file or directory.")) {
         windowMain->show_dialog(0, "The executable path is invalid.\n%f%B", "OK");
         return;
      }

      (void)EvaluateCommand(std::format("start {}", arguments->text()));

      if (should_ask) {
         CommandParseInternal("gf-get-pwd", true);
      }

      if (!pause) {
         (void)CommandParseInternal("run", false);
      } else {
         DebuggerGetStack();
         DisplaySetPositionFromStack();
      }
   }

   void save() {
      FILE* f = fopen(localConfigPath, "rb");
      if (f) {
         auto result = windowMain->show_dialog(0, ".project.gf already exists in the current directory.\n%f%B%C",
                                               "Overwrite", "Cancel");
         if (result != "Overwrite")
            return;
         fclose(f);
      }

      f = fopen(localConfigPath, "wb");
      print(f, "[executable]\npath={}\narguments={}\nask_directory={}\n", path->text(), arguments->text(),
            should_ask ? '1' : '0');
      fclose(f);
      SettingsAddTrustedFolder();
      windowMain->show_dialog(0, "Saved executable settings!\n%f%B", "OK");
   }
};

UIElement* ExecutableWindowCreate(UIElement* parent) {
   ExecutableWindow* win   = new ExecutableWindow;
   UIPanel*          panel = &parent->add_panel(UIPanel::COLOR_1 | UIPanel::EXPAND);

   panel->add_n(
      [&](auto& p) { p.add_label(0, "Path to executable:"); },
      [&](auto& p) { win->path = &p.add_textbox(0).replace_text(gfc.exe_path, false); },
      [&](auto& p) { p.add_label(0, "Command line arguments:"); },
      [&](auto& p) { win->arguments = &p.add_textbox(0).replace_text(gfc.exe_args, false); },
      [&](auto& p) {
         p.add_checkbox(0, "Ask GDB for working directory").set_checked(gfc.exe_ask_dir).track(&win->should_ask);
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

// ---------------------------------------------------/
// Command search window:
// ---------------------------------------------------/

struct GDBCommand {
   char* name             = nullptr;
   char* description      = nullptr;
   char* descriptionLower = nullptr;
};

struct CommandSearchWindow {
   UICode*            display = nullptr;
   UITextbox*         textbox = nullptr;
   vector<GDBCommand> commands;
};

int TextboxSearchCommandMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   CommandSearchWindow* window = (CommandSearchWindow*)el->_cp;

   if (msg == UIMessage::KEY_TYPED) {
      if (!window->commands.size()) {
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
               command.name       = (char*)calloc(1, dash - 1 - position + 1);

               for (int i = 0, j = 0; i < dash - 1 - position; i++) {
                  if (position[i] != ' ' || position[i + 1] != ' ') {
                     command.name[j++] = position[i];
                  }
               }

               command.description      = (char*)calloc(1, next - (dash + 3) + 1);
               command.descriptionLower = (char*)calloc(1, next - (dash + 3) + 1);
               memcpy(command.description, dash + 3, next - (dash + 3));

               for (int i = 0; command.description[i]; i++) {
                  command.descriptionLower[i] = command.description[i] >= 'A' && command.description[i] <= 'Z'
                                                   ? command.description[i] + 'a' - 'A'
                                                   : command.description[i];
               }

               window->commands.push_back(command);
            }

            position = next + 1;
         }
      }

      char query[4096];
      char buffer[4096];
      bool firstMatch = true;

      std_format_to_n(query, sizeof(query), "{}", window->textbox->text());
      for (int i = 0; query[i]; i++) {
         query[i] = query[i] >= 'A' && query[i] <= 'Z' ? query[i] + 'a' - 'A' : query[i];
      }

      for (const auto& cmd : window->commands) {
         if (strstr(cmd.descriptionLower, query)) {
            std_format_to_n(buffer, sizeof(buffer), "{}: {}", cmd.name, cmd.description);
            window->display->insert_content(buffer, firstMatch);
            firstMatch = false;
         }
      }

      if (firstMatch) {
         window->display->insert_content("(no matches)", firstMatch);
      }

      window->display->reset_vscroll();
      window->display->refresh();
   }

   return 0;
}

UIElement* CommandSearchWindowCreate(UIElement* parent) {
   CommandSearchWindow* win = new CommandSearchWindow;

   UIPanel* panel =
      &parent->add_panel(UIPanel::COLOR_1 | UIPanel::EXPAND)
          .add_n(
             [&](auto& p) { win->textbox = &p.add_textbox(0).set_user_proc(TextboxSearchCommandMessage).set_cp(win); },
             [&](auto& p) {
                win->display = &p.add_code(UIElement::v_fill | UICode::NO_MARGIN | UICode::SELECTABLE)
                                   .insert_content("Type here to search \nGDB command descriptions.", true);
             });
   return panel;
}

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
   void*    thisFunction = nullptr;
   uint64_t timeStamp    = 0; // High bit set if exiting the function.
};

struct ProfWindow {
   uint64_t ticksPerMs         = 0;
   UIFont*  fontFlameGraph     = nullptr;
   bool     inStepOverProfiled = false;
};

struct ProfFlameGraphEntry {
   void*       thisFunction = nullptr;
   const char* cName        = nullptr;
   double      startTime    = 0;
   double      endTime      = 0;
   int         depth        = 0;
   uint8_t     colorIndex   = 0;
};

struct ProfFlameGraphEntryTime {
   // Keep this structure as small as possible!
   float start = 0;
   float end   = 0;
   int   depth = 0;
};

struct ProfSourceFileEntry {
   char cPath[256];
};

struct ProfFunctionEntry {
   uint32_t callCount       = 0;
   int      lineNumber      = 0;
   int      sourceFileIndex = 0;
   double   totalTime       = 0;
   char     cName[64];
};

int ProfFlameGraphMessage(UIElement* el, UIMessage msg, int di, void* dp);

struct ProfFlameGraphReport : public UIElement {
   UIRectangle  client;
   UIFont*      font;
   UITable*     table;
   UIButton*    switchViewButton;
   UIScrollBar* vScroll;
   bool         showingTable;

   vector<ProfFlameGraphEntry>             entries;
   vector<ProfFlameGraphEntryTime>         entryTimes;
   vector<ProfFunctionEntry>               sortedFunctions;
   unordered_map<void*, ProfFunctionEntry> functions;
   vector<ProfSourceFileEntry>             sourceFiles;

   uint32_t* thumbnail;
   int       thumbnailWidth, thumbnailHeight;

   double totalTime;
   double xStart, xEnd;

   ProfFlameGraphEntry* hover;
   ProfFlameGraphEntry* menuItem;

#define FLAME_GRAPH_DRAG_ZOOM_RANGE (1)
#define FLAME_GRAPH_DRAG_PAN (2)
#define FLAME_GRAPH_DRAG_X_PAN_AND_ZOOM (3)
#define FLAME_GRAPH_DRAG_X_SCROLL (4)
   bool   dragStarted;
   int    dragMode;
   double dragInitialValue, dragInitialValue2;
   int    dragInitialPoint, dragInitialPoint2;
   int    dragCurrentPoint;
   double dragScrollRate;

   ProfFlameGraphReport(UIElement* parent, uint32_t flags)
      : UIElement(parent, flags, ProfFlameGraphMessage, "flame graph")
      , client(0)
      , font(nullptr)
      , table(nullptr)
      , switchViewButton(nullptr)
      , vScroll(nullptr)
      , showingTable(false)
      , thumbnail(nullptr)
      , thumbnailWidth(0)
      , thumbnailHeight(0)
      , totalTime(0)
      , xStart(0)
      , xEnd(0)
      , hover(nullptr)
      , menuItem(nullptr)
      , dragStarted(false)
      , dragMode(0)
      , dragInitialValue(0)
      , dragInitialValue2(0)
      , dragInitialPoint(0)
      , dragInitialPoint2(0)
      , dragCurrentPoint(0)
      , dragScrollRate(0) {}
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
   return a->depth > b->depth           ? 1
          : a->depth < b->depth         ? -1
          : a->startTime > b->startTime ? 1
          : a->startTime < b->startTime ? -1
                                        : 0;
}

void ProfShowSource(ProfFlameGraphReport* report) {
   ProfFlameGraphEntry* entry = report->menuItem;
   if (!report->functions.contains(entry->thisFunction)) {
      windowMain->show_dialog(0, "Source information was not found for this function.\n%f%b", "OK");
      return;
   }
   ProfFunctionEntry& function = report->functions[entry->thisFunction];

   if (!function.cName[0]) {
      windowMain->show_dialog(0, "Source information was not found for this function.\n%f%b", "OK");
      return;
   } else {
      DisplaySetPosition(report->sourceFiles[function.sourceFileIndex].cPath, function.lineNumber - 1, false);
   }
}

void ProfAddBreakpoint(ProfFlameGraphEntry* entry) {
   CommandSendToGDB(std::format("b {}", entry->cName));
}

void ProfFillView(ProfFlameGraphReport* report) {
   ProfFlameGraphEntry* entry = report->menuItem;
   report->xStart             = entry->startTime;
   report->xEnd               = entry->endTime;
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

#define PROFILER_ENTRY_RECTANGLE_EARLY() \
   int64_t rr = report->client.l + (int64_t)((time->end - report->xStart) * zoomX + 0.999);

#define PROFILER_ENTRY_RECTANGLE_OTHER()                                                                        \
   int64_t rl = report->client.l + (int64_t)((time->start - report->xStart) * zoomX);                           \
   int64_t rt = report->client.t + time->depth * profRowHeight + profScaleHeight - report->vScroll->position(); \
   int64_t rb = rt + profRowHeight;

void* ProfFlameGraphRenderThread(void* _unused) {
   (void)_unused;
   int threadIndex = __sync_fetch_and_add(&profRenderThreadIndexAllocator, 1);

   while (true) {
      sem_wait(&profRenderStartSemaphores[threadIndex]);

      ProfFlameGraphReport* report = profRenderReport;
      UIElement*            el     = report;

      double     zoomX    = (double)report->client.width() / (report->xEnd - report->xStart);
      UIPainter  _painter = *profRenderPainter; // Some of the draw functions modify the painter's clip, so make a copy.
      UIPainter* painter  = &_painter;

      int64_t pr = 0, pd = 0;
      float   xStartF = (float)report->xStart;
      float   xEndF   = (float)report->xEnd;

      size_t startIndex = report->entries.size() / profRenderThreadIndexAllocator * threadIndex;
      size_t endIndex   = report->entries.size() / profRenderThreadIndexAllocator * (threadIndex + 1);

      if (profRenderThreadCount == threadIndex + 1) {
         endIndex = report->entries.size();
      }

      // printf("render on thread %d from %d to %d\n", threadIndex, startIndex, endIndex);

      for (size_t i = startIndex; i < endIndex; i++) {
         ProfFlameGraphEntryTime* time = &report->entryTimes[i];

         if (time->end < xStartF || time->start > xEndF) {
            continue;
         }

         PROFILER_ENTRY_RECTANGLE_EARLY();

         if (pr == rr && pd == time->depth) {
            continue;
         }

         ProfFlameGraphEntry* entry = &report->entries[i];
         PROFILER_ENTRY_RECTANGLE_OTHER();

         if (rl <= el->_clip.r && rr >= el->_clip.l && rt <= el->_clip.b && rb >= el->_clip.t) {
            // Carefully convert 64-bit integers to 32-bit integers for UIRectangle,
            // since the rectangle may be really large when zoomed in.
            UIRectangle r;
            r.l = rl < report->client.l ? report->client.l : rl;
            r.r = rr > report->client.r ? report->client.r : rr;
            r.t = rt < report->client.t ? report->client.t : rt;
            r.b = rb > report->client.b ? report->client.b : rb;

            painter->draw_block(UIRectangle(r.r - 1, r.r, r.t, r.b - 1), profBorderDarkColor);
            painter->draw_block(UIRectangle(r.l, r.r, r.b - 1, r.b), profBorderDarkColor);
            painter->draw_block(UIRectangle(r.l, r.r - 1, r.t, r.t + 1), profBorderLightColor);
            painter->draw_block(UIRectangle(r.l, r.l + 1, r.t + 1, r.b - 1), profBorderLightColor);

            bool     hovered = report->hover && report->hover->thisFunction == entry->thisFunction && !report->dragMode;
            uint32_t color   = hovered ? profHoverColor : profEntryColorPalette[entry->colorIndex];
            /// uint32_t color = hovered ? profHoverColor : profMainColor;
            painter->draw_block(UIRectangle(r.l + 1, r.r - 1, r.t + 1, r.b - 1), color);

            if (r.width() > 40) {
               auto string = std::format("{} {:f}ms", entry->cName, entry->endTime - entry->startTime);
               painter->draw_string(UIRectangle(r.l + 2, r.r, r.t, r.b), string, profTextColor, UIAlign::left, NULL);
            }
         }

         pr = rr, pd = entry->depth;

         float nextDrawTime = 0.99f / zoomX + time->end;

         for (; i < report->entries.size(); i++) {
            if (report->entryTimes[i].end >= nextDrawTime || report->entryTimes[i].depth != time->depth) {
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
      UIFont* previousFont = report->font->activate();

      if (report->xStart < 0)
         report->xStart = 0;
      if (report->xEnd > report->totalTime)
         report->xEnd = report->totalTime;
      if (report->xEnd < report->xStart + 1e-7)
         report->xEnd = report->xStart + 1e-7;

      double zoomX = (double)report->client.width() / (report->xEnd - report->xStart);

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
      painter->draw_block(report->client, profBackgroundColor);

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
            UIRectangle(report->client.l, report->client.r, report->client.t, report->client.t + profScaleHeight);
         painter->draw_rectangle(r, profMainColor, profBorderDarkColor, UIRectangle(0, 0, 0, 1));

         double increment = 1000.0;
         while (increment > 1e-6 && increment * zoomX > 600.0)
            increment *= 0.1;

         double start = (painter->_clip.l - report->client.l) / zoomX + report->xStart;
         start -= fmod(start, increment) + increment;

         for (double i = start; i < report->totalTime; i += increment) {
            UIRectangle r;
            r.t = report->client.t;
            r.b = r.t + profScaleHeight;
            r.l = report->client.l + (int)((i - report->xStart) * zoomX);
            r.r = r.l + (int)(increment * zoomX);
            if (r.l > painter->_clip.r)
               break;
            auto string = std::format("{:.4f}ms", i);
            painter->draw_block(UIRectangle(r.l, r.l + 1, r.t, r.b), profBorderLightColor);
            painter->draw_string(r, string, profTextColor, UIAlign::left, NULL);
         }
      }

      if (report->dragMode == FLAME_GRAPH_DRAG_ZOOM_RANGE) {
         UIRectangle r = report->client;
         r.l = report->dragInitialPoint, r.r = report->dragCurrentPoint;
         if (r.l > r.r)
            r.r = report->dragInitialPoint, r.l = report->dragCurrentPoint;
         painter->draw_invert(r);
      }

      if (report->thumbnail) {
         UIRectangle zoomBar =
            UIRectangle(report->client.l, report->client.r, report->client.b - profZoomBarHeight, report->client.b);
         UIRectangle zoomBarThumb = zoomBar;
         zoomBarThumb.l           = zoomBar.l + zoomBar.width() * (report->xStart / report->totalTime);
         zoomBarThumb.r           = zoomBar.l + zoomBar.width() * (report->xEnd / report->totalTime);
         UIRectangle drawBounds   = intersection(zoomBar, painter->_clip);

         for (int i = drawBounds.t; i < drawBounds.b; i++) {
            for (int j = drawBounds.l; j < drawBounds.r; j++) {
               int si = (i - zoomBar.t) * report->thumbnailHeight / zoomBar.height();
               int sj = (j - zoomBar.l) * report->thumbnailWidth / zoomBar.width();

               if (si >= 0 && si < report->thumbnailHeight && sj >= 0 && sj < report->thumbnailWidth) {
                  painter->_bits[i * painter->_width + j] = report->thumbnail[si * report->thumbnailWidth + sj];
               }
            }
         }

         painter->draw_border(zoomBar, profBorderDarkColor, UIRectangle(2));
         painter->draw_border(zoomBarThumb, profBorderLightColor, UIRectangle(4));
      }

      if (report->hover && !report->dragMode) {
         const ProfFunctionEntry& function = report->functions[report->hover->thisFunction];

         char line1[256], line2[256], line3[256];
         std_format_to_n(line1, sizeof(line1), "[{}] {}:{}", report->hover->cName,
                         function.sourceFileIndex != -1 ? report->sourceFiles[function.sourceFileIndex].cPath : "??",
                         function.lineNumber);
         std_format_to_n(line2, sizeof(line2), "This call: {:f}ms {:.1f}%%",
                         report->hover->endTime - report->hover->startTime,
                         (report->hover->endTime - report->hover->startTime) / report->totalTime * 100.0);
         std_format_to_n(line3, sizeof(line3), "Total: {:f}ms in {} calls ({:f}ms avg) {:.1f}%%", function.totalTime,
                         function.callCount, function.totalTime / function.callCount,
                         function.totalTime / report->totalTime * 100.0);

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
      double               zoomX = (double)report->client.width() / (report->xEnd - report->xStart);
      ProfFlameGraphEntry* hover = nullptr;
      auto                 pos   = el->cursor_pos();

      int   depth   = (pos.y - report->client.t + report->vScroll->position() - profScaleHeight) / profRowHeight;
      float xStartF = (float)report->xStart;
      float xEndF   = (float)report->xEnd;

      for (size_t i = 0; i < report->entries.size(); i++) {
         ProfFlameGraphEntryTime* time = &report->entryTimes[i];

         if (time->depth != depth || time->end < xStartF || time->start > xEndF) {
            continue;
         }

         PROFILER_ENTRY_RECTANGLE_EARLY();
         PROFILER_ENTRY_RECTANGLE_OTHER();

         (void)rt;
         (void)rb;

         if (pos.x >= rl && pos.x < rr) {
            hover = &report->entries[i];
            break;
         }
      }

      if (hover != report->hover || hover /* to repaint the tooltip */) {
         report->hover = hover;
         el->repaint(NULL);
      }
   } else if (msg == UIMessage::UPDATE) {
      if (report->hover && !el->is_hovered()) {
         report->hover = NULL;
         el->repaint(NULL);
      }
   } else if (msg == UIMessage::LEFT_DOWN) {
      auto pos = el->cursor_pos();
      if (pos.y < report->client.b - profZoomBarHeight) {
         report->dragMode          = FLAME_GRAPH_DRAG_PAN;
         report->dragInitialValue  = report->xStart;
         report->dragInitialPoint  = pos.x;
         report->dragInitialValue2 = report->vScroll->position();
         report->dragInitialPoint2 = pos.y;
         el->_window->set_cursor((int)UICursor::hand);
      } else {
         report->dragMode         = FLAME_GRAPH_DRAG_X_SCROLL;
         report->dragInitialValue = report->xStart;
         report->dragInitialPoint = pos.x;
         report->dragScrollRate   = 1.0;

         if (pos.x < report->client.l + report->client.width() * (report->xStart / report->totalTime) ||
             pos.y >= report->client.l + report->client.width() * (report->xEnd / report->totalTime)) {
            report->dragScrollRate = 0.2;
         }
      }
   } else if (msg == UIMessage::MIDDLE_DOWN) {
      auto pos = el->cursor_pos();
      if (pos.y < report->client.b - profZoomBarHeight) {
         report->dragMode          = FLAME_GRAPH_DRAG_X_PAN_AND_ZOOM;
         report->dragInitialValue  = report->xStart;
         report->dragInitialPoint  = pos.x;
         report->dragInitialPoint2 = pos.y;
         el->_window->set_cursor((int)UICursor::cross_hair);
      }
   } else if (msg == UIMessage::RIGHT_DOWN) {
      auto pos = el->cursor_pos();
      if (pos.y < report->client.b - profZoomBarHeight) {
         report->dragMode         = FLAME_GRAPH_DRAG_ZOOM_RANGE;
         report->dragInitialPoint = pos.x;
      }
   } else if (msg == UIMessage::LEFT_UP || msg == UIMessage::RIGHT_UP || msg == UIMessage::MIDDLE_UP) {
      if (report->dragMode == FLAME_GRAPH_DRAG_ZOOM_RANGE && report->dragStarted) {
         UIRectangle r = report->client;
         r.l = report->dragInitialPoint, r.r = report->dragCurrentPoint;
         if (r.l > r.r)
            r.r = report->dragInitialPoint, r.l = report->dragCurrentPoint;
         double zoomX   = (double)report->client.width() / (report->xEnd - report->xStart);
         report->xEnd   = (r.r - report->client.l) / zoomX + report->xStart;
         report->xStart = (r.l - report->client.l) / zoomX + report->xStart;
      } else if (!report->dragStarted && msg == UIMessage::RIGHT_UP && report->hover) {
         report->menuItem = report->hover;
         el->ui()
            ->create_menu(el->_window, UIMenu::NO_SCROLL)
            .add_item(0, "Show source", [report](UIButton&) { ProfShowSource(report); })
            .add_item(0, "Add breakpoint", [report](UIButton&) { ProfAddBreakpoint(report->hover); })
            .add_item(0, "Fill view", [report](UIButton&) { ProfFillView(report); })
            .show();
      } else if (!report->dragStarted && msg == UIMessage::MIDDLE_UP && report->hover) {
         report->menuItem = report->hover;
         ProfFillView(report);
      }

      report->dragMode    = 0;
      report->dragStarted = false;
      el->repaint(NULL);
      el->_window->set_cursor((int)UICursor::arrow);
   } else if (msg == UIMessage::MOUSE_DRAG) {
      report->dragStarted = true;
      auto pos            = el->cursor_pos();

      if (report->dragMode == FLAME_GRAPH_DRAG_PAN) {
         double delta   = report->xEnd - report->xStart;
         report->xStart = report->dragInitialValue - (double)(pos.x - report->dragInitialPoint) * report->totalTime /
                                                        report->client.width() * delta / report->totalTime;
         report->xEnd = report->xStart + delta;
         if (report->xStart < 0) {
            report->xEnd -= report->xStart;
            report->xStart = 0;
         }
         if (report->xEnd > report->totalTime) {
            report->xStart += report->totalTime - report->xEnd;
            report->xEnd = report->totalTime;
         }
         report->vScroll->position() = report->dragInitialValue2 - (double)(pos.y - report->dragInitialPoint2);
         report->vScroll->refresh();
      } else if (report->dragMode == FLAME_GRAPH_DRAG_X_SCROLL) {
         double delta   = report->xEnd - report->xStart;
         report->xStart = report->dragInitialValue + (double)(pos.x - report->dragInitialPoint) * report->totalTime /
                                                        report->client.width() * report->dragScrollRate;
         report->xEnd = report->xStart + delta;
         if (report->xStart < 0) {
            report->xEnd -= report->xStart;
            report->xStart = 0;
         }
         if (report->xEnd > report->totalTime) {
            report->xStart += report->totalTime - report->xEnd;
            report->xEnd = report->totalTime;
         }
      } else if (report->dragMode == FLAME_GRAPH_DRAG_X_PAN_AND_ZOOM) {
         double delta = report->xEnd - report->xStart;
         report->xStart += (double)(pos.x - report->dragInitialPoint) * report->totalTime / report->client.width() *
                           delta / report->totalTime * 3.0;
         report->xEnd  = report->xStart + delta;
         double factor = powf(1.02, pos.y - report->dragInitialPoint2);
         double mouse  = (double)(pos.x - report->client.l) / report->client.width();
#if 0
         mouse = 0.5;
         XWarpPointer(ui->display, None, windowMain->window, 0, 0, 0, 0, report->dragInitialPoint, report->dragInitialPoint2);
#else
         report->dragInitialPoint  = pos.x;
         report->dragInitialPoint2 = pos.y;
#endif
         double newZoom = (report->xEnd - report->xStart) / report->totalTime * factor;
         report->xStart += mouse * (report->xEnd - report->xStart) * (1 - factor);
         report->xEnd = newZoom * report->totalTime + report->xStart;
      } else if (report->dragMode == FLAME_GRAPH_DRAG_ZOOM_RANGE) {
         report->dragCurrentPoint = pos.x;
      }

      el->repaint(NULL);
   } else if (msg == UIMessage::MOUSE_WHEEL) {
      auto   pos         = el->cursor_pos();
      int    divisions   = di / 72;
      double factor      = 1;
      double perDivision = 1.2f;
      while (divisions > 0)
         factor *= perDivision, divisions--;
      while (divisions < 0)
         factor /= perDivision, divisions++;
      double mouse   = (double)(pos.x - report->client.l) / report->client.width();
      double newZoom = (report->xEnd - report->xStart) / report->totalTime * factor;
      report->xStart += mouse * (report->xEnd - report->xStart) * (1 - factor);
      report->xEnd = newZoom * report->totalTime + report->xStart;
      el->repaint(NULL);
      return 1;
   } else if (msg == UIMessage::GET_CURSOR) {
      return report->dragMode == FLAME_GRAPH_DRAG_PAN              ? (int)UICursor::hand
             : report->dragMode == FLAME_GRAPH_DRAG_X_PAN_AND_ZOOM ? (int)UICursor::cross_hair
                                                                   : (int)UICursor::arrow;
   } else if (msg == UIMessage::LAYOUT) {
      UIRectangle scrollBarBounds = el->_bounds;
      scrollBarBounds.l           = scrollBarBounds.r - ui_size::scroll_bar * el->_window->scale();
      report->vScroll->set_page(el->_bounds.height() - profZoomBarHeight);
      report->vScroll->move(scrollBarBounds, true);
      report->client   = el->_bounds;
      report->client.r = scrollBarBounds.l;
   } else if (msg == UIMessage::SCROLLED) {
      el->refresh();
   } else if (msg == UIMessage::DESTROY) {
      report->entries.clear();
      report->functions.clear();
      report->sourceFiles.clear();
      report->entryTimes.clear();
      free(report->thumbnail);
   }

   return 0;
}

int ProfReportWindowMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   ProfFlameGraphReport* report = (ProfFlameGraphReport*)el->_cp;

   if (msg == UIMessage::LAYOUT) {
      if (report->showingTable) {
         report->_flags |= UIElement::hide_flag;
         report->table->_flags &= ~UIElement::hide_flag;
      } else {
         report->_flags &= ~UIElement::hide_flag;
         report->table->_flags |= UIElement::hide_flag;
      }
      el->_class_proc(el, msg, di, dp);
      report->table->move(report->_bounds, false);
      return 1;
   }

   return 0;
}

void ProfSwitchView(ProfFlameGraphReport* report) {
   report->showingTable = !report->showingTable;
   report->switchViewButton->set_label(report->showingTable ? "Graph view" : "Table view");
   report->_parent->refresh();
}

#define PROF_FUNCTION_COMPARE(a, b)                                 \
   int a(const void* c, const void* d) {                            \
      const ProfFunctionEntry* left  = (const ProfFunctionEntry*)c; \
      const ProfFunctionEntry* right = (const ProfFunctionEntry*)d; \
      return b;                                                     \
   }
#define PROF_COMPARE_NUMBERS(a, b) (a) > (b) ? -1 : (a) < (b) ? 1 : 0

PROF_FUNCTION_COMPARE(ProfFunctionCompareName, strcmp(left->cName, right->cName));
PROF_FUNCTION_COMPARE(ProfFunctionCompareTotalTime, PROF_COMPARE_NUMBERS(left->totalTime, right->totalTime));
PROF_FUNCTION_COMPARE(ProfFunctionCompareCallCount, PROF_COMPARE_NUMBERS(left->callCount, right->callCount));
PROF_FUNCTION_COMPARE(ProfFunctionCompareAverage,
                      PROF_COMPARE_NUMBERS(left->totalTime / left->callCount, right->totalTime / right->callCount));
PROF_FUNCTION_COMPARE(ProfFunctionComparePercentage, PROF_COMPARE_NUMBERS(left->totalTime, right->totalTime));

int ProfTableMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   ProfFlameGraphReport* report = (ProfFlameGraphReport*)el->_cp;
   UITable*              table  = report->table;

   if (msg == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem*    m     = (UITableGetItem*)dp;
      ProfFunctionEntry* entry = &report->sortedFunctions[m->_row];

      if (m->_column == 0) {
         return m->format_to("{}", entry->cName);
      } else if (m->_column == 1) {
         return m->format_to("{:f}", entry->totalTime);
      } else if (m->_column == 2) {
         return m->format_to("{}", entry->callCount);
      } else if (m->_column == 3) {
         return m->format_to("{:f}", entry->totalTime / entry->callCount);
      } else if (m->_column == 4) {
         return m->format_to("{:f}", entry->totalTime / report->totalTime * 100);
      }
   } else if (msg == UIMessage::LEFT_DOWN) {
      int index = table->header_hittest(el->cursor_pos());

      if (index != -1) {
         if (index == 0) {
            qsort(report->sortedFunctions.data(), report->sortedFunctions.size(), sizeof(ProfFunctionEntry),
                  ProfFunctionCompareName);
         } else if (index == 1) {
            qsort(report->sortedFunctions.data(), report->sortedFunctions.size(), sizeof(ProfFunctionEntry),
                  ProfFunctionCompareTotalTime);
         } else if (index == 2) {
            qsort(report->sortedFunctions.data(), report->sortedFunctions.size(), sizeof(ProfFunctionEntry),
                  ProfFunctionCompareCallCount);
         } else if (index == 3) {
            qsort(report->sortedFunctions.data(), report->sortedFunctions.size(), sizeof(ProfFunctionEntry),
                  ProfFunctionCompareAverage);
         } else if (index == 4) {
            qsort(report->sortedFunctions.data(), report->sortedFunctions.size(), sizeof(ProfFunctionEntry),
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
   data->ticksPerMs             = ticksPerMsString ? sv_atoi(ticksPerMsString, 2) : 0;

   if (!ticksPerMsString || !data->ticksPerMs) {
      windowMain->show_dialog(0, "Profile data could not be loaded (1).\nConsult the guide.\n%f%b", "OK");
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
      UIWindow* window = windowMain;
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
      windowMain->show_dialog(0, "Profile data could not be loaded (2).\nConsult the guide.\n%f%b", "OK");
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
      if (rawEntries[i].timeStamp >> 63) {
         if (stackDepth)
            stackDepth--;
         else
            stackErrorCount++;
      } else {
         stackDepth++;
      }

      if (functions.contains(rawEntries[i].thisFunction))
         continue;
      ProfFunctionEntry& function = functions[rawEntries[i].thisFunction];

      function.sourceFileIndex = -1;

      std_format_to_n(buffer, sizeof(buffer), "(void *) {:p}", rawEntries[i].thisFunction);
      auto cName = EvaluateExpression(buffer);
      if (cName.empty())
         continue;

      if (strchr(cName.c_str(), '<'))
         cName = strchr(cName.c_str(), '<') + 1;
      int length = strlen(cName.c_str());
      if (length > (int)sizeof(function.cName) - 1)
         length = sizeof(function.cName) - 1;
      memcpy(function.cName, cName.c_str(), length);
      function.cName[length] = 0;

      int inTemplate = 0;

      for (int j = 0; j < length; j++) {
         if (function.cName[j] == '(' && !inTemplate) {
            function.cName[j] = 0;
            break;
         } else if (function.cName[j] == '<') {
            inTemplate++;
         } else if (function.cName[j] == '>') {
            if (inTemplate) {
               inTemplate--;
            } else {
               function.cName[j] = 0;
               break;
            }
         }
      }

      std_format_to_n(buffer, sizeof(buffer), "py print(gdb.lookup_global_symbol('{}').symtab.filename)",
                      function.cName);
      auto res = EvaluateCommand(buffer);

      if (!res.contains("Traceback (most recent call last):")) {
         resize_to_lf(res);
         ProfSourceFileEntry sourceFile  = {};
         const char*         cSourceFile = res.c_str();
         length                          = strlen(cSourceFile);
         if (length > (int)sizeof(sourceFile.cPath) - 1)
            length = sizeof(sourceFile.cPath) - 1;
         memcpy(sourceFile.cPath, cSourceFile, length);
         sourceFile.cPath[length] = 0;
         std_format_to_n(buffer, sizeof(buffer), "py print(gdb.lookup_global_symbol('{}').line)", function.cName);
         res                 = EvaluateCommand(buffer);
         function.lineNumber = sv_atoi(res);

         for (size_t i = 0; i < sourceFiles.size(); i++) {
            if (0 == strcmp(sourceFiles[i].cPath, sourceFile.cPath)) {
               function.sourceFileIndex = i;
               break;
            }
         }

         if (function.sourceFileIndex == -1) {
            function.sourceFileIndex = sourceFiles.size();
            sourceFiles.push_back(sourceFile);
         }
      }
   }

   UIMDIChild* window = &dataWindow->add_mdichild(UIMDIChild::CLOSE_BUTTON, ui_rect_2s(800, 600), "Flame graph");
   ProfFlameGraphReport* report = new ProfFlameGraphReport(window, 0);

   report->switchViewButton = &window->add_button(UIButton::SMALL | UIElement::non_client_flag, "Table view")
                                  .set_cp(report)
                                  .on_click([report](UIButton&) { ProfSwitchView(report); });
   UITable* table = report->table = &window->add_table(0, "Name\tTime spent (ms)\tCall count\tAverage per call (ms)")
                                        .set_cp(report)
                                        .set_user_proc(ProfTableMessage);

   report->vScroll = &report->add_scrollbar(0);
   report->font    = data->fontFlameGraph;

   window->set_cp(report).set_user_proc(ProfReportWindowMessage);

   report->functions   = functions;
   functions           = {};
   report->sourceFiles = sourceFiles;
   sourceFiles         = {};

   vector<ProfFlameGraphEntry> stack = {};

   for (int i = 0; i < stackErrorCount; i++) {
      ProfFlameGraphEntry entry = {};
      entry.cName               = "[unknown]";
      entry.startTime           = 0;
      entry.depth               = stack.size();
      stack.push_back(entry);
   }

   for (int i = 0; i < rawEntryCount; i++) {
      if (rawEntries[i].timeStamp >> 63) {
         if (!stack.size()) {
            continue;
         }

         ProfFlameGraphEntry entry = stack.back();
         entry.endTime             = (double)((rawEntries[i].timeStamp & 0x7FFFFFFFFFFFFFFFUL) -
                                  (rawEntries[0].timeStamp & 0x7FFFFFFFFFFFFFFFUL)) /
                         data->ticksPerMs;

         if (0 == strcmp(entry.cName, "[unknown]")) {
            if (report->functions.contains(rawEntries[i].thisFunction))
               entry.cName = report->functions[rawEntries[i].thisFunction].cName;
         }

         entry.thisFunction = rawEntries[i].thisFunction;
         stack.pop_back();
         report->entries.push_back(entry);
      } else {
         ProfFlameGraphEntry entry = {};
         if (report->functions.contains(rawEntries[i].thisFunction)) {
            ProfFunctionEntry& function = report->functions[rawEntries[i].thisFunction];
            entry.cName                 = function.cName;
            entry.colorIndex =
               function.sourceFileIndex % (sizeof(profEntryColorPalette) / sizeof(profEntryColorPalette[0]));
         }

         entry.startTime =
            (double)(rawEntries[i].timeStamp - (rawEntries[0].timeStamp & 0x7FFFFFFFFFFFFFFFUL)) / data->ticksPerMs;
         entry.thisFunction = rawEntries[i].thisFunction;
         entry.depth        = stack.size();
         stack.push_back(entry);
      }
   }

   for (const auto& entry : report->entries) {
      if (entry.endTime > report->totalTime) {
         report->totalTime = entry.endTime;
      }
   }

   while (stack.size()) {
      ProfFlameGraphEntry entry = stack.back();
      entry.endTime             = report->totalTime;
      stack.pop_back();
      report->entries.push_back(entry);
   }

   if (!report->totalTime) {
      report->totalTime = 1;
   }

   stack.clear();
   report->xEnd = report->totalTime;
   qsort(report->entries.data(), report->entries.size(), sizeof(ProfFlameGraphEntry), ProfFlameGraphEntryCompare);

   int maxDepth = 0;

   for (const auto& entry : report->entries) {
      ProfFlameGraphEntryTime time;
      time.start = entry.startTime;
      time.end   = entry.endTime;
      time.depth = entry.depth;
      report->entryTimes.push_back(time);

      if (entry.depth > maxDepth) {
         maxDepth = entry.depth;
      }

      ProfFunctionEntry& function = report->functions[entry.thisFunction];
      function.callCount++;
      function.totalTime += entry.endTime - entry.startTime;
   }

   print("Found {} functions over {} source files.\n", report->functions.size(), report->sourceFiles.size());

   report->vScroll->set_maximum((maxDepth + 2) * 30);

   for (const auto& [k, v] : report->functions) {
      if (k)
         report->sortedFunctions.push_back(v);
   }

   {
      // Create an image of the graph for the zoom bar.
      uint32_t  width  = 1200;
      uint32_t  height = maxDepth * 30 + 30;
      UIPainter painter(report->ui(), width, height, (uint32_t*)malloc(width * height * 4));

      report->client = report->_bounds = report->_clip = painter._clip;
      ProfFlameGraphMessage(report, UIMessage::PAINT, 0, &painter);
      int newHeight = 30;
      ThumbnailResize(painter._bits, painter._width, painter._height, painter._width, newHeight);
      report->thumbnail       = (uint32_t*)realloc(painter._bits, painter._width * newHeight * 4);
      report->thumbnailWidth  = width;
      report->thumbnailHeight = newHeight;
   }

   table->set_num_items(report->sortedFunctions.size());
   qsort(report->sortedFunctions.data(), report->sortedFunctions.size(), sizeof(ProfFunctionEntry),
         ProfFunctionCompareTotalTime);
   table->set_column_highlight(1);
   table->resize_columns();

   free(rawEntries);
}

void ProfStepOverProfiled(ProfWindow* window) {
   (void)EvaluateCommand("call GfProfilingStart()");
   CommandSendToGDB("gf-next");
   window->inStepOverProfiled = true;
}

void ProfWindowUpdate(const char* data, UIElement* el) {
   ProfWindow* window = (ProfWindow*)el->_cp;

   if (window->inStepOverProfiled) {
      (void)EvaluateCommand("call GfProfilingStop()");
      ProfLoadProfileData(window);
      ctx.InterfaceWindowSwitchToAndFocus("Data");
      dataWindow->refresh();
      window->inStepOverProfiled = false;
   }
}

UIElement* ProfWindowCreate(UIElement* parent) {
   const int   fontSizeFlameGraph = 8;
   ProfWindow* window             = new ProfWindow;
   UI*         ui                 = parent->ui();
   window->fontFlameGraph         = ui->create_font(ui->default_font_path(), fontSizeFlameGraph);
   UIPanel* panel                 = &parent->add_panel(UIPanel::COLOR_1 | UIPanel::EXPAND).set_cp(window);
   panel->add_button(UIElement::v_fill, "Step over profiled").on_click([window](UIButton&) {
      ProfStepOverProfiled(window);
   });

#ifdef UI_FREETYPE
   // Since we will do multithreaded painting with fontFlameGraph, we need to make sure all its glyphs are ready to go.
   for (uintptr_t i = 0; i < sizeof(window->fontFlameGraph->_glyphs_rendered); i++) {
      UIPainter fakePainter(parent->ui(), 0, 0, nullptr);
      UIFont*   previousFont = window->fontFlameGraph->activate();
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
   UIButton*       gotoButton;
   vector<int16_t> loadedBytes;
   uint64_t        offset;

   MemoryWindow(UIElement* parent)
      : UIElement(parent, 0, MemoryWindowMessage, "memory window")
      , gotoButton(
           &add_button(UIButton::SMALL, "&").on_click([this](UIButton&) { MemoryWindowGotoButtonInvoke(this); })) {}
};

int MemoryWindowMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   MemoryWindow* window = (MemoryWindow*)el;

   if (msg == UIMessage::PAINT) {
      auto&      theme   = el->theme();
      UIPainter* painter = (UIPainter*)dp;
      painter->draw_block(el->_bounds, theme.panel1);

      char        buffer[64];
      uint64_t    address   = window->offset;
      const int   rowHeight = el->ui()->string_height();
      UIRectangle row       = el->_bounds + ui_rect_1i(10);
      size_t      rowCount  = (painter->_clip.b - row.t) / rowHeight;
      row.b                 = row.t + rowHeight;

      {
         std_format_to_n(buffer, sizeof(buffer), "Inspecting memory @{:p}", (void*)window->offset);
         painter->draw_string(row, buffer, theme.codeString, UIAlign::left, 0);
         row.t += rowHeight;
         row.b += rowHeight;
         const char* header = "         0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F   0123456789ABCDEF";
         painter->draw_string(row, header, theme.codeComment, UIAlign::left, 0);
         row.t += rowHeight;
         row.b += rowHeight;
      }

      if (rowCount > 0 && rowCount * 16 > window->loadedBytes.size()) {
         window->loadedBytes.clear();

         for (size_t i = 0; i < (size_t)rowCount * 16 / 8; i++) {
            std_format_to_n(buffer, sizeof(buffer), "x/8xb 0x{:x}", window->offset + i * 8);
            auto res = EvaluateCommand(buffer);

            bool error = true;

            if (!res.contains("Cannot access memory")) {
               const char* position = strchr(res.c_str(), ':');

               if (position) {
                  position++;

                  for (int i = 0; i < 8; i++) {
                     window->loadedBytes.push_back(strtol(position, nullptr, 0));
                  }

                  error = false;
               }
            }

            if (error) {
               for (int i = 0; i < 8; i++) {
                  window->loadedBytes.push_back(-1);
               }
            }
         }
      }

      while (row.t < painter->_clip.b) {
         int position = 0;

         UI*   ui    = el->ui();
         auto& theme = el->theme();
         std_format_to_n(buffer, sizeof(buffer), "{:8X} ", (uint32_t)(address & 0xFFFFFFFF));
         painter->draw_string(row, buffer, theme.codeComment, UIAlign::left, 0);
         UIRectangle r          = row + UIRectangle(ui->string_width(buffer), 0, 0, 0);
         int         glyphWidth = ui->string_width("a");

         for (int i = 0; i < 16; i++) {
            if (address + i >= window->offset + window->loadedBytes.size() ||
                window->loadedBytes[address + i - window->offset] < 0) {
               painter->draw_glyph(r.l + position, r.t, '?', theme.codeOperator);
               position += glyphWidth;
               painter->draw_glyph(r.l + position, r.t, '?', theme.codeOperator);
               position += glyphWidth;
            } else {
               const char* hexChars = "0123456789ABCDEF";
               uint8_t     byte     = window->loadedBytes[address + i - window->offset];
               painter->draw_glyph(r.l + position, r.t, hexChars[(byte & 0xF0) >> 4], theme.codeNumber);
               position += glyphWidth;
               painter->draw_glyph(r.l + position, r.t, hexChars[(byte & 0x0F) >> 0], theme.codeNumber);
               position += glyphWidth;

               if (byte >= 0x20 && byte < 0x7F) {
                  painter->draw_glyph(r.l + (49 + i) * glyphWidth, r.t, byte, theme.codeString);
               }
            }

            position += glyphWidth;
         }

         row.t += rowHeight;
         row.b += rowHeight;
         address += 0x10;
      }
   } else if (msg == UIMessage::LAYOUT) {
      UIRectangle bounds = el->_bounds + ui_rect_1i(10);
      window->gotoButton->move(UIRectangle(bounds.r - window->gotoButton->message(UIMessage::GET_WIDTH, 0, 0), bounds.r,
                                           bounds.t,
                                           bounds.t + window->gotoButton->message(UIMessage::GET_HEIGHT, 0, 0)),
                               false);
   } else if (msg == UIMessage::MOUSE_WHEEL) {
      window->offset += di / 72 * 0x10;
      window->loadedBytes.clear();
      window->repaint(nullptr);
   }

   return 0;
}

void MemoryWindowUpdate(const char* data, UIElement* el) {
   MemoryWindow* window = (MemoryWindow*)el;
   window->loadedBytes.clear();
   el->repaint(NULL);
}

void MemoryWindowGotoButtonInvoke(void* cp) {
   MemoryWindow* window     = (MemoryWindow*)cp;
   char*         expression = nullptr;

   if (windowMain->show_dialog(0, "Enter address expression:\n%t\n%f%b%b", &expression, "Goto", "Cancel") == "Goto") {
      char buffer[4096];
      std_format_to_n(buffer, sizeof(buffer), "py gf_valueof(['{}'],' ')", expression);
      auto        res    = EvaluateCommand(buffer);
      const char* result = res.c_str();

      if (result && ((*result == '(' && isdigit(result[1])) || isdigit(*result))) {
         if (*result == '(')
            result++;
         uint64_t address = strtol(result, nullptr, 0);

         if (address) {
            window->loadedBytes.clear();
            window->offset = address & ~0xF;
            window->repaint(nullptr);
         } else {
            windowMain->show_dialog(0, "Cannot access memory at address 0.\n%f%b", "OK");
         }
      } else {
         windowMain->show_dialog(0, "Expression did not evaluate to an address.\n%f%b", "OK");
      }
   }

   free(expression);
}

UIElement* MemoryWindowCreate(UIElement* parent) {
   return new MemoryWindow(parent);
}

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
      auto&       theme   = el->theme();
      uint32_t    color   = ((ViewWindowColorSwatch*)el)->color;
      UIPainter*  painter = (UIPainter*)dp;
      const char* message = "Col: ";

      painter->draw_string(el->_bounds, message, theme.text, UIAlign::left, nullptr);
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
      UI*   ui                       = el->ui();
      auto& theme                    = el->theme();
      auto [glyphWidth, glyphHeight] = ui->string_dims("A");
      UIPainter* painter             = (UIPainter*)dp;

      for (int i = 0; i < grid->h; i++) {
         for (int j = 0; j < grid->w; j++) {
            if (grid->grid_type == grid_type_t::char_t) {
               char c = grid->data()[i * grid->w + j];
               if (!c)
                  continue;
               painter->draw_glyph(el->_bounds.l + j * glyphWidth - grid->hScroll->position(),
                                   el->_bounds.t + i * glyphHeight - grid->vScroll->position(), c, theme.text);
            } else if (grid->grid_type == grid_type_t::float_t || grid->grid_type == grid_type_t::double_t) {
               double f = grid->grid_type == grid_type_t::double_t ? ((double*)grid->data())[i * grid->w + j]
                                                                   : (double)((float*)grid->data())[i * grid->w + j];
               char   buffer[64];
               std_format_to_n(buffer, sizeof(buffer), "{:f}", f);
               UIRectangle rectangle =
                  UIRectangle(j * glyphWidth * 14, (j + 1) * glyphWidth * 14, i * glyphHeight, (i + 1) * glyphHeight);
               UIRectangle offset = UIRectangle(el->_bounds.l - (int)grid->hScroll->position(),
                                                el->_bounds.t - (int)grid->vScroll->position());
               painter->draw_string(rectangle + offset, buffer, theme.text, UIAlign::right, nullptr);
            }
         }
      }

      int scrollBarSize = ui_size::scroll_bar * el->_window->scale();
      painter->draw_block(
         UIRectangle(el->_bounds.r - scrollBarSize, el->_bounds.r, el->_bounds.b - scrollBarSize, el->_bounds.b),
         theme.panel1);
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
   auto& theme                    = ui->theme();

   for (int i = 0; i < display->length; i++) {
      if (x + glyphWidth > clientBounds.r) {
         x = clientBounds.l + glyphWidth;
         y += glyphHeight;
         if (painter)
            painter->draw_glyph(clientBounds.l, y, '>', theme.codeComment);
      }

      if (display->data[i] < 0x20 || display->data[i] >= 0x7F) {
         if (display->data[i] == '\n') {
            if (painter)
               painter->draw_glyph(x, y, '\\', theme.codeComment);
            x += glyphWidth;
            if (painter)
               painter->draw_glyph(x, y, 'n', theme.codeComment);
            x = clientBounds.l;
            y += glyphHeight;
         } else if (display->data[i] == '\t') {
            if (painter)
               painter->draw_glyph(x, y, '\\', theme.codeNumber);
            x += glyphWidth;
            if (painter)
               painter->draw_glyph(x, y, 't', theme.codeNumber);
            x += glyphWidth;
         } else {
            const char* hexChars = "0123456789ABCDEF";
            if (painter)
               painter->draw_glyph(x, y, '<', theme.codeNumber);
            x += glyphWidth;
            if (painter)
               painter->draw_glyph(x, y, hexChars[(display->data[i] & 0xF0) >> 4], theme.codeNumber);
            x += glyphWidth;
            if (painter)
               painter->draw_glyph(x, y, hexChars[(display->data[i] & 0x0F) >> 0], theme.codeNumber);
            x += glyphWidth;
            if (painter)
               painter->draw_glyph(x, y, '>', theme.codeNumber);
            x += glyphWidth;
         }
      } else {
         if (painter)
            painter->draw_glyph(x, y, display->data[i], theme.codeDefault);
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
      auto& theme = el->theme();
      static_cast<UIPainter*>(dp)->draw_block(el->_bounds, theme.codeBackground);
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
   UIElement* watchElement = ctx.InterfaceWindowSwitchToAndFocus("Watch");
   if (!watchElement)
      return;
   WatchWindow* w = (WatchWindow*)watchElement->_cp;
   if (w->textbox)
      return;
   if (w->selectedRow > w->rows.size() || !w->rows.size())
      return;
   const shared_ptr<Watch>& watch = w->rows[w->selectedRow == w->rows.size() ? w->selectedRow - 1 : w->selectedRow];
   if (!watch)
      return;
   if (!cp)
      cp = ctx.InterfaceWindowSwitchToAndFocus("View");
   if (!cp)
      return;

   // Destroy the previous panel contents.
   UIElement* panel = (UIElement*)cp;
   panel->destroy_descendents();
   panel->add_button(0, "View (Ctrl+Shift+V)").on_click([panel](UIButton&) { ViewWindowView(panel); });

   // Get information about the watch expression.
   char type[256], buffer[256];
   char oldFormat = watch->format;
   watch->format  = 0;

   auto res = WatchEvaluate("gf_typeof", watch);
   resize_to_lf(res);
   std_format_to_n(type, sizeof(type), "{}", res);
   std_format_to_n(buffer, sizeof(buffer), "Type: {}", type);
   panel->add_label(0, buffer);

   res = WatchEvaluate("gf_valueof", watch);
   resize_to_lf(res);
   watch->format = oldFormat;
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
         res = WatchEvaluate("gf_addressof", watch);
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
      auto res = WatchGetAddress(watch);
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

void ViewWindowView() {
   ViewWindowView(nullptr);
}

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
      zoomOut->_user_proc   = WaveformDisplayZoomButtonMessage;
      zoomIn->_user_proc    = WaveformDisplayZoomButtonMessage;
      normalize->_user_proc = WaveformDisplayNormalizeButtonMessage;
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
      display->repaint(NULL);
   } else if (msg == UIMessage::MOUSE_MOVE) {
      display->repaint(NULL);
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
      el->repaint(NULL);
   } else if (msg == UIMessage::PAINT) {
      UIRectangle client = el->_bounds;
      auto&       theme  = el->theme();
      client.b -= display->scrollBar->_bounds.height();

      UIPainter*  painter = (UIPainter*)dp;
      UIRectangle oldClip = painter->_clip;
      painter->_clip      = intersection(client, painter->_clip);
      int ym              = (client.t + client.b) / 2;
      int h2              = (client.b - client.t) / 2;
      int yp              = ym;
      painter->draw_block(painter->_clip, theme.panel1);
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
               WaveformDisplayDrawVerticalLineWithTranslucency(painter, r, theme.text, alpha);
            }
         }
      } else {
         for (size_t channel = 0; channel < display->channels; channel++) {
            yp = ym + h2 * yScale * samples[channel + 0];

            for (int32_t i = 0; i < sampleCount; i++) {
               int32_t x0 = (int)((float)i / sampleCount * client.width()) + client.l;
               int32_t x1 = (int)((float)(i + 1) / sampleCount * client.width()) + client.l;
               int32_t y  = ym + h2 * yScale * samples[channel + display->channels * (int)i];
               painter->draw_line(x0, yp, x1, y, theme.text);
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

            painter->draw_string(stringRectangle, buffer, theme.text, UIAlign::right, NULL);

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

WaveformDisplay* WaveformDisplayCreate(UIElement* parent, uint32_t flags) {
   return new WaveformDisplay(parent, flags);
}

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
   static char* path = NULL;
   auto         result =
      windowMain->show_dialog(0, "Save to file       \nPath:\n%t\n%f%b%b%b", &path, "Save", "Save and open", "Cancel");
   if (result == "Cancel")
      return;
   FILE* f = fopen(path, "wb");
   if (!f) {
      windowMain->show_dialog(0, "Unable to open file for writing.\n%f%b", "OK");
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

      UIMDIChild* window = &dataWindow->add_mdichild(UIMDIChild::CLOSE_BUTTON, UIRectangle(0), "Waveform")
                               .set_user_proc(WaveformViewerWindowMessage)
                               .set_cp(viewer);
      viewer->autoToggle = &window->add_button(UIButton::SMALL | UIElement::non_client_flag, "Auto")
                               .set_cp((void*)WaveformViewerAutoUpdateCallback)
                               .set_user_proc(DataViewerAutoUpdateButtonMessage);
      window->add_button(UIButton::SMALL | UIElement::non_client_flag, "Refresh")
         .set_user_proc(WaveformViewerRefreshMessage);
      owner = window;

      UIPanel* panel              = &owner->add_panel(UIPanel::EXPAND);
      viewer->labelPanel          = &panel->add_panel(UIPanel::COLOR_1 | UIElement::v_fill);
      viewer->label               = &viewer->labelPanel->add_label(UIElement::h_fill, {});
      viewer->display             = WaveformDisplayCreate(panel, UIElement::v_fill);
      viewer->display->_user_proc = WaveformViewerDisplayMessage;
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
   dataWindow->refresh();

   free(samples);
}

void WaveformAddDialog() {
   static char *pointer = nullptr, *sampleCount = nullptr, *channels = nullptr;

   auto result = windowMain->show_dialog(
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

void Context::RegisterExtensions() {
   interfaceWindows["Prof"]   = {.create = ProfWindowCreate, .update = ProfWindowUpdate, .alwaysUpdate = true};
   interfaceWindows["Memory"] = {MemoryWindowCreate, MemoryWindowUpdate};
   interfaceWindows["View"]   = {ViewWindowCreate, ViewWindowUpdate};

   interfaceDataViewers.push_back({"Add waveform...", WaveformAddDialog});
   interfaceCommands.push_back({
      .label = nullptr, .shortcut{.code = UI_KEYCODE_LETTER('V'), .ctrl = true, .shift = true, .invoke = []() {
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
   ctx.programRunning = false;

   if (ctx.firstUpdate) {
      (void)EvaluateCommand(pythonCode);

      char path[PATH_MAX];
      std_format_to_n(path, sizeof(path), "{}/.config/gf2_watch.txt", getenv("HOME"));
      std::string s    = LoadFile(path);
      const char* data = s.c_str();

      while (data && restoreWatchWindow) {
         const char* end = strchr(data, '\n');
         if (!end)
            break;
         WatchAddExpression2(string_view{data, static_cast<size_t>(end - data)});
         data = end + 1;
      }

      ctx.firstUpdate = false;
   }

   if (WatchLoggerUpdate(*input))
      return;
   if (showingDisassembly)
      DisassemblyUpdateLine();

   if (!regex::match_stack_or_breakpoint_output(*input)) {
      // we don't want to call `DebuggerGetBreakpoints()` upon receiving the result of `DebuggerGetBreakpoints()`,
      // causing an infinite loop!!!
      DebuggerGetStack();
      DebuggerGetBreakpoints();
   }

   for (auto& [name, iw] : ctx.interfaceWindows) {
      InterfaceWindow* window = &iw;
      if (!window->update || !window->el)
         continue;
      if (!window->alwaysUpdate && ElementHidden(window->el))
         window->queuedUpdate = true;
      else
         window->update(input->c_str(), window->el);
   }

   DataViewersUpdateAll();

   if (displayOutput) {
      displayOutput->insert_content(*input, false);
      displayOutput->refresh();
   }

   if (trafficLight)
      trafficLight->repaint(nullptr);
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

auto gdb_invoker(string_view cmd) {
   return [cmd]() {
      CommandSendToGDB(cmd);
      return true;
   };
}

void Context::InterfaceAddBuiltinWindowsAndCommands() {
   interfaceWindows["Stack"]       = {StackWindowCreate, StackWindowUpdate};
   interfaceWindows["Source"]      = {SourceWindowCreate, SourceWindowUpdate};
   interfaceWindows["Breakpoints"] = {BreakpointsWindowCreate, BreakpointsWindowUpdate};
   interfaceWindows["Registers"]   = {RegistersWindowCreate, RegistersWindowUpdate};
   interfaceWindows["Watch"]       = {WatchWindowCreate, WatchWindowUpdate, WatchWindowFocus};
   interfaceWindows["Locals"]      = {LocalsWindowCreate, WatchWindowUpdate, WatchWindowFocus};
   interfaceWindows["Commands"]    = {CommandsWindowCreate, nullptr};
   interfaceWindows["Data"]        = {DataWindowCreate, nullptr};
   interfaceWindows["Struct"]      = {StructWindowCreate, nullptr};
   interfaceWindows["Files"]       = {FilesWindowCreate, nullptr};
   interfaceWindows["Console"]     = {ConsoleWindowCreate, nullptr};
   interfaceWindows["Log"]         = {LogWindowCreate, nullptr};
   interfaceWindows["Thread"]      = {ThreadWindowCreate, ThreadWindowUpdate};
   interfaceWindows["Exe"]         = {ExecutableWindowCreate, nullptr};
   interfaceWindows["CmdSearch"]   = {CommandSearchWindowCreate, nullptr};

   interfaceDataViewers.push_back({"Add bitmap...", BitmapAddDialog});

   interfaceCommands.push_back({
      .label = "Run\tShift+F5", .shortcut{.code = UI_KEYCODE_FKEY(5), .shift = true, .invoke = gdb_invoker("r")}
   });
   interfaceCommands.push_back({
      .label = "Run paused\tCtrl+F5",
      .shortcut{.code = UI_KEYCODE_FKEY(5), .ctrl = true, .invoke = gdb_invoker("start")}
   });
   interfaceCommands.push_back({
      .label = "Kill\tF3", .shortcut{.code = UI_KEYCODE_FKEY(3), .invoke = gdb_invoker("kill")}
   });
   interfaceCommands.push_back({
      .label = "Restart GDB\tCtrl+R",
      .shortcut{.code = UI_KEYCODE_LETTER('R'), .ctrl = true, .invoke = gdb_invoker("gf-restart-gdb")}
   });
   interfaceCommands.push_back({
      .label = "Connect\tF4", .shortcut{.code = UI_KEYCODE_FKEY(4), .invoke = gdb_invoker("target remote :1234")}
   });
   interfaceCommands.push_back({
      .label = "Continue\tF5", .shortcut{.code = UI_KEYCODE_FKEY(5), .invoke = gdb_invoker("c")}
   });
   interfaceCommands.push_back({
      .label = "Step over\tF10", .shortcut{.code = UI_KEYCODE_FKEY(10), .invoke = gdb_invoker("gf-next")}
   });
   interfaceCommands.push_back({
      .label = "Step out of block\tShift+F10",
      .shortcut{.code = UI_KEYCODE_FKEY(10), .shift = true, .invoke = gdb_invoker("gf-step-out-of-block")}
   });
   interfaceCommands.push_back({
      .label = "Step in\tF11", .shortcut{.code = UI_KEYCODE_FKEY(11), .invoke = gdb_invoker("gf-step")}
   });
   interfaceCommands.push_back({
      .label = "Step into outer\tShift+F8",
      .shortcut{.code = UI_KEYCODE_FKEY(8), .shift = true, .invoke = gdb_invoker("gf-step-into-outer")}
   });
   interfaceCommands.push_back({
      .label = "Step out\tShift+F11",
      .shortcut{.code = UI_KEYCODE_FKEY(11), .shift = true, .invoke = gdb_invoker("finish")}
   });
   interfaceCommands.push_back({
      .label = "Reverse continue\tCtrl+Shift+F5",
      .shortcut{.code = UI_KEYCODE_FKEY(5), .ctrl = true, .shift = true, .invoke = gdb_invoker("reverse-continue")}
   });
   interfaceCommands.push_back({
      .label = "Reverse step over\tCtrl+Shift+F10",
      .shortcut{.code = UI_KEYCODE_FKEY(10), .ctrl = true, .shift = true, .invoke = gdb_invoker("reverse-next")}
   });
   interfaceCommands.push_back({
      .label = "Reverse step in\tCtrl+Shift+F11",
      .shortcut{.code = UI_KEYCODE_FKEY(11), .ctrl = true, .shift = true, .invoke = gdb_invoker("reverse-step")}
   });
   interfaceCommands.push_back({
      .label = "Break\tF8", .shortcut{.code = UI_KEYCODE_FKEY(8), .invoke = [&]() {
                                         ctx.InterruptGdb(0);
                                         return true;
                                      }}
   });
   interfaceCommands.push_back({
      .label = "Toggle breakpoint\tF9", .shortcut{.code = UI_KEYCODE_FKEY(9), .invoke = []() {
                                                     CommandToggleBreakpoint();
                                                     return true;
                                                  }}
   });
   interfaceCommands.push_back({
      .label = "Sync with gvim\tF2", .shortcut{.code = UI_KEYCODE_FKEY(2), .invoke = CommandSyncWithGvim}
   });
   interfaceCommands.push_back({
      .label = "Ask GDB for PWD\tCtrl+Shift+P",
      .shortcut{.code = UI_KEYCODE_LETTER('P'), .ctrl = true, .shift = true, .invoke = gdb_invoker("gf-get-pwd")}
   });
   interfaceCommands.push_back({
      .label = "Set disassembly mode\tCtrl+M",
      .shortcut{.code = UI_KEYCODE_LETTER('M'), .ctrl = true, .invoke = CommandSetDisassemblyMode}
   });
   interfaceCommands.push_back({
      .label = "Inspect line", .shortcut{.code = UIKeycode::BACKTICK, .invoke = CommandInspectLine}
   });
   interfaceCommands.push_back({
      .label = nullptr, .shortcut{.code = UI_KEYCODE_LETTER('G'), .ctrl = true, .invoke = []() {
                                     CommandWatchViewSourceAtAddress();
                                     return true;
                                  }}
   });
   interfaceCommands.push_back({
      .label = nullptr,
      .shortcut{.code = UI_KEYCODE_LETTER('P'), .ctrl = true, .shift = false, .invoke = CommandPreviousCommand}
   });
   interfaceCommands.push_back({
      .label = nullptr,
      .shortcut{.code = UI_KEYCODE_LETTER('N'), .ctrl = true, .shift = false, .invoke = CommandNextCommand}
   });
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
      assert(ctx.logWindow);
      ctx.logWindow->insert_content(*buffer, false);
      ctx.logWindow->refresh();
   });
}

void Context::InterfaceShowMenu(UIButton* self) {
   UIMenu& menu = self->ui()->create_menu((UIElement*)self, UIMenu::PLACE_ABOVE | UIMenu::NO_SCROLL);

   for (const auto& ic : interfaceCommands) {
      if (ic.label)
         menu.add_item(0, ic.label, [&](UIButton&) { ic.shortcut.invoke(); });
   }
   menu.show();
}

UIElement* Context::InterfaceWindowSwitchToAndFocus(string_view target_name) {
   for (auto& [name, w] : interfaceWindows) {
      if (!w.el)
         continue;
      if (target_name != name)
         continue;

      if ((w.el->_flags & UIElement::hide_flag) && w.el->_parent->get_class_proc() == UITabPane::_ClassMessageProc) {
         UITabPane* tabPane = (UITabPane*)w.el->_parent;

         for (uint32_t i = 0; i < tabPane->_children.size(); i++) {
            if (tabPane->_children[i] == w.el) {
               tabPane->set_active(i);
               break;
            }
         }

         tabPane->refresh();
      }

      if (w.focus) {
         w.focus(w.el);
      } else if (w.el->_flags & UIElement::tab_stop_flag) {
         w.el->focus();
      }

      return w.el;
   }

   windowMain->show_dialog(0, "Couldn't find the window '%s'.\n%f%B", target_name, "OK");
   return nullptr;
}

int MainWindowMessageProc(UIElement*, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::WINDOW_ACTIVATE) {
      DisplaySetPosition(currentFileFull, displayCode->current_line(), false);
   } else {
      for (const auto& msgtype : receiveMessageTypes) {
         if (msgtype.msg == msg) {
            msgtype.callback(std::unique_ptr<std::string>(static_cast<std::string*>(dp)));
            break;
         }
      }
   }

   return 0;
}

int InterfaceTabPaneMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::LAYOUT) {
      el->_class_proc(el, msg, di, dp);

      for (auto& [name, w] : ctx.interfaceWindows) {
         if (w.el && (~w.el->_flags & UIElement::hide_flag) && w.queuedUpdate) {
            w.queuedUpdate = false;
            w.update("", w.el);
            w.el->move(w.el->_bounds, false);
         }
      }

      return 1;
   }

   return 0;
}

// !!! Oh no! this actually advances the global pointer `gfc.layout_string`
// ------------------------------------------------------------------------
const char* InterfaceLayoutNextToken(const char* expected = nullptr) {
   static char buffer[32];
   char*       out = buffer;

   while (isspace(*gfc.layout_string)) {
      gfc.layout_string++;
   }

   char first = *gfc.layout_string;

   if (first == 0) {
      *out = 0;
   } else if (first == ',' || first == '(' || first == ')') {
      out[0] = first;
      out[1] = 0;
      gfc.layout_string++;
   } else if (isalnum(first)) {
      for (uintptr_t i = 0; i < sizeof(buffer) - 1; i++) {
         if (isalnum(*gfc.layout_string)) {
            *out++ = *gfc.layout_string++;
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

void Context::InterfaceLayoutCreate(UIElement* parent) {
   const char* token = InterfaceLayoutNextToken();

   if (0 == strcmp("h", token) || 0 == strcmp("v", token)) {
      uint32_t flags = UIElement::v_fill | UIElement::h_fill;
      if (*token == 'v')
         flags |= UIElement::vertical_flag;
      InterfaceLayoutNextToken("(");
      UIElement* container = &parent->add_splitpane(flags, sv_atoi(InterfaceLayoutNextToken("#")) * 0.01f);
      InterfaceLayoutNextToken(",");
      InterfaceLayoutCreate(container);
      InterfaceLayoutNextToken(",");
      InterfaceLayoutCreate(container);
      InterfaceLayoutNextToken(")");
   } else if (0 == strcmp("t", token)) {
      InterfaceLayoutNextToken("(");
      char* copy = strdup(gfc.layout_string); // watch out, gfc.layout_string is modified by InterfaceLayoutNextToken
      for (uintptr_t i = 0; copy[i]; i++)
         if (copy[i] == ',')
            copy[i] = '\t';
         else if (copy[i] == ')')
            copy[i] = 0;
      UIElement* container =
         &parent->add_tabpane(UIElement::v_fill | UIElement::h_fill, copy).set_user_proc(InterfaceTabPaneMessage);
      free(copy);
      InterfaceLayoutCreate(container);

      while (true) {
         token = InterfaceLayoutNextToken();

         if (0 == strcmp(token, ",")) {
            InterfaceLayoutCreate(container);
         } else if (0 == strcmp(token, ")")) {
            break;
         } else {
            print(std::cerr, "Error: Invalid layout string! Expected ',' or ')' in tab container list; got '{}'.\n",
                  token);
            exit(1);
         }
      }
   } else {
      if (auto it = interfaceWindows.find(token); it != interfaceWindows.end()) {
         auto& [name, w] = *it;
         w.el            = w.create(parent);
      } else {
         print(std::cerr, "Error: Invalid layout string! The window '{}' was not found.\n", token);
         exit(1);
      }
   }
}

unique_ptr<UI> Context::GfMain(int argc, char** argv) {
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
      ctx.KillGdb();
      exit(0);
   });
   std::signal(SIGPIPE, [](int) { print(std::cerr, "SIGPIPE Received - ignored.\n"); });

   // process command arguments and create updated version to pass to gdb
   // -------------------------------------------------------------------
   ctx.gdbArgv    = (char**)malloc(sizeof(char*) * (argc + 1));
   ctx.gdbArgv[0] = (char*)"gdb";
   memcpy(ctx.gdbArgv + 1, argv + 1, sizeof(argv) * argc);
   ctx.gdbArgc = argc;

   if (argc >= 2 && 0 == strcmp(argv[1], "--rr-replay")) {
      ctx.gdbArgv[0] = (char*)"rr";
      ctx.gdbArgv[1] = (char*)"replay";
      ctx.gdbPath    = "rr";
   }

   // load settings and initialize ui
   // -------------------------------
   getcwd(localConfigDirectory, sizeof(localConfigDirectory));
   std_format_to_n(globalConfigPath, sizeof(globalConfigPath), "{}/.config/gf2_config.ini", getenv("HOME"));
   std_format_to_n(localConfigPath, sizeof(localConfigPath), "{}/.project.gf", localConfigDirectory);

   UIConfig ui_config = ctx.SettingsLoad(true);

   ui_config.default_font_size = interface_font_size;

   auto ui = UI::initialise(ui_config); // sets `ui.default_font_path`

   // ui->_theme = uiThemeDark; // force it for now, overriding `gf2_config.ini` - should remove though!

   // create fonts for interface and code
   // -----------------------------------
   const auto& font_path = ui->default_font_path();
   code_font             = ui->create_font(font_path, code_font_size);
   ui->create_font(font_path, interface_font_size)->activate();

   windowMain = &ui->create_window(0, maximize ? UIWindow::MAXIMIZE : 0, "gf", window_width, window_height);
   windowMain->set_scale(ui_scale);
   windowMain->_user_proc = MainWindowMessageProc;

   for (const auto& ic : interfaceCommands) {
      if (!(int)ic.shortcut.code)
         continue;
      windowMain->register_shortcut(ic.shortcut);
   }

   switcherMain = &windowMain->add_switcher(0);
   InterfaceLayoutCreate(&switcherMain->add_panel(UIPanel::EXPAND));
   switcherMain->switch_to(switcherMain->_children[0]);

   if (*InterfaceLayoutNextToken()) {
      print(std::cerr, "Warning: Layout string has additional text after the end of the top-level entry.\n");
   }

   ui_config   = ctx.SettingsLoad(false);
   ui->theme() = ui_config._theme;

   DebuggerStartThread();
   CommandSyncWithGvim();
   return ui;
}

Context::Context() {
   InterfaceAddBuiltinWindowsAndCommands();
   RegisterExtensions();
}

int main(int argc, char** argv) {
   auto ui_ptr = ctx.GfMain(argc, argv);
   if (!ui_ptr)
      return 1;

   ui_ptr->message_loop();
   ctx.KillGdb();

   if (restoreWatchWindow && firstWatchWindow) {
      std_format_to_n(globalConfigPath, sizeof(globalConfigPath), "{}/.config/gf2_watch.txt", getenv("HOME"));
      FILE* f = fopen(globalConfigPath, "wb");

      if (f) {
         for (const auto& exp : firstWatchWindow->baseExpressions) {
            print(f, "{}\n", exp->key);
         }

         fclose(f);
      } else {
         print(std::cerr, "Warning: Could not save the contents of the watch window; '{}' was not accessible.\n",
               globalConfigPath);
      }
   }

   return 0;
}
