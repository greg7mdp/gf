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
      bool res = cv_.wait_for(lock, std::chrono::seconds(1), [this] { return !queue_.empty() || quit_; });

      if (!res || quit_) {       // !res means we hit the timeout
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

void print(const std::string_view str) {
    std::cout << str;
}

// Variadic template for print with format arguments
template<typename... Args>
void print(std::format_string<Args...> fmt, Args&&... args) {
    std::cout << std::format(fmt, std::forward<Args>(args)...);
}

template<typename... Args>
void print(std::ostream& stream, std::format_string<Args...> fmt, Args&&... args) {
    stream << std::format(fmt, std::forward<Args>(args)...);
}

template<typename... Args>
void print(FILE* f, std::format_string<Args...> fmt, Args&&... args) {
   std::string formatted = std::format(fmt, std::forward<Args>(args)...);
   fprintf(f, "%s", formatted.c_str());
}

template<class OutputIt, class... Args>
int std_format_to_n(OutputIt buffer, std::iter_difference_t<OutputIt> n,
                    std::format_string<Args...> fmt, Args&&... args) {
   auto max_chars = n-1;
   auto res = std::format_to_n(buffer, max_chars, fmt, std::forward<Args>(args)...);
   auto written =  std::min(res.size, max_chars);
   buffer[written] = '\0'; // adds terminator to buffer
   // fprintf(stderr, "%s\n", buffer);
   return written;
}

// ---------------------------------------------------------------------------------------------
//                              Data structures
// ---------------------------------------------------------------------------------------------
struct InterfaceCommand {
   const char* label = nullptr;
   UIShortcut  shortcut;
};

struct InterfaceWindow {
   UIElement* (*create)(UIElement* parent)              = nullptr;
   void (*update)(const char* data, UIElement* element) = nullptr;
   void (*focus)(UIElement* element)                    = nullptr;
   UIElement* element                                   = nullptr;
   bool       queuedUpdate                              = false;
   bool       alwaysUpdate                              = false;
   void (*config)(const char* key, const char* value)   = nullptr;
};

struct InterfaceDataViewer {
   const char* addButtonLabel = nullptr;
   void (*addButtonCallback)();
};

struct INIState {
   char*  buffer  = nullptr;
   char*  section = nullptr;
   char*  key     = nullptr;
   char*  value   = nullptr;
   size_t bytes = 0, sectionBytes = 0, keyBytes = 0, valueBytes = 0;
};

using str_unique_ptr = std::unique_ptr<char, decltype([](char* s) { free(s); })>;

struct ReceiveMessageType {
   UIMessage                           message;
   std::function<void(str_unique_ptr)> callback;
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
   bool                   programRunning = true; // true

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

   void DebuggerThread();
   void SettingsLoad(bool earlyPass);
   void InterfaceAddBuiltinWindowsAndCommands();
   void RegisterExtensions();
   void InterfaceShowMenu(UIButton* self);
   void InterfaceLayoutCreate(UIElement* parent);
   UIElement* InterfaceWindowSwitchToAndFocus(string_view name);
   unique_ptr<UI> GfMain(int argc, char** argv);
};

Context ctx;

// --------------------------------------------------------------------------------------------
FILE*                       commandLog = nullptr;
char                        emptyString;
const char*                 vimServerName   = "GVIM";
const char*                 logPipePath     = nullptr;
const char*                 controlPipePath = nullptr;
vector<INIState>            presetCommands;
char                        globalConfigPath[PATH_MAX];
char                        localConfigDirectory[PATH_MAX];
char                        localConfigPath[PATH_MAX];
const char*                 executablePath         = nullptr;
const char*                 executableArguments    = nullptr;
bool                        executableAskDirectory = true;
vector<ReceiveMessageType>  receiveMessageTypes;
char* layoutString = (char*)"v(75,h(80,Source,v(50,t(Exe,Breakpoints,Commands,Struct),t(Stack,Files,Thread,CmdSearch)))"
                            ",h(65,Console,t(Watch,Locals,Registers,Data)))";
const char*  fontPath          = nullptr;
int          fontSizeCode      = 13;
int          fontSizeInterface = 11;
int          window_width      = 800;
int          window_height     = 600;
float        uiScale           = 1;
bool         selectableSource  = true;
bool         restoreWatchWindow;
WatchWindow* firstWatchWindow = nullptr;
bool         maximize;
bool         confirmCommandConnect = true, confirmCommandKill = true;
int          backtraceCountLimit = 50;
UIMessage    msgReceivedData, msgReceivedLog, msgReceivedControl, msgReceivedNext = (UIMessage)(UIMessage::USER_PLUS_1);

UIConfig ui_config = {.rfu = true};

// Current file and line:

char   currentFile[PATH_MAX];
char   currentFileFull[PATH_MAX];
int    currentLine;
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

UIFont* fontCode = nullptr;

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

bool       DisplaySetPosition(const char* file, int line, bool useGDBToGetFullPath);
void       WatchAddExpression2(string_view string);
int        WatchWindowMessage(UIElement* element, UIMessage message, int di, void* dp);
void       CommandInspectLine();

// ------------------------------------------------------
// Utilities:
// ------------------------------------------------------

inline uint64_t Hash(const uint8_t* key, size_t keyBytes) {
   uint64_t hash = 0xCBF29CE484222325;
   for (uintptr_t i = 0; i < keyBytes; i++)
      hash = (hash ^ key[i]) * 0x100000001B3;
   return hash;
}

int StringFormat(char* buffer, size_t bufferSize, const char* format, ...) {
   int length = 0;
   va_list arguments;

   va_start(arguments, format);
   length = vsnprintf(buffer, bufferSize, format, arguments);
   va_end(arguments);

   if (length > (int)bufferSize) {
      // HACK This could truncate a UTF-8 codepoint.
      length = bufferSize;
   }

   return length;
}

vector<char> LoadFile(const char* path, size_t* _bytes) {
   FILE* f = fopen(path, "rb");

   if (!f) {
      if (_bytes)
         *_bytes = 0;
      return vector<char>{'\0'};
   }

   fseek(f, 0, SEEK_END);
   size_t bytes = ftell(f);
   fseek(f, 0, SEEK_SET);
   vector<char> buffer(bytes + 1);

   buffer[bytes] = 0;
   fread(buffer.data(), 1, bytes, f);
   fclose(f);
   if (_bytes)
      *_bytes = bytes;

   return buffer;
}

bool INIParse(INIState* s) {
#define INI_READ(destination, counter, c1, c2)              \
   s->destination = s->buffer, s->counter = 0;              \
   while (s->bytes && *s->buffer != c1 && *s->buffer != c2) \
      s->counter++, s->buffer++, s->bytes--;                \
   if (s->bytes && *s->buffer == c1)                        \
      s->buffer++, s->bytes--;

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

int ModifiedRowMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::PAINT) {
      UIDrawBorder((UIPainter*)dp, element->bounds, ui->theme.selected, UIRectangle(2));
   }

   return 0;
}

int TrafficLightMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::PAINT) {
      UIDrawRectangle((UIPainter*)dp, element->bounds, ctx.programRunning ? ui->theme.accent1 : ui->theme.accent2,
                      ui->theme.border, UIRectangle(1));
   }

   return 0;
}

int SourceFindEndOfBlock() {
   int num_lines = (int)displayCode->num_lines();

   if (!currentLine || currentLine - 1 >= (int)num_lines)
      return -1;

   int tabs = 0;

   for (size_t i = 0; i < displayCode->lines[currentLine - 1].bytes; i++) {
      if (isspace(displayCode->content[displayCode->lines[currentLine - 1].offset + i]))
         tabs++;
      else
         break;
   }

   for (int j = currentLine; j < num_lines; j++) {
      int t = 0;

      for (int i = 0; i < (int)displayCode->lines[j].bytes - 1; i++) {
         if (isspace(displayCode->content[displayCode->lines[j].offset + i]))
            t++;
         else
            break;
      }

      if (t < tabs && displayCode->content[displayCode->lines[j].offset + t] == '}') {
         return j + 1;
      }
   }

   return -1;
}

bool SourceFindOuterFunctionCall(char** start, char** end) {
   int num_lines = (int)displayCode->num_lines();

   if (!currentLine || currentLine - 1 >= num_lines)
      return false;
   size_t offset = displayCode->lines[currentLine - 1].offset;
   bool   found  = false;

   // Look forwards for the end of the call ");".

   size_t num_chars = displayCode->size() ;
   while (offset < num_chars - 1) {
      if (displayCode->content[offset] == ')' && displayCode->content[offset + 1] == ';') {
         found = true;
         break;
      } else if (displayCode->content[offset] == ';' || displayCode->content[offset] == '{') {
         break;
      }

      offset++;
   }

   if (!found)
      return false;

   // Look backwards for the matching bracket.

   int level = 0;

   while (offset > 0) {
      if (displayCode->content[offset] == ')') {
         level++;
      } else if (displayCode->content[offset] == '(') {
         level--;
         if (level == 0)
            break;
      }

      offset--;
   }

   if (level)
      return false;

   *start = *end = &displayCode->content[offset];
   found         = false;
   offset--;

   // Look backwards for the start of the function name.
   // TODO Support function pointers.

   while (offset > 0) {
      char c = displayCode->content[offset];

      if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == ' ' || (c >= '0' && c <= '9')) {
         // Part of the function name.
         offset--;
      } else {
         *start = &displayCode->content[offset + 1];
         found  = true;
         break;
      }
   }

   return found;
}

UIMessage ReceiveMessageRegister(std::function<void(str_unique_ptr)> callback) {
   receiveMessageTypes.push_back({.message = msgReceivedNext, .callback = std::move(callback)});
   msgReceivedNext = (UIMessage)((uint32_t)msgReceivedNext + 1);
   return receiveMessageTypes.back().message;
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
            UIWindowPostMessage(windowMain, msgReceivedLog, strdup(buffer));

         if (catBuffer.size() + count + 1 > catBuffer.capacity())
            catBuffer.reserve(catBuffer.capacity() * 2);
         catBuffer.insert(catBuffer.end(), buffer, buffer + count);

         // wait till we get the prompt again so we know we received the complete output
         // ----------------------------------------------------------------------------
         if (!catBuffer.contains("(gdb) "))
            continue;

         // print("================ got ({}) catBuffer=\"{}\"\n", evaluateMode, catBuffer);

         // Notify the main thread we have data.
         // ------------------------------------
         if (evaluateMode) {
            evaluateResultQueue.push(std::move(catBuffer));
            catBuffer.clear();
            evaluateMode = false;
         } else {
            UIWindowPostMessage(windowMain, msgReceivedData, strdup(catBuffer.c_str()));
         }

         catBuffer.clear();
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
   if (synchronous) {
      ctx.InterruptGdb();
      ctx.evaluateMode = true;
   } else if (ctx.programRunning) {
      ctx.InterruptGdb(0);
   }

   ctx.programRunning = true;

   if (trafficLight)
      trafficLight->Repaint(nullptr);

   // std::cout << "sending: " << string << '\n';

   if (echo && displayOutput) {
      UICodeInsertContent(displayOutput, command, false);
      displayOutput->Refresh();
   }

   ctx.SendToGdb(command);

   if (synchronous) {
      bool quit = !ctx.evaluateResultQueue.pop(res);
      if (!res) {
         print("Hit timeout on command \"{}\"\n", command);
         res = std::string{}; // in synchronous mode we always return a (possibly empty) string
      }
      ctx.programRunning = false;
      if (!quit && trafficLight)
         trafficLight->Repaint(nullptr);
   }
   // print("{} ==> {}\n", command, res ? *res : "???"s);
   return res;
}

std::string EvaluateCommand(string_view command, bool echo = false) {
   auto res =  *std::move(DebuggerSend(command, echo, true));
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
      char  input[256];
      input[fread(input, 1, sizeof(input) - 1, file)] = 0;
      UIWindowPostMessage(windowMain, msgReceivedControl, strdup(input));
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
      std_format_to_n(entry.function, sizeof(entry.function), "{:.{}}", functionName, position - functionName);

      const char* file = strstr(position, " at ");

      if (file && file < next) {
         file += 4;
         const char* end = file;
         while (*end != '\n' && end < next)
            end++;
         std_format_to_n(entry.location, sizeof(entry.location), "{:.{}}", file, end - file);
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
         std_format_to_n(breakpoint.condition, sizeof(breakpoint.condition), "{:.{}}", condition, end - condition);
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
            std_format_to_n(breakpoint.file, sizeof(breakpoint.file), "{:.{}}", file, end - file);
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
   auto buffer = std::format("complete {}{}", addPrintPrefix ? "p " : "",
                             textbox->text().substr(0, lastKeyWasTab ? completer->lastTabBytes : (int)textbox->bytes));
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
      completer->lastTabBytes        = textbox->bytes;
   }

   while (start && end && memcmp(start + (addPrintPrefix ? 2 : 0), textbox->string, completer->lastTabBytes)) {
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
      UITextboxClear(textbox, false);
      UITextboxReplace(textbox, {start, static_cast<size_t>(end - start)}, false);
      textbox->Refresh();
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
      char *start, *end;
      bool  found = SourceFindOuterFunctionCall(&start, &end);

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
               UICodeInsertContent(displayOutput, std::format("New working directory: {}", pwd), false);
               displayOutput->Refresh();
            }
         }
         return {};
      }

      UIDialogShow(windowMain, 0, "Couldn't get the working directory.\n%f%B", "OK");
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
               UICodeInsertContent(displayOutput, *res, false);
            if (end)
               position = end + 1;
            else
               break;
         }

         if (displayOutput)
            displayOutput->Refresh();
         free(copy);
         break;
      }
   } else if (command == "gf-inspect-line") {
      CommandInspectLine();
   } else if (command == "target remote :1234" && confirmCommandConnect &&
              0 == strcmp("Cancel",
                          UIDialogShow(windowMain, 0, "Connect to remote target?\n%f%B%C", "Connect", "Cancel"))) {
   } else if (command == "kill" && confirmCommandKill &&
              0 == strcmp("Cancel", UIDialogShow(windowMain, 0, "Kill debugging target?\n%f%B%C", "Kill", "Cancel"))) {
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

void CommandSyncWithGvim() {
   char buffer[1024];
   std_format_to_n(buffer, sizeof(buffer), "vim --servername {} --remote-expr \"execute(\\\"ls\\\")\" | grep %%",
                    vimServerName);
   FILE* file = popen(buffer, "r");
   if (!file)
      return;
   buffer[fread(buffer, 1, 1023, file)] = 0;
   pclose(file);
   char* name = strchr(buffer, '"');
   if (!name)
      return;
   char* nameEnd = strchr(++name, '"');
   if (!nameEnd)
      return;
   *nameEnd   = 0;
   char* line = nameEnd + 1;
   while (!isdigit(*line) && *line)
      line++;
   if (!line)
      return;
   int  lineNumber = sv_atoi(line);
   char buffer2[PATH_MAX];

   if (name[0] != '/' && name[0] != '~') {
      char buffer[1024];
      std_format_to_n(buffer, sizeof(buffer), "vim --servername {} --remote-expr \"execute(\\\"pwd\\\")\" | grep '/'",
                       vimServerName);
      FILE* file = popen(buffer, "r");
      if (!file)
         return;
      buffer[fread(buffer, 1, 1023, file)] = 0;
      pclose(file);
      if (!strchr(buffer, '\n'))
         return;
      *strchr(buffer, '\n') = 0;
      std_format_to_n(buffer2, sizeof(buffer2), "{}/{}", buffer, name);
   } else {
      strcpy(buffer2, name);
   }

   DisplaySetPosition(buffer2, lineNumber, false);
}

void CommandToggleBreakpoint(int line) {
   if (showingDisassembly) {
      // TODO.
      return;
   }

   if (!line) {
      line = currentLine;
   }

   for (const auto& bp : breakpoints) {
      if (bp.line == line && 0 == strcmp(bp.fileFull, currentFileFull)) {
         (void)DebuggerSend(std::format("clear {}:{}", currentFile, line), true, false);
         return;
      }
   }

   (void)DebuggerSend(std::format("b {}:{}", currentFile, line), true, false);
}

void CommandToggleBreakpoint() {
   CommandToggleBreakpoint(currentLine);
}

void CommandCustom(string_view command) {

   if (command.starts_with("shell ")) {
      // TODO Move this into CommandParseInternal?

      if (displayOutput)
         UICodeInsertContent(displayOutput, std::format("Running shell command \"{}\"...\n", command), false);
      int          start  = time(nullptr);
      int          result = system(std::format("{} > .output.gf 2>&1", command).c_str());
      size_t       bytes  = 0;
      vector<char> output = LoadFile(".output.gf", &bytes);
      unlink(".output.gf");
      vector<char> copy(bytes + 1);
      uintptr_t    j = 0;

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
         UICodeInsertContent(displayOutput, {copy.data(), j}, false);
         UICodeInsertContent(displayOutput,
                             std::format("(exit code: {}; time: {}s)\n", result, (int)(time(nullptr) - start)), false);
         displayOutput->Refresh();
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
   vector<char> config           = LoadFile(globalConfigPath, nullptr);
   size_t       length           = strlen(config.data());
   size_t       insert           = 0;
   const char*  sectionString    = "\n[trusted_folders]\n";
   bool         addSectionString = true;

   if (length) {
      char* section = strstr(config.data(), sectionString);

      if (section) {
         insert           = section - config.data() + strlen(sectionString);
         addSectionString = false;
      } else {
         insert = length;
      }
   }

   FILE* f = fopen(globalConfigPath, "wb");

   if (!f) {
      print(std::cerr, "Error: Could not modify the global config file!\n");
   } else {
      if (insert)
         fwrite(config.data(), 1, insert, f);
      if (addSectionString)
         fwrite(sectionString, 1, strlen(sectionString), f);
      fwrite(localConfigDirectory, 1, strlen(localConfigDirectory), f);
      char newline = '\n';
      fwrite(&newline, 1, 1, f);
      if (length - insert)
         fwrite(config.data() + insert, 1, length - insert, f);
      fclose(f);
   }
}

void Context::SettingsLoad(bool earlyPass) {
   bool        currentFolderIsTrusted = false;
   static bool cwdConfigNotTrusted    = false;

   for (int i = 0; i < 2; i++) {
      INIState state;
      auto     config_vec = LoadFile(i ? localConfigPath : globalConfigPath, &state.bytes);
      state.buffer        = config_vec[0] ? strdup(config_vec.data()) : nullptr;

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
            shortcut.invoke = [cmd = state.value]() { CommandCustom(cmd); };

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
               UIWindowRegisterShortcut(windowMain, std::move(shortcut));
            }
         } else if (0 == strcmp(state.section, "ui") && earlyPass) {
            if (0 == strcmp(state.key, "font_path")) {
               fontPath = state.value;
            } else if (0 == strcmp(state.key, "font_size")) {
               fontSizeInterface = fontSizeCode = sv_atoi(state.value);
            } else if (0 == strcmp(state.key, "font_size_code")) {
               fontSizeCode = sv_atoi(state.value);
            } else if (0 == strcmp(state.key, "font_size_interface")) {
               fontSizeInterface = sv_atoi(state.value);
            } else if (0 == strcmp(state.key, "scale")) {
               uiScale = atof(state.value);
            } else if (0 == strcmp(state.key, "layout")) {
               layoutString = state.value;
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
               ctx.gdbArgv[ctx.gdbArgc - 1] = state.value;
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

                  std_format_to_n(buffer, sizeof(buffer), "{:.{}}", &state.value[argumentStart],
                                   argumentEnd - argumentStart);

                  ctx.gdbArgc++;
                  ctx.gdbArgv                  = (char**)realloc(ctx.gdbArgv, sizeof(char*) * (ctx.gdbArgc + 1));
                  ctx.gdbArgv[ctx.gdbArgc - 1] = strdup(buffer);
                  ctx.gdbArgv[ctx.gdbArgc]     = nullptr;
               }
            } else if (0 == strcmp(state.key, "path")) {
               ctx.gdbPath    = state.value;
               ctx.gdbArgv[0] = state.value;
            } else if (0 == strcmp(state.key, "log_all_output") && sv_atoi(state.value)) {
               if (auto it = interfaceWindows.find("Log"); it != interfaceWindows.end()) {
                  const auto& [name, window] = *it;
                  ctx.logWindow = static_cast<UICode*>(window.element);
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
            presetCommands.push_back(state);
         } else if (0 == strcmp(state.section, "trusted_folders") && earlyPass && state.keyBytes) {
            if (0 == strcmp(localConfigDirectory, state.key))
               currentFolderIsTrusted = true;
         } else if (0 == strcmp(state.section, "theme") && !earlyPass && state.keyBytes && state.valueBytes) {
            for (uintptr_t i = 0; i < sizeof(themeItems) / sizeof(themeItems[0]); i++) {
               if (strcmp(state.key, themeItems[i]))
                  continue;
               ((uint32_t*)&ui->theme)[i] = strtoul(state.value, nullptr, 16);
            }
         } else if (0 == strcmp(state.section, "vim") && earlyPass && 0 == strcmp(state.key, "server_name")) {
            vimServerName = state.value;
         } else if (0 == strcmp(state.section, "pipe") && earlyPass && 0 == strcmp(state.key, "log")) {
            logPipePath = state.value;
            mkfifo(logPipePath, 6 + 6 * 8 + 6 * 64);
         } else if (0 == strcmp(state.section, "pipe") && earlyPass && 0 == strcmp(state.key, "control")) {
            controlPipePath = state.value;
            mkfifo(controlPipePath, 6 + 6 * 8 + 6 * 64);
            pthread_t thread;
            pthread_create(&thread, nullptr, ControlPipeThread, nullptr);
         } else if (0 == strcmp(state.section, "executable") && earlyPass) {
            if (0 == strcmp(state.key, "path")) {
               executablePath = state.value;
            } else if (0 == strcmp(state.key, "arguments")) {
               executableArguments = state.value;
            } else if (0 == strcmp(state.key, "ask_directory")) {
               executableAskDirectory = sv_atoi(state.value);
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
   }
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
bool          noInspectResults;
bool          inInspectLineMode;
int           inspectModeRestoreLine;
UIRectangle   displayCurrentLineBounds;
const char*   disassemblyCommand = "disas /s";

bool DisplaySetPosition(const char* file, int line, bool useGDBToGetFullPath) {
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
            std_format_to_n(buffer, sizeof(buffer), "{:.{}}", f, end - f);
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
      currentLine = 0;
      std_format_to_n(currentFile, 4096, "{}", file);
      realpath(currentFile, currentFileFull);

      XStoreName(ui->display, windowMain->xwindow, currentFileFull);

      size_t       bytes;
      vector<char> buffer2 = LoadFile(file, &bytes);

      if (!bytes) {
         UICodeInsertContent(displayCode,
                             std::format("The file '{}' (from '{}') could not be loaded.", file, originalFile), true);
      } else {
         UICodeInsertContent(displayCode, {buffer2.data(), bytes}, true);
      }

      changed            = true;
      autoPrintResult[0] = 0;
   }

   if (line != -1 && currentLine != line) {
      currentLine = line;
      UICodeFocusLine(displayCode, line);
      changed = true;
   }

   currentEndOfBlock = SourceFindEndOfBlock();
   displayCode->Refresh();

   return changed;
}

void DisplaySetPositionFromStack() {
   if (stackSelected < stack.size()) {
      char location[sizeof(previousLocation)];
      strcpy(previousLocation, stack[stackSelected].location);
      strcpy(location, stack[stackSelected].location);
      char* line = strchr(location, ':');
      if (line)
         *line = 0;
      DisplaySetPosition(location, line ? sv_atoi(line + 1) : -1, true);
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

   UICodeInsertContent(displayCode, {start, static_cast<size_t>(end - start)}, true);
}

void DisassemblyUpdateLine() {
   auto        res     = EvaluateCommand("p $pc");
   const char* address = strstr(res.c_str(), "0x");

   if (address) {
      char*    addressEnd;
      uint64_t a = strtoul(address, &addressEnd, 0);

      for (int i = 0; i < 2; i++) {
         // Look for the line in the disassembly.

         bool found = false;

         size_t num_lines = displayCode->num_lines();
         for (size_t i = 0; i < num_lines; i++) {
            uint64_t b = strtoul(displayCode->line(i) + 3, &addressEnd, 0);

            if (a == b) {
               UICodeFocusLine(displayCode, i + 1);
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

      displayCode->Refresh();
   }
}

void CommandToggleDisassembly() {
   showingDisassembly     = !showingDisassembly;
   autoPrintResultLine    = 0;
   autoPrintExpression[0] = 0;
   displayCode->flags ^= UICode::NO_MARGIN;

   if (showingDisassembly) {
      UICodeInsertContent(displayCode, "Disassembly could not be loaded.\nPress Ctrl+D to return to source view.",
                          true);
      displayCode->tabSize = 8;
      DisassemblyLoad();
      DisassemblyUpdateLine();
   } else {
      currentLine         = -1;
      currentEndOfBlock   = -1;
      currentFile[0]      = 0;
      currentFileReadTime = 0;
      DisplaySetPositionFromStack();
      displayCode->tabSize = 4;
   }

   displayCode->Refresh();
}

void CommandSetDisassemblyMode() {
   const char* newMode = UIDialogShow(windowMain, 0, "Select the disassembly mode:\n%b\n%b\n%b", "Disassembly only",
                                      "With source", "Source centric");

   if (0 == strcmp(newMode, "Disassembly only"))
      disassemblyCommand = "disas";
   if (0 == strcmp(newMode, "With source"))
      disassemblyCommand = "disas /s";
   if (0 == strcmp(newMode, "Source centric"))
      disassemblyCommand = "disas /m";

   if (showingDisassembly) {
      CommandToggleDisassembly();
      CommandToggleDisassembly();
   }
}

void DisplayCodeDrawInspectLineModeOverlay(UIPainter* painter) {
   const char* instructions = "(Press Esc to exit inspect line mode.)";
   int         width        = (strlen(instructions) + 8) * ui->activeFont->glyphWidth;

   for (const auto& ir : inspectResults) {
      int w = (ir.expression.size() + ir.value.size() + 8) * ui->activeFont->glyphWidth;
      if (w > width)
         width = w;
   }

   int xOffset = 0;

   {
      UICodeLine* line = &displayCode->lines[currentLine - 1];

      for (size_t i = 0; i < line->bytes; i++) {
         if (displayCode->content[line->offset + i] == '\t') {
            xOffset += 4 * ui->activeFont->glyphWidth;
         } else if (displayCode->content[line->offset + i] == ' ') {
            xOffset += 1 * ui->activeFont->glyphWidth;
         } else {
            break;
         }
      }
   }

   int         lineHeight = UIMeasureStringHeight();
   UIRectangle bounds =
      displayCurrentLineBounds + UIRectangle(xOffset, 0, lineHeight, 8 + lineHeight * (inspectResults.size() / 2 + 1));
   bounds.r = bounds.l + width;
   UIDrawBlock(painter, bounds + UIRectangle(3), ui->theme.border);
   UIDrawRectangle(painter, bounds, ui->theme.codeBackground, ui->theme.border, UIRectangle(2));
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

      UIDrawString(painter, line, buffer, noInspectResults ? ui->theme.codeOperator : ui->theme.codeString,
                   UIAlign::left, NULL);
      line = line + UIRectangle(0, lineHeight);
      ++index;
   }

   UIDrawString(painter, line, instructions, ui->theme.codeNumber, UIAlign::right, NULL);
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

int DisplayCodeMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UICode* code = (UICode*)element;

   if (message == UIMessage::CLICKED && !showingDisassembly) {
      int result = UICodeHitTest(code, element->window->cursor.x, element->window->cursor.y);

      if (result < 0 && code->leftDownInMargin) {
         int line = -result;
         CommandToggleBreakpoint(line);
      } else if (result > 0 && !code->leftDownInMargin) {
         int line = result;

         if (element->window->ctrl) {
            (void)DebuggerSend(std::format("until {}", line), true, false);
         } else if (element->window->alt || element->window->shift) {
            EvaluateCommand(std::format("tbreak {}", line));
            (void)DebuggerSend(std::format("jump {}", line), true, false);
         }
      }
   } else if (message == UIMessage::RIGHT_DOWN && !showingDisassembly) {
      int result = UICodeHitTest(code, element->window->cursor.x, element->window->cursor.y);

      bool atLeastOneBreakpointEnabled = false;

      for (const auto& bp : breakpoints) {
         if (bp.line == -result && 0 == strcmp(bp.fileFull, currentFileFull) && bp.enabled) {
            atLeastOneBreakpointEnabled = true;
            break;
         }
      }

      for (const auto& bp : breakpoints) {
         if (bp.line == -result && 0 == strcmp(bp.fileFull, currentFileFull)) {
            UIMenu* menu = UIMenuCreate(element->window, UIMenu::NO_SCROLL);
            UIMenuAddItem(menu, 0, "Delete", [=]() { CommandDeleteAllBreakpointsOnLine(-result); });
            if (atLeastOneBreakpointEnabled)
               UIMenuAddItem(menu, 0, "Disable", [=]() { CommandDisableAllBreakpointsOnLine(-result); });
            else
               UIMenuAddItem(menu, 0, "Enable", [=]() { CommandEnableAllBreakpointsOnLine(-result); });
            UIMenuShow(menu);
         }
      }
   } else if (message == UIMessage::CODE_GET_MARGIN_COLOR && !showingDisassembly) {
      bool atLeastOneBreakpointDisabled = false;

      for (const auto& bp : breakpoints) {
         if (bp.line == di && 0 == strcmp(bp.fileFull, currentFileFull)) {
            if (bp.enabled)
               return ui->theme.accent1;
            else
               atLeastOneBreakpointDisabled = true;
         }
      }

      if (atLeastOneBreakpointDisabled) {
         return (((ui->theme.accent1 & 0xFF0000) >> 1) & 0xFF0000) | (((ui->theme.accent1 & 0xFF00) >> 1) & 0xFF00) |
                ((ui->theme.accent1 & 0xFF) >> 1);
      }
   } else if (message == UIMessage::PAINT) {
      element->messageClass(element, message, di, dp);

      if (inInspectLineMode) {
         UIFont* previousFont = UIFontActivate(code->font);
         DisplayCodeDrawInspectLineModeOverlay((UIPainter*)dp);
         UIFontActivate(previousFont);
      }

      return 1;
   } else if (message == UIMessage::CODE_DECORATE_LINE) {
      UICodeDecorateLine* m = (UICodeDecorateLine*)dp;

      if (m->index == currentLine) {
         displayCurrentLineBounds = m->bounds;
      }

      if (m->index == autoPrintResultLine) {
         UIRectangle rectangle =
            UIRectangle(m->x + ui->activeFont->glyphWidth, m->bounds.r, m->y, m->y + UIMeasureStringHeight());
         UIDrawString(m->painter, rectangle, autoPrintResult, ui->theme.codeComment, UIAlign::left, NULL);
      }

      if (UICodeHitTest(code, element->window->cursor.x, element->window->cursor.y) == m->index &&
          element->window->hovered == element &&
          (element->window->ctrl || element->window->alt || element->window->shift) &&
          !element->window->textboxModifiedFlag) {
         UIDrawBorder(m->painter, m->bounds, element->window->ctrl ? ui->theme.selected : ui->theme.codeOperator,
                      UIRectangle(2));
         UIDrawString(m->painter, m->bounds, element->window->ctrl ? "=> run until " : "=> skip to ",
                      ui->theme.text, UIAlign::right, NULL);
      } else if (m->index == currentEndOfBlock) {
         UIDrawString(m->painter, m->bounds, "[Shift+F10]", ui->theme.codeComment, UIAlign::right, NULL);
      }

      if (m->index == ifConditionLine && ifConditionEvaluation) {
         int columnFrom = _UICodeByteToColumn(code, ifConditionLine - 1, ifConditionFrom);
         int columnTo   = _UICodeByteToColumn(code, ifConditionLine - 1, ifConditionTo);
         UIDrawBlock(m->painter,
                     UIRectangle(m->bounds.l + columnFrom * ui->activeFont->glyphWidth,
                                 m->bounds.l + columnTo * ui->activeFont->glyphWidth, m->bounds.b - 2, m->bounds.b),
                     ifConditionEvaluation == 2 ? ui->theme.accent2 : ui->theme.accent1);
      }
   } else if (message == UIMessage::MOUSE_MOVE || message == UIMessage::UPDATE) {
      if (element->window->cursor.x != lastCursorX || element->window->cursor.y != lastCursorY) {
         lastCursorX                          = element->window->cursor.x;
         lastCursorY                          = element->window->cursor.y;
         element->window->textboxModifiedFlag = false;
      }

      element->Refresh();
   }

   return 0;
}

UIElement* SourceWindowCreate(UIElement* parent) {
   displayCode              = UICodeCreate(parent, selectableSource ? UICode::SELECTABLE : 0);
   displayCode->font        = fontCode;
   displayCode->messageUser = DisplayCodeMessage;
   return displayCode;
}

void SourceWindowUpdate(const char* data, UIElement* element) {
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

   if (changedSourceLine && currentLine < (int)displayCode->num_lines() && currentLine > 0) {
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

      UICodeLine* line     = &displayCode->lines[currentLine - 1];
      const char* text     = displayCode->line(currentLine - 1);
      size_t      bytes    = line->bytes;
      uintptr_t   position = 0;

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
            if (!_UICharIsAlphaOrDigitOrUnderscore(c))
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
         if (!_UICharIsAlphaOrDigitOrUnderscore(text[position2]))
            goto noTypeName;

         position = expressionStart = position2;
      noTypeName:;
      }

      while (position < bytes) {
         char c = text[position];
         if (!_UICharIsAlphaOrDigitOrUnderscore(c) && c != '[' && c != ']' && c != ' ' && c != '.' && c != '-' &&
             c != '>')
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
         std_format_to_n(autoPrintExpression, sizeof(autoPrintExpression), "{:.{}}", text + expressionStart,
                          expressionEnd - expressionStart);
      }

      autoPrintExpressionLine = currentLine;

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
                  ifConditionLine = currentLine;
               } else if (res == "= false") {
                  ifConditionEvaluation = 1;
                  ifConditionFrom = expressionStart, ifConditionTo = i;
                  ifConditionLine = currentLine;
               }
#endif
               break;
            }
         }
      }
   }

   element->Refresh();
}

bool InspectIsTokenCharacter(char c) {
   return isalpha(c) || c == '_';
}

void InspectCurrentLine() {
   inspectResults.clear();

   UICodeLine* line   = &displayCode->lines[currentLine - 1];
   const char* string = displayCode->line(currentLine - 1);
   auto code = string_view{string, size_t(line->bytes)};

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

void InspectLineModeExit(UIElement* element) {
   element->Destroy();
   textboxInput->Focus();
   inInspectLineMode = false;
   currentLine       = inspectModeRestoreLine;
   UICodeFocusLine(displayCode, currentLine);
   displayCode->Refresh();
}

int InspectLineModeMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::UPDATE && element->window->focused != element) {
      InspectLineModeExit(element);
   } else if (message == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if ((m->text == "`") || m->code == UIKeycode::ESCAPE) {
         InspectLineModeExit(element);
      } else if (m->code >= UI_KEYCODE_DIGIT('1') && m->code <= UI_KEYCODE_DIGIT('9')) {
         int index = ((int)m->code - (int)UI_KEYCODE_DIGIT('1'));

         if (index < (int)inspectResults.size()) {
            InspectLineModeExit(element);
            WatchAddExpression2(inspectResults[index].expression);
         }
      } else if ((m->code == UIKeycode::UP && currentLine != 1) ||
                 (m->code == UIKeycode::DOWN && currentLine != (int)displayCode->num_lines())) {
         currentLine += m->code == UIKeycode::UP ? -1 : 1;
         UICodeFocusLine(displayCode, currentLine);
         InspectCurrentLine();
         displayCode->Refresh();
      }

      return 1;
   }

   return 0;
}

void CommandInspectLine() {
   if (!currentLine || currentLine - 1 >= (int)displayCode->num_lines())
      return;

   inspectModeRestoreLine = currentLine;
   inInspectLineMode      = true;
   InspectCurrentLine();
   displayCode->Refresh();

   // Create an element to receive key input messages.
   UIElement* element = UIElementCreate(sizeof(UIElement), windowMain, 0, InspectLineModeMessage, 0);
   element->Focus();
}

// ---------------------------------------------------/
// Data viewers:
// ---------------------------------------------------/

struct AutoUpdateViewer {
   UIElement* element;
   void (*callback)(UIElement*);
};

vector<AutoUpdateViewer> autoUpdateViewers;
bool                     autoUpdateViewersQueued;

bool DataViewerRemoveFromAutoUpdateList(UIElement* element) {
   if (auto it = rng::find_if(autoUpdateViewers, [&](const auto& auv) { return auv.element == element; });
       it != rng::end(autoUpdateViewers)) {
      autoUpdateViewers.erase(it);
      return true;
   }

   return false;
}

int DataViewerAutoUpdateButtonMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::CLICKED) {
      element->flags ^= UIButton::CHECKED;

      if (element->flags & UIButton::CHECKED) {
         AutoUpdateViewer v = {.element = element->parent, .callback = (void (*)(UIElement*))element->cp};
         autoUpdateViewers.push_back(v);
      } else {
         [[maybe_unused]] bool found = DataViewerRemoveFromAutoUpdateList(element->parent);
         assert(found);
      }
   }

   return 0;
}

void DataViewersUpdateAll() {
   if (~dataTab->flags & UIElement::HIDE) {
      for (const auto& auv : autoUpdateViewers) {
         auv.callback(auv.element);
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

int BitmapViewerWindowMessage(UIElement* element, UIMessage message, int di, void* dp) {
   BitmapViewer* viewer = (BitmapViewer*)element->cp;
   if (message == UIMessage::DESTROY) {
      DataViewerRemoveFromAutoUpdateList(element);
      delete viewer;
      element->cp = nullptr;
   } else if (message == UIMessage::GET_WIDTH) {
      int fit = viewer->parsedWidth + 40;
      return fit > 300 ? fit : 300;
   } else if (message == UIMessage::GET_HEIGHT) {
      int fit = viewer->parsedHeight + 40;
      return fit > 100 ? fit : 100;
   }

   return 0;
}

void BitmapViewerUpdate(std::string pointerString, std::string widthString, std::string heightString,
                        std::string strideString, UIElement* owner = nullptr);

int BitmapViewerRefreshMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::CLICKED) {
      BitmapViewer* bitmap = (BitmapViewer*)element->parent->cp;
      BitmapViewerUpdate(bitmap->pointer, bitmap->width, bitmap->height, bitmap->stride, element->parent);
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

int BitmapViewerDisplayMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::RIGHT_UP) {
      UIMenu* menu = UIMenuCreate(element->window, UIMenu::NO_SCROLL);

      UIMenuAddItem(menu, 0, "Save to file...", [element]() {
         static char* path = NULL;
         const char*  result =
            UIDialogShow(windowMain, 0, "Save to file       \nPath:\n%t\n%f%B%C", &path, "Save", "Cancel");
         if (strcmp(result, "Save"))
            return;

         UIImageDisplay* display = (UIImageDisplay*)element;
         FILE*           f       = fopen(path, "wb");
         print(f, "P6\n{} {}\n255\n", display->width, display->height);

         for (int i = 0; i < display->width * display->height; i++) {
            uint8_t pixel[3] = {(uint8_t)(display->bits[i] >> 16), (uint8_t)(display->bits[i] >> 8),
                                (uint8_t)display->bits[i]};
            fwrite(pixel, 1, 3, f);
         }

         fclose(f);
      });

      UIMenuShow(menu);
   }

   return 0;
}

void BitmapViewerAutoUpdateCallback(UIElement* element) {
   BitmapViewer* bitmap = (BitmapViewer*)element->cp;
   BitmapViewerUpdate(bitmap->pointer, bitmap->width, bitmap->height, bitmap->stride, element);
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

      UIMDIChild* window     = UIMDIChildCreate(dataWindow, UIMDIChild::CLOSE_BUTTON, UIRectangle(0), "Bitmap");
      window->messageUser    = BitmapViewerWindowMessage;
      window->cp             = bitmap;
      bitmap->autoToggle     = UIButtonCreate(window, UIButton::SMALL | UIElement::NON_CLIENT, "Auto");
      bitmap->autoToggle->cp = (void*)BitmapViewerAutoUpdateCallback;
      bitmap->autoToggle->messageUser = DataViewerAutoUpdateButtonMessage;
      UIButtonCreate(window, UIButton::SMALL | UIElement::NON_CLIENT, "Refresh")->messageUser =
         BitmapViewerRefreshMessage;
      owner = window;

      UIPanel* panel = UIPanelCreate(owner, UIPanel::EXPAND);
      bitmap->display =
         UIImageDisplayCreate(panel, UIImageDisplay::INTERACTIVE | UIElement::V_FILL, bits, width, height, stride);
      bitmap->labelPanel           = UIPanelCreate(panel, UIPanel::COLOR_1 | UIElement::V_FILL);
      bitmap->label                = UILabelCreate(bitmap->labelPanel, UIElement::H_FILL, {});
      bitmap->display->messageUser = BitmapViewerDisplayMessage;
   }

   BitmapViewer* bitmap = (BitmapViewer*)owner->cp;
   bitmap->parsedWidth = width, bitmap->parsedHeight = height;
   UIImageDisplaySetContent(bitmap->display, bits, width, height, stride);
   if (error)
      UILabelSetContent(bitmap->label, error);
   if (error)
      bitmap->labelPanel->flags &= ~UIElement::HIDE, bitmap->display->flags |= UIElement::HIDE;
   else
      bitmap->labelPanel->flags |= UIElement::HIDE, bitmap->display->flags &= ~UIElement::HIDE;
   bitmap->labelPanel->parent->Refresh();
   owner->Refresh();
   dataWindow->Refresh();

   free(bits);
}

void BitmapAddDialog() {
   static char *pointer = nullptr, *width = nullptr, *height = nullptr, *stride = nullptr;

   const char* result = UIDialogShow(windowMain, 0,
                                     "Add bitmap\n\n%l\n\nPointer to bits: (32bpp, RR GG BB "
                                     "AA)\n%t\nWidth:\n%t\nHeight:\n%t\nStride: (optional)\n%t\n\n%l\n\n%f%B%C",
                                     &pointer, &width, &height, &stride, "Add", "Cancel");

   if (0 == strcmp(result, "Add")) {
      BitmapViewerUpdate(pointer ?: "", width ?: "", height ?: "", (stride && stride[0]) ? stride : "");
   }
}

// ---------------------------------------------------/
// Console:
// ---------------------------------------------------/

vector<unique_ptr<char[]>> commandHistory;
size_t                     commandHistoryIndex;

void CommandPreviousCommand() {
   if (commandHistoryIndex < commandHistory.size()) {
      UITextboxClear(textboxInput, false);
      UITextboxReplace(textboxInput, commandHistory[commandHistoryIndex].get(), false);
      if (commandHistoryIndex < commandHistory.size() - 1)
         commandHistoryIndex++;
      textboxInput->Refresh();
   }
}

void CommandNextCommand() {
   UITextboxClear(textboxInput, false);

   if (commandHistoryIndex > 0) {
      commandHistoryIndex--;
      UITextboxReplace(textboxInput, commandHistory[commandHistoryIndex].get(), false);
   }

   textboxInput->Refresh();
}

void CommandClearOutput() {
   displayOutput->clear();
   displayOutput->Refresh();
}

int TextboxInputMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UITextbox* textbox = (UITextbox*)element;

   if (message == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      static TabCompleter tabCompleter  = {};
      bool                lastKeyWasTab = tabCompleter._lastKeyWasTab;
      tabCompleter._lastKeyWasTab       = false;

      if (m->text.size() && !element->window->ctrl && !element->window->alt && m->text[0] == '`' && !textbox->bytes) {
         textbox->rejectNextKey = true;
      } else if (m->code == UIKeycode::ENTER && !element->window->shift) {
         if (!textbox->bytes) {
            if (commandHistory.size()) {
               CommandSendToGDB(commandHistory[0].get());
            }

            return 1;
         }

         auto buffer = std::format("{}", textbox->text());
         if (commandLog)
            print(commandLog, "{}\n", buffer);
         CommandSendToGDB(buffer);

         unique_ptr<char[]> string = std::make_unique<char[]>(textbox->bytes + 1);
         memcpy(string.get(), textbox->string, textbox->bytes);
         string[textbox->bytes] = 0;
         commandHistory.insert(commandHistory.cbegin(), std::move(string));
         commandHistoryIndex = 0;

         if (commandHistory.size() > 500) {
            commandHistory.pop_back();
         }

         UITextboxClear(textbox, false);
         textbox->Refresh();

         return 1;
      } else if (m->code == UIKeycode::TAB && textbox->bytes && !element->window->shift) {
         TabCompleterRun(&tabCompleter, textbox, lastKeyWasTab, false);
         return 1;
      } else if (m->code == UIKeycode::UP) {
         if (element->window->shift) {
            if (currentLine > 1) {
               DisplaySetPosition(NULL, currentLine - 1, false);
            }
         } else {
            CommandPreviousCommand();
         }
      } else if (m->code == UIKeycode::DOWN) {
         if (element->window->shift) {
            if (currentLine < (int)displayCode->num_lines()) {
               DisplaySetPosition(NULL, currentLine + 1, false);
            }
         } else {
            CommandNextCommand();
         }
      }
   }

   return 0;
}

UIElement* ConsoleWindowCreate(UIElement* parent) {
   UIPanel* panel2           = UIPanelCreate(parent, UIPanel::EXPAND);
   displayOutput             = UICodeCreate(panel2, UICode::NO_MARGIN | UIElement::V_FILL | UICode::SELECTABLE);
   UIPanel* panel3           = UIPanelCreate(panel2, UIPanel::HORIZONTAL | UIPanel::EXPAND | UIPanel::COLOR_1);
   panel3->border            = UIRectangle(5);
   panel3->gap               = 5;
   trafficLight              = UISpacerCreate(panel3, 0, 30, 30);
   trafficLight->messageUser = TrafficLightMessage;
   UIButton* buttonMenu      = UIButtonCreate(panel3, 0, "Menu");
   buttonMenu->invoke        = [buttonMenu]() { ctx.InterfaceShowMenu(buttonMenu); };
   textboxInput              = UITextboxCreate(panel3, UIElement::H_FILL);
   textboxInput->messageUser = TextboxInputMessage;
   textboxInput->Focus();
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

int WatchTextboxMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UITextbox* textbox = (UITextbox*)element;

   if (message == UIMessage::UPDATE) {
      if (element->window->focused != element) {
         element->Destroy();
         ((WatchWindow*)element->cp)->textbox = nullptr;
      }
   } else if (message == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      static TabCompleter tabCompleter  = {};
      bool                lastKeyWasTab = tabCompleter._lastKeyWasTab;
      tabCompleter._lastKeyWasTab       = false;

      if (m->code == UIKeycode::TAB && textbox->bytes && !element->window->shift) {
         TabCompleterRun(&tabCompleter, textbox, lastKeyWasTab, true);
         return 1;
      }
   }

   return 0;
}

void WatchDestroyTextbox(WatchWindow* w) {
   if (!w->textbox)
      return;
   w->textbox->Destroy();
   w->textbox = nullptr;
   w->Focus();
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
   UIScrollBar* scroll    = ((UIPanel*)w->parent)->scrollBar;
   int          rowHeight = (int)(ui_size::TEXTBOX_HEIGHT * w->window->scale);
   int          start = index * rowHeight, end = (index + 1) * rowHeight, height = w->parent->bounds.height();
   bool         unchanged = false;
   if (end >= scroll->position + height)
      scroll->position = end - height;
   else if (start <= scroll->position)
      scroll->position = start;
   else
      unchanged = true;
   if (!unchanged)
      w->parent->Refresh();
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
   if (string.empty() && w->textbox && !w->textbox->bytes) {
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
   UIElement*   element = ctx.InterfaceWindowSwitchToAndFocus("Watch");
   WatchWindow* w       = (WatchWindow*)element->cp;
   w->selectedRow       = w->rows.size();
   WatchAddExpression(w, string);
   if (w->selectedRow)
      w->selectedRow--;
   WatchEnsureRowVisible(w, w->selectedRow);
   w->parent->Refresh();
   w->Refresh();
}

int WatchLoggerWindowMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::DESTROY) {
      if (element->cp) {
         WatchLogger* logger = (WatchLogger*)element->cp;

         if (auto it = rng::find(watchLoggers, logger); it != rng::end(watchLoggers))
            watchLoggers.erase(it);

         EvaluateCommand(std::format("delete {}", logger->id));
         delete logger;
      }
   } else if (message == UIMessage::GET_WIDTH || message == UIMessage::GET_HEIGHT) {
      return element->window->scale * 400;
   }

   return 0;
}

void WatchLoggerTraceSelectFrame(UIElement* element, int index, WatchLogger* logger) {
   if (index == -1) {
      return;
   }

   StackEntry* entry = &logger->entries[logger->selectedEntry].trace[index];
   char        location[sizeof(entry->location)];
   strcpy(location, entry->location);
   char* colon = strchr(location, ':');

   if (colon) {
      *colon = 0;
      DisplaySetPosition(location, sv_atoi(colon, 1), false);
      element->Refresh();
   }
}

int WatchLoggerTableMessage(UIElement* element, UIMessage message, int di, void* dp) {
   WatchLogger* logger = (WatchLogger*)element->cp;

   if (message == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m     = (UITableGetItem*)dp;
      WatchLogEntry*  entry = &logger->entries[m->index];
      m->isSelected         = m->index == logger->selectedEntry;

      if (m->column == 0) {
         return std_format_to_n(m->buffer, m->bufferBytes, "{}", entry->value);
      } else if (m->column == 1) {
         return std_format_to_n(m->buffer, m->bufferBytes, "{}", entry->where);
      } else {
         if (m->column - 2 < (int)entry->evaluated.size()) {
            return std_format_to_n(m->buffer, m->bufferBytes, "{}", entry->evaluated[m->column - 2].result);
         } else {
            return 0;
         }
      }
   } else if (message == UIMessage::LEFT_DOWN || message == UIMessage::MOUSE_DRAG) {
      int index = UITableHitTest((UITable*)element, element->window->cursor.x, element->window->cursor.y);

      if (index != -1 && logger->selectedEntry != index) {
         logger->selectedEntry    = index;
         logger->trace->itemCount = logger->entries[index].trace.size();
         WatchLoggerTraceSelectFrame(logger->trace, 0, logger);
         UITableResizeColumns(logger->trace);
         logger->trace->Refresh();
         element->Refresh();
      }
   }

   return 0;
}

int WatchLoggerTraceMessage(UIElement* element, UIMessage message, int di, void* dp) {
   WatchLogger* logger = (WatchLogger*)element->cp;

   if (message == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m     = (UITableGetItem*)dp;
      StackEntry*     entry = &logger->entries[logger->selectedEntry].trace[m->index];

      if (m->column == 0) {
         return std_format_to_n(m->buffer, m->bufferBytes, "{}", entry->id);
      } else if (m->column == 1) {
         return std_format_to_n(m->buffer, m->bufferBytes, "{}", entry->function);
      } else if (m->column == 2) {
         return std_format_to_n(m->buffer, m->bufferBytes, "{}", entry->location);
      } else if (m->column == 3) {
         return std_format_to_n(m->buffer, m->bufferBytes, "0x{:X}", entry->address);
      }
   } else if (message == UIMessage::LEFT_DOWN || message == UIMessage::MOUSE_DRAG) {
      int index = UITableHitTest((UITable*)element, element->window->cursor.x, element->window->cursor.y);
      WatchLoggerTraceSelectFrame(element, index, logger);
   }

   return 0;
}

std::string WatchGetAddress(const shared_ptr<Watch>& watch) {
   auto res = WatchEvaluate("gf_addressof", watch);

   if (strstr(res.c_str(), "??")) {
      UIDialogShow(windowMain, 0, "Couldn't get the address of the variable.\n%f%B", "OK");
      return {};
   }

   auto end = res.find_first_of(' ');
   if (end == npos) {
      UIDialogShow(windowMain, 0, "Couldn't get the address of the variable.\n%f%B", "OK");
      return {};
   }
   res.resize(end);

   resize_to_lf(res);
   return res;
}

void WatchLoggerResizeColumns(WatchLogger* logger) {
   UITableResizeColumns(logger->table);
   logger->table->Refresh();
}

void WatchChangeLoggerCreate(WatchWindow* w) {
   if (w->selectedRow == w->rows.size()) {
      return;
   }

   if (!dataTab) {
      UIDialogShow(windowMain, 0, "The data window is not open.\nThe watch log cannot be created.\n%f%B", "OK");
      return;
   }

   auto res = WatchGetAddress(w->rows[w->selectedRow]);
   if (res.empty()) {
      return;
   }

   char*       expressionsToEvaluate = nullptr;
   const char* result                = UIDialogShow(
      windowMain, 0,
      "-- Watch logger settings --\nExpressions to evaluate (separate with semicolons):\n%t\n\n%l\n\n%f%B%C",
      &expressionsToEvaluate, "Start", "Cancel");

   if (0 == strcmp(result, "Cancel")) {
      free(expressionsToEvaluate);
      return;
   }

   UIMDIChild* child =
      UIMDIChildCreate(dataWindow, UIMDIChild::CLOSE_BUTTON, UIRectangle(0), std::format("Log {}", res));

   res                = EvaluateCommand(std::format("watch * {}", res));
   const char* number = strstr(res.c_str(), "point ");

   if (!number) {
      UIDialogShow(windowMain, 0, "Couldn't set the watchpoint.\n%f%B", "OK");
      return;
   }

   WatchLogger* logger = new WatchLogger;

   UIButton* button = UIButtonCreate(child, UIButton::SMALL | UIElement::NON_CLIENT, "Resize columns");
   button->invoke   = [logger]() { WatchLoggerResizeColumns(logger); };

   uintptr_t position = 0;
   position += std_format_to_n(logger->columns + position, sizeof(logger->columns) - position, "New value\tWhere");

   if (expressionsToEvaluate) {
      uintptr_t start = 0;

      for (uintptr_t i = 0; true; i++) {
         if (expressionsToEvaluate[i] == ';' || !expressionsToEvaluate[i]) {
            position += std_format_to_n(logger->columns + position, sizeof(logger->columns) - position, "\t{:.{}}",
                                         expressionsToEvaluate + start, i - start);
            start = i + 1;
         }

         if (!expressionsToEvaluate[i]) {
            break;
         }
      }
   }

   UISplitPane* panel = UISplitPaneCreate(child, 0, 0.5f);
   UITable*     table = UITableCreate(panel, UIElement::H_FILL | UIElement::V_FILL, logger->columns);
   UITable* trace = UITableCreate(panel, UIElement::H_FILL | UIElement::V_FILL, "Index\tFunction\tLocation\tAddress");

   logger->id                    = sv_atoi(number, 6);
   logger->table                 = table;
   logger->trace                 = trace;
   logger->selectedEntry         = -1;
   logger->expressionsToEvaluate = expressionsToEvaluate;
   child->cp                     = logger;
   table->cp                     = logger;
   trace->cp                     = logger;
   child->messageUser            = WatchLoggerWindowMessage;
   table->messageUser            = WatchLoggerTableMessage;
   trace->messageUser            = WatchLoggerTraceMessage;
   watchLoggers.push_back(logger);
   dataWindow->Refresh();
   WatchLoggerResizeColumns(logger);

   UIDialogShow(windowMain, 0, "The log has been setup in the data window.\n%f%B", "OK");
   return;
}

bool WatchLoggerUpdate(char* data) {
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
   logger->table->itemCount++;
   logger->table->Refresh();
   (void)DebuggerSend("c", false, false);
   return true;
}

void WatchCreateTextboxForRow(WatchWindow* w, bool addExistingText) {
   int         rowHeight = (int)(ui_size::TEXTBOX_HEIGHT * w->window->scale);
   UIRectangle row       = w->bounds;
   row.t += w->selectedRow * rowHeight, row.b = row.t + rowHeight;
   w->textbox              = UITextboxCreate(w, 0);
   w->textbox->messageUser = WatchTextboxMessage;
   w->textbox->cp          = w;
   w->textbox->Move(row, true);
   w->textbox->Focus();

   if (addExistingText) {
      UITextboxReplace(w->textbox, w->rows[w->selectedRow]->key, false);
   }
}

WatchWindow* WatchGetFocused() {
   return windowMain->focused->messageClass == WatchWindowMessage ? (WatchWindow*)windowMain->focused->cp : NULL;
}

void CommandWatchAddEntryForAddress(WatchWindow* _w) {
   WatchWindow* w = _w ? _w : WatchGetFocused();
   if (!w)
      return;
   if (w->mode == WATCH_NORMAL && w->selectedRow == w->rows.size())
      return;
   const auto& watch = w->rows[w->selectedRow];
   auto        res   = WatchGetAddress(watch);
   if (res.empty())
      return;

   if (w->mode != WATCH_NORMAL) {
      ctx.InterfaceWindowSwitchToAndFocus("Watch");
      w = WatchGetFocused();
      assert(w != NULL);
   }

   auto address = res;
   res = WatchEvaluate("gf_typeof", watch);
   if (res.empty() || res.contains("??"))
      return;
   resize_to_lf(res);

   auto buffer = std::format("({}*){}", res, address);
   WatchAddExpression(w, buffer);
   WatchEnsureRowVisible(w, w->selectedRow);
   w->parent->Refresh();
   w->Refresh();
}

void CommandWatchAddEntryForAddress() {
   CommandWatchAddEntryForAddress(WatchGetFocused());
}

void CommandWatchViewSourceAtAddress(WatchWindow* _w) {
   WatchWindow* w = _w ? _w : WatchGetFocused();
   if (!w)
      return;
   if (w->mode == WATCH_NORMAL && w->selectedRow == w->rows.size())
      return;
   char* position = (char*)w->rows[w->selectedRow]->value.c_str();
   while (*position && !isdigit(*position))
      position++;
   if (!(*position))
      return;
   uint64_t value = strtoul(position, nullptr, 0);
   auto res = EvaluateCommand(std::format("info line * 0x{:x}", value));
   position = (char*)res.c_str();

   if (res.contains("No line number")) {
      resize_to_lf(res);
      UIDialogShow(windowMain, 0, "%s\n%f%B", res.c_str(), "OK");
      return;
   }

   while (*position && !isdigit(*position))
      position++;
   if (!(*position))
      return;
   int line = strtol(position, &position, 0);
   while (*position && *position != '"')
      position++;
   if (!(*position))
      return;
   char* file = position + 1;
   char* end  = strchr(file, '"');
   if (!end)
      return;
   *end = 0;
   DisplaySetPosition(file, line, false);
}

void CommandWatchViewSourceAtAddress() {
   CommandWatchViewSourceAtAddress(WatchGetFocused());
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

   char*       filePath = nullptr;
   const char* result   = UIDialogShow(windowMain, 0, "Path:            \n%t\n%f%B%C", &filePath, "Save", "Cancel");

   if (0 == strcmp(result, "Cancel")) {
      free(filePath);
      return;
   }

   FILE* f = fopen(filePath, "wb");
   free(filePath);

   if (!f) {
      UIDialogShow(windowMain, 0, "Could not open the file for writing!\n%f%B", "OK");
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
      _UIClipboardWriteText(w->window, strdup(res.c_str()), sel_target_t::clipboard);
   }
}

int WatchWindowMessage(UIElement* element, UIMessage message, int di, void* dp) {
   WatchWindow* w         = (WatchWindow*)element->cp;
   int          rowHeight = (int)(ui_size::TEXTBOX_HEIGHT * element->window->scale);
   int          result    = 0;

   if (message == UIMessage::PAINT) {
      UIPainter* painter = (UIPainter*)dp;

      for (size_t i = (painter->clip.t - element->bounds.t) / rowHeight; i <= WatchLastRow(w); i++) {
         UIRectangle row = element->bounds;
         row.t += i * rowHeight, row.b = row.t + rowHeight;

         UIRectangle rect_intersection = intersection(row, painter->clip);
         if (!rect_intersection.valid())
            break;

         bool focused = i == w->selectedRow && element->window->focused == element;

         if (focused)
            UIDrawBlock(painter, row, ui->theme.selected);
         UIDrawBorder(painter, row, ui->theme.border, UIRectangle(0, 1, 0, 1));

         row.l += ui_size::TEXTBOX_MARGIN;
         row.r -= ui_size::TEXTBOX_MARGIN;

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
               UIDrawString(painter, row, buffer, ui->theme.textSelected, UIAlign::left, nullptr);
            } else {
               UIDrawStringHighlighted(painter, row, buffer, 1, NULL);
            }
         }
      }
   } else if (message == UIMessage::GET_HEIGHT) {
      return (WatchLastRow(w) + 1) * rowHeight;
   } else if (message == UIMessage::LEFT_DOWN) {
      if (element->window->cursor.y >= element->bounds.t) {
         w->selectedRow = (element->window->cursor.y - element->bounds.t) / rowHeight;

         if (w->selectedRow < w->rows.size()) {
            const shared_ptr<Watch>& watch = w->rows[w->selectedRow];
            int                      x = (element->window->cursor.x - element->bounds.l) / ui->activeFont->glyphWidth;

            if (x >= watch->depth * 3 - 1 && x <= watch->depth * 3 + 1 && watch->hasFields) {
               UIKeyTyped m;
               m.code       = watch->open ? UIKeycode::LEFT : UIKeycode::RIGHT;
               WatchWindowMessage(element, UIMessage::KEY_TYPED, 0, &m);
            }
         }
      } else
         w->selectedRow = 0;

      element->Focus();
      element->Repaint(nullptr);
   } else if (message == UIMessage::RIGHT_DOWN) {
      if (element->window->cursor.y >= element->bounds.t) {
         size_t index = (element->window->cursor.y - element->bounds.t) / rowHeight;

         if (index < w->rows.size()) {
            WatchWindowMessage(element, UIMessage::LEFT_DOWN, di, dp);
            UIMenu* menu = UIMenuCreate(element->window, UIMenu::NO_SCROLL);

            if (w->mode == WATCH_NORMAL && !w->rows[index]->parent) {
               UIMenuAddItem(menu, 0, "Edit expression", [w]() { WatchCreateTextboxForRow(w, true); });

               UIMenuAddItem(menu, 0, "Delete", [w]() {
                  WatchDeleteExpression(w);
                  w->parent->Refresh();
                  w->Refresh();
               });
            }

            UIMenuAddItem(menu, 0, "Copy value to clipboard\tCtrl+C",
                          [w]() { CommandWatchCopyValueToClipboard(w); });

            UIMenuAddItem(menu, 0, "Log writes to address...", [w]() { WatchChangeLoggerCreate(w); });

            UIMenuAddItem(menu, 0, "Break on writes to address", [w]() {
               if (w->selectedRow == w->rows.size())
                  return;
               auto res = WatchGetAddress(w->rows[w->selectedRow]);
               if (res.empty())
                  return;

               auto buffer = std::format("watch * {}", res);
               (void)DebuggerSend(buffer, true, false);
            });

            if (firstWatchWindow) {
               UIMenuAddItem(menu, 0, "Add entry for address\tCtrl+E",
                             [w]() { CommandWatchAddEntryForAddress(w); });
            }

            UIMenuAddItem(menu, 0, "View source at address\tCtrl+G", [w]() { CommandWatchViewSourceAtAddress(w); });
            UIMenuAddItem(menu, 0, "Save as...", [w]() { CommandWatchSaveAs(w); });

            UIMenuShow(menu);
         }
      }
   } else if (message == UIMessage::UPDATE) {
      element->Repaint(nullptr);
   } else if (message == UIMessage::KEY_TYPED) {
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
                 !element->window->ctrl && !element->window->alt &&
                 (w->selectedRow == w->rows.size() || !w->rows[w->selectedRow]->parent)) {
         WatchCreateTextboxForRow(w, false);
         w->textbox->Message(message, di, dp);
      } else if (w->mode == WATCH_NORMAL && !m->text.empty() && m->code == UI_KEYCODE_LETTER('V') && !w->textbox &&
                 element->window->ctrl && !element->window->alt && !element->window->shift &&
                 (w->selectedRow == w->rows.size() || !w->rows[w->selectedRow]->parent)) {
         WatchCreateTextboxForRow(w, false);
         w->textbox->Message(message, di, dp);
      } else if (m->code == UIKeycode::ENTER && w->textbox) {
         WatchAddExpression(w);
      } else if (m->code == UIKeycode::ESCAPE) {
         WatchDestroyTextbox(w);
      } else if (m->code == UIKeycode::UP) {
         if (element->window->shift) {
            if (currentLine > 1) {
               DisplaySetPosition(NULL, currentLine - 1, false);
            }
         } else {
            WatchDestroyTextbox(w);
            if (w->selectedRow)
               w->selectedRow--;
         }
      } else if (m->code == UIKeycode::DOWN) {
         if (element->window->shift) {
            if (currentLine < (int)displayCode->num_lines()) {
               DisplaySetPosition(NULL, currentLine + 1, false);
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
      } else if (m->code == UI_KEYCODE_LETTER('C') && !w->textbox && !element->window->shift && !element->window->alt &&
                 element->window->ctrl) {
         CommandWatchCopyValueToClipboard(w);
      } else {
         result = 0;
      }

      WatchEnsureRowVisible(w, w->selectedRow);
      element->parent->Refresh();
      element->Refresh();
   } else if (message == UIMessage::MIDDLE_DOWN) {
      if (w->mode == WATCH_NORMAL && !w->textbox && !element->window->ctrl && !element->window->alt &&
          (w->selectedRow == w->rows.size() || !w->rows[w->selectedRow]->parent)) {
         WatchCreateTextboxForRow(w, false);
         UITextboxPasteText(w->textbox, sel_target_t::primary);
         element->Repaint(NULL);
      }
      return 1;
   }

   if (w->selectedRow > WatchLastRow(w)) {
      w->selectedRow = WatchLastRow(w);
   }

   return result;
}

int WatchPanelMessage(UIElement* element, UIMessage message, int di, void* dp) {
   WatchWindow* window = (WatchWindow*)element->cp;
   if (message == UIMessage::LEFT_DOWN) {
      window->Focus();
      window->Repaint(nullptr);
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
   cp = this; // todo: shouldn't be needed
}

UIElement* WatchWindowCreate(UIElement* parent) {
   UIPanel*     panel = UIPanelCreate(parent, UIPanel::SCROLL | UIPanel::COLOR_1);
   WatchWindow* w     = new WatchWindow(panel, UIElement::H_FILL | UIElement::TAB_STOP, "Watch");
   panel->messageUser = WatchPanelMessage;
   panel->cp          = w;

   w->mode      = WATCH_NORMAL;
   w->extraRows = 1;
   if (!firstWatchWindow)
      firstWatchWindow = w;
   return panel;
}

UIElement* LocalsWindowCreate(UIElement* parent) {
   UIPanel*     panel = UIPanelCreate(parent, UIPanel::SCROLL | UIPanel::COLOR_1);
   WatchWindow* w     = new WatchWindow(panel, UIElement::H_FILL | UIElement::TAB_STOP, "Locals");
   panel->messageUser = WatchPanelMessage;
   panel->cp          = w;
   w->mode            = WATCH_LOCALS;
   return panel;
}

void WatchWindowUpdate(const char*, UIElement* element) {
   WatchWindow* w = (WatchWindow*)element->cp;

   if (w->mode == WATCH_LOCALS) {
      auto res = EvaluateCommand("py gf_locals()");

      bool newFrame = (w->lastLocalList.empty() || w->lastLocalList != res);

      if (newFrame) {
         w->lastLocalList = res;

         char*         buffer      = strdup(res.c_str());
         char*         s           = buffer;
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
   element->parent->Refresh();
   element->Refresh();
}

void WatchWindowFocus(UIElement* element) {
   WatchWindow* w = (WatchWindow*)element;
   w->Focus();
}

void CommandAddWatch() {
   UIElement* element = ctx.InterfaceWindowSwitchToAndFocus("Watch");
   if (!element)
      return;
   WatchWindow* w = (WatchWindow*)element->cp;
   if (w->textbox)
      return;
   w->selectedRow = w->rows.size();
   WatchCreateTextboxForRow(w, false);
}

// ---------------------------------------------------/
// Stack window:
// ---------------------------------------------------/

void StackSetFrame(UIElement* element, int index) {
   if (index >= 0 && index < ((UITable*)element)->itemCount) {
      stackChanged  = true;
      if (stackSelected != (size_t)index) {
         (void)DebuggerSend(std::format("frame {}", index), false, false);
         stackSelected = index;
         element->Repaint(nullptr);
      } else {
         currentLine = -1; // force the update in DisplayPosition as we may have scrolled away
         DisplaySetPositionFromStack();
      }
   }
}

int TableStackMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m = (UITableGetItem*)dp;
      m->isSelected     = (size_t)m->index == stackSelected;
      StackEntry* entry = &stack[m->index];

      if (m->column == 0) {
         return std_format_to_n(m->buffer, m->bufferBytes, "{}", entry->id);
      } else if (m->column == 1) {
         return std_format_to_n(m->buffer, m->bufferBytes, "{}", entry->function);
      } else if (m->column == 2) {
         return std_format_to_n(m->buffer, m->bufferBytes, "{}", entry->location);
      } else if (m->column == 3) {
         return std_format_to_n(m->buffer, m->bufferBytes, "0x{:X}", entry->address);
      }
   } else if (message == UIMessage::LEFT_DOWN || message == UIMessage::MOUSE_DRAG) {
      StackSetFrame(element, UITableHitTest((UITable*)element, element->window->cursor.x, element->window->cursor.y));
   } else if (message == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if (m->code == UIKeycode::UP || m->code == UIKeycode::DOWN) {
         StackSetFrame(element, stackSelected + (m->code == UIKeycode::UP ? -1 : 1));
         // TODO Scroll the row into view if necessary.
         return 1;
      }
   }

   return 0;
}

UIElement* StackWindowCreate(UIElement* parent) {
   UITable* table     = UITableCreate(parent, 0, "Index\tFunction\tLocation\tAddress");
   table->messageUser = TableStackMessage;
   return table;
}

void StackWindowUpdate(const char*, UIElement* _table) {
   UITable* table   = (UITable*)_table;
   table->itemCount = stack.size();
   UITableResizeColumns(table);
   table->Refresh();
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

int TableBreakpointsMessage(UIElement* element, UIMessage message, int di, void* dp) {
   BreakpointTableData* data = (BreakpointTableData*)element->cp;

   if (message == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m     = (UITableGetItem*)dp;
      Breakpoint*     entry = &breakpoints[m->index];
      m->isSelected         = rng::find(data->selected, entry->number) != rng::end(data->selected);

      if (m->column == 0) {
         return StringFormat(m->buffer, m->bufferBytes, "%s", entry->file);
      } else if (m->column == 1) {
         if (entry->watchpoint)
            return StringFormat(m->buffer, m->bufferBytes, "watch %d", entry->number);
         else
            return StringFormat(m->buffer, m->bufferBytes, "%d", entry->line);
      } else if (m->column == 2) {
         return StringFormat(m->buffer, m->bufferBytes, "%s", entry->enabled ? "yes" : "no");
      } else if (m->column == 3) {
         return StringFormat(m->buffer, m->bufferBytes, "%s", entry->condition);
      } else if (m->column == 4) {
         if (entry->hit > 0) {
            return StringFormat(m->buffer, m->bufferBytes, "%d", entry->hit);
         }
      }
   } else if (message == UIMessage::RIGHT_DOWN) {
      int index = UITableHitTest((UITable*)element, element->window->cursor.x, element->window->cursor.y);

      if (index != -1) {
         Breakpoint* entry = &breakpoints[index];

         bool found = rng::find(data->selected, entry->number) != rng::end(data->selected);
         if (data->selected.size() <= 1 || !found) {
            if (!element->window->ctrl)
               data->selected.clear();
            data->selected.push_back(entry->number);
         }

         UIMenu* menu = UIMenuCreate(element->window, UIMenu::NO_SCROLL);

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
            UIMenuAddItem(menu, 0, "Delete", [data]() { CommandDeleteSelectedBreakpoints(data); });

            if (atLeastOneBreakpointDisabled)
               UIMenuAddItem(menu, 0, "Enable", [data]() { CommandEnableSelectedBreakpoints(data); });
            else
               UIMenuAddItem(menu, 0, "Disable", [data]() { CommandDisableSelectedBreakpoints(data); });
         } else {
            UIMenuAddItem(menu, 0, "Delete", [index]() { CommandDeleteBreakpoint(index); });

            if (breakpoints[index].enabled)
               UIMenuAddItem(menu, 0, "Disable", [index]() { CommandDisableBreakpoint(index); });
            else
               UIMenuAddItem(menu, 0, "Enable", [index]() { CommandEnableBreakpoint(index); });
         }

         UIMenuShow(menu);
      }
   } else if (message == UIMessage::LEFT_DOWN) {
      int index = UITableHitTest((UITable*)element, element->window->cursor.x, element->window->cursor.y);

      if (index != -1) {
         Breakpoint* entry = &breakpoints[index];

         if (!element->window->shift)
            data->anchor = entry->number;
         if (!element->window->ctrl)
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
            if (element->window->ctrl && !element->window->shift) {
               if (auto it = rng::find(data->selected, breakpoints[i].number); it != rng::end(data->selected))
                  data->selected.erase(it);
            } else {
               data->selected.push_back(breakpoints[i].number);
            }
         }

         if (!entry->watchpoint && rng::find(data->selected, entry->number) != rng::end(data->selected)) {
            DisplaySetPosition(entry->file, entry->line, false);
         }
      } else if (!element->window->ctrl && !element->window->shift) {
         data->selected.clear();
      }
      element->Focus();
   } else if (message == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if (m->code == UIKeycode::DEL && data->selected.size() > 0) {
         CommandDeleteSelectedBreakpoints(data);
      }
   }

   return 0;
}

UIElement* BreakpointsWindowCreate(UIElement* parent) {
   UITable* table     = UITableCreate(parent, 0, "File\tLine\tEnabled\tCondition\tHit");
   table->cp          = new BreakpointTableData;
   table->messageUser = TableBreakpointsMessage;
   return table;
}

void BreakpointsWindowUpdate(const char*, UIElement* _table) {
   UITable* table   = (UITable*)_table;
   table->itemCount = breakpoints.size();
   UITableResizeColumns(table);
   table->Refresh();
}

// ---------------------------------------------------/
// Data window:
// ---------------------------------------------------/

UIButton* buttonFillWindow;

int DataTabMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::TAB_SELECTED && autoUpdateViewersQueued) {
      // If we've switched to the data tab, we may need to update the bitmap viewers.

      for (const auto& auw : autoUpdateViewers)
         auw.callback(auw.element);

      autoUpdateViewersQueued = false;
   }

   return 0;
}

void CommandToggleFillDataTab() {
   if (!dataTab)
      return;
   static UIElement *oldParent, *oldBefore;
   buttonFillWindow->flags ^= UIButton::CHECKED;

   if (switcherMain->active == dataTab) {
      UISwitcherSwitchTo(switcherMain, switcherMain->children[0]);
      dataTab->ChangeParent(oldParent, oldBefore);
   } else {
      dataTab->Message(UIMessage::TAB_SELECTED, 0, 0);
      oldParent = dataTab->parent;
      oldBefore = dataTab->ChangeParent(switcherMain, NULL);
      UISwitcherSwitchTo(switcherMain, dataTab);
   }
}

UIElement* DataWindowCreate(UIElement* parent) {
   dataTab                  = UIPanelCreate(parent, UIPanel::EXPAND);
   UIPanel* panel5          = UIPanelCreate(dataTab, UIPanel::COLOR_1 | UIPanel::HORIZONTAL | UIPanel::SMALL_SPACING);
   buttonFillWindow         = UIButtonCreate(panel5, UIButton::SMALL, "Fill window");
   buttonFillWindow->invoke = []() { CommandToggleFillDataTab(); };

   for (const auto& idw : ctx.interfaceDataViewers) {
      UIButton* b = UIButtonCreate(panel5, UIButton::SMALL, idw.addButtonLabel);
      b->invoke   = [&]() { idw.addButtonCallback(); };
   }

   dataWindow           = UIMDIClientCreate(dataTab, UIElement::V_FILL);
   dataTab->messageUser = DataTabMessage;
   return dataTab;
}

// ---------------------------------------------------/
// Struct window:
// ---------------------------------------------------/

struct StructWindow {
   UICode*    display = nullptr;
   UITextbox* textbox = nullptr;
};

int TextboxStructNameMessage(UIElement* element, UIMessage message, int di, void* dp) {
   StructWindow* window = (StructWindow*)element->cp;

   if (message == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if (m->code == UIKeycode::ENTER) {
         auto  res = EvaluateCommand(std::format("ptype /o {}", window->textbox->text()));
         char* end = (char*)strstr(res.c_str(), "\n(gdb)");
         if (end)
            *end = 0;
         UICodeInsertContent(window->display, res, true);
         UITextboxClear(window->textbox, false);
         window->display->Refresh();
         element->Refresh();
         return 1;
      }
   }

   return 0;
}

UIElement* StructWindowCreate(UIElement* parent) {
   StructWindow* window         = new StructWindow;
   UIPanel*      panel          = UIPanelCreate(parent, UIPanel::COLOR_1 | UIPanel::EXPAND);
   window->textbox              = UITextboxCreate(panel, 0);
   window->textbox->messageUser = TextboxStructNameMessage;
   window->textbox->cp          = window;
   window->display              = UICodeCreate(panel, UIElement::V_FILL | UICode::NO_MARGIN | UICode::SELECTABLE);
   UICodeInsertContent(window->display, "Type the name of a struct to view its layout.", false);
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
   const char* name = button->label.data();
   *oldLength       = strlen(window->directory);
   strcat(window->directory, "/");
   strcat(window->directory, name);
   struct stat s;
   stat(window->directory, &s);
   return s.st_mode;
}

int FilesButtonMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UIButton* button = (UIButton*)element;

   if (message == UIMessage::CLICKED) {
      FilesWindow* window = (FilesWindow*)element->cp;
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
         DisplaySetPosition(window->directory, 1, false);
      }

      window->directory[oldLength] = 0;
   } else if (message == UIMessage::PAINT) {
      UIPainter* painter = (UIPainter*)dp;
      int        i       = (element == element->window->pressed) + (element == element->window->hovered);
      if (i)
         UIDrawBlock(painter, element->bounds, i == 2 ? ui->theme.buttonPressed : ui->theme.buttonHovered);
      UIDrawString(painter, element->bounds + UIRectangle(ui_size::BUTTON_PADDING, 0, 0, 0), button->label,
                   button->flags & UIButton::CHECKED ? ui->theme.codeNumber : ui->theme.codeDefault,
                   UIAlign::left, NULL);
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
   vector<char*> names = {};
   while ((entry = readdir(directory)))
      names.push_back(strdup(entry->d_name));
   closedir(directory);
   window->panel->DestroyDescendents();

   qsort(names.data(), names.size(), sizeof(char*),
         [](const void* a, const void* b) { return strcmp(*(const char**)a, *(const char**)b); });

   for (auto name : names) {
      if (name[0] != '.' || name[1] != 0) {
         UIButton* button = UIButtonCreate(window->panel, 0, name);
         button->flags &= ~UIElement::TAB_STOP;
         button->cp          = window;
         button->messageUser = FilesButtonMessage;

         if (S_ISDIR(FilesGetMode(window, button, &oldLength))) {
            button->flags |= UIButton::CHECKED;
         }

         window->directory[oldLength] = 0;
      }

      free(name);
   }

   names.clear();
   window->panel->Refresh();

   {
      char path[PATH_MAX];
      realpath(window->directory, path);
      UILabelSetContent(window->path, path);
   }

   return true;
}

void FilesNavigateToCWD(FilesWindow* window) {
   getcwd(window->directory, sizeof(window->directory));
   FilesPanelPopulate(window);
}

void FilesNavigateToActiveFile(FilesWindow* window) {
   StringFormat(window->directory, sizeof(window->directory), "%s", currentFileFull);
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
   UIPanel*     container = UIPanelCreate(parent, UIPanel::EXPAND);
   window->panel = UIPanelCreate(container, UIPanel::COLOR_1 | UIPanel::EXPAND | UIPanel::SCROLL | UIElement::V_FILL);
   window->panel->gap = -1, window->panel->border = UIRectangle(1);
   window->panel->cp = window;
   UIPanel* row      = UIPanelCreate(container, UIPanel::COLOR_2 | UIPanel::HORIZONTAL | UIPanel::SMALL_SPACING);

   UIButton* button = UIButtonCreate(row, UIButton::SMALL, "-> cwd");
   button->invoke   = [window]() { FilesNavigateToCWD(window); };

   button         = UIButtonCreate(row, UIButton::SMALL, "-> active file");
   button->invoke = [window]() { FilesNavigateToActiveFile(window); };

   window->path = UILabelCreate(row, UIElement::H_FILL, "");
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
   return UIPanelCreate(parent, UIPanel::SMALL_SPACING | UIPanel::COLOR_1 | UIPanel::SCROLL);
}

void RegistersWindowUpdate(const char*, UIElement* panel) {
   auto res = EvaluateCommand("info registers");

   if (res.empty() || res.contains("The program has no registers now.") ||
       res.contains("The current thread has terminated")) {
      return;
   }

   panel->DestroyDescendents();
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
      StringFormat(data.string, sizeof(data.string), "%.*s", (int)(stringEnd - stringStart), stringStart);
      bool modified = false;

      if (registerData.size() > newRegisterData.size()) {
         RegisterData* old = &registerData[newRegisterData.size()];

         if (strcmp(old->string, data.string)) {
            modified = true;
         }
      }

      newRegisterData.push_back(data);

      UIPanel* row = UIPanelCreate(panel, UIPanel::HORIZONTAL | UIElement::H_FILL);
      if (modified)
         row->messageUser = ModifiedRowMessage;
      UILabelCreate(row, 0, {stringStart, static_cast<size_t>(stringEnd - stringStart)});

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
            StringFormat(autoPrintResult + position, sizeof(autoPrintResult) - position, ", ");
         }

         int position = strlen(autoPrintResult);
         StringFormat(autoPrintResult + position, sizeof(autoPrintResult) - position, "%.*s=%.*s",
                      (int)(nameEnd - nameStart), nameStart, (int)(format1End - format1Start), format1Start);
      }
   }

   panel->Refresh();
   registerData.clear();
   registerData = newRegisterData;
}

// ---------------------------------------------------/
// Commands window:
// ---------------------------------------------------/

UIElement* CommandsWindowCreate(UIElement* parent) {
   UIPanel* panel =
      UIPanelCreate(parent, UIPanel::COLOR_1 | UIPanel::SMALL_SPACING | UIPanel::EXPAND | UIPanel::SCROLL);
   if (!presetCommands.size())
      UILabelCreate(panel, 0, "No preset commands found in config file!");

   for (const auto& cmd : presetCommands) {
      UIButton* button = UIButtonCreate(panel, 0, cmd.key);
      button->invoke   = [command = std::format("gf-command {}", cmd.key)]() { CommandSendToGDB(command); };
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
         void* buffer  = malloc(strlen(input) + sizeof(context) + 1);
         memcpy(buffer, &context, sizeof(context));
         strcpy((char*)buffer + sizeof(context), input);
         UIWindowPostMessage(windowMain, msgReceivedLog, buffer);
      }
   }
}

void LogReceived(char* buffer) {
   UICodeInsertContent(ctx.logWindow, buffer, false);
   (*(UIElement**)buffer)->Refresh();
   free(buffer);
}

UIElement* LogWindowCreate(UIElement* parent) {
   UICode*   code = UICodeCreate(parent, UICode::SELECTABLE);
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

int ThreadTableMessage(UIElement* element, UIMessage message, int di, void* dp) {
   ThreadWindow* window = (ThreadWindow*)element->cp;

   if (message == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m = (UITableGetItem*)dp;
      m->isSelected     = window->threads[m->index].active;

      if (m->column == 0) {
         return StringFormat(m->buffer, m->bufferBytes, "%d", window->threads[m->index].id);
      } else if (m->column == 1) {
         return StringFormat(m->buffer, m->bufferBytes, "%s", window->threads[m->index].frame);
      }
   } else if (message == UIMessage::LEFT_DOWN) {
      int index = UITableHitTest((UITable*)element, element->window->cursor.x, element->window->cursor.y);

      if (index != -1) {
         (void)DebuggerSend(std::format("thread {}", window->threads[index].id), true, false);
      }
   }

   return 0;
}

UIElement* ThreadWindowCreate(UIElement* parent) {
   UITable* table     = UITableCreate(parent, 0, "ID\tFrame");
   table->cp          = new ThreadWindow;
   table->messageUser = ThreadTableMessage;
   return table;
}

void ThreadWindowUpdate(const char*, UIElement* _table) {
   ThreadWindow* window = (ThreadWindow*)_table->cp;
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

   UITable* table   = (UITable*)_table;
   table->itemCount = window->threads.size();
   UITableResizeColumns(table);
   table->Refresh();
}

// ---------------------------------------------------/
// Executable window:
// ---------------------------------------------------/

struct ExecutableWindow {
   UITextbox*  path         = nullptr;
   UITextbox*  arguments    = nullptr;
   UICheckbox* askDirectory = nullptr;
};

void ExecutableWindowStartOrRun(ExecutableWindow* window, bool pause) {
   auto res = EvaluateCommand(std::format("file \"{}\"", window->path->text()));

   if (res.contains("No such file or directory.")) {
      UIDialogShow(windowMain, 0, "The executable path is invalid.\n%f%B", "OK");
      return;
   }

   (void)EvaluateCommand(std::format("start {}", window->arguments->text()));

   if (window->askDirectory->check == UICheckbox::CHECKED) {
      CommandParseInternal("gf-get-pwd", true);
   }

   if (!pause) {
      (void)CommandParseInternal("run", false);
   } else {
      DebuggerGetStack();
      DisplaySetPositionFromStack();
   }
}

void ExecutableWindowRunButton(void* _window) {
   ExecutableWindowStartOrRun((ExecutableWindow*)_window, false);
}

void ExecutableWindowStartButton(void* _window) {
   ExecutableWindowStartOrRun((ExecutableWindow*)_window, true);
}

void ExecutableWindowSaveButton(void* _window) {
   ExecutableWindow* window = (ExecutableWindow*)_window;
   FILE*             f      = fopen(localConfigPath, "rb");

   if (f) {
      const char* result = UIDialogShow(windowMain, 0, ".project.gf already exists in the current directory.\n%f%B%C",
                                        "Overwrite", "Cancel");
      if (strcmp(result, "Overwrite"))
         return;
      fclose(f);
   }

   f = fopen(localConfigPath, "wb");
   print(f, "[executable]\npath={}\narguments={}\nask_directory={}\n", window->path->text(),
         window->arguments->text(), window->askDirectory->check == UICheckbox::CHECKED ? '1' : '0');
   fclose(f);
   SettingsAddTrustedFolder();
   UIDialogShow(windowMain, 0, "Saved executable settings!\n%f%B", "OK");
}

UIElement* ExecutableWindowCreate(UIElement* parent) {
   ExecutableWindow* window = new ExecutableWindow;
   UIPanel*          panel  = UIPanelCreate(parent, UIPanel::COLOR_1 | UIPanel::EXPAND);
   UILabelCreate(panel, 0, "Path to executable:");
   window->path = UITextboxCreate(panel, 0);
   UITextboxReplace(window->path, executablePath ?: "", false);
   UILabelCreate(panel, 0, "Command line arguments:");
   window->arguments = UITextboxCreate(panel, 0);
   UITextboxReplace(window->arguments, executableArguments ?: "", false);
   window->askDirectory        = UICheckboxCreate(panel, 0, "Ask GDB for working directory");
   window->askDirectory->check = executableAskDirectory ? UICheckbox::CHECKED : UICheckbox::UNCHECKED;
   UIPanel* row                = UIPanelCreate(panel, UIPanel::HORIZONTAL);

   UIButton* button = UIButtonCreate(row, 0, "Run");
   button->invoke   = [window]() { ExecutableWindowRunButton(window); };

   button         = UIButtonCreate(row, 0, "Start");
   button->invoke = [window]() { ExecutableWindowStartButton(window); };

   UISpacerCreate(row, 0, 10, 0);

   button         = UIButtonCreate(row, 0, "Save to .project.gf");
   button->invoke = [window]() { ExecutableWindowSaveButton(window); };
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

int TextboxSearchCommandMessage(UIElement* element, UIMessage message, int di, void* dp) {
   CommandSearchWindow* window = (CommandSearchWindow*)element->cp;

   if (message == UIMessage::KEY_TYPED) {
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

      StringFormat(query, sizeof(query), "%.*s", (int)window->textbox->bytes, window->textbox->string);
      for (int i = 0; query[i]; i++) {
         query[i] = query[i] >= 'A' && query[i] <= 'Z' ? query[i] + 'a' - 'A' : query[i];
      }

      for (const auto& cmd : window->commands) {
         if (strstr(cmd.descriptionLower, query)) {
            StringFormat(buffer, sizeof(buffer), "%s: %s", cmd.name, cmd.description);
            UICodeInsertContent(window->display, buffer, firstMatch);
            firstMatch = false;
         }
      }

      if (firstMatch) {
         UICodeInsertContent(window->display, "(no matches)", firstMatch);
      }

      window->display->vScroll->position = 0;
      window->display->Refresh();
   }

   return 0;
}

UIElement* CommandSearchWindowCreate(UIElement* parent) {
   CommandSearchWindow* window  = new CommandSearchWindow;
   UIPanel*             panel   = UIPanelCreate(parent, UIPanel::COLOR_1 | UIPanel::EXPAND);
   window->textbox              = UITextboxCreate(panel, 0);
   window->textbox->messageUser = TextboxSearchCommandMessage;
   window->textbox->cp          = window;
   window->display              = UICodeCreate(panel, UIElement::V_FILL | UICode::NO_MARGIN | UICode::SELECTABLE);
   UICodeInsertContent(window->display, "Type here to search \nGDB command descriptions.", true);
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

int ProfFlameGraphMessage(UIElement* element, UIMessage message, int di, void* dp);

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
      UIDialogShow(windowMain, 0, "Source information was not found for this function.\n%f%b", "OK");
      return;
   }
   ProfFunctionEntry& function = report->functions[entry->thisFunction];

   if (!function.cName[0]) {
      UIDialogShow(windowMain, 0, "Source information was not found for this function.\n%f%b", "OK");
      return;
   } else {
      DisplaySetPosition(report->sourceFiles[function.sourceFileIndex].cPath, function.lineNumber, false);
   }
}

void ProfAddBreakpoint(ProfFlameGraphEntry* entry) {
   CommandSendToGDB(std::format("b {}", entry->cName));
}

void ProfFillView(ProfFlameGraphReport* report) {
   ProfFlameGraphEntry* entry = report->menuItem;
   report->xStart             = entry->startTime;
   report->xEnd               = entry->endTime;
   report->Repaint(0);
}

void ProfDrawTransparentOverlay(UIPainter* painter, UIRectangle rectangle, uint32_t color) {
   rectangle = intersection(painter->clip, rectangle);
   if (!rectangle.valid())
      return;

   for (int line = rectangle.t; line < rectangle.b; line++) {
      uint32_t* bits = painter->bits + line * painter->width + rectangle.l;

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
#define PROFILER_ENTRY_RECTANGLE_OTHER()                                                                      \
   int64_t rl = report->client.l + (int64_t)((time->start - report->xStart) * zoomX);                         \
   int64_t rt = report->client.t + time->depth * profRowHeight + profScaleHeight - report->vScroll->position; \
   int64_t rb = rt + profRowHeight;

void* ProfFlameGraphRenderThread(void* _unused) {
   (void)_unused;
   int threadIndex = __sync_fetch_and_add(&profRenderThreadIndexAllocator, 1);

   while (true) {
      sem_wait(&profRenderStartSemaphores[threadIndex]);

      ProfFlameGraphReport* report  = profRenderReport;
      UIElement*            element = report;

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

         if (rl <= element->clip.r && rr >= element->clip.l && rt <= element->clip.b && rb >= element->clip.t) {
            // Carefully convert 64-bit integers to 32-bit integers for UIRectangle,
            // since the rectangle may be really large when zoomed in.
            UIRectangle r;
            r.l = rl < report->client.l ? report->client.l : rl;
            r.r = rr > report->client.r ? report->client.r : rr;
            r.t = rt < report->client.t ? report->client.t : rt;
            r.b = rb > report->client.b ? report->client.b : rb;

            UIDrawBlock(painter, UIRectangle(r.r - 1, r.r, r.t, r.b - 1), profBorderDarkColor);
            UIDrawBlock(painter, UIRectangle(r.l, r.r, r.b - 1, r.b), profBorderDarkColor);
            UIDrawBlock(painter, UIRectangle(r.l, r.r - 1, r.t, r.t + 1), profBorderLightColor);
            UIDrawBlock(painter, UIRectangle(r.l, r.l + 1, r.t + 1, r.b - 1), profBorderLightColor);

            bool     hovered = report->hover && report->hover->thisFunction == entry->thisFunction && !report->dragMode;
            uint32_t color   = hovered ? profHoverColor : profEntryColorPalette[entry->colorIndex];
            /// uint32_t color = hovered ? profHoverColor : profMainColor;
            UIDrawBlock(painter, UIRectangle(r.l + 1, r.r - 1, r.t + 1, r.b - 1), color);

            if (r.width() > 40) {
               auto string = std::format("{} {:f}ms", entry->cName, entry->endTime - entry->startTime);
               UIDrawString(painter, UIRectangle(r.l + 2, r.r, r.t, r.b), string, profTextColor, UIAlign::left,
                            NULL);
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

int ProfFlameGraphMessage(UIElement* element, UIMessage message, int di, void* dp) {
   ProfFlameGraphReport* report = (ProfFlameGraphReport*)element;

   if (message == UIMessage::PAINT) {
      UIFont* previousFont = UIFontActivate(report->font);

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
      UIDrawBlock(painter, report->client, profBackgroundColor);

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
         UIDrawRectangle(painter, r, profMainColor, profBorderDarkColor, UIRectangle(0, 0, 0, 1));

         double increment = 1000.0;
         while (increment > 1e-6 && increment * zoomX > 600.0)
            increment *= 0.1;

         double start = (painter->clip.l - report->client.l) / zoomX + report->xStart;
         start -= fmod(start, increment) + increment;

         for (double i = start; i < report->totalTime; i += increment) {
            UIRectangle r;
            r.t = report->client.t;
            r.b = r.t + profScaleHeight;
            r.l = report->client.l + (int)((i - report->xStart) * zoomX);
            r.r = r.l + (int)(increment * zoomX);
            if (r.l > painter->clip.r)
               break;
            auto string = std::format("{:.4f}ms", i);
            UIDrawBlock(painter, UIRectangle(r.l, r.l + 1, r.t, r.b), profBorderLightColor);
            UIDrawString(painter, r, string, profTextColor, UIAlign::left, NULL);
         }
      }

      if (report->dragMode == FLAME_GRAPH_DRAG_ZOOM_RANGE) {
         UIRectangle r = report->client;
         r.l = report->dragInitialPoint, r.r = report->dragCurrentPoint;
         if (r.l > r.r)
            r.r = report->dragInitialPoint, r.l = report->dragCurrentPoint;
         UIDrawInvert(painter, r);
      }

      if (report->thumbnail) {
         UIRectangle zoomBar =
            UIRectangle(report->client.l, report->client.r, report->client.b - profZoomBarHeight, report->client.b);
         UIRectangle zoomBarThumb = zoomBar;
         zoomBarThumb.l           = zoomBar.l + zoomBar.width() * (report->xStart / report->totalTime);
         zoomBarThumb.r           = zoomBar.l + zoomBar.width() * (report->xEnd / report->totalTime);
         UIRectangle drawBounds   = intersection(zoomBar, painter->clip);

         for (int i = drawBounds.t; i < drawBounds.b; i++) {
            for (int j = drawBounds.l; j < drawBounds.r; j++) {
               int si = (i - zoomBar.t) * report->thumbnailHeight / zoomBar.height();
               int sj = (j - zoomBar.l) * report->thumbnailWidth / zoomBar.width();

               if (si >= 0 && si < report->thumbnailHeight && sj >= 0 && sj < report->thumbnailWidth) {
                  painter->bits[i * painter->width + j] = report->thumbnail[si * report->thumbnailWidth + sj];
               }
            }
         }

         UIDrawBorder(painter, zoomBar, profBorderDarkColor, UIRectangle(2));
         UIDrawBorder(painter, zoomBarThumb, profBorderLightColor, UIRectangle(4));
      }

      if (report->hover && !report->dragMode) {
         const ProfFunctionEntry& function = report->functions[report->hover->thisFunction];

         char line1[256], line2[256], line3[256];
         StringFormat(line1, sizeof(line1), "[%s] %s:%d", report->hover->cName,
                      function.sourceFileIndex != -1 ? report->sourceFiles[function.sourceFileIndex].cPath : "??",
                      function.lineNumber);
         StringFormat(line2, sizeof(line2), "This call: %fms %.1f%%", report->hover->endTime - report->hover->startTime,
                      (report->hover->endTime - report->hover->startTime) / report->totalTime * 100.0);
         StringFormat(line3, sizeof(line3), "Total: %fms in %d calls (%fms avg) %.1f%%", function.totalTime,
                      function.callCount, function.totalTime / function.callCount,
                      function.totalTime / report->totalTime * 100.0);

         int width      = 0;
         int line1Width = UIMeasureStringWidth(line1);
         if (width < line1Width)
            width = line1Width;
         int line2Width = UIMeasureStringWidth(line2);
         if (width < line2Width)
            width = line2Width;
         int line3Width = UIMeasureStringWidth(line3);
         if (width < line3Width)
            width = line3Width;
         int lineHeight = UIMeasureStringHeight();
         int height     = 3 * lineHeight;
         int x          = element->window->cursor.x;
         if (x + width > element->clip.r)
            x = element->clip.r - width;
         int y = element->window->cursor.y + 25;
         if (y + height > element->clip.b)
            y = element->window->cursor.y - height - 10;
         UIRectangle rectangle = UIRectangle(x, x + width, y, y + height);

         ProfDrawTransparentOverlay(painter, rectangle + ui_rect_1i(-5), 0xFF000000);
         UIDrawString(painter, UIRectangle(x, x + width, y + lineHeight * 0, y + lineHeight * 1), line1, 0xFFFFFFFF,
                      UIAlign::left, 0);
         UIDrawString(painter, UIRectangle(x, x + width, y + lineHeight * 1, y + lineHeight * 2), line2, 0xFFFFFFFF,
                      UIAlign::left, 0);
         UIDrawString(painter, UIRectangle(x, x + width, y + lineHeight * 2, y + lineHeight * 3), line3, 0xFFFFFFFF,
                      UIAlign::left, 0);
      }

      UIFontActivate(previousFont);
   } else if (message == UIMessage::MOUSE_MOVE) {
      double               zoomX = (double)report->client.width() / (report->xEnd - report->xStart);
      ProfFlameGraphEntry* hover = nullptr;
      int                  depth =
         (element->window->cursor.y - report->client.t + report->vScroll->position - profScaleHeight) / profRowHeight;
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

         if (element->window->cursor.x >= rl && element->window->cursor.x < rr) {
            hover = &report->entries[i];
            break;
         }
      }

      if (hover != report->hover || hover /* to repaint the tooltip */) {
         report->hover = hover;
         element->Repaint(NULL);
      }
   } else if (message == UIMessage::UPDATE) {
      if (report->hover && element->window->hovered != element) {
         report->hover = NULL;
         element->Repaint(NULL);
      }
   } else if (message == UIMessage::LEFT_DOWN) {
      if (element->window->cursor.y < report->client.b - profZoomBarHeight) {
         report->dragMode          = FLAME_GRAPH_DRAG_PAN;
         report->dragInitialValue  = report->xStart;
         report->dragInitialPoint  = element->window->cursor.x;
         report->dragInitialValue2 = report->vScroll->position;
         report->dragInitialPoint2 = element->window->cursor.y;
         element->window->SetCursor((int)UICursor::hand);
      } else {
         report->dragMode         = FLAME_GRAPH_DRAG_X_SCROLL;
         report->dragInitialValue = report->xStart;
         report->dragInitialPoint = element->window->cursor.x;
         report->dragScrollRate   = 1.0;

         if (element->window->cursor.x <
                report->client.l + report->client.width() * (report->xStart / report->totalTime) ||
             element->window->cursor.y >=
                report->client.l + report->client.width() * (report->xEnd / report->totalTime)) {
            report->dragScrollRate = 0.2;
         }
      }
   } else if (message == UIMessage::MIDDLE_DOWN) {
      if (element->window->cursor.y < report->client.b - profZoomBarHeight) {
         report->dragMode          = FLAME_GRAPH_DRAG_X_PAN_AND_ZOOM;
         report->dragInitialValue  = report->xStart;
         report->dragInitialPoint  = element->window->cursor.x;
         report->dragInitialPoint2 = element->window->cursor.y;
         element->window->SetCursor((int)UICursor::cross_hair);
      }
   } else if (message == UIMessage::RIGHT_DOWN) {
      if (element->window->cursor.y < report->client.b - profZoomBarHeight) {
         report->dragMode         = FLAME_GRAPH_DRAG_ZOOM_RANGE;
         report->dragInitialPoint = element->window->cursor.x;
      }
   } else if (message == UIMessage::LEFT_UP || message == UIMessage::RIGHT_UP || message == UIMessage::MIDDLE_UP) {
      if (report->dragMode == FLAME_GRAPH_DRAG_ZOOM_RANGE && report->dragStarted) {
         UIRectangle r = report->client;
         r.l = report->dragInitialPoint, r.r = report->dragCurrentPoint;
         if (r.l > r.r)
            r.r = report->dragInitialPoint, r.l = report->dragCurrentPoint;
         double zoomX   = (double)report->client.width() / (report->xEnd - report->xStart);
         report->xEnd   = (r.r - report->client.l) / zoomX + report->xStart;
         report->xStart = (r.l - report->client.l) / zoomX + report->xStart;
      } else if (!report->dragStarted && message == UIMessage::RIGHT_UP && report->hover) {
         report->menuItem = report->hover;
         UIMenu* menu     = UIMenuCreate(element->window, UIMenu::NO_SCROLL);
         UIMenuAddItem(menu, 0, "Show source", [report]() { ProfShowSource(report); });
         UIMenuAddItem(menu, 0, "Add breakpoint", [report]() { ProfAddBreakpoint(report->hover); });
         UIMenuAddItem(menu, 0, "Fill view", [report]() { ProfFillView(report); });
         UIMenuShow(menu);
      } else if (!report->dragStarted && message == UIMessage::MIDDLE_UP && report->hover) {
         report->menuItem = report->hover;
         ProfFillView(report);
      }

      report->dragMode    = 0;
      report->dragStarted = false;
      element->Repaint(NULL);
      element->window->SetCursor((int)UICursor::arrow);
   } else if (message == UIMessage::MOUSE_DRAG) {
      report->dragStarted = true;

      if (report->dragMode == FLAME_GRAPH_DRAG_PAN) {
         double delta   = report->xEnd - report->xStart;
         report->xStart = report->dragInitialValue - (double)(element->window->cursor.x - report->dragInitialPoint) *
                                                        report->totalTime / report->client.width() * delta /
                                                        report->totalTime;
         report->xEnd = report->xStart + delta;
         if (report->xStart < 0) {
            report->xEnd -= report->xStart;
            report->xStart = 0;
         }
         if (report->xEnd > report->totalTime) {
            report->xStart += report->totalTime - report->xEnd;
            report->xEnd = report->totalTime;
         }
         report->vScroll->position =
            report->dragInitialValue2 - (double)(element->window->cursor.y - report->dragInitialPoint2);
         report->vScroll->Refresh();
      } else if (report->dragMode == FLAME_GRAPH_DRAG_X_SCROLL) {
         double delta   = report->xEnd - report->xStart;
         report->xStart = report->dragInitialValue + (double)(element->window->cursor.x - report->dragInitialPoint) *
                                                        report->totalTime / report->client.width() *
                                                        report->dragScrollRate;
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
         report->xStart += (double)(element->window->cursor.x - report->dragInitialPoint) * report->totalTime /
                           report->client.width() * delta / report->totalTime * 3.0;
         report->xEnd  = report->xStart + delta;
         double factor = powf(1.02, element->window->cursor.y - report->dragInitialPoint2);
         double mouse  = (double)(element->window->cursor.x - report->client.l) / report->client.width();
#if 0
         mouse = 0.5;
         XWarpPointer(ui->display, None, windowMain->window, 0, 0, 0, 0, report->dragInitialPoint, report->dragInitialPoint2);
#else
         report->dragInitialPoint  = element->window->cursor.x;
         report->dragInitialPoint2 = element->window->cursor.y;
#endif
         double newZoom = (report->xEnd - report->xStart) / report->totalTime * factor;
         report->xStart += mouse * (report->xEnd - report->xStart) * (1 - factor);
         report->xEnd = newZoom * report->totalTime + report->xStart;
      } else if (report->dragMode == FLAME_GRAPH_DRAG_ZOOM_RANGE) {
         report->dragCurrentPoint = element->window->cursor.x;
      }

      element->Repaint(NULL);
   } else if (message == UIMessage::MOUSE_WHEEL) {
      int    divisions   = di / 72;
      double factor      = 1;
      double perDivision = 1.2f;
      while (divisions > 0)
         factor *= perDivision, divisions--;
      while (divisions < 0)
         factor /= perDivision, divisions++;
      double mouse   = (double)(element->window->cursor.x - report->client.l) / report->client.width();
      double newZoom = (report->xEnd - report->xStart) / report->totalTime * factor;
      report->xStart += mouse * (report->xEnd - report->xStart) * (1 - factor);
      report->xEnd = newZoom * report->totalTime + report->xStart;
      element->Repaint(NULL);
      return 1;
   } else if (message == UIMessage::GET_CURSOR) {
      return report->dragMode == FLAME_GRAPH_DRAG_PAN              ? (int)UICursor::hand
             : report->dragMode == FLAME_GRAPH_DRAG_X_PAN_AND_ZOOM ? (int)UICursor::cross_hair
                                                                   : (int)UICursor::arrow;
   } else if (message == UIMessage::LAYOUT) {
      UIRectangle scrollBarBounds = element->bounds;
      scrollBarBounds.l           = scrollBarBounds.r - ui_size::SCROLL_BAR * element->window->scale;
      report->vScroll->page       = element->bounds.height() - profZoomBarHeight;
      report->vScroll->Move(scrollBarBounds, true);
      report->client   = element->bounds;
      report->client.r = scrollBarBounds.l;
   } else if (message == UIMessage::SCROLLED) {
      element->Refresh();
   } else if (message == UIMessage::DESTROY) {
      report->entries.clear();
      report->functions.clear();
      report->sourceFiles.clear();
      report->entryTimes.clear();
      free(report->thumbnail);
   }

   return 0;
}

int ProfReportWindowMessage(UIElement* element, UIMessage message, int di, void* dp) {
   ProfFlameGraphReport* report = (ProfFlameGraphReport*)element->cp;

   if (message == UIMessage::LAYOUT) {
      if (report->showingTable) {
         report->flags |= UIElement::HIDE;
         report->table->flags &= ~UIElement::HIDE;
      } else {
         report->flags &= ~UIElement::HIDE;
         report->table->flags |= UIElement::HIDE;
      }
      element->messageClass(element, message, di, dp);
      report->table->Move(report->bounds, false);
      return 1;
   }

   return 0;
}

void ProfSwitchView(ProfFlameGraphReport* report) {
   report->showingTable = !report->showingTable;
   report->switchViewButton->label = report->showingTable ? "Graph view" : "Table view";
   report->parent->Refresh();
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

int ProfTableMessage(UIElement* element, UIMessage message, int di, void* dp) {
   ProfFlameGraphReport* report = (ProfFlameGraphReport*)element->cp;
   UITable*              table  = report->table;

   if (message == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem*    m     = (UITableGetItem*)dp;
      ProfFunctionEntry* entry = &report->sortedFunctions[m->index];

      if (m->column == 0) {
         return StringFormat(m->buffer, m->bufferBytes, "%s", entry->cName);
      } else if (m->column == 1) {
         return StringFormat(m->buffer, m->bufferBytes, "%f", entry->totalTime);
      } else if (m->column == 2) {
         return StringFormat(m->buffer, m->bufferBytes, "%d", entry->callCount);
      } else if (m->column == 3) {
         return StringFormat(m->buffer, m->bufferBytes, "%f", entry->totalTime / entry->callCount);
      } else if (m->column == 4) {
         return StringFormat(m->buffer, m->bufferBytes, "%f", entry->totalTime / report->totalTime * 100);
      }
   } else if (message == UIMessage::LEFT_DOWN) {
      int index = UITableHeaderHitTest(table, element->window->cursor.x, element->window->cursor.y);

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

         element->Refresh();
         table->columnHighlight = index;
      }
   } else if (message == UIMessage::GET_CURSOR) {
      return UITableHeaderHitTest(table, element->window->cursor.x, element->window->cursor.y) == -1
                ? (int)UICursor::arrow
                : (int)UICursor::hand;
   }

   return 0;
}

void ProfLoadProfileData(void* _window) {
   ProfWindow* data = (ProfWindow*)_window;

   auto        res              = EvaluateExpression("gfProfilingTicksPerMs");
   const char* ticksPerMsString = strstr(res.c_str(), "= ");
   data->ticksPerMs             = ticksPerMsString ? sv_atoi(ticksPerMsString, 2) : 0;

   if (!ticksPerMsString || !data->ticksPerMs) {
      UIDialogShow(windowMain, 0, "Profile data could not be loaded (1).\nConsult the guide.\n%f%b", "OK");
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
      UIWindow* window  = windowMain;
      UIPainter painter = {};
      painter.bits      = window->bits.data();
      painter.width     = window->width;
      painter.height    = window->height;
      painter.clip      = ui_rect_2s(window->width, window->height);
      char string[256];
      StringFormat(string, sizeof(string), "Loading data... (estimated time: %d seconds)", rawEntryCount / 5000000 + 1);
      UIDrawBlock(&painter, painter.clip, ui->theme.panel1);
      UIDrawString(&painter, painter.clip, string, ui->theme.text, UIAlign::center, 0);
      window->updateRegion = ui_rect_2s(window->width, window->height);
      window->EndPaint(nullptr);
      window->updateRegion = painter.clip;
   }

   ProfProfilingEntry* rawEntries = (ProfProfilingEntry*)calloc(sizeof(ProfProfilingEntry), rawEntryCount);

   char path[PATH_MAX];
   realpath(".profile.gf", path);
   char buffer[PATH_MAX * 2];
   StringFormat(buffer, sizeof(buffer),
                "dump binary memory %s (gfProfilingBuffer) (gfProfilingBuffer+gfProfilingBufferPosition)", path);
   (void)EvaluateCommand(buffer);
   FILE* f = fopen(path, "rb");

   if (!f) {
      UIDialogShow(windowMain, 0, "Profile data could not be loaded (2).\nConsult the guide.\n%f%b", "OK");
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

      StringFormat(buffer, sizeof(buffer), "(void *) %ld", rawEntries[i].thisFunction);
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

      StringFormat(buffer, sizeof(buffer), "py print(gdb.lookup_global_symbol('%s').symtab.filename)", function.cName);
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
         StringFormat(buffer, sizeof(buffer), "py print(gdb.lookup_global_symbol('%s').line)", function.cName);
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

   UIMDIChild* window = UIMDIChildCreate(dataWindow, UIMDIChild::CLOSE_BUTTON, ui_rect_2s(800, 600), "Flame graph");
   UIButton*   switchViewButton = UIButtonCreate(window, UIButton::SMALL | UIElement::NON_CLIENT, "Table view");
   UITable*    table            = UITableCreate(window, 0, "Name\tTime spent (ms)\tCall count\tAverage per call (ms)");
   ProfFlameGraphReport* report = new ProfFlameGraphReport(window, 0);

   report->vScroll = UIScrollBarCreate(report, 0);
   report->font    = data->fontFlameGraph;

   window->cp          = report;
   window->messageUser = ProfReportWindowMessage;

   switchViewButton->cp     = report;
   switchViewButton->invoke = [report]() { ProfSwitchView(report); };
   table->cp                = report;
   table->messageUser       = ProfTableMessage;
   report->switchViewButton = switchViewButton;
   report->table            = table;

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

   report->vScroll->maximum = (maxDepth + 2) * 30;

   for (const auto& [k, v] : report->functions) {
      if (k)
         report->sortedFunctions.push_back(v);
   }

   {
      // Create an image of the graph for the zoom bar.

      UIPainter painter = {};
      painter.width     = 1200;
      painter.height    = maxDepth * 30 + 30;
      painter.clip      = UIRectangle(0, painter.width, 0, painter.height);
      painter.bits      = (uint32_t*)malloc(painter.width * painter.height * 4);
      report->client = report->bounds = report->clip = painter.clip;
      ProfFlameGraphMessage(report, UIMessage::PAINT, 0, &painter);
      int newHeight = 30;
      ThumbnailResize(painter.bits, painter.width, painter.height, painter.width, newHeight);
      painter.height          = newHeight;
      painter.bits            = (uint32_t*)realloc(painter.bits, painter.width * painter.height * 4);
      report->thumbnail       = painter.bits;
      report->thumbnailWidth  = painter.width;
      report->thumbnailHeight = painter.height;
   }

   table->itemCount = report->sortedFunctions.size();
   qsort(report->sortedFunctions.data(), report->sortedFunctions.size(), sizeof(ProfFunctionEntry),
         ProfFunctionCompareTotalTime);
   table->columnHighlight = 1;
   UITableResizeColumns(table);

   free(rawEntries);
}

void ProfStepOverProfiled(ProfWindow* window) {
   (void)EvaluateCommand("call GfProfilingStart()");
   CommandSendToGDB("gf-next");
   window->inStepOverProfiled = true;
}

void ProfWindowUpdate(const char* data, UIElement* element) {
   ProfWindow* window = (ProfWindow*)element->cp;

   if (window->inStepOverProfiled) {
      (void)EvaluateCommand("call GfProfilingStop()");
      ProfLoadProfileData(window);
      ctx.InterfaceWindowSwitchToAndFocus("Data");
      dataWindow->Refresh();
      window->inStepOverProfiled = false;
   }
}

UIElement* ProfWindowCreate(UIElement* parent) {
   const int   fontSizeFlameGraph = 8;
   ProfWindow* window             = new ProfWindow;
   window->fontFlameGraph         = UIFontCreate(_UI_TO_STRING_2(UI_FONT_PATH), fontSizeFlameGraph);
   UIPanel* panel                 = UIPanelCreate(parent, UIPanel::COLOR_1 | UIPanel::EXPAND);
   panel->cp                      = window;
   UIButton* button               = UIButtonCreate(panel, UIElement::V_FILL, "Step over profiled");
   button->invoke                 = [window]() { ProfStepOverProfiled(window); };

#ifdef UI_FREETYPE
   // Since we will do multithreaded painting with fontFlameGraph, we need to make sure all its glyphs are ready to go.
   for (uintptr_t i = 0; i < sizeof(window->fontFlameGraph->glyphsRendered); i++) {
      UIPainter fakePainter  = {};
      UIFont*   previousFont = UIFontActivate(window->fontFlameGraph);
      UIDrawGlyph(&fakePainter, 0, 0, i, 0xFF000000);
      UIFontActivate(previousFont);
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

static int  MemoryWindowMessage(UIElement* element, UIMessage message, int di, void* dp);
static void MemoryWindowGotoButtonInvoke(void* cp);

struct MemoryWindow : public UIElement {
   UIButton*       gotoButton;
   vector<int16_t> loadedBytes;
   uint64_t        offset;

   MemoryWindow(UIElement* parent)
      : UIElement(parent, 0, MemoryWindowMessage, "memory window")
      , gotoButton(UIButtonCreate(this, UIButton::SMALL, "&")) {
      gotoButton->invoke = [this]() { MemoryWindowGotoButtonInvoke(this); };
   }
};

int MemoryWindowMessage(UIElement* element, UIMessage message, int di, void* dp) {
   MemoryWindow* window = (MemoryWindow*)element;

   if (message == UIMessage::PAINT) {
      UIPainter* painter = (UIPainter*)dp;
      UIDrawBlock(painter, element->bounds, ui->theme.panel1);

      char        buffer[64];
      uint64_t    address   = window->offset;
      const int   rowHeight = UIMeasureStringHeight();
      UIRectangle row       = element->bounds + ui_rect_1i(10);
      size_t      rowCount  = (painter->clip.b - row.t) / rowHeight;
      row.b                 = row.t + rowHeight;

      {
         StringFormat(buffer, sizeof(buffer), "Inspecting memory @%p", (void*)window->offset);
         UIDrawString(painter, row, buffer, ui->theme.codeString, UIAlign::left, 0);
         row.t += rowHeight;
         row.b += rowHeight;
         const char* header = "         0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F   0123456789ABCDEF";
         UIDrawString(painter, row, header, ui->theme.codeComment, UIAlign::left, 0);
         row.t += rowHeight;
         row.b += rowHeight;
      }

      if (rowCount > 0 && rowCount * 16 > window->loadedBytes.size()) {
         window->loadedBytes.clear();

         for (size_t i = 0; i < (size_t)rowCount * 16 / 8; i++) {
            StringFormat(buffer, sizeof(buffer), "x/8xb 0x%lx", window->offset + i * 8);
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

      while (row.t < painter->clip.b) {
         int position = 0;

         StringFormat(buffer, sizeof(buffer), "%.8X ", (uint32_t)(address & 0xFFFFFFFF));
         UIDrawString(painter, row, buffer, ui->theme.codeComment, UIAlign::left, 0);
         UIRectangle r          = row + UIRectangle(UIMeasureStringWidth(buffer), 0, 0, 0);
         int         glyphWidth = UIMeasureStringWidth("a");

         for (int i = 0; i < 16; i++) {
            if (address + i >= window->offset + window->loadedBytes.size() ||
                window->loadedBytes[address + i - window->offset] < 0) {
               UIDrawGlyph(painter, r.l + position, r.t, '?', ui->theme.codeOperator);
               position += glyphWidth;
               UIDrawGlyph(painter, r.l + position, r.t, '?', ui->theme.codeOperator);
               position += glyphWidth;
            } else {
               const char* hexChars = "0123456789ABCDEF";
               uint8_t     byte     = window->loadedBytes[address + i - window->offset];
               UIDrawGlyph(painter, r.l + position, r.t, hexChars[(byte & 0xF0) >> 4], ui->theme.codeNumber);
               position += glyphWidth;
               UIDrawGlyph(painter, r.l + position, r.t, hexChars[(byte & 0x0F) >> 0], ui->theme.codeNumber);
               position += glyphWidth;

               if (byte >= 0x20 && byte < 0x7F) {
                  UIDrawGlyph(painter, r.l + (49 + i) * glyphWidth, r.t, byte, ui->theme.codeString);
               }
            }

            position += glyphWidth;
         }

         row.t += rowHeight;
         row.b += rowHeight;
         address += 0x10;
      }
   } else if (message == UIMessage::LAYOUT) {
      UIRectangle bounds = element->bounds + ui_rect_1i(10);
      window->gotoButton->Move(UIRectangle(bounds.r - window->gotoButton->Message(UIMessage::GET_WIDTH, 0, 0), bounds.r,
                                           bounds.t,
                                           bounds.t + window->gotoButton->Message(UIMessage::GET_HEIGHT, 0, 0)),
                               false);
   } else if (message == UIMessage::MOUSE_WHEEL) {
      window->offset += di / 72 * 0x10;
      window->loadedBytes.clear();
      window->Repaint(nullptr);
   }

   return 0;
}

void MemoryWindowUpdate(const char* data, UIElement* element) {
   MemoryWindow* window = (MemoryWindow*)element;
   window->loadedBytes.clear();
   element->Repaint(NULL);
}

void MemoryWindowGotoButtonInvoke(void* cp) {
   MemoryWindow* window     = (MemoryWindow*)cp;
   char*         expression = nullptr;

   if (0 == strcmp("Goto", UIDialogShow(windowMain, 0, "Enter address expression:\n%t\n%f%b%b", &expression, "Goto",
                                        "Cancel"))) {
      char buffer[4096];
      StringFormat(buffer, sizeof(buffer), "py gf_valueof(['%s'],' ')", expression);
      auto        res    = EvaluateCommand(buffer);
      const char* result = res.c_str();

      if (result && ((*result == '(' && isdigit(result[1])) || isdigit(*result))) {
         if (*result == '(')
            result++;
         uint64_t address = strtol(result, nullptr, 0);

         if (address) {
            window->loadedBytes.clear();
            window->offset = address & ~0xF;
            window->Repaint(nullptr);
         } else {
            UIDialogShow(windowMain, 0, "Cannot access memory at address 0.\n%f%b", "OK");
         }
      } else {
         UIDialogShow(windowMain, 0, "Expression did not evaluate to an address.\n%f%b", "OK");
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

static int ViewWindowColorSwatchMessage(UIElement* element, UIMessage message, int di, void* dp);
static int ViewWindowMatrixGridMessage(UIElement* element, UIMessage message, int di, void* dp);

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
      : UIElement(parent, UIElement::H_FILL | UIElement::V_FILL, ViewWindowMatrixGridMessage, "Matrix grid")
      , w(w)
      , h(h) {
      hScroll          = UIScrollBarCreate(this, UIScrollBar::HORIZONTAL);
      vScroll          = UIScrollBarCreate(this, 0);
      hScroll->maximum = w * UIMeasureStringWidth("A") * itemCharacters;
      vScroll->maximum = h * UIMeasureStringHeight();

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

int ViewWindowStringMessage(UIElement* element, UIMessage message, int di, void* dp);

struct ViewWindowString : public UIElement {
   UIScrollBar*       vScroll;
   unique_ptr<char[]> data;
   int                length;

   ViewWindowString(UIElement* parent, unique_ptr<char[]> data, int length)
      : UIElement(parent, UIElement::H_FILL | UIElement::V_FILL, ViewWindowStringMessage, "String display")
      , vScroll(UIScrollBarCreate(this, 0))
      , data(std::move(data))
      , length(length) {}
};

int ViewWindowColorSwatchMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::GET_HEIGHT) {
      return UIMeasureStringHeight();
   } else if (message == UIMessage::PAINT) {
      uint32_t    color   = ((ViewWindowColorSwatch*)element)->color;
      UIPainter*  painter = (UIPainter*)dp;
      const char* message = "Col: ";
      UIDrawString(painter, element->bounds, message, ui->theme.text, UIAlign::left, nullptr);
      UIRectangle swatch = UIRectangle(element->bounds.l + UIMeasureStringWidth(message), 0, element->bounds.t + 2,
                                       element->bounds.b - 2);
      swatch.r           = swatch.l + 50;
      UIDrawRectangle(painter, swatch, color, 0xFF000000, UIRectangle(1));
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

int ViewWindowMatrixGridMessage(UIElement* element, UIMessage message, int di, void* dp) {
   ViewWindowMatrixGrid* grid = (ViewWindowMatrixGrid*)element;

   if (message == UIMessage::PAINT) {
      // TODO Optimise for really large arrays.
      // TODO Calculate eigenvectors/values.

      int        glyphWidth  = UIMeasureStringWidth("A");
      int        glyphHeight = UIMeasureStringHeight();
      UIPainter* painter     = (UIPainter*)dp;

      for (int i = 0; i < grid->h; i++) {
         for (int j = 0; j < grid->w; j++) {
            if (grid->grid_type == grid_type_t::char_t) {
               char c = grid->data()[i * grid->w + j];
               if (!c)
                  continue;
               UIDrawGlyph(painter, element->bounds.l + j * glyphWidth - grid->hScroll->position,
                           element->bounds.t + i * glyphHeight - grid->vScroll->position, c, ui->theme.text);
            } else if (grid->grid_type == grid_type_t::float_t || grid->grid_type == grid_type_t::double_t) {
               double f = grid->grid_type == grid_type_t::double_t ? ((double*)grid->data())[i * grid->w + j]
                                                                   : (double)((float*)grid->data())[i * grid->w + j];
               char   buffer[64];
               StringFormat(buffer, sizeof(buffer), "%f", f);
               UIRectangle rectangle =
                  UIRectangle(j * glyphWidth * 14, (j + 1) * glyphWidth * 14, i * glyphHeight, (i + 1) * glyphHeight);
               UIRectangle offset = UIRectangle(element->bounds.l - (int)grid->hScroll->position,
                                                element->bounds.t - (int)grid->vScroll->position);
               UIDrawString(painter, rectangle + offset, buffer, ui->theme.text, UIAlign::right, nullptr);
            }
         }
      }

      int scrollBarSize = ui_size::SCROLL_BAR * element->window->scale;
      UIDrawBlock(painter,
                  UIRectangle(element->bounds.r - scrollBarSize, element->bounds.r, element->bounds.b - scrollBarSize,
                              element->bounds.b),
                  ui->theme.panel1);
   } else if (message == UIMessage::LAYOUT) {
      UIRectangle scrollBarBounds = element->bounds;
      scrollBarBounds.l           = scrollBarBounds.r - ui_size::SCROLL_BAR * element->window->scale;
      scrollBarBounds.b -= ui_size::SCROLL_BAR * element->window->scale;
      grid->vScroll->page = scrollBarBounds.height();
      grid->vScroll->Move(scrollBarBounds, true);
      scrollBarBounds   = element->bounds;
      scrollBarBounds.t = scrollBarBounds.b - ui_size::SCROLL_BAR * element->window->scale;
      scrollBarBounds.r -= ui_size::SCROLL_BAR * element->window->scale;
      grid->hScroll->page = scrollBarBounds.width();
      grid->hScroll->Move(scrollBarBounds, true);
   } else if (message == UIMessage::SCROLLED) {
      element->Repaint(nullptr);
   }

   return 0;
}

int ViewWindowStringLayout(ViewWindowString* display, UIPainter* painter, int offset) {
   UIRectangle clientBounds = display->bounds;
   clientBounds.r -= ui_size::SCROLL_BAR * display->window->scale;
   int x = clientBounds.l, y = clientBounds.t - offset;
   int glyphWidth = UIMeasureStringWidth("a"), glyphHeight = UIMeasureStringHeight();

   for (int i = 0; i < display->length; i++) {
      if (x + glyphWidth > clientBounds.r) {
         x = clientBounds.l + glyphWidth;
         y += glyphHeight;
         if (painter)
            UIDrawGlyph(painter, clientBounds.l, y, '>', ui->theme.codeComment);
      }

      if (display->data[i] < 0x20 || display->data[i] >= 0x7F) {
         if (display->data[i] == '\n') {
            if (painter)
               UIDrawGlyph(painter, x, y, '\\', ui->theme.codeComment);
            x += glyphWidth;
            if (painter)
               UIDrawGlyph(painter, x, y, 'n', ui->theme.codeComment);
            x = clientBounds.l;
            y += glyphHeight;
         } else if (display->data[i] == '\t') {
            if (painter)
               UIDrawGlyph(painter, x, y, '\\', ui->theme.codeNumber);
            x += glyphWidth;
            if (painter)
               UIDrawGlyph(painter, x, y, 't', ui->theme.codeNumber);
            x += glyphWidth;
         } else {
            const char* hexChars = "0123456789ABCDEF";
            if (painter)
               UIDrawGlyph(painter, x, y, '<', ui->theme.codeNumber);
            x += glyphWidth;
            if (painter)
               UIDrawGlyph(painter, x, y, hexChars[(display->data[i] & 0xF0) >> 4], ui->theme.codeNumber);
            x += glyphWidth;
            if (painter)
               UIDrawGlyph(painter, x, y, hexChars[(display->data[i] & 0x0F) >> 0], ui->theme.codeNumber);
            x += glyphWidth;
            if (painter)
               UIDrawGlyph(painter, x, y, '>', ui->theme.codeNumber);
            x += glyphWidth;
         }
      } else {
         if (painter)
            UIDrawGlyph(painter, x, y, display->data[i], ui->theme.codeDefault);
         x += glyphWidth;
      }
   }

   return y - clientBounds.t + glyphHeight;
}

int ViewWindowStringMessage(UIElement* element, UIMessage message, int di, void* dp) {
   ViewWindowString* display = (ViewWindowString*)element;

   if (message == UIMessage::DESTROY) {
      display->data.reset();
   } else if (message == UIMessage::LAYOUT) {
      UIRectangle scrollBarBounds = element->bounds;
      scrollBarBounds.l           = scrollBarBounds.r - ui_size::SCROLL_BAR * element->window->scale;
      UIRectangle clientBounds    = element->bounds;
      clientBounds.r -= ui_size::SCROLL_BAR * element->window->scale;
      display->vScroll->maximum = ViewWindowStringLayout(display, nullptr, 0);
      display->vScroll->page    = element->bounds.height();
      display->vScroll->Move(scrollBarBounds, true);
   } else if (message == UIMessage::PAINT) {
      UIDrawBlock((UIPainter*)dp, element->bounds, ui->theme.codeBackground);
      ViewWindowStringLayout(display, (UIPainter*)dp, display->vScroll->position);
   } else if (message == UIMessage::MOUSE_WHEEL) {
      return display->vScroll->Message(message, di, dp);
   } else if (message == UIMessage::SCROLLED) {
      element->Repaint(nullptr);
   }

   return 0;
}

void ViewWindowView(void* cp) {
   // Get the selected watch expression.
   UIElement* watchElement = ctx.InterfaceWindowSwitchToAndFocus("Watch");
   if (!watchElement)
      return;
   WatchWindow* w = (WatchWindow*)watchElement->cp;
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
   panel->DestroyDescendents();
   UIButton* button = UIButtonCreate(panel, 0, "View (Ctrl+Shift+V)");
   button->invoke   = [panel]() { ViewWindowView(panel); };

   // Get information about the watch expression.
   char type[256], buffer[256];
   char oldFormat = watch->format;
   watch->format  = 0;

   auto res = WatchEvaluate("gf_typeof", watch);
   resize_to_lf(res);
   StringFormat(type, sizeof(type), "%s", res.c_str());
   StringFormat(buffer, sizeof(buffer), "Type: %s", type);
   UILabelCreate(panel, 0, buffer);

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

      StringFormat(buffer, sizeof(buffer), " 8b: %d %u 0x%x '%c'", (int8_t)value, (uint8_t)value, (uint8_t)value,
                   (char)value);
      UILabelCreate(panel, 0, buffer);
      StringFormat(buffer, sizeof(buffer), "16b: %d %u 0x%x", (int16_t)value, (uint16_t)value, (uint16_t)value);
      UILabelCreate(panel, 0, buffer);
      StringFormat(buffer, sizeof(buffer), "32b: %d %u 0x%x", (int32_t)value, (uint32_t)value, (uint32_t)value);
      UILabelCreate(panel, 0, buffer);
      StringFormat(buffer, sizeof(buffer), "64b: %ld %lu 0x%lx", (int64_t)value, (uint64_t)value, (uint64_t)value);
      UILabelCreate(panel, 0, buffer);

      int p = StringFormat(buffer, sizeof(buffer), "Bin: ");

      for (int64_t i = 63; i >= 32; i--) {
         buffer[p++] = (value & ((uint64_t)1 << i)) ? '1' : '0';
         if ((i & 7) == 0)
            buffer[p++] = ' ';
      }

      UILabelCreate(panel, 0, {buffer, static_cast<size_t>(p)});

      p = StringFormat(buffer, sizeof(buffer), "     ");

      for (int64_t i = 31; i >= 0; i--) {
         buffer[p++] = (value & ((uint64_t)1 << i)) ? '1' : '0';
         if ((i & 7) == 0)
            buffer[p++] = ' ';
      }

      UILabelCreate(panel, 0, {buffer, static_cast<size_t>(p)});

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
         StringFormat(address, sizeof(address), "%s", res.c_str());
      } else {
         char* end = (char*)strchr(res.c_str() + 1, ' ');
         if (!end)
            goto unrecognised;
         *end = 0;
         StringFormat(address, sizeof(address), "%s", res.c_str() + 1);
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

      StringFormat(buffer, sizeof(buffer), "dump binary memory %s (%s) (%s+%d)", tempPath, address, address, length);
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
         StringFormat(buffer, sizeof(buffer), "%d+1 bytes", length);
         UILabelCreate(panel, UIElement::H_FILL, buffer);
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
      StringFormat(buffer, sizeof(buffer), "dump binary memory %s (%s) (%s+%d)", tempPath, res.c_str(), res.c_str(),
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
         StringFormat(buffer, sizeof(buffer), "Determinant: %f", determinant);
         UILabelCreate(panel, 0, buffer);
      }
   } else {
   unrecognised:;
      // TODO Custom view.
      // TODO Table view for array of structures.
      UILabelCreate(panel, 0, "No view available for type.");
   }

   // Relayout the panel.
   panel->Refresh();
}

void ViewWindowView() {
   ViewWindowView(nullptr);
}

void ViewWindowUpdate(const char* data, UIElement* element) {}

UIElement* ViewWindowCreate(UIElement* parent) {
   UIPanel*  panel  = UIPanelCreate(parent, UIPanel::EXPAND | UIPanel::COLOR_1);
   UIButton* button = UIButtonCreate(panel, 0, "View (Ctrl+Shift+V)");
   button->invoke   = [panel]() { ViewWindowView(panel); };
   UILabelCreate(panel, 0, "Select a watch expression, then click View.");
   return panel;
}

// ----------------------------------------------------------
// Waveform display:
// ----------------------------------------------------------

int WaveformDisplayMessage(UIElement* element, UIMessage message, int di, void* dp);
int WaveformDisplayZoomButtonMessage(UIElement* element, UIMessage message, int di, void* dp);
int WaveformDisplayNormalizeButtonMessage(UIElement* element, UIMessage message, int di, void* dp);

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
      , scrollBar(new UIScrollBar(this, UIElement::NON_CLIENT | UIScrollBar::HORIZONTAL))
      , zoomOut(new UIButton(this, UIButton::SMALL, "-"))
      , zoomIn(new UIButton(this, UIButton::SMALL, "+"))
      , normalize(new UIButton(this, UIButton::SMALL, "Norm"))
      , dragLastX(0)
      , dragLastModification(0)
      , peak(0) {
      zoomOut->messageUser   = WaveformDisplayZoomButtonMessage;
      zoomIn->messageUser    = WaveformDisplayZoomButtonMessage;
      normalize->messageUser = WaveformDisplayNormalizeButtonMessage;
   }
};

void WaveformDisplayDrawVerticalLineWithTranslucency(UIPainter* painter, UIRectangle rectangle, uint32_t color,
                                                     uint32_t alpha) {
   rectangle = intersection(painter->clip, rectangle);
   if (!rectangle.valid())
      return;
   uint32_t* bits = painter->bits + rectangle.t * painter->width + rectangle.l;

   for (int y = 0; y < rectangle.b - rectangle.t; y++) {
      uint32_t* destination = &bits[y * painter->width];
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

int WaveformDisplayMessage(UIElement* element, UIMessage message, int di, void* dp) {
   WaveformDisplay* display = (WaveformDisplay*)element;

   if (display->sampleCount == 0 && message != UIMessage::DESTROY) {
      return 0;
   }

   if (message == UIMessage::DESTROY) {
      free(display->samples);
      display->samples = nullptr;
   } else if (message == UIMessage::LAYOUT) {
      if (display->samplesOnScreen > (int)display->sampleCount) {
         display->samplesOnScreen = display->sampleCount;
      }

      int         scrollBarHeight = ui_size::SCROLL_BAR * element->window->scale;
      UIRectangle scrollBarBounds = element->bounds;
      scrollBarBounds.t           = scrollBarBounds.b - scrollBarHeight;
      display->scrollBar->maximum = display->sampleCount;
      display->scrollBar->page    = display->samplesOnScreen;
      display->scrollBar->Move(scrollBarBounds, true);

      display->zoomOut->Move(UIRectangle(element->bounds.l + (int)(15 * element->window->scale),
                                         element->bounds.l + (int)(45 * element->window->scale),
                                         element->bounds.t + (int)(15 * element->window->scale),
                                         element->bounds.t + (int)(45 * element->window->scale)),
                             true);
      display->zoomIn->Move(UIRectangle(element->bounds.l + (int)(45 * element->window->scale),
                                        element->bounds.l + (int)(75 * element->window->scale),
                                        element->bounds.t + (int)(15 * element->window->scale),
                                        element->bounds.t + (int)(45 * element->window->scale)),
                            true);
      display->normalize->Move(UIRectangle(element->bounds.l + (int)(75 * element->window->scale),
                                           element->bounds.l + (int)(135 * element->window->scale),
                                           element->bounds.t + (int)(15 * element->window->scale),
                                           element->bounds.t + (int)(45 * element->window->scale)),
                               true);
   } else if (message == UIMessage::MOUSE_DRAG && element->window->pressedButton == 1) {
      display->scrollBar->position += display->dragLastModification;
      display->dragLastModification =
         (element->window->cursor.x - display->dragLastX) * display->samplesOnScreen / element->bounds.width();
      display->scrollBar->position -= display->dragLastModification;
      display->Refresh();
   } else if (message == UIMessage::MOUSE_DRAG && element->window->pressedButton == 2) {
      display->Repaint(NULL);
   } else if (message == UIMessage::MOUSE_MOVE) {
      display->Repaint(NULL);
   } else if (message == UIMessage::MIDDLE_UP) {
      int l = element->window->cursor.x - element->bounds.l, r = display->dragLastX - element->bounds.l;
      if (r < l) {
         int t = l;
         l     = r;
         r     = t;
      }
      float lf = (float)l / element->bounds.width() * display->samplesOnScreen + display->scrollBar->position;
      float rf = (float)r / element->bounds.width() * display->samplesOnScreen + display->scrollBar->position;

      if (rf - lf >= display->minimumZoom) {
         display->scrollBar->position = lf;
         display->samplesOnScreen     = rf - lf;
      }

      display->Refresh();
   } else if (message == UIMessage::LEFT_DOWN || message == UIMessage::MIDDLE_DOWN) {
      display->dragLastX            = element->window->cursor.x;
      display->dragLastModification = 0;
   } else if (message == UIMessage::MOUSE_WHEEL) {
      int    divisions   = di / 72;
      double factor      = 1;
      double perDivision = 1.2f;
      while (divisions > 0)
         factor *= perDivision, divisions--;
      while (divisions < 0)
         factor /= perDivision, divisions++;
      double mouse   = (double)(element->window->cursor.x - element->bounds.l) / element->bounds.width();
      double newZoom = (double)display->samplesOnScreen / display->sampleCount * factor;

      if (newZoom * display->sampleCount >= display->minimumZoom) {
         display->scrollBar->position += mouse * display->samplesOnScreen * (1 - factor);
         display->samplesOnScreen = newZoom * display->sampleCount;
      }

      display->Refresh();
   } else if (message == UIMessage::SCROLLED) {
      element->Repaint(NULL);
   } else if (message == UIMessage::PAINT) {
      UIRectangle client = element->bounds;
      client.b -= display->scrollBar->bounds.height();

      UIPainter*  painter = (UIPainter*)dp;
      UIRectangle oldClip = painter->clip;
      painter->clip       = intersection(client, painter->clip);
      int ym              = (client.t + client.b) / 2;
      int h2              = (client.b - client.t) / 2;
      int yp              = ym;
      UIDrawBlock(painter, painter->clip, ui->theme.panel1);
      UIDrawBlock(painter, UIRectangle(client.l, client.r, ym, ym + 1), 0x707070);

      float yScale =
         (display->normalize->flags & UIButton::CHECKED) && display->peak > 0.00001f ? 1.0f / display->peak : 1.0f;

      int    sampleOffset = (int)display->scrollBar->position;
      float* samples      = &display->samples[display->channels * sampleOffset];
      int    sampleCount  = display->samplesOnScreen;
      UI_ASSERT(sampleOffset + sampleCount <= (int)display->sampleCount);

      if (sampleCount > client.width()) {
         uint32_t alpha = 255 - 80 * (display->channels - 1);

         for (size_t channel = 0; channel < display->channels; channel++) {
            for (int32_t x = painter->clip.l; x < painter->clip.r; x++) {
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
               WaveformDisplayDrawVerticalLineWithTranslucency(painter, r, ui->theme.text, alpha);
            }
         }
      } else {
         for (size_t channel = 0; channel < display->channels; channel++) {
            yp = ym + h2 * yScale * samples[channel + 0];

            for (int32_t i = 0; i < sampleCount; i++) {
               int32_t x0 = (int)((float)i / sampleCount * client.width()) + client.l;
               int32_t x1 = (int)((float)(i + 1) / sampleCount * client.width()) + client.l;
               int32_t y  = ym + h2 * yScale * samples[channel + display->channels * (int)i];
               UIDrawLine(painter, x0, yp, x1, y, ui->theme.text);
               yp = y;
            }
         }

         if (sampleCount < client.width() / 4) {
            for (size_t channel = 0; channel < display->channels; channel++) {
               for (int32_t i = 0; i < sampleCount; i++) {
                  int32_t x1 = (int)((float)(i + 1) / sampleCount * client.width()) + client.l;
                  int32_t y  = ym + h2 * yScale * samples[channel + display->channels * (int)i];
                  UIDrawBlock(painter, UIRectangle(x1 - 2, x1 + 2, y - 2, y + 2),
                              channel % 2 ? 0xFFFF00FF : 0xFF00FFFF);
               }
            }
         }

         int mouseXSample =
            (float)(element->window->cursor.x - client.l) / element->bounds.width() * display->samplesOnScreen - 0.5f;

         if (mouseXSample >= 0 && mouseXSample < sampleCount && element->clip.contains(element->window->cursor) &&
             !display->scrollBar->clip.contains(element->window->cursor)) {
            int         stringOffset = 20 * element->window->scale;
            UIRectangle stringRectangle =
               UIRectangle(client.l + stringOffset, client.r - stringOffset, client.t + stringOffset,
                           client.t + stringOffset + UIMeasureStringHeight());
            char buffer[100];
            snprintf(buffer, sizeof(buffer), "%d: ", (int)(mouseXSample + display->scrollBar->position));

            for (size_t channel = 0; channel < display->channels; channel++) {
               char  buffer2[30];
               float sample = samples[channel + display->channels * mouseXSample];
               snprintf(buffer2, sizeof(buffer2), "%s%s%.5f", channel ? ", " : "", sample >= 0.0f ? "+" : "", sample);
               if (strlen(buffer) + strlen(buffer2) > sizeof(buffer) - 1)
                  break;
               strcat(buffer, buffer2);
            }

            UIDrawString(painter, stringRectangle, buffer, ui->theme.text, UIAlign::right, NULL);

            int32_t x1 = (int)((float)(mouseXSample + 1) / sampleCount * client.width()) + client.l;
            WaveformDisplayDrawVerticalLineWithTranslucency(painter, UIRectangle(x1, x1 + 1, client.t, client.b),
                                                            0xFFFFFF, 100);
         }
      }

      if (element->window->pressedButton == 2 && element->window->pressed) {
         int l = element->window->cursor.x, r = display->dragLastX;
         UIDrawInvert(painter, UIRectangle(l > r ? r : l, l > r ? l : r, element->bounds.t, element->bounds.r));
      }

      painter->clip = oldClip;
   }

   return 0;
}

int WaveformDisplayZoomButtonMessage(UIElement* element, UIMessage message, int di, void* dp) {
   WaveformDisplay* display = (WaveformDisplay*)element->parent;

   if (message == UIMessage::CLICKED) {
      if (element == display->zoomOut) {
         display->scrollBar->position -= display->samplesOnScreen / 2;
         display->samplesOnScreen *= 2;
      } else if (element == display->zoomIn && display->samplesOnScreen >= display->minimumZoom) {
         display->samplesOnScreen /= 2;
         display->scrollBar->position += display->samplesOnScreen / 2;
      }

      display->Refresh();
   }

   return 0;
}

int WaveformDisplayNormalizeButtonMessage(UIElement* element, UIMessage message, int di, void* dp) {
   WaveformDisplay* display = (WaveformDisplay*)element->parent;

   if (message == UIMessage::CLICKED) {
      element->flags ^= UIButton::CHECKED;
      display->Refresh();
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
   StringFormat(_pointerResult, sizeof(_pointerResult), "%s", pointerResult.c_str());
   pointerResult = strstr(_pointerResult, " 0x");
   if (pointerResult.empty()) {
      return "Pointer to sample data does not look like an address!";
   }

   size_t byteCount = sampleCount * channels * 4;
   float* samples   = (float*)malloc(byteCount);

   char transferPath[PATH_MAX];
   realpath(".transfer.gf", transferPath);

   char buffer[PATH_MAX * 2];
   StringFormat(buffer, sizeof(buffer), "dump binary memory %s (%s) (%s+%d)", transferPath, pointerResult.c_str() + 1,
                pointerResult.c_str() + 1, byteCount);
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

int WaveformViewerWindowMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::DESTROY) {
      DataViewerRemoveFromAutoUpdateList(element);
      delete (WaveformViewer*)element->cp;
   } else if (message == UIMessage::GET_WIDTH) {
      return 300;
   } else if (message == UIMessage::GET_HEIGHT) {
      return 300;
   }

   return 0;
}

void WaveformViewerAutoUpdateCallback(UIElement* element) {
   WaveformViewer* viewer = (WaveformViewer*)element->cp;
   WaveformViewerUpdate(viewer->pointer, viewer->sampleCount, viewer->channels, element);
}

int WaveformViewerRefreshMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::CLICKED) {
      WaveformViewerAutoUpdateCallback(element->parent);
   }

   return 0;
}

void WaveformViewerSaveToFile(WaveformDisplay* display) {
   static char* path = NULL;
   const char*  result =
      UIDialogShow(windowMain, 0, "Save to file       \nPath:\n%t\n%f%b%b%b", &path, "Save", "Save and open", "Cancel");
   if (0 == strcmp(result, "Cancel"))
      return;
   FILE* f = fopen(path, "wb");
   if (!f) {
      UIDialogShow(windowMain, 0, "Unable to open file for writing.\n%f%b", "OK");
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

   if (0 == strcmp(result, "Save and open")) {
      char buffer[4000];
      snprintf(buffer, sizeof(buffer), "xdg-open \"%s\"", path);
      system(buffer);
   }
}

int WaveformViewerDisplayMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::RIGHT_UP) {
      WaveformDisplay* display = (WaveformDisplay*)element;
      UIMenu*          menu    = UIMenuCreate(element->window, UIMenu::NO_SCROLL);
      UIMenuAddItem(menu, 0, "Save to .wav...", [display]() { WaveformViewerSaveToFile(display); });
      UIMenuShow(menu);
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
         StringFormat(viewer->pointer, sizeof(viewer->pointer), "%s", pointerString);
      if (sampleCountString)
         StringFormat(viewer->sampleCount, sizeof(viewer->sampleCount), "%s", sampleCountString);
      if (channelsString)
         StringFormat(viewer->channels, sizeof(viewer->channels), "%s", channelsString);

      UIMDIChild* window     = UIMDIChildCreate(dataWindow, UIMDIChild::CLOSE_BUTTON, UIRectangle(0), "Waveform");
      window->messageUser    = WaveformViewerWindowMessage;
      window->cp             = viewer;
      viewer->autoToggle     = UIButtonCreate(window, UIButton::SMALL | UIElement::NON_CLIENT, "Auto");
      viewer->autoToggle->cp = (void*)WaveformViewerAutoUpdateCallback;
      viewer->autoToggle->messageUser = DataViewerAutoUpdateButtonMessage;
      UIButtonCreate(window, UIButton::SMALL | UIElement::NON_CLIENT, "Refresh")->messageUser =
         WaveformViewerRefreshMessage;
      owner = window;

      UIPanel* panel               = UIPanelCreate(owner, UIPanel::EXPAND);
      viewer->labelPanel           = UIPanelCreate(panel, UIPanel::COLOR_1 | UIElement::V_FILL);
      viewer->label                = UILabelCreate(viewer->labelPanel, UIElement::H_FILL, {});
      viewer->display              = WaveformDisplayCreate(panel, UIElement::V_FILL);
      viewer->display->messageUser = WaveformViewerDisplayMessage;
   }

   WaveformViewer* viewer    = (WaveformViewer*)owner->cp;
   viewer->parsedSampleCount = sampleCount, viewer->parsedChannels = channels;

   if (error) {
      UILabelSetContent(viewer->label, error);
      viewer->labelPanel->flags &= ~UIElement::HIDE;
      viewer->display->flags |= UIElement::HIDE;
   } else {
      viewer->labelPanel->flags |= UIElement::HIDE;
      viewer->display->flags &= ~UIElement::HIDE;
      WaveformDisplaySetContent(viewer->display, samples, sampleCount, channels);
   }

   viewer->display->Refresh();
   viewer->label->Refresh();
   viewer->labelPanel->parent->Refresh();
   owner->Refresh();
   dataWindow->Refresh();

   free(samples);
}

void WaveformAddDialog() {
   static char *pointer = nullptr, *sampleCount = nullptr, *channels = nullptr;

   const char* result =
      UIDialogShow(windowMain, 0,
                   "Add waveform\n\n%l\n\nPointer to samples: (float *)\n%t\nSample count (per channel):\n%t\n"
                   "Channels (interleaved):\n%t\n\n%l\n\n%f%b%b",
                   &pointer, &sampleCount, &channels, "Add", "Cancel");

   if (0 == strcmp(result, "Add")) {
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
      .label = nullptr,
      .shortcut{.code = UI_KEYCODE_LETTER('V'), .ctrl = true, .shift = true, .invoke = []() { ViewWindowView(); }}
   });
}

#if __has_include("plugins.cpp")
   #include "plugins.cpp"
#endif

// ------------------------------------------------------
// Interface and main:
// ------------------------------------------------------

bool ElementHidden(UIElement* element) {
   while (element) {
      if (element->flags & UIElement::HIDE) {
         return true;
      } else {
         element = element->parent;
      }
   }

   return false;
}

void MsgReceivedData(str_unique_ptr input) {
   ctx.programRunning = false;

   if (ctx.firstUpdate) {
      (void)EvaluateCommand(pythonCode);

      char path[PATH_MAX];
      StringFormat(path, sizeof(path), "%s/.config/gf2_watch.txt", getenv("HOME"));
      vector<char> data_vec = LoadFile(path, NULL);
      const char*  data     = data_vec.data();

      while (data && restoreWatchWindow) {
         const char* end = strchr(data, '\n');
         if (!end)
            break;
         WatchAddExpression2(string_view{data, static_cast<size_t>(end - data)});
         data = end + 1;
      }

      ctx.firstUpdate = false;
   }

   if (WatchLoggerUpdate(input.get()))
      return;
   if (showingDisassembly)
      DisassemblyUpdateLine();

   DebuggerGetStack();
   DebuggerGetBreakpoints();

   for (auto& [name, iw] : ctx.interfaceWindows) {
      InterfaceWindow* window = &iw;
      if (!window->update || !window->element)
         continue;
      if (!window->alwaysUpdate && ElementHidden(window->element))
         window->queuedUpdate = true;
      else
         window->update(input.get(), window->element);
   }

   DataViewersUpdateAll();

   if (displayOutput) {
      UICodeInsertContent(displayOutput, input.get(), false);
      displayOutput->Refresh();
   }

   if (trafficLight)
      trafficLight->Repaint(nullptr);
}

void MsgReceivedControl(str_unique_ptr input) {
   const char* start = input.get();
   char*       end   = strchr(input.get(), '\n');
   if (end)
      *end = 0;

   if (start[0] == 'f' && start[1] == ' ') {
      DisplaySetPosition(start + 2, 1, false);
   } else if (start[0] == 'l' && start[1] == ' ') {
      DisplaySetPosition(nullptr, sv_atoi(start, 2), false);
   } else if (start[0] == 'c' && start[1] == ' ') {
      (void)CommandParseInternal(start + 2, false);
   }
}

auto gdb_invoker(string_view cmd) {
   return [cmd]() { CommandSendToGDB(cmd); };
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
      .label = "Break\tF8", .shortcut{.code = UI_KEYCODE_FKEY(8), .invoke = [&]() { ctx.InterruptGdb(0); }}
   });
   interfaceCommands.push_back({
      .label = "Toggle breakpoint\tF9",
      .shortcut{.code = UI_KEYCODE_FKEY(9), .invoke = []() { CommandToggleBreakpoint(); }}
   });
   interfaceCommands.push_back({
      .label = "Sync with gvim\tF2", .shortcut{.code = UI_KEYCODE_FKEY(2), .invoke = CommandSyncWithGvim}
   });
   interfaceCommands.push_back({
      .label = "Ask GDB for PWD\tCtrl+Shift+P",
      .shortcut{.code = UI_KEYCODE_LETTER('P'), .ctrl = true, .shift = true, .invoke = gdb_invoker("gf-get-pwd")}
   });
   interfaceCommands.push_back({
      .label = "Toggle disassembly\tCtrl+D",
      .shortcut{.code = UI_KEYCODE_LETTER('D'), .ctrl = true, .invoke = CommandToggleDisassembly}
   });
   interfaceCommands.push_back({
      .label = "Set disassembly mode\tCtrl+M",
      .shortcut{.code = UI_KEYCODE_LETTER('M'), .ctrl = true, .invoke = CommandSetDisassemblyMode}
   });
   interfaceCommands.push_back({.label = "Add watch", .shortcut{.invoke = CommandAddWatch}});
   interfaceCommands.push_back({
      .label = "Inspect line", .shortcut{.code = UIKeycode::BACKTICK, .invoke = CommandInspectLine}
   });
   interfaceCommands.push_back({
      .label = nullptr,
      .shortcut{.code = UI_KEYCODE_LETTER('E'), .ctrl = true, .invoke = []() { CommandWatchAddEntryForAddress(); }}
   });
   interfaceCommands.push_back({
      .label = nullptr,
      .shortcut{.code = UI_KEYCODE_LETTER('G'), .ctrl = true, .invoke = []() { CommandWatchViewSourceAtAddress(); }}
   });
   interfaceCommands.push_back({
      .label = nullptr, .shortcut{.code = UI_KEYCODE_LETTER('B'), .ctrl = true, .invoke = CommandToggleFillDataTab}
   });
   interfaceCommands.push_back({
      .label = nullptr,
      .shortcut{.code = UI_KEYCODE_LETTER('P'), .ctrl = true, .shift = false, .invoke = CommandPreviousCommand}
   });
   interfaceCommands.push_back({
      .label = nullptr,
      .shortcut{.code = UI_KEYCODE_LETTER('N'), .ctrl = true, .shift = false, .invoke = CommandNextCommand}
   });
   interfaceCommands.push_back({
      .label = nullptr,
      .shortcut{.code = UI_KEYCODE_LETTER('L'), .ctrl = true, .shift = false, .invoke = CommandClearOutput}
   });

   msgReceivedData    = ReceiveMessageRegister(MsgReceivedData);
   msgReceivedControl = ReceiveMessageRegister(MsgReceivedControl);

   // received buffer contains debugger output to add to log window
   msgReceivedLog = ReceiveMessageRegister([](str_unique_ptr buffer) {
      assert(ctx.logWindow);
      UICodeInsertContent(ctx.logWindow, buffer.get(), false);
      ctx.logWindow->Refresh();
   });
}

void Context::InterfaceShowMenu(UIButton* self) {
   UIMenu* menu = UIMenuCreate((UIElement*)self, UIMenu::PLACE_ABOVE | UIMenu::NO_SCROLL);

   for (const auto& ic : interfaceCommands) {
      if (!ic.label)
         continue;
      UIMenuAddItem(menu, 0, ic.label, ic.shortcut.invoke);
   }

   UIMenuShow(menu);
}

UIElement* Context::InterfaceWindowSwitchToAndFocus(string_view target_name) {
   for (auto& [name, w] : interfaceWindows) {
      if (!w.element)
         continue;
      if (target_name != name)
         continue;

      if ((w.element->flags & UIElement::HIDE) && w.element->parent->messageClass == _UITabPaneMessage) {
         UITabPane* tabPane = (UITabPane*)w.element->parent;

         for (uint32_t i = 0; i < tabPane->children.size(); i++) {
            if (tabPane->children[i] == w.element) {
               tabPane->active = i;
               break;
            }
         }

         tabPane->Refresh();
      }

      if (w.focus) {
         w.focus(w.element);
      } else if (w.element->flags & UIElement::TAB_STOP) {
         w.element->Focus();
      }

      return w.element;
   }

   UIDialogShow(windowMain, 0, "Couldn't find the window '%s'.\n%f%B", target_name, "OK");
   return nullptr;
}

int MainWindowMessageProc(UIElement*, UIMessage message, int di, void* dp) {
   if (message == UIMessage::WINDOW_ACTIVATE) {
      DisplaySetPosition(currentFileFull, currentLine, false);
   } else {
      for (const auto& msgtype : receiveMessageTypes) {
         if (msgtype.message == message) {
            msgtype.callback(str_unique_ptr((char*)dp));
            break;
         }
      }
   }

   return 0;
}

int InterfaceTabPaneMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::LAYOUT) {
      element->messageClass(element, message, di, dp);

      for (auto& [name, w] : ctx.interfaceWindows) {
         if (w.element && (~w.element->flags & UIElement::HIDE) && w.queuedUpdate) {
            w.queuedUpdate = false;
            w.update("", w.element);
            w.element->Move(w.element->bounds, false);
         }
      }

      return 1;
   }

   return 0;
}

const char* InterfaceLayoutNextToken(const char* expected = nullptr) {
   static char buffer[32];
   char*       out = buffer;

   while (isspace(*layoutString)) {
      layoutString++;
   }

   char first = *layoutString;

   if (first == 0) {
      *out = 0;
   } else if (first == ',' || first == '(' || first == ')') {
      out[0] = first;
      out[1] = 0;
      layoutString++;
   } else if (isalnum(first)) {
      for (uintptr_t i = 0; i < sizeof(buffer) - 1; i++) {
         if (isalnum(*layoutString)) {
            *out++ = *layoutString++;
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
      uint32_t flags = UIElement::V_FILL | UIElement::H_FILL;
      if (*token == 'v')
         flags |= UIElement::VERTICAL;
      InterfaceLayoutNextToken("(");
      UIElement* container = UISplitPaneCreate(parent, flags, sv_atoi(InterfaceLayoutNextToken("#")) * 0.01f);
      InterfaceLayoutNextToken(",");
      InterfaceLayoutCreate(container);
      InterfaceLayoutNextToken(",");
      InterfaceLayoutCreate(container);
      InterfaceLayoutNextToken(")");
   } else if (0 == strcmp("t", token)) {
      InterfaceLayoutNextToken("(");
      char* copy = strdup(layoutString);
      for (uintptr_t i = 0; copy[i]; i++)
         if (copy[i] == ',')
            copy[i] = '\t';
         else if (copy[i] == ')')
            copy[i] = 0;
      UIElement* container   = UITabPaneCreate(parent, UIElement::V_FILL | UIElement::H_FILL, copy);
      container->messageUser = InterfaceTabPaneMessage;
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
         w.element = w.create(parent);
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
   StringFormat(globalConfigPath, sizeof(globalConfigPath), "%s/.config/gf2_config.ini", getenv("HOME"));
   StringFormat(localConfigPath, sizeof(localConfigPath), "%s/.project.gf", localConfigDirectory);

   ctx.SettingsLoad(true);
   auto ui_ptr = UIInitialise(ui_config);
   ui->theme   = uiThemeDark;

   // create fonts for interface and code
   // -----------------------------------
#ifdef UI_FREETYPE
   if (!fontPath) {
      // Ask fontconfig for a monospaced font. If this fails, the fallback font will be used.
      FILE* f = popen("fc-list | grep -F `fc-match mono | awk '{ print($1) }'` "
                      "| awk 'BEGIN { FS = \":\" } ; { print($1) }'",
                      "r");

      if (f) {
         char* buffer                          = (char*)malloc(PATH_MAX + 1);
         buffer[fread(buffer, 1, PATH_MAX, f)] = 0;
         pclose(f);
         char* newline = strchr(buffer, '\n');
         if (newline)
            *newline = 0;
         fontPath = buffer;
         print(std::cerr, "Using font {}\n", fontPath);
      }
   }
#endif

   fontCode = UIFontCreate(fontPath, fontSizeCode);
   UIFontActivate(UIFontCreate(fontPath, fontSizeInterface));

   windowMain              = UIWindowCreate(0, maximize ? UIWindow::MAXIMIZE : 0, "gf", window_width, window_height);
   windowMain->scale       = uiScale;
   windowMain->messageUser = MainWindowMessageProc;

   for (const auto& ic : interfaceCommands) {
      if (!(int)ic.shortcut.code)
         continue;
      UIWindowRegisterShortcut(windowMain, ic.shortcut);
   }

   switcherMain = UISwitcherCreate(windowMain, 0);
   InterfaceLayoutCreate(UIPanelCreate(switcherMain, UIPanel::EXPAND));
   UISwitcherSwitchTo(switcherMain, switcherMain->children[0]);

   if (*InterfaceLayoutNextToken()) {
      print(std::cerr, "Warning: Layout string has additional text after the end of the top-level entry.\n");
   }

   ctx.SettingsLoad(false);
   DebuggerStartThread();
   CommandSyncWithGvim();
   return ui_ptr;
}

Context::Context() {
   InterfaceAddBuiltinWindowsAndCommands();
   RegisterExtensions();
}

int main(int argc, char** argv) {
   auto ui_ptr = ctx.GfMain(argc, argv);
   if (!ui_ptr)
      return 1;

   UIMessageLoop();
   ctx.KillGdb();

   if (restoreWatchWindow && firstWatchWindow) {
      StringFormat(globalConfigPath, sizeof(globalConfigPath), "%s/.config/gf2_watch.txt", getenv("HOME"));
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
