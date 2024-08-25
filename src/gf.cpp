// TODO Disassembly window:
// 	- Setting/clearing/showing breakpoints.
// 	- Jump/run to line.
// 	- Shift+F10: run to next instruction (for skipping past loops).
// 	- Split source and disassembly view.

// TODO Inspect line mode:
// 	- Jump/run to selected line.
// 	- How to show overloaded variables correctly when moving lines?

// TODO More data visualization tools in the data window.

#include <cstdint>
#include <cstddef>
#include <cstdarg>
#include <cassert>

#include <pthread.h>
#include <signal.h>
#include <spawn.h>
#include <cstdio>
#include <ctype.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>
#include <poll.h>
#include <semaphore.h>
#include <ctime>

#include <vector>
#include <unordered_map>
#include <algorithm>
#include <ranges>
#include <memory>

namespace views = std::views;
namespace rng   = std::ranges;

using namespace std;

#include "luigi.hpp"

// ---------------------------------------------------------------------------------------------
//                              Data structures
// ---------------------------------------------------------------------------------------------
struct InterfaceCommand {
   const char* label = nullptr;
   UIShortcut  shortcut;
};

struct InterfaceWindow {
   const char* name = nullptr;
   UIElement* (*create)(UIElement* parent);
   void       (*update)(const char* data, UIElement* element);
   void       (*focus)(UIElement* element);
   UIElement* element = nullptr;
   bool       queuedUpdate, alwaysUpdate;
   void       (*config)(const char* key, const char* value);
};

struct InterfaceDataViewer {
   const char* addButtonLabel = nullptr;
   void (*addButtonCallback)();
};

struct INIState {
   char * buffer = nullptr;
   char *section = nullptr;
   char *key = nullptr;
   char *value = nullptr;
   size_t bytes = 0, sectionBytes = 0, keyBytes = 0, valueBytes = 0;
};

struct ReceiveMessageType {
   UIMessage message;
   void (*callback)(char* input);
};

struct WatchWindow;

// --------------------------------------------------------------------------------------------
FILE*                       commandLog = nullptr;
char                        emptyString;
bool                        programRunning  = true;
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
vector<InterfaceWindow>     interfaceWindows;
vector<InterfaceCommand>    interfaceCommands;
vector<InterfaceDataViewer> interfaceDataViewers;
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

UIConfig ui_config = {
   .rfu = true
};

// Current file and line:

char   currentFile[PATH_MAX];
char   currentFileFull[PATH_MAX];
int    currentLine;
time_t currentFileReadTime;
bool   showingDisassembly;
char   previousLocation[256];

// User interface:

UIWindow*   windowMain = nullptr;
UISwitcher* switcherMain = nullptr;

UICode*    displayCode = nullptr;
UICode*    displayOutput = nullptr;
UITextbox* textboxInput = nullptr;
UISpacer*  trafficLight = nullptr;

UIMDIClient* dataWindow = nullptr;
UIPanel*     dataTab = nullptr;

UIFont* fontCode = nullptr;

// Breakpoints:

struct Breakpoint {
   int      number;
   char     file[PATH_MAX];
   char     fileFull[PATH_MAX];
   int      line;
   bool     watchpoint;
   int      hit;
   bool     enabled;
   char     condition[128];
   uint64_t conditionHash;
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
void       InterfaceShowMenu(UIButton* self);
UIElement* InterfaceWindowSwitchToAndFocus(const char* name);
void       WatchAddExpression2(char* string);
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
   va_list arguments;

   va_start(arguments, format);
   size_t length = vsnprintf(buffer, bufferSize, format, arguments);
   va_end(arguments);

   if (length > bufferSize) {
      // HACK This could truncate a UTF-8 codepoint.
      length = bufferSize;
   }

   return length;
}

char* LoadFile(const char* path, size_t* _bytes) {
   FILE* f = fopen(path, "rb");

   if (!f) {
      return nullptr;
   }

   fseek(f, 0, SEEK_END);
   size_t bytes = ftell(f);
   fseek(f, 0, SEEK_SET);
   char* buffer = (char*)malloc(bytes + 1);

   if (!buffer) {
      fclose(f);
      return nullptr;
   }

   buffer[bytes] = 0;
   fread(buffer, 1, bytes, f);
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
      UIDrawRectangle((UIPainter*)dp, element->bounds, programRunning ? ui->theme.accent1 : ui->theme.accent2,
                      ui->theme.border, UIRectangle(1));
   }

   return 0;
}

int SourceFindEndOfBlock() {
   if (!currentLine || currentLine - 1 >= displayCode->lineCount)
      return -1;

   int tabs = 0;

   for (int i = 0; i < displayCode->lines[currentLine - 1].bytes; i++) {
      if (isspace(displayCode->content[displayCode->lines[currentLine - 1].offset + i]))
         tabs++;
      else
         break;
   }

   for (int j = currentLine; j < displayCode->lineCount; j++) {
      int t = 0;

      for (int i = 0; i < displayCode->lines[j].bytes - 1; i++) {
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
   if (!currentLine || currentLine - 1 >= displayCode->lineCount)
      return false;
   uintptr_t offset = displayCode->lines[currentLine - 1].offset;
   bool      found  = false;

   // Look forwards for the end of the call ");".

   while (offset < displayCode->contentBytes - 1) {
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

   *start = *end = displayCode->content + offset;
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
         *start = displayCode->content + offset + 1;
         found  = true;
         break;
      }
   }

   return found;
}

UIMessage ReceiveMessageRegister(void (*callback)(char* input)) {
   receiveMessageTypes.push_back({.message = msgReceivedNext, .callback = callback});
   msgReceivedNext = (UIMessage)((uint32_t)msgReceivedNext + 1);
   return receiveMessageTypes.back().message;
}

// ------------------------------------------------------
// Debugger interaction:
// ------------------------------------------------------

volatile int       pipeToGDB;
volatile pid_t     gdbPID;
volatile pthread_t gdbThread;
pthread_cond_t     evaluateEvent;
pthread_mutex_t    evaluateMutex;
char*              evaluateResult;
bool               evaluateMode;
char**             gdbArgv;
int                gdbArgc;

#if defined(__OpenBSD__)
const char* gdbPath = "egdb";
#else
const char* gdbPath = "gdb";
#endif

char  initialGDBCommand[8192] = "set prompt (gdb) \n";
bool  firstUpdate             = true;
void* sendAllGDBOutputToLogWindowContext;

void* DebuggerThread(void*) {
   int outputPipe[2], inputPipe[2];
   pipe(outputPipe);
   pipe(inputPipe);

#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__APPLE__)
   gdbPID = fork();

   if (gdbPID == 0) {
      setsid();
      dup2(inputPipe[0], 0);
      dup2(outputPipe[1], 1);
      dup2(outputPipe[1], 2);
      execvp(gdbPath, gdbArgv);
      fprintf(stderr, "Error: Couldn't execute gdb.\n");
      exit(EXIT_FAILURE);
   } else if (gdbPID < 0) {
      fprintf(stderr, "Error: Couldn't fork.\n");
      exit(EXIT_FAILURE);
   }
#else
   posix_spawn_file_actions_t actions = {};
   posix_spawn_file_actions_init(&actions);
   posix_spawn_file_actions_adddup2(&actions, inputPipe[0], 0);
   posix_spawn_file_actions_adddup2(&actions, outputPipe[1], 1);
   posix_spawn_file_actions_adddup2(&actions, outputPipe[1], 2);

   posix_spawnattr_t attrs = {};
   posix_spawnattr_init(&attrs);
   posix_spawnattr_setflags(&attrs, POSIX_SPAWN_SETSID);

   posix_spawnp((pid_t*)&gdbPID, gdbPath, &actions, &attrs, gdbArgv, environ);

   posix_spawn_file_actions_destroy(&actions);
   posix_spawnattr_destroy(&attrs);
#endif

   pipeToGDB = inputPipe[1];

   write(pipeToGDB, initialGDBCommand, strlen(initialGDBCommand));

   char*  catBuffer          = NULL;
   size_t catBufferUsed      = 0;
   size_t catBufferAllocated = 0;

   while (true) {
      char buffer[512 + 1];
      int  count    = read(outputPipe[0], buffer, 512);
      buffer[count] = 0;
      if (!count)
         break;

      if (sendAllGDBOutputToLogWindowContext && !evaluateMode) {
         void* message = malloc(count + sizeof(sendAllGDBOutputToLogWindowContext) + 1);
         memcpy(message, &sendAllGDBOutputToLogWindowContext, sizeof(sendAllGDBOutputToLogWindowContext));
         strcpy((char*)message + sizeof(sendAllGDBOutputToLogWindowContext), buffer);
         UIWindowPostMessage(windowMain, msgReceivedLog, message);
      }

      size_t neededSpace = catBufferUsed + count + 1;

      if (neededSpace > catBufferAllocated) {
         catBufferAllocated *= 2;
         if (catBufferAllocated < neededSpace)
            catBufferAllocated = neededSpace;
         catBuffer = (char*)realloc(catBuffer, catBufferAllocated);
      }

      strcpy(catBuffer + catBufferUsed, buffer);
      catBufferUsed += count;
      if (!strstr(catBuffer, "(gdb) "))
         continue;

      // printf("got (%d) {%s}\n", evaluateMode, copy);

      // Notify the main thread we have data.

      if (evaluateMode) {
         free(evaluateResult);
         evaluateResult = catBuffer;
         evaluateMode   = false;
         pthread_mutex_lock(&evaluateMutex);
         pthread_cond_signal(&evaluateEvent);
         pthread_mutex_unlock(&evaluateMutex);
      } else {
         UIWindowPostMessage(windowMain, msgReceivedData, catBuffer);
      }

      catBuffer          = NULL;
      catBufferUsed      = 0;
      catBufferAllocated = 0;
   }

   return nullptr;
}

void DebuggerStartThread() {
   pthread_t      debuggerThread;
   pthread_attr_t attributes;
   pthread_attr_init(&attributes);
   pthread_create(&debuggerThread, &attributes, DebuggerThread, nullptr);
   gdbThread = debuggerThread;
}

void DebuggerSend(const char* string, bool echo, bool synchronous) {
   if (synchronous) {
      if (programRunning) {
         kill(gdbPID, SIGINT);
         usleep(1000 * 1000);
         programRunning = false;
      }

      evaluateMode = true;
      pthread_mutex_lock(&evaluateMutex);
   }

   if (programRunning) {
      kill(gdbPID, SIGINT);
   }

   programRunning = true;
   if (trafficLight)
      trafficLight->Repaint(nullptr);

   // printf("sending: %s\n", string);

   char newline = '\n';

   if (echo && displayOutput) {
      UICodeInsertContent(displayOutput, string, -1, false);
      displayOutput->Refresh();
   }

   write(pipeToGDB, string, strlen(string));
   write(pipeToGDB, &newline, 1);

   if (synchronous) {
      struct timespec timeout;
      clock_gettime(CLOCK_REALTIME, &timeout);
      timeout.tv_sec++;
      pthread_cond_timedwait(&evaluateEvent, &evaluateMutex, &timeout);
      pthread_mutex_unlock(&evaluateMutex);
      programRunning = false;
      if (trafficLight)
         trafficLight->Repaint(nullptr);
      if (!evaluateResult)
         evaluateResult = strdup("\n(gdb) \n");
   }
}

void EvaluateCommand(const char* command, bool echo = false) {
   DebuggerSend(command, echo, true);
}

const char* EvaluateExpression(const char* expression, const char* format = nullptr) {
   char buffer[1024];
   StringFormat(buffer, sizeof(buffer), "p%s %s", format ?: "", expression);
   EvaluateCommand(buffer);
   char* result = strchr(evaluateResult, '=');

   if (result) {
      char* end = strchr(result, '\n');

      if (end) {
         *end = 0;
         return result;
      }
   }

   return nullptr;
}

void DebuggerClose() {
   kill(gdbPID, SIGKILL);
   pthread_cancel(gdbThread);
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
   char buffer[16];
   StringFormat(buffer, sizeof(buffer), "bt %d", backtraceCountLimit);
   EvaluateCommand(buffer);
   stack.clear();

   const char* position = evaluateResult;

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
      StringFormat(entry.function, sizeof(entry.function), "%.*s", (int)(position - functionName), functionName);

      const char* file = strstr(position, " at ");

      if (file && file < next) {
         file += 4;
         const char* end = file;
         while (*end != '\n' && end < next)
            end++;
         StringFormat(entry.location, sizeof(entry.location), "%.*s", (int)(end - file), file);
      }

      stack.push_back(entry);

      if (!(*next))
         break;
      position = next + 1;
   }
}

void DebuggerGetBreakpoints() {
   EvaluateCommand("info break");
   breakpoints.clear();

   const char* position = evaluateResult;

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

      int number = atoi(position);

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
         StringFormat(breakpoint.condition, sizeof(breakpoint.condition), "%.*s", (int)(end - condition), condition);
         breakpoint.conditionHash = Hash((const uint8_t*)condition, end - condition);
      }

      const char* hitCountNeedle = "breakpoint already hit";
      const char* hitCount       = strstr(position, hitCountNeedle);
      if (hitCount)
         hitCount += strlen(hitCountNeedle);

      if (hitCount && hitCount < next) {
         breakpoint.hit = atoi(hitCount);
      }

      if (file && file < next) {
         const char* end = strchr(file, ':');

         if (end && isdigit(end[1])) {
            if (file[0] == '.' && file[1] == '/')
               file += 2;
            StringFormat(breakpoint.file, sizeof(breakpoint.file), "%.*s", (int)(end - file), file);
            breakpoint.line = atoi(end + 1);
         } else
            recognised = false;
      } else
         recognised = false;

      if (recognised) {
         realpath(breakpoint.file, breakpoint.fileFull);

         for (const auto& bp : breakpoints) {
            if (strcmp(bp.fileFull, breakpoint.fileFull) == 0 &&
                bp.conditionHash == breakpoint.conditionHash && bp.line == breakpoint.line) {
               // Prevent having identical breakpoints on the same line.
               char buffer[1024];
               StringFormat(buffer, 1024, "delete %d", breakpoint.number);
               DebuggerSend(buffer, true, true);
               goto doNext;
            }
         }

         breakpoints.push_back(breakpoint);
      } else {
         if (!strstr(position, "watchpoint"))
            goto doNext;
         const char* address = strstr(position, enabled ? " y  " : " n  ");
         if (!address)
            goto doNext;
         address += 2;
         while (*address == ' ')
            address++;
         if (isspace(*address))
            goto doNext;
         const char* end = strchr(address, '\n');
         if (!end)
            goto doNext;
         breakpoint.watchpoint = true;
         snprintf(breakpoint.file, sizeof(breakpoint.file), "%.*s", (int)(end - address), address);
         breakpoints.push_back(breakpoint);
      }

   doNext:;
      position = next;
   }
}

struct TabCompleter {
   bool _lastKeyWasTab;
   int  consecutiveTabCount;
   int  lastTabBytes;
};

void TabCompleterRun(TabCompleter* completer, UITextbox* textbox, bool lastKeyWasTab, bool addPrintPrefix) {
   char buffer[4096];
   StringFormat(buffer, sizeof(buffer), "complete %s%.*s", addPrintPrefix ? "p " : "",
                lastKeyWasTab ? completer->lastTabBytes : (int)textbox->bytes, textbox->string);
   for (int i = 0; buffer[i]; i++)
      if (buffer[i] == '\\')
         buffer[i] = ' ';
   EvaluateCommand(buffer);

   const char* start = evaluateResult;
   const char* end   = strchr(evaluateResult, '\n');

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
      start                          = evaluateResult;
      end                            = strchr(evaluateResult, '\n');
   }

   completer->_lastKeyWasTab = true;
   completer->consecutiveTabCount++;

   if (end) {
      if (addPrintPrefix)
         start += 2;
      UITextboxClear(textbox, false);
      UITextboxReplace(textbox, start, end - start, false);
      textbox->Refresh();
   }
}

// ------------------------------------------------------
// Commands:
// ------------------------------------------------------

bool CommandParseInternal(const char* command, bool synchronous) {
   if (0 == strcmp(command, "gf-step")) {
      if (!programRunning)
         DebuggerSend(showingDisassembly ? "stepi" : "s", true, synchronous);
      return true;
   } else if (0 == strcmp(command, "gf-next")) {
      if (!programRunning)
         DebuggerSend(showingDisassembly ? "nexti" : "n", true, synchronous);
      return true;
   } else if (0 == strcmp(command, "gf-step-out-of-block")) {
      int line = SourceFindEndOfBlock();

      if (line != -1) {
         char buffer[256];
         StringFormat(buffer, sizeof(buffer), "until %d", line);
         DebuggerSend(buffer, true, synchronous);
         return false;
      }
   } else if (0 == strcmp(command, "gf-step-into-outer")) {
      char *start, *end;
      bool  found = SourceFindOuterFunctionCall(&start, &end);

      if (found) {
         char buffer[256];
         StringFormat(buffer, sizeof(buffer), "advance %.*s", (int)(end - start), start);
         DebuggerSend(buffer, true, synchronous);
         return true;
      } else {
         return CommandParseInternal("gf-step", synchronous);
      }
   } else if (0 == strcmp(command, "gf-restart-gdb")) {
      firstUpdate = true;
      kill(gdbPID, SIGKILL);
      pthread_cancel(gdbThread); // TODO Is there a nicer way to do this?
      DebuggerStartThread();
   } else if (0 == strcmp(command, "gf-get-pwd")) {
      EvaluateCommand("info source");
      const char* needle = "Compilation directory is ";
      char*       pwd    = strstr(evaluateResult, needle);

      if (pwd) {
         pwd += strlen(needle);
         char* end = strchr(pwd, '\n');
         if (end)
            *end = 0;

         if (!chdir(pwd)) {
            if (!displayOutput)
               return false;
            char buffer[4096];
            StringFormat(buffer, sizeof(buffer), "New working directory: %s", pwd);
            UICodeInsertContent(displayOutput, buffer, -1, false);
            displayOutput->Refresh();
            return false;
         }
      }

      UIDialogShow(windowMain, 0, "Couldn't get the working directory.\n%f%B", "OK");
   } else if (strlen(command) > 13 && 0 == memcmp(command, "gf-switch-to ", 13)) {
      InterfaceWindowSwitchToAndFocus(command + 13);
   } else if (strlen(command) > 11 && 0 == memcmp(command, "gf-command ", 11)) {
      for (const auto& cmd : presetCommands) {
         if (strcmp(command + 11, cmd.key))
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
            bool hasOutput = CommandParseInternal(position, !async) && !async;
            if (displayOutput && hasOutput)
               UICodeInsertContent(displayOutput, evaluateResult, -1, false);
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
   } else if (0 == strcmp(command, "gf-inspect-line")) {
      CommandInspectLine();
   } else if (0 == strcmp(command, "target remote :1234") && confirmCommandConnect &&
              0 == strcmp("Cancel",
                          UIDialogShow(windowMain, 0, "Connect to remote target?\n%f%B%C", "Connect", "Cancel"))) {
   } else if (0 == strcmp(command, "kill") && confirmCommandKill &&
              0 == strcmp("Cancel", UIDialogShow(windowMain, 0, "Kill debugging target?\n%f%B%C", "Kill", "Cancel"))) {
   } else {
      DebuggerSend(command, true, synchronous);
      return true;
   }

   return false;
}

void CommandSendToGDB(const char* s) {
   CommandParseInternal(s, false);
}

#define BREAKPOINT_COMMAND(function, action)                        \
   void function(int index) {                                       \
      Breakpoint* breakpoint = &breakpoints[index];                 \
      char        buffer[1024];                                     \
      StringFormat(buffer, 1024, action " %d", breakpoint->number); \
      DebuggerSend(buffer, true, false);                            \
   }

BREAKPOINT_COMMAND(CommandDeleteBreakpoint, "delete");
BREAKPOINT_COMMAND(CommandDisableBreakpoint, "disable");
BREAKPOINT_COMMAND(CommandEnableBreakpoint, "enable");

void CommandPause() {
   kill(gdbPID, SIGINT);
}

void CommandSyncWithGvim() {
   char buffer[1024];
   StringFormat(buffer, sizeof(buffer), "vim --servername %s --remote-expr \"execute(\\\"ls\\\")\" | grep %%",
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
   int  lineNumber = atoi(line);
   char buffer2[PATH_MAX];

   if (name[0] != '/' && name[0] != '~') {
      char buffer[1024];
      StringFormat(buffer, sizeof(buffer), "vim --servername %s --remote-expr \"execute(\\\"pwd\\\")\" | grep '/'",
                   vimServerName);
      FILE* file = popen(buffer, "r");
      if (!file)
         return;
      buffer[fread(buffer, 1, 1023, file)] = 0;
      pclose(file);
      if (!strchr(buffer, '\n'))
         return;
      *strchr(buffer, '\n') = 0;
      StringFormat(buffer2, sizeof(buffer2), "%s/%s", buffer, name);
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
         char buffer[1024];
         StringFormat(buffer, 1024, "clear %s:%d", currentFile, line);
         DebuggerSend(buffer, true, false);
         return;
      }
   }

   char buffer[1024];
   StringFormat(buffer, 1024, "b %s:%d", currentFile, line);
   DebuggerSend(buffer, true, false);
}

void CommandToggleBreakpoint() {
   CommandToggleBreakpoint(currentLine);
}

void CommandCustom(const char* command) {

   if (0 == memcmp(command, "shell ", 6)) {
      // TODO Move this into CommandParseInternal?

      char buffer[4096];
      StringFormat(buffer, 4096, "Running shell command \"%s\"...\n", command);
      if (displayOutput)
         UICodeInsertContent(displayOutput, buffer, -1, false);
      StringFormat(buffer, 4096, "%s > .output.gf 2>&1", command);
      int    start  = time(nullptr);
      int    result = system(buffer);
      size_t bytes  = 0;
      char*  output = LoadFile(".output.gf", &bytes);
      unlink(".output.gf");
      char*     copy = (char*)malloc(bytes + 1);
      uintptr_t j    = 0;

      for (uintptr_t i = 0; i <= bytes;) {
         if ((uint8_t)output[i] == 0xE2 && (uint8_t)output[i + 1] == 0x80 &&
             ((uint8_t)output[i + 2] == 0x98 || (uint8_t)output[i + 2] == 0x99)) {
            copy[j++] = '\'';
            i += 3;
         } else {
            copy[j++] = output[i++];
         }
      }

      if (displayOutput)
         UICodeInsertContent(displayOutput, copy, j, false);
      free(output);
      free(copy);
      StringFormat(buffer, 4096, "(exit code: %d; time: %ds)\n", result, (int)(time(nullptr) - start));
      if (displayOutput)
         UICodeInsertContent(displayOutput, buffer, -1, false);
      if (displayOutput)
         displayOutput->Refresh();
   } else {
      CommandParseInternal(command, false);
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
   char*       config           = LoadFile(globalConfigPath, nullptr);
   size_t      length           = config ? strlen(config) : 0;
   size_t      insert           = 0;
   const char* sectionString    = "\n[trusted_folders]\n";
   bool        addSectionString = true;

   if (config) {
      char* section = strstr(config, sectionString);

      if (section) {
         insert           = section - config + strlen(sectionString);
         addSectionString = false;
      } else {
         insert = length;
      }
   }

   FILE* f = fopen(globalConfigPath, "wb");

   if (!f) {
      fprintf(stderr, "Error: Could not modify the global config file!\n");
   } else {
      if (insert)
         fwrite(config, 1, insert, f);
      if (addSectionString)
         fwrite(sectionString, 1, strlen(sectionString), f);
      fwrite(localConfigDirectory, 1, strlen(localConfigDirectory), f);
      char newline = '\n';
      fwrite(&newline, 1, 1, f);
      if (length - insert)
         fwrite(config + insert, 1, length - insert, f);
      fclose(f);
   }
}

void SettingsLoad(bool earlyPass) {
   bool        currentFolderIsTrusted = false;
   static bool cwdConfigNotTrusted    = false;

   for (int i = 0; i < 2; i++) {
      INIState state;
      state.buffer = LoadFile(i ? localConfigPath : globalConfigPath, &state.bytes);

      if (earlyPass && i && !currentFolderIsTrusted && state.buffer) {
         fprintf(stderr, "Would you like to load the config file .project.gf from your current directory?\n");
         fprintf(stderr, "You have not loaded this config file before.\n");
         fprintf(stderr, "(Y) - Yes, and add it to the list of trusted files\n");
         fprintf(stderr, "(N) - No\n");
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
               if (codeStart[0] == 'f' && isdigit(codeStart[1]) && atoi(codeStart + 1) == i) {
                  shortcut.code = UI_KEYCODE_FKEY(i);
               }
            }

            if ((int)shortcut.code == 0) {
               fprintf(stderr, "Warning: Could not register shortcut for '%s'.\n", state.key);
            } else {
               UIWindowRegisterShortcut(windowMain, std::move(shortcut));
            }
         } else if (0 == strcmp(state.section, "ui") && earlyPass) {
            if (0 == strcmp(state.key, "font_path")) {
               fontPath = state.value;
            } else if (0 == strcmp(state.key, "font_size")) {
               fontSizeInterface = fontSizeCode = atoi(state.value);
            } else if (0 == strcmp(state.key, "font_size_code")) {
               fontSizeCode = atoi(state.value);
            } else if (0 == strcmp(state.key, "font_size_interface")) {
               fontSizeInterface = atoi(state.value);
            } else if (0 == strcmp(state.key, "scale")) {
               uiScale = atof(state.value);
            } else if (0 == strcmp(state.key, "layout")) {
               layoutString = state.value;
            } else if (0 == strcmp(state.key, "maximize")) {
               maximize = atoi(state.value);
            } else if (0 == strcmp(state.key, "restore_watch_window")) {
               restoreWatchWindow = atoi(state.value);
            } else if (0 == strcmp(state.key, "selectable_source")) {
               selectableSource = atoi(state.value);
            } else if (0 == strcmp(state.key, "window_width")) {
               window_width = atoi(state.value);
            } else if (0 == strcmp(state.key, "window_height")) {
               window_height = atoi(state.value);
            }
         } else if (0 == strcmp(state.section, "gdb") && !earlyPass) {
            if (0 == strcmp(state.key, "argument")) {
               gdbArgc++;
               gdbArgv              = (char**)realloc(gdbArgv, sizeof(char*) * (gdbArgc + 1));
               gdbArgv[gdbArgc - 1] = state.value;
               gdbArgv[gdbArgc]     = nullptr;
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

                  StringFormat(buffer, sizeof(buffer), "%.*s", argumentEnd - argumentStart,
                               &state.value[argumentStart]);

                  gdbArgc++;
                  gdbArgv              = (char**)realloc(gdbArgv, sizeof(char*) * (gdbArgc + 1));
                  gdbArgv[gdbArgc - 1] = strdup(buffer);
                  gdbArgv[gdbArgc]     = nullptr;
               }
            } else if (0 == strcmp(state.key, "path")) {
               gdbPath = state.value;
               gdbArgv[0] = state.value;
            } else if (0 == strcmp(state.key, "log_all_output") && atoi(state.value)) {
               for (const auto& iw : interfaceWindows) {
                  const InterfaceWindow* window = &iw;

                  if (0 == strcmp(window->name, "Log")) {
                     sendAllGDBOutputToLogWindowContext = window->element;
                  }
               }

               if (!sendAllGDBOutputToLogWindowContext) {
                  fprintf(stderr, "Warning: gdb.log_all_output was enabled, "
                                  "but your layout does not have a 'Log' window.\n");
               }
            } else if (0 == strcmp(state.key, "confirm_command_kill")) {
               confirmCommandKill = atoi(state.value);
            } else if (0 == strcmp(state.key, "confirm_command_connect")) {
               confirmCommandConnect = atoi(state.value);
            } else if (0 == strcmp(state.key, "backtrace_count_limit")) {
               backtraceCountLimit = atoi(state.value);
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
               executableAskDirectory = atoi(state.value);
            }
         } else if (earlyPass && *state.section && *state.key && *state.value) {
            for (const auto& iw : interfaceWindows) {
               if (0 == strcmp(state.section, iw.name) && iw.config) {
                  iw.config(state.key, state.value);
                  break;
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

vector<char*> inspectResults;
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
      StringFormat(buffer, sizeof(buffer), "%s/%s", getenv("HOME"), 1 + file);
      file = buffer;
   } else if (file && file[0] != '/' && useGDBToGetFullPath) {
      EvaluateCommand("info source");
      const char* f = strstr(evaluateResult, "Located in ");

      if (f) {
         f += 11;
         const char* end = strchr(f, '\n');

         if (end) {
            StringFormat(buffer, sizeof(buffer), "%.*s", (int)(end - f), f);
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
      StringFormat(currentFile, 4096, "%s", file);
      realpath(currentFile, currentFileFull);

      XStoreName(ui->display, windowMain->xwindow, currentFileFull);

      size_t bytes;
      char*  buffer2 = LoadFile(file, &bytes);

      if (!buffer2) {
         char buffer3[4096];
         StringFormat(buffer3, 4096, "The file '%s' (from '%s') could not be loaded.", file, originalFile);
         UICodeInsertContent(displayCode, buffer3, -1, true);
      } else {
         UICodeInsertContent(displayCode, buffer2, bytes, true);
         free(buffer2);
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
   if (stack.size() > stackSelected) {
      char location[sizeof(previousLocation)];
      strcpy(previousLocation, stack[stackSelected].location);
      strcpy(location, stack[stackSelected].location);
      char* line = strchr(location, ':');
      if (line)
         *line = 0;
      DisplaySetPosition(location, line ? atoi(line + 1) : -1, true);
   }
}

void DisassemblyLoad() {
   EvaluateCommand(disassemblyCommand);

   if (!strstr(evaluateResult, "Dump of assembler code for function")) {
      char buffer[32];
      StringFormat(buffer, sizeof(buffer), "disas $pc,+1000");
      EvaluateCommand(buffer);
   }

   char* end = strstr(evaluateResult, "End of assembler dump.");

   if (!end) {
      printf("Disassembly failed. GDB output:\n%s\n", evaluateResult);
      return;
   }

   char* start = strstr(evaluateResult, ":\n");

   if (!start) {
      printf("Disassembly failed. GDB output:\n%s\n", evaluateResult);
      return;
   }

   start += 2;

   if (start >= end) {
      printf("Disassembly failed. GDB output:\n%s\n", evaluateResult);
      return;
   }

   char* pointer = strstr(start, "=> ");

   if (pointer) {
      pointer[0] = ' ';
      pointer[1] = ' ';
   }

   UICodeInsertContent(displayCode, start, end - start, true);
}

void DisassemblyUpdateLine() {
   EvaluateCommand("p $pc");
   char* address = strstr(evaluateResult, "0x");

   if (address) {
      char*    addressEnd;
      uint64_t a = strtoul(address, &addressEnd, 0);

      for (int i = 0; i < 2; i++) {
         // Look for the line in the disassembly.

         bool found = false;

         for (int i = 0; i < displayCode->lineCount; i++) {
            uint64_t b = strtoul(displayCode->content + displayCode->lines[i].offset + 3, &addressEnd, 0);

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
      UICodeInsertContent(displayCode, "Disassembly could not be loaded.\nPress Ctrl+D to return to source view.", -1,
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

   for (size_t index = 0; index < inspectResults.size() / 2; index++) {
      int w =
         (strlen(inspectResults[index * 2]) + strlen(inspectResults[index * 2 + 1]) + 8) * ui->activeFont->glyphWidth;
      if (w > width)
         width = w;
   }

   int xOffset = 0;

   {
      UICodeLine* line = &displayCode->lines[currentLine - 1];

      for (int i = 0; i < line->bytes; i++) {
         if (displayCode->content[line->offset + i] == '\t') {
            xOffset += 4 * ui->activeFont->glyphWidth;
         } else if (displayCode->content[line->offset + i] == ' ') {
            xOffset += 1 * ui->activeFont->glyphWidth;
         } else {
            break;
         }
      }
   }

   char        buffer[256];
   int         lineHeight = UIMeasureStringHeight();
   UIRectangle bounds     =
      displayCurrentLineBounds + UIRectangle(xOffset, 0, lineHeight, 8 + lineHeight * (inspectResults.size() / 2 + 1));
   bounds.r = bounds.l + width;
   UIDrawBlock(painter, bounds + UIRectangle(3), ui->theme.border);
   UIDrawRectangle(painter, bounds, ui->theme.codeBackground, ui->theme.border, UIRectangle(2));
   UIRectangle line = bounds + UIRectangle(4, -4, 4, 0);
   line.b           = line.t + lineHeight;

   for (size_t index = 0; index < inspectResults.size() / 2; index++) {
      if (noInspectResults) {
         StringFormat(buffer, sizeof(buffer), "%s", inspectResults[index * 2]);
      } else if (index < 9) {
         StringFormat(buffer, sizeof(buffer), "[%d] %s %s", index + 1, inspectResults[index * 2],
                      inspectResults[index * 2 + 1]);
      } else {
         StringFormat(buffer, sizeof(buffer), "    %s %s", inspectResults[index * 2], inspectResults[index * 2 + 1]);
      }

      UIDrawString(painter, line, buffer, -1, noInspectResults ? ui->theme.codeOperator : ui->theme.codeString,
                   UIAlign::left, NULL);
      line = line + UIRectangle(0, lineHeight);
   }

   UIDrawString(painter, line, instructions, -1, ui->theme.codeNumber, UIAlign::right, NULL);
}

#define DISPLAY_CODE_COMMAND_FOR_ALL_BREAKPOINTS_ON_LINE(function, command)                          \
   void function(int line) {                                                                         \
      for (size_t i = 0; i < breakpoints.size(); i++) {                                              \
         if (breakpoints[i].line == line && 0 == strcmp(breakpoints[i].fileFull, currentFileFull)) { \
            command(i);                                                                              \
         }                                                                                           \
      }                                                                                              \
   }

DISPLAY_CODE_COMMAND_FOR_ALL_BREAKPOINTS_ON_LINE(CommandDeleteAllBreakpointsOnLine, CommandDeleteBreakpoint);
DISPLAY_CODE_COMMAND_FOR_ALL_BREAKPOINTS_ON_LINE(CommandDisableAllBreakpointsOnLine, CommandDisableBreakpoint);
DISPLAY_CODE_COMMAND_FOR_ALL_BREAKPOINTS_ON_LINE(CommandEnableAllBreakpointsOnLine, CommandEnableBreakpoint);

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
            char buffer[1024];
            StringFormat(buffer, 1024, "until %d", line);
            DebuggerSend(buffer, true, false);
         } else if (element->window->alt || element->window->shift) {
            char buffer[1024];
            StringFormat(buffer, 1024, "tbreak %d", line);
            EvaluateCommand(buffer);
            StringFormat(buffer, 1024, "jump %d", line);
            DebuggerSend(buffer, true, false);
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
            UIMenuAddItem(menu, 0, "Delete", -1, [=]() { CommandDeleteAllBreakpointsOnLine(-result); });
            if (atLeastOneBreakpointEnabled)
               UIMenuAddItem(menu, 0, "Disable", -1, [=]() { CommandDisableAllBreakpointsOnLine(-result); });
            else
               UIMenuAddItem(menu, 0, "Enable",  -1, [=]() { CommandEnableAllBreakpointsOnLine(-result); });
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
         UIDrawString(m->painter, rectangle, autoPrintResult, -1, ui->theme.codeComment, UIAlign::left, NULL);
      }

      if (UICodeHitTest(code, element->window->cursor.x, element->window->cursor.y) == m->index &&
          element->window->hovered == element &&
          (element->window->ctrl || element->window->alt || element->window->shift) &&
          !element->window->textboxModifiedFlag) {
         UIDrawBorder(m->painter, m->bounds, element->window->ctrl ? ui->theme.selected : ui->theme.codeOperator,
                      UIRectangle(2));
         UIDrawString(m->painter, m->bounds, element->window->ctrl ? "=> run until " : "=> skip to ", -1, ui->theme.text,
                      UIAlign::right, NULL);
      } else if (m->index == currentEndOfBlock) {
         UIDrawString(m->painter, m->bounds, "[Shift+F10]", -1, ui->theme.codeComment, UIAlign::right, NULL);
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
   displayCode                = UICodeCreate(parent, selectableSource ? UICode::SELECTABLE : 0);
   displayCode->font          = fontCode;
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

   if (changedSourceLine && currentLine < displayCode->lineCount && currentLine > 0) {
      // If there is an auto-print expression from the previous line, evaluate it.

      if (autoPrintExpression[0]) {
         char buffer[1024];
         StringFormat(buffer, sizeof(buffer), "p %s", autoPrintExpression);
         EvaluateCommand(buffer);
         const char* result = strchr(evaluateResult, '=');

         if (result) {
            autoPrintResultLine = autoPrintExpressionLine;
            StringFormat(autoPrintResult, sizeof(autoPrintResult), "%s", result);
            char* end = strchr(autoPrintResult, '\n');
            if (end)
               *end = 0;
         } else {
            autoPrintResult[0] = 0;
         }

         autoPrintExpression[0] = 0;
      }

      // Parse the new source line.

      UICodeLine* line     = displayCode->lines + currentLine - 1;
      char*       text     = displayCode->content + line->offset;
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
         StringFormat(autoPrintExpression, sizeof(autoPrintExpression), "%.*s", (int)(expressionEnd - expressionStart),
                      text + expressionStart);
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
               text[i]            = 0;
               const char* result = EvaluateExpression(&text[expressionStart]);
               text[i]            = ')';

               if (!result) {
               } else if (0 == strcmp(result, "= true")) {
                  ifConditionEvaluation = 2;
                  ifConditionFrom = expressionStart, ifConditionTo = i;
                  ifConditionLine = currentLine;
               } else if (0 == strcmp(result, "= false")) {
                  ifConditionEvaluation = 1;
                  ifConditionFrom = expressionStart, ifConditionTo = i;
                  ifConditionLine = currentLine;
               }

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
   for (auto& ir : inspectResults)
      free(ir);
   inspectResults.clear();

   UICodeLine* line   = &displayCode->lines[currentLine - 1];
   const char* string = displayCode->content + line->offset;

   for (int i = 0; i < line->bytes; i++) {
      if ((i != line->bytes - 1 && InspectIsTokenCharacter(string[i]) && !InspectIsTokenCharacter(string[i + 1])) ||
          string[i] == ']') {
         int b = 0, j = i;

         for (; j >= 0; j--) {
            if (j && string[j] == '>' && string[j - 1] == '-') {
               j--;
            } else if (string[j] == ']') {
               b++;
            } else if (string[j] == '[' && b) {
               b--;
            } else if (InspectIsTokenCharacter(string[j]) || b || string[j] == '.') {
            } else {
               j++;
               break;
            }
         }

         char buffer[256];
         if (i - j + 1 > 255 || j < 1)
            continue;
         StringFormat(buffer, sizeof(buffer), "%.*s", i - j + 1, string + j);

         if (0 == strcmp(buffer, "true") || 0 == strcmp(buffer, "false") || 0 == strcmp(buffer, "if") ||
             0 == strcmp(buffer, "for") || 0 == strcmp(buffer, "else") || 0 == strcmp(buffer, "while") ||
             0 == strcmp(buffer, "int") || 0 == strcmp(buffer, "char") || 0 == strcmp(buffer, "switch") ||
             0 == strcmp(buffer, "float")) {
            continue;
         }

         bool match = false;

         for (size_t k = 0; k < inspectResults.size(); k += 2) {
            if (0 == strcmp(inspectResults[k], buffer)) {
               match = true;
            }
         }

         if (match)
            continue;

         const char* result = EvaluateExpression(buffer);
         if (!result)
            continue;
         if (0 == memcmp(result, "= {", 3) && !strchr(result + 3, '='))
            continue;
         inspectResults.push_back(strdup(buffer));
         inspectResults.push_back(strdup(result));
      }
   }

   if (!inspectResults.size()) {
      inspectResults.push_back(strdup("No expressions to display."));
      inspectResults.push_back(strdup(" "));
      noInspectResults = true;
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

      if ((m->textBytes == 1 && m->text[0] == '`') || m->code == UIKeycode::ESCAPE) {
         InspectLineModeExit(element);
      } else if (m->code >= UI_KEYCODE_DIGIT('1') && m->code <= UI_KEYCODE_DIGIT('9')) {
         int index = ((int)m->code - (int)UI_KEYCODE_DIGIT('1')) * 2;

         if (index < (int)inspectResults.size()) {
            InspectLineModeExit(element);
            WatchAddExpression2(inspectResults[index]);
         }
      } else if ((m->code == UIKeycode::UP && currentLine != 1) ||
                 (m->code == UIKeycode::DOWN && currentLine != displayCode->lineCount)) {
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
   if (!currentLine || currentLine - 1 >= displayCode->lineCount)
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
   char            pointer[256];
   char            width[256];
   char            height[256];
   char            stride[256];
   int             parsedWidth, parsedHeight;
   UIButton*       autoToggle;
   UIImageDisplay* display;
   UIPanel*        labelPanel;
   UILabel*        label;
};

int BitmapViewerWindowMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::DESTROY) {
      DataViewerRemoveFromAutoUpdateList(element);
      free(element->cp);
   } else if (message == UIMessage::GET_WIDTH) {
      int fit = ((BitmapViewer*)element->cp)->parsedWidth + 40;
      return fit > 300 ? fit : 300;
   } else if (message == UIMessage::GET_HEIGHT) {
      int fit = ((BitmapViewer*)element->cp)->parsedHeight + 40;
      return fit > 100 ? fit : 100;
   }

   return 0;
}

void BitmapViewerUpdate(const char* pointerString, const char* widthString, const char* heightString,
                        const char* strideString, UIElement* owner = nullptr);

int BitmapViewerRefreshMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::CLICKED) {
      BitmapViewer* bitmap = (BitmapViewer*)element->parent->cp;
      BitmapViewerUpdate(bitmap->pointer, bitmap->width, bitmap->height, bitmap->stride, element->parent);
   }

   return 0;
}

const char* BitmapViewerGetBits(const char* pointerString, const char* widthString, const char* heightString,
                                const char* strideString, uint32_t** _bits, int* _width, int* _height, int* _stride) {
   const char* widthResult = EvaluateExpression(widthString);
   if (!widthResult) {
      return "Could not evaluate width.";
   }
   int         width        = atoi(widthResult + 1);
   const char* heightResult = EvaluateExpression(heightString);
   if (!heightResult) {
      return "Could not evaluate height.";
   }
   int         height        = atoi(heightResult + 1);
   int         stride        = width * 4;
   const char* pointerResult = EvaluateExpression(pointerString, "/x");
   if (!pointerResult) {
      return "Could not evaluate pointer.";
   }
   char _pointerResult[1024];
   StringFormat(_pointerResult, sizeof(_pointerResult), "%s", pointerResult);
   pointerResult = strstr(_pointerResult, " 0x");
   if (!pointerResult) {
      return "Pointer to image bits does not look like an address!";
   }
   pointerResult++;

   if (strideString && *strideString) {
      const char* strideResult = EvaluateExpression(strideString);
      if (!strideResult) {
         return "Could not evaluate stride.";
      }
      stride = atoi(strideResult + 1);
   }

   uint32_t* bits = (uint32_t*)malloc(stride * height * 4); // TODO Is this multiply by 4 necessary?! And the one below.

   char bitmapPath[PATH_MAX];
   realpath(".bitmap.gf", bitmapPath);

   char buffer[PATH_MAX * 2];
   StringFormat(buffer, sizeof(buffer), "dump binary memory %s (%s) (%s+%d)", bitmapPath, pointerResult, pointerResult,
                stride * height);
   EvaluateCommand(buffer);

   FILE* f = fopen(bitmapPath, "rb");

   if (f) {
      fread(bits, 1, stride * height * 4, f); // TODO Is this multiply by 4 necessary?!
      fclose(f);
      unlink(bitmapPath);
   }

   if (!f || strstr(evaluateResult, "access")) {
      return "Could not read the image bits!";
   }

   *_bits = bits, *_width = width, *_height = height, *_stride = stride;
   return nullptr;
}

int BitmapViewerDisplayMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::RIGHT_UP) {
      UIMenu* menu = UIMenuCreate(element->window, UIMenu::NO_SCROLL);

      UIMenuAddItem(
         menu, 0, "Save to file...", -1,
         [element]() {
            static char* path = NULL;
            const char*  result =
               UIDialogShow(windowMain, 0, "Save to file       \nPath:\n%t\n%f%B%C", &path, "Save", "Cancel");
            if (strcmp(result, "Save"))
               return;

            UIImageDisplay* display = (UIImageDisplay*)element;
            FILE*           f       = fopen(path, "wb");
            fprintf(f, "P6\n%d %d\n255\n", display->width, display->height);

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

void BitmapViewerUpdate(const char* pointerString, const char* widthString, const char* heightString,
                        const char* strideString, UIElement* owner) {
   uint32_t*   bits  = nullptr;
   int         width = 0, height = 0, stride = 0;
   const char* error =
      BitmapViewerGetBits(pointerString, widthString, heightString, strideString, &bits, &width, &height, &stride);

   if (!owner) {
      BitmapViewer* bitmap = (BitmapViewer*)calloc(1, sizeof(BitmapViewer));
      if (pointerString)
         StringFormat(bitmap->pointer, sizeof(bitmap->pointer), "%s", pointerString);
      if (widthString)
         StringFormat(bitmap->width, sizeof(bitmap->width), "%s", widthString);
      if (heightString)
         StringFormat(bitmap->height, sizeof(bitmap->height), "%s", heightString);
      if (strideString)
         StringFormat(bitmap->stride, sizeof(bitmap->stride), "%s", strideString);

      UIMDIChild* window     = UIMDIChildCreate(dataWindow, UIMDIChild::CLOSE_BUTTON, UIRectangle(0), "Bitmap", -1);
      window->messageUser    = BitmapViewerWindowMessage;
      window->cp             = bitmap;
      bitmap->autoToggle     = UIButtonCreate(window, UIButton::SMALL | UIElement::NON_CLIENT, "Auto", -1);
      bitmap->autoToggle->cp = (void*)BitmapViewerAutoUpdateCallback;
      bitmap->autoToggle->messageUser = DataViewerAutoUpdateButtonMessage;
      UIButtonCreate(window, UIButton::SMALL | UIElement::NON_CLIENT, "Refresh", -1)->messageUser =
         BitmapViewerRefreshMessage;
      owner = window;

      UIPanel* panel = UIPanelCreate(owner, UIPanel::EXPAND);
      bitmap->display =
         UIImageDisplayCreate(panel, UIImageDisplay::INTERACTIVE | UIElement::V_FILL, bits, width, height, stride);
      bitmap->labelPanel             = UIPanelCreate(panel, UIPanel::COLOR_1 | UIElement::V_FILL);
      bitmap->label                  = UILabelCreate(bitmap->labelPanel, UIElement::H_FILL, nullptr, 0);
      bitmap->display->messageUser = BitmapViewerDisplayMessage;
   }

   BitmapViewer* bitmap = (BitmapViewer*)owner->cp;
   bitmap->parsedWidth = width, bitmap->parsedHeight = height;
   UIImageDisplaySetContent(bitmap->display, bits, width, height, stride);
   if (error)
      UILabelSetContent(bitmap->label, error, -1);
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
      BitmapViewerUpdate(pointer, width, height, (stride && stride[0]) ? stride : nullptr);
   }
}

// ---------------------------------------------------/
// Console:
// ---------------------------------------------------/

vector<char*> commandHistory;
size_t        commandHistoryIndex;

void CommandPreviousCommand() {
   if (commandHistoryIndex < commandHistory.size()) {
      UITextboxClear(textboxInput, false);
      UITextboxReplace(textboxInput, commandHistory[commandHistoryIndex], -1, false);
      if (commandHistoryIndex < commandHistory.size() - 1)
         commandHistoryIndex++;
      textboxInput->Refresh();
   }
}

void CommandNextCommand() {
   UITextboxClear(textboxInput, false);

   if (commandHistoryIndex > 0) {
      commandHistoryIndex--;
      UITextboxReplace(textboxInput, commandHistory[commandHistoryIndex], -1, false);
   }

   textboxInput->Refresh();
}

void CommandClearOutput() {
   UI_FREE(displayOutput->content);
   UI_FREE(displayOutput->lines);
   displayOutput->content      = NULL;
   displayOutput->lines        = NULL;
   displayOutput->contentBytes = 0;
   displayOutput->lineCount    = 0;
   displayOutput->Refresh();
}

int TextboxInputMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UITextbox* textbox = (UITextbox*)element;

   if (message == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      static TabCompleter tabCompleter  = {};
      bool                lastKeyWasTab = tabCompleter._lastKeyWasTab;
      tabCompleter._lastKeyWasTab       = false;

      if (m->textBytes && !element->window->ctrl && !element->window->alt && m->text[0] == '`' && !textbox->bytes) {
         textbox->rejectNextKey = true;
      } else if (m->code == UIKeycode::ENTER && !element->window->shift) {
         if (!textbox->bytes) {
            if (commandHistory.size()) {
               CommandSendToGDB(commandHistory[0]);
            }

            return 1;
         }

         char buffer[1024];
         StringFormat(buffer, 1024, "%.*s", (int)textbox->bytes, textbox->string);
         if (commandLog)
            fprintf(commandLog, "%s\n", buffer);
         CommandSendToGDB(buffer);

         char* string = (char*)malloc(textbox->bytes + 1);
         memcpy(string, textbox->string, textbox->bytes);
         string[textbox->bytes] = 0;
         commandHistory.insert(commandHistory.cbegin(), string);
         commandHistoryIndex = 0;

         if (commandHistory.size() > 100) {
            free(commandHistory.back());
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
            if (currentLine < displayCode->lineCount) {
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
   UIPanel* panel2             = UIPanelCreate(parent, UIPanel::EXPAND);
   displayOutput               = UICodeCreate(panel2, UICode::NO_MARGIN | UIElement::V_FILL | UICode::SELECTABLE);
   UIPanel* panel3             = UIPanelCreate(panel2, UIPanel::HORIZONTAL | UIPanel::EXPAND | UIPanel::COLOR_1);
   panel3->border              = UIRectangle(5);
   panel3->gap                 = 5;
   trafficLight                = UISpacerCreate(panel3, 0, 30, 30);
   trafficLight->messageUser   = TrafficLightMessage;
   UIButton* buttonMenu        = UIButtonCreate(panel3, 0, "Menu", -1);
   buttonMenu->invoke          = [buttonMenu]() { InterfaceShowMenu(buttonMenu); };
   textboxInput                = UITextboxCreate(panel3, UIElement::H_FILL);
   textboxInput->messageUser   = TextboxInputMessage;
   textboxInput->Focus();
   return panel2;
}

// ---------------------------------------------------/
// Watch window:
// ---------------------------------------------------/

struct Watch {
   bool           open, hasFields, loadedFields, isArray, isDynamicArray;
   uint8_t        depth;
   char           format;
   uintptr_t      arrayIndex;
   char *         key, *value, *type;
   vector<Watch*> fields;
   Watch*         parent;
   uint64_t       updateIndex;
};

enum WatchWindowMode {
   WATCH_NORMAL,
   WATCH_LOCALS,
};

struct WatchWindow {
   vector<Watch*>  rows;
   vector<Watch*>  baseExpressions;
   vector<Watch*>  dynamicArrays;
   UIElement*      element;
   UITextbox*      textbox;
   char*           lastLocalList;
   size_t          selectedRow;
   int             extraRows;
   WatchWindowMode mode;
   uint64_t        updateIndex;
   bool            waitingForFormatCharacter;
};

struct WatchLogEvaluated {
   char result[64];
};

struct WatchLogEntry {
   char                      value[64];
   char                      where[128];
   vector<WatchLogEvaluated> evaluated;
   vector<StackEntry>        trace;
};

struct WatchLogger {
   int                   id, selectedEntry;
   char                  columns[256];
   char*                 expressionsToEvaluate;
   vector<WatchLogEntry> entries;
   UITable *             table, *trace;
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
   w->element->Focus();
}

void WatchFree(WatchWindow* w, Watch* watch, bool fieldsOnly = false) {
   for (const auto& field : watch->fields) {
      WatchFree(w, field);
      if (!watch->isArray)
         free(field);
   }

   if (watch->isDynamicArray) {
      if (auto it = rng::find(w->dynamicArrays, watch); it != rng::end(w->dynamicArrays))
         w->dynamicArrays.erase(it);
   }

   if (watch->isArray && watch->fields.size()) {
      free(watch->fields[0]);
   }

   watch->loadedFields = false;
   watch->fields.clear();

   if (!fieldsOnly) {
      free(watch->key);
      free(watch->value);
      free(watch->type);
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

   Watch* watch = w->rows[w->selectedRow];

   if (!fieldsOnly) {
      if (auto it = rng::find(w->baseExpressions, watch); it != rng::end(w->baseExpressions))
         w->baseExpressions.erase(it);
   }

   if (fieldsOnly)
      w->selectedRow++;
   w->rows.erase(w->rows.cbegin() + w->selectedRow, w->rows.cbegin() + end);
   WatchFree(w, watch, fieldsOnly);
   if (!fieldsOnly)
      free(watch);
}

void WatchEvaluate(const char* function, Watch* watch) {
   char      buffer[4096];
   uintptr_t position = 0;

   position += StringFormat(buffer + position, sizeof(buffer) - position, "py %s([", function);

   Watch* stack[32];
   int    stackCount = 0;
   stack[0]          = watch;

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
         position += StringFormat(buffer + position, sizeof(buffer) - position, ",");
      } else {
         first = false;
      }

      if (stack[stackCount]->key) {
         position += StringFormat(buffer + position, sizeof(buffer) - position, "'%s'", stack[stackCount]->key);
      } else if (stack[stackCount]->parent && stack[stackCount]->parent->isDynamicArray) {
         position +=
            StringFormat(buffer + position, sizeof(buffer) - position, "'[%lu]'", stack[stackCount]->arrayIndex);
      } else {
         position += StringFormat(buffer + position, sizeof(buffer) - position, "%lu", stack[stackCount]->arrayIndex);
      }
   }

   position += StringFormat(buffer + position, sizeof(buffer) - position, "]");

   if (0 == strcmp(function, "gf_valueof")) {
      position += StringFormat(buffer + position, sizeof(buffer) - position, ",'%c'", watch->format ?: ' ');
   }

   position += StringFormat(buffer + position, sizeof(buffer) - position, ")");

   EvaluateCommand(buffer);
}

bool WatchHasFields(Watch* watch) {
   WatchEvaluate("gf_fields", watch);

   if (strstr(evaluateResult, "(array)") || strstr(evaluateResult, "(d_arr)")) {
      return true;
   } else {
      char* position = evaluateResult;
      char* end      = strchr(position, '\n');
      if (!end)
         return false;
      *end = 0;
      if (strstr(position, "(gdb)"))
         return false;
      return true;
   }
}

void WatchAddFields(WatchWindow* w, Watch* watch) {
   if (watch->loadedFields) {
      return;
   }

   watch->loadedFields = true;

   WatchEvaluate("gf_fields", watch);

   if (strstr(evaluateResult, "(array)") || strstr(evaluateResult, "(d_arr)")) {
      int count = atoi(evaluateResult + 7);

#define WATCH_ARRAY_MAX_FIELDS (10000000)
      if (count > WATCH_ARRAY_MAX_FIELDS)
         count = WATCH_ARRAY_MAX_FIELDS;
      if (count < 0)
         count = 0;

      Watch* fields     = (Watch*)calloc(count, sizeof(Watch));
      watch->isArray    = true;
      bool hasSubFields = false;

      if (strstr(evaluateResult, "(d_arr)")) {
         watch->isDynamicArray = true;
         w->dynamicArrays.push_back(watch);
      }

      for (int i = 0; i < count; i++) {
         fields[i].format     = watch->format;
         fields[i].parent     = watch;
         fields[i].arrayIndex = i;
         watch->fields.push_back(&fields[i]);
         if (!i)
            hasSubFields = WatchHasFields(&fields[i]);
         fields[i].hasFields = hasSubFields;
         fields[i].depth     = watch->depth + 1;
      }
   } else {
      char* start    = strdup(evaluateResult);
      char* position = start;

      while (true) {
         char* end = strchr(position, '\n');
         if (!end)
            break;
         *end = 0;
         if (strstr(position, "(gdb)"))
            break;
         Watch* field  = (Watch*)calloc(1, sizeof(Watch));
         field->depth  = watch->depth + 1;
         field->parent = watch;
         field->key    = (char*)malloc(end - position + 1);
         strcpy(field->key, position);
         watch->fields.push_back(field);
         field->hasFields = WatchHasFields(field);
         position         = end + 1;
      }

      free(start);
   }
}

void WatchEnsureRowVisible(WatchWindow* w, size_t index) {
   if (w->selectedRow > w->rows.size())
      w->selectedRow = w->rows.size();
   UIScrollBar* scroll    = ((UIPanel*)w->element->parent)->scrollBar;
   int          rowHeight = (int)(ui_size::TEXTBOX_HEIGHT * w->element->window->scale);
   int  start = index * rowHeight, end = (index + 1) * rowHeight, height = w->element->parent->bounds.height();
   bool unchanged = false;
   if (end >= scroll->position + height)
      scroll->position = end - height;
   else if (start <= scroll->position)
      scroll->position = start;
   else
      unchanged = true;
   if (!unchanged)
      w->element->parent->Refresh();
}

void WatchInsertFieldRows2(WatchWindow* w, Watch* watch, vector<Watch*>* array) {
   for (const auto& field : watch->fields) {
      array->push_back(field);
      if (field->open)
         WatchInsertFieldRows2(w, field, array);
   }
}

void WatchInsertFieldRows(WatchWindow* w, Watch* watch, size_t position, bool ensureLastVisible) {
   vector<Watch*> array = {};
   WatchInsertFieldRows2(w, watch, &array);
   w->rows.insert(w->rows.cbegin() + position, array.cbegin(), array.cend());
   if (ensureLastVisible)
      WatchEnsureRowVisible(w, position + array.size() - 1);
   array.clear();
}

void WatchAddExpression(WatchWindow* w, char* string = nullptr) {
   if (!string && w->textbox && !w->textbox->bytes) {
      WatchDestroyTextbox(w);
      return;
   }

   Watch* watch = (Watch*)calloc(1, sizeof(Watch));

   if (string) {
      watch->key = string;
   } else {
      watch->key                    = (char*)malloc(w->textbox->bytes + 1);
      watch->key[w->textbox->bytes] = 0;
      memcpy(watch->key, w->textbox->string, w->textbox->bytes);
   }

   WatchDeleteExpression(w); // Deletes textbox.
   w->rows.insert(w->rows.cbegin() + w->selectedRow, watch);
   w->baseExpressions.push_back(watch);
   w->selectedRow++;

   WatchEvaluate("gf_typeof", watch);

   if (!strstr(evaluateResult, "??")) {
      watch->type = strdup(evaluateResult);
      char* end   = strchr(watch->type, '\n');
      if (end)
         *end = 0;
      watch->hasFields = WatchHasFields(watch);
   }
}

void WatchAddExpression2(char* string) {
   UIElement*   element = InterfaceWindowSwitchToAndFocus("Watch");
   WatchWindow* w       = (WatchWindow*)element->cp;
   w->selectedRow       = w->rows.size();
   WatchAddExpression(w, strdup(string));
   if (w->selectedRow)
      w->selectedRow--;
   WatchEnsureRowVisible(w, w->selectedRow);
   w->element->parent->Refresh();
   w->element->Refresh();
}

int WatchLoggerWindowMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::DESTROY) {
      if (element->cp) {
         WatchLogger* logger = (WatchLogger*)element->cp;

         if (auto it = rng::find(watchLoggers, logger); it != rng::end(watchLoggers))
            watchLoggers.erase(it);

         char buffer[256];
         StringFormat(buffer, sizeof(buffer), "delete %d", logger->id);
         EvaluateCommand(buffer);

         for (auto& e : logger->entries) {
            e.trace.clear();
            e.evaluated.clear();
         }

         logger->entries.clear();
         free(logger->expressionsToEvaluate);
         free(logger);
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
      DisplaySetPosition(location, atoi(colon + 1), false);
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
         return StringFormat(m->buffer, m->bufferBytes, "%s", entry->value);
      } else if (m->column == 1) {
         return StringFormat(m->buffer, m->bufferBytes, "%s", entry->where);
      } else {
         if (m->column - 2 < (int)entry->evaluated.size()) {
            return StringFormat(m->buffer, m->bufferBytes, "%s", entry->evaluated[m->column - 2].result);
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
         return StringFormat(m->buffer, m->bufferBytes, "%d", entry->id);
      } else if (m->column == 1) {
         return StringFormat(m->buffer, m->bufferBytes, "%s", entry->function);
      } else if (m->column == 2) {
         return StringFormat(m->buffer, m->bufferBytes, "%s", entry->location);
      } else if (m->column == 3) {
         return StringFormat(m->buffer, m->bufferBytes, "0x%lX", entry->address);
      }
   } else if (message == UIMessage::LEFT_DOWN || message == UIMessage::MOUSE_DRAG) {
      int index = UITableHitTest((UITable*)element, element->window->cursor.x, element->window->cursor.y);
      WatchLoggerTraceSelectFrame(element, index, logger);
   }

   return 0;
}

bool WatchGetAddress(Watch* watch) {
   WatchEvaluate("gf_addressof", watch);

   if (strstr(evaluateResult, "??")) {
      UIDialogShow(windowMain, 0, "Couldn't get the address of the variable.\n%f%B", "OK");
      return false;
   }

   char* end = strstr(evaluateResult, " ");

   if (!end) {
      UIDialogShow(windowMain, 0, "Couldn't get the address of the variable.\n%f%B", "OK");
      return false;
   }

   *end = 0;

   char* end2 = strchr(evaluateResult, '\n');
   if (end2)
      *end2 = 0;

   return true;
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

   if (!WatchGetAddress(w->rows[w->selectedRow])) {
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

   char buffer[256];
   StringFormat(buffer, sizeof(buffer), "Log %s", evaluateResult);
   UIMDIChild* child = UIMDIChildCreate(dataWindow, UIMDIChild::CLOSE_BUTTON, UIRectangle(0), buffer, -1);
   StringFormat(buffer, sizeof(buffer), "watch * %s", evaluateResult);
   EvaluateCommand(buffer);
   char* number = strstr(evaluateResult, "point ");

   if (!number) {
      UIDialogShow(windowMain, 0, "Couldn't set the watchpoint.\n%f%B", "OK");
      return;
   }

   WatchLogger* logger = (WatchLogger*)calloc(1, sizeof(WatchLogger));

   UIButton* button = UIButtonCreate(child, UIButton::SMALL | UIElement::NON_CLIENT, "Resize columns", -1);
   button->invoke   = [logger]() { WatchLoggerResizeColumns(logger); };

   uintptr_t position = 0;
   position += StringFormat(logger->columns + position, sizeof(logger->columns) - position, "New value\tWhere");

   if (expressionsToEvaluate) {
      uintptr_t start = 0;

      for (uintptr_t i = 0; true; i++) {
         if (expressionsToEvaluate[i] == ';' || !expressionsToEvaluate[i]) {
            position += StringFormat(logger->columns + position, sizeof(logger->columns) - position, "\t%.*s",
                                     i - start, expressionsToEvaluate + start);
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

   logger->id                    = atoi(number + 6);
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
   int   id    = atoi(stringWatchpoint + 11);
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

   char* expressionsToEvaluate = logger->expressionsToEvaluate;

   if (expressionsToEvaluate) {
      uintptr_t start = 0;

      for (uintptr_t i = 0; true; i++) {
         if (expressionsToEvaluate[i] == ';' || !expressionsToEvaluate[i]) {
            WatchLogEvaluated evaluated;
            char              buffer[256];
            StringFormat(buffer, sizeof(buffer), "%.*s", i - start, expressionsToEvaluate + start);
            EvaluateExpression(buffer);
            start         = i + 1;
            size_t length = strlen(evaluateResult);
            if (length >= sizeof(evaluated.result))
               length = sizeof(evaluated.result) - 1;
            char* start = strstr(evaluateResult, " = ");
            memcpy(evaluated.result, start ? start + 3 : evaluateResult, length);
            evaluated.result[length] = 0;
            entry.evaluated.push_back(evaluated);
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
   strcpy(entry.value, value);
   strcpy(entry.where, where);
   vector<StackEntry> previousStack = stack;
   stack                            = {};
   DebuggerGetStack();
   entry.trace = stack;
   stack       = previousStack;
   logger->entries.push_back(entry);
   logger->table->itemCount++;
   logger->table->Refresh();
   DebuggerSend("c", false, false);
   return true;
}

void WatchCreateTextboxForRow(WatchWindow* w, bool addExistingText) {
   int         rowHeight = (int)(ui_size::TEXTBOX_HEIGHT * w->element->window->scale);
   UIRectangle row       = w->element->bounds;
   row.t += w->selectedRow * rowHeight, row.b = row.t + rowHeight;
   w->textbox                = UITextboxCreate(w->element, 0);
   w->textbox->messageUser = WatchTextboxMessage;
   w->textbox->cp          = w;
   w->textbox->Move(row, true);
   w->textbox->Focus();

   if (addExistingText) {
      UITextboxReplace(w->textbox, w->rows[w->selectedRow]->key, -1, false);
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
   Watch* watch = w->rows[w->selectedRow];
   if (!WatchGetAddress(watch))
      return;

   if (w->mode != WATCH_NORMAL) {
      InterfaceWindowSwitchToAndFocus("Watch");
      w = WatchGetFocused();
      assert(w != NULL);
   }

   char address[64];
   StringFormat(address, sizeof(address), "%s", evaluateResult);
   WatchEvaluate("gf_typeof", watch);
   if (strstr(evaluateResult, "??"))
      return;
   char* end = strchr(evaluateResult, '\n');
   if (end)
      *end = 0;
   size_t size   = strlen(address) + strlen(evaluateResult) + 16;
   char*  buffer = (char*)malloc(size);
   StringFormat(buffer, size, "(%s*)%s", evaluateResult, address);
   WatchAddExpression(w, buffer);
   WatchEnsureRowVisible(w, w->selectedRow);
   w->element->parent->Refresh();
   w->element->Refresh();
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
   char* position = w->rows[w->selectedRow]->value;
   while (*position && !isdigit(*position))
      position++;
   if (!(*position))
      return;
   uint64_t value = strtoul(position, &position, 0);
   char     buffer[256];
   StringFormat(buffer, sizeof(buffer), "info line * 0x%lx", value);
   EvaluateCommand(buffer);
   position = evaluateResult;

   if (strstr(evaluateResult, "No line number")) {
      char* end = strchr(evaluateResult, '\n');
      if (end)
         *end = 0;
      UIDialogShow(windowMain, 0, "%s\n%f%B", evaluateResult, "OK");
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

void CommandWatchSaveAsRecurse(FILE* file, Watch* watch, int indent, int indexInParentArray) {
   fprintf(file, "%.*s", indent, "\t\t\t\t\t\t\t\t\t\t\t\t\t\t");

   if (indexInParentArray == -1) {
      fprintf(file, "%s = ", watch->key);
   } else {
      fprintf(file, "[%d] = ", indexInParentArray);
   }

   if (watch->open) {
      fprintf(file, "\n");

      for (size_t i = 0; i < watch->fields.size(); i++) {
         CommandWatchSaveAsRecurse(file, watch->fields[i], indent + 1, watch->isArray ? i : -1);
      }
   } else {
      WatchEvaluate("gf_valueof", watch);
      char* value = strdup(evaluateResult);
      char* end   = strchr(value, '\n');
      if (end)
         *end = 0;
      fprintf(file, "%s\n", value);
      free(value);
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

   Watch* watch = w->rows[w->selectedRow];
   CommandWatchSaveAsRecurse(f, watch, 0, -1);
   fclose(f);
}

void CommandWatchCopyValueToClipboard(WatchWindow* w) {
   if (!w)
      return;
   if (w->mode == WATCH_NORMAL && w->selectedRow == w->rows.size())
      return;

   Watch* watch = w->rows[w->selectedRow];

   WatchEvaluate("gf_valueof", watch);
   char* value = strdup(evaluateResult);
   char* end   = strchr(value, '\n');
   if (end)
      *end = 0;

   _UIClipboardWriteText(w->element->window, value, sel_target_t::clipboard);
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

         UIRectangle intersection = UIRectangleIntersection(row, painter->clip);
         if (!intersection.valid())
            break;

         bool focused = i == w->selectedRow && element->window->focused == element;

         if (focused)
            UIDrawBlock(painter, row, ui->theme.selected);
         UIDrawBorder(painter, row, ui->theme.border, UIRectangle(0, 1, 0, 1));

         row.l += ui_size::TEXTBOX_MARGIN;
         row.r -= ui_size::TEXTBOX_MARGIN;

         if (i != w->rows.size()) {
            Watch* watch = w->rows[i];
            char   buffer[256];

            if ((!watch->value || watch->updateIndex != w->updateIndex) && !watch->open) {
               if (!programRunning) {
                  free(watch->value);
                  watch->updateIndex = w->updateIndex;
                  WatchEvaluate("gf_valueof", watch);
                  watch->value = strdup(evaluateResult);
                  char* end    = strchr(watch->value, '\n');
                  if (end)
                     *end = 0;
               } else {
                  free(watch->value);
                  watch->value = strdup("..");
               }
            }

            char keyIndex[64];

            if (!watch->key) {
               StringFormat(keyIndex, sizeof(keyIndex), "[%lu]", watch->arrayIndex);
            }

            if (focused && w->waitingForFormatCharacter) {
               StringFormat(buffer, sizeof(buffer), "Enter format character: (e.g. 'x' for hex)");
            } else {
               StringFormat(buffer, sizeof(buffer), "%.*s%s%s%s%s", watch->depth * 3,
                            "                                           ",
                            watch->open        ? "v "
                            : watch->hasFields ? "> "
                                               : "",
                            watch->key ?: keyIndex, watch->open ? "" : " = ", watch->open ? "" : watch->value);
            }

            if (focused) {
               UIDrawString(painter, row, buffer, -1, ui->theme.textSelected, UIAlign::left, nullptr);
            } else {
               UIDrawStringHighlighted(painter, row, buffer, -1, 1, NULL);
            }
         }
      }
   } else if (message == UIMessage::GET_HEIGHT) {
      return (WatchLastRow(w) + 1) * rowHeight;
   } else if (message == UIMessage::LEFT_DOWN) {
      if (element->window->cursor.y >= element->bounds.t) {
         w->selectedRow = (element->window->cursor.y - element->bounds.t) / rowHeight;

         if (w->selectedRow < w->rows.size()) {
            Watch* watch = w->rows[w->selectedRow];
            int    x     = (element->window->cursor.x - element->bounds.l) / ui->activeFont->glyphWidth;

            if (x >= watch->depth * 3 - 1 && x <= watch->depth * 3 + 1 && watch->hasFields) {
               UIKeyTyped m = {0};
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
               UIMenuAddItem(menu, 0, "Edit expression", -1, [w]() { WatchCreateTextboxForRow(w, true); });

               UIMenuAddItem(menu, 0, "Delete", -1, [w]() {
                  WatchDeleteExpression(w);
                  w->element->parent->Refresh();
                  w->element->Refresh();
               });
            }

            UIMenuAddItem(menu, 0, "Copy value to clipboard\tCtrl+C", -1,
                          [w]() { CommandWatchCopyValueToClipboard(w); });

            UIMenuAddItem(menu, 0, "Log writes to address...", -1, [w]() { WatchChangeLoggerCreate(w); });

            UIMenuAddItem(menu, 0, "Break on writes to address", -1, [w]() {
               if (w->selectedRow == w->rows.size())
                  return;
               if (!WatchGetAddress(w->rows[w->selectedRow]))
                  return;
               char buffer[256];
               StringFormat(buffer, sizeof(buffer), "watch * %s", evaluateResult);
               DebuggerSend(buffer, true, false);
            });

            if (firstWatchWindow) {
               UIMenuAddItem(menu, 0, "Add entry for address\tCtrl+E", -1,
                             [w]() { CommandWatchAddEntryForAddress(w); });
            }

            UIMenuAddItem(menu, 0, "View source at address\tCtrl+G", -1, [w]() { CommandWatchViewSourceAtAddress(w); });
            UIMenuAddItem(menu, 0, "Save as...", -1, [w]() { CommandWatchSaveAs(w); });

            UIMenuShow(menu);
         }
      }
   } else if (message == UIMessage::UPDATE) {
      element->Repaint(nullptr);
   } else if (message == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;
      result        = 1;

      if (w->waitingForFormatCharacter) {
         w->rows[w->selectedRow]->format = (m->textBytes && isalpha(m->text[0])) ? m->text[0] : 0;
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
      } else if (m->textBytes && m->text[0] == '/' && w->selectedRow != w->rows.size()) {
         w->waitingForFormatCharacter = true;
      } else if (m->textBytes && m->text[0] == '`') {
         result = 0;
      } else if (w->mode == WATCH_NORMAL && m->textBytes && m->code != UIKeycode::TAB && !w->textbox &&
                 !element->window->ctrl && !element->window->alt &&
                 (w->selectedRow == w->rows.size() || !w->rows[w->selectedRow]->parent)) {
         WatchCreateTextboxForRow(w, false);
         w->textbox->Message(message, di, dp);
      } else if (w->mode == WATCH_NORMAL && m->textBytes && m->code == UI_KEYCODE_LETTER('V') && !w->textbox &&
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
            if (currentLine < displayCode->lineCount) {
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
         Watch* watch = w->rows[w->selectedRow];
         watch->open  = true;
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
            if (w->rows[w->selectedRow]->parent == w->rows[i]) {
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
   }

   if (w->selectedRow > WatchLastRow(w)) {
      w->selectedRow = WatchLastRow(w);
   }

   return result;
}

int WatchPanelMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::LEFT_DOWN) {
      UIElement* window = ((WatchWindow*)element->cp)->element;
      window->Focus();
      window->Repaint(nullptr);
   }

   return 0;
}

UIElement* WatchWindowCreate(UIElement* parent) {
   WatchWindow* w       = (WatchWindow*)calloc(1, sizeof(WatchWindow));
   UIPanel*     panel   = UIPanelCreate(parent, UIPanel::SCROLL | UIPanel::COLOR_1);
   panel->messageUser = WatchPanelMessage;
   panel->cp          = w;
   w->element           = UIElementCreate(sizeof(UIElement), panel, UIElement::H_FILL | UIElement::TAB_STOP,
                                          WatchWindowMessage, "Watch");
   w->element->cp       = w;
   w->mode              = WATCH_NORMAL;
   w->extraRows         = 1;
   if (!firstWatchWindow)
      firstWatchWindow = w;
   return panel;
}

UIElement* LocalsWindowCreate(UIElement* parent) {
   WatchWindow* w       = (WatchWindow*)calloc(1, sizeof(WatchWindow));
   UIPanel*     panel   = UIPanelCreate(parent, UIPanel::SCROLL | UIPanel::COLOR_1);
   panel->messageUser = WatchPanelMessage;
   panel->cp          = w;
   w->element           = UIElementCreate(sizeof(UIElement), panel, UIElement::H_FILL | UIElement::TAB_STOP,
                                          WatchWindowMessage, "Locals");
   w->element->cp       = w;
   w->mode              = WATCH_LOCALS;
   return panel;
}

void WatchWindowUpdate(const char*, UIElement* element) {
   WatchWindow* w = (WatchWindow*)element->cp;

   if (w->mode == WATCH_LOCALS) {
      EvaluateCommand("py gf_locals()");

      bool newFrame = (!w->lastLocalList || 0 != strcmp(w->lastLocalList, evaluateResult));

      if (newFrame) {
         if (w->lastLocalList)
            free(w->lastLocalList);
         w->lastLocalList = strdup(evaluateResult);

         char*        buffer = strdup(evaluateResult);
         char*        s      = buffer;
         char*        end;
         vector<char*> expressions = {};

         while ((end = strchr(s, '\n')) != NULL) {
            *end = '\0';
            if (strstr(s, "(gdb)"))
               break;
            expressions.push_back(s);
            s = end + 1;
         }

         if (expressions.size() > 0) {
            for (size_t watchIndex = 0; watchIndex < w->baseExpressions.size(); watchIndex++) {
               Watch* watch   = w->baseExpressions[watchIndex];
               bool   matched = false;

               if (auto it = rng::find_if(expressions, [&](char* e) { return !strcmp(watch->key, e); });
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
               char* expression = strdup(exp);
               w->selectedRow   = w->rows.size();
               WatchAddExpression(w, expression);
            }

            w->selectedRow = w->rows.size();
         }

         free(buffer);
         expressions.clear();
      }
   }

   for (size_t i = 0; i < w->baseExpressions.size(); i++) {
      Watch* watch = w->baseExpressions[i];
      WatchEvaluate("gf_typeof", watch);
      char* result = strdup(evaluateResult);
      char* end    = strchr(result, '\n');
      if (end)
         *end = 0;
      const char* oldType = watch->type ?: "??";

      if (strcmp(result, oldType) && strcmp(result, "??")) {
         free(watch->type);
         watch->type = result;

         for (size_t j = 0; j < w->rows.size(); j++) {
            if (w->rows[j] == watch) {
               w->selectedRow = j;
               WatchAddExpression(w, strdup(watch->key));
               w->selectedRow = w->rows.size(), i--;
               break;
            }
         }
      } else {
         free(result);
      }
   }

   for (size_t i = 0; i < w->dynamicArrays.size(); i++) {
      Watch* watch = w->dynamicArrays[i];
      WatchEvaluate("gf_fields", watch);
      if (!strstr(evaluateResult, "(d_arr)"))
         continue;
      int count = atoi(evaluateResult + 7);
      if (count > WATCH_ARRAY_MAX_FIELDS)
         count = WATCH_ARRAY_MAX_FIELDS;
      if (count < 0)
         count = 0;
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
   WatchWindow* w = (WatchWindow*)element->cp;
   w->element->Focus();
}

void CommandAddWatch() {
   UIElement* element = InterfaceWindowSwitchToAndFocus("Watch");
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
   if (index >= 0 && index < ((UITable*)element)->itemCount && stackSelected != (size_t)index) {
      char buffer[64];
      StringFormat(buffer, 64, "frame %d", index);
      DebuggerSend(buffer, false, false);
      stackSelected = index;
      stackChanged  = true;
      element->Repaint(nullptr);
   }
}

int TableStackMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m = (UITableGetItem*)dp;
      m->isSelected     = (size_t)m->index == stackSelected;
      StackEntry* entry = &stack[m->index];

      if (m->column == 0) {
         return StringFormat(m->buffer, m->bufferBytes, "%d", entry->id);
      } else if (m->column == 1) {
         return StringFormat(m->buffer, m->bufferBytes, "%s", entry->function);
      } else if (m->column == 2) {
         return StringFormat(m->buffer, m->bufferBytes, "%s", entry->location);
      } else if (m->column == 3) {
         return StringFormat(m->buffer, m->bufferBytes, "0x%lX", entry->address);
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
   UITable* table       = UITableCreate(parent, 0, "Index\tFunction\tLocation\tAddress");
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
   int         anchor;
};

#define BREAKPOINT_WINDOW_COMMAND_FOR_EACH_SELECTED(function, action)       \
   void function(BreakpointTableData* data) {                               \
      for (auto selected : data->selected) {                                \
         for (const auto& breakpoint : breakpoints) {                       \
            if (breakpoint.number == selected) {                            \
               char buffer[1024];                                           \
               StringFormat(buffer, 1024, action " %d", selected);          \
               DebuggerSend(buffer, true, false);                           \
               break;                                                       \
            }                                                               \
         }                                                                  \
      }                                                                     \
   }

BREAKPOINT_WINDOW_COMMAND_FOR_EACH_SELECTED(CommandDeleteSelectedBreakpoints, "delete");
BREAKPOINT_WINDOW_COMMAND_FOR_EACH_SELECTED(CommandDisableSelectedBreakpoints, "disable");
BREAKPOINT_WINDOW_COMMAND_FOR_EACH_SELECTED(CommandEnableSelectedBreakpoints, "enable");

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
            UIMenuAddItem(menu, 0, "Delete", -1, [data]() { CommandDeleteSelectedBreakpoints(data); });

            if (atLeastOneBreakpointDisabled)
               UIMenuAddItem(menu, 0, "Enable", -1, [data]() { CommandEnableSelectedBreakpoints(data); });
            else
               UIMenuAddItem(menu, 0, "Disable", -1, [data]() { CommandDisableSelectedBreakpoints(data); });
         } else {
            UIMenuAddItem(menu, 0, "Delete", -1, [index]() { CommandDeleteBreakpoint(index); });

            if (breakpoints[index].enabled)
               UIMenuAddItem(menu, 0, "Disable", -1, [index]() { CommandDisableBreakpoint(index); });
            else
               UIMenuAddItem(menu, 0, "Enable", -1, [index]() { CommandEnableBreakpoint(index); });
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
   UITable* table       = UITableCreate(parent, 0, "File\tLine\tEnabled\tCondition\tHit");
   table->cp          = (BreakpointTableData*)calloc(1, sizeof(BreakpointTableData));
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
   dataTab          = UIPanelCreate(parent, UIPanel::EXPAND);
   UIPanel* panel5  = UIPanelCreate(dataTab, UIPanel::COLOR_1 | UIPanel::HORIZONTAL | UIPanel::SMALL_SPACING);
   buttonFillWindow = UIButtonCreate(panel5, UIButton::SMALL, "Fill window", -1);
   buttonFillWindow->invoke = []() { CommandToggleFillDataTab(); };

   for (const auto& idw : interfaceDataViewers) {
      UIButton *b = UIButtonCreate(panel5, UIButton::SMALL, idw.addButtonLabel, -1);
      b->invoke = [&]() { idw.addButtonCallback(); };
   }

   dataWindow             = UIMDIClientCreate(dataTab, UIElement::V_FILL);
   dataTab->messageUser = DataTabMessage;
   return dataTab;
}

// ---------------------------------------------------/
// Struct window:
// ---------------------------------------------------/

struct StructWindow {
   UICode*    display;
   UITextbox* textbox;
};

int TextboxStructNameMessage(UIElement* element, UIMessage message, int di, void* dp) {
   StructWindow* window = (StructWindow*)element->cp;

   if (message == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if (m->code == UIKeycode::ENTER) {
         char buffer[4096];
         StringFormat(buffer, sizeof(buffer), "ptype /o %.*s", (int)window->textbox->bytes, window->textbox->string);
         EvaluateCommand(buffer);
         char* end = strstr(evaluateResult, "\n(gdb)");
         if (end)
            *end = 0;
         UICodeInsertContent(window->display, evaluateResult, -1, true);
         UITextboxClear(window->textbox, false);
         window->display->Refresh();
         element->Refresh();
         return 1;
      }
   }

   return 0;
}

UIElement* StructWindowCreate(UIElement* parent) {
   StructWindow* window           = (StructWindow*)calloc(1, sizeof(StructWindow));
   UIPanel*      panel            = UIPanelCreate(parent, UIPanel::COLOR_1 | UIPanel::EXPAND);
   window->textbox                = UITextboxCreate(panel, 0);
   window->textbox->messageUser = TextboxStructNameMessage;
   window->textbox->cp          = window;
   window->display                = UICodeCreate(panel, UIElement::V_FILL | UICode::NO_MARGIN | UICode::SELECTABLE);
   UICodeInsertContent(window->display, "Type the name of a struct to view its layout.", -1, false);
   return panel;
}

// ---------------------------------------------------/
// Files window:
// ---------------------------------------------------/

struct FilesWindow {
   char     directory[PATH_MAX];
   UIPanel* panel;
   UILabel* path;
};

bool FilesPanelPopulate(FilesWindow* window);

mode_t FilesGetMode(FilesWindow* window, UIButton* button, size_t* oldLength) {
   const char* name = button->label;
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
                   button->labelBytes, button->flags & UIButton::CHECKED ? ui->theme.codeNumber : ui->theme.codeDefault,
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
         UIButton* button = UIButtonCreate(window->panel, 0, name, -1);
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
      UILabelSetContent(window->path, path, -1);
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
   FilesWindow* window    = (FilesWindow*)calloc(1, sizeof(FilesWindow));
   UIPanel*     container = UIPanelCreate(parent, UIPanel::EXPAND);
   window->panel =
      UIPanelCreate(container, UIPanel::COLOR_1 | UIPanel::EXPAND | UIPanel::SCROLL | UIElement::V_FILL);
   window->panel->gap = -1, window->panel->border = UIRectangle(1);
   window->panel->cp = window;
   UIPanel*  row       = UIPanelCreate(container, UIPanel::COLOR_2 | UIPanel::HORIZONTAL | UIPanel::SMALL_SPACING);

   UIButton* button    = UIButtonCreate(row, UIButton::SMALL, "-> cwd", -1);
   button->invoke      = [window]() { FilesNavigateToCWD(window); };

   button              = UIButtonCreate(row, UIButton::SMALL, "-> active file", -1);
   button->invoke      = [window]() { FilesNavigateToActiveFile(window); };

   window->path        = UILabelCreate(row, UIElement::H_FILL, "", 0);
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
   EvaluateCommand("info registers");

   if (strstr(evaluateResult, "The program has no registers now.") ||
       strstr(evaluateResult, "The current thread has terminated")) {
      return;
   }

   panel->DestroyDescendents();
   char*                position        = evaluateResult;
   vector<RegisterData> newRegisterData = {};
   bool                 anyChanges      = false;

   while (*position != '(') {
      char* nameStart = position;
      while (isspace(*nameStart))
         nameStart++;
      char* nameEnd = position = strchr(nameStart, ' ');
      if (!nameEnd)
         break;
      char* format1Start = position;
      while (isspace(*format1Start))
         format1Start++;
      char* format1End = position = strchr(format1Start, ' ');
      if (!format1End)
         break;
      char* format2Start = position;
      while (isspace(*format2Start))
         format2Start++;
      char* format2End = position = strchr(format2Start, '\n');
      if (!format2End)
         break;

      char* stringStart = nameStart;
      char* stringEnd   = format2End;

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
      UILabelCreate(row, 0, stringStart, stringEnd - stringStart);

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
      UILabelCreate(panel, 0, "No preset commands found in config file!", -1);

   for (const auto& cmd : presetCommands) {
      char buffer[256];
      StringFormat(buffer, sizeof(buffer), "gf-command %s", cmd.key);
      UIButton* button = UIButtonCreate(panel, 0, cmd.key, -1);
      char *b = strdup(buffer);
      button->invoke   = [b]() { CommandSendToGDB(b); };
   }

   return panel;
}

// ---------------------------------------------------/
// Log window:
// ---------------------------------------------------/

void* LogWindowThread(void* context) {
   if (!logPipePath) {
      fprintf(stderr, "Warning: The log pipe path has not been set in the configuration file!\n");
      return nullptr;
   }

   int file = open(logPipePath, O_RDONLY | O_NONBLOCK);

   if (file == -1) {
      fprintf(stderr, "Warning: Could not open the log pipe!\n");
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
   UICodeInsertContent(*(UICode**)buffer, buffer + sizeof(void*), -1, false);
   (*(UIElement**)buffer)->Refresh();
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
   bool active;
   int  id;
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
         char buffer[1024];
         StringFormat(buffer, 1024, "thread %d", window->threads[index].id);
         DebuggerSend(buffer, true, false);
      }
   }

   return 0;
}

UIElement* ThreadWindowCreate(UIElement* parent) {
   UITable* table       = UITableCreate(parent, 0, "ID\tFrame");
   table->cp          = (ThreadWindow*)calloc(1, sizeof(ThreadWindow));
   table->messageUser = ThreadTableMessage;
   return table;
}

void ThreadWindowUpdate(const char*, UIElement* _table) {
   ThreadWindow* window   = (ThreadWindow*)_table->cp;
   window->threads.clear();

   EvaluateCommand("info threads");
   char* position = evaluateResult;

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
      thread.id = atoi(position + 2);
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
   UITextbox * path, *arguments;
   UICheckbox* askDirectory;
};

void ExecutableWindowStartOrRun(ExecutableWindow* window, bool pause) {
   char buffer[4096];
   StringFormat(buffer, sizeof(buffer), "file \"%.*s\"", window->path->bytes, window->path->string);
   EvaluateCommand(buffer);

   if (strstr(evaluateResult, "No such file or directory.")) {
      UIDialogShow(windowMain, 0, "The executable path is invalid.\n%f%B", "OK");
      return;
   }

   StringFormat(buffer, sizeof(buffer), "start %.*s", window->arguments->bytes, window->arguments->string);
   EvaluateCommand(buffer);

   if (window->askDirectory->check == UICheckbox::CHECKED) {
      CommandParseInternal("gf-get-pwd", true);
   }

   if (!pause) {
      CommandParseInternal("run", false);
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
   fprintf(f, "[executable]\npath=%.*s\narguments=%.*s\nask_directory=%c\n", (int)window->path->bytes,
           window->path->string, (int)window->arguments->bytes, window->arguments->string,
           window->askDirectory->check == UICheckbox::CHECKED ? '1' : '0');
   fclose(f);
   SettingsAddTrustedFolder();
   UIDialogShow(windowMain, 0, "Saved executable settings!\n%f%B", "OK");
}

UIElement* ExecutableWindowCreate(UIElement* parent) {
   ExecutableWindow* window = (ExecutableWindow*)calloc(1, sizeof(ExecutableWindow));
   UIPanel*          panel  = UIPanelCreate(parent, UIPanel::COLOR_1 | UIPanel::EXPAND);
   UILabelCreate(panel, 0, "Path to executable:", -1);
   window->path = UITextboxCreate(panel, 0);
   UITextboxReplace(window->path, executablePath, -1, false);
   UILabelCreate(panel, 0, "Command line arguments:", -1);
   window->arguments = UITextboxCreate(panel, 0);
   UITextboxReplace(window->arguments, executableArguments, -1, false);
   window->askDirectory        = UICheckboxCreate(panel, 0, "Ask GDB for working directory", -1);
   window->askDirectory->check = executableAskDirectory ? UICheckbox::CHECKED : UICheckbox::UNCHECKED;
   UIPanel* row                = UIPanelCreate(panel, UIPanel::HORIZONTAL);

   UIButton* button = UIButtonCreate(row, 0, "Run", -1);
   button->invoke   = [window]() { ExecutableWindowRunButton(window); };

   button         = UIButtonCreate(row, 0, "Start", -1);
   button->invoke = [window]() { ExecutableWindowStartButton(window); };

   UISpacerCreate(row, 0, 10, 0);

   button         = UIButtonCreate(row, 0, "Save to .project.gf", -1);
   button->invoke = [window]() { ExecutableWindowSaveButton(window); };
   return panel;
}

// ---------------------------------------------------/
// Command search window:
// ---------------------------------------------------/

struct GDBCommand {
   char* name;
   char* description;
   char* descriptionLower;
};

struct CommandSearchWindow {
   UICode*            display;
   UITextbox*         textbox;
   vector<GDBCommand> commands;
};

int TextboxSearchCommandMessage(UIElement* element, UIMessage message, int di, void* dp) {
   CommandSearchWindow* window = (CommandSearchWindow*)element->cp;

   if (message == UIMessage::KEY_TYPED) {
      if (!window->commands.size()) {
         EvaluateCommand("help all");

         for (int i = 0; evaluateResult[i]; i++) {
            if (evaluateResult[i] == ',' && evaluateResult[i + 1] == ' ' && evaluateResult[i + 2] == '\n') {
               evaluateResult[i + 2] = ' ';
            }
         }

         char* position = evaluateResult;

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
            UICodeInsertContent(window->display, buffer, -1, firstMatch);
            firstMatch = false;
         }
      }

      if (firstMatch) {
         UICodeInsertContent(window->display, "(no matches)", -1, firstMatch);
      }

      window->display->vScroll->position = 0;
      window->display->Refresh();
   }

   return 0;
}

UIElement* CommandSearchWindowCreate(UIElement* parent) {
   CommandSearchWindow* window    = (CommandSearchWindow*)calloc(1, sizeof(CommandSearchWindow));
   UIPanel*             panel     = UIPanelCreate(parent, UIPanel::COLOR_1 | UIPanel::EXPAND);
   window->textbox                = UITextboxCreate(panel, 0);
   window->textbox->messageUser = TextboxSearchCommandMessage;
   window->textbox->cp          = window;
   window->display                = UICodeCreate(panel, UIElement::V_FILL | UICode::NO_MARGIN | UICode::SELECTABLE);
   UICodeInsertContent(window->display, "Type here to search \nGDB command descriptions.", -1, true);
   return panel;
}

// ----------------------------------------------------------
// Utilities:
// ----------------------------------------------------------

void ThumbnailResize(uint32_t* bits, uint32_t originalWidth, uint32_t originalHeight,
                     uint32_t targetWidth, uint32_t targetHeight) {
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
   void*    thisFunction;
   uint64_t timeStamp; // High bit set if exiting the function.
};

struct ProfWindow {
   uint64_t ticksPerMs;
   UIFont*  fontFlameGraph;
   bool     inStepOverProfiled;
};

struct ProfFlameGraphEntry {
   void*       thisFunction;
   const char* cName;
   double      startTime, endTime;
   int         depth;
   uint8_t     colorIndex;
};

struct ProfFlameGraphEntryTime {
   // Keep this structure as small as possible!
   float start, end;
   int   depth;
};

struct ProfSourceFileEntry {
   char cPath[256];
};

struct ProfFunctionEntry {
   uint32_t callCount;
   int      lineNumber;
   int      sourceFileIndex;
   double   totalTime;
   char     cName[64];
};

int ProfFlameGraphMessage(UIElement* element, UIMessage message, int di, void* dp);

struct ProfFlameGraphReport : public UIElement{
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
   ProfFlameGraphEntry*  entry    = report->menuItem;
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
   char buffer[80];
   StringFormat(buffer, sizeof(buffer), "b %s", entry->cName);
   CommandSendToGDB(buffer);
}

void ProfFillView(ProfFlameGraphReport* report) {
   ProfFlameGraphEntry*  entry  = report->menuItem;
   report->xStart               = entry->startTime;
   report->xEnd                 = entry->endTime;
   report->Repaint(0);
}

void ProfDrawTransparentOverlay(UIPainter* painter, UIRectangle rectangle, uint32_t color) {
   rectangle = UIRectangleIntersection(painter->clip, rectangle);
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
               char string[128];
               StringFormat(string, sizeof(string), "%s %fms", entry->cName, entry->endTime - entry->startTime);
               UIDrawString(painter, UIRectangle(r.l + 2, r.r, r.t, r.b), string, -1, profTextColor, UIAlign::left, NULL);
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
         printf("Using %d render threads.\n", profRenderThreadCount);

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
            char string[128];
            StringFormat(string, sizeof(string), "%.4fms", i);
            UIDrawBlock(painter, UIRectangle(r.l, r.l + 1, r.t, r.b), profBorderLightColor);
            UIDrawString(painter, r, string, -1, profTextColor, UIAlign::left, NULL);
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
         UIRectangle drawBounds   = UIRectangleIntersection(zoomBar, painter->clip);

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
         int line1Width = UIMeasureStringWidth(line1, -1);
         if (width < line1Width)
            width = line1Width;
         int line2Width = UIMeasureStringWidth(line2, -1);
         if (width < line2Width)
            width = line2Width;
         int line3Width = UIMeasureStringWidth(line3, -1);
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
         UIDrawString(painter, UIRectangle(x, x + width, y + lineHeight * 0, y + lineHeight * 1), line1, -1, 0xFFFFFFFF,
                      UIAlign::left, 0);
         UIDrawString(painter, UIRectangle(x, x + width, y + lineHeight * 1, y + lineHeight * 2), line2, -1, 0xFFFFFFFF,
                      UIAlign::left, 0);
         UIDrawString(painter, UIRectangle(x, x + width, y + lineHeight * 2, y + lineHeight * 3), line3, -1, 0xFFFFFFFF,
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
         UIMenuAddItem(menu, 0, "Show source", -1,    [report]() { ProfShowSource(report); });
         UIMenuAddItem(menu, 0, "Add breakpoint", -1, [report]() { ProfAddBreakpoint(report->hover); });
         UIMenuAddItem(menu, 0, "Fill view", -1,      [report]() { ProfFillView(report); });
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
   UI_FREE(report->switchViewButton->label);
   report->switchViewButton->label =
      UIStringCopy(report->showingTable ? "Graph view" : "Table view", (report->switchViewButton->labelBytes = -1));
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

   const char* ticksPerMsString = EvaluateExpression("gfProfilingTicksPerMs");
   ticksPerMsString             = ticksPerMsString ? strstr(ticksPerMsString, "= ") : nullptr;
   data->ticksPerMs             = ticksPerMsString ? atoi(ticksPerMsString + 2) : 0;

   if (!ticksPerMsString || !data->ticksPerMs) {
      UIDialogShow(windowMain, 0, "Profile data could not be loaded (1).\nConsult the guide.\n%f%b", "OK");
      return;
   }

   int rawEntryCount = atoi(strstr(EvaluateExpression("gfProfilingBufferPosition"), "= ") + 2);
   printf("Reading %d profiling entries...\n", rawEntryCount);

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
      UIDrawString(&painter, painter.clip, string, -1, ui->theme.text, UIAlign::center, 0);
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
   EvaluateCommand(buffer);
   FILE* f = fopen(path, "rb");

   if (!f) {
      UIDialogShow(windowMain, 0, "Profile data could not be loaded (2).\nConsult the guide.\n%f%b", "OK");
      free(rawEntries);
      return;
   }

   fread(rawEntries, 1, sizeof(ProfProfilingEntry) * rawEntryCount, f);
   fclose(f);
   unlink(path);

   printf("Got raw profile data.\n");

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
      const char* cName = EvaluateExpression(buffer);
      if (!cName)
         continue;

      if (strchr(cName, '<'))
         cName = strchr(cName, '<') + 1;
      int length = strlen(cName);
      if (length > (int)sizeof(function.cName) - 1)
         length = sizeof(function.cName) - 1;
      memcpy(function.cName, cName, length);
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
      EvaluateCommand(buffer);

      if (!strstr(evaluateResult, "Traceback (most recent call last):")) {
         char* end = strchr(evaluateResult, '\n');
         if (end)
            *end = 0;
         ProfSourceFileEntry sourceFile  = {};
         char*               cSourceFile = evaluateResult;
         length                          = strlen(cSourceFile);
         if (length > (int)sizeof(sourceFile.cPath) - 1)
            length = sizeof(sourceFile.cPath) - 1;
         memcpy(sourceFile.cPath, cSourceFile, length);
         sourceFile.cPath[length] = 0;
         StringFormat(buffer, sizeof(buffer), "py print(gdb.lookup_global_symbol('%s').line)", function.cName);
         EvaluateCommand(buffer);
         function.lineNumber = atoi(evaluateResult);

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

   UIMDIChild* window =
      UIMDIChildCreate(dataWindow, UIMDIChild::CLOSE_BUTTON, ui_rect_2s(800, 600), "Flame graph", -1);
   UIButton* switchViewButton = UIButtonCreate(window, UIButton::SMALL | UIElement::NON_CLIENT, "Table view", -1);
   UITable*  table = UITableCreate(window, 0, "Name\tTime spent (ms)\tCall count\tAverage per call (ms)");
   ProfFlameGraphReport* report = new ProfFlameGraphReport(window, 0);

   report->vScroll = UIScrollBarCreate(report, 0);
   report->font    = data->fontFlameGraph;

   window->cp             = report;
   window->messageUser    = ProfReportWindowMessage;

   switchViewButton->cp   = report;
   switchViewButton->invoke = [report]() { ProfSwitchView(report); };
   table->cp              = report;
   table->messageUser     = ProfTableMessage;
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
         ProfFlameGraphEntry entry    = {};
         if (report->functions.contains(rawEntries[i].thisFunction)) {
            ProfFunctionEntry& function = report->functions[rawEntries[i].thisFunction];
            entry.cName = function.cName;
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

   printf("Found %ld functions over %zu source files.\n", report->functions.size(), report->sourceFiles.size());

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
   EvaluateCommand("call GfProfilingStart()");
   CommandSendToGDB("gf-next");
   window->inStepOverProfiled = true;
}

void ProfWindowUpdate(const char* data, UIElement* element) {
   ProfWindow* window = (ProfWindow*)element->cp;

   if (window->inStepOverProfiled) {
      EvaluateCommand("call GfProfilingStop()");
      ProfLoadProfileData(window);
      InterfaceWindowSwitchToAndFocus("Data");
      dataWindow->Refresh();
      window->inStepOverProfiled = false;
   }
}

UIElement* ProfWindowCreate(UIElement* parent) {
   const int   fontSizeFlameGraph = 8;
   ProfWindow* window             = (ProfWindow*)calloc(1, sizeof(ProfWindow));
   window->fontFlameGraph         = UIFontCreate(_UI_TO_STRING_2(UI_FONT_PATH), fontSizeFlameGraph);
   UIPanel* panel                 = UIPanelCreate(parent, UIPanel::COLOR_1 | UIPanel::EXPAND);
   panel->cp                    = window;
   UIButton* button               = UIButtonCreate(panel, UIElement::V_FILL, "Step over profiled", -1);
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

static int MemoryWindowMessage(UIElement* element, UIMessage message, int di, void* dp);
static void MemoryWindowGotoButtonInvoke(void* cp);

struct MemoryWindow : public UIElement {
   UIButton*       gotoButton;
   vector<int16_t> loadedBytes;
   uint64_t        offset;

   MemoryWindow(UIElement* parent)
      : UIElement(parent, 0, MemoryWindowMessage, "memory window")
      , gotoButton(UIButtonCreate(this, UIButton::SMALL, "&", -1)) {
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
         UIDrawString(painter, row, buffer, -1, ui->theme.codeString, UIAlign::left, 0);
         row.t += rowHeight;
         row.b += rowHeight;
         const char* header = "         0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F   0123456789ABCDEF";
         UIDrawString(painter, row, header, -1, ui->theme.codeComment, UIAlign::left, 0);
         row.t += rowHeight;
         row.b += rowHeight;
      }

      if (rowCount > 0 && rowCount * 16 > window->loadedBytes.size()) {
         window->loadedBytes.clear();

         for (size_t i = 0; i < (size_t)rowCount * 16 / 8; i++) {
            StringFormat(buffer, sizeof(buffer), "x/8xb 0x%lx", window->offset + i * 8);
            EvaluateCommand(buffer);

            bool error = true;

            if (!strstr(evaluateResult, "Cannot access memory")) {
               char* position = strchr(evaluateResult, ':');

               if (position) {
                  position++;

                  for (int i = 0; i < 8; i++) {
                     window->loadedBytes.push_back(strtol(position, &position, 0));
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
         UIDrawString(painter, row, buffer, -1, ui->theme.codeComment, UIAlign::left, 0);
         UIRectangle r          = row + UIRectangle(UIMeasureStringWidth(buffer, -1), 0, 0, 0);
         int         glyphWidth = UIMeasureStringWidth("a", 1);

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
                                         bounds.t, bounds.t + window->gotoButton->Message(UIMessage::GET_HEIGHT, 0, 0)),
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
      EvaluateCommand(buffer);
      const char* result = evaluateResult;

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
   virtual ~storage_t() = default;
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
      hScroll->maximum = w * UIMeasureStringWidth("A", 1) * itemCharacters;
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
      UIDrawString(painter, element->bounds, message, -1, ui->theme.text, UIAlign::left, nullptr);
      UIRectangle swatch = UIRectangle(element->bounds.l + UIMeasureStringWidth(message, -1), 0, element->bounds.t + 2,
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

      int        glyphWidth  = UIMeasureStringWidth("A", 1);
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
            } else if (grid->grid_type == grid_type_t::float_t ||
                       grid->grid_type == grid_type_t::double_t) {
               double f = grid->grid_type == grid_type_t::double_t
                  ? ((double*)grid->data())[i * grid->w + j]
                  : (double)((float*)grid->data())[i * grid->w + j];
               char   buffer[64];
               StringFormat(buffer, sizeof(buffer), "%f", f);
               UIRectangle rectangle =
                  UIRectangle(j * glyphWidth * 14, (j + 1) * glyphWidth * 14, i * glyphHeight, (i + 1) * glyphHeight);
               UIRectangle offset = UIRectangle(element->bounds.l - (int)grid->hScroll->position,
                                              element->bounds.t - (int)grid->vScroll->position);
               UIDrawString(painter, rectangle + offset, buffer, -1, ui->theme.text, UIAlign::right, nullptr);
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
   int glyphWidth = UIMeasureStringWidth("a", 1), glyphHeight = UIMeasureStringHeight();

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
   UIElement* watchElement = InterfaceWindowSwitchToAndFocus("Watch");
   if (!watchElement)
      return;
   WatchWindow* w = (WatchWindow*)watchElement->cp;
   if (w->textbox)
      return;
   if (w->selectedRow > w->rows.size() || !w->rows.size())
      return;
   Watch* watch = w->rows[w->selectedRow == w->rows.size() ? w->selectedRow - 1 : w->selectedRow];
   if (!watch)
      return;
   if (!cp)
      cp = InterfaceWindowSwitchToAndFocus("View");
   if (!cp)
      return;

   // Destroy the previous panel contents.
   UIElement* panel = (UIElement*)cp;
   panel->DestroyDescendents();
   UIButton* button = UIButtonCreate(panel, 0, "View (Ctrl+Shift+V)", -1);
   button->invoke   = [panel]() { ViewWindowView(panel); };

   // Get information about the watch expression.
   char *end, type[256], buffer[256];
   char  oldFormat = watch->format;
   watch->format   = 0;
   WatchEvaluate("gf_typeof", watch);
   end = strchr(evaluateResult, '\n');
   if (end)
      *end = 0;
   StringFormat(type, sizeof(type), "%s", evaluateResult);
   StringFormat(buffer, sizeof(buffer), "Type: %s", type);
   UILabelCreate(panel, 0, buffer, -1);
   WatchEvaluate("gf_valueof", watch);
   end = strchr(evaluateResult, '\n');
   if (end)
      *end = 0;
   watch->format = oldFormat;
   // printf("valueof: {%s}\n", evaluateResult);

   // Create the specific display for the given type.
   if (0 == strcmp(type, "uint8_t") || 0 == strcmp(type, "uint16_t") || 0 == strcmp(type, "uint32_t") ||
       0 == strcmp(type, "uint64_t") || 0 == strcmp(type, "int8_t") || 0 == strcmp(type, "int16_t") ||
       0 == strcmp(type, "int32_t") || 0 == strcmp(type, "int64_t") || 0 == strcmp(type, "unsigned char") ||
       0 == strcmp(type, "unsigned short") || 0 == strcmp(type, "unsigned int") || 0 == strcmp(type, "unsigned") ||
       0 == strcmp(type, "unsigned long") || 0 == strcmp(type, "unsigned long long") || 0 == strcmp(type, "char") ||
       0 == strcmp(type, "short") || 0 == strcmp(type, "int") || 0 == strcmp(type, "long") ||
       0 == strcmp(type, "long long")) {
      uint64_t value;

      if (evaluateResult[0] == '-') {
         value = -strtoul(evaluateResult + 1, nullptr, 10);
      } else {
         value = strtoul(evaluateResult, nullptr, 10);
      }

      StringFormat(buffer, sizeof(buffer), " 8b: %d %u 0x%x '%c'", (int8_t)value, (uint8_t)value, (uint8_t)value,
                   (char)value);
      UILabelCreate(panel, 0, buffer, -1);
      StringFormat(buffer, sizeof(buffer), "16b: %d %u 0x%x", (int16_t)value, (uint16_t)value, (uint16_t)value);
      UILabelCreate(panel, 0, buffer, -1);
      StringFormat(buffer, sizeof(buffer), "32b: %d %u 0x%x", (int32_t)value, (uint32_t)value, (uint32_t)value);
      UILabelCreate(panel, 0, buffer, -1);
      StringFormat(buffer, sizeof(buffer), "64b: %ld %lu 0x%lx", (int64_t)value, (uint64_t)value, (uint64_t)value);
      UILabelCreate(panel, 0, buffer, -1);

      int p = StringFormat(buffer, sizeof(buffer), "Bin: ");

      for (int64_t i = 63; i >= 32; i--) {
         buffer[p++] = (value & ((uint64_t)1 << i)) ? '1' : '0';
         if ((i & 7) == 0)
            buffer[p++] = ' ';
      }

      UILabelCreate(panel, 0, buffer, p);

      p = StringFormat(buffer, sizeof(buffer), "     ");

      for (int64_t i = 31; i >= 0; i--) {
         buffer[p++] = (value & ((uint64_t)1 << i)) ? '1' : '0';
         if ((i & 7) == 0)
            buffer[p++] = ' ';
      }

      UILabelCreate(panel, 0, buffer, p);

      if (value <= 0xFFFFFFFF) {
         new ViewWindowColorSwatch(panel, (uint32_t)value);
      }
   } else if ((0 == memcmp(type, "char [", 6) && !strstr(type, "][")) || 0 == strcmp(type, "const char *") ||
              0 == strcmp(type, "char *")) {
      printf("string '%s'\n", evaluateResult);
      char address[64];

      if (evaluateResult[0] != '(') {
         WatchEvaluate("gf_addressof", watch);
         printf("addressof '%s'\n", evaluateResult);
         char* end = strchr(evaluateResult, ' ');
         if (end)
            *end = 0;
         end = strchr(evaluateResult, '\n');
         if (end)
            *end = 0;
         StringFormat(address, sizeof(address), "%s", evaluateResult);
      } else {
         char* end = strchr(evaluateResult + 1, ' ');
         if (!end)
            goto unrecognised;
         *end = 0;
         StringFormat(address, sizeof(address), "%s", evaluateResult + 1);
      }

      char tempPath[PATH_MAX];
      realpath(".temp.gf", tempPath);
      char buffer[PATH_MAX * 2];
      StringFormat(buffer, sizeof(buffer), "(size_t)strlen((const char *)(%s))", address);
      EvaluateExpression(buffer);
      printf("'%s' -> '%s'\n", buffer, evaluateResult);
      const char* lengthString = evaluateResult ? strstr(evaluateResult, "= ") : nullptr;
      size_t      length       = lengthString ? atoi(lengthString + 2) : 0;
      // TODO Preventing errors when calling strlen from crashing the target?

      if (!length) {
         goto unrecognised;
      }

      unique_ptr<char[]> data = make_unique<char[]>(length + 1);

      if (!data) {
         goto unrecognised;
      }

      StringFormat(buffer, sizeof(buffer), "dump binary memory %s (%s) (%s+%d)", tempPath, address, address, length);
      EvaluateCommand(buffer);
      printf("'%s' -> '%s'\n", buffer, evaluateResult);
      FILE* f = fopen(tempPath, "rb");

      if (f) {
         fread(data.get(), 1, length, f);
         fclose(f);
         unlink(tempPath);
         data[length] = 0;
         // printf("got '%s'\n", data);
         new ViewWindowString(panel, std::move(data), length);
         StringFormat(buffer, sizeof(buffer), "%d+1 bytes", length);
         UILabelCreate(panel, UIElement::H_FILL, buffer, -1);
      } else {
         goto unrecognised;
      }
   } else if (0 == memcmp(type, "char [", 6) || 0 == memcmp(type, "float [", 7) || 0 == memcmp(type, "double [", 8)) {
      int itemSize       = type[0] == 'c' ? sizeof(char) : type[0] == 'f' ? sizeof(float) : sizeof(double);
      char* p = strchr(type, '[') + 1;
      int   w = strtol(p, &p, 0);
      if (memcmp(p, "][", 2))
         goto unrecognised;
      p += 2;
      int h = strtol(p, &p, 0);
      if (strcmp(p, "]"))
         goto unrecognised;
      if (w <= 1 || h <= 1)
         goto unrecognised;
      if (!WatchGetAddress(watch))
         goto unrecognised;

      ViewWindowMatrixGrid* grid = new ViewWindowMatrixGrid(panel, w, h, type[0]);

      char tempPath[PATH_MAX];
      realpath(".temp.gf", tempPath);
      char buffer[PATH_MAX * 2];
      StringFormat(buffer, sizeof(buffer), "dump binary memory %s (%s) (%s+%d)", tempPath, evaluateResult,
                   evaluateResult, w * h * itemSize);
      EvaluateCommand(buffer);
      FILE* f = fopen(tempPath, "rb");

      if (f) {
         grid->read(f);
         fclose(f);
         unlink(tempPath);
      }

      if ((grid->grid_type == grid_type_t::float_t || grid->grid_type == grid_type_t::double_t) && w == h &&
          w <= 4 && w >= 2) {
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
         UILabelCreate(panel, 0, buffer, -1);
      }
   } else {
   unrecognised:;
      // TODO Custom view.
      // TODO Table view for array of structures.
      UILabelCreate(panel, 0, "No view available for type.", -1);
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
   UIButton* button = UIButtonCreate(panel, 0, "View (Ctrl+Shift+V)", -1);
   button->invoke   = [panel]() { ViewWindowView(panel); };
   UILabelCreate(panel, 0, "Select a watch expression, then click View.", -1);
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
      , zoomOut(new UIButton(this, UIButton::SMALL, "-", -1))
      , zoomIn(new UIButton(this, UIButton::SMALL, "+", -1))
      , normalize(new UIButton(this, UIButton::SMALL, "Norm", -1))
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
   rectangle = UIRectangleIntersection(painter->clip, rectangle);
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
      painter->clip       = UIRectangleIntersection(client, painter->clip);
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
                  UIDrawBlock(painter, UIRectangle(x1 - 2, x1 + 2, y - 2, y + 2), channel % 2 ? 0xFFFF00FF : 0xFF00FFFF);
               }
            }
         }

         int mouseXSample =
            (float)(element->window->cursor.x - client.l) / element->bounds.width() * display->samplesOnScreen -
            0.5f;

         if (mouseXSample >= 0 && mouseXSample < sampleCount &&
             element->clip.contains(element->window->cursor) &&
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

            UIDrawString(painter, stringRectangle, buffer, -1, ui->theme.text, UIAlign::right, NULL);

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
   int              parsedSampleCount, parsedChannels;
   UIButton*        autoToggle;
   UIPanel*         labelPanel;
   UILabel*         label;
   WaveformDisplay* display;
};

void WaveformViewerUpdate(const char* pointerString, const char* sampleCountString, const char* channelsString,
                          UIElement* owner);

const char* WaveformViewerGetSamples(const char* pointerString, const char* sampleCountString,
                                     const char* channelsString, float** _samples, int* _sampleCount, int* _channels) {
   const char* sampleCountResult = EvaluateExpression(sampleCountString);
   if (!sampleCountResult) {
      return "Could not evaluate sample count.";
   }
   int         sampleCount    = atoi(sampleCountResult + 1);
   const char* channelsResult = EvaluateExpression(channelsString);
   if (!channelsResult) {
      return "Could not evaluate channels.";
   }
   int channels = atoi(channelsResult + 1);
   if (channels < 1 || channels > 8) {
      return "Channels must be between 1 and 8.";
   }
   const char* pointerResult = EvaluateExpression(pointerString, "/x");
   if (!pointerResult) {
      return "Could not evaluate pointer.";
   }
   char _pointerResult[1024];
   StringFormat(_pointerResult, sizeof(_pointerResult), "%s", pointerResult);
   pointerResult = strstr(_pointerResult, " 0x");
   if (!pointerResult) {
      return "Pointer to sample data does not look like an address!";
   }
   pointerResult++;

   size_t byteCount = sampleCount * channels * 4;
   float* samples   = (float*)malloc(byteCount);

   char transferPath[PATH_MAX];
   realpath(".transfer.gf", transferPath);

   char buffer[PATH_MAX * 2];
   StringFormat(buffer, sizeof(buffer), "dump binary memory %s (%s) (%s+%d)", transferPath, pointerResult,
                pointerResult, byteCount);
   EvaluateCommand(buffer);

   FILE* f = fopen(transferPath, "rb");

   if (f) {
      fread(samples, 1, byteCount, f);
      fclose(f);
      unlink(transferPath);
   }

   if (!f || strstr(evaluateResult, "access")) {
      return "Could not read the waveform samples!";
   }

   *_samples = samples, *_sampleCount = sampleCount, *_channels = channels;
   return nullptr;
}

int WaveformViewerWindowMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::DESTROY) {
      DataViewerRemoveFromAutoUpdateList(element);
      free(element->cp);
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
   FILE*            f       = fopen(path, "wb");
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
      UIMenu* menu = UIMenuCreate(element->window, UIMenu::NO_SCROLL);
      UIMenuAddItem(menu, 0, "Save to .wav...", -1, [display]() { WaveformViewerSaveToFile(display); });
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
      WaveformViewer* viewer = (WaveformViewer*)calloc(1, sizeof(WaveformViewer));
      if (pointerString)
         StringFormat(viewer->pointer, sizeof(viewer->pointer), "%s", pointerString);
      if (sampleCountString)
         StringFormat(viewer->sampleCount, sizeof(viewer->sampleCount), "%s", sampleCountString);
      if (channelsString)
         StringFormat(viewer->channels, sizeof(viewer->channels), "%s", channelsString);

      UIMDIChild* window    = UIMDIChildCreate(dataWindow, UIMDIChild::CLOSE_BUTTON, UIRectangle(0), "Waveform", -1);
      window->messageUser = WaveformViewerWindowMessage;
      window->cp          = viewer;
      viewer->autoToggle    = UIButtonCreate(window, UIButton::SMALL | UIElement::NON_CLIENT, "Auto", -1);
      viewer->autoToggle->cp          = (void*)WaveformViewerAutoUpdateCallback;
      viewer->autoToggle->messageUser = DataViewerAutoUpdateButtonMessage;
      UIButtonCreate(window, UIButton::SMALL | UIElement::NON_CLIENT, "Refresh", -1)->messageUser =
         WaveformViewerRefreshMessage;
      owner = window;

      UIPanel* panel                 = UIPanelCreate(owner, UIPanel::EXPAND);
      viewer->labelPanel             = UIPanelCreate(panel, UIPanel::COLOR_1 | UIElement::V_FILL);
      viewer->label                  = UILabelCreate(viewer->labelPanel, UIElement::H_FILL, nullptr, 0);
      viewer->display                = WaveformDisplayCreate(panel, UIElement::V_FILL);
      viewer->display->messageUser = WaveformViewerDisplayMessage;
   }

   WaveformViewer* viewer    = (WaveformViewer*)owner->cp;
   viewer->parsedSampleCount = sampleCount, viewer->parsedChannels = channels;

   if (error) {
      UILabelSetContent(viewer->label, error, -1);
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

__attribute__((constructor)) void ExtensionsRegister() {
   interfaceWindows.push_back({.name = "Prof", .create = ProfWindowCreate, .update = ProfWindowUpdate, .alwaysUpdate = true});
   interfaceWindows.push_back({"Memory", MemoryWindowCreate, MemoryWindowUpdate});
   interfaceWindows.push_back({"View", ViewWindowCreate, ViewWindowUpdate});
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

void MsgReceivedData(char* input) {
   programRunning = false;

   if (firstUpdate) {
      EvaluateCommand(pythonCode);

      char path[PATH_MAX];
      StringFormat(path, sizeof(path), "%s/.config/gf2_watch.txt", getenv("HOME"));
      char* data = LoadFile(path, NULL);

      while (data && restoreWatchWindow) {
         char* end = strchr(data, '\n');
         if (!end)
            break;
         *end = 0;
         WatchAddExpression2(data);
         data = end + 1;
      }

      firstUpdate = false;
   }

   if (WatchLoggerUpdate(input))
      return;
   if (showingDisassembly)
      DisassemblyUpdateLine();

   DebuggerGetStack();
   DebuggerGetBreakpoints();

   for (auto& iw : interfaceWindows) {
      InterfaceWindow* window = &iw;
      if (!window->update || !window->element)
         continue;
      if (!window->alwaysUpdate && ElementHidden(window->element))
         window->queuedUpdate = true;
      else
         window->update(input, window->element);
   }

   DataViewersUpdateAll();

   if (displayOutput) {
      UICodeInsertContent(displayOutput, input, -1, false);
      displayOutput->Refresh();
   }

   if (trafficLight)
      trafficLight->Repaint(nullptr);

   free(input);
}

void MsgReceivedControl(char* input) {
   char* end = strchr(input, '\n');
   if (end)
      *end = 0;

   if (input[0] == 'f' && input[1] == ' ') {
      DisplaySetPosition(input + 2, 1, false);
   } else if (input[0] == 'l' && input[1] == ' ') {
      DisplaySetPosition(nullptr, atoi(input + 2), false);
   } else if (input[0] == 'c' && input[1] == ' ') {
      CommandParseInternal(input + 2, false);
   }
}

auto gdb_invoker(const char* cmd) {
   return  [cmd]() { CommandSendToGDB(cmd); };
}

__attribute__((constructor)) void InterfaceAddBuiltinWindowsAndCommands() {
   interfaceWindows.push_back({"Stack", StackWindowCreate, StackWindowUpdate});
   interfaceWindows.push_back({"Source", SourceWindowCreate, SourceWindowUpdate});
   interfaceWindows.push_back({"Breakpoints", BreakpointsWindowCreate, BreakpointsWindowUpdate});
   interfaceWindows.push_back({"Registers", RegistersWindowCreate, RegistersWindowUpdate});
   interfaceWindows.push_back({"Watch", WatchWindowCreate, WatchWindowUpdate, WatchWindowFocus});
   interfaceWindows.push_back({"Locals", LocalsWindowCreate, WatchWindowUpdate, WatchWindowFocus});
   interfaceWindows.push_back({"Commands", CommandsWindowCreate, nullptr});
   interfaceWindows.push_back({"Data", DataWindowCreate, nullptr});
   interfaceWindows.push_back({"Struct", StructWindowCreate, nullptr});
   interfaceWindows.push_back({"Files", FilesWindowCreate, nullptr});
   interfaceWindows.push_back({"Console", ConsoleWindowCreate, nullptr});
   interfaceWindows.push_back({"Log", LogWindowCreate, nullptr});
   interfaceWindows.push_back({"Thread", ThreadWindowCreate, ThreadWindowUpdate});
   interfaceWindows.push_back({"Exe", ExecutableWindowCreate, nullptr});
   interfaceWindows.push_back({"CmdSearch", CommandSearchWindowCreate, nullptr});

   interfaceDataViewers.push_back({"Add bitmap...", BitmapAddDialog});

   interfaceCommands.push_back({
      .label = "Run\tShift+F5",
         .shortcut{.code = UI_KEYCODE_FKEY(5), .shift = true, .invoke = gdb_invoker("r") }
   });
   interfaceCommands.push_back({
      .label = "Run paused\tCtrl+F5",
         .shortcut{.code = UI_KEYCODE_FKEY(5), .ctrl = true, .invoke = gdb_invoker("start") }
   });
   interfaceCommands.push_back({
         .label = "Kill\tF3", .shortcut{.code = UI_KEYCODE_FKEY(3), .invoke = gdb_invoker("kill") }
   });
   interfaceCommands.push_back({
      .label = "Restart GDB\tCtrl+R",
      .shortcut{
         .code = UI_KEYCODE_LETTER('R'), .ctrl = true, .invoke = gdb_invoker("gf-restart-gdb") }
   });
   interfaceCommands.push_back({
      .label = "Connect\tF4",
      .shortcut{.code = UI_KEYCODE_FKEY(4), .invoke = gdb_invoker("target remote :1234") }
   });
   interfaceCommands.push_back({
         .label = "Continue\tF5", .shortcut{.code = UI_KEYCODE_FKEY(5), .invoke = gdb_invoker("c") }
   });
   interfaceCommands.push_back({
      .label = "Step over\tF10",
      .shortcut{.code = UI_KEYCODE_FKEY(10), .invoke = gdb_invoker("gf-next") }
   });
   interfaceCommands.push_back({
      .label = "Step out of block\tShift+F10",
      .shortcut{
         .code = UI_KEYCODE_FKEY(10), .shift = true, .invoke = gdb_invoker("gf-step-out-of-block") }
   });
   interfaceCommands.push_back({
      .label = "Step in\tF11",
      .shortcut{.code = UI_KEYCODE_FKEY(11), .invoke = gdb_invoker("gf-step") }
   });
   interfaceCommands.push_back({
      .label = "Step into outer\tShift+F8",
      .shortcut{
         .code = UI_KEYCODE_FKEY(8), .shift = true, .invoke = gdb_invoker("gf-step-into-outer") }
   });
   interfaceCommands.push_back({
      .label = "Step out\tShift+F11",
      .shortcut{.code = UI_KEYCODE_FKEY(11), .shift = true, .invoke = gdb_invoker("finish") }
   });
   interfaceCommands.push_back({
      .label = "Reverse continue\tCtrl+Shift+F5",
      .shortcut{.code = UI_KEYCODE_FKEY(5), .ctrl = true, .shift = true, .invoke = gdb_invoker("reverse-continue")}
   });
   interfaceCommands.push_back({
      .label = "Reverse step over\tCtrl+Shift+F10",
      .shortcut{.code   = UI_KEYCODE_FKEY(10),
                .ctrl   = true,
                .shift  = true,
                .invoke = gdb_invoker("reverse-next") }
   });
   interfaceCommands.push_back({
      .label = "Reverse step in\tCtrl+Shift+F11",
      .shortcut{.code = UI_KEYCODE_FKEY(11), .ctrl = true, .shift = true, .invoke = gdb_invoker("reverse-step")}
   });
   interfaceCommands.push_back({
      .label = "Pause\tF8", .shortcut{.code = UI_KEYCODE_FKEY(8), .invoke = CommandPause}
   });
   interfaceCommands.push_back({
         .label = "Toggle breakpoint\tF9", .shortcut{.code = UI_KEYCODE_FKEY(9), .invoke = []() { CommandToggleBreakpoint(); }}
   });
   interfaceCommands.push_back({
      .label = "Sync with gvim\tF2", .shortcut{.code = UI_KEYCODE_FKEY(2), .invoke = CommandSyncWithGvim}
   });
   interfaceCommands.push_back({
      .label = "Ask GDB for PWD\tCtrl+Shift+P",
      .shortcut{.code   = UI_KEYCODE_LETTER('P'),
                .ctrl   = true,
                .shift  = true,
                .invoke = gdb_invoker("gf-get-pwd") }
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
         .shortcut{.code = UI_KEYCODE_LETTER('E'), .ctrl = true, .invoke = []() { CommandWatchAddEntryForAddress(); } }
   });
   interfaceCommands.push_back({
      .label = nullptr,
      .shortcut{.code = UI_KEYCODE_LETTER('G'), .ctrl = true, .invoke = []() { CommandWatchViewSourceAtAddress(); } }
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
   msgReceivedLog     = ReceiveMessageRegister(LogReceived);
}

void InterfaceShowMenu(UIButton* self) {
   UIMenu* menu = UIMenuCreate((UIElement*)self, UIMenu::PLACE_ABOVE | UIMenu::NO_SCROLL);

   for (const auto& ic : interfaceCommands) {
      if (!ic.label)
         continue;
      UIMenuAddItem(menu, 0, ic.label, -1, ic.shortcut.invoke);
   }

   UIMenuShow(menu);
}

UIElement* InterfaceWindowSwitchToAndFocus(const char* name) {
   for (auto& iw : interfaceWindows) {
      InterfaceWindow* window = &iw;
      if (!window->element)
         continue;
      if (strcmp(window->name, name))
         continue;

      if ((window->element->flags & UIElement::HIDE) && window->element->parent->messageClass == _UITabPaneMessage) {
         UITabPane* tabPane = (UITabPane*)window->element->parent;

         for (uint32_t i = 0; i < tabPane->childCount; i++) {
            if (tabPane->children[i] == window->element) {
               tabPane->active = i;
               break;
            }
         }

         tabPane->Refresh();
      }

      if (window->focus) {
         window->focus(window->element);
      } else if (window->element->flags & UIElement::TAB_STOP) {
         window->element->Focus();
      }

      return window->element;
   }

   UIDialogShow(windowMain, 0, "Couldn't find the window '%s'.\n%f%B", name, "OK");
   return nullptr;
}

int WindowMessage(UIElement*, UIMessage message, int di, void* dp) {
   if (message == UIMessage::WINDOW_ACTIVATE) {
      DisplaySetPosition(currentFileFull, currentLine, false);
   } else {
      for (const auto& msgtype : receiveMessageTypes) {
         if (msgtype.message == message) {
            msgtype.callback((char*)dp);
            break;
         }
      }
   }

   return 0;
}

int InterfaceTabPaneMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::LAYOUT) {
      element->messageClass(element, message, di, dp);

      for (auto& iw : interfaceWindows) {
         InterfaceWindow* window = &iw;

         if (window->element && (~window->element->flags & UIElement::HIDE) && window->queuedUpdate) {
            window->queuedUpdate = false;
            window->update("", window->element);
            window->element->Move(window->element->bounds, false);
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
      fprintf(stderr, "Error: Invalid character in layout string '%c'.\n", first);
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
            fprintf(stderr, "Error: Expected a number in layout string; got '%s'.\n", buffer);
            exit(1);
         }
      } else if (strcmp(expected, buffer)) {
         fprintf(stderr, "Error: Expected '%s' in layout string; got '%s'.\n", expected, buffer);
         exit(1);
      }
   }

   return buffer;
}

void InterfaceLayoutCreate(UIElement* parent) {
   const char* token = InterfaceLayoutNextToken();

   if (0 == strcmp("h", token) || 0 == strcmp("v", token)) {
      uint32_t flags = UIElement::V_FILL | UIElement::H_FILL;
      if (*token == 'v')
         flags |= UIElement::VERTICAL;
      InterfaceLayoutNextToken("(");
      UIElement* container = UISplitPaneCreate(parent, flags, atoi(InterfaceLayoutNextToken("#")) * 0.01f);
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
            fprintf(stderr, "Error: Invalid layout string! Expected ',' or ')' in tab container list; got '%s'.\n",
                    token);
            exit(1);
         }
      }
   } else {
      bool found = false;

      for (auto& iw : interfaceWindows) {
         InterfaceWindow* w = &iw;

         if (0 == strcmp(token, w->name)) {
            w->element = w->create(parent);
            found      = true;
            break;
         }
      }

      if (!found) {
         fprintf(stderr, "Error: Invalid layout string! The window '%s' was not found.\n", token);
         exit(1);
      }
   }
}

unique_ptr<UI> GfMain(int argc, char** argv) {
   if (argc == 2 && (0 == strcmp(argv[1], "-?") || 0 == strcmp(argv[1], "-h") || 0 == strcmp(argv[1], "--help"))) {
      fprintf(stderr,
              "Usage: %s [GDB args]\n\n"
              "GDB args: Pass any GDB arguments here, they will be forwarded to GDB.\n\n"
              "For more information, view the README at https://github.com/nakst/gf/blob/master/README.md.\n",
              argv[0]);
      return {};
   }

   struct sigaction sigintHandler = {};
   sigintHandler.sa_handler       = [](int) {
      DebuggerClose();
      exit(0);
   };
   sigaction(SIGINT, &sigintHandler, nullptr);

   gdbArgv    = (char**)malloc(sizeof(char*) * (argc + 1));
   gdbArgv[0] = (char*)"gdb";
   memcpy(gdbArgv + 1, argv + 1, sizeof(argv) * argc);
   gdbArgc = argc;

   if (argc >= 2 && 0 == strcmp(argv[1], "--rr-replay")) {
      gdbArgv[0] = (char*)"rr";
      gdbArgv[1] = (char*)"replay";
      gdbPath    = "rr";
   }

   getcwd(localConfigDirectory, sizeof(localConfigDirectory));
   StringFormat(globalConfigPath, sizeof(globalConfigPath), "%s/.config/gf2_config.ini", getenv("HOME"));
   StringFormat(localConfigPath, sizeof(localConfigPath), "%s/.project.gf", localConfigDirectory);

   SettingsLoad(true);
   auto ui_ptr = UIInitialise(ui_config);
   ui->theme = uiThemeDark;

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
         fprintf(stderr, "Using font %s\n", fontPath);
      }
   }
#endif

   fontCode = UIFontCreate(fontPath, fontSizeCode);
   UIFontActivate(UIFontCreate(fontPath, fontSizeInterface));

   windowMain                = UIWindowCreate(0, maximize ? UIWindow::MAXIMIZE : 0, "gf", window_width, window_height);
   windowMain->scale         = uiScale;
   windowMain->messageUser   = WindowMessage;

   for (const auto& ic : interfaceCommands) {
      if (!(int)ic.shortcut.code)
         continue;
      UIWindowRegisterShortcut(windowMain, ic.shortcut);
   }

   switcherMain = UISwitcherCreate(windowMain, 0);
   InterfaceLayoutCreate(UIPanelCreate(switcherMain, UIPanel::EXPAND));
   UISwitcherSwitchTo(switcherMain, switcherMain->children[0]);

   if (*InterfaceLayoutNextToken()) {
      fprintf(stderr, "Warning: Layout string has additional text after the end of the top-level entry.\n");
   }

   SettingsLoad(false);
   pthread_cond_init(&evaluateEvent, nullptr);
   pthread_mutex_init(&evaluateMutex, nullptr);
   DebuggerStartThread();
   CommandSyncWithGvim();
   return ui_ptr;
}

int main(int argc, char** argv) {
   auto ui_ptr = GfMain(argc, argv);
   if (!ui_ptr)
      return 1;

   UIMessageLoop();
   DebuggerClose();

   if (restoreWatchWindow && firstWatchWindow) {
      StringFormat(globalConfigPath, sizeof(globalConfigPath), "%s/.config/gf2_watch.txt", getenv("HOME"));
      FILE* f = fopen(globalConfigPath, "wb");

      if (f) {
         for (const auto& exp : firstWatchWindow->baseExpressions) {
            fprintf(f, "%s\n", exp->key);
         }

         fclose(f);
      } else {
         fprintf(stderr, "Warning: Could not save the contents of the watch window; '%s' was not accessible.\n",
                 globalConfigPath);
      }
   }

   return 0;
}
