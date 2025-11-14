# Improvements Made to [gf](https://github.com/greg7mdp/gf)


This document lists the major improvements made to [gf](https://github.com/greg7mdp/gf) since the initial fork from the [original repository](https://github.com/nakst/gf). 

## Table of Contents

- [Feature Implementation Status](#feature-implementation-status)
- [Code Modernization](#code-modernization)
- [User Interface Enhancements](#user-interface-enhancements)
- [Configuration System](#configuration-system)
- [Bug Fixes and Stability](#bug-fixes-and-stability)
- [Documentation](#documentation)
- [Build System](#build-system)

---

## Feature Implementation Status

This section tracks the implementation status of major features (from GitHub issue #43):

### ✅ Implemented Features

#### Configuration and Persistence
- **`.gf` Directory Management**: Manage `.gf` configuration directory, either in launch directory or one level up if launch directory starts with `build`
- **Persist Breakpoints**: Breakpoints saved in `.gf/<progname>.ini`
- **Persist Watches**: Watch expressions saved in `.gf/<progname>.ini`
- **Persist Executable Launch Parameters**: Program arguments saved in `.gf/<progname>.ini` with up/down arrow keys in executable window to switch between saved configurations
- **Persist Command History**: Command history saved in `.gf/<progname>.hist`
- **Persist Layout and Window Size**: Persist layout/window size in `.gf/gf_config.ini` and load if present

#### Code Architecture
- **Global Buffer Manager**: Implemented for storing `UICode` files (commit ad0d8b7)
- **Command Line Parsing**: Extract executable name and arguments from command line using GDB syntax and populate ExecutableWindow (commits 540cf71, 76eae88)
- **Multi-language/Multi-debugger Support**: Infrastructure for supporting multiple languages and debuggers

#### clangd Integration
- **Code Navigation**: Connect to clangd server and navigate with `Alt+.` (goto definition) and `Alt+,` (go back) support (also supports Windows key bindings)
- **Semantic Highlighting**: Full syntax highlighting using clangd's semantic tokens with correct type information

#### User Interface
- **Mouse Selection in UITextbox**: Select text in `UITextbox` with mouse (commit fdca9d8)
- **Smart F5/F10 Behavior**: If program isn't running, F5 runs it and F10 starts it (commit 404f6d6)
- **Keyboard Accelerators**: Readline-style keyboard shortcuts in `UITextbox` including undo
- **Double-Click Support**: Context-aware selection in `UICode` based on clicked character
- **PRIMARY Selection**: Support for `PRIMARY` selection in addition to `CLIPBOARD`, process `SelectionRequest` events
- **Focus Management**: Grab focus when breakpoint is hit (saving old focus window), restore when hitting F5 (configurable via `grab_focus_on_breakpoint` in ini file)

#### Watch and Variable Display
- **Struct Display**: Nicer display of struct variables (still not perfect though)
- **Visual Studio-style Watch Window**: Enhanced watch window display
- **Modified Values Highlighted**: Show values modified since last evaluation in red

#### Breakpoints
- **Display Breakpoint Numbers**: Show breakpoint numbers in breakpoints window
- **Enable/Disable by Number**: Press single digit number to enable/disable breakpoint
- **Double-Click Toggle**: Double-click to toggle breakpoint
- **Ctrl+Click Selection**: Allow Ctrl+click selection toggle in breakpoints window

#### Testing
- **Test Infrastructure**: Implement tests using doctest framework, starting with regex tests

### 🚧 Planned Features (Not Yet Implemented)

#### Search and Navigation
- **Emacs-like Search**: Implement `Ctrl-S` / `Ctrl-R` incremental search in Source Code window with status line
- **Command History Search**: Incremental and reverse search through command history

#### clangd Advanced Features
- **Find References** (`Alt+?`): Show all locations where a symbol is used throughout the codebase
- **Hover Information**: Display type information, documentation, and function signatures when hovering over functions

#### Display Enhancements
- **Hover for Full Values (Watch Window)**: Show complete, unabbreviated value when hovering in watch window
- **Hover for Full Values (Code Window)**: Show complete, unabbreviated value when hovering over selected expression
- **Enhanced Caret Visibility**: Make caret more visible in `UITextbox`, especially when empty (whitespace on left, thicker/blinking caret)
- **Shorten Template Names**: Abbreviate long template names in stack trace (see issue #40)

#### Focus Management
- **Auto-Focus on Tab Switch**: Move focus to first `UITextbox` when entering `cmdsearch` or selecting a tab (use `UIElement::tab_stop_flag` and `switch_to_window_and_focus()`)

---

## Code Modernization

- Upgraded codebase to use more idiomatic C++
- Replaced C-style arrays and hash map with `std::vector`, `std::string` and `std::unordered_map`
- Converted `malloc/free` to `new/delete` and RAII patterns
- Proper C++ object initialization
- Replaced raw pointers with smart pointers where appropriate, eliminated manual memory management in favor of standard containers
- Replace some manual string pattern matching with regular expression using CTRE (compile-time regular expressions)
- Systematically converted camelCase variables to snake_case throughout codebase
- Replaced preprocessor macros with proper functions and constants
- Improved const-correctness throughout
- Ran clang-tidy and sanitizers and addressed issues
- Added test suites for some new features

---

## User Interface Enhancements

### Theme System
- Multiple built-in color themes (17+ themes)
- Theme categories: dark and light variants
- Notable themes:
  - `dark_github`, `dark_github_colorblind`
  - `dark_one_dark_pro`, `dark_modern`
  - `dark_jacaranda`, `dark_green_tea`, `dark_tsoding`
  - `light_github`, `light_solarized`, `light_quiet`
  - `light_dev_cpp`, `light_vs_cpp`, `light_vs_2017_cpp`
- Theme switching shortcuts: `Ctrl+T` (next), `Ctrl+Shift+T` (previous)
- Predefined theme selection in config file
- Theme color overrides in config

### Breakpoints Window
- Display breakpoint numbers
- Click number (single digit) to enable/disable breakpoint
- Double-click to toggle breakpoint
- Ctrl+click for selection toggle
- Enhanced right-click menu

### Watch Window
- Values modified since last evaluation shown in red
- Improved alignment of values
- Proper handling of complex structs
- Support for backslash (`\`) character in expressions
- "Add watch" option in right-click menu
- Format specifier support

### Source Window
- Improved annotation display
- Fixed multiline comment highlighting
- Better handling of current position indicators
- Enhanced double-click selection (smarter word boundaries)
- Augmented right-click menu
- Fixed scrolling issues
- Stores `starts_in_comment` to avoid recompute on every paint

### Layout Management
- Copy current window layout to clipboard
- Correct rounding in `GenerateLayoutString`
- Configurable window dimensions
- Maximize option
- Proper screen size detection and sane defaults

### Focus Management
- `grab_focus_on_breakpoint` option to grab focus when breakpoint hit
- `get_focus()` and `set_focus()` methods
- Restore focus when continuing in debugger


---

## Configuration System

### INI File Management
- Rewrite of INI file parsing
- `INI_File` class for structured access
- `INI_Updater` class for safe updates
- `with_section` and `with_section_lines` for efficient processing
- `insert_in_section` for powerful content manipulation
- Avoid duplicate lines in program-specific `.ini` files

### Program-Specific Configuration
- Individual `.ini` files per debugged program in `.gf` directory
- JSON format for program start configuration
- Automatic save of program arguments at exit
- Command history per program
- Breakpoint and watch restoration

### Project Configuration
- `.project.gf` file for project-wide settings
- Watched variables saved in project config
- Breakpoint persistence

### Enhanced Settings
- `gdb.arguments`: Pass arguments to GDB
- `gdb.path`: Custom GDB executable path
- `gdb.log_all_output`: Log all GDB output
- `gdb.confirm_command_kill`: Disable confirmation dialogs
- `gdb.backtrace_count_limit`: Stack frame limit
- `clangd.path`: Custom clangd executable path
- `ui.scale`: UI scaling factor
- `ui.font_path` and `ui.font_size_code`: Font customization
- `ui.window_width`, `ui.window_height`, `ui.maximize`: Window sizing
- `ui.selectable_source`: Enable source text selection
- `ui.grab_focus_on_breakpoint`: Focus behavior

### Backward Compatibility
- Loads `~/.config/gf_config.ini` if present
- Falls back to `~/.config/gf2_config.ini`

---

## Bug Fixes and Stability

### Memory Management
- Fixed memory leaks in UIElement destruction
- Eliminated null pointer dereferences
- Proper cleanup of dynamically allocated objects
- Added virtual destructors where needed
- Fixed use-after-free issues

### GDB Communication
- Fixed detection of end of debugger output
- Added timeout to GDB queue wait
- Better handling of GDB responses
- Fixed queue processing and quit handling
- Allow DebuggerThread to be killed cleanly

### Display Issues
- Fixed scrolling in Executable textbox
- Fixed multiline comment highlighting
- Fixed issue where extra newlines show up in log

---

## Documentation

### User Documentation
- Comprehensive `README.md` updates
- Installation instructions for pre-built binaries
- Build from source guide with prerequisites
- Quick Start guide with essential shortcuts
- Non-essential shortcuts section
- Complete theme list with descriptions
- Configuration guide with examples
- Layout customization documentation
- clangd integration instructions
- Text editor and Vim integration

### Developer Documentation
- `doc/luigi.md`: Luigi GUI framework documentation
- `doc/plugins.md`: Plugin system documentation
- `doc/profiler_instructions.txt`: Profiler usage guide
- `doc/contributing.md`: Contribution guidelines
- `doc/release.md`: Release process

### Code Documentation
- Added extensive comments throughout codebase
- Documented keyboard and mouse actions
- Explained complex algorithms and data structures
- API documentation for Luigi framework

---

## Build System

### CMake Modernization
- Updated `CMakeLists.txt` for modern CMake
- Build luigi as separate library
- Build examples using CMake
- Support for `CMAKE_EXPORT_COMPILE_COMMANDS`
- Proper compiler option settings
- Support for both Ninja and Make generators

### CI/CD
- GitHub Actions workflow for Linux
- Automated testing on commits
- Build status badges

### Portable Builds
- Reproducible and portable build process
- Pre-built binary releases
- Proper dependency management

### Dependencies
- Bundled dependencies in `deps/` directory:
  - CTRE for compile-time regex
  - doctest for testing
  - nlohmann/json for JSON parsing
