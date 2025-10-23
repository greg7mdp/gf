# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

gf is a GDB frontend written in C++23 that provides an enhanced debugging experience with a custom UI framework called Luigi.

## Build Commands

```bash
# Standard debug build
cmake -Bbuild -DCMAKE_BUILD_TYPE=Debug -DCMAKE_CXX_COMPILER=clang++-18 -GNinja .
cmake --build build

# Run the application
./build/gf

# Run tests
./build/tests/luigi_tests
./build/tests/re_cpp_tests
./build/tests/re_gdb_tests
```

## Architecture

### Core Components

1. **Luigi UI Framework** (`src/luigi.cpp`, `src/luigi.hpp`)
   - Custom immediate-mode UI framework
   - Platform-specific code for Linux (X11/FreeType) and Windows
   - Uses SSE2 optimizations for rendering

2. **GF Core** (`src/gf.cpp`, `src/gf.hpp`)
   - Main debugger frontend implementation
   - Handles GDB communication via pipes
   - Manages UI windows (Source, Watch, Stack, etc.)
   - Features include: breakpoints, watch expressions, disassembly view, control pipes

3. **Regular Expression Engine** (`src/re/`)
   - `re.hpp`: Core regex functionality using CTRE library
   - `cpp.cpp`: C++ code parsing and analysis
   - `gdb.cpp`: GDB output parsing
   - `misc.cpp`: Utility regex functions

### Key Design Patterns

- **Window System**: Windows are registered in `interfaceWindows` array with create/update callbacks
- **Command System**: Commands use `DebuggerSend()` for GDB communication
- **Expression Evaluation**: `EvaluateExpression()` handles watch expressions and hover evaluation
- **Configuration**: INI-style config files loaded from `~/.config/gf2_config.ini` and `.project.gf`

### Important Functions

- `DebuggerSend()`: Send commands to GDB (src/gf.cpp:2066)
- `EvaluateExpression()`: Evaluate expressions in GDB context (src/gf.cpp:2106)
- `SourceWindowUpdate()`: Main source view update logic (src/gf.cpp:3686)
- `WatchWindowUpdate()`: Watch expression handling (src/gf.cpp:4214)

## Development Guidelines

- Use C++23 features (requires clang++-18 or g++-13)
- Follow existing code style: tabs for indentation, braces on same line
- Platform-specific code should be isolated with `#ifdef` blocks
- All GDB communication must go through `DebuggerSend()` or `EvaluateExpression()`

## Testing

- Unit tests use doctest framework (included in deps/)
- Test files in `tests/` directory test specific components
- Example programs in `examples/` for integration testing

## Luigi UI Framework Architecture

### Core Concepts

Luigi is a custom UI framework that combines retained-mode structure with immediate-mode style APIs:

- **UIElement**: Base class for all UI components with hierarchy, bounds, and message handling
- **Message System**: All UI communication through `UIMessage` enum (PAINT, LAYOUT, input events)
- **CRTP Pattern**: Enables fluent API with proper return types via `UIElementCast<T>`

### Element Hierarchy

**Container Elements:**
- `UIWindow`: Top-level windows
- `UIPanel`: Vertical/horizontal layout containers
- `UISplitPane`: Resizable splits
- `UITabPane`: Tabbed interfaces
- `UIMDIClient/Child`: Multiple document interface

**Interactive Elements:**
- `UIButton`, `UICheckbox`, `UISlider`: Basic controls
- `UITextbox`: Text input with clipboard support
- `UITable`: Data grids with columns
- `UICode`: Syntax-highlighted code editor

### Layout System

Two-pass layout process:
1. **Measure Pass**: Elements report preferred size via GET_WIDTH/GET_HEIGHT messages
2. **Arrange Pass**: Parents assign bounds via LAYOUT message

Layout features:
- Fill flags for expanding elements
- Fixed and flexible sizing
- Gap and border support in panels
- Nested layout composition

### Rendering Pipeline

1. Window maintains double-buffered bitmap
2. Paint requests create `UIPainter` with clipping
3. Elements handle PAINT messages recursively
4. Platform layer blits buffer to screen

Drawing operations:
- `draw_block()`: Filled rectangles
- `draw_string()`: Text with FreeType fonts
- `draw_control_default()`: Theme-based control rendering

### Event Handling

- Platform events arrive at window level
- Window dispatches to element under cursor
- Elements process or propagate messages
- State changes trigger UPDATE messages for refresh

Example usage pattern:
```cpp
panel.add_button(0, "Click Me").on_click([](UIButton& btn) {
    // Handle click
});
```

## Keyboard and Mouse Shortcuts

### Debugging Control

**Function Keys:**
- `F2`: Sync with gvim
- `F3`: Kill debugged process
- `F4`: Connect to remote target (target remote :1234)
- `F5`: Continue execution
- `F8`: Break (interrupt debugger)
- `F9`: Toggle breakpoint at current line
- `F10`: Step over (next line, don't enter functions)
- `F11`: Step in (enter functions)

**Function Keys with Shift:**
- `Shift+F5`: Run program from start
- `Shift+F8`: Step into outer scope
- `Shift+F10`: Step out of current block
- `Shift+F11`: Step out (finish current function)

**Function Keys with Ctrl:**
- `Ctrl+F5`: Run paused (start command)

**Function Keys with Ctrl+Shift:**
- `Ctrl+Shift+F5`: Reverse continue (record/replay debugging)
- `Ctrl+Shift+F10`: Reverse step over
- `Ctrl+Shift+F11`: Reverse step in

### Inspection and Navigation

- **Backtick (`)**: Inspect line (shows expressions at current line in popup)
  - In inspect mode: `1-9` keys add selected expression to watch
  - `Escape` or backtick again exits inspect mode
  - `Up/Down` arrows navigate lines while inspecting

### Watch Window

- **Ctrl+C**: Copy value to clipboard
- **Ctrl+E**: Add entry for address (context menu also available)
- **Ctrl+G**: View source at address
- **Enter**: Edit watch expression
- **Backspace**: Edit watch expression
- **Delete**: Remove watch expression
- **Left Arrow** (when collapsed): Edit expression
- **/ (forward slash)**: Change format specifier (e.g., `/x` for hexadecimal, `/d` for decimal)
- **Format keys**: After pressing `/`, type format character (x, d, f, etc.)
- **Tab**: Auto-complete watch expression while typing

### Console/Command Window

- **Enter**: Send command to GDB (or repeat last command if empty)
- **Shift+Enter**: Insert newline in textbox
- **Tab**: Tab completion for GDB commands and symbols
- **Backtick (`)**: Exit to source window (when empty)

### Breakpoints Window

- **Delete**: Remove selected breakpoint(s)
- **0-9**: Toggle breakpoint by number
- **Double-click**: Toggle breakpoint enable/disable

### Stack Window

- **Up/Down arrows**: Navigate stack frames

### General Commands

- **Ctrl+R**: Restart GDB
- **Ctrl+M**: Set disassembly mode
- **Ctrl+Shift+P**: Ask GDB for current working directory (PWD)
- **Ctrl+Shift+V**: View window command

### Commented Out (Conflicts with Textbox Bindings)
The following shortcuts are defined but disabled due to conflicts:
- `Ctrl+D`: Toggle disassembly
- `Ctrl+E`: Add watch (alternative)
- `Ctrl+B`: Toggle fill data tab
- `Ctrl+L`: Clear output
- `Ctrl+P`: Previous command
- `Ctrl+N`: Next command

### Mouse Actions

**Source Window:**
- **Left click**: Set cursor, select text
- **Ctrl+Left click**: Run "until" that line (continue execution until reaching the clicked line)
- **Shift+Left click**: Skip to that line without executing code in between (jump command)
- **Left double-click**: Select expression under cursor (for inspection)
- **Right click**: Context menu
  - Enable/disable breakpoints on line
  - Breakpoint management options
- **Mouse move/hover**: Shows expression evaluation tooltip

**Watch Window:**
- **Left click**: Select watch expression
- **Right click**: Context menu
  - Edit expression
  - Delete expression
  - Copy value to clipboard (Ctrl+C)
  - Log writes to address
  - Break on writes to address
  - Add entry for address (Ctrl+E)
  - View source at address (Ctrl+G)

**Breakpoints Window:**
- **Left click**: Select breakpoint
- **Left double-click**: Toggle breakpoint enable/disable
- **Right click**: Context menu
  - Delete selected breakpoint(s)
  - Enable/disable selected breakpoint(s)

**Stack Window:**
- **Left click**: Select and switch to stack frame
- **Left drag**: Select frame while dragging

**Data/Memory/Profiling Windows:**
- **Left click/drag**: Selection and interaction
- **Middle click/drag**: Zoom and pan operations (profiling flame graph)
- **Mouse move**: Update hover information

### Text Selection

- Standard text selection works in all `UICode` elements (source, output, log windows)
- Click and drag to select
- Double-click to select word/expression
- Clipboard operations available via context menus

## Reproducible Build System

The project includes a Docker-based reproducible build system for creating portable Linux binaries.

### Location and Files

- **Dockerfile**: `.github/tools/reproducible.Dockerfile`
- **Build script**: `.github/tools/build-reproducible.sh`
- **GitHub Actions**: `.github/workflows/release.yml` ("Release Build" workflow)
- **Documentation**: `.github/tools/README.md`

### Build Environment

- **Base image**: Debian Bullseye (bullseye-20240722-slim) for glibc 2.31 compatibility
- **Compiler**: LLVM/Clang 21.1.4 with libc++ (statically linked)
- **CMake**: 3.27.6 (built from source)
- **Bootstrap**: Uses LLVM 18 from apt.llvm.org to build LLVM 21.1.4
- **Target**: Generic x86-64 (`-march=x86-64`) for maximum CPU compatibility

### Key Design Decisions

1. **Bootstrap compilation**: Debian Bullseye's g++ 10.2.1 cannot compile LLVM 21 (segfaults), so we install LLVM 18 from apt first, then use it to build LLVM 21.1.4

2. **Generic CPU target**: Uses `-march=x86-64` (baseline x86-64 with SSE2 only) to ensure the binary runs on all x86-64 CPUs, not just the build host's CPU

3. **Static libc++**: Statically links libc++ and libc++abi to avoid ABI compatibility issues across different Linux distributions

4. **Security hardening**: Includes `-fstack-protector-strong`, `-D_FORTIFY_SOURCE=2`, PIE, and RELRO

### Important Code Considerations

**`ensure_null_terminated` class (luigi.hpp:156)**:
- NEVER access `string_view[size()]` - this is undefined behavior
- The class always copies data to ensure null termination
- Previous optimization attempt (`if (sz && sv[sz] == 0)`) caused illegal instruction errors with LLVM 21's stricter UB detection

**CMakeLists.txt subdirectories**:
- `examples/` and `tests/` subdirectories are made optional with `if(EXISTS ...)` checks
- This allows building without these directories (excluded by `.dockerignore`)

### Local Testing

```bash
# Build reproducible binary locally
./.github/tools/build-reproducible.sh [output-dir]

# Default output: build-reproducible/gf
# Build time: ~20-30 minutes (mostly LLVM compilation)
```

### GitHub Actions

The "Release Build" workflow can be triggered:
1. **Automatically**: When a GitHub release is published
2. **Manually**: Via Actions tab → Release Build → Run workflow

Output:
- `gf-{version}-linux-x86_64.tar.gz`
- `gf-{version}-linux-x86_64.tar.gz.sha256`

### Compatibility

The resulting binary runs on:
- Ubuntu 18.04+ (Bionic and newer)
- Debian 10+ (Buster and newer)
- RHEL/CentOS 8+
- Fedora 28+
- Most modern Linux distributions with glibc 2.31+

Runtime dependencies:
- libX11 (X11 libraries)
- libfreetype (FreeType library)
- Linux kernel 3.2+
- glibc 2.31+

## Dependencies

External (must be installed):
- Linux: X11, FreeType, pthread
- Windows: native Win32 APIs

Bundled (in deps/):
- Boost.Mp11: Template metaprogramming
- CTRE: Compile-time regular expressions
- doctest: Testing framework
- nlohmann/json: JSON parsing