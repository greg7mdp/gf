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

## Dependencies

External (must be installed):
- Linux: X11, FreeType, pthread
- Windows: native Win32 APIs

Bundled (in deps/):
- Boost.Mp11: Template metaprogramming
- CTRE: Compile-time regular expressions
- doctest: Testing framework
- nlohmann/json: JSON parsing