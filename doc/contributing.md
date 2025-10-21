# Contributing to gf

**[Getting Started](#getting-started)** • **[Project Structure](#project-structure)** • **[Testing](#testing)** • **[Code Style](#code-style)** • **[Architecture](#architecture-guidelines)** • **[Making Changes](#making-changes)** • **[Areas for Contribution](#areas-for-contribution)** • **[Getting Help](#getting-help)**

---

Thank you for your interest in contributing to gf! This document provides guidelines and information for contributors.

## Getting Started

gf is built with:
- **Language**: C++23 (requires clang++-18 or g++-13 or higher)
- **GUI Framework**: Custom [Luigi framework](luigi.md)
- **Build System**: CMake + Ninja
- **Platforms**: Linux (X11), Windows (Win32)

## Development Setup

### Building with Debug Symbols

```bash
cmake -Bbuild -DCMAKE_BUILD_TYPE=Debug -DCMAKE_CXX_COMPILER=clang++-18 -GNinja .
cmake --build build
```

### Building for Development

```bash
# Enable compile_commands.json for clangd/IDE integration
cmake -Bbuild \
  -DCMAKE_BUILD_TYPE=Debug \
  -DCMAKE_CXX_COMPILER=clang++-18 \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -GNinja .

cmake --build build
```

## Project Structure

```
gf/
├── src/
│   ├── gf.cpp, gf.hpp          # Main debugger frontend
│   ├── luigi.cpp, luigi.hpp    # GUI framework
│   └── re/                     # Regular expression engine
│       ├── re.hpp              # Core regex using CTRE
│       ├── cpp.cpp             # C++ code parsing
│       ├── gdb.cpp             # GDB output parsing
│       └── misc.cpp            # Utility functions
├── examples/
│   └── luigi_example.cpp       # Luigi GUI example
├── tests/                      # Unit tests (doctest framework)
│   ├── luigi_tests.cpp
│   ├── re_cpp_tests.cpp
│   └── re_gdb_tests.cpp
├── doc/                        # Documentation
│   ├── luigi.md                # Luigi GUI documentation
│   ├── plugins.md              # Plugin system guide
│   └── profiler_instructions.txt
└── deps/                       # Bundled dependencies
    ├── boost/                  # Boost.Mp11
    ├── ctre/                   # Compile-time regex
    ├── doctest/                # Testing framework
    └── nlohmann/               # JSON library
```

## Testing

### Running Tests

```bash
# Run all tests
./build/tests/luigi_tests
./build/tests/re_cpp_tests
./build/tests/re_gdb_tests
```

### Writing Tests

Tests use the [doctest](https://github.com/doctest/doctest) framework. Add tests to the appropriate file in `tests/`:

```cpp
#include <doctest/doctest.h>

TEST_CASE("My feature works correctly") {
    // Arrange
    auto value = calculate_something();

    // Assert
    CHECK(value == expected_value);
}
```

## Code Style

### General Guidelines

- Use **spaces for indentation** (existing codebase style)
- Braces on the same line: `if (condition) {`
- Modern C++23 features are encouraged
- Use `std::string_view` for string parameters when appropriate
- Prefer `auto` for type deduction where it improves readability

### Example

```cpp
void my_function(std::string_view name, int value) {
    if (value > 0) {
        auto result = process(name, value);
        // ...
    }
}
```

### Platform-Specific Code

Isolate platform-specific code with preprocessor guards:

```cpp
#ifdef UI_LINUX
    // Linux/X11 specific code
#elif defined(UI_WINDOWS)
    // Windows specific code
#endif
```

## Architecture Guidelines

### GDB Communication

All GDB communication must go through these functions:
- `DebuggerSend()` - Send commands to GDB (src/gf.cpp)
- `EvaluateExpression()` - Evaluate expressions in GDB context (src/gf.cpp)

### Window System

Windows are registered in the `interfaceWindows` array with:
- Create callback - Initialize window state
- Update callback - Handle messages and render

Example:

```cpp
InterfaceWindow my_window = {
    .label = "My Window",
    .create = MyWindowCreate,
    .update = MyWindowUpdate,
};
```

### Luigi UI Framework

See [luigi.md](luigi.md) for complete documentation on:
- Element hierarchy
- Message system
- Layout system
- Event handling

Key patterns:
- Use method chaining for element creation
- Call `refresh()` after state changes
- Handle messages by returning `1` if processed

## Making Changes

### Before Submitting

1. **Build and test** - Ensure the code compiles and all tests pass
2. **Test manually** - Run gf and verify your changes work as expected
3. **Check for regressions** - Test existing functionality still works
4. **Follow code style** - Match the existing codebase style

### Commit Messages

Write clear, concise commit messages:

```
Add support for custom breakpoint colors

- Add color configuration to breakpoint struct
- Update rendering code to use configured colors
- Add INI configuration options for breakpoint colors
```

### Pull Requests

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/my-feature`)
3. Make your changes
4. Commit with clear messages
5. Push to your fork
6. Open a pull request with:
   - Clear description of changes
   - Motivation/use case
   - Testing performed
   - Screenshots (for UI changes)

## Areas for Contribution

### High Priority

- **Documentation** - Improve user and developer documentation
- **Bug fixes** - Check the issue tracker for reported bugs
- **Testing** - Add tests for untested functionality
- **Platform support** - Improve Windows support

### Feature Ideas

- **New window types** - Add specialized debugging windows
- **GDB integration** - Enhance GDB command support
- **UI improvements** - Better themes, layout options
- **Python hooks** - More watch window customization examples
- **Editor integration** - Support for more editors beyond vim

### Luigi Framework

The Luigi GUI framework is a core component and welcomes:
- New UI elements
- Performance improvements
- Platform-specific enhancements
- Documentation and examples

See [luigi.md](luigi.md) and `examples/luigi_example.cpp` for details.

## Dependencies

### External (Must be installed)

**Linux:**
- X11 development libraries
- FreeType development libraries
- pthread

**Windows:**
- Native Win32 APIs (no external dependencies)

### Bundled (In deps/)

- **Boost.Mp11** - Template metaprogramming
- **CTRE** - Compile-time regular expressions
- **doctest** - Testing framework
- **nlohmann/json** - JSON parsing

These are header-only or single-file libraries included directly.

## Debugging gf Itself

### Using GDB on gf

```bash
# Build with debug symbols
cmake -Bbuild -DCMAKE_BUILD_TYPE=Debug -GNinja .
cmake --build build

# Run under GDB
gdb ./build/gf
```

### Logging

Add debug output to stderr:

```cpp
std::print(stderr, "Debug: value = {}\n", value);
```

### UI Debugging

Set `UI_DEBUG` to `1` in `src/luigi.hpp`:

```cpp
#define UI_DEBUG 1
```

This enables additional debug output for the UI system.

## Getting Help

- **Documentation**: See files in `doc/` directory
- **Examples**: Check `examples/luigi_example.cpp`
- **Issue Tracker**: Report bugs or ask questions on GitHub
- **Code Comments**: The codebase includes helpful comments