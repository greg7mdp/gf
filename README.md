# gf – A Modern GDB Frontend

[![Linux](https://github.com/greg7mdp/gf/actions/workflows/linux.yml/badge.svg)](https://github.com/greg7mdp/gf/actions/workflows/linux.yml)

**[Features](#features)** • **[Installation](#installation)** • **[Quick Start](#quick-start)** • **[Documentation](#documentation)** • **[Configuration](#configuration)** • **[Contributing](doc/contributing.md)** • **[Contributors](#contributors)**

---

gf is a lightweight, modern GDB frontend built with C++23, featuring an intuitive UI and powerful debugging capabilities. Built on the custom [Luigi GUI framework](doc/luigi.md), gf provides a responsive debugging experience for Linux.

## Features

### Core Debugging Features
- **Interactive Source View** - View and navigate source code with syntax highlighting
- **Watch Expressions** - Monitor variables with format specifiers (hex, decimal, etc.)
- **Stack Navigation** - Browse call stack and switch between frames
- **Breakpoint Management** - Set, disable, and manage breakpoints visually
- **Register View** - Inspect CPU registers in real-time
- **Memory Window** - View and edit raw memory with various display formats
- **Disassembly View** - Step through assembly with instruction-level debugging

### Advanced Features
- **Line Inspect Mode** - Press backtick (`) to evaluate all expressions on current line
- **Reverse Debugging** - Full support for rr (record/replay) with reverse continue/step
- **Custom Watch Hooks** - Python integration for custom type visualization
- **Control Pipe** - Text editor integration via named pipe commands
- **Log Window** - Real-time application logging via pipe
- **Embedded Profiler** - Tracing profiler with flame graph visualization
- **Waveform Viewer** - Signal visualization for embedded/hardware debugging

### Power User Features
- **Smart Click Actions**
  - `Ctrl+Click` a line → run until that line
  - `Shift+Click` a line → skip to it without executing
  - Double-click → select expression for inspection
- **Keyboard Shortcuts** - Full keyboard navigation (F5-F11, Ctrl+shortcuts)
- **Tab Completion** - Auto-complete GDB commands and symbols
- **Custom Commands** - Define preset command sequences
- **Flexible Layout** - Configure window layout via INI file
- **Theme Support** - Customize colors and appearance

## Installation

### Download Pre-built Binary (Linux)

Pre-built portable binaries are available for each release:

```bash
# Download latest release
wget https://github.com/greg7mdp/gf/releases/latest/download/gf-VERSION-linux-x86_64.tar.gz

# Extract
tar -xzf gf-VERSION-linux-x86_64.tar.gz

# Run
./gf
```

**Requirements:** Linux with glibc 2.27+ (Ubuntu 18.04+, Debian 10+, etc.), X11, and FreeType installed.

### Build from Source

#### Prerequisites
- C++23 compiler: `clang++-18` or `g++-13` or higher
- CMake 3.15+
- GDB (15.2+ recommended for best C++ expression evaluation)
- Linux: X11, FreeType
- Windows: Native Win32

#### Building

```bash
# Clone the repository
git clone https://github.com/greg7mdp/gf.git
cd gf

# Configure with CMake
cmake -Bbuild -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=clang++-18 -GNinja .

# Build
cmake --build build

# Run
./build/gf
```

### CMake Options

| Option | Values | Description |
|--------|--------|-------------|
| `CMAKE_BUILD_TYPE` | `Release`, `Debug`, `RelWithDebInfo` | Build configuration |
| `CMAKE_CXX_COMPILER` | `clang++-18`, `g++-13`, etc. | C++ compiler (must support C++23) |
| `CMAKE_EXPORT_COMPILE_COMMANDS` | `ON`, `OFF` | Generate compile_commands.json for clangd |

## Quick Start

### First Run

```bash
# Run gf
./build/gf

# if your program `./myprog` crashed and generated a core file
./build/gf ./myprog <core_dump_file>

# to attach to a running instance
./build/gf ./myprog <pid>

# Or with command-line arguments forwarded to GDB
./build/gf --args ./myprog arg1 arg2
```

Running `gf` with `--args` is quite useful as it will load the correct config file from the `.gf` directory, which may contain additional setup commands. If you have run the same program before, no need to provide the arguments again, they will be loaded from the program `.ini` file in the `.gf` directory.


### Essential Shortcuts

| Action | Shortcut | Description |
|--------|----------|-------------|
| **Continue** | `F5` | Resume execution |
| **Step Over** | `F10` | Execute next line without entering functions |
| **Step In** | `F11` | Step into function calls |
| **Step Out** | `Shift+F11` | Finish current function |
| **Toggle Breakpoint** | `F9` | Add/remove breakpoint at current line |
| **Break** | `F8` | Interrupt running program |
| **Run Until Line** | `Ctrl+Click` | Continue execution to clicked line |
| **Jump to Line** | `Shift+Click` | Skip to line without executing |
| **Inspect Line** | `` ` `` (backtick) | Evaluate all expressions on current line |

### Using with rr (Record/Replay)

```bash
# Record your program
rr record ./your_program

# Replay in gf
gf --rr-replay
```

Use `Ctrl+Shift+F5/F10/F11` for reverse continue/step.

### Quick Tips
- Press `Ctrl+Shift+P` to sync working directory with GDB after starting your executable
- Use `Tab` for auto-completion while typing watch expressions or GDB commands
- Press `/` in watch window to change format specifier (e.g., `/x` for hexadecimal)
- Right-click for context menus with additional options
- Add recommended GDB settings to `~/.gdbinit`:
  ```
  set breakpoint pending on
  set disassembly-flavor intel
  ```

## Documentation

### User Guides
- **[Quick Tips](https://github.com/greg7mdp/gf#tips)** - Essential keyboard shortcuts and features
- **[Configuration Guide](https://github.com/greg7mdp/gf#configuration)** - Customize gf to your workflow
- **[Special Commands](https://github.com/greg7mdp/gf#special-commands)** - gf-specific GDB commands
- **[Watch Hooks](https://github.com/greg7mdp/gf#watch-window-hooks)** - Python integration for custom types

### Developer Documentation
- **[Luigi GUI Framework](doc/luigi.md)** - Documentation for the UI framework powering gf
- **[Plugin System](doc/plugins.md)** - Extend gf with custom windows and commands
- **[Profiler Guide](doc/profiler_instructions.txt)** - Using the embedded profiler

### External Resources
- **[GDB Tutorial](https://handmade.network/forums/articles/t/2883-gdb)** - Introduction to GDB debugging
- **[rr Project](https://rr-project.org/)** - Record and replay framework

## Screenshots

![Main debugging interface](https://raw.githubusercontent.com/nakst/cdn/main/unknown2.png)
*Main debugging interface showing source view, watch window, and stack*

![Memory window and extended watch view](https://raw.githubusercontent.com/nakst/cdn/main/%20memory%20window%20and%20extended%20view%20window.png)
*Memory window and extended watch expression view*

## Configuration

gf loads configuration from two files on startup (in order):
1. `~/.config/gf_config.ini` - Global user settings
2. `.gf/gf_config.ini` - Project-specific settings, with the `.gf` directory located either in current directory, or one directory up if the current directory starts with `build`. Project-specific settings override Global settings.

### Basic Configuration

```ini
[ui]
scale=1.5
font_path=/usr/share/fonts/TTF/DejaVuSansMono.ttf
font_size_code=14
window_width=1920
window_height=1080
maximize=1

[gdb]
path=/usr/local/bin/gdb
log_all_output=1

[shortcuts]
Ctrl+I=print i
F12=continue
```

### Layout Customization

Configure window layout with horizontal (`h`) and vertical (`v`) splits:

```ini
[ui]
layout=h(75,v(75,Source,Console),v(50,t(Watch,Breakpoints),Stack))
```

- `h(position,left,right)` - Horizontal split
- `v(position,top,bottom)` - Vertical split
- `t(tab1,tab2,...)` - Tab pane

### Custom Keyboard Shortcuts

```ini
[shortcuts]
Ctrl+I=print i
Ctrl+Shift+F10=reverse-next
Ctrl+Shift+F11=reverse-step
F12=continue
```

You can use any GDB command or gf special commands.

### Preset Commands

Create quick-access command buttons:

```ini
[commands]
Compile=shell gcc -o bin/app src/main.c
Run=file bin/app;run&
Debug Tests=file bin/tests;b main;run test_suite&
```

Separate commands with `;`, use `&` at the end to run asynchronously.

### Themes

Customize colors in the `[theme]` section. See [theme examples](https://github.com/nakst/gf/wiki/Themes).

### GDB Configuration

```ini
[gdb]
# Path to GDB executable
path=/usr/local/bin/gdb

# Pass arguments to GDB
arguments=-nx -ex record

# Log all GDB output to Log window
log_all_output=1

# Disable confirmation dialogs
confirm_command_kill=0
confirm_command_connect=0

# Stack frame limit
backtrace_count_limit=50
```

### Text Editor Integration

Configure control pipe for editor integration:

```ini
[pipe]
control=/tmp/gf_control.pipe
log=/tmp/gf_log.pipe
```

Send commands to the pipe:

```bash
# Jump to file and line
echo "f /path/to/file.cpp" > /tmp/gf_control.pipe
echo "l 123" > /tmp/gf_control.pipe

# Execute GDB command
echo "c file myapp" > /tmp/gf_control.pipe
```

### Vim Integration

```ini
[vim]
server_name=GVIMServer
```

Use `F2` in gf to sync with gvim.

## Special Commands

gf provides custom GDB commands:

| Command | Description |
|---------|-------------|
| `gf-step` | Step line or instruction (context-aware) |
| `gf-next` | Step over line or instruction (context-aware) |
| `gf-step-out-of-block` | Step to next line after closing `}` |
| `gf-restart-gdb` | Restart GDB process (loses state) |
| `gf-get-pwd` | Sync working directory from executable |
| `gf-switch-to <window>` | Switch to named window |
| `gf-command <name>` | Run preset command from `[commands]` section |
| `gf-inspect-line` | Toggle line inspect mode (bound to backtick) |

## Watch Window Hooks

Extend the watch window with Python to customize display of complex types:

```python
def RectangleHook(item, field):
    if field:
        if field == '[width]':
            return gdb.Value(int(item['right']) - item['left'])
        if field == '[height]':
            return gdb.Value(int(item['bottom']) - item['top'])
    else:
        print('[width]')
        print('[height]')
        _gf_fields_recurse(item)

gf_hooks = { 'Rectangle': RectangleHook }
```

### Dynamic Arrays

```python
def MyArrayHook(item, field):
    if field:
        return item['items'][int(field[1:-1])]
    else:
        print('(d_arr)', int(item['length']))
```

Template parameters are removed from type names, so `Array<int>` uses the `Array` hook.

## Contributing

See **[CONTRIBUTING.md](doc/contributing.md)** for details on how to contribute to gf.


## Contributors

gf was originally created by `nakst` (see https://github.com/nakst/gf). Without him, this current repo wouldn't exist. Much of that original version still lives in this repo. Beside its creator, `nakst`, it also was improved by the contributors listed below:

```
Philippe Mongeau (phmongeau)
Jimmy "Keeba" Lefevre (JimmyLefevre)
John Blat (johnblat64)
IWouldRatherUsePasteBin
Gavin Beatty (gavinbeatty)
Michael Stopa (StomyPX)
Anders Kaare (sqaxomonophonen)
Arseniy Khvorov (khvorov45)
```

I (`Gregory Popovitch, @greg7mdp`) found `gf` quite useful, but felt like fixing some issues that bothered me in my daily use. The more I hacked at `gf`, the more I was amazed as what it achieved to implement in so few lines of code, and the more fun I had with this process. 

Eventually I decided to update the code to a more typical C++ implementation, even at the cost of making a few extra string copies if needed. Still, I attempted to stay as close to the metal as possible, in order to retain gf's impressive responsiveness. This is the code you can see in my current repository.

Thank you for contributing to gf!

## License

Check the repository for license information.

---

**Built with ❤️ using C++23 and the Luigi GUI framework**
