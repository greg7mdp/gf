## Plugins

There is a simple plugin system. Make a file called `plugins.cpp` in the source code folder. 
It will be found automatically, and #included in the compilation of the main translation unit.

gf uses the [Luigi](https://github.com/greg7mdp/gf/blob/main/doc/luigi.md) UI library.

You can register new windows, command and data viewers in a constructor function. For example,

```cpp
__attribute__((constructor)) 
void MyPluginRegister() {
    _interface_windows["Hello"]   = {._create = MyPluginHelloWindowCreate, // should create an instance of the window.
                                     ._update = MyPluginHelloWindowUpdate  // called every time the target pauses/steps
                                     };

	_interface_data_viewers.push_back({"Add waveform...",    // The label of the button to show in the Data tab.
                                       WaveformAddDialog});  // The callback to create the data viewer.

    _interface_commands.push_back({
      ._label = "My command",                                // The label to show in the application menu.
      ._shortcut{.code = UI_KEYCODE_LETTER('V'), .ctrl = true, .shift = true, .invoke = []() {
                                       // do something here
                                       return true;
                                    }}
    });
}
```

The interface window creation callback is passed the parent UIElement and should return the UIElement it creates.

```cpp
UIElement *MyPluginHelloWindowCreate(UIElement *parent) {
	UIPanel &panel = parent->add_panel(UIPanel::GRAY || UIPanel::EXPAND);
    panel.add_label(0, "Hello, world!");
	return &panel;
}
```

The interface window update callback is passed the output of GDB from the most recent step, 
and the UIElement returned by the creation callback.

```cpp
void MyPluginHelloWindowUpdate(const char *gdbOutput, UIElement *element) {
	// TODO Update the window.
}
```

The interface data viewer creation callback should create a MDI child of the data tab as follows:

```cpp
void MyPluginTestViewerCreate(void *unused) {
	UIMDIChild &window = dataWindow->add_mdichild(UIMDIChild::CLOSE_BUTTON, ui_rect_1(0), "Title");
	// TODO Configure the viewer.
	dataWindow->refresh();
}
```

For communicating with GDB, there are the following member functions of the `Context` class (use `ctx` object).

```cpp
// Evaluate an expression. The result is overwritten between calls!
std::string eval_expression(string_view expression, string_view format = {});

// Send and run a command in GDB. Set `echo` to log the command in the console window. 
// If `synchronous` is set the function will wait for the command to complete before it returns.
std::optional<std::string> send_command_to_debugger(string_view command, bool echo, bool synchronous);
```

There are many examples of how to do these things in `gf.cpp`.