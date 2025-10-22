# Luigi GUI Framework

**[Introduction](#introduction)** • **[Getting Started](#getting-started)** • **[Core Concepts](#core-concepts)** • **[UI Elements](#ui-elements)** • **[Layout System](#layout-system)** • **[Event Handling](#event-handling)** • **[Examples](#examples)** • **[Advanced Topics](#advanced-topics)**

---

## Introduction

Luigi is a lightweight, modern C++ GUI framework that provides a simple yet powerful API for building desktop applications. It features a retained-mode element hierarchy with immediate-mode style APIs and supports both Linux (X11) and Windows platforms.

The debugger frontend [gf](https://github.com/greg7mdp/gf) is built with `luigi`.

Also, to get an idea of how luigi works, you can check out the [example](https://github.com/greg7mdp/gf/blob/main/examples/luigi_example.cpp).

Luigi example app screenshot:
![Screenshot luigi example application.](https://github.com/greg7mdp/gf/blob/main/img/luigi_example.png)

`luigi` consists of two files, `luigi.hpp` and `luigi.cpp`. To integrate directly into your application, you can #include `luigi.cpp` in one file, in `luigi.hpp` in any other file that needs to make gui calls.


## Getting Started

### Basic Application Structure

Every Luigi application follows this basic pattern:

```cpp
#include <luigi.hpp>

int main(int argc, char** argv) {
    // 1. Create UI configuration
    UIConfig cfg;
    auto ui_ptr = UI::initialise(cfg);

    if (!ui_ptr)
        return 1;

    // 2. Create main window
    UIWindow& window = ui_ptr->create_window(0, 0, "My Application", 0, 0);

    // 3. Add UI elements to window
    window.add_panel(UIPanel::COLOR_1)
          .add_button(0, "Click Me")
          .on_click([](UIButton& btn) {
              std::print("Button clicked!\n");
          });

    // 4. Run message loop
    return ui_ptr->message_loop();
}
```

### Custom Fonts (Optional)

Luigi supports custom fonts via FreeType. You can set the font path in the UIConfig before initialization:

```cpp
UIConfig cfg;

// Optional: set custom font if present
std::string home = getenv("HOME");
std::string fontPath = home + "/.fonts/FiraCode-Regular.ttf";
if (fs::exists(fontPath))
    cfg.font_path = fontPath;

auto ui_ptr = UI::initialise(cfg);
```

## Core Concepts

### Element Hierarchy

Luigi uses a tree structure where every UI element has:
- A **parent** element (except the root window)
- Zero or more **child** elements
- A **bounds** rectangle defining its position and size
- A **window** pointer to the top-level window

### Message System

All UI communication happens through messages (`UIMessage` enum):

```cpp
// Common messages:
UIMessage::PAINT              // Draw the element
UIMessage::LAYOUT             // Calculate size and position
UIMessage::LEFT_DOWN          // Mouse left button pressed
UIMessage::KEY_TYPED          // Keyboard key pressed
UIMessage::UPDATE             // Element state changed
UIMessage::VALUE_CHANGED      // Value changed (sliders, checkboxes)
```

### Flags

Elements are configured using bitwise flags:

```cpp
// Common flags:
UIElement::h_fill        // Fill horizontal space
UIElement::v_fill        // Fill vertical space
UIElement::hv_fill       // Fill both directions
UIElement::disabled_flag // Disable user interaction
```

### CRTP Pattern and Method Chaining

Luigi uses the CRTP (Curiously Recurring Template Pattern) via `UIElementCast<T>` to enable fluent method chaining that returns the correct derived type:

```cpp
UIButton& btn = panel.add_button(0, "Hello")
    .set_label("New Label")
    .on_click([](UIButton& b) { /* handler */ })
    .focus();  // All methods return UIButton&, not UIElement&
```

## UI Elements

### UIWindow

The top-level container for all UI elements.

```cpp
// Create window
UIWindow& window = ui_ptr->create_window(
    flags,           // Window flags (MENU, MAXIMIZE, etc.)
    rect,            // Initial rectangle (or 0 for default)
    "Window Title",  // Title bar text
    width,           // Width (or 0 for default)
    height           // Height (or 0 for default)
);

// Register keyboard shortcuts
window.register_shortcut(UIShortcut{
    .code = UI_KEYCODE_LETTER('T'),
    .ctrl = true,
    .invoke = []() {
        std::print("Ctrl+T pressed\n");
        return true;  // Return true if handled
    }
});

// Access clipboard
window.write_clipboard_text("Hello", sel_target_t::clipboard);
std::string text = window.read_clipboard_text(sel_target_t::clipboard);
```

### UIPanel

Container for organizing child elements vertically or horizontally.

```cpp
UIPanel& panel = parent.add_panel(
    UIPanel::COLOR_1 |      // Background color style 1
    UIPanel::HORIZONTAL |   // Lay out children horizontally
    UIPanel::MEDIUM_SPACING // Add medium spacing between children
);

// Configure border and gap
panel.set_border(ui_rect_1(10))  // 10px border on all sides
     .set_gap(5);                 // 5px gap between children

// Common panel flags:
// - COLOR_1, COLOR_2: Background colors
// - HORIZONTAL: Horizontal layout (default is vertical)
// - SMALL_SPACING, MEDIUM_SPACING, LARGE_SPACING
// - SCROLL: Add scrollbar if content overflows
// - EXPAND: Expand to fill available space
```

### UIButton

Clickable button with label and callback.

```cpp
UIButton& button = parent.add_button(
    0,                // Flags (SMALL, MENU_ITEM, CAN_FOCUS, etc.)
    "Button Label"    // Button text
);

button.on_click([](UIButton& btn) {
    std::print("Clicked: {}\n", btn.label());
    btn.set_label("Clicked!");
    btn.refresh();  // Redraw button
});

// Button flags:
// - UIButton::SMALL: Smaller button
// - UIButton::MENU_ITEM: Styled as menu item
// - UIButton::CAN_FOCUS: Can receive keyboard focus
// - UIButton::DROP_DOWN: Show dropdown indicator
// - UIButton::CHECKED: Show as checked/selected
```

### UICheckbox

Checkbox with label and checked state.

```cpp
bool my_option = false;

UICheckbox& checkbox = parent.add_checkbox(0, "Enable Feature");

checkbox.on_click([](UICheckbox& cb) {
    std::print("Checked: {}\n", cb.checked());
    cb.set_label(cb.checked() ? "Enabled" : "Disabled");
});

// Track an external bool variable
checkbox.track(&my_option);  // Updates my_option when toggled

// Set/get state
checkbox.set_checked(true);
bool is_checked = checkbox.checked();
```

### UILabel

Static text label.

```cpp
UILabel& label = parent.add_label(
    UIElement::h_fill,  // Fill horizontal space
    "Hello, World!"     // Label text
);

label.set_label("New text");
std::string_view text = label.label();
```

### UITextbox

Single-line text input field.

```cpp
UITextbox& textbox = parent.add_textbox(0);

// Get text content
std::string_view text = textbox.text();

// Replace text
textbox.replace_text("New text", true);  // true = send VALUE_CHANGED message

// Clear textbox
textbox.clear(false);  // false = don't send VALUE_CHANGED message

// Text operations
textbox.select_all();
textbox.copy();
textbox.paste(sel_target_t::clipboard);
textbox.undo();
textbox.redo();

// Handle up/down arrow keys
textbox.on_key_up_down([](UITextbox& tb, UIKeycode code) {
    if (code == UIKeycode::UP) {
        std::print("Up pressed\n");
        return true;  // Handled
    }
    return false;  // Not handled
});
```

### UICode

Multi-line code/text editor with syntax highlighting support.

```cpp
UICode& code = parent.add_code(
    UICode::NO_MARGIN |   // Don't show line number margin
    UICode::SELECTABLE    // Allow text selection
);

// Load file
code.load_file("/path/to/file.cpp");

// Insert/replace content
code.insert_content("int main() {\n    return 0;\n}\n", false);

// Clear content
code.clear();

// Line operations
size_t num_lines = code.num_lines();
std::string_view line = code.line(5);  // Get line 5 (0-indexed)

// Current line (highlighted line)
code.set_current_line(10);
std::optional<size_t> current = code.current_line();

// Focus line (scroll to this line)
code.set_focus_line(20);

// Selection
code.set_selection(2, line, offset);  // index 2 = anchor
code.set_selection(3, line, offset);  // index 3 = caret
code.update_selection();
std::string_view selected = code.selection();

// Add custom context menu items for selected text
code.add_selection_menu_item("To Upper", [](std::string_view sel) {
    // Handle menu item click
});
```

### UISlider

Horizontal or vertical slider control.

```cpp
UISlider& slider = parent.add_slider(
    UIElement::vertical_flag  // Make it vertical (default is horizontal)
);

slider.set_position(0.5);  // Set to 50%
double pos = slider.position();  // Get position (0.0 to 1.0)

// Add callback for value changes
slider.on_value_changed([](UISlider& s) {
    std::print("Slider value: {}\n", s.position());
});

// Discrete steps
slider.set_steps(10);  // Snap to 10 discrete positions
```

### UIGauge

Progress bar or level indicator.

```cpp
UIGauge& gauge = parent.add_gauge(
    UIElement::vertical_flag  // Vertical gauge
);

gauge.set_position(0.75);  // Set to 75%
double pos = gauge.position();
```

### UITable

Table/list view with columns and rows.

```cpp
int selected_row = -1;

UITable& table = parent.add_table(
    0,                          // Flags
    "Name\tAge\tCity"          // Column headers (tab-separated)
);

table.set_num_items(1000);  // Set number of rows

// Provide row data on-demand
table.on_getitem([](UITable& tbl, UITableGetItem& m) -> int {
    if (m._column == 0) {
        return m.format_to("Item {}", m._row);
    } else if (m._column == 1) {
        return m.format_to("{}", m._row * 10);
    } else {
        return m.format_to("Data {}", m._row);
    }
});

// Handle clicks
table.on_click([&selected_row](UITable& tbl) {
    int hit = tbl.hittest(tbl.cursor_pos());
    if (selected_row != hit) {
        selected_row = hit;
        tbl.ensure_visible(selected_row);  // Scroll to row
        tbl.repaint(nullptr);
    }
});

// Adjust column widths
table.resize_columns();
```

### UISplitPane

Resizable split container for two child elements.

```cpp
// Vertical split (top/bottom)
UISplitPane& vsplit = parent.add_splitpane(
    UIElement::vertical_flag,  // Split direction
    0.75f                      // Initial weight (75% top, 25% bottom)
);

// Add children to the split pane
vsplit.add_panel(UIPanel::COLOR_1);  // Top/left child
vsplit.add_panel(UIPanel::COLOR_2);  // Bottom/right child

// Horizontal split (left/right)
UISplitPane& hsplit = parent.add_splitpane(
    0,      // Horizontal split
    0.3f    // 30% left, 70% right
);
```

### UITabPane

Tabbed container for multiple pages.

```cpp
UITabPane& tabs = parent.add_tabpane(
    0,                           // Flags
    "Tab 1\tTab 2\tTab 3"       // Tab labels (tab-separated)
);

// Add content to each tab (in order)
tabs.add_panel(UIPanel::COLOR_1).add_label(0, "Content for Tab 1");
tabs.add_panel(UIPanel::COLOR_1).add_label(0, "Content for Tab 2");
tabs.add_panel(UIPanel::COLOR_1).add_label(0, "Content for Tab 3");

// Access/change active tab
tabs.set_active(1);  // Switch to Tab 2 (0-indexed)
uint32_t active = tabs.get_active();
```

### UIMenu

Context menu or popup menu.

```cpp
UIMenu& menu = ui_ptr->create_menu(&parent_element, 0);

menu.add_item(0, "Copy\tCtrl+C", [](UIButton&) {
        std::print("Copy selected\n");
    })
    .add_item(0, "Paste\tCtrl+V", [](UIButton&) {
        std::print("Paste selected\n");
    })
    .add_item(0, "Delete\tDel", [](UIButton&) {
        std::print("Delete selected\n");
    });

menu.show();  // Display the menu

// Menu flags:
// - UIMenu::PLACE_ABOVE: Place menu above parent
// - UIMenu::NO_SCROLL: Don't add scrollbar for long menus
```

### UIMDIClient and UIMDIChild

Multiple Document Interface (MDI) for floating child windows.

```cpp
// Create MDI client area
UIMDIClient& client = window.add_mdiclient(0);

// Add child windows
client.add_mdichild(
    UIMDIChild::CLOSE_BUTTON,           // Show close button
    UIRectangle(10, 400, 10, 300),     // Initial bounds (l, r, t, b)
    "Document 1"                        // Window title
).add_panel(UIPanel::COLOR_1)
 .add_label(0, "Content here");

client.add_mdichild(
    UIMDIChild::CLOSE_BUTTON,
    UIRectangle(50, 450, 50, 350),
    "Document 2"
).add_panel(UIPanel::COLOR_1)
 .add_label(0, "More content");
```

### UISpacer

Empty space element for layout control.

```cpp
// Add fixed-size spacer
UISpacer& spacer = parent.add_spacer(
    0,      // Flags
    20,     // Width in pixels
    10      // Height in pixels
);
```

### UIScrollBar

Scrollbar control (usually created automatically by scrollable containers).

```cpp
UIScrollBar& scrollbar = parent.add_scrollbar(
    UIScrollBar::HORIZONTAL  // Or 0 for vertical
);

scrollbar.set_maximum(1000);  // Total scrollable range
scrollbar.set_page(100);      // Visible page size

int64_t pos = scrollbar.position();  // Get scroll position
scrollbar.position() = 500;          // Set scroll position
```

## Layout System

### Understanding Layout

Luigi uses a two-pass layout system:

1. **Measure Pass**: Elements report their preferred size
2. **Arrange Pass**: Parents assign actual bounds to children

### Fill Flags

Elements can request to fill available space. This is crucial for proper layout - elements without fill flags will use their minimum size:

```cpp
// Fill horizontally
element.set_flag(UIElement::h_fill);

// Fill vertically
element.set_flag(UIElement::v_fill);

// Fill both directions (most commonly used)
element.set_flag(UIElement::hv_fill);
```

**Important**: Many container elements (like `UISplitPane`, `UITabPane`, `UIPanel`) and content elements (like `UICode`, `UITable`) typically need `UIElement::hv_fill` to be visible and properly sized. Always use fill flags when you want elements to expand to use available space.

### Panel Layout

Panels lay out children in sequence (vertically by default, or horizontally with the `HORIZONTAL` flag):

```cpp
UIPanel& panel = parent.add_panel(UIPanel::COLOR_1 | UIPanel::HORIZONTAL);

// Elements are laid out left-to-right in a horizontal panel
panel.add_button(0, "Button 1");   // Left
panel.add_button(0, "Button 2");   // Middle
panel.add_button(0, "Button 3");   // Right
```

### Border and Gap

Control spacing within panels:

```cpp
// Set border (padding around all edges)
panel.set_border(UIRectangle(10, 10, 10, 10));  // left, right, top, bottom
panel.set_border(ui_rect_1(10));                // 10px on all sides

// Set gap between children
panel.set_gap(5);  // 5px between each child

// Or use predefined spacing flags:
// - UIPanel::SMALL_SPACING
// - UIPanel::MEDIUM_SPACING
// - UIPanel::LARGE_SPACING
```

### Nested Layouts

Create complex layouts by nesting panels:

```cpp
// Main vertical panel
UIPanel& main = window.add_panel(UIPanel::COLOR_1);

// Top section
main.add_label(0, "Header");

// Middle section: horizontal panel with two columns
UIPanel& middle = main.add_panel(UIPanel::HORIZONTAL);
middle.add_panel(UIPanel::COLOR_2).add_label(0, "Left");
middle.add_panel(UIPanel::COLOR_2).add_label(0, "Right");

// Bottom section
main.add_button(0, "OK");
```

### Helper Functions for UIRectangle

```cpp
ui_rect_1(x)           // {x, x, x, x}
ui_rect_1i(x)          // {x, -x, x, -x}
ui_rect_2i(x, y)       // {x, -x, y, -y}
ui_rect_2s(x, y)       // {0, x, 0, y}
ui_rect_4pd(x,y,w,h)   // {x, x+w, y, y+h} - point+dimensions
```

## Event Handling

### Mouse Events

```cpp
element.set_user_proc([](UIElement* el, UIMessage msg, int di, void* dp) {
    if (msg == UIMessage::LEFT_DOWN) {
        std::print("Left click at ({}, {})\n",
                   el->cursor_pos().x, el->cursor_pos().y);
        return 1;  // Return 1 if handled
    }
    return 0;
});

// Mouse messages:
// - LEFT_DOWN, MIDDLE_DOWN, RIGHT_DOWN
// - LEFT_UP, MIDDLE_UP, RIGHT_UP
// - LEFT_DBLCLICK, MIDDLE_DBLCLICK, RIGHT_DBLCLICK
// - MOUSE_MOVE, MOUSE_DRAG
// - MOUSE_WHEEL (di = delta)
```

### Keyboard Events

```cpp
element.set_user_proc([](UIElement* el, UIMessage msg, int di, void* dp) {
    if (msg == UIMessage::KEY_TYPED) {
        auto* m = static_cast<UIKeyTyped*>(dp);

        if (m->code == UIKeycode::ENTER) {
            std::print("Enter pressed\n");
            return 1;
        }

        // Check modifier keys
        if (el->is_ctrl_on() && m->code == UI_KEYCODE_LETTER('S')) {
            std::print("Ctrl+S pressed\n");
            return 1;
        }

        // Check typed text
        if (!m->text.empty()) {
            std::print("Typed: {}\n", m->text);
        }
    }
    return 0;
});

// Common keycodes:
// - UIKeycode::ENTER, ESCAPE, TAB, BACKSPACE, DEL
// - UIKeycode::UP, DOWN, LEFT, RIGHT
// - UIKeycode::HOME, END, PAGE_UP, PAGE_DOWN
// - UIKeycode::F1 through F12
// - UI_KEYCODE_LETTER('A') through UI_KEYCODE_LETTER('Z')
// - UI_KEYCODE_DIGIT('0') through UI_KEYCODE_DIGIT('9')
```

### Modifier Key Checking

```cpp
// Check individual modifiers
if (element.is_ctrl_on()) { /* Ctrl is pressed */ }
if (element.is_shift_on()) { /* Shift is pressed */ }
if (element.is_alt_on()) { /* Alt is pressed */ }

// Check if any modifier is pressed
if (element.is_modifier_on()) { /* Any modifier */ }

// Check only one modifier (no others)
if (element.is_only_ctrl_on()) { /* Only Ctrl, no Shift/Alt */ }
if (element.is_only_shift_on()) { /* Only Shift */ }
if (element.is_only_alt_on()) { /* Only Alt */ }
```

### Custom Message Handlers

Elements can have custom message handlers:

```cpp
int MyElementMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
    if (msg == UIMessage::PAINT) {
        auto* painter = static_cast<UIPainter*>(dp);
        // Custom painting code
        return 1;
    }
    return 0;
}

element.set_user_proc(MyElementMessageProc);
```

### Element State Queries

```cpp
// Check element state
if (element.is_hovered()) { /* Mouse is over element */ }
if (element.is_focused()) { /* Element has keyboard focus */ }
if (element.is_pressed()) { /* Mouse button pressed on element */ }
if (element.is_disabled()) { /* Element is disabled */ }

// Check which button is pressed
int button = element.pressed_button();  // 1=left, 2=middle, 3=right
```

## Examples

### Example 1: Simple Button Counter

```cpp
int counter = 0;
UILabel* label = nullptr;

UIPanel& panel = window.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING);

label = &panel.add_label(0, "Count: 0");

panel.add_button(0, "Increment").on_click([&counter](UIButton&) {
    counter++;
    label->set_label(std::format("Count: {}", counter));
    label->refresh();
});

panel.add_button(0, "Reset").on_click([&counter](UIButton&) {
    counter = 0;
    label->set_label("Count: 0");
    label->refresh();
});
```

### Example 2: Slider Controlling Gauges

```cpp
UIGauge* gauge1 = nullptr;
UIGauge* gauge2 = nullptr;

UIPanel& panel = window.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING);

gauge1 = &panel.add_gauge(0);
gauge2 = &panel.add_gauge(0);

UISlider& slider = panel.add_slider(0);
slider.on_value_changed([gauge1, gauge2](UISlider& s) {
    gauge1->set_position(s.position());
    gauge2->set_position(s.position() * 0.5);  // Half speed
});

slider.set_position(0.5);  // Initial position
```

### Example 3: Text Editor with File Loading

```cpp
UIPanel& panel = window.add_panel(UIPanel::COLOR_1 | UIPanel::EXPAND | UIElement::hv_fill);

UIPanel& toolbar = panel.add_panel(UIPanel::HORIZONTAL | UIPanel::MEDIUM_SPACING)
                        .set_border(UIRectangle(5))
                        .set_gap(5);

UITextbox& pathbox = toolbar.add_textbox(0).replace_text("../src/luigi.hpp", false);

UICode& code = panel.add_code(UIElement::hv_fill | UICode::SELECTABLE)
                    .insert_content("// Text Display Example\n// Click 'Load' to display a file\n\n", false);

toolbar.add_button(0, "Load").on_click([&pathbox, &code](UIButton&) {
    code.load_file(std::string{pathbox.text()});
    code.refresh();
});

toolbar.add_button(0, "Clear").on_click([&code](UIButton&) {
    code.clear();
    code.refresh();
});
```

### Example 4: Split Pane Layout

```cpp
// Vertical split (top 75%, bottom 25%)
UISplitPane& vsplit = window.add_splitpane(UIElement::vertical_flag | UIElement::hv_fill, 0.75f);

// Top: horizontal split (left 30%, right 70%)
UISplitPane& hsplit = vsplit.add_splitpane(0, 0.3f);

// Top-right: code view
UICode& code = hsplit.add_code(0).insert_content(
   "// This is the split pane example\n// Top-right section with code\n\nint main() {\n    return 0;\n}\n", false);

// Bottom: console output
UICode& console = vsplit.add_code(UICode::NO_MARGIN)
                     .insert_content("Console output:\nClick buttons on the left to see output here\n\n", false);

// Top-left: buttons (created after console so we can capture it)
UIPanel& left = hsplit.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING);

left.add_button(0, "Button 1").on_click([&console](UIButton&) {
    console.insert_content("Button 1 clicked!\n", false);
    console.refresh();
});

left.add_button(0, "Button 2").on_click([&console](UIButton&) {
    console.insert_content("Button 2 clicked!\n", false);
    console.refresh();
});

left.add_button(0, "Button 3").on_click([&console](UIButton&) {
    console.insert_content("Button 3 clicked!\n", false);
    console.refresh();
});
```

### Example 5: Tabbed Interface

```cpp
// This example shows how to create a tabbed interface
UIPanel& panel = window.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING | UIElement::hv_fill);

panel.add_label(0, "This tab demonstrates the UITabPane element:");

UITabPane& tabs = panel.add_tabpane(UIElement::hv_fill, "Editor\tSettings\tAbout");

// Tab 1: Editor
UICode& editor = tabs.add_code(UICode::SELECTABLE);
editor.insert_content("// Example code editor\n#include <iostream>\n\nint main() {\n    std::cout << \"Hello from tabbed interface!\\n\";\n    return 0;\n}\n", false);

// Tab 2: Settings
UIPanel& settings = tabs.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING);
settings.add_checkbox(0, "Enable auto-save");
settings.add_checkbox(0, "Show line numbers");
settings.add_checkbox(0, "Dark theme");

// Tab 3: About
tabs.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING)
    .add_label(0, "Luigi GUI Framework v2.0\n\nA lightweight, modern C++ GUI framework");
```

### Example 6: Custom Context Menu

```cpp
UIPanel& panel = window.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING | UIElement::hv_fill);

panel.add_label(0, "Select text below and right-click to see custom context menu:");

UICode& code = panel.add_code(UICode::SELECTABLE | UIElement::hv_fill);

// Add custom menu items for selected text
code.add_selection_menu_item("Convert to UPPERCASE", [](std::string_view sel) {
    std::string upper{sel};
    std::transform(upper.begin(), upper.end(), upper.begin(), ::toupper);
    std::print("Uppercase: {}\n", upper);
});

code.add_selection_menu_item("Count Characters", [](std::string_view sel) {
    std::print("Character count: {}\n", sel.size());
});

code.insert_content("Select this text and right-click to see custom menu items.\n\nThe context menu will include:\n- Convert to UPPERCASE\n- Count Characters\n\nTry selecting different parts of this text!", false);
```

### Example 7: Keyboard Shortcuts

```cpp
UIWindow& window = ui_ptr->create_window(0, 0, "Shortcut Demo", 0, 0);

// Register Ctrl+S for save
window.register_shortcut(UIShortcut{
    .code = UI_KEYCODE_LETTER('S'),
    .ctrl = true,
    .invoke = []() {
        std::print("Save shortcut pressed\n");
        return true;
    }
});

// Register Ctrl+Shift+O for open
window.register_shortcut(UIShortcut{
    .code = UI_KEYCODE_LETTER('O'),
    .ctrl = true,
    .shift = true,
    .invoke = []() {
        std::print("Open shortcut pressed\n");
        return true;
    }
});

// Register F1 for help
window.register_shortcut(UIShortcut{
    .code = UIKeycode::F1,
    .invoke = []() {
        std::print("F1 help pressed\n");
        return true;
    }
});
```

### Example 8: Dynamic Table

```cpp
struct Person {
    std::string name;
    int age;
    std::string city;
};

static std::vector<Person> people = {
    {"Alice", 30, "New York"},
    {"Bob", 25, "London"},
    {"Charlie", 35, "Paris"},
    {"David", 28, "Tokyo"},
    {"Eve", 32, "Berlin"},
    {"Frank", 45, "Sydney"},
    {"Grace", 27, "Toronto"},
    {"Henry", 38, "Mumbai"}
};

static int selected = -1;

UIPanel& panel = window.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING | UIElement::hv_fill);
panel.add_label(0, "Click on rows to select them:");

UITable& table = panel.add_table(UIElement::hv_fill, "Name\tAge\tCity");
table.set_num_items(people.size());

table.on_getitem([](UITable&, UITableGetItem& m) -> int {
    m._is_selected = (selected == (int)m._row);
    const Person& p = people[m._row];

    if (m._column == 0) return m.format_to("{}", p.name);
    if (m._column == 1) return m.format_to("{}", p.age);
    if (m._column == 2) return m.format_to("{}", p.city);
    return 0;
});

table.on_click([](UITable& tbl) {
    int hit = tbl.hittest(tbl.cursor_pos());
    if (hit >= 0 && hit < (int)people.size()) {
        selected = hit;
        tbl.repaint(nullptr);
        std::print("Selected: {}\n", people[hit].name);
    }
});

table.resize_columns();
```

## Advanced Topics

### Refresh and Repaint

```cpp
// Request full relayout and repaint
element.relayout();

// Request repaint of element
element.refresh();

// Request repaint of specific region
UIRectangle region = {0, 100, 0, 50};
element.repaint(&region);
```

### Element Destruction

```cpp
// Mark element for destruction (happens in next update cycle)
element.destroy();

// Destroy all children
element.destroy_descendents();
```

### Focus Management

```cpp
// Set keyboard focus to element
element.focus();

// Check if element has focus
if (element.is_focused()) {
    // Element has focus
}
```

### Scaling and DPI

```cpp
// Get window scale factor (for HiDPI displays)
float scale = element.get_scale();

// Scale a size value
int scaled_size = element.scale(20);  // Scale 20px by window scale
```

### Context Pointer

Store custom data with any element:

```cpp
struct MyData {
    int value;
    std::string name;
};

MyData* data = new MyData{42, "Test"};
element.set_cp(data);

// Later, retrieve it:
MyData* retrieved = static_cast<MyData*>(element._cp);
```

## Utility Functions

### String Operations

```cpp
// Trim whitespace
std::string_view trimmed = ui_trim(str);
std::string_view left_trimmed = ui_trimstart(str);
std::string_view right_trimmed = ui_trimend(str);

// Parse numbers
int i = ui_atoi<int>("123");
float f = ui_atof<float>("3.14");

// Iterate lines
for_each_line(text, [](std::string_view line) {
    std::print("{}\n", line);
    return false;  // return true to break
});

// Get vector of lines
std::vector<std::string_view> lines = get_lines(text);
```

### File Loading

```cpp
// Load entire file into string
std::optional<std::string> content = LoadFile("/path/to/file.txt");
if (content) {
    std::print("File size: {}\n", content->size());
}
```

### Value Setting with Change Detection

```cpp
int value = 10;

// Set value and check if it changed
if (ui_set(value, 20)) {
    std::print("Value changed to {}\n", value);
}
```

## Platform-Specific Features

### Linux (X11)

```cpp
// Access native X11 window handle
ui_handle xwindow = window.native_window();
```

### Windows

```cpp
// Access native Win32 HWND
ui_handle hwnd = window.native_window();
```

## Tips and Best Practices

1. **Always use fill flags for containers and content**: Use `UIElement::hv_fill` on elements like `UISplitPane`, `UITabPane`, `UICode`, `UITable`, and most panels to ensure they're visible and properly sized
2. **Use method chaining**: Take advantage of the fluent API for cleaner code
3. **Prefer `add_*` functions**: Use the provided element creation functions rather than constructing elements manually
4. **Use lambdas for callbacks**: Capture variables by reference `[&]` or by value (for pointers) to access surrounding scope
5. **Call `refresh()` after changes**: After modifying element state, call `refresh()` to trigger a repaint
6. **Use flags for configuration**: Combine flags with `|` operator for flexible element configuration
7. **Store element pointers**: Keep pointers to elements you need to update later
8. **Check state with `is_*` functions**: Use `is_hovered()`, `is_focused()`, etc. for state-based logic
9. **Return proper values from handlers**: Return `1` from message handlers when you've handled the message
10. **Use `insert_content()` with method chaining**: For `UICode` elements, use `.insert_content(..., false)` which returns the element for further chaining

##  Further Reading

- See `src/luigi.hpp` for complete API reference
- See `src/luigi.cpp` for implementation details
- See `examples/luigi_example.cpp` for a comprehensive demonstration
- See `examples/luigi_doc_examples.cpp` for all documentation examples (runnable)
- See `src/gf.cpp` for real-world usage in the gf debugger frontend
