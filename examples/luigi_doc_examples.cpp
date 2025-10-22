// luigi_doc_examples.cpp
//
// This file contains all the code examples from doc/luigi.md
// It serves as both:
//   1. A verification that all documentation examples compile correctly
//   2. A runnable demonstration of Luigi GUI features
//
// Build:
//   cmake --build build --target luigi_doc_examples
//
// Run:
//   ./build/examples/luigi_doc_examples
//
// The application creates a tabbed interface where each tab demonstrates
// a different example from the documentation.

#include <luigi.hpp>
#include <algorithm>
#include <vector>

// Global pointers for examples that need them
UILabel* g_label = nullptr;
UIGauge* g_gauge1 = nullptr;
UIGauge* g_gauge2 = nullptr;

// =============================================================================
// Example 1: Simple Button Counter
// =============================================================================
void example1_button_counter(UIPanel& parent) {
    static int counter = 0;
    static UILabel* label = nullptr;

    UIPanel& panel = parent.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING);

    label = &panel.add_label(0, "Count: 0");

    panel.add_button(0, "Increment").on_click([](UIButton&) {
        counter++;
        label->set_label(std::format("Count: {}", counter));
        label->refresh();
    });

    panel.add_button(0, "Reset").on_click([](UIButton&) {
        counter = 0;
        label->set_label("Count: 0");
        label->refresh();
    });
}

// =============================================================================
// Example 2: Slider Controlling Gauges
// =============================================================================
void example2_slider_gauges(UIPanel& parent) {
    UIGauge* gauge1 = nullptr;
    UIGauge* gauge2 = nullptr;

    UIPanel& panel = parent.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING);

    gauge1 = &panel.add_gauge(0);
    gauge2 = &panel.add_gauge(0);

    UISlider& slider = panel.add_slider(0);
    slider.on_value_changed([gauge1, gauge2](UISlider& s) {
        gauge1->set_position(s.position());
        gauge2->set_position(s.position() * 0.5);  // Half speed
    });

    slider.set_position(0.5);  // Initial position
}

// =============================================================================
// Example 3: Text Editor with File Loading
// =============================================================================
void example3_text_editor(UIPanel& parent) {
   UIPanel& panel = parent.add_panel(UIPanel::COLOR_1 | UIPanel::EXPAND | UIElement::hv_fill);

    UIPanel& toolbar =
       panel.add_panel(UIPanel::HORIZONTAL | UIPanel::MEDIUM_SPACING).set_border(UIRectangle(5)).set_gap(5);

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

}

// =============================================================================
// Example 4: Split Pane Layout
// =============================================================================
void example4_split_pane(UIElement& parent) {
    // Vertical split (top 75%, bottom 25%)
    UISplitPane& vsplit = parent.add_splitpane(UIElement::vertical_flag | UIElement::hv_fill, 0.75f);

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
        console.insert_content( "Button 1 clicked!\n", false);
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
}

// =============================================================================
// Example 5: Tabbed Interface
// =============================================================================
void example5_tabbed(UIElement& parent) {
    // This example shows how to create a tabbed interface
    UIPanel& panel = parent.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING | UIElement::hv_fill);

    panel.add_label(0, "This tab demonstrates the UITabPane element:");

    UITabPane& tabs = panel.add_tabpane(UIElement::hv_fill, "Editor\tSettings\tAbout");

    // Tab 1: Editor
    UICode& editor = tabs.add_code(UICode::SELECTABLE);
    editor.insert_content("// Example code editor\n#include <iostream>\n\nint main() {\n    std::cout << \"Hello from "
                          "tabbed interface!\\n\";\n    return 0;\n}\n",
                          false);

    // Tab 2: Settings
    UIPanel& settings = tabs.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING);
    settings.add_checkbox(0, "Enable auto-save");
    settings.add_checkbox(0, "Show line numbers");
    settings.add_checkbox(0, "Dark theme");

    // Tab 3: About
    tabs.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING)
       .add_label(0, "Luigi GUI Framework v2.0\n\nA lightweight, modern C++ GUI framework");
}

// =============================================================================
// Example 6: Custom Context Menu
// =============================================================================
void example6_context_menu(UIElement& parent) {
    UIPanel& panel = parent.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING | UIElement::hv_fill);

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

    code.insert_content(
       "Select this text and right-click to see custom menu items.\n\nThe context menu will include:\n- Convert to "
       "UPPERCASE\n- Count Characters\n\nTry selecting different parts of this text!",
       false);
}

// =============================================================================
// Example 7: Keyboard Shortcuts (registered on window)
// =============================================================================
void example7_keyboard_shortcuts(UIWindow& window) {
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
}

// =============================================================================
// Example 8: Dynamic Table
// =============================================================================
void example8_dynamic_table(UIElement& parent) {
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

    UIPanel& panel = parent.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING | UIElement::hv_fill);
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
}

// =============================================================================
// Additional Examples from Documentation Sections
// =============================================================================

void example_ui_elements(UIElement& parent) {
    UIPanel& panel = parent.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING | UIElement::hv_fill);

    // UIButton example
    UIButton& button = panel.add_button(0, "Button Label");
    button.on_click([](UIButton& btn) {
        std::print("Clicked: {}\n", btn.label());
        btn.set_label("Clicked!");
        btn.refresh();
    });

    // UICheckbox example
    static bool my_option = false;
    UICheckbox& checkbox = panel.add_checkbox(0, "Enable Feature");
    checkbox.on_click([](UICheckbox& cb) {
        std::print("Checked: {}\n", cb.checked());
        cb.set_label(cb.checked() ? "Enabled" : "Disabled");
    });
    checkbox.track(&my_option);

    // UILabel example
    UILabel& label = panel.add_label(UIElement::h_fill, "Hello, World!");

    // UITextbox example
    UITextbox& textbox = panel.add_textbox(0);
    textbox.replace_text("Type here", true);

    // UISlider example
    UISlider& slider = panel.add_slider(0);
    slider.set_position(0.5);
    slider.on_value_changed([](UISlider& s) {
        std::print("Slider value: {}\n", s.position());
    });

    // UIGauge example
    UIGauge& gauge = panel.add_gauge(0);
    gauge.set_position(0.75);

    // UISpacer example
    panel.add_spacer(0, 20, 10);
}

void example_nested_layouts(UIElement& parent) {
    // Main vertical panel
    UIPanel& main = parent.add_panel(UIPanel::COLOR_1 | UIElement::hv_fill);

    // Top section
    main.add_label(0, "Header");

    // Middle section: horizontal panel with two columns
    UIPanel& middle = main.add_panel(UIPanel::HORIZONTAL);
    middle.add_panel(UIPanel::COLOR_2).add_label(0, "Left");
    middle.add_panel(UIPanel::COLOR_2).add_label(0, "Right");

    // Bottom section
    main.add_button(0, "OK");
}

void example_event_handling(UIElement& parent) {
    UIPanel& panel = parent.add_panel(UIPanel::COLOR_1);

    // Mouse event example
    panel.set_user_proc([](UIElement* el, UIMessage msg, int di, void* dp) {
        if (msg == UIMessage::LEFT_DOWN) {
            std::print("Left click at ({}, {})\n",
                       el->cursor_pos().x, el->cursor_pos().y);
            return 1;
        }
        return 0;
    });
}

void example_clipboard(UIWindow& window) {
    // Access clipboard
    window.write_clipboard_text("Hello", sel_target_t::clipboard);
    std::string text = window.read_clipboard_text(sel_target_t::clipboard);
    std::print("Clipboard: {}\n", text);
}

// =============================================================================
// Main Application
// =============================================================================

int main(int argc, char** argv) {
    // Basic application structure from documentation
    UIConfig cfg;

   // optional: set local font if present
   std::string home     = getenv("HOME");
   std::string fontPath = home + "/.fonts/FiraCode-Regular.ttf";
   if (fs::exists(fontPath))
      cfg.font_path = fontPath;

    auto ui_ptr = UI::initialise(cfg);

    if (!ui_ptr)
        return 1;

    // Create main window
    UIWindow& window = ui_ptr->create_window(0, 0, "Luigi Documentation Examples", 1600, 900);

    // Register keyboard shortcuts (Example 7)
    example7_keyboard_shortcuts(window);

#if 0
    example3_text_editor(window.add_panel(UIPanel::COLOR_1));
#else
    // Create tabbed interface to organize all examples
    UITabPane& tabs = window.add_tabpane(
        0,
        "Counter\tSlider\tEditor\tSplit\tTabs\tContext\tTable\tElements\tLayouts"
    );

    // Add all examples as tabs
    example1_button_counter(tabs.add_panel(UIPanel::COLOR_1));
    example2_slider_gauges(tabs.add_panel(UIPanel::COLOR_1));
    example3_text_editor(tabs.add_panel(UIPanel::COLOR_1));
    example4_split_pane(tabs.add_panel(UIPanel::COLOR_1));
    example5_tabbed(tabs.add_panel(UIPanel::COLOR_1));
    example6_context_menu(tabs.add_panel(UIPanel::COLOR_1));
    example8_dynamic_table(tabs.add_panel(UIPanel::COLOR_1));
    example_ui_elements(tabs.add_panel(UIPanel::COLOR_1));
    example_nested_layouts(tabs.add_panel(UIPanel::COLOR_1));

    // Test clipboard (will print to console)
    example_clipboard(window);
#endif

    // Run message loop
    return ui_ptr->message_loop();
}
