#ifdef _MSC_VER
   #pragma warning(disable : 4100) // unreferenced formal parameter
   #pragma warning(disable : 4996) // This function or variable may be unsafe. Consider using ... instead.
#endif

#include <luigi.hpp>

UILabel* label = nullptr;

UISlider* slider_horiz = nullptr;
UIGauge*  gauge_horiz1 = nullptr;
UIGauge*  gauge_horiz2 = nullptr;

UISlider* slider_vert = nullptr;
UIGauge*  gauge_vert1 = nullptr;
UIGauge*  gauge_vert2 = nullptr;

UICheckbox* check_delete;

const char* themeItems[] = {
   "panel1",         "panel2",       "selected",         "border",        "text",           "textDisabled",
   "textSelected",   "buttonNormal", "buttonHovered",    "buttonPressed", "buttonDisabled", "textboxNormal",
   "textboxFocused", "codeFocused",  "codeBackground",   "codeDefault",   "codeComment",    "codeString",
   "codeNumber",     "codeOperator", "codePreprocessor",
};

int selected;

int main(int argc, char** argv) {
   UIConfig cfg;
   auto     ui_ptr = UI::initialise(cfg);

   if (!ui_ptr)
      return 1;
#if 1
   std::string home     = getenv("HOME");
   std::string fontPath = home + "/fonts/FiraCode-Regular.ttf";
   auto        fontCode = ui_ptr->create_font(fontPath, 12);
   fontCode->activate();
#endif
   
   UIWindow& window = ui_ptr->create_window(0, 0, "luigi2 - Example Application", 0, 0);

   // Split window (vertically) into top/bottom panes.
   UISplitPane& uisplit_topbottom = window.add_splitpane(UIElement::vertical_flag, 0.75f);

   // Split top pane (horizontally) into left/right panes.
   UISplitPane& uisplit_top_leftright = uisplit_topbottom.add_splitpane(0, 0.3f);

   auto button_cb = [](UIButton& button) {
      std_print("clicked button '{}'...", button.label());

      if (check_delete->checked()) {
         button.parent().refresh();
         button.destroy();
         std_print(" and deleted it!\n");
      } else {
         std_print(" but not deleted!\n");
      }
   };

   {
      // In the top-left pane - create a single panel taking up the whole pane.
      UIPanel& panel = uisplit_top_leftright.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING);

      // Panels are by default vertical in layout, so items start at top and go down.
      panel.add_button(0, "Hello World").on_click(button_cb);

      // Create a new horizontal-layout "sub-panel" and put left and right panels inside it.
      UIPanel& subpanel = panel.add_panel(UIPanel::COLOR_1 | UIPanel::HORIZONTAL);

      // The left side will layout elements horizontally, with custom borders and gap.
      UIPanel& sub_left =
         subpanel.add_panel(UIPanel::COLOR_1 | UIPanel::HORIZONTAL).set_border(ui_rect_1(10)).set_gap(2);
      gauge_vert1 = &sub_left.add_gauge(UIElement::vertical_flag);
      gauge_vert2 = &sub_left.add_gauge(UIElement::vertical_flag);

      slider_vert = &sub_left.add_slider(UIElement::vertical_flag).on_value_changed([](UISlider&) {
         gauge_vert2->set_position(slider_vert->position());
         gauge_horiz1->set_position(slider_vert->position());
      });

      // The right side will lay out elements vertically (the default), with default medium spacing.
      UIPanel& sub_right = subpanel.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING);
      sub_right.add_button(0, "1").on_click(button_cb);
      sub_right.add_button(0, "2").on_click(button_cb);
      sub_right.add_button(0, "3").on_click(button_cb);
      sub_right.add_button(0, "4").on_click(button_cb);
      sub_right.add_button(0, "5").on_click(button_cb);

      // Back outside of the "sub-panel", we continue layout downwards.
      panel.add_button(0, "Goodbye World").on_click(button_cb);

      gauge_horiz1 = &panel.add_gauge(0);
      gauge_horiz2 = &panel.add_gauge(0);

      slider_horiz = &panel.add_slider(0).on_value_changed([](UISlider&) {
         gauge_horiz2->set_position(slider_horiz->position());
         gauge_vert1->set_position(slider_horiz->position());
      });

      panel.add_textbox(0);
      panel.add_textbox(0); // UITextbox::HIDE_CHARACTERS);

      // Set default slider positions.
      slider_vert->set_position(0.1);
      slider_horiz->set_position(0.3);
   }

   // Top-Right pane.
   uisplit_top_leftright.add_code(0).load_file("../src/luigi.hpp").set_focus_line(0);

   // Split bottom pane (horizontally) into left/right panes.
   UISplitPane& uisplit_bottom_leftright = uisplit_topbottom.add_splitpane(0, 0.3f);

   {
      // Bottom-Left pane.
      UIPanel& panel = uisplit_bottom_leftright.add_panel(UIPanel::COLOR_2).set_border(UIRectangle(5)).set_gap(5);
      panel.add_button(0, "It's a button??").on_click([&ui_ptr](UIButton& el) {
         ui_ptr->create_menu(&el, 0)
            .add_item(0, "Item 1\tCtrl+F5",
                      [](UIButton&) {
                         label->set_label("Item 1 clicked!");
                         label->refresh();
                      })
            .add_item(0, "Item 2\tF6",
                      [](UIButton&) {
                         label->set_label("Item 2 clicked!");
                         label->refresh();
                      })
            .show();
      });
      label = &panel.add_label(UIElement::h_fill, "Hello, I am a label!");
   }

   {
      // Bottom-Right pane.
      UITabPane& tabPane = uisplit_bottom_leftright.add_tabpane(0, "Tab 1\tMiddle Tab\tTab 3");

      // First tab in tabPane
      tabPane.add_table(0, "Column 1\tColumn 2")
         .set_num_items(100000)
         .on_getitem([](UITable&, UITableGetItem& m) -> int {
            m._is_selected = (selected == (int)m._row);
            if (m._column == 0) {
               return m.format_to("Item {}", m._row);
            } else {
               return m.format_to("other column {}", m._row);
            }
         })
         .on_click([](UITable& table) {
            int hit = table.hittest(table.cursor_pos());

            if (selected != hit) {
               selected = hit;
               if (!table.ensure_visible(selected)) {
                  table.repaint(NULL);
               }
            }
         })
         .resize_columns();

      // Second tab
      tabPane.add_panel(UIPanel::COLOR_1).add_label(0, "you're in tab 2, bucko");

      // Third tab
      UIPanel& settingsPanel = tabPane.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING | UIPanel::HORIZONTAL);
      settingsPanel.add_label(0, "Delete top-left panel buttons on click:");
      check_delete = &settingsPanel.add_checkbox(0, "Off").on_click([](UICheckbox& cb) {
         cb.set_label(cb.checked() ? "On" : "Off");
      });
   }

   window.register_shortcut(UIShortcut{.code = UI_KEYCODE_LETTER('T'), .ctrl = true, .invoke = []() {
                                          if (!label)
                                             return false;
                                          label->set_label("Keyboard shortcut!");
                                          label->refresh();
                                          return true;
                                       }});

   {
      // Create a separate window demonstrating the MDI element
      UIMDIClient& client = ui_ptr->create_window(0, 0, "luigi 2 - MDI Example", 0, 0).add_mdiclient(0);

      client.add_mdichild(UIMDIChild::CLOSE_BUTTON, UIRectangle(10, 600, 10, 400), "My Window")
         .add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING)
         .add_label(0, "It's a christmas miracle");

      client.add_mdichild(UIMDIChild::CLOSE_BUTTON, UIRectangle(40, 630, 40, 430), "Second Window")
         .add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING)
         .add_label(0, "the system is down");

      client.add_mdichild(UIMDIChild::CLOSE_BUTTON, UIRectangle(70, 670, 70, 470), "Third Window")
         .add_button(0, "giant button!!");
   }

   return ui_ptr->message_loop();
}
