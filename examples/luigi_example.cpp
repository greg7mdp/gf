#ifdef _MSC_VER
   #pragma warning(disable : 4100) // unreferenced formal parameter
   #pragma warning(disable : 4996) // This function or variable may be unsafe. Consider using ... instead.
#endif

#include "../src/luigi.hpp"

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

int MyButtonMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::CLICKED) {
      std_print("clicked button '{}'...", ((UIButton*)el)->label);

      if (check_delete->check == UICheckbox::CHECKED) {
         el->_parent->refresh();
         el->destroy();
         std_print(" and deleted it!\n");
      } else {
         std_print(" but not deleted!\n");
      }
   }

   return 0;
}

void MyMenuCallback(const char* cp) {
   UILabelSetContent(label, cp);
   label->refresh();
}

int MyButton2Message(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::CLICKED) {
      UIMenu* menu = UIMenuCreate(el, 0);
      UIMenuAddItem(menu, 0, "Item 1\tCtrl+F5", []() { MyMenuCallback("Item 1 clicked!"); });
      UIMenuAddItem(menu, 0, "Item 2\tF6", []() { MyMenuCallback("Item 2 clicked!"); });
      UIMenuShow(menu);
   }

   return 0;
}

int MySliderHMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::VALUE_CHANGED) {
      gauge_horiz2->set_position(slider_horiz->_position);
      gauge_vert1->set_position(slider_horiz->_position);
   }

   return 0;
}

int MySliderVMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::VALUE_CHANGED) {
      gauge_vert2->set_position(slider_vert->_position);
      gauge_horiz1->set_position(slider_vert->_position);
   }

   return 0;
}

int selected;

int MyTableMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m = (UITableGetItem*)dp;
      m->isSelected     = selected == m->index;

      if (m->column == 0) {
         return m->format_to("Item {}", m->index);
      } else {
         return m->format_to("other column {}", m->index);
      }
   } else if (msg == UIMessage::LEFT_DOWN) {
      int hit = ((UITable*)el)->hittest(el->_window->_cursor.x, el->_window->_cursor.y);

      if (selected != hit) {
         selected = hit;
         if (!((UITable*)el)->ensure_visible(selected)) {
            el->repaint(NULL);
         }
      }
   }

   return 0;
}

int MyCheckboxMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::CLICKED) {
      auto cb = (UICheckbox*)el;
      cb->set_label(cb->check == UICheckbox::CHECKED ? "Off" : "On");

      // Note, because this message function is run when the checkbox is
      // clicked _before_ the main checkbox update message is executed, the
      // UICheckbox->check is in the state _prior_ to the update taking place.
      // Consider the operation here to mean:
      //  "if the state _was_ UI_CHECK_CHECKED then now set the label to..."
   }

   return 0;
}


int main(int argc, char** argv) {
   UIConfig cfg;
   auto     ui_ptr = UIInitialise(cfg);

   if (!ui_ptr)
      return 1;

   std::string home     = getenv("HOME");
   std::string fontPath = home + "/fonts/FiraCode-Regular.ttf";
   auto        fontCode = UIFontCreate(fontPath.c_str(), 12);
   UIFontActivate(fontCode);

   UIWindow& window = ui->add_window(0, 0, "luigi2 - Example Application", 0, 0);

   // Split window (vertically) into top/bottom panes.
   UISplitPane& uisplit_topbottom = window.add_splitpane(UIElement::VERTICAL, 0.75f);

   // Split top pane (horizontally) into left/right panes.
   UISplitPane& uisplit_top_leftright = uisplit_topbottom.add_splitpane(0, 0.3f);

   {
      // In the top-left pane - create a single panel taking up the whole pane.
      UIPanel& panel = uisplit_top_leftright.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING);

      // Panels are by default vertical in layout, so items start at top and go down.
      panel.add_button(0, "Hello World").set_user_proc(MyButtonMessage);

      // Create a new horizontal-layout "sub-panel" and put left and right panels inside it.
      UIPanel& subpanel = panel.add_panel(UIPanel::COLOR_1 | UIPanel::HORIZONTAL);

      // The left side will layout elements horizontally, with custom borders and gap.
      UIPanel& sub_left =
         subpanel.add_panel(UIPanel::COLOR_1 | UIPanel::HORIZONTAL).set_border(ui_rect_1(10)).set_gap(2);
      gauge_vert1 = &sub_left.add_gauge(UIElement::VERTICAL);
      gauge_vert2 = &sub_left.add_gauge(UIElement::VERTICAL);
      slider_vert = &sub_left.add_slider(UIElement::VERTICAL);
      slider_vert->set_user_proc(MySliderVMessage);

      // The right side will lay out elements vertically (the default), with default medium spacing.
      UIPanel& sub_right = subpanel.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING);
      sub_right.add_button(0, "1").set_user_proc(MyButtonMessage);
      sub_right.add_button(0, "2").set_user_proc(MyButtonMessage);
      sub_right.add_button(0, "3").set_user_proc(MyButtonMessage);
      sub_right.add_button(0, "4").set_user_proc(MyButtonMessage);
      sub_right.add_button(0, "5").set_user_proc(MyButtonMessage);

      // Back outside of the "sub-panel", we continue layout downwards.
      panel.add_button(0, "Goodbye World").set_user_proc(MyButtonMessage);

      gauge_horiz1             = &panel.add_gauge(0);
      gauge_horiz2             = &panel.add_gauge(0);
      slider_horiz             = &panel.add_slider(0);
      slider_horiz->_user_proc = MySliderHMessage;
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
      panel.add_button(0, "It's a button??").set_user_proc(MyButton2Message);
      label = &panel.add_label(UIElement::H_FILL, "Hello, I am a label!");
   }

   {
      // Bottom-Right pane.
      UITabPane& tabPane = uisplit_bottom_leftright.add_tabpane(0, "Tab 1\tMiddle Tab\tTab 3");

      // First tab in tabPane
      tabPane.add_table(0, "Column 1\tColumn 2").set_num_items(100000).set_user_proc(MyTableMessage).resize_columns();

      // Second tab
      tabPane.add_panel(UIPanel::COLOR_1).add_label(0, "you're in tab 2, bucko");

      // Third tab
      UIPanel& settingsPanel = tabPane.add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING | UIPanel::HORIZONTAL);
      settingsPanel.add_label(0, "Delete top-left panel buttons on click:");
      check_delete = &settingsPanel.add_checkbox(0, "Off").set_user_proc(MyCheckboxMessage);
   }

   UIWindowRegisterShortcut(&window, UIShortcut{.code = UI_KEYCODE_LETTER('T'), .ctrl = true, .invoke = []() {
                                                   MyMenuCallback("Keyboard shortcut!");
                                                }});

   {
      // Create a separate window demonstrating the MDI element
      UIMDIClient& client = ui->add_window(0, 0, "luigi 2 - MDI Example", 0, 0).add_mdiclient(0);

      client.add_mdichild(UIMDIChild::CLOSE_BUTTON, UIRectangle(10, 600, 10, 400), "My Window")
         .add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING)
         .add_label(0, "It's a christmas miracle");

      client.add_mdichild(UIMDIChild::CLOSE_BUTTON, UIRectangle(40, 630, 40, 430), "Second Window")
         .add_panel(UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING)
         .add_label(0, "the system is down");

      client.add_mdichild(UIMDIChild::CLOSE_BUTTON, UIRectangle(70, 670, 70, 470), "Third Window")
         .add_button(0, "giant button!!");
   }

   return UIMessageLoop();
}
