
#include "../src/luigi.hpp"

UIWindow* window;
UILabel*  label;

UISlider* slider_horiz;
UIGauge*  gauge_horiz1;
UIGauge*  gauge_horiz2;

UISlider* slider_vert;
UIGauge*  gauge_vert1;
UIGauge*  gauge_vert2;

UICheckbox *check_delete;

const char* themeItems[] = {
   "panel1",         "panel2",       "selected",         "border",        "text",           "textDisabled",
   "textSelected",   "buttonNormal", "buttonHovered",    "buttonPressed", "buttonDisabled", "textboxNormal",
   "textboxFocused", "codeFocused",  "codeBackground",   "codeDefault",   "codeComment",    "codeString",
   "codeNumber",     "codeOperator", "codePreprocessor",
};

int MyButtonMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::CLICKED) {
      printf("clicked button '%.*s'...", ((UIButton*)element)->labelBytes, ((UIButton*)element)->label);
      // Note, the printf specifier %.*s expects an 'int', while labelBytes
      // is a 'ptrdiff_t'. Compilers may warn about this implicit casting,
      // however for this example (and in general) the number of characters
      // in a label will not exceed the maximum value of an 'int'.
      if (check_delete->check == UICheckbox::CHECKED) {
         element->parent->Refresh();
         element->Destroy();
         printf(" and deleted it!\n");
      } else {
         printf(" but not deleted!\n");
      }
   }

   return 0;
}

void MyMenuCallback(const char* cp) {
   UILabelSetContent(label, cp, -1);
   label->Refresh();
}

int MyButton2Message(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::CLICKED) {
      UIMenu* menu = UIMenuCreate(element, 0);
      UIMenuAddItem(menu, 0, "Item 1\tCtrl+F5", -1, []() { MyMenuCallback("Item 1 clicked!"); });
      UIMenuAddItem(menu, 0, "Item 2\tF6", -1, []() { MyMenuCallback("Item 2 clicked!"); });
      UIMenuShow(menu);
   }

   return 0;
}

int MySliderHMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::VALUE_CHANGED) {
      gauge_horiz2->position = slider_horiz->position;
      gauge_vert1->position  = slider_horiz->position;
      gauge_horiz2->Repaint(NULL);
      gauge_vert1->Repaint(NULL);
   }

   return 0;
}

int MySliderVMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::VALUE_CHANGED) {
      gauge_vert2->position  = slider_vert->position;
      gauge_horiz1->position = slider_vert->position;
      gauge_vert1->Repaint(NULL);
      gauge_horiz1->Repaint(NULL);
   }

   return 0;
}

int selected;

int MyTableMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m = (UITableGetItem*)dp;
      m->isSelected     = selected == m->index;

      if (m->column == 0) {
         return snprintf(m->buffer, m->bufferBytes, "Item %d", m->index);
      } else {
         return snprintf(m->buffer, m->bufferBytes, "other column %d", m->index);
      }
   } else if (message == UIMessage::LEFT_DOWN) {
      int hit = UITableHitTest((UITable*)element, element->window->cursor.x, element->window->cursor.y);

      if (selected != hit) {
         selected = hit;

         if (!UITableEnsureVisible((UITable*)element, selected)) {
            element->Repaint(NULL);
         }
      }
   }

   return 0;
}

int MyCheckboxMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::CLICKED) {
      const char* labelOn  = "On";
      const char* labelOff = "Off";
      auto cb = (UICheckbox*)element;
      cb->SetLabel(cb->check == UICheckbox::CHECKED ? labelOff : labelOn, -1);
      // Note, because this message function is run when the checkbox is
      // clicked _before_ the main checkbox update message is executed, the
      // UICheckbox->check is in the state _prior_ to the update taking place.
      // Consider the operation here to mean:
      //  "if the state _was_ UI_CHECK_CHECKED then now set the label to..."
   }

   return 0;
}


#ifdef UI_LINUX
int main(int argc, char** argv) {
#else
int WinMain(HINSTANCE instance, HINSTANCE previousInstance, LPSTR commandLine, int showCommand) {
#endif
   UIConfig cfg;
   auto     ui = UIInitialise(cfg);

   std::string home = getenv("HOME");
   std::string fontPath = home + "/fonts/FiraCode-Regular.ttf";
   auto fontCode = UIFontCreate(fontPath.c_str(), 12);
   UIFontActivate(fontCode);

   window = UIWindowCreate(0, 0, "luigi2 - Example Application", 0, 0);

   // Split window (vertically) into top/bottom panes.
   UISplitPane* uisplit_topbottom = UISplitPaneCreate(window, UIElement::VERTICAL, 0.75f);

   // Split top pane (horizontally) into left/right panes.
   UISplitPane* uisplit_top_leftright = UISplitPaneCreate(uisplit_topbottom, 0, 0.3f);

   {
      // In the top-left pane - create a single panel taking up the whole pane.
      UIPanel* panel = UIPanelCreate(uisplit_top_leftright, UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING);
      // Panels are by default vertical in layout, so items start at top and go down.
      UIButtonCreate(panel, 0, "Hello World", -1)->messageUser = MyButtonMessage;
      // Create a new horizontal-layout "sub-panel" and put left and right panels inside it.
      UIPanel* subpanel = UIPanelCreate(panel, UIPanel::COLOR_1 | UIPanel::HORIZONTAL);
      // The left side will layout elements horizontally, with custom borders and gap.
      UIPanel* sub_left        = UIPanelCreate(subpanel, UIPanel::COLOR_1 | UIPanel::HORIZONTAL);
      sub_left->border.t       = 10;
      sub_left->border.b       = 10;
      sub_left->border.l       = 10;
      sub_left->border.r       = 10;
      sub_left->gap            = 2;
      gauge_vert1              = UIGaugeCreate(sub_left, UIElement::VERTICAL);
      gauge_vert2              = UIGaugeCreate(sub_left, UIElement::VERTICAL);
      slider_vert              = UISliderCreate(sub_left, UIElement::VERTICAL);
      slider_vert->messageUser = MySliderVMessage;
      // The right side will lay out elements vertically (the default), with default medium spacing.
      UIPanel* sub_right = UIPanelCreate(subpanel, UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING);
      UIButtonCreate(sub_right, 0, "1", -1)->messageUser = MyButtonMessage;
      UIButtonCreate(sub_right, 0, "2", -1)->messageUser = MyButtonMessage;
      UIButtonCreate(sub_right, 0, "3", -1)->messageUser = MyButtonMessage;
      UIButtonCreate(sub_right, 0, "4", -1)->messageUser = MyButtonMessage;
      UIButtonCreate(sub_right, 0, "5", -1)->messageUser = MyButtonMessage;
      // Back outside of the "sub-panel", we continue layout downwards.
      UIButtonCreate(panel, 0, "Goodbye World", -1)->messageUser = MyButtonMessage;
      gauge_horiz1                                               = UIGaugeCreate(panel, 0);
      gauge_horiz2                                               = UIGaugeCreate(panel, 0);
      slider_horiz                                               = UISliderCreate(panel, 0);
      slider_horiz->messageUser                                  = MySliderHMessage;
      UITextboxCreate(panel, 0);
      UITextboxCreate(panel, 0); // UITextbox::HIDE_CHARACTERS);
      // Set default slider positions.
      slider_vert->SetPosition(0.1);
      slider_horiz->SetPosition(0.3);
   }

   {
      // Top-Right pane.
      UICode*               code = UICodeCreate(uisplit_top_leftright, 0);
      std::unique_ptr<char[]> buffer(new char[262144]);
      FILE*                 f    = fopen("../src/luigi.hpp", "rb");
      size_t                size = fread(buffer.get(), 1, 262144, f);
      fclose(f);
      UICodeInsertContent(code, buffer.get(), size, true);
      UICodeFocusLine(code, 0);
   }

   // Split bottom pane (horizontally) into left/right panes.
   UISplitPane* uisplit_bottom_leftright = UISplitPaneCreate(uisplit_topbottom, 0, 0.3f);

   {
      // Bottom-Left pane.
      UIPanel* panel = UIPanelCreate(uisplit_bottom_leftright, UIPanel::COLOR_2);
      panel->border  = UIRectangle(5);
      panel->gap     = 5;
      UIButtonCreate(panel, 0, "It's a button??", -1)->messageUser = MyButton2Message;
      label = UILabelCreate(panel, UIElement::H_FILL, "Hello, I am a label!", -1);
   }

   {
      // Bottom-Right pane.
      UITabPane* tabPane = UITabPaneCreate(uisplit_bottom_leftright, 0, "Tab 1\tMiddle Tab\tTab 3");
      // First tab in tabPane
      UITable*   table   = UITableCreate(tabPane, 0, "Column 1\tColumn 2");
      table->itemCount   = 100000;
      table->messageUser = MyTableMessage;
      UITableResizeColumns(table);
      // Second tab
      UILabelCreate(UIPanelCreate(tabPane, UIPanel::COLOR_1), 0, "you're in tab 2, bucko", -1);
      // Third tab
      UIPanel *settingsPanel = UIPanelCreate(tabPane, UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING | UIPanel::HORIZONTAL);
      UILabelCreate(settingsPanel, 0, "Delete top-left panel buttons on click:", -1);
      check_delete = UICheckboxCreate(settingsPanel, 0, "Off", -1);
      check_delete->messageUser = MyCheckboxMessage;
   }

   UIWindowRegisterShortcut(window, UIShortcut{.code = UI_KEYCODE_LETTER('T'), .ctrl = true, .invoke = []() {
                                                    MyMenuCallback("Keyboard shortcut!");
                                                 }});

   {
      // Create a separate window demonstrating the MDI element
      UIWindow*    window = UIWindowCreate(0, 0, "luigi 2 - MDI Example", 0, 0);
      UIMDIClient* client = UIMDIClientCreate(window, 0);
      UIMDIChild*  child1 =
         UIMDIChildCreate(client, UIMDIChild::CLOSE_BUTTON, UIRectangle(10, 600, 10, 400), "My Window", -1);
      UIPanel* panel1 = UIPanelCreate(child1, UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING);
      UILabelCreate(panel1, 0, "It's a christmas miracle", -1);
      UIMDIChild* child2 =
         UIMDIChildCreate(client, UIMDIChild::CLOSE_BUTTON, UIRectangle(40, 630, 40, 430), "Second Window", -1);
      UIPanel* panel2 = UIPanelCreate(child2, UIPanel::COLOR_1 | UIPanel::MEDIUM_SPACING);
      UILabelCreate(panel2, 0, "the system is down", -1);
      UIMDIChild* child3 =
         UIMDIChildCreate(client, UIMDIChild::CLOSE_BUTTON, UIRectangle(70, 670, 70, 470), "Third Window", -1);
      UIButtonCreate(child3, 0, "giant button!!", -1);
   }

   return UIMessageLoop();
}
