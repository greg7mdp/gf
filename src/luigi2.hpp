// TODO UITextbox features - mouse input, undo, number dragging.
// TODO New elements - list view, menu bar.
// TODO Keyboard navigation in menus.
// TODO Easier to use fonts.

// --------------------------------------------------
// Header includes.
// --------------------------------------------------

#include <cstdint>
#include <cstddef>
#include <cstdarg>
#include <cstring>
#include <array>

#ifdef UI_LINUX
   #include <X11/Xlib.h>
   #include <X11/Xutil.h>
   #include <X11/Xatom.h>
   #include <X11/cursorfont.h>
#endif

#ifdef UI_SSE2
   #include <xmmintrin.h>
#endif

#ifdef UI_WINDOWS
   #undef _UNICODE
   #undef UNICODE
   #include <windows.h>

   #define UI_ASSERT(x)                                                                  \
      do {                                                                               \
         if (!(x)) {                                                                     \
            ui.assertionFailure = true;                                                  \
            MessageBox(0, "Assertion failure on line " _UI_TO_STRING_2(__LINE__), 0, 0); \
            ExitProcess(1);                                                              \
         }                                                                               \
      } while (0)
   #define UI_CALLOC(x) HeapAlloc(ui.heap, HEAP_ZERO_MEMORY, (x))
   #define UI_FREE(x) HeapFree(ui.heap, 0, (x))
   #define UI_MALLOC(x) HeapAlloc(ui.heap, 0, (x))
   #define UI_REALLOC _UIHeapReAlloc
   #define UI_CLOCK GetTickCount
   #define UI_CLOCKS_PER_SECOND (1000)
   #define UI_CLOCK_T DWORD
#endif

#if defined(UI_LINUX)
   #include <cstdlib>
   #include <cstring>
   #include <cassert>
   #include <ctime>
   #include <cmath>

   #define UI_ASSERT assert
   #define UI_CALLOC(x) calloc(1, (x))
   #define UI_FREE free
   #define UI_MALLOC malloc
   #define UI_REALLOC realloc
   #define UI_CLOCK _UIClock
   #define UI_CLOCKS_PER_SECOND 1000
   #define UI_CLOCK_T clock_t
#endif

#ifdef UI_DEBUG
   #include <stdio.h>
#endif

#ifdef UI_FREETYPE
   #include <ft2build.h>
   #include FT_FREETYPE_H
   #include <freetype/ftbitmap.h>
#endif

// --------------------------------------------------
// Definitions.
// --------------------------------------------------

#define _UI_TO_STRING_1(x) #x
#define _UI_TO_STRING_2(x) _UI_TO_STRING_1(x)

#define UI_SIZE_BUTTON_MINIMUM_WIDTH (100)
#define UI_SIZE_BUTTON_PADDING (16)
#define UI_SIZE_BUTTON_HEIGHT (27)
#define UI_SIZE_BUTTON_CHECKED_AREA (4)

#define UI_SIZE_CHECKBOX_BOX (14)
#define UI_SIZE_CHECKBOX_GAP (8)

#define UI_SIZE_MENU_ITEM_HEIGHT (24)
#define UI_SIZE_MENU_ITEM_MINIMUM_WIDTH (160)
#define UI_SIZE_MENU_ITEM_MARGIN (9)

#define UI_SIZE_GAUGE_WIDTH (200)
#define UI_SIZE_GAUGE_HEIGHT (22)

#define UI_SIZE_SLIDER_WIDTH (200)
#define UI_SIZE_SLIDER_HEIGHT (25)
#define UI_SIZE_SLIDER_THUMB (15)
#define UI_SIZE_SLIDER_TRACK (5)

#define UI_SIZE_TEXTBOX_MARGIN (3)
#define UI_SIZE_TEXTBOX_WIDTH (200)
#define UI_SIZE_TEXTBOX_HEIGHT (27)

#define UI_SIZE_TAB_PANE_SPACE_TOP (2)
#define UI_SIZE_TAB_PANE_SPACE_LEFT (4)

#define UI_SIZE_SPLITTER (8)

#define UI_SIZE_SCROLL_BAR (16)
#define UI_SIZE_SCROLL_MINIMUM_THUMB (20)

#define UI_SIZE_CODE_MARGIN (ui.activeFont->glyphWidth * 5)
#define UI_SIZE_CODE_MARGIN_GAP (ui.activeFont->glyphWidth * 1)

#define UI_SIZE_TABLE_HEADER (26)
#define UI_SIZE_TABLE_COLUMN_GAP (20)
#define UI_SIZE_TABLE_ROW (20)

#define UI_SIZE_PANE_LARGE_BORDER (20)
#define UI_SIZE_PANE_LARGE_GAP (10)
#define UI_SIZE_PANE_MEDIUM_BORDER (5)
#define UI_SIZE_PANE_MEDIUM_GAP (5)
#define UI_SIZE_PANE_SMALL_BORDER (3)
#define UI_SIZE_PANE_SMALL_GAP (3)

#define UI_SIZE_MDI_CHILD_BORDER (6)
#define UI_SIZE_MDI_CHILD_TITLE (30)
#define UI_SIZE_MDI_CHILD_CORNER (12)
#define UI_SIZE_MDI_CHILD_MINIMUM_WIDTH (100)
#define UI_SIZE_MDI_CHILD_MINIMUM_HEIGHT (50)
#define UI_SIZE_MDI_CASCADE (30)

#define UI_MDI_CHILD_CALCULATE_LAYOUT(bounds, scale)                                          \
   int         titleSize  = UI_SIZE_MDI_CHILD_TITLE * scale;                                  \
   int         borderSize = UI_SIZE_MDI_CHILD_BORDER * scale;                                 \
   UIRectangle title      = UIRectangleAdd(bounds, ui_rect_4(borderSize, -borderSize, 0, 0)); \
   title.b                = title.t + titleSize;                                              \
   UIRectangle content    = UIRectangleAdd(bounds, ui_rect_4(borderSize, -borderSize, titleSize, -borderSize));

#define UI_UPDATE_HOVERED (1)
#define UI_UPDATE_PRESSED (2)
#define UI_UPDATE_FOCUSED (3)
#define UI_UPDATE_DISABLED (4)

enum UIMessage {
   // General messages.
   UI_MSG_PAINT,            // dp = pointer to UIPainter
   UI_MSG_PAINT_FOREGROUND, // after children have painted
   UI_MSG_LAYOUT,
   UI_MSG_DESTROY,
   UI_MSG_DEALLOCATE,
   UI_MSG_UPDATE, // di = UI_UPDATE_... constant
   UI_MSG_ANIMATE,
   UI_MSG_SCROLLED,
   UI_MSG_GET_WIDTH,           // di = height (if known); return width
   UI_MSG_GET_HEIGHT,          // di = width (if known); return height
   UI_MSG_GET_CHILD_STABILITY, // dp = child element; return stable axes, 1 (width) | 2 (height)

   // Input events.
   UI_MSG_INPUT_EVENTS_START, // not sent to disabled elements
   UI_MSG_LEFT_DOWN,
   UI_MSG_LEFT_UP,
   UI_MSG_MIDDLE_DOWN,
   UI_MSG_MIDDLE_UP,
   UI_MSG_RIGHT_DOWN,
   UI_MSG_RIGHT_UP,
   UI_MSG_KEY_TYPED,    // dp = pointer to UIKeyTyped; return 1 if handled
   UI_MSG_KEY_RELEASED, // dp = pointer to UIKeyTyped; return 1 if handled
   UI_MSG_MOUSE_MOVE,
   UI_MSG_MOUSE_DRAG,
   UI_MSG_MOUSE_WHEEL, // di = delta; return 1 if handled
   UI_MSG_CLICKED,
   UI_MSG_GET_CURSOR,         // return cursor code
   UI_MSG_PRESSED_DESCENDENT, // dp = pointer to child that is/contains pressed element
   UI_MSG_INPUT_EVENTS_END,

   // Specific elements.
   UI_MSG_VALUE_CHANGED,         // sent to notify that the element's value has changed
   UI_MSG_TABLE_GET_ITEM,        // dp = pointer to UITableGetItem; return string length
   UI_MSG_CODE_GET_MARGIN_COLOR, // di = line index (starts at 1); return color
   UI_MSG_CODE_DECORATE_LINE,    // dp = pointer to UICodeDecorateLine
   UI_MSG_TAB_SELECTED,          // sent to the tab that was selected (not the tab pane itself)

   // Windows.
   UI_MSG_WINDOW_DROP_FILES, // di = count, dp = char ** of paths
   UI_MSG_WINDOW_ACTIVATE,
   UI_MSG_WINDOW_CLOSE, // return 1 to prevent default (process exit for UIWindow; close for UIMDIChild)
   UI_MSG_WINDOW_UPDATE_START,
   UI_MSG_WINDOW_UPDATE_BEFORE_DESTROY,
   UI_MSG_WINDOW_UPDATE_BEFORE_LAYOUT,
   UI_MSG_WINDOW_UPDATE_BEFORE_PAINT,
   UI_MSG_WINDOW_UPDATE_END,

   // User-defined messages.
   UI_MSG_USER,
};

struct UIPoint {
   int x, y;
};

struct UIRectangle {
   int l, r, t, b;

   int     width()  const { assert(r >= l); return r - l; }
   int     height() const { assert(b >= t); return b - t; }
   UIPoint center() const { return { l + width() / 2, t + height() / 2 }; }

   bool    valid()  const { return l < r && t < b; }
   bool    contains(const UIPoint& p) const { return p.x >= l && p.x < r && p.y >= t && p.y < b; }
   bool    contains(int x, int y) const { return x >= l && x < r && y >= t && y < b; }

   UIRectangle operator+(const UIRectangle& o) const { return { l + o.l, r + o.r, t + o.t, b + o.b }; }

   // following APIs  use `UIRectangle& o` as widths for left,right, top, bottom, instead of a proper rect.
   // -----------------------------------------------------------------------------------------------------
   UIRectangle shrink(const UIRectangle& o) const { return { l + o.l, r - o.r, t + o.t, b - o.b }; }
   int total_width() const { return r + l; }
   int total_height() const { return b + t; }

   // returns the 4 rectangles for drawing a border where the widths are from `o`
   std::array<UIRectangle, 4> border(const UIRectangle& o) const {
      return {{ { l, r, t, t + o.t },
                { l, l + o.l, t + o.t, b - o.b },
                { r - o.r, r, t + o.t, b - o.b },
                { l, r, b - o.b, b }
         }};
   }

   auto operator<=>(const UIRectangle&) const = default;
};

struct UITheme {
   uint32_t panel1, panel2, selected, border;
   uint32_t text, textDisabled, textSelected;
   uint32_t buttonNormal, buttonHovered, buttonPressed, buttonDisabled;
   uint32_t textboxNormal, textboxFocused;
   uint32_t codeFocused, codeBackground, codeDefault, codeComment, codeString, codeNumber, codeOperator,
      codePreprocessor;
   uint32_t accent1, accent2;
};

struct UIPainter {
   UIRectangle clip;
   uint32_t*   bits;
   int         width, height;
#ifdef UI_DEBUG
   int fillCount;
#endif
};

struct UIFont {
   int glyphWidth, glyphHeight;

#ifdef UI_FREETYPE
   bool      isFreeType;
   FT_Face   font;
   FT_Bitmap glyphs[128];
   bool      glyphsRendered[128];
   int       glyphOffsetsX[128], glyphOffsetsY[128];
#endif
};

struct UIShortcut {
   intptr_t code;
   bool     ctrl, shift, alt;
   void (*invoke)(void* cp);
   void* cp;
};

struct UIStringSelection {
   int      carets[2];
   uint32_t colorText, colorBackground;
};

struct UIKeyTyped {
   char*    text;
   int      textBytes;
   intptr_t code;
};

struct UITableGetItem {
   char*  buffer;
   size_t bufferBytes;
   int    index, column;
   bool   isSelected;
};

struct UICodeDecorateLine {
   UIRectangle bounds;
   int         index; // Starting at 1!
   int         x, y;  // Position where additional text can be drawn.
   UIPainter*  painter;
};

inline UIRectangle ui_rect_1(int x) { return UIRectangle{x, x, x, x}; }
inline UIRectangle ui_rect_1i(int x) { return UIRectangle{x, -x, x, -x}; }
inline UIRectangle ui_rect_2(int x, int y) { return UIRectangle{x, x, y, y}; }
inline UIRectangle ui_rect_2i(int x, int y) { return UIRectangle{x, -x, y, -y}; }
inline UIRectangle ui_rect_2s(int x, int y) { return UIRectangle{0, x, 0, y}; }
inline UIRectangle ui_rect_4(int x, int y, int z, int w) { return UIRectangle{x, y, z, w}; }
inline UIRectangle ui_rect_4pd(int x, int y, int w, int h) { return UIRectangle{x, (x + w), y, (y + h)}; }

#define UI_RECT_SIZE(_r) (_r).width(), (_r).height()
#define UI_RECT_TOP_LEFT(_r) (_r).l, (_r).t
#define UI_RECT_BOTTOM_LEFT(_r) (_r).l, (_r).b
#define UI_RECT_BOTTOM_RIGHT(_r) (_r).r, (_r).b
#define UI_RECT_ALL(_r) (_r).l, (_r).r, (_r).t, (_r).b

inline float ui_color_alpha_f(uint32_t x) { return ((x >> 24) & 0xFF) / 255.0f; }
inline float ui_color_red_f(uint32_t x)   { return ((x >> 16) & 0xFF) / 255.0f; }
inline float ui_color_green_f(uint32_t x) { return ((x >> 8)  & 0xFF) / 255.0f; }
inline float ui_color_blue_f(uint32_t x)  { return ((x >> 0)  & 0xFF) / 255.0f; }
inline float ui_color_alpha(uint32_t x)   { return ((x >> 24) & 0xFF); }
inline float ui_color_red(uint32_t x)     { return ((x >> 16) & 0xFF); }
inline float ui_color_green(uint32_t x)   { return ((x >> 8)  & 0xFF); }
inline float ui_color_blue(uint32_t x)    { return ((x >> 0)  & 0xFF); }

inline uint32_t ui_color_from_rgb(float r, float g, float b) {
   return (((uint32_t)(r * 255.0f) << 16) | ((uint32_t)(g * 255.0f) << 8) | ((uint32_t)(b * 255.0f) << 0));
}

inline uint32_t ui_color_from_rgba(float r, float g, float b, float a) {
   return (((uint32_t)((r) * 255.0f) << 16) | ((uint32_t)((g) * 255.0f) << 8) | ((uint32_t)((b) * 255.0f) << 0) |
           ((uint32_t)((a) * 255.0f) << 24));
}


#ifndef UI_DRAW_CONTROL_CUSTOM
   #define UIDrawControl UIDrawControlDefault
#endif
#define UI_DRAW_CONTROL_PUSH_BUTTON (1)
#define UI_DRAW_CONTROL_DROP_DOWN (2)
#define UI_DRAW_CONTROL_MENU_ITEM (3)
#define UI_DRAW_CONTROL_CHECKBOX (4)
#define UI_DRAW_CONTROL_LABEL (5)
#define UI_DRAW_CONTROL_SPLITTER (6)
#define UI_DRAW_CONTROL_SCROLL_TRACK (7)
#define UI_DRAW_CONTROL_SCROLL_UP (8)
#define UI_DRAW_CONTROL_SCROLL_DOWN (9)
#define UI_DRAW_CONTROL_SCROLL_THUMB (10)
#define UI_DRAW_CONTROL_GAUGE (11)
#define UI_DRAW_CONTROL_SLIDER (12)
#define UI_DRAW_CONTROL_TEXTBOX (13)
#define UI_DRAW_CONTROL_MODAL_POPUP (14)
#define UI_DRAW_CONTROL_MENU (15)
#define UI_DRAW_CONTROL_TABLE_ROW (16)
#define UI_DRAW_CONTROL_TABLE_CELL (17)
#define UI_DRAW_CONTROL_TABLE_BACKGROUND (18)
#define UI_DRAW_CONTROL_TABLE_HEADER (19)
#define UI_DRAW_CONTROL_MDI_CHILD (20)
#define UI_DRAW_CONTROL_TAB (21)
#define UI_DRAW_CONTROL_TAB_BAND (22)
#define UI_DRAW_CONTROL_TYPE_MASK (0xFF)
#define UI_DRAW_CONTROL_STATE_SELECTED (1 << 24)
#define UI_DRAW_CONTROL_STATE_VERTICAL (1 << 25)
#define UI_DRAW_CONTROL_STATE_INDETERMINATE (1 << 26)
#define UI_DRAW_CONTROL_STATE_CHECKED (1 << 27)
#define UI_DRAW_CONTROL_STATE_HOVERED (1 << 28)
#define UI_DRAW_CONTROL_STATE_FOCUSED (1 << 29)
#define UI_DRAW_CONTROL_STATE_PRESSED (1 << 30)
#define UI_DRAW_CONTROL_STATE_DISABLED (1 << 31)
#define UI_DRAW_CONTROL_STATE_FROM_ELEMENT(x)                                   \
   ((((x)->flags & UIElement::DISABLED) ? UI_DRAW_CONTROL_STATE_DISABLED : 0) | \
    (((x)->window->hovered == (x)) ? UI_DRAW_CONTROL_STATE_HOVERED : 0) |       \
    (((x)->window->focused == (x)) ? UI_DRAW_CONTROL_STATE_FOCUSED : 0) |       \
    (((x)->window->pressed == (x)) ? UI_DRAW_CONTROL_STATE_PRESSED : 0))

#define UI_CURSOR_ARROW (0)
#define UI_CURSOR_TEXT (1)
#define UI_CURSOR_SPLIT_V (2)
#define UI_CURSOR_SPLIT_H (3)
#define UI_CURSOR_FLIPPED_ARROW (4)
#define UI_CURSOR_CROSS_HAIR (5)
#define UI_CURSOR_HAND (6)
#define UI_CURSOR_RESIZE_UP (7)
#define UI_CURSOR_RESIZE_LEFT (8)
#define UI_CURSOR_RESIZE_UP_RIGHT (9)
#define UI_CURSOR_RESIZE_UP_LEFT (10)
#define UI_CURSOR_RESIZE_DOWN (11)
#define UI_CURSOR_RESIZE_RIGHT (12)
#define UI_CURSOR_RESIZE_DOWN_RIGHT (13)
#define UI_CURSOR_RESIZE_DOWN_LEFT (14)
#define UI_CURSOR_COUNT (15)

#define UI_ALIGN_LEFT (1)
#define UI_ALIGN_RIGHT (2)
#define UI_ALIGN_CENTER (3)

extern const int UI_KEYCODE_A;
extern const int UI_KEYCODE_BACKSPACE;
extern const int UI_KEYCODE_DELETE;
extern const int UI_KEYCODE_DOWN;
extern const int UI_KEYCODE_END;
extern const int UI_KEYCODE_ENTER;
extern const int UI_KEYCODE_ESCAPE;
extern const int UI_KEYCODE_F1;
extern const int UI_KEYCODE_HOME;
extern const int UI_KEYCODE_LEFT;
extern const int UI_KEYCODE_RIGHT;
extern const int UI_KEYCODE_SPACE;
extern const int UI_KEYCODE_TAB;
extern const int UI_KEYCODE_UP;
extern const int UI_KEYCODE_INSERT;
extern const int UI_KEYCODE_0;
extern const int UI_KEYCODE_BACKTICK;
extern const int UI_KEYCODE_PAGE_UP;
extern const int UI_KEYCODE_PAGE_DOWN;

#define UI_KEYCODE_LETTER(x) (UI_KEYCODE_A + (x) - 'A')
#define UI_KEYCODE_DIGIT(x) (UI_KEYCODE_0 + (x) - '0')
#define UI_KEYCODE_FKEY(x) (UI_KEYCODE_F1 + (x)-1)

struct UIWindow;
struct UIRectangle;
struct UIScrollBar;

struct UIElement {
   enum {
      V_FILL      = 1 << 16,
      H_FILL      = 1 << 17,
      FILL        = V_FILL | H_FILL,
      WINDOW      = 1 << 18,
      PARENT_PUSH = 1 << 19,
      TAB_STOP    = 1 << 20,
      NON_CLIENT  = 1 << 21, // Don't destroy in UIElementDestroyDescendents, like scroll bars.
      DISABLED    = 1 << 22, // Don't receive input events.
      BORDER      = 1 << 23,

      HIDE                = 1 << 27,
      RELAYOUT            = 1 << 28,
      RELAYOUT_DESCENDENT = 1 << 29,
      DESTROY             = 1 << 30,
      DESTROY_DESCENDENT  = 1 << 31
   };

   uint32_t flags; // First 16 bits are element specific.
   uint32_t id;
   uint32_t childCount;
   uint32_t _unused0;

   UIElement*  parent;
   UIElement** children;
   UIWindow*   window;

   UIRectangle bounds, clip;

   void* cp; // Context pointer (for user).

   int (*messageClass)(UIElement* element, UIMessage message, int di /* data integer */,
                       void* dp /* data pointer */);
   int (*messageUser)(UIElement* element, UIMessage message, int di, void* dp);

   const char* cClassName;
};

#define UI_SHORTCUT(code, ctrl, shift, alt, invoke, cp) ((UIShortcut){(code), (ctrl), (shift), (alt), (invoke), (cp)})

struct UIWindow {
#define UI_WINDOW_MENU (1 << 0)
#define UI_WINDOW_INSPECTOR (1 << 1)
#define UI_WINDOW_CENTER_IN_OWNER (1 << 2)
#define UI_WINDOW_MAXIMIZE (1 << 3)

   UIElement        e;
   UIElement*       dialog;
   UIShortcut*      shortcuts;
   size_t           shortcutCount, shortcutAllocated;
   float            scale;
   uint32_t*        bits;
   int              width, height;
   struct UIWindow* next;
   UIElement *      hovered, *pressed, *focused, *dialogOldFocus;
   int              pressedButton;
   UIPoint          cursor;
   int              cursorStyle;

   // Set when a textbox is modified.
   // Useful for tracking whether changes to the loaded document have been saved.
   bool        textboxModifiedFlag;
   bool        ctrl, shift, alt;
   UIRectangle updateRegion;

#ifdef UI_DEBUG
   float lastFullFillCount;
#endif

#ifdef UI_LINUX
   Window   window;
   XImage*  image;
   XIC      xic;
   unsigned ctrlCode, shiftCode, altCode;
   Window   dragSource;
#endif

#ifdef UI_WINDOWS
   HWND hwnd;
   bool trackingLeave;
#endif
};

struct UIPanel {
#define UI_PANEL_HORIZONTAL (1 << 0)
#define UI_PANEL_COLOR_1 (1 << 2)
#define UI_PANEL_COLOR_2 (1 << 3)
#define UI_PANEL_SMALL_SPACING (1 << 5)
#define UI_PANEL_MEDIUM_SPACING (1 << 6)
#define UI_PANEL_LARGE_SPACING (1 << 7)
#define UI_PANEL_SCROLL (1 << 8)
#define UI_PANEL_EXPAND (1 << 9)
   UIElement           e;
   UIScrollBar* scrollBar;
   UIRectangle         border;
   int                 gap;
};

struct UIButton {
#define UI_BUTTON_SMALL (1 << 0)
#define UI_BUTTON_MENU_ITEM (1 << 1)
#define UI_BUTTON_CAN_FOCUS (1 << 2)
#define UI_BUTTON_DROP_DOWN (1 << 3)
#define UI_BUTTON_CHECKED (1 << 15)
   UIElement e;
   char*     label;
   ptrdiff_t labelBytes;
   void (*invoke)(void* cp);
};

struct UICheckbox {
#define UI_CHECKBOX_ALLOW_INDETERMINATE (1 << 0)
   UIElement e;
#define UI_CHECK_UNCHECKED (0)
#define UI_CHECK_CHECKED (1)
#define UI_CHECK_INDETERMINATE (2)
   uint8_t   check;
   char*     label;
   ptrdiff_t labelBytes;
   void (*invoke)(void* cp);
};

struct UILabel {
   UIElement e;
   char*     label;
   ptrdiff_t labelBytes;
};

struct UISpacer {
   UIElement e;
   int       width, height;
};

struct UISplitPane {
#define UI_SPLIT_PANE_VERTICAL (1 << 0)
   UIElement e;
   float     weight;
};

struct UITabPane {
   UIElement e;
   char*     tabs;
   uint32_t  active;
};

struct UIScrollBar {
#define UI_SCROLL_BAR_HORIZONTAL (1 << 0)
   UIElement  e;
   int64_t    maximum, page;
   int64_t    dragOffset;
   double     position;
   UI_CLOCK_T lastAnimateTime;
   bool       inDrag, horizontal;
};

#define _UI_LAYOUT_SCROLL_BAR_PAIR(element)                                                                          \
   element->vScroll->page       = vSpace - (element->hScroll->page < element->hScroll->maximum ? scrollBarSize : 0); \
   element->hScroll->page       = hSpace - (element->vScroll->page < element->vScroll->maximum ? scrollBarSize : 0); \
   element->vScroll->page       = vSpace - (element->hScroll->page < element->hScroll->maximum ? scrollBarSize : 0); \
   UIRectangle vScrollBarBounds = element->e.bounds, hScrollBarBounds = element->e.bounds;                           \
   hScrollBarBounds.r = vScrollBarBounds.l =                                                                         \
      vScrollBarBounds.r - (element->vScroll->page < element->vScroll->maximum ? scrollBarSize : 0);                 \
   vScrollBarBounds.b = hScrollBarBounds.t =                                                                         \
      hScrollBarBounds.b - (element->hScroll->page < element->hScroll->maximum ? scrollBarSize : 0);                 \
   UIElementMove(&element->vScroll->e, vScrollBarBounds, true);                                                      \
   UIElementMove(&element->hScroll->e, hScrollBarBounds, true);

#define _UI_KEY_INPUT_VSCROLL(element, rowHeight, pageHeight) \
   if (m->code == UI_KEYCODE_UP)                              \
      element->vScroll->position -= (rowHeight);              \
   else if (m->code == UI_KEYCODE_DOWN)                       \
      element->vScroll->position += (rowHeight);              \
   else if (m->code == UI_KEYCODE_PAGE_UP)                    \
      element->vScroll->position += (pageHeight);             \
   else if (m->code == UI_KEYCODE_PAGE_DOWN)                  \
      element->vScroll->position -= (pageHeight);             \
   else if (m->code == UI_KEYCODE_HOME)                       \
      element->vScroll->position = 0;                         \
   else if (m->code == UI_KEYCODE_END)                        \
      element->vScroll->position = element->vScroll->maximum; \
   UIElementRefresh(&element->e);

struct UICodeLine {
   int offset, bytes;
};

struct UICode {
#define UI_CODE_NO_MARGIN (1 << 0)
#define UI_CODE_SELECTABLE (1 << 1)
   UIElement    e;
   UIScrollBar *vScroll, *hScroll;
   UICodeLine*  lines;
   UIFont*      font;
   int          lineCount, focused;
   bool         moveScrollToFocusNextLayout;
   bool         leftDownInMargin;
   char*        content;
   size_t       contentBytes;
   int          tabSize;
   int          columns;
   UI_CLOCK_T   lastAnimateTime;
   struct {
      int line, offset;
   } selection[4 /* start, end, anchor, caret */];
   int  verticalMotionColumn;
   bool useVerticalMotionColumn;
   bool moveScrollToCaretNextLayout;
};

struct UIGauge {
   UIElement e;
   double    position;
};

struct UITable {
   UIElement    e;
   UIScrollBar *vScroll, *hScroll;
   int          itemCount;
   char*        columns;
   int *        columnWidths, columnCount, columnHighlight;
};

struct UITextbox {
   UIElement e;
   char*     string;
   ptrdiff_t bytes;
   int       carets[2];
   int       scroll;
   bool      rejectNextKey;
};

#define UI_MENU_PLACE_ABOVE (1 << 0)
#define UI_MENU_NO_SCROLL (1 << 1)

struct UIMenu {
   UIElement    e;
   int          pointX, pointY;
   UIScrollBar* vScroll;
   UIWindow*    parentWindow;
};

struct UISlider {
   UIElement e;
   double    position;
   int       steps;
};

struct UIMDIClient {
#define UI_MDI_CLIENT_TRANSPARENT (1 << 0)
   UIElement          e;
   struct UIMDIChild* active;
   int                cascade;
};

struct UIMDIChild {
#define UI_MDI_CHILD_CLOSE_BUTTON (1 << 0)
   UIElement   e;
   UIRectangle bounds;
   char*       title;
   ptrdiff_t   titleBytes;
   int         dragHitTest;
   UIRectangle dragOffset;
};

struct UIExpandPane {
   UIElement e;
   UIButton* button;
   UIPanel*  panel;
   bool      expanded;
};

struct UIImageDisplay {
#define UI_IMAGE_DISPLAY_INTERACTIVE (1 << 0)
#define _UI_IMAGE_DISPLAY_ZOOM_FIT (1 << 1)

   UIElement e;
   uint32_t* bits;
   int       width, height;
   float     panX, panY, zoom;

   // Internals:
   int previousWidth, previousHeight;
   int previousPanPointX, previousPanPointY;
};

struct UIWrapPanel {
   UIElement e;
};

struct UISwitcher {
   UIElement  e;
   UIElement* active;
};

void UIInitialise();
int  UIMessageLoop();

UIElement* UIElementCreate(size_t bytes, UIElement* parent, uint32_t flags,
                           int (*messageClass)(UIElement*, UIMessage, int, void*), const char* cClassName);

UICheckbox*   UICheckboxCreate(UIElement* parent, uint32_t flags, const char* label, ptrdiff_t labelBytes);
UIExpandPane* UIExpandPaneCreate(UIElement* parent, uint32_t flags, const char* label, ptrdiff_t labelBytes,
                                 uint32_t panelFlags);
UIMDIClient*  UIMDIClientCreate(UIElement* parent, uint32_t flags);
UIMDIChild*   UIMDIChildCreate(UIElement* parent, uint32_t flags, UIRectangle initialBounds, const char* title,
                               ptrdiff_t titleBytes);
UIPanel*      UIPanelCreate(UIElement* parent, uint32_t flags);
UIScrollBar*  UIScrollBarCreate(UIElement* parent, uint32_t flags);
UISlider*     UISliderCreate(UIElement* parent, uint32_t flags);
UISpacer*     UISpacerCreate(UIElement* parent, uint32_t flags, int width, int height);
UISplitPane*  UISplitPaneCreate(UIElement* parent, uint32_t flags, float weight);
UITabPane*    UITabPaneCreate(UIElement* parent, uint32_t flags,
                              const char* tabs /* separate with \t, terminate with \0 */);
UIWrapPanel*  UIWrapPanelCreate(UIElement* parent, uint32_t flags);

UIGauge* UIGaugeCreate(UIElement* parent, uint32_t flags);
void     UIGaugeSetPosition(UIGauge* gauge, float value);

UIButton* UIButtonCreate(UIElement* parent, uint32_t flags, const char* label, ptrdiff_t labelBytes);
void      UIButtonSetLabel(UIButton* button, const char* string, ptrdiff_t stringBytes);
UILabel*  UILabelCreate(UIElement* parent, uint32_t flags, const char* label, ptrdiff_t labelBytes);
void      UILabelSetContent(UILabel* code, const char* content, ptrdiff_t byteCount);

UIImageDisplay* UIImageDisplayCreate(UIElement* parent, uint32_t flags, uint32_t* bits, size_t width, size_t height,
                                     size_t stride);
void UIImageDisplaySetContent(UIImageDisplay* display, uint32_t* bits, size_t width, size_t height, size_t stride);

UISwitcher* UISwitcherCreate(UIElement* parent, uint32_t flags);
void        UISwitcherSwitchTo(UISwitcher* switcher, UIElement* child);

UIWindow* UIWindowCreate(UIWindow* owner, uint32_t flags, const char* cTitle, int width, int height);
void      UIWindowRegisterShortcut(UIWindow* window, UIShortcut shortcut);
void      UIWindowPostMessage(UIWindow* window, UIMessage message, void* dp); // Thread-safe.
void      UIWindowPack(UIWindow* window, int width); // Change the size of the window to best match its contents.

typedef void (*UIDialogUserCallback)(UIElement*);
const char* UIDialogShow(UIWindow* window, uint32_t flags, const char* format, ...);

UIMenu* UIMenuCreate(UIElement* parent, uint32_t flags);
void    UIMenuAddItem(UIMenu* menu, uint32_t flags, const char* label, ptrdiff_t labelBytes, void (*invoke)(void* cp),
                      void* cp);
void    UIMenuShow(UIMenu* menu);
bool    UIMenusOpen();

UITextbox* UITextboxCreate(UIElement* parent, uint32_t flags);
void       UITextboxReplace(UITextbox* textbox, const char* text, ptrdiff_t bytes, bool sendChangedMessage);
void       UITextboxClear(UITextbox* textbox, bool sendChangedMessage);
void       UITextboxMoveCaret(UITextbox* textbox, bool backward, bool word);
char*      UITextboxToCString(UITextbox* textbox); // Free with UI_FREE.

UITable* UITableCreate(UIElement* parent, uint32_t flags,
                       const char* columns /* separate with \t, terminate with \0 */);
int      UITableHitTest(UITable* table, int x, int y);       // Returns item index. Returns -1 if not on an item.
int      UITableHeaderHitTest(UITable* table, int x, int y); // Returns column index or -1.
bool     UITableEnsureVisible(UITable* table, int index);    // Returns false if the item was already visible.
void     UITableResizeColumns(UITable* table);

UICode* UICodeCreate(UIElement* parent, uint32_t flags);
void    UICodeFocusLine(UICode* code, int index); // Line numbers are 1-indexed!!
int UICodeHitTest(UICode* code, int x, int y); // Returns line number; negates if in margin. Returns 0 if not on a line.
void UICodePositionToByte(UICode* code, int x, int y, int* line, int* byte);
void UICodeInsertContent(UICode* code, const char* content, ptrdiff_t byteCount, bool replace);
void UICodeMoveCaret(UICode* code, bool backward, bool word);

void UIDrawBlock(UIPainter* painter, UIRectangle rectangle, uint32_t color);
void UIDrawCircle(UIPainter* painter, int centerX, int centerY, int radius, uint32_t fillColor, uint32_t outlineColor,
                  bool hollow);
void UIDrawControl(UIPainter* painter, UIRectangle bounds, uint32_t mode /* UI_DRAW_CONTROL_* */, const char* label,
                   ptrdiff_t labelBytes, double position, float scale);
void UIDrawControlDefault(UIPainter* painter, UIRectangle bounds, uint32_t mode, const char* label,
                          ptrdiff_t labelBytes, double position, float scale);
void UIDrawInvert(UIPainter* painter, UIRectangle rectangle);
bool UIDrawLine(UIPainter* painter, int x0, int y0, int x1, int y1,
                uint32_t color); // Returns false if the line was not visible.
void UIDrawTriangle(UIPainter* painter, int x0, int y0, int x1, int y1, int x2, int y2, uint32_t color);
void UIDrawTriangleOutline(UIPainter* painter, int x0, int y0, int x1, int y1, int x2, int y2, uint32_t color);
void UIDrawGlyph(UIPainter* painter, int x, int y, int c, uint32_t color);
void UIDrawRectangle(UIPainter* painter, UIRectangle r, uint32_t mainColor, uint32_t borderColor,
                     UIRectangle borderSize);
void UIDrawBorder(UIPainter* painter, UIRectangle r, uint32_t borderColor, UIRectangle borderSize);
void UIDrawString(UIPainter* painter, UIRectangle r, const char* string, ptrdiff_t bytes, uint32_t color, int align,
                  UIStringSelection* selection);
int  UIDrawStringHighlighted(UIPainter* painter, UIRectangle r, const char* string, ptrdiff_t bytes, int tabSize,
                             UIStringSelection* selection); // Returns final x position.

int UIMeasureStringWidth(const char* string, ptrdiff_t bytes);
int UIMeasureStringHeight();

uint64_t UIAnimateClock(); // In ms.

bool        UIElementAnimate(UIElement* element, bool stop);
void        UIElementDestroy(UIElement* element);
void        UIElementDestroyDescendents(UIElement* element);
UIElement*  UIElementFindByPoint(UIElement* element, int x, int y);
void        UIElementFocus(UIElement* element);
UIRectangle UIElementScreenBounds(
   UIElement* element); // Returns bounds of element in same coordinate system as used by UIWindowCreate.
void       UIElementRefresh(UIElement* element);
void       UIElementRelayout(UIElement* element);
void       UIElementRepaint(UIElement* element, UIRectangle* region);
void       UIElementMeasurementsChanged(UIElement* element, int which);
void       UIElementMove(UIElement* element, UIRectangle bounds, bool alwaysLayout);
int        UIElementMessage(UIElement* element, UIMessage message, int di, void* dp);
UIElement* UIElementChangeParent(UIElement* element, UIElement* newParent,
                                 UIElement* insertBefore); // Set insertBefore to null to insert at the end. Returns the
                                                           // element it was before in its previous parent, or NULL.

UIElement* UIParentPush(UIElement* element);
UIElement* UIParentPop();

UIRectangle UIRectangleIntersection(const UIRectangle& a, const UIRectangle& b);
UIRectangle UIRectangleBounding(const UIRectangle& a, const UIRectangle& b);
UIRectangle UIRectangleTranslate(const UIRectangle& a, const UIRectangle& b);
UIRectangle UIRectangleCenter(const UIRectangle& parent, UIRectangle child);
UIRectangle UIRectangleFit(UIRectangle parent, UIRectangle child, bool allowScalingUp);

bool UIColorToHSV(uint32_t rgb, float* hue, float* saturation, float* value);
void UIColorToRGB(float hue, float saturation, float value, uint32_t* rgb);

char* UIStringCopy(const char* in, ptrdiff_t inBytes);

UIFont* UIFontCreate(const char* cPath, uint32_t size);
UIFont* UIFontActivate(UIFont* font); // Returns the previously active font.

#ifdef UI_DEBUG
void UIInspectorLog(const char* cFormat, ...);
#endif

struct UI {
   UIWindow* windows;
   UITheme   theme;

   UIElement** animating;
   uint32_t    animatingCount;

   UIElement* parentStack[16];
   int        parentStackCount;

   bool        quit;
   const char* dialogResult;
   UIElement*  dialogOldFocus;
   bool        dialogCanExit;

   UIFont* activeFont;

#ifdef UI_DEBUG
   UIWindow* inspector;
   UITable*  inspectorTable;
   UIWindow* inspectorTarget;
   UICode*   inspectorLog;
#endif

#ifdef UI_LINUX
   Display* display;
   Visual*  visual;
   XIM      xim;
   Atom     windowClosedID, primaryID, uriListID, plainTextID;
   Atom   dndEnterID, dndPositionID, dndStatusID, dndActionCopyID, dndDropID, dndSelectionID, dndFinishedID, dndAwareID;
   Atom   clipboardID, xSelectionDataID, textID, targetID, incrID;
   Cursor cursors[UI_CURSOR_COUNT];
   char*  pasteText;
   XEvent copyEvent;
#endif

#ifdef UI_WINDOWS
   HCURSOR cursors[UI_CURSOR_COUNT];
   HANDLE  heap;
   bool    assertionFailure;
#endif

#ifdef UI_FREETYPE
   FT_Library ft;
#endif
};

// ----------------------------------------
//      Forward declarations.
// ----------------------------------------

void  _UIWindowEndPaint(UIWindow* window, UIPainter* painter);
void  _UIWindowSetCursor(UIWindow* window, int cursor);
void  _UIWindowGetScreenPosition(UIWindow* window, int* x, int* y);
void  _UIWindowSetPressed(UIWindow* window, UIElement* element, int button);
void  _UIClipboardWriteText(UIWindow* window, char* text);
char* _UIClipboardReadTextStart(UIWindow* window, size_t* bytes);
void  _UIClipboardReadTextEnd(UIWindow* window, char* text);
bool  _UIMessageLoopSingle(int* result);
void  _UIInspectorRefresh();
void  _UIUpdate();

#if defined(UI_LINUX)
inline UI_CLOCK_T _UIClock() {
   struct timespec spec;
   clock_gettime(CLOCK_REALTIME, &spec);
   return spec.tv_sec * 1000 + spec.tv_nsec / 1000000;
}
#endif

#ifdef UI_WINDOWS
void* _UIHeapReAlloc(void* pointer, size_t size);
void* _UIMemmove(void* dest, const void* src, size_t n);
#endif


inline bool _UICharIsAlpha(char c) {
   return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

inline bool _UICharIsDigit(char c) {
   return c >= '0' && c <= '9';
}

inline bool _UICharIsAlphaOrDigitOrUnderscore(char c) {
   return _UICharIsAlpha(c) || _UICharIsDigit(c) || c == '_';
}

int _UICodeColumnToByte(UICode* code, int line, int column);
int _UICodeByteToColumn(UICode* code, int line, int byte);

int _UITabPaneMessage(UIElement* element, UIMessage message, int di, void* dp);

// ----------------------------------------
//      Variables
// ----------------------------------------

extern UI      ui;
extern UITheme uiThemeClassic;
extern UITheme uiThemeDark;
