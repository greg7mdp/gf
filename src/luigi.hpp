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
   #include <intrin.h>
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

#ifdef UI_LINUX
enum class UIKeycode : int {
    A         = XK_a,
    ZERO      = XK_0,
    BACKSPACE = XK_BackSpace,
    DEL       = XK_Delete,
    DOWN      = XK_Down,
    END       = XK_End,
    ENTER     = XK_Return,
    ESCAPE    = XK_Escape,
    F1        = XK_F1,
    HOME      = XK_Home,
    LEFT      = XK_Left,
    RIGHT     = XK_Right,
    SPACE     = XK_space,
    TAB       = XK_Tab,
    UP        = XK_Up,
    INSERT    = XK_Insert,
    BACKTICK  = XK_grave,
    PAGE_UP   = XK_Page_Up,
    PAGE_DOWN = XK_Page_Down,
};
#endif

#ifdef UI_WINDOWS
enum class UIKeycode : int {
    A         = 'A',
    ZERO      = '0',
    BACKSPACE = VK_BACK,
    DEL       = VK_DELETE,
    DOWN      = VK_DOWN,
    END       = VK_END,
    ENTER     = VK_RETURN,
    ESCAPE    = VK_ESCAPE,
    F1        = VK_F1,
    HOME      = VK_HOME,
    LEFT      = VK_LEFT,
    RIGHT     = VK_RIGHT,
    SPACE     = VK_SPACE,
    TAB       = VK_TAB,
    UP        = VK_UP,
    INSERT    = VK_INSERT,
    BACKTICK  = VK_OEM_3,
    PAGE_UP   = VK_PRIOR,
    PAGE_DOWN = VK_NEXT,
};
#endif

inline UIKeycode UI_KEYCODE_LETTER(char x) { return (UIKeycode)((int)UIKeycode::A + (x - 'A')); }
inline UIKeycode UI_KEYCODE_DIGIT(char x) { return (UIKeycode)((int)UIKeycode::ZERO + (x - '0')); }
inline UIKeycode UI_KEYCODE_FKEY(char x) { return (UIKeycode)((int)UIKeycode::F1 + (x - 1)); }

// --------------------------------------------------
// Definitions.
// --------------------------------------------------

#define _UI_TO_STRING_1(x) #x
#define _UI_TO_STRING_2(x) _UI_TO_STRING_1(x)

namespace ui_size {

inline constexpr int BUTTON_MINIMUM_WIDTH = 100;
inline constexpr int BUTTON_PADDING       = 16;
inline constexpr int BUTTON_HEIGHT        = 27;
inline constexpr int BUTTON_CHECKED_AREA  = 4;

inline constexpr int CHECKBOX_BOX = 14;
inline constexpr int CHECKBOX_GAP = 8;

inline constexpr int MENU_ITEM_HEIGHT        = 24;
inline constexpr int MENU_ITEM_MINIMUM_WIDTH = 160;
inline constexpr int MENU_ITEM_MARGIN        = 9;

inline constexpr int GAUGE_WIDTH  = 200;
inline constexpr int GAUGE_HEIGHT = 22;

inline constexpr int SLIDER_WIDTH  = 200;
inline constexpr int SLIDER_HEIGHT = 25;
inline constexpr int SLIDER_THUMB  = 15;
inline constexpr int SLIDER_TRACK  = 5;

inline constexpr int TEXTBOX_MARGIN = 3;
inline constexpr int TEXTBOX_WIDTH  = 200;
inline constexpr int TEXTBOX_HEIGHT = 27;

inline constexpr int TAB_PANE_SPACE_TOP  = 2;
inline constexpr int TAB_PANE_SPACE_LEFT = 4;

inline constexpr int SPLITTER = 8;

inline constexpr int SCROLL_BAR           = 16;
inline constexpr int SCROLL_MINIMUM_THUMB = 20;

inline constexpr int TABLE_HEADER     = 26;
inline constexpr int TABLE_COLUMN_GAP = 20;
inline constexpr int TABLE_ROW        = 20;

inline constexpr int PANE_LARGE_BORDER  = 20;
inline constexpr int PANE_LARGE_GAP     = 10;
inline constexpr int PANE_MEDIUM_BORDER = 5;
inline constexpr int PANE_MEDIUM_GAP    = 5;
inline constexpr int PANE_SMALL_BORDER  = 3;
inline constexpr int PANE_SMALL_GAP     = 3;

inline constexpr int MDI_CHILD_BORDER         = 6;
inline constexpr int MDI_CHILD_TITLE          = 30;
inline constexpr int MDI_CHILD_CORNER         = 12;
inline constexpr int MDI_CHILD_MINIMUM_WIDTH  = 100;
inline constexpr int MDI_CHILD_MINIMUM_HEIGHT = 50;
inline constexpr int MDI_CASCADE              = 30;

} // namespace ui_size


namespace UIUpdate {
   enum {
      HOVERED = 1,
      PRESSED = 2,
      FOCUSED = 3,
      DISABLED = 4
   };
}

enum class UIMessage : uint32_t {
   // General messages.
   PAINT,               // dp = pointer to UIPainter
   PAINT_FOREGROUND,    // after children have painted
   LAYOUT,
   DESTROY,
   DEALLOCATE,
   UPDATE,              // di = UI_UPDATE_... constant
   ANIMATE,
   SCROLLED,
   GET_WIDTH,           // di = height (if known); return width
   GET_HEIGHT,          // di = width (if known); return height
   GET_CHILD_STABILITY, // dp = child element; return stable axes, 1 (width) | 2 (height)

   // Input events.
   INPUT_EVENTS_START,  // not sent to disabled elements
   LEFT_DOWN,
   LEFT_UP,
   MIDDLE_DOWN,
   MIDDLE_UP,
   RIGHT_DOWN,
   RIGHT_UP,
   KEY_TYPED,           // dp = pointer to UIKeyTyped; return 1 if handled
   KEY_RELEASED,        // dp = pointer to UIKeyTyped; return 1 if handled
   MOUSE_MOVE,
   MOUSE_DRAG,
   MOUSE_WHEEL,         // di = delta; return 1 if handled
   CLICKED,
   GET_CURSOR,          // return cursor code
   PRESSED_DESCENDENT,  // dp = pointer to child that is/contains pressed element
   INPUT_EVENTS_END,

   // Specific elements.
   VALUE_CHANGED,         // sent to notify that the element's value has changed
   TABLE_GET_ITEM,        // dp = pointer to UITableGetItem; return string length
   CODE_GET_MARGIN_COLOR, // di = line index (starts at 1); return color
   CODE_DECORATE_LINE,    // dp = pointer to UICodeDecorateLine
   TAB_SELECTED,          // sent to the tab that was selected (not the tab pane itself)

   // Windows.
   WINDOW_DROP_FILES,     // di = count, dp = char ** of paths
   WINDOW_ACTIVATE,
   WINDOW_CLOSE,          // return 1 to prevent default (process exit for UIWindow; close for UIMDIChild)
   WINDOW_UPDATE_START,
   WINDOW_UPDATE_BEFORE_DESTROY,
   WINDOW_UPDATE_BEFORE_LAYOUT,
   WINDOW_UPDATE_BEFORE_PAINT,
   WINDOW_UPDATE_END,

   // User-defined messages.
   USER,
   USER_PLUS_1,
};

struct UIPoint {
   int x, y;
};

struct UIRectangle {
   int l, r, t, b;

   int     width()  const { return r - l; }
   int     height() const { return b - t; }
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
   UIKeycode code;
   bool      ctrl, shift, alt;
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
   UIKeycode code;
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
   return (((uint32_t)(r * 255.0f) << 16) | ((uint32_t)(g * 255.0f) << 8) | ((uint32_t)(b * 255.0f) << 0) |
           ((uint32_t)(a * 255.0f) << 24));
}


#ifndef UI_DRAW_CONTROL_CUSTOM
   #define UIDrawControl UIDrawControlDefault
#endif


enum {
   UI_DRAW_CONTROL_PUSH_BUTTON      = 1,
   UI_DRAW_CONTROL_DROP_DOWN        = 2,
   UI_DRAW_CONTROL_MENU_ITEM        = 3,
   UI_DRAW_CONTROL_CHECKBOX         = 4,
   UI_DRAW_CONTROL_LABEL            = 5,
   UI_DRAW_CONTROL_SPLITTER         = 6,
   UI_DRAW_CONTROL_SCROLL_TRACK     = 7,
   UI_DRAW_CONTROL_SCROLL_UP        = 8,
   UI_DRAW_CONTROL_SCROLL_DOWN      = 9,
   UI_DRAW_CONTROL_SCROLL_THUMB     = 10,
   UI_DRAW_CONTROL_GAUGE            = 11,
   UI_DRAW_CONTROL_SLIDER           = 12,
   UI_DRAW_CONTROL_TEXTBOX          = 13,
   UI_DRAW_CONTROL_MODAL_POPUP      = 14,
   UI_DRAW_CONTROL_MENU             = 15,
   UI_DRAW_CONTROL_TABLE_ROW        = 16,
   UI_DRAW_CONTROL_TABLE_CELL       = 17,
   UI_DRAW_CONTROL_TABLE_BACKGROUND = 18,
   UI_DRAW_CONTROL_TABLE_HEADER     = 19,
   UI_DRAW_CONTROL_MDI_CHILD        = 20,
   UI_DRAW_CONTROL_TAB              = 21,
   UI_DRAW_CONTROL_TAB_BAND         = 22,

   UI_DRAW_CONTROL_TYPE_MASK           = 0xFF,
   UI_DRAW_CONTROL_STATE_SELECTED      = 1 << 24,
   UI_DRAW_CONTROL_STATE_VERTICAL      = 1 << 25,
   UI_DRAW_CONTROL_STATE_INDETERMINATE = 1 << 26,
   UI_DRAW_CONTROL_STATE_CHECKED       = 1 << 27,
   UI_DRAW_CONTROL_STATE_HOVERED       = 1 << 28,
   UI_DRAW_CONTROL_STATE_FOCUSED       = 1 << 29,
   UI_DRAW_CONTROL_STATE_PRESSED       = 1 << 30,
   UI_DRAW_CONTROL_STATE_DISABLED      = 1 << 31,
};

enum class UICursor : uint32_t {
   arrow             = 0,
   text              = 1,
   split_v           = 2,
   split_h           = 3,
   flipped_arrow     = 4,
   cross_hair        = 5,
   hand              = 6,
   resize_up         = 7,
   resize_left       = 8,
   resize_up_right   = 9,
   resize_up_left    = 10,
   resize_down       = 11,
   resize_right      = 12,
   resize_down_right = 13,
   resize_down_left  = 14,
   count             = 15,
};

enum class UIAlign : uint32_t {
   left   = 1,
   right  = 2,
   center = 3,
};

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

   uint32_t state() const;
 };

#define UI_SHORTCUT(code, ctrl, shift, alt, invoke, cp) ((UIShortcut){(code), (ctrl), (shift), (alt), (invoke), (cp)})

struct UIWindow {
   enum {
      MENU            = (1 << 0),
      INSPECTOR       = (1 << 1),
      CENTER_IN_OWNER = (1 << 2),
      MAXIMIZE        = (1 << 3),
   };

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
   enum {
      HORIZONTAL     = 1 << 0,
      COLOR_1        = 1 << 2,
      COLOR_2        = 1 << 3,
      SMALL_SPACING  = 1 << 5,
      MEDIUM_SPACING = 1 << 6,
      LARGE_SPACING  = 1 << 7,
      SCROLL         = 1 << 8,
      EXPAND         = 1 << 9,
   };

   UIElement    e;
   UIScrollBar* scrollBar;
   UIRectangle  border;
   int          gap;
};

struct UIButton {
   enum {
      SMALL     = 1 << 0,
      MENU_ITEM = 1 << 1,
      CAN_FOCUS = 1 << 2,
      DROP_DOWN = 1 << 3,
      CHECKED   = 1 << 15,
   };

   UIElement e;
   char*     label;
   ptrdiff_t labelBytes;
   void (*invoke)(void* cp);
};

struct UICheckbox {
   enum { ALLOW_INDETERMINATE = 1 << 0 };

   enum {
      UNCHECKED = 0,
      CHECKED = 1,
      INDETERMINATE = 2
   };

   UIElement e;
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
   enum { VERTICAL = 1 << 0 };

   UIElement e;
   float     weight;
};

struct UITabPane {
   UIElement e;
   char*     tabs;
   uint32_t  active;
};

struct UIScrollBar {
   enum { HORIZONTAL = 1 << 0 };

   UIElement  e;
   int64_t    maximum, page;
   int64_t    dragOffset;
   double     position;
   UI_CLOCK_T lastAnimateTime;
   bool       inDrag, horizontal;
};

template <class EL>
inline void UILayoutScrollbarPair(const EL* el, int hSpace, int vSpace, int scrollBarSize) {
   el->vScroll->page            = vSpace - (el->hScroll->page < el->hScroll->maximum ? scrollBarSize : 0);
   el->hScroll->page            = hSpace - (el->vScroll->page < el->vScroll->maximum ? scrollBarSize : 0);
   el->vScroll->page            = vSpace - (el->hScroll->page < el->hScroll->maximum ? scrollBarSize : 0);
   UIRectangle vScrollBarBounds = el->e.bounds, hScrollBarBounds = el->e.bounds;
   hScrollBarBounds.r = vScrollBarBounds.l =
      vScrollBarBounds.r - (el->vScroll->page < el->vScroll->maximum ? scrollBarSize : 0);
   vScrollBarBounds.b = hScrollBarBounds.t =
      hScrollBarBounds.b - (el->hScroll->page < el->hScroll->maximum ? scrollBarSize : 0);
   UIElementMove(&el->vScroll->e, vScrollBarBounds, true);
   UIElementMove(&el->hScroll->e, hScrollBarBounds, true);
}

template <class EL>
inline void  UIKeyInputVScroll(EL* el, UIKeyTyped* m, int rowHeight, int pageHeight) {
   if (m->code == UIKeycode::UP)
      el->vScroll->position -= (rowHeight);
   else if (m->code == UIKeycode::DOWN)
      el->vScroll->position += (rowHeight);
   else if (m->code == UIKeycode::PAGE_UP)
      el->vScroll->position += (pageHeight);
   else if (m->code == UIKeycode::PAGE_DOWN)
      el->vScroll->position -= (pageHeight);
   else if (m->code == UIKeycode::HOME)
      el->vScroll->position = 0;
   else if (m->code == UIKeycode::END)
      el->vScroll->position = el->vScroll->maximum;
   UIElementRefresh(&el->e);
}

struct UICodeLine {
   int offset, bytes;
};

struct UICode {
   enum {
      NO_MARGIN  = 1 << 0,
      SELECTABLE = 1 << 1
   };

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

struct UIMenu {
   enum {
      PLACE_ABOVE = 1 << 0,
      NO_SCROLL   = 1 << 1
   };

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
   enum {
      _TRANSPARENT = 1 << 0
   };

   UIElement          e;
   struct UIMDIChild* active;
   int                cascade;
};

struct UIMDIChild {
   enum {
      CLOSE_BUTTON = 1 << 0
   };

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
   enum {
      INTERACTIVE = (1 << 0),
      ZOOM_FIT = (1 << 1)
   };

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
void UIDrawString(UIPainter* painter, UIRectangle r, const char* string, ptrdiff_t bytes, uint32_t color, UIAlign align,
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
   Cursor cursors[(uint32_t)UICursor::count];
   char*  pasteText;
   XEvent copyEvent;
#endif

#ifdef UI_WINDOWS
   HCURSOR cursors[(uint32_t)UICursor::count];
   HANDLE  heap;
   bool    assertionFailure;
#endif

#ifdef UI_FREETYPE
   FT_Library ft;
#endif

   int code_margin() { return activeFont->glyphWidth * 5; }
   int code_margin_gap() { return activeFont->glyphWidth * 1; }
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
#undef max
#undef min
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
