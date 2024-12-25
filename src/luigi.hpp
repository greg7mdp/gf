// TODO UITextbox features - mouse input, undo, number dragging.
// TODO New elements - list view, menu bar.
// TODO Keyboard navigation in menus.
// TODO Easier to use fonts.

// --------------------------------------------------
// Header includes.
// --------------------------------------------------
#pragma once

#include <algorithm>
#include <array>
#include <cassert>
#include <cstdarg>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <memory>
#include <string>
#include <iostream>
#include <format>
#include <unordered_map>

using std::unique_ptr;
using std::shared_ptr;
using std::make_unique;
using std::make_shared;

#define UI_DEBUG 0

#ifdef UI_LINUX
   #include <X11/Xlib.h>
   #include <X11/Xutil.h>
   #include <X11/Xatom.h>
   #include <X11/cursorfont.h>
#endif

#ifdef UI_WINDOWS
   #undef _UNICODE
   #undef UNICODE
   #include <windows.h>
   #include <shellapi.h>
   #undef min
   #undef max

   #define UI_ASSERT(x)                                                                  \
      do {                                                                               \
         if (!(x)) {                                                                     \
            ui->assertionFailure = true;                                                  \
            MessageBox(0, "Assertion failure on line " _UI_TO_STRING_2(__LINE__), 0, 0); \
            ExitProcess(1);                                                              \
         }                                                                               \
      } while (0)
   #define UI_CLOCK GetTickCount
   #define UI_CLOCKS_PER_SECOND (1000)
   #define UI_CLOCK_T DWORD
#endif

#if defined(UI_LINUX)
   #include <ctime>
   #include <cmath>

   #define UI_ASSERT assert
   #define UI_CLOCK _UIClock
   #define UI_CLOCKS_PER_SECOND 1000
   #define UI_CLOCK_T clock_t
#endif

#ifdef DMALLOC
   #include "dmalloc.h"
#endif

#ifdef UI_FREETYPE
   #include <ft2build.h>
   #include FT_FREETYPE_H
   #include <freetype/ftbitmap.h>

   #ifdef UI_FREETYPE_SUBPIXEL
      inline constexpr auto ft_render_mode = FT_RENDER_MODE_LCD;
   #else
      inline constexpr auto ft_render_mode = FT_RENDER_MODE_NORMAL;
   #endif
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
inline UIKeycode UI_KEYCODE_DIGIT(char x)  { return (UIKeycode)((int)UIKeycode::ZERO + (x - '0')); }
inline UIKeycode UI_KEYCODE_FKEY(char x)   { return (UIKeycode)((int)UIKeycode::F1 + (x - 1)); }

// ---------------------------------------------------------------------------------------------
//                              Utilities
// ---------------------------------------------------------------------------------------------
template <class OutputIt, class... Args>
int std_format_to_n(OutputIt buffer, std::iter_difference_t<OutputIt> n, std::format_string<Args...> fmt,
                    Args&&... args) {
   auto max_chars  = n - 1;
   auto res        = std::format_to_n(buffer, max_chars, fmt, std::forward<Args>(args)...);
   auto written    = std::min(res.size, max_chars);
   buffer[written] = '\0'; // adds terminator to buffer
   // fprintf(stderr, "%s\n", buffer);
   return (int)written;
}

template< class... Args >
void std_print(std::format_string<Args...> fmt, Args&&... args ) {
   std::ostreambuf_iterator<char> out(std::cout);
   std::format_to(out, fmt, std::forward<Args>(args)...);
}

template <typename... Args>
void print(std::ostream& stream, std::format_string<Args...> fmt, Args&&... args) {
   stream << std::format(fmt, std::forward<Args>(args)...);
}

template <typename... Args>
void print(FILE* f, std::format_string<Args...> fmt, Args&&... args) {
   std::string formatted = std::format(fmt, std::forward<Args>(args)...);
   fprintf(f, "%s", formatted.c_str());
}

std::string LoadFile(const char* path); // load whole file into string

// --------------------------------------------------
// Definitions.
// --------------------------------------------------

#define _UI_TO_STRING_1(x) #x
#define _UI_TO_STRING_2(x) _UI_TO_STRING_1(x)

namespace ui_size {

inline constexpr int button_minimum_width = 100;
inline constexpr int button_padding       = 16;
inline constexpr int button_height        = 27;
inline constexpr int button_checked_area  = 4;

inline constexpr int checkbox_box = 14;
inline constexpr int checkbox_gap = 8;

inline constexpr int menu_item_height        = 24;
inline constexpr int menu_item_minimum_width = 160;
inline constexpr int menu_item_margin        = 9;

inline constexpr int gauge_width  = 200;
inline constexpr int gauge_height = 22;

inline constexpr int slider_width  = 200;
inline constexpr int slider_height = 25;
inline constexpr int slider_thumb  = 15;
inline constexpr int slider_track  = 5;

inline constexpr int textbox_margin = 3;
inline constexpr int textbox_width  = 200;
inline constexpr int textbox_height = 27;

inline constexpr int tab_pane_space_top  = 2;
inline constexpr int tab_pane_space_left = 4;

inline constexpr int splitter = 8;

inline constexpr int scroll_bar           = 16;
inline constexpr int scroll_minimum_thumb = 20;

inline constexpr int table_header     = 26;
inline constexpr int table_column_gap = 20;
inline constexpr int table_row        = 20;

inline constexpr int pane_large_border  = 20;
inline constexpr int pane_large_gap     = 10;
inline constexpr int pane_medium_border = 5;
inline constexpr int pane_medium_gap    = 5;
inline constexpr int pane_small_border  = 3;
inline constexpr int pane_small_gap     = 3;

inline constexpr int mdi_child_border         = 6;
inline constexpr int mdi_child_title          = 30;
inline constexpr int mdi_child_corner         = 12;
inline constexpr int mdi_child_minimum_width  = 100;
inline constexpr int mdi_child_minimum_height = 50;
inline constexpr int mdi_cascade              = 30;

} // namespace ui_size

// forward declarations
// --------------------
namespace UIUpdate {
   enum {
      hovered  = 1,
      pressed  = 2,
      focused  = 3,
      disabled = 4
   };
}

struct UI;
struct UIElement;
struct UIWindow;
struct UIPanel;
struct UIButton;
struct UICheckbox;
struct UILabel;
struct UISpacer;
struct UISplitPane;
struct UISplitter;
struct UITabPane;
struct UIScrollBar;
struct UIScrollbarPair;
struct UICode;
struct UIGauge;
struct UISlider;
struct UITable;
struct UITextbox;
struct UIMenu;
struct UIMDIClient;
struct UIMDIChild;
struct UIImageDisplay;
struct UIWrapPanel;
struct UISwitcher;

// ------------------------------------------------------------------------------------------
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

// ------------------------------------------------------------------------------------------
struct UIPoint {
   int x = 0;
   int y = 0;
};

// ------------------------------------------------------------------------------------------
struct UIRectangle {
   int l, r, t, b;

   UIRectangle() {}
   UIRectangle(int v) : l(v), r(v), t(v), b(v) {}
   UIRectangle(int lr, int tb) : l(lr), r(lr), t(tb), b(tb) {}
   UIRectangle(int l, int r, int t, int b)  : l(l), r(r), t(t), b(b) {}

   int     width()  const { return r - l; }
   int     height() const { return b - t; }
   UIPoint dims() const { return {width(), height()}; }
   UIPoint center() const { return { l + width() / 2, t + height() / 2 }; }

   bool    valid()  const { return l < r && t < b; }
   bool    contains(const UIPoint& p) const { return p.x >= l && p.x < r && p.y >= t && p.y < b; }
   bool    contains(int x, int y) const { return x >= l && x < r && y >= t && y < b; }

   friend UIRectangle intersection(const UIRectangle& a, const UIRectangle& b) {
      return {std::max(a.l, b.l), std::min(a.r, b.r), std::max(a.t, b.t), std::min(a.b, b.b)};
   }

   friend UIRectangle bounding(const UIRectangle& a, const UIRectangle& b) {
      return {std::min(a.l, b.l), std::max(a.r, b.r), std::min(a.t, b.t), std::max(a.b, b.b)};
   }

   friend UIRectangle add(const UIRectangle& a, const UIRectangle& b) {
      return { a.l + b.l, a.r + b.r, a.t + b.t, a.b + b.b };
   }

   friend UIRectangle translate(const UIRectangle& a, const UIRectangle& b) {
      return { a.l + b.l, a.r + b.l, a.t + b.t, a.b + b.t };
   }

   friend UIRectangle center(const UIRectangle& parent, UIRectangle child) {
      auto c = parent.center();
      auto [childWidth, childHeight] = child.dims();
      child.l = c.x - childWidth  / 2, child.r = child.l + childWidth;
      child.t = c.y - childHeight / 2, child.b = child.t + childHeight;
      return child;
   }

   friend UIRectangle fit(const UIRectangle& parent, UIRectangle child, bool allowScalingUp);

   UIRectangle operator+(const UIRectangle& o) const { return { l + o.l, r + o.r, t + o.t, b + o.b }; }

   // following APIs  use `UIRectangle& o` as widths for left,right, top, bottom, instead of a proper rect.
   // -----------------------------------------------------------------------------------------------------
   UIRectangle shrink(const UIRectangle& o) const { return {l + o.l, r - o.r, t + o.t, b - o.b}; }
   int         total_width() const { return r + l; }
   int         total_height() const { return b + t; }

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

inline UIRectangle ui_rect_1(int x)         { return UIRectangle{x, x, x, x}; }
inline UIRectangle ui_rect_1i(int x)        { return UIRectangle{x, -x, x, -x}; }
inline UIRectangle ui_rect_2i(int x, int y) { return UIRectangle{x, -x, y, -y}; }
inline UIRectangle ui_rect_2s(int x, int y) { return UIRectangle{0, x, 0, y}; }
inline UIRectangle ui_rect_4pd(int x, int y, int w, int h) { return UIRectangle{x, (x + w), y, (y + h)}; }

#define UI_RECT_SIZE(_r) (_r).width(), (_r).height()

// ------------------------------------------------------------------------------------------
struct UITheme {
   uint32_t panel1 = 0, panel2 = 0, selected = 0, border = 0;
   uint32_t text = 0, textDisabled = 0, textSelected = 0;
   uint32_t buttonNormal = 0, buttonHovered = 0, buttonPressed = 0, buttonDisabled = 0;
   uint32_t textboxNormal = 0, textboxFocused = 0;
   uint32_t codeFocused = 0, codeBackground = 0, codeDefault = 0, codeComment = 0, codeString = 0, codeNumber = 0,
            codeOperator = 0, codePreprocessor = 0;
   uint32_t accent1 = 0, accent2 = 0;
};

// ------------------------------------------------------------------------------------------
struct UIPainter {
   UIRectangle clip   = UIRectangle(0);
   uint32_t*   bits   = nullptr;
   uint32_t    width  = 0;
   uint32_t    height = 0;
#ifdef UI_DEBUG
   int fillCount = 0;
#endif
};

struct UIFontSpec {
   std::string path;
   uint32_t    size;

   bool operator==(const UIFontSpec&) const = default;
};

// ------------------------------------------------------------------------------------------
template<>
struct std::hash<UIFontSpec>
{
    std::size_t operator()(const UIFontSpec& spec) const noexcept
    {
        std::size_t h1 = std::hash<std::string>{}(spec.path);
        return h1 ^ (spec.size << 1);
    }
};

// ------------------------------------------------------------------------------------------
struct UIFont {
   int glyphWidth = 0;
   int glyphHeight = 0;

#ifdef UI_FREETYPE
   bool    isFreeType = false;
   FT_Face font       = nullptr;

   unique_ptr<FT_Bitmap[]> glyphs;
   unique_ptr<bool[]>      glyphsRendered;
   unique_ptr<int[]>       glyphOffsetsX;
   unique_ptr<int[]>       glyphOffsetsY;
#endif

   ~UIFont();
};

// ------------------------------------------------------------------------------------------
struct UIShortcut {
   UIKeycode             code  = static_cast<UIKeycode>(0);
   bool                  ctrl  = false;
   bool                  shift = false;
   bool                  alt   = false;
   std::function<void()> invoke;
};

// ------------------------------------------------------------------------------------------
struct UIStringSelection {
   int      carets[2];
   uint32_t colorText       = 0;
   uint32_t colorBackground = 0;
};

// ------------------------------------------------------------------------------------------
struct UIKeyTyped {
   std::string_view text;
   UIKeycode        code = static_cast<UIKeycode>(0);
};

// ------------------------------------------------------------------------------------------
struct UITableGetItem {
   std::string buffer;
   int         index      = 0;
   int         column     = 0;
   bool        isSelected = false;

   UITableGetItem(size_t buffer_size) {
      buffer.resize(buffer_size);
   }

   template <class... Args>
   int format_to(std::format_string<Args...> fmt, Args&&... args) {
      return std_format_to_n(&buffer[0], buffer.size(), fmt, std::forward<Args>(args)...);
   }

   std::string_view buff(int num_chars) { return std::string_view(&buffer[0], (size_t)num_chars); }
   size_t           buff_size() const { return buffer.size(); }
};

// ------------------------------------------------------------------------------------------
struct UICodeDecorateLine {
   UIRectangle bounds;
   int         index; // Starting at 1!
   int         x, y;  // Position where additional text can be drawn.
   UIPainter*  painter = nullptr;
};

inline float ui_color_alpha_f(uint32_t x) { return static_cast<float>(((x >> 24) & 0xFF)) / 255.0f; }
inline float ui_color_red_f(uint32_t x)   { return static_cast<float>(((x >> 16) & 0xFF)) / 255.0f; }
inline float ui_color_green_f(uint32_t x) { return static_cast<float>(((x >> 8)  & 0xFF)) / 255.0f; }
inline float ui_color_blue_f(uint32_t x)  { return static_cast<float>(((x >> 0)  & 0xFF)) / 255.0f; }
inline float ui_color_alpha(uint32_t x)   { return static_cast<float>(((x >> 24) & 0xFF)); }
inline float ui_color_red(uint32_t x)     { return static_cast<float>(((x >> 16) & 0xFF)); }
inline float ui_color_green(uint32_t x)   { return static_cast<float>(((x >> 8)  & 0xFF)); }
inline float ui_color_blue(uint32_t x)    { return static_cast<float>(((x >> 0)  & 0xFF)); }

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


// ------------------------------------------------------------------------------------------
namespace UIControl {
enum {
   push_button      = 1,
   drop_down        = 2,
   menu_item        = 3,
   checkbox         = 4,
   label            = 5,
   splitter         = 6,
   scroll_track     = 7,
   scroll_up        = 8,
   scroll_down      = 9,
   scroll_thumb     = 10,
   gauge            = 11,
   slider           = 12,
   textbox          = 13,
   modal_popup      = 14,
   menu             = 15,
   table_row        = 16,
   table_cell       = 17,
   table_background = 18,
   table_header     = 19,
   mdi_child        = 20,
   tab              = 21,
   tab_band         = 22,

   type_mask           = 0xFF,
   state_selected      = 1 << 24,
   state_vertical      = 1 << 25,
   state_indeterminate = 1 << 26,
   state_checked       = 1 << 27,
   state_hovered       = 1 << 28,
   state_focused       = 1 << 29,
   state_pressed       = 1 << 30,
   state_disabled      = 1 << 31,
};
}

// ------------------------------------------------------------------------------------------
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

// ------------------------------------------------------------------------------------------
enum class UIAlign : uint32_t {
   left   = 1,
   right  = 2,
   center = 3,
};

using  message_proc_t = int(*)(UIElement*, UIMessage msg, int di, void* dp); // data integer, data pointer

// ------------------------------------------------------------------------------------------
struct UIElement {
protected:
   void _destroy_descendents(bool topLevel);
   bool _destroy();

public:
   enum {
      v_fill           = 1 << 16,
      h_fill           = 1 << 17,
      hv_fill          = v_fill | h_fill,
      window_flag      = 1 << 18,
      parent_push_flag = 1 << 19,
      tab_stop_flag    = 1 << 20,
      non_client_flag  = 1 << 21, // Don't destroy in destroy_descendents(), like scroll bars.
      disabled_flag    = 1 << 22, // Don't receive input events.
      border_flag      = 1 << 23,
      vertical_flag    = 1 << 24,

      hide_flag                = 1 << 27,
      relayout_flag            = 1 << 28,
      relayout_descendent_flag = 1 << 29,
      destroy_flag             = 1 << 30,
      destroy_descendent_flag  = 1 << 31
   };

   uint32_t                _flags   = 0; // First 16 bits are element specific.
   uint32_t                _id      = 0;
   uint32_t                _unused0 = 0;
   UIElement*              _parent  = nullptr;
   UIWindow*               _window  = nullptr;
   std::vector<UIElement*> _children;
   UIRectangle             _bounds;
   UIRectangle             _clip;
   void*                   _cp         = nullptr; // Context pointer (for user).
   message_proc_t          _class_proc = nullptr;
   message_proc_t          _user_proc  = nullptr;
   const char*             _class_name = nullptr;

   UIElement(UIElement* parent, uint32_t flags, message_proc_t message_proc, const char* cClassName);
   virtual ~UIElement();

   uint32_t       state() const;

   bool           animate(bool stop);
   void           destroy();                  // really just set flags so they'll be destroyed in `_UIUpdate`
   void           destroy_descendents();      // really just set flags so they'll be destroyed in `_UIUpdate`
   int            message(UIMessage msg, int di, void* dp);
   UIElement*     change_parent(UIElement* newParent, UIElement* insertBefore);
   UIElement*     next_or_previous_sibling(bool previous);
   UIElement&     parent() { return *_parent; }
   void           measurements_changed(int which);

   void           refresh();
   void           relayout();
   void           repaint(const UIRectangle* region);
   void           paint(UIPainter* painter);

   void           focus();                    // sets the input focus to this element
   void           set_disabled(bool disabled);

   void           move(UIRectangle bounds, bool layout);
   UIElement*     find_by_point(int x, int y);
   UIRectangle    screen_bounds();            // Returns bounds of element in same coordinate system as used by UIWindowCreate.

   int            scale(auto sz) const;

   message_proc_t get_class_proc() const { return _class_proc; }

   UIElement&     set_user_proc(message_proc_t proc) { _user_proc = proc; return *this; }
   message_proc_t user_proc() const { return _user_proc; }

   
   // functions to create child UI elements
   // -------------------------------------
   UIButton&       add_button(uint32_t flags, std::string_view label);
   UICheckbox&     add_checkbox(uint32_t flags, std::string_view label);
   UICode&         add_code(uint32_t flags);
   UIGauge&        add_gauge(uint32_t flags);
   UIImageDisplay& add_imagedisplay(uint32_t flags, uint32_t* bits, size_t width, size_t height, size_t stride);
   UILabel&        add_label(uint32_t flags, std::string_view label);
   UIMDIChild&     add_mdichild(uint32_t flags, UIRectangle initialBounds, std::string_view title);
   UIMDIClient&    add_mdiclient(uint32_t flags);
   UIMenu&         add_menu(uint32_t flags);
   UIPanel&        add_panel(uint32_t flags);
   UIScrollBar&    add_scrollbar(uint32_t flags);
   UISlider&       add_slider(uint32_t flags);
   UISpacer&       add_spacer(uint32_t flags, int width, int height);
   UISplitPane&    add_splitpane(uint32_t flags, float weight);
   UISwitcher&     add_switcher(uint32_t flags);
   UITabPane&      add_tabpane(uint32_t flags, const char* tabs);
   UITable&        add_table(uint32_t flags, const char* columns);
   UITextbox&      add_textbox(uint32_t flags);
   UIWrapPanel&    add_wrappanel(uint32_t flags);
};

// ------------------------------------------------------------------------------------------
// Class used for the `set_` functions which return the object itself. This uses `crtp` so
// that a reference to the derived type is returned, not to `UIElement`.
// ------------------------------------------------------------------------------------------
template <class Derived>
struct UIElementCast : public UIElement{
   UIElementCast(UIElement* parent, uint32_t flags, message_proc_t message_proc, const char* cClassName) :
      UIElement(parent, flags, message_proc, cClassName)
   {}

   Derived& set_user_proc(message_proc_t proc) { return static_cast<Derived&>(UIElement::set_user_proc(proc)); }
};

// ------------------------------------------------------------------------------------------
struct UIConfig {
   std::string font_path;
   uint32_t    default_font_size = 13;
   bool        rfu = false;
};

enum class sel_target_t { primary, clipboard };

// ------------------------------------------------------------------------------------------
struct UIWindow : public UIElementCast<UIWindow> {
private:
   static int _ClassMessageProcCommon(UIElement* el, UIMessage msg, int di, void* dp);
   friend struct UI;

public:
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);

   enum {
      MENU            = (1 << 0),
      INSPECTOR       = (1 << 1),
      CENTER_IN_OWNER = (1 << 2),
      MAXIMIZE        = (1 << 3),
   };

   UIElement*              _dialog;
   std::vector<UIShortcut> _shortcuts;
   float                   _scale;
   std::vector<uint32_t>   _bits;
   uint32_t                _width;
   uint32_t                _height;
   UIWindow*               _next;
   UIElement*              _hovered;
   UIElement*              _pressed;
   UIElement*              _focused;
   UIElement*              _dialog_old_focus;
   int                     _pressed_button;
   UIPoint                 _cursor;
   int                     _cursor_style;

   // Set when a textbox is modified.
   // Useful for tracking whether changes to the loaded document have been saved.
   bool        _textbox_modified_flag;
   bool        _ctrl;
   bool        _shift;
   bool        _alt;
   UIRectangle _update_region;

#ifdef UI_DEBUG
   float _last_full_fill_count = 0;
#endif

#ifdef UI_LINUX
   Window   _xwindow     = 0;
   XImage*  _image       = nullptr;
   XIC      _xic         = nullptr;
   unsigned _ctrl_code   = 0;
   unsigned _shift_code  = 0;
   unsigned _alt_code    = 0;
   Window   _drag_source = 0;
#endif

#ifdef UI_WINDOWS
   HWND _hwnd           = 0;
   bool _tracking_leave = false;
#endif

   UIWindow(UIElement* parent, uint32_t flags, message_proc_t message_proc, const char* cClassName);
   virtual ~UIWindow();

   void endpaint(UIPainter* painter);
   void set_cursor(int cursor);
   void get_screen_position(int* _x, int* _y);
   void set_pressed(UIElement* el, int button);
   bool input_event(UIMessage message, int di, void* dp);
};

inline int UIElement::scale(auto sz) const { return (int)((float)sz * _window->_scale); }

// ------------------------------------------------------------------------------------------
struct UIPanel : public UIElementCast<UIPanel> {
private:
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);

public:
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

   UIScrollBar* _scrollBar;
   UIRectangle  _border;
   int          _gap;

   UIPanel(UIElement* parent, uint32_t flags);

   UIPanel& set_border(const UIRectangle& b) { _border = b; return *this; }
   const UIRectangle& border() const { return _border; }

   UIPanel& set_gap(int gap) { _gap = gap;  return *this; }
   int gap() const { return _gap; }
};

// ------------------------------------------------------------------------------------------
struct UIButton : public UIElementCast<UIButton> {
public:
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);

   enum {
      SMALL     = 1 << 0,
      MENU_ITEM = 1 << 1,
      CAN_FOCUS = 1 << 2,
      DROP_DOWN = 1 << 3,
      CHECKED   = 1 << 15,
   };

   std::string           label;
   std::function<void()> invoke;

   UIButton(UIElement* parent, uint32_t flags, std::string_view label);
};

// ------------------------------------------------------------------------------------------
struct UICheckbox : public UIElementCast<UICheckbox> {
private:
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);

public:
   enum { ALLOW_INDETERMINATE = 1 << 0 };

   enum { UNCHECKED = 0, CHECKED = 1, INDETERMINATE = 2 };

   uint8_t               check;
   std::string           label;
   std::function<void()> invoke;

   UICheckbox(UIElement* parent, uint32_t flags, std::string_view label);

   void set_label(std::string_view label);
};

// ------------------------------------------------------------------------------------------
struct UILabel : public UIElementCast<UILabel> {
private:
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);
   
public:
   std::string _label;

   UILabel(UIElement* parent, uint32_t flags, std::string_view label);
};

// ------------------------------------------------------------------------------------------
struct UISpacer : public UIElementCast<UISpacer> {
private:
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);

public:
   size_t   _width;
   size_t   _height;

   UISpacer(UIElement* parent, uint32_t flags, int width, int height);
};

// ------------------------------------------------------------------------------------------
struct UISplitPane : public UIElementCast<UISplitPane> {
private:
public:
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);

   float     _weight;

   UISplitPane(UIElement* parent, uint32_t flags, float weight);
};

// ------------------------------------------------------------------------------------------
struct UISplitter {
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);

};

// ------------------------------------------------------------------------------------------
struct UITabPane : public UIElementCast<UITabPane> {
private:
   std::string _tabs;
   uint32_t    _active;

public:
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);

   UITabPane(UIElement* parent, uint32_t flags,  const char* tabs);

   void     set_active(uint32_t idx) { _active = idx; }
   uint32_t get_active() const { return _active; }

   template <class F>
   void for_each_tab(F&& f) {
      uint32_t position = 0;
      uint32_t index    = 0;
      while (true) {
         uint32_t end = position;
         for (; _tabs[end] != '\t' && _tabs[end]; end++)
            ;
         std::string_view tab_text{&_tabs[0] + position, end - position};
         if (!std::forward<F>(f)(tab_text, index, _active == index) || _tabs[end] == 0)
            break;
         position = end + 1;
         index++;
      }
   }
};

// ------------------------------------------------------------------------------------------
struct UIScrollBar : public UIElementCast<UIScrollBar> {
private:
   int64_t    _maximum;
   int64_t    _page;
   int64_t    _drag_offset;

   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);

public:
   enum { HORIZONTAL = 1 << 0 };

   int64_t    _position;
   UI_CLOCK_T _last_animate_time;
   bool       _in_drag;
   bool       _horizontal;

   UIScrollBar(UIElement* parent, uint32_t flags);

   void    set_maximum(int64_t m) { _maximum = m; }
   int64_t maximum() const { return _maximum; }

   void    set_page(int64_t m) { _page = m; }
   int64_t page() const { return _page; }

   void    set_drag_offset(int64_t m) { _drag_offset = m; }
   int64_t drag_offset() const { return _drag_offset; }
};

// ------------------------------------------------------------------------------------------
struct UIScrollbarPair {
protected:
   UIScrollBar* _vscroll;
   UIScrollBar* _hscroll;

   UIScrollbarPair(UIElement* el)
      : _vscroll(new UIScrollBar(el, 0))
      , _hscroll(new UIScrollBar(el, UIScrollBar::HORIZONTAL)) {}

   void layout_scrollbar_pair(int hSpace, int vSpace, int scrollBarSize, UIElement* el);
   void key_input_vscroll(UIKeyTyped* m, int rowHeight, int pageHeight, UIElement* el);

public:
   void reset_vscroll() { _vscroll->_position = 0; }
};

// ------------------------------------------------------------------------------------------
struct UICode : public UIElementCast<UICode>, public UIScrollbarPair {
private:
   struct code_pos {
      size_t line   = 0;
      size_t offset = 0;
   };

   struct code_line {
      size_t offset;
      size_t bytes;
   };

   std::vector<char>       _content;
   std::vector<code_line>  _lines;
   std::optional<size_t>   _current_line{0}; // if set, 0 <= currentLine < lines.size()
   size_t                  _focus_line{0};
   UIFont*                 _font;
   bool                    _move_scroll_to_focus_next_layout{false};
   bool                    _move_scroll_to_caret_next_layout{false};
   int                     _tab_columns{4};
   size_t                  _max_columns{0};
   UI_CLOCK_T              _last_animate_time{0};
   bool                    _left_down_in_margin{false};
   int                     _vertical_motion_column{0};
   bool                    _use_vertical_motion_column{false};
   std::array<code_pos, 4> _selection{}; // start, end, anchor, caret

   UICode& _set_vertical_motion_column(bool restore);
   UICode& _update_selection();

   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);

public:
   enum { NO_MARGIN = 1 << 0, SELECTABLE = 1 << 1 };

   UICode(UIElement* parent, uint32_t flags);

   UICode&   clear();
   UICode&   insert_content(std::string_view new_content, bool replace);
   UICode&   load_file(const char* path, std::optional<std::string_view> err = {});

   std::string_view line(size_t line) const {
      const auto& l = _lines[line];
      return std::string_view{&_content[l.offset], l.bytes};
   }
   size_t    line_offset(size_t line) const { return _lines[line].offset; }
   
   size_t    num_lines() const { return _lines.size(); }
   size_t    size() const { return _content.size(); }

   code_pos  selection(size_t idx) const { assert(idx < _selection.size()); return _selection[idx]; }
   
   void      emplace_back_line(size_t offset, size_t bytes) {
      if (bytes > _max_columns)
         _max_columns = bytes;
      _lines.emplace_back(offset, bytes);
   }

   UICode&    move_caret(bool backward, bool word);
   UICode&    position_to_byte(int x, int y, size_t* line, size_t* byte);
   int        hittest(int x, int y);

   UICode&    set_tab_columns(uint32_t sz) { _tab_columns = sz; return *this; }
   uint32_t   tab_columns() const { return _tab_columns; }

   UICode&    set_focus_line(size_t index);                                        // Line numbers are 0-based
   size_t     focus_line() const { return _focus_line; }

   bool       left_down_in_margin() const { return _left_down_in_margin; }

   UICode&    set_last_animate_time(UI_CLOCK_T t) { _last_animate_time = t; return *this; }
   UI_CLOCK_T last_animate_time() const { return _last_animate_time; }

   size_t     max_columns() const { return _max_columns; }
      
   UICode&    set_current_line(std::optional<size_t> l) {
      if (!l || *l < num_lines())
         _current_line = l;
      return *this;
   }
   std::optional<size_t> current_line() {
      if (_current_line && *_current_line >= num_lines())
         _current_line.reset();
      return _current_line;
   }

   const char& operator[](size_t idx) const { return _content[idx]; }

   size_t     offset(const code_pos& pos) { return _lines[pos.line].offset + pos.offset; }

   UICode&    set_font(UIFont* font) { _font = font; return *this; }
   UIFont*    font() const { return _font; }

   UICode&    copy_text(sel_target_t t);

   int        column_to_byte(size_t ln, size_t column) const;
   int        byte_to_column(size_t ln, size_t byte) const;
};

// ------------------------------------------------------------------------------------------
struct UIGauge : public UIElementCast<UIGauge> {
   double _position;
   bool   _vertical;

   UIGauge(UIElement* parent, uint32_t flags);

   UIGauge& set_position(double position);
   double   position() const { return _position; }

   bool     vertical() const { return _vertical; }
};

// ------------------------------------------------------------------------------------------
struct UISlider : public UIElementCast<UISlider> {
   double _position;
   int    _steps;
   bool   _vertical;

   UISlider(UIElement* parent, uint32_t flags);

   UISlider& set_position(double position);
   double    position() const { return _position; }

   bool      vertical() const { return _vertical; }

   UISlider& set_steps(int steps) { _steps = steps; return *this; }
   int       steps() const { return _steps; }
};

// ------------------------------------------------------------------------------------------
struct UITable : public UIElementCast<UITable>, public UIScrollbarPair {
private:
   size_t              _num_items;
   std::string         _columns;       // list of column headers separated by '\t' characters
   std::vector<size_t> _column_widths;
   size_t              _column_highlight;
   
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);

public:
   UITable(UIElement* parent, uint32_t flags, const char* columns);

   size_t column_end(size_t start) const {
      size_t end = start;
      for (; _columns[end] != '\t' && _columns[end]; ++end)
         ;
      return end;
   }

   std::string_view column(size_t start, size_t end) const {
      return std::string_view{_columns.c_str() + start, end - start};
   }

   int      hittest(int x, int y);
   int      header_hittest(int x, int y);
   bool     ensure_visible(int index);
   size_t&  num_items()                    { return _num_items; }
   UITable& set_num_items(size_t n)        { _num_items = n; return *this; }
   UITable& set_column_highlight(size_t c) { _column_highlight = c; return *this; }
   UITable& resize_columns();
};

// ------------------------------------------------------------------------------------------
struct UITextbox : public UIElementCast<UITextbox> {
   std::string        buffer;
   std::array<int, 2> carets{};
   int                scroll;
   bool               rejectNextKey;

   UITextbox(UIElement* parent, uint32_t flags);
   
   std::string_view text() const { return std::string_view(buffer); }
};

// ------------------------------------------------------------------------------------------
struct UIMenu : public UIElementCast<UIMenu> {
   enum {
      PLACE_ABOVE = 1 << 0,
      NO_SCROLL   = 1 << 1
   };

   int          pointX; // keep as int for X11 APIs
   int          pointY;
   UIScrollBar* vScroll;
   UIWindow*    parentWindow;

   UIMenu(UIElement* parent, uint32_t flags);
};

struct UIMDIChild;

// ------------------------------------------------------------------------------------------
struct UIMDIClient : public UIElementCast<UIMDIClient> {
private:
   UIMDIChild* _active;
   int         _cascade;
   
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);
   friend struct UIMDIChild;

public:
   enum { _TRANSPARENT = 1 << 0 };

   UIMDIClient(UIElement* parent, uint32_t flags);
};

// ------------------------------------------------------------------------------------------
struct UIMDIChild : public UIElementCast<UIMDIChild> {
private:
   UIRectangle _mdi_bounds;
   std::string _title;
   int         _drag_hit_test;
   UIRectangle _drag_offset;

   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);
   friend struct UIMDIClient;
   
public:
   enum { CLOSE_BUTTON = 1 << 0 };

   UIMDIChild(UIElement* parent, uint32_t flags, const UIRectangle& initialBounds, std::string_view title);
};

// ------------------------------------------------------------------------------------------
struct UIImageDisplay : public UIElementCast<UIImageDisplay> {
   enum { INTERACTIVE = (1 << 0), ZOOM_FIT = (1 << 1) };

   uint32_t* bits;
   int       width;
   int       height;
   float     panX;
   float     panY;
   float     zoom;

   // Internals:
   int previousWidth;
   int previousHeight;
   int previousPanPointX;
   int previousPanPointY;

   UIImageDisplay(UIElement* parent, uint32_t flags, uint32_t* bits, size_t width, size_t height, size_t stride);
};

// ------------------------------------------------------------------------------------------
struct UIWrapPanel : public UIElementCast<UIWrapPanel> {
private:
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);

public:
   UIWrapPanel(UIElement* parent, uint32_t flags);
};

// ------------------------------------------------------------------------------------------
struct UISwitcher : public UIElementCast<UISwitcher> {
private:
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp);

public:
   UIElement* active = nullptr;

   UISwitcher(UIElement* parent, uint32_t flags);
};

// ------------------------------------------------------------------------------------------
unique_ptr<UI> UIInitialise(const UIConfig& cfg);

int  UIMessageLoop();

UIElement* UIElementCreate(size_t bytes, UIElement* parent, uint32_t flags,
                           int (*messageClass)(UIElement*, UIMessage, int, void*), const char* cClassName);

UICheckbox*   UICheckboxCreate(UIElement* parent, uint32_t flags, std::string_view label);
UIMDIClient*  UIMDIClientCreate(UIElement* parent, uint32_t flags);
UIMDIChild*   UIMDIChildCreate(UIElement* parent, uint32_t flags, UIRectangle initialBounds, std::string_view title);
UIPanel*      UIPanelCreate(UIElement* parent, uint32_t flags);
UIScrollBar*  UIScrollBarCreate(UIElement* parent, uint32_t flags);
UISlider*     UISliderCreate(UIElement* parent, uint32_t flags);
UISpacer*     UISpacerCreate(UIElement* parent, uint32_t flags, int width, int height);
UISplitPane*  UISplitPaneCreate(UIElement* parent, uint32_t flags, float weight);
UITabPane*    UITabPaneCreate(UIElement* parent, uint32_t flags,
                              const char* tabs /* separate with \t, terminate with \0 */);
UIWrapPanel*  UIWrapPanelCreate(UIElement* parent, uint32_t flags);

UIGauge* UIGaugeCreate(UIElement* parent, uint32_t flags);

UIButton* UIButtonCreate(UIElement* parent, uint32_t flags, std::string_view label);
void      UIButtonSetLabel(UIButton* button, std::string_view string);

UILabel*  UILabelCreate(UIElement* parent, uint32_t flags, std::string_view label);
void      UILabelSetContent(UILabel* code, std::string_view content);

UIImageDisplay* UIImageDisplayCreate(UIElement* parent, uint32_t flags, uint32_t* bits, size_t width, size_t height,
                                     size_t stride);
void UIImageDisplaySetContent(UIImageDisplay* display, uint32_t* bits, size_t width, size_t height, size_t stride);

UISwitcher* UISwitcherCreate(UIElement* parent, uint32_t flags);
void        UISwitcherSwitchTo(UISwitcher* switcher, UIElement* child);

UIWindow* UIWindowCreate(UIWindow* owner, uint32_t flags, const char* cTitle, int width, int height);
void      UIWindowRegisterShortcut(UIWindow* window, UIShortcut shortcut);
void      UIWindowPostMessage(UIWindow* window, UIMessage msg, void* dp); // Thread-safe.
void      UIWindowPack(UIWindow* window, int width); // Change the size of the window to best match its contents.

typedef void (*UIDialogUserCallback)(UIElement*);
const char* UIDialogShow(UIWindow* window, uint32_t flags, const char* format, ...);

UIMenu* UIMenuCreate(UIElement* parent, uint32_t flags);
void    UIMenuAddItem(UIMenu* menu, uint32_t flags, std::string_view label, std::function<void ()> invoke);
void    UIMenuShow(UIMenu* menu);
bool    UIMenusOpen();

UITextbox* UITextboxCreate(UIElement* parent, uint32_t flags);
void       UITextboxReplace(UITextbox* textbox, std::string_view text, bool sendChangedMessage);
void       UITextboxClear(UITextbox* textbox, bool sendChangedMessage);
void       UITextboxMoveCaret(UITextbox* textbox, bool backward, bool word);
void       UITextboxCopyText(void* cp);
void       UITextboxPasteText(void* cp, sel_target_t t);

UITable* UITableCreate(UIElement* parent, uint32_t flags,
                       const char* columns /* separate with \t, terminate with \0 */);

UICode* UICodeCreate(UIElement* parent, uint32_t flags);

void UIDrawBlock(UIPainter* painter, UIRectangle rectangle, uint32_t color);
void UIDrawCircle(UIPainter* painter, int centerX, int centerY, int radius, uint32_t fillColor, uint32_t outlineColor,
                  bool hollow);
void UIDrawControl(UIPainter* painter, UIRectangle bounds, uint32_t mode /* UI_DRAW_CONTROL_* */,
                   std::string_view label, double position, float scale);
void UIDrawControlDefault(UIPainter* painter, UIRectangle bounds, uint32_t mode, std::string_view label,
                          double position, float scale);
void UIDrawInvert(UIPainter* painter, UIRectangle rectangle);
bool UIDrawLine(UIPainter* painter, int x0, int y0, int x1, int y1,
                uint32_t color); // Returns false if the line was not visible.
void UIDrawTriangle(UIPainter* painter, int x0, int y0, int x1, int y1, int x2, int y2, uint32_t color);
void UIDrawTriangleOutline(UIPainter* painter, int x0, int y0, int x1, int y1, int x2, int y2, uint32_t color);
void UIDrawGlyph(UIPainter* painter, int x, int y, int c, uint32_t color);
void UIDrawRectangle(UIPainter* painter, UIRectangle r, uint32_t mainColor, uint32_t borderColor,
                     UIRectangle borderSize);
void UIDrawBorder(UIPainter* painter, UIRectangle r, uint32_t borderColor, UIRectangle borderSize);
void UIDrawString(UIPainter* painter, UIRectangle r, std::string_view string, uint32_t color, UIAlign align,
                  UIStringSelection* selection);
int  UIDrawStringHighlighted(UIPainter* painter, UIRectangle r, std::string_view string, int tabSize,
                             UIStringSelection* selection); // Returns final x position.

int UIMeasureStringWidth(std::string_view string);
int UIMeasureStringHeight();

uint64_t UIAnimateClock(); // In ms.

UIElement* UIParentPush(UIElement* el);
UIElement* UIParentPop();

UIRectangle UIRectangleFit(UIRectangle parent, UIRectangle child, bool allowScalingUp);

bool UIColorToHSV(uint32_t rgb, float* hue, float* saturation, float* value);
void UIColorToRGB(float hue, float saturation, float value, uint32_t* rgb);

char* UIStringCopy(const char* in, ptrdiff_t inBytes);

UIFont* UIFontCreate(const char* cPath, uint32_t size);
UIFont* UIFontActivate(UIFont* font); // Returns the previously active font.

struct UIInspector {
   static constexpr bool _enabled = (bool)UI_DEBUG;
   UIWindow* _inspector = nullptr;
   UITable*  _table     = nullptr;
   UIWindow* _target    = nullptr;
   UICode*   _log       = nullptr;

   static constexpr bool enabled() { return _enabled; }
   void create();
   void set_focused_window(UIWindow* window);
   void notify_destroyed_window(UIWindow* window);
   void refresh();
};

struct UI {
public:
   static void        ClipboardWriteText(UIWindow* window, std::string text, sel_target_t t);
   static std::string ClipboardReadText(UIWindow* window, sel_target_t t);
   static bool        MessageLoopSingle(int* result);
   static void        InspectorRefresh();
   static void        Update();
   static void        ProcessAnimations();

   static int byte_to_column(std::string_view string, size_t byte, size_t tabSize);
   static int column_to_byte(std::string_view string, size_t column, size_t tabSize);

   static bool is_digit(int c) { return std::isdigit(c); }
   static bool is_alpha(int c) { return std::isalpha(c) || c > 127; }
   static bool is_alnum(int c) { return std::isalnum(c) || c > 127; }
   static bool is_alnum_or_underscore(int c) { return is_alnum(c) || c == '_'; }

public:
   UIWindow* windows = nullptr;
   UITheme   theme;

   std::vector<UIElement*> animating;

   std::array<UIElement*, 16> parentStack{};
   int                        parentStackCount = 0;

   bool        quit           = false;
   const char* dialogResult   = nullptr;
   UIElement*  dialogOldFocus = nullptr;
   bool        dialogCanExit  = false;

   std::string default_font_path;     // default font used
   UIFont*     activeFont  = nullptr;
   UIFont*     defaultFont = nullptr;

   UIInspector inspector;
   
#ifdef UI_LINUX
   using cursors_t   = std::array<Cursor, (uint32_t)UICursor::count>;

   Display*    display        = nullptr;
   Visual*     visual         = nullptr;
   XIM         xim            = nullptr;
   Atom        windowClosedID = 0, primaryID = 0, uriListID = 0, plainTextID = 0;
   Atom        dndEnterID = 0, dndPositionID = 0, dndStatusID = 0, dndActionCopyID = 0;
   Atom        dndDropID = 0, dndSelectionID = 0, dndFinishedID = 0, dndAwareID = 0;
   Atom        clipboardID = 0, xSelectionDataID = 0, textID = 0, targetID = 0, incrID = 0;
   cursors_t   cursors{};
   std::string pasteText;
   XEvent      copyEvent;
#endif

#ifdef UI_WINDOWS
   using cursors_t = std::array<HCURSOR, (uint32_t)UICursor::count>;

   cursors_t cursors{};
   HANDLE    heap             = 0;
   bool      assertionFailure = false;
#endif

#ifdef UI_FREETYPE
   FT_Library ft = nullptr;
#endif

   std::unordered_map<UIFontSpec, unique_ptr<UIFont>> font_map;

   ~UI() {
      font_map.clear();
#ifdef UI_FREETYPE
      FT_Done_FreeType(ft);
#endif
   }

   int code_margin() { return activeFont->glyphWidth * 5; }
   int code_margin_gap() { return activeFont->glyphWidth * 1; }

   UIWindow& add_window(UIWindow* owner, uint32_t flags, const char* cTitle, int width, int height);

   
};

// ----------------------------------------
#ifdef UI_DEBUG
template< class... Args >
void UIInspectorLog(UI* ui, std::format_string<Args...> fmt, Args&&... args ) {
   char buffer[4096];
   std_format_to_n(buffer, sizeof(buffer), fmt, std::forward<Args>(args)...);
   ui->inspector._log->insert_content(buffer, false);
   ui->inspector._log->refresh();
}
#endif

// ----------------------------------------
//      Forward declarations.
// ----------------------------------------

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


// ----------------------------------------
//      Variables
// ----------------------------------------

extern UI*     ui;              // global pointer to the UIInitialise return value
extern UITheme uiThemeClassic;
extern UITheme uiThemeDark;
