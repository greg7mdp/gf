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
#include <cctype>
#include <charconv>
#include <functional>
#include <memory>
#include <string>
#include <iostream>
#include <format>
#include <unordered_map>

using std::make_shared;
using std::make_unique;
using std::shared_ptr;
using std::unique_ptr;

#define UI_DEBUG 0
#define UI_AUTOMATION_TESTS 1

#ifdef UI_LINUX
   #include <X11/Xlib.h>
   #include <X11/Xutil.h>
   #include <X11/Xatom.h>
   #include <X11/cursorfont.h>
   #include <ctime>
   #include <cmath>

   #define UI_ASSERT assert
   #define UI_CLOCK _UIClock
   #define UI_CLOCKS_PER_SECOND 1000
   #define UI_CLOCK_T clock_t

inline UI_CLOCK_T _UIClock() {
   struct timespec spec;
   clock_gettime(CLOCK_REALTIME, &spec);
   return spec.tv_sec * 1000 + spec.tv_nsec / 1000000;
}
#endif

#ifdef UI_WINDOWS
   #undef _UNICODE
   #undef UNICODE
   #include <windows.h>
   #include <shellapi.h>
   #undef min
   #undef max

   #define UI_ASSERT assert
   #define UI_CLOCK GetTickCount
   #define UI_CLOCKS_PER_SECOND (1000)
   #define UI_CLOCK_T DWORD
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
   A          = XK_A,
   ZERO       = XK_0,
   BACKSPACE  = XK_BackSpace,
   DEL        = XK_Delete,
   DOWN       = XK_Down,
   END        = XK_End,
   ENTER      = XK_Return,
   ESCAPE     = XK_Escape,
   F1         = XK_F1,
   HOME       = XK_Home,
   LEFT       = XK_Left,
   RIGHT      = XK_Right,
   SPACE      = XK_space,
   TAB        = XK_Tab,
   UP         = XK_Up,
   INSERT     = XK_Insert,
   BACKTICK   = XK_grave,
   PAGE_UP    = XK_Page_Up,
   PAGE_DOWN  = XK_Page_Down,
   UNDERSCORE = XK_underscore
};
#endif

#ifdef UI_WINDOWS
enum class UIKeycode : int {
   A          = 'A',
   ZERO       = '0',
   BACKSPACE  = VK_BACK,
   DEL        = VK_DELETE,
   DOWN       = VK_DOWN,
   END        = VK_END,
   ENTER      = VK_RETURN,
   ESCAPE     = VK_ESCAPE,
   F1         = VK_F1,
   HOME       = VK_HOME,
   LEFT       = VK_LEFT,
   RIGHT      = VK_RIGHT,
   SPACE      = VK_SPACE,
   TAB        = VK_TAB,
   UP         = VK_UP,
   INSERT     = VK_INSERT,
   BACKTICK   = VK_OEM_3,
   PAGE_UP    = VK_PRIOR,
   PAGE_DOWN  = VK_NEXT,
   UNDERSCORE = 0x5f
};
#endif

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

template <class... Args>
void std_print(std::format_string<Args...> fmt, Args&&... args) {
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

template<class T>
inline T ui_atoi(std::string_view sv) {
   T result{};
   auto [ptr, ec] = std::from_chars(sv.data(), sv.data() + sv.size(), result);
   if (ec == std::errc())
      return result;
   return 0;
}

template<class T>
inline T ui_atof(std::string_view sv) {
   T result{};
   auto [ptr, ec] = std::from_chars(sv.data(), sv.data() + sv.size(), result);
   if (ec == std::errc())
      return result;
   return 0;
}

// ---------------------------------------------------------------------------
// assigns val to var, and returns true if the value changed
// ---------------------------------------------------------------------------
template <class T, class V>
bool ui_set(T& var, V&& val) noexcept(std::is_nothrow_move_assignable_v<T> && std::is_nothrow_copy_assignable_v<T>) {
   if (var != val) {
      var = std::forward<V>(val);
      return true;
   }
   return false;
}

// ---------------------------------------------------------------------------
// An object which calls a lambda in its  destructor
//
// This object must be captured in a local variable, Otherwise, since it is a
// temporary, it will be destroyed immediately, thus calling the function.
//
//          scoped_guard rollback(...); // good
// ---------------------------------------------------------------------------
template <class F>
class scoped_guard {
public:
   [[nodiscard]] scoped_guard(F&& unset, bool do_it = true) noexcept(std::is_nothrow_move_constructible_v<F>)
      : do_it_(do_it)
      , unset_(std::move(unset)) {}

   ~scoped_guard() {
      if (do_it_)
         unset_();
   }

   void dismiss() noexcept { do_it_ = false; }

   scoped_guard(scoped_guard&&)                 = delete;
   scoped_guard(const scoped_guard&)            = delete;
   scoped_guard& operator=(const scoped_guard&) = delete;
   void*         operator new(std::size_t)      = delete;

private:
   bool do_it_;
   F    unset_;
};

// --------------------------------------------------
// Definitions.
// --------------------------------------------------

#define _UI_TO_STRING_1(x) #x
#define _UI_TO_STRING_2(x) _UI_TO_STRING_1(x)

using ui_handle = uintptr_t;

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
struct UI;
struct UIButton;
struct UICheckbox;
struct UICode;
struct UIElement;
struct UIGauge;
struct UIImageDisplay;
struct UILabel;
struct UIMDIChild;
struct UIMDIClient;
struct UIMenu;
struct UIPainter;
struct UIPanel;
struct UIScrollBar;
struct UIScrollbarPair;
struct UISlider;
struct UISpacer;
struct UISplitPane;
struct UISplitter;
struct UISwitcher;
struct UITabPane;
struct UITable;
struct UITextbox;
struct UIWindow;
struct UIWrapPanel;

// clang-format off

namespace UIUpdate {
   enum {
      hovered  = 1,
      pressed  = 2,
      focused  = 3,
      disabled = 4
   };
}

// ------------------------------------------------------------------------------------------
enum class UIMessage : uint32_t {
   // General messages.
   PAINT,                 // dp = pointer to UIPainter
   PAINT_FOREGROUND,      // after children have painted
   LAYOUT,
   DESTROY,
   DEALLOCATE,
   UPDATE,                // di = UI_UPDATE_... constant
   ANIMATE,
   SCROLLED,
   GET_WIDTH,             // di = height (if known); return width
   GET_HEIGHT,            // di = width (if known); return height
   GET_CHILD_STABILITY,   // dp = child element; return stable axes, 1 (width) | 2 (height)

   // Input events.
   INPUT_EVENTS_START,    // not sent to disabled elements
   LEFT_UP,
   MIDDLE_UP,
   RIGHT_UP,
   LEFT_DOWN,
   MIDDLE_DOWN,
   RIGHT_DOWN,
   LEFT_DBLCLICK,         // this one and the following 5 have to stay in order
   MIDDLE_DBLCLICK,
   RIGHT_DBLCLICK,
   LEFT_TRIPLECLICK,
   MIDDLE_TRIPLECLICK,
   RIGHT_TRIPLECLICK,
   KEY_TYPED,             // dp = pointer to UIKeyTyped; return 1 if handled
   KEY_RELEASED,          // dp = pointer to UIKeyTyped; return 1 if handled
   MOUSE_MOVE,
   MOUSE_DRAG,
   MOUSE_WHEEL,           // di = delta; return 1 if handled
   CLICKED,
   GET_CURSOR,            // return cursor code
   PRESSED_DESCENDENT,    // dp = pointer to child that is/contains pressed element
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

   bool approx_equal(const UIPoint&o, int diff = 6) const {
      return std::abs(x - o.x) <= diff && std::abs(y - o.y) <= diff;
   }
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
struct UIFontSpec {
   std::string _path;
   uint32_t    _size;

   bool operator==(const UIFontSpec&) const = default;
};

// ------------------------------------------------------------------------------------------
template<>
struct std::hash<UIFontSpec> {
   std::size_t operator()(const UIFontSpec& spec) const noexcept {
       std::size_t h1 = std::hash<std::string>{}(spec._path);
       return h1 ^ (spec._size << 1);
   }
};

// ------------------------------------------------------------------------------------------
struct UIFont {
   UI* _ui           = nullptr;
   
   int _glyph_width  = 0;
   int _glyph_height = 0;

#ifdef UI_FREETYPE
   bool                    _is_freetype = false;
   FT_Face                 _font        = nullptr;
   
   unique_ptr<FT_Bitmap[]> _glyphs;
   unique_ptr<bool[]>      _glyphs_rendered;
   unique_ptr<int[]>       _glyphs_offsets_x;
   unique_ptr<int[]>       _glyphs_offsets_y;
#endif

   UIFont* activate();    // returns previously active font

   ~UIFont();
};

// ------------------------------------------------------------------------------------------
struct UIShortcut {
   UIKeycode             code  = static_cast<UIKeycode>(0);
   bool                  ctrl  = false;
   bool                  shift = false;
   bool                  alt   = false;
   std::function<bool()> invoke; // should return true if event handled
};

// ------------------------------------------------------------------------------------------
struct UIStringSelection {
   int      carets[2];
   uint32_t colorText       = 0;
   uint32_t colorBackground = 0;
};

// ------------------------------------------------------------------------------------------
inline UIKeycode UI_KEYCODE_LETTER(char x) { return (UIKeycode)((int)UIKeycode::A + (x - 'A')); }
inline UIKeycode UI_KEYCODE_DIGIT(char x)  { return (UIKeycode)((int)UIKeycode::ZERO + (x - '0')); }
inline UIKeycode UI_KEYCODE_FKEY(char x)   { return (UIKeycode)((int)UIKeycode::F1 + (x - 1)); }

// ------------------------------------------------------------------------------------------
struct UIKeyTyped {
   std::string_view text;
   UIKeycode        code = static_cast<UIKeycode>(0);

   bool is(char c) { /*assert(std::islower(c));*/ return code == UI_KEYCODE_LETTER(c) || code == UI_KEYCODE_LETTER(c - ('a' - 'A')); }
};

// ------------------------------------------------------------------------------------------
struct UITableGetItem {
   std::string _buffer;
   size_t      _row         = 0;
   size_t      _column      = 0;
   bool        _is_selected = false;

   UITableGetItem(size_t buffer_size) {
      _buffer.resize(buffer_size);
   }

   UITableGetItem(size_t buffer_size, size_t row, size_t column)
      : UITableGetItem(buffer_size) {
      _row    = row;
      _column = column;
   }

   template <class... Args>
   int format_to(std::format_string<Args...> fmt, Args&&... args) {
      return std_format_to_n(&_buffer[0], _buffer.size(), fmt, std::forward<Args>(args)...);
   }

   std::string_view buff(int num_chars) { return std::string_view(&_buffer[0], (size_t)num_chars); }
   size_t           buff_size() const { return _buffer.size(); }
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
   #define draw_control draw_control_default
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
      window_flag      = 1 << 18,  // `parent` passed to `UIElement` constructor is the `UIWindow`, parent should be set to nullptr
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
   UIPoint        cursor_pos() const;
   
   void           refresh();
   void           relayout();
   void           repaint(const UIRectangle* region);
   void           paint(UIPainter* painter);

   UIElement&     focus();                     // sets the input focus to this element
   void           set_disabled(bool disabled);

   void           move(UIRectangle bounds, bool layout);
   UIElement*     find_by_point(int x, int y);
   UIRectangle    screen_bounds();            // Returns bounds of element in same coordinate system as used by UIWindowCreate.

   int            scale(auto sz) const;

   message_proc_t get_class_proc() const { return _class_proc; }

   UIElement&     set_user_proc(message_proc_t proc) { _user_proc = proc; return *this; }
   message_proc_t user_proc() const { return _user_proc; }

   UIElement&     set_cp(void* cp) { _cp = cp; return *this; }
   UIElement&     clear_flag(uint32_t flag) { _flags &= ~flag; return *this; }
   UIElement&     set_flag(uint32_t flag)   { _flags |= flag; return *this; }

   bool           is_hovered() const;
   bool           is_focused() const;
   bool           is_pressed() const;
   bool           is_disabled() const { return !!(_flags & disabled_flag); }

   UI*            ui() const;
   UITheme&       theme() const;                 // indirect access to `UI`
   UIFont*        active_font() const;           // indirect access to `UI`
   
   // functions to create child UI elements (alphabetical order)
   // ----------------------------------------------------------
   UIButton&       add_button(uint32_t flags, std::string_view label);
   UICheckbox&     add_checkbox(uint32_t flags, std::string_view label);
   UICode&         add_code(uint32_t flags);
   UIElement&      add_element(uint32_t flags, message_proc_t message_proc, const char* cClassName);
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
   UISplitter&     add_splitter(uint32_t flags);
   UISwitcher&     add_switcher(uint32_t flags);
   UITabPane&      add_tabpane(uint32_t flags, const char* tabs);  // tabs: separate with \t, terminate with \0 
   UITable&        add_table(uint32_t flags, const char* columns); // tabs: separate with \t, terminate with \0 
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
   Derived& focus() { return static_cast<Derived&>(UIElement::focus()); }
   Derived& set_cp(void* cp) { return static_cast<Derived&>(UIElement::set_cp(cp)); }
   Derived& clear_flag(uint32_t flag) { return static_cast<Derived&>(UIElement::clear_flag(flag)); }
   Derived& set_flag(uint32_t flag) { return static_cast<Derived&>(UIElement::set_flag(flag)); }

   template<class... F>
   Derived&      add_n(F&&... f) {
      (std::forward<F>(f)(*this), ...);
      return static_cast<Derived&>(*this);
   }
};

// ------------------------------------------------------------------------------------------
struct UIConfig {
   std::string font_path;
   uint32_t    default_font_size = 13;
   bool        _has_theme        = false;
   UITheme     _theme;
   bool        rfu = false;
};

enum class sel_target_t : int { primary = 0, clipboard = 1 };

// ------------------------------------------------------------------------------------------
struct UIWindow : public UIElementCast<UIWindow> {
private:
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
   bool                    _textbox_modified_flag;
   bool                    _urgent;
   UIRectangle             _update_region;
   UI*                     _ui;

   void       _init_toplevel();
   int        _class_message_proc_common(UIMessage msg, int di, void* dp);

   static int _ClassMessageProcCommon(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UIWindow*>(el)->_class_message_proc_common(msg, di, dp);
   }

public:
   friend struct UI;

   enum {
      MENU            = (1 << 0),
      INSPECTOR       = (1 << 1),
      CENTER_IN_OWNER = (1 << 2),
      MAXIMIZE        = (1 << 3),
   };

   bool        _ctrl;
   bool        _shift;
   bool        _alt;

#ifdef UI_DEBUG
   float _last_full_fill_count = 0;
#endif

#if defined(UI_LINUX)
   Window   _xwindow     = 0;
   XImage*  _image       = nullptr;
   XIC      _xic         = nullptr;
   unsigned _ctrl_code   = 0;
   unsigned _shift_code  = 0;
   unsigned _alt_code    = 0;
   Window   _drag_source = 0;
#elif defined(UI_WINDOWS)
   HWND _hwnd           = 0;
   bool _tracking_leave = false;

   static LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);
#endif

   UIWindow(UI* ui, UIElement* parent, uint32_t flags, message_proc_t message_proc, const char* cClassName);
   virtual ~UIWindow();

   void        endpaint(UIPainter* painter) const;
   void        get_screen_position(int* _x, int* _y) const;
   UIWindow&   set_cursor(int cursor);

   UIWindow&   set_pressed(UIElement* el, int button);
   UIElement*  pressed() const { return _pressed; }

   UIWindow&   set_hovered(UIElement* el) { _hovered = el; return *this; }
   UIElement*  hovered() const { return _hovered; }

   UIWindow&   set_focused(UIElement* el) { _focused = el; return *this; }
   UIElement*  focused() const { return _focused; }

   UIWindow&   set_dialog_old_focus(UIElement* e) { _dialog_old_focus = e; return *this; }
   UIElement*  dialog_old_focus() const { return _dialog_old_focus; }

   UIWindow&   set_textbox_modified_flag(bool b) { _textbox_modified_flag = b;  return *this; }
   bool        textbox_modified_flag() const { return _textbox_modified_flag; }
   
   int         pressed_button() const { return _pressed_button; }

   UIWindow&   set_next(UIWindow* w) { _next = w; return *this; }
   UIWindow*   next() const { return _next; }
   
   UIWindow&   set_scale(float scale) { _scale = scale; return *this; }
   float       scale() const { return _scale; }

   std::vector<uint32_t>& bits() { return _bits; }

   uint32_t    width()   const { return _width; }
   uint32_t    height()  const { return _height; }
   UIWindow&   set_size(uint32_t w, uint32_t h) { _width = w; _height = h; return *this; }

   UIWindow&   set_cursor_pos(UIPoint pt) { _cursor = pt; return *this; }
   UIPoint     cursor_pos() const { return _cursor; }

   UIWindow&   set_update_region(const UIRectangle& r) { _update_region = r; return *this; }
   const UIRectangle& update_region() const { return _update_region; }

   std::string_view   show_dialog(uint32_t flags, const char* format, ...);

   int         cursor_style() const { return _cursor_style; }

   UIWindow&   register_shortcut(UIShortcut shortcut);

   bool        input_event(UIMessage message, int di, void* dp);

   void        write_clipboard_text(std::string_view text, sel_target_t t);
   std::string read_clipboard_text(sel_target_t t);

   void        post_message(UIMessage msg, void* _dp) const;

   UIWindow&   set_name(std::string_view name);
   
   UI*         ui() const { assert(_ui == _window->_ui); return _window->_ui; }

   UIWindow&   set_urgent(bool urgent);
   UIWindow&   grab_focus();                // when a breakpoint is hit for example

   // ------ delete functions from UIElement we shouldn't use on a UIWindow --------------
   bool        is_hovered() const = delete; // do not call on UIWindow. only on UIElement
   bool        is_focused() const = delete; // do not call on UIWindow. only on UIElement
   bool        is_pressed() const = delete; // do not call on UIWindow. only on UIElement

#if defined(UI_LINUX)
   ui_handle native_window() const { return _xwindow; }
#elif defined(UI_WINDOWS)
   ui_handle native_window() const { return _hwnd; }
#endif
};

// ------------------------------- need UIWindow to be defined -------------------------------
inline int     UIElement::scale(auto sz) const { return (int)((float)sz * _window->scale()); }
inline bool    UIElement::is_hovered() const { return _window->hovered() == this; }
inline bool    UIElement::is_focused() const { return _window->focused() == this; }
inline bool    UIElement::is_pressed() const { return _window->pressed() == this; }
inline UIPoint UIElement::cursor_pos() const { return _window->cursor_pos(); }
inline UI*     UIElement::ui() const         { return _window->ui(); }

// ------------------------------------------------------------------------------------------
struct UIPanel : public UIElementCast<UIPanel> {
private:
   UIScrollBar* _scrollBar;
   UIRectangle  _border;
   int          _gap;

   int        _class_message_proc(UIMessage msg, int di, void* dp);
   
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UIPanel*>(el)->_class_message_proc(msg, di, dp);
   }

   int _layout(UIRectangle bounds, bool measure);
   int _calculate_per_fill(int* _count, int hSpace, int vSpace, float scale);
   int _measure(int di);
   
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

   UIPanel(UIElement* parent, uint32_t flags);

   UIPanel& set_border(const UIRectangle& b) { _border = b; return *this; }
   const UIRectangle& border() const { return _border; }

   UIPanel& set_gap(int gap) { _gap = gap;  return *this; }
   int gap() const { return _gap; }

   UIScrollBar* scrollbar() const { return _scrollBar; }
};

// ------------------------------------------------------------------------------------------
struct UIButton : public UIElementCast<UIButton> {
private:
   std::string                    _label;
   std::function<void(UIButton&)> _on_click;

   int        _class_message_proc(UIMessage msg, int di, void* dp);

public:
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UIButton*>(el)->_class_message_proc(msg, di, dp);
   }

   enum {
      SMALL     = 1 << 0,
      MENU_ITEM = 1 << 1,
      CAN_FOCUS = 1 << 2,
      DROP_DOWN = 1 << 3,
      CHECKED   = 1 << 15,
   };

   UIButton(UIElement* parent, uint32_t flags, std::string_view label);

   UIButton& on_click(std::function<void(UIButton&)> f) { _on_click = std::move(f); return *this; }

   UIButton& set_label(std::string_view s); 
   std::string_view label() const { return _label; }
};

// ------------------------------------------------------------------------------------------
struct UICheckbox : public UIElementCast<UICheckbox> {
private:
   bool                             _checked;     // value that is updated by default, unless `track` is called
   bool*                            _checked_ptr;
   std::string                      _label;
   std::function<void(UICheckbox&)> _on_click;

   int        _class_message_proc(UIMessage msg, int di, void* dp);
   
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UICheckbox*>(el)->_class_message_proc(msg, di, dp);
   }

public:
   UICheckbox(UIElement* parent, uint32_t flags, std::string_view label);

   UICheckbox&      set_label(std::string_view label);
   std::string_view label() const     { return _label; }

   UICheckbox&      set_checked(bool b) { if (ui_set(*_checked_ptr,  b)) repaint(nullptr); return *this; }
   bool             checked() const     { return *_checked_ptr; }

   UICheckbox&      track(bool* val_ptr) {
      bool cur = *_checked_ptr;
      _checked_ptr  = val_ptr ? val_ptr : &_checked;
      *_checked_ptr = cur; // inherits the current value of the checkbox
      return *this; 
   }

   UICheckbox&      on_click(std::function<void(UICheckbox&)> f) { _on_click = std::move(f); return *this; }
};

// ------------------------------------------------------------------------------------------
struct UILabel : public UIElementCast<UILabel> {
private:
   std::string _label;

   int        _class_message_proc(UIMessage msg, int di, void* dp);
   
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UILabel*>(el)->_class_message_proc(msg, di, dp);
   }
   
public:
   UILabel(UIElement* parent, uint32_t flags, std::string_view label);

   UILabel&         set_label(std::string_view label);
   std::string_view label() const     { return _label; }
};

// ------------------------------------------------------------------------------------------
struct UISpacer : public UIElementCast<UISpacer> {
private:
   size_t   _width;
   size_t   _height;

   int        _class_message_proc(UIMessage msg, int di, void* dp);

   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UISpacer*>(el)->_class_message_proc(msg, di, dp);
   }

public:
   UISpacer(UIElement* parent, uint32_t flags, int width, int height);

   size_t width()  const { return _width; }
   size_t height() const { return _height; }
};

// ------------------------------------------------------------------------------------------
struct UISplitPane : public UIElementCast<UISplitPane> {
private:
   float _weight;

   int        _class_message_proc(UIMessage msg, int di, void* dp);

public:
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UISplitPane*>(el)->_class_message_proc(msg, di, dp);
   }

   UISplitPane(UIElement* parent, uint32_t flags, float weight);

   float       weight() const { return _weight; }
   UISplitPane set_weight(float w) { _weight = w; return *this; }
};

// ------------------------------------------------------------------------------------------
struct UISplitter : public UIElementCast<UISplitter> {
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UISplitter*>(el)->_class_message_proc(msg, di, dp);
   }

private:
   int        _class_message_proc(UIMessage msg, int di, void* dp);

public:
   UISplitter(UIElement* parent, uint32_t flags);
};

// ------------------------------------------------------------------------------------------
struct UITabPane : public UIElementCast<UITabPane> {
private:
   std::string _tabs;
   uint32_t    _active;

   int        _class_message_proc(UIMessage msg, int di, void* dp);
   
public:
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UITabPane*>(el)->_class_message_proc(msg, di, dp);
   }

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
   int64_t    _position;

   int        _class_message_proc(UIMessage msg, int di, void* dp);

   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UIScrollBar*>(el)->_class_message_proc(msg, di, dp);
   }

public:
   enum { HORIZONTAL = 1 << 0 };

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

   int64_t& position() { return _position; }
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
   void reset_vscroll() { _vscroll->position() = 0; }
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

   struct menu_item {
      std::string label;
      std::function<void(std::string_view)> invoke; // invoked on selection
   };

   std::vector<char>       _content;
   std::vector<code_line>  _lines;
   std::optional<size_t>   _current_line{0};                   // if set, 0 <= currentLine < lines.size()
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
   std::array<code_pos, 4> _sel{};                            // start, end (ordered), anchor, caret (unordered)
   std::vector<menu_item>  _menu_items;                       // added to right click menu on selection

   UICode&    _set_vertical_motion_column(bool restore);

   int        _class_message_proc(UIMessage msg, int di, void* dp);
   
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UICode*>(el)->_class_message_proc(msg, di, dp);
   }

public:
   enum { NO_MARGIN = 1 << 0, SELECTABLE = 1 << 1 };

   UICode(UIElement* parent, uint32_t flags);

   UICode&    clear();
   UICode&    insert_content(std::string_view new_content, bool replace);
   UICode&    load_file(const char* path, std::optional<std::string_view> err = {});

   std::string_view line(size_t line) const {
      const auto& l = _lines[line];
      return std::string_view{&_content[l.offset], l.bytes};
   }
   size_t     line_offset(size_t line) const { return _lines[line].offset; }
   
   size_t     num_lines() const { return _lines.size(); }
   size_t     size() const { return _content.size(); }
   bool       empty() const { return _lines.empty(); }

   std::string_view selection() const {
      size_t from = offset(selection(0));
      size_t to   = offset(selection(1));
      return from >= to ?  std::string_view{} : std::string_view{&(*this)[from], to - from};
   }

   code_pos   selection(size_t idx) const { assert(idx < _sel.size()); return _sel[idx]; }
   UICode&    set_selection(size_t idx, size_t line, size_t offset) {
                            assert(idx < _sel.size());  _sel[idx] = {line, offset}; return *this; }
   UICode&    update_selection();
   
   void       emplace_back_line(size_t offset, size_t bytes) {
      if (bytes > _max_columns)
         _max_columns = bytes;
      _lines.emplace_back(offset, bytes);
   }

   UICode&    move_caret(bool backward, bool word);
   code_pos   code_pos_from_point(UIPoint pt);

   int        hittest(int x, int y);
   int        hittest(UIPoint p) { return hittest(p.x, p.y); }

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

   size_t     offset(const code_pos& pos) const { return _lines[pos.line].offset + pos.offset; }

   UICode&    set_font(UIFont* font) { _font = font; return *this; }
   UIFont*    font() const { return _font; }

   UICode&    copy(sel_target_t t);

   int        column_to_byte(size_t ln, size_t column) const;
   int        byte_to_column(size_t ln, size_t byte) const;

   UICode&    add_selection_menu_item(std::string_view label, std::function<void(std::string_view)> invoke) {
      _menu_items.emplace_back(std::string{label}, std::move(invoke)); return *this;
   }
};

// ------------------------------------------------------------------------------------------
struct UIGauge : public UIElementCast<UIGauge> {
private:
   double _position;
   bool   _vertical;

   int        _class_message_proc(UIMessage msg, int di, void* dp);

   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UIGauge*>(el)->_class_message_proc(msg, di, dp);
   }

public:
   UIGauge(UIElement* parent, uint32_t flags);

   UIGauge& set_position(double position);
   double   position() const { return _position; }

   bool     vertical() const { return _vertical; }
};

// ------------------------------------------------------------------------------------------
struct UISlider : public UIElementCast<UISlider> {
private:
   double                         _position;
   int                            _steps;
   bool                           _vertical;
   std::function<void(UISlider&)> _on_value_changed;

   int        _class_message_proc(UIMessage msg, int di, void* dp);

   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UISlider*>(el)->_class_message_proc(msg, di, dp);
   }

public:
   UISlider(UIElement* parent, uint32_t flags);

   UISlider& set_position(double position);
   double    position() const { return _position; }

   bool      vertical() const { return _vertical; }

   UISlider& set_steps(int steps) { _steps = steps; return *this; }
   int       steps() const { return _steps; }

   UISlider& on_value_changed(std::function<void(UISlider&)> f) { _on_value_changed = std::move(f); return *this; }
};

// -----------------------------------------------------------------------------------------
struct UITable : public UIElementCast<UITable>, public UIScrollbarPair {
   using on_getitem_t = std::function<int(UITable&, UITableGetItem&)>;
   using on_click_t   = std::function<void(UITable&)>;

private:
   size_t              _num_items;
   std::string         _columns; // list of column headers separated by '\t' characters
   std::vector<size_t> _column_widths;
   size_t              _column_highlight;
   on_getitem_t        _on_getitem;
   on_click_t          _on_click;

   int        _class_message_proc(UIMessage msg, int di, void* dp);

   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UITable*>(el)->_class_message_proc(msg, di, dp);
   }

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
   int      hittest(UIPoint p) { return hittest(p.x, p.y); }

   int      header_hittest(int x, int y);
   int      header_hittest(UIPoint p) { return header_hittest(p.x, p.y); }

   bool     ensure_visible(int index);
   size_t   num_columns() const            { return _column_widths.size(); }
   size_t&  num_items()                    { return _num_items; }
   UITable& set_num_items(size_t n)        { _num_items = n; return *this; }
   UITable& set_column_highlight(size_t c) { _column_highlight = c; return *this; }
   UITable& resize_columns();

   UITable& on_getitem(on_getitem_t f) { _on_getitem = std::move(f); return *this; }
   UITable& on_click(on_click_t f)     { _on_click = std::move(f); return *this; }
};

// ------------------------------------------------------------------------------------------------
struct textbox_state_t {
   std::string             _buffer;
   std::array<uint32_t, 2> _carets{0, 0}; // carets[0] is the cursor position, carets[1] end of selection
   int                     _scroll{0};

   bool operator==(const textbox_state_t& o) const {
      if (_buffer != o._buffer)
         return false;
      if (_scroll != o._scroll)
         return false;
      if (_carets != o._carets && (_carets[0] != _carets[1] || o._carets[0] != o._carets[1]))
         return false; // just moving the caret does not make the state different
      return true;
   }
};

struct UITextbox : public UIElementCast<UITextbox>, public textbox_state_t  {
private:
   bool                         _reject_next_key{false};
   std::vector<textbox_state_t> _undo_states;
   size_t                       _undo_idx{0};
   
   int     _class_message_proc(UIMessage msg, int di, void* dp);

   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UITextbox*>(el)->_class_message_proc(msg, di, dp);
   }

   int  _byte_to_column(std::string_view string, int byte);
   int  _column_to_byte(std::string_view string, int column);

   void _delete_one(bool backwards, bool by_word);
   void _move_one(bool backwards, bool select, bool by_word);
   void _move_to_end(bool backwards, bool select);
   void _select_all();
   void _save_state();
   void _update_state(const textbox_state_t& s);

public:
   UITextbox(UIElement* parent, uint32_t flags);
   
   std::string_view text() const { return std::string_view(_buffer); }
   
   UITextbox& replace_text(std::string_view text, bool sendChangedMessage);
   UITextbox& clear(bool sendChangedMessage);

   UITextbox& move_caret(bool backward, bool word);
   UITextbox& select_all();
   UITextbox& copy();
   UITextbox& paste(sel_target_t t);
   UITextbox& undo();
   UITextbox& redo();

   UITextbox& set_reject_next_key(bool v) { _reject_next_key = v; return *this; }
};

// ------------------------------------------------------------------------------------------
struct UIMenu : public UIElementCast<UIMenu> {
private:
   UIPoint      _point;            // keep as int for X11 APIs
   UIScrollBar* _vscroll;
   UIWindow*    _parent_window;

   void       _prepare(int* width, int* height);
   int        _class_message_proc(UIMessage msg, int di, void* dp);

   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UIMenu*>(el)->_class_message_proc(msg, di, dp);
   }
   
   static int _MenuItemMessageProc(UIElement* el, UIMessage msg, int di, void* dp);

public:
   enum {
      PLACE_ABOVE = 1 << 0,
      NO_SCROLL   = 1 << 1
   };

   UIMenu(UI* ui, UIElement* parent, uint32_t flags);

   UIMenu& add_item(uint32_t flags, std::string_view label, std::function<void(UIButton&)> invoke);
   UIMenu& show();
};

struct UIMDIChild;

// ------------------------------------------------------------------------------------------
struct UIMDIClient : public UIElementCast<UIMDIClient> {
private:
   UIMDIChild* _active;
   int         _cascade;
   
   int        _class_message_proc(UIMessage msg, int di, void* dp);

   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UIMDIClient*>(el)->_class_message_proc(msg, di, dp);
   }

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

   int        _class_message_proc(UIMessage msg, int di, void* dp);
   
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UIMDIChild*>(el)->_class_message_proc(msg, di, dp);
   }


   friend struct UIMDIClient;

   int hittest(UIPoint pt);
   
public:
   enum { CLOSE_BUTTON = 1 << 0 };

   UIMDIChild(UIElement* parent, uint32_t flags, const UIRectangle& initialBounds, std::string_view title);
};

// ------------------------------------------------------------------------------------------
struct UIImageDisplay : public UIElementCast<UIImageDisplay> {
private:
   int _previousWidth;
   int _previousHeight;
   int _previousPanPointX;
   int _previousPanPointY;

   int        _class_message_proc(UIMessage msg, int di, void* dp);

   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UIImageDisplay*>(el)->_class_message_proc(msg, di, dp);
   }
   
   UIImageDisplay& _update_viewport();

public:
   enum { INTERACTIVE = (1 << 0), ZOOM_FIT = (1 << 1) };

   uint32_t* _bits;
   size_t    _width;
   size_t    _height;
   float     _panX;
   float     _panY;
   float     _zoom;

   UIImageDisplay(UIElement* parent, uint32_t flags, uint32_t* bits, size_t width, size_t height, size_t stride);

   UIImageDisplay& set_content(uint32_t* bits, size_t width, size_t height, size_t stride);
};

// ------------------------------------------------------------------------------------------
struct UIWrapPanel : public UIElementCast<UIWrapPanel> {
private:
   void       _layout_row(uint32_t rowStart, uint32_t rowEnd, int rowY, int rowHeight);
   int        _class_message_proc(UIMessage msg, int di, void* dp);
   
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UIWrapPanel*>(el)->_class_message_proc(msg, di, dp);
   }

public:
   UIWrapPanel(UIElement* parent, uint32_t flags);
};

// ------------------------------------------------------------------------------------------
struct UISwitcher : public UIElementCast<UISwitcher> {
private:
   int        _class_message_proc(UIMessage msg, int di, void* dp);
   
   static int _ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
      return static_cast<UISwitcher*>(el)->_class_message_proc(msg, di, dp);
   }

public:
   UIElement* _active = nullptr;

   UISwitcher(UIElement* parent, uint32_t flags);

   void switch_to(UIElement* child);
};

// ------------------------------------------------------------------------------------------
struct UIInspector {
   static constexpr bool _enabled = (bool)UI_DEBUG;

   UI*       _ui;
   UIWindow* _inspector = nullptr;
   UITable*  _table     = nullptr;
   UIWindow* _target    = nullptr;
   UICode*   _log       = nullptr;

public:
   UIInspector(UI* ui);

   static constexpr bool enabled() { return _enabled; }
   
   void set_focused_window(UIWindow* window);
   void notify_destroyed_window(UIWindow* window);
   void refresh();
};

// ------------------------------------------------------------------------------------------
struct UI {
private:
   using font_map_t = std::unordered_map<UIFontSpec, unique_ptr<UIFont>>;
   font_map_t              _font_map;
   UIWindow*               _toplevel_windows = nullptr;
   UITheme                 _theme;
   std::vector<UIElement*> _animating;
   std::string             _default_font_path;
   UIFont*                 _active_font  = nullptr;
   UIFont*                 _default_font = nullptr;
   UIPoint                 _screen_size;

public:
   bool                    _quit             = false;
   const char*             _dialog_result    = nullptr;
   bool                    _dialog_can_exit  = false;

   unique_ptr<UIInspector> _inspector;

#ifdef UI_FREETYPE
   FT_Library              _ft = nullptr;
#endif

   // ------ public functions -------------------------------------------------------------
   ~UI();

   static unique_ptr<UI> initialise(const UIConfig& cfg);  // main entry point of the library

   int          message_loop();
   void         update();
   void         process_animations();
   bool         is_menu_open() const;
   bool         close_menus();
   bool         animate(UIElement *el, bool stop);

   UIMenu&      create_menu(UIElement* parent, uint32_t flags);
   UIWindow&    create_window(UIWindow* owner, uint32_t flags, const char* cTitle, int width, int height);
   UIFont*      create_font(std::string_view path, uint32_t size);

   UITheme&     theme() { return _theme; }
   UIFont*      active_font() const { return _active_font; }
   UIFont*      default_font() const { return _default_font; }
   void         set_active_font(UIFont *font) { _active_font = font; }

   UIPoint      screen_size() const { return _screen_size; }

   ui_handle    get_focus() const;
   void         set_focus(ui_handle window) const;

   const std::string& default_font_path() const { return _default_font_path; }

   UIWindow**   toplevel_windows_head() { return &_toplevel_windows; }

   void         write_clipboard_text(std::string_view text, UIWindow* w, sel_target_t t);
   std::string  read_clipboard_text(UIWindow* w, sel_target_t t);

   // ----------- utilities --------------------------------------------------------
   int          code_margin()     { return _active_font->_glyph_width * 5; }
   int          code_margin_gap() { return _active_font->_glyph_width * 1; }

   int          string_width(std::string_view string) const;
   int          string_height() const { return _active_font->_glyph_height; }
   UIPoint      string_dims(std::string_view s) const { return UIPoint{string_width(s), string_height()}; }

   static int   byte_to_column(std::string_view string, size_t byte, size_t tabSize);
   static int   column_to_byte(std::string_view string, size_t column, size_t tabSize);

   static bool  is_digit(int c) { return std::isdigit(c); }
   static bool  is_alpha(int c) { return std::isalpha(c) || c > 127; }
   static bool  is_alnum(int c) { return std::isalnum(c) || c > 127; }
   static bool  is_alnum_or_underscore(int c) { return is_alnum(c) || c == '_'; }

   // ----------- internal library use - do not call ----------------------------------------
   bool         platform_message_loop_single(int* result);

   //  inspector
   void         inspector_refresh()                            { if (_inspector) _inspector->refresh(); }
   void         inspector_notify_destroyed_window(UIWindow* w) { if (_inspector) _inspector->notify_destroyed_window(w); }
   void         inspector_set_focused_window(UIWindow* w)      { if (_inspector) _inspector->set_focused_window(w); }

   int          automation_run_tests();
   void         automation_process_message();
   void         automation_keyboard_type_single(int code, bool ctrl, bool shift, bool alt);
   void         automation_keyboard_type(const char* string);
   bool         automation_check_code_line_matches(UICode* code, size_t lineIndex, std::string_view input);
   bool         automation_check_table_item_matches(UITable* table, size_t row, size_t column, std::string_view input);

private:
   // ----- internal functions  --------------------------------------------------------------
   void         _initialize_common(const UIConfig& cfg, const std::string& default_font_path);
   UIWindow&    _platform_create_window(UIWindow* owner, uint32_t flags, const char* cTitle, int _width, int _height);
   static int   _platform_message_proc(UIElement* el, UIMessage msg, int di, void* dp);
   void         _inspector_refresh();

   // platform dependent stuff
#if defined(UI_LINUX)
private:
   
   struct selection {
      Atom        _atom;
      std::string _paste_text;

      std::string read_clipboard_text(UI& ui, UIWindow* w);
      void        write_clipboard_text(UI& ui, std::string_view text, UIWindow* w);
      void        process_selection_request(UI& ui, UIWindow* w, XSelectionRequestEvent& event);
   };

   using cursors_t = std::array<Cursor, (uint32_t)UICursor::count>;
   Display*                 _display = nullptr;
   Visual*                  _visual  = nullptr;
   XIM                      _xim     = nullptr;
   std::array<Atom, 17>     _atoms;
   cursors_t                _cursors{};
   std::array<selection, 2> _sel;

   UIWindow*   _find_x11_window(Window window) const;
   bool        _process_x11_event(Display* dpy, XEvent* x_event);
public:
   Display*    native_display() const { return _display; }
   cursors_t&  native_cursors() { return _cursors; }
#elif defined(UI_WINDOWS)
private:
   using cursors_t = std::array<HCURSOR, (uint32_t)UICursor::count>;
   cursors_t               _cursors{};
   bool                    _assertion_failure = false;
public:
   cursors_t&  native_cursors() { return _cursors; }
#endif
};

// ------------------------------------------------------------------------------------------
struct UIPainter {
   UI*             _ui;     // painter holds a `UI` to compute `string_width()` mostly
   UIRectangle     _clip;   // clip to the window rectangle or sub-rectangle  we are drawing on
   uint32_t* const _bits;   // typically the bits of the window we are drawing on, except when offscreen output
   uint32_t        _width;  // full drawable width (_width * _height * sizeof(uint32_t) == sizeof _bits buffer)
   uint32_t        _height; // full drawable height
#ifdef UI_DEBUG
   int             _fill_count = 0; 
#endif

   UIPainter(UI* ui, uint32_t width, uint32_t height, uint32_t* bits)
      : _ui(ui), _clip(ui_rect_2s(width, height)), _bits(bits), _width(width), _height(height) {}

   UIPainter(UIWindow* w);

   UI*        ui() const { return _ui; }
   UITheme&   theme() const { return ui()->theme(); }              // indirect access to `UI`
   UIFont*    active_font() const { return ui()->active_font(); }  // indirect access to `UI`

   UIPainter& draw_glyph(int x0, int y0, int c, uint32_t color);
   UIPainter& draw_block(UIRectangle rectangle, uint32_t color);
   UIPainter& draw_line(int x0, int y0, int x1, int y1, uint32_t color);
   UIPainter& draw_circle(int cx, int cy, int radius, uint32_t fillColor, uint32_t outlineColor, bool hollow);
   UIPainter& draw_triangle(int x0, int y0, int x1, int y1, int x2, int y2, uint32_t color);
   UIPainter& draw_triangle_outline(int x0, int y0, int x1, int y1, int x2, int y2, uint32_t color);
   UIPainter& draw_invert(UIRectangle rectangle);
   UIPainter& draw_string(UIRectangle r, std::string_view string, uint32_t color, UIAlign align,
                          UIStringSelection* selection);
   UIPainter& draw_border(UIRectangle r, uint32_t borderColor, UIRectangle borderSize);
   UIPainter& draw_rectangle(UIRectangle r, uint32_t mainColor, uint32_t borderColor, UIRectangle borderSize);
   UIPainter& draw_control_default(UIRectangle bounds, uint32_t mode, std::string_view label, double position,
                                   float scale);
   int        draw_string_highlighted(UIRectangle lineBounds, std::string_view string, int tabSize,
                                      UIStringSelection* selection);
};
// clang-format on

// ------------------------------------------------------------------------------------------
struct UICodeDecorateLine {
   UIRectangle bounds;
   int         index; // Starting at 1!
   int         x, y;  // Position where additional text can be drawn.
   UIPainter*  painter = nullptr;
};

// ------------------------------------------------------------------------------------------
inline UITheme& UIElement::theme() const { return ui()->theme(); }
inline UIFont*  UIElement::active_font() const { return ui()->active_font(); }

// ------------------------------------------------------------------------------------------

/**/ void UISwitcherSwitchTo(UISwitcher* switcher, UIElement* child);

typedef void (*UIDialogUserCallback)(UIElement*);

uint64_t UIAnimateClock(); // In ms.

bool UIColorToHSV(uint32_t rgb, float* hue, float* saturation, float* value);
void UIColorToRGB(float hue, float saturation, float value, uint32_t* rgb);

// ----------------------------------------
#ifdef UI_DEBUG
template <class... Args>
void UIInspectorLog(UI* ui, std::format_string<Args...> fmt, Args&&... args) {
   char buffer[4096];
   std_format_to_n(buffer, sizeof(buffer), fmt, std::forward<Args>(args)...);
   ui->_inspector->_log->insert_content(buffer, false);
   ui->_inspector->_log->refresh();
}
#endif


// ------------------------------------------------------------------------------------------
// example of use:
//    const std::string config = LoadFile("config.ini");
//    INIFile config_view(config);
//    for (auto [section, key, value] : config_view) {
//        ...                                     // iterate over `std::string_view` triplets
//    }
// ------------------------------------------------------------------------------------------
struct INI_Parser {
   struct parse_result_t {
      std::string_view _section;
      std::string_view _key;
      std::string_view _value;

      template <class T, class F>
      bool parse(std::string_view sv, T& dest, F&& f) {
         if (_key == sv && !_value.empty()) {
            dest = std::forward<F>(f)(_value);
            return true;
         }
         return false;
      }

      bool parse_str(std::string_view sv, std::string& dest) {
         return parse(sv, dest, [](std::string_view s) { return std::string(s); });
      }

      bool parse_int(std::string_view sv, int& dest) {
         return parse(sv, dest, [](std::string_view s) { return ui_atoi<int>(s); });
      }

      bool parse_bool(std::string_view sv, bool& dest) {
         return parse(sv, dest, [](std::string_view s) { return s[0] == '1' || s.substr(0, 4) == "true"; });
      }

      float parse_float(std::string_view sv, float& dest) {
         return parse(sv, dest, [](std::string_view s) { return ui_atof<float>(s); });
      }

      bool operator==(const parse_result_t&) const = default;
      auto  operator<=>(const parse_result_t&) const = default;
   };

   struct iterator {
      using iterator_category = std::input_iterator_tag;
      using value_type        = const parse_result_t;
      using pointer           = value_type*;
      using reference         = value_type&;
      using difference_type   = std::ptrdiff_t;

      iterator()
         : _curr_pos(nullptr)
         , _remaining(0) {}

      iterator(const char* start, size_t bytes)
         : _curr_pos(start)
         , _remaining(bytes) {}

      friend bool operator==(const iterator& a, const iterator& b) { return a._curr_pos == b._curr_pos; }
      friend auto operator<=>(const iterator& a, const iterator& b) { return a._curr_pos <=> b._curr_pos; }

      reference operator*() const {
         assert(_parse_result._section.size());
         return _parse_result;
      }
      pointer operator->() { return &_parse_result; }

      iterator& operator++();
      iterator operator ++ (int) { iterator temp = *this; ++*this; return temp; }

   private:
      const char* _curr_pos;
      size_t      _remaining;

      parse_result_t _parse_result;

      // last key/value parsed
      // ---------------------
      const char* _section       = nullptr; // non-owning ptr
      const char* _key           = nullptr; // non-owning ptr
      const char* _value         = nullptr; // non-owning ptr
      size_t      _section_bytes = 0;
      size_t      _key_bytes     = 0;
      size_t      _value_bytes   = 0;
   };

   // first four are required for `iterator` to be a valid `std::input_iterator`
   static_assert(std::weakly_incrementable<iterator>);
   static_assert(std::movable<iterator>);
   static_assert(std::default_initializable<iterator>);
   static_assert(std::indirectly_readable<iterator>);
   static_assert(std::input_iterator<iterator>);

   using const_iterator = iterator;

   // `buff` *must* be null-terminated, even though it is a `std::string_view`
   INI_Parser(std::string_view buff)
      : _buff(buff) {}

   const_iterator cbegin() const {
      if (_buff.empty())
         return cend();
      iterator res(_buff.data(), _buff.size());
      ++res; // populate first _parse_result so we are at the beginning
      return res;
   }

   const_iterator cend() const {
      return iterator();
   }

   iterator begin() { return cbegin(); }
   iterator end()   { return cend(); }

   parse_result_t parse_next();

private:
   std::string_view _buff;
};



static_assert(std::ranges::input_range<INI_Parser>);

// ----------------------------------------
//      Variables
// ----------------------------------------
extern UITheme uiThemeClassic;
extern UITheme uiThemeDark;
