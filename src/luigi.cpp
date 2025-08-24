#include "luigi.hpp"

#ifdef _MSC_VER
   #pragma warning(disable : 4100) // unreferenced formal parameter
   #pragma warning(disable : 4127) // get_screen_position
   #pragma warning(disable : 4244) // 'initializing': conversion from 'int64_t' to 'int', possible loss of data
   #pragma warning(disable : 4267) // conversion from 'size_t' to '_Ty', possible loss of data
   #pragma warning(disable : 4996) // This function or variable may be unsafe. Consider using ... instead.
   #pragma warning(disable : 5054) // operator '|': deprecated between enumerations of different types
#endif

#ifdef UI_SSE2
   #include <xmmintrin.h>
   #ifdef UI_WINDOWS
      #include <intrin.h>
   #endif
#endif


#include <tuple>
#include <ranges>
#include <algorithm>

namespace views = std::views;
namespace rng   = std::ranges;

// static variables
std::string UITextbox::_hidden_str;


// --------------------------------------------------
// Themes.
// --------------------------------------------------
std::unordered_map<std::string, UITheme> ui_themes{
   {"classic",
    UITheme{
       .panel1   = 0xFFF0F0F0,
       .panel2   = 0xFFFFFFFF,
       .selected = 0xFF94BEFE,
       .border   = 0xFF404040,

       .text         = 0xFF000000,
       .textDisabled = 0xFF404040,
       .textSelected = 0xFF000000,

       .buttonNormal   = 0xFFE0E0E0,
       .buttonHovered  = 0xFFF0F0F0,
       .buttonPressed  = 0xFFA0A0A0,
       .buttonDisabled = 0xFFF0F0F0,

       .textboxNormal  = 0xFFF8F8F8,
       .textboxFocused = 0xFFFFFFFF,

       .codeFocused      = 0xFFE0E0E0,
       .codeBackground   = 0xFFFFFFFF,
       .codeDefault      = 0xFF000000,
       .codeComment      = 0xFFA11F20,
       .codeString       = 0xFF037E01,
       .codeNumber       = 0xFF213EF1,
       .codeOperator     = 0xFF7F0480,
       .codePreprocessor = 0xFF545D70,

       .accent1 = 0xFF0000,
       .accent2 = 0x00FF00,
    }                   },
   {"dark",
    UITheme{
       .panel1   = 0xFF252B31,
       .panel2   = 0xFF14181E,
       .selected = 0xFF94BEFE,
       .border   = 0xFF000000,

       .text         = 0xFFFFFFFF,
       .textDisabled = 0xFF787D81,
       .textSelected = 0xFF000000,

       .buttonNormal   = 0xFF383D41,
       .buttonHovered  = 0xFF4B5874,
       .buttonPressed  = 0xFF0D0D0F,
       .buttonDisabled = 0xFF1B1F23,

       .textboxNormal  = 0xFF31353C,
       .textboxFocused = 0xFF4D4D59,

       .codeFocused      = 0xFF505055,
       .codeBackground   = 0xFF212126,
       .codeDefault      = 0xFFFFFFFF,
       .codeComment      = 0xFFB4B4B4,
       .codeString       = 0xFFF5DDD1,
       .codeNumber       = 0xFFC3F5D3,
       .codeOperator     = 0xFFF5D499,
       .codePreprocessor = 0xFFF5F3D1,

       .accent1 = 0xF01231,
       .accent2 = 0x45F94E,
    }                   },
   {"ice",
    UITheme{
       .panel1           = 0xF1F4FF,
       .panel2           = 0xFFFFFF,
       .selected         = 0xB5D0FE,
       .border           = 0x000000,
       .text             = 0x000000,
       .textDisabled     = 0x787D81,
       .textSelected     = 0x000000,
       .buttonNormal     = 0xEAEDFF,
       .buttonHovered    = 0xF0F8FF,
       .buttonPressed    = 0xB6C5FB,
       .buttonDisabled   = 0x1B1F23,
       .textboxNormal    = 0xFFFFFF,
       .textboxFocused   = 0xFFFFFF,
       .codeFocused      = 0x9CD6FF,
       .codeBackground   = 0xE8F2FF,
       .codeDefault      = 0x000000,
       .codeComment      = 0x7B6F81,
       .codeString       = 0x0F7D32,
       .codeNumber       = 0x0058F5,
       .codeOperator     = 0x720EE7,
       .codePreprocessor = 0x900092,
       .accent1          = 0xFF0000,
       .accent2          = 0x00FF00,
    }                   },
   {"lotus",
    UITheme{
       .panel1           = 0xFFF1F4,
       .panel2           = 0xFFFFFF,
       .selected         = 0xFEB5D0,
       .border           = 0x000000,
       .text             = 0x000000,
       .textDisabled     = 0x81787D,
       .textSelected     = 0x000000,
       .buttonNormal     = 0xFFEAED,
       .buttonHovered    = 0xFFF0F8,
       .buttonPressed    = 0xFBB6C5,
       .buttonDisabled   = 0x231B1F,
       .textboxNormal    = 0xFFFFFF,
       .textboxFocused   = 0xFFFFFF,
       .codeFocused      = 0xFCBAFF,
       .codeBackground   = 0xFFE8F2,
       .codeDefault      = 0x000000,
       .codeComment      = 0x817B6F,
       .codeString       = 0x704697,
       .codeNumber       = 0xC21140,
       .codeOperator     = 0xC76716,
       .codePreprocessor = 0x7A7092,
       .accent1          = 0xFF0000,
       .accent2          = 0x00FF00,
    }                   },
   {"hero",
    UITheme{
       .panel1           = 0x202020,
       .panel2           = 0x202020,
       .selected         = 0x3C3836,
       .border           = 0x404040,
       .text             = 0xDDDDDD,
       .textDisabled     = 0x787D81,
       .textSelected     = 0xFFFFFF,
       .buttonNormal     = 0x202020,
       .buttonHovered    = 0x4B5874,
       .buttonPressed    = 0x0D0D0F,
       .buttonDisabled   = 0x1B1F23,
       .textboxNormal    = 0x202020,
       .textboxFocused   = 0x3C3836,
       .codeFocused      = 0x3C3836,
       .codeBackground   = 0x202020,
       .codeDefault      = 0xBDA175,
       .codeComment      = 0xA8A5A2,
       .codeString       = 0xB3B54A,
       .codeNumber       = 0xD3869B,
       .codeOperator     = 0xBDA175,
       .codePreprocessor = 0xA0B8A0,
       .accent1          = 0xFF0000,
       .accent2          = 0x00FF00,
    }}
};

// ---------------------------------------------------------------------------------------------
//                              Utilities
// ---------------------------------------------------------------------------------------------
std::string LoadFile(std::string_view sv_path) {
   bool null_terminated =
      (sv_path[sv_path.size()] == 0); // ahhh ugly accessing past the end of the `string_view`, hopefully OK.
   std::ifstream ifs(null_terminated ? sv_path.data() : std::string{sv_path}.c_str(), std::ifstream::binary);
   if (ifs.fail())
      return {};

   std::filebuf* pbuf = ifs.rdbuf();
   std::size_t   sz   = pbuf->pubseekoff(0, ifs.end, ifs.in);
   pbuf->pubseekpos(0, ifs.in);

   std::string s;
   s.resize_and_overwrite(sz, [&](char* p, size_t sz) {
      sz = pbuf->sgetn(p, sz);
      return sz;
   });
   s.resize(sz); // shouldn't be necessary

   return s;
}

#if 0
void read_file_contents( const std::filesystem::path& filename, std::string& result )
   {
      std::ifstream f( filename.string(), std::ios::in | std::ios::binary );
      FC_ASSERT(f, "Failed to open ${filename}", ("filename", filename.string()));
      // don't use fc::stringstream here as we need something with override for << rdbuf()
      std::stringstream ss;
      ss << f.rdbuf();
      FC_ASSERT(f, "Failed reading ${filename}", ("filename", filename.string()));
      result = ss.str();
   }
#endif

// --------------------------------------------------
// Member functions.
// --------------------------------------------------
uint32_t UIElement::state() const {
   uint32_t res = 0;
   res |= is_disabled() ? UIControl::state_disabled : 0;
   res |= is_hovered() ? UIControl::state_hovered : 0;
   res |= is_focused() ? UIControl::state_focused : 0;
   res |= is_pressed() ? UIControl::state_pressed : 0;
   return res;
}

// --------------------------------------------------
// Helper functions.
// --------------------------------------------------
#ifdef UI_UNICODE

   #ifndef UI_FREETYPE
      #error "Unicode support requires Freetype"
   #endif

inline constexpr size_t _UNICODE_MAX_CODEPOINT = 0x10FFFF;
inline constexpr int    max_glyphs             = _UNICODE_MAX_CODEPOINT + 1;

int Utf8GetCodePoint(const char* cString, size_t bytesLength, size_t* bytesConsumed) {
   assert(bytesLength > 0 && "Attempted to get UTF-8 code point from an empty string");

   if (bytesConsumed == nullptr) {
      size_t bytesConsumed;
      return Utf8GetCodePoint(cString, bytesLength, &bytesConsumed);
   }

   size_t  numExtraBytes;
   uint8_t first = cString[0];

   *bytesConsumed = 1;
   if ((first & 0xF0) == 0xF0) {
      numExtraBytes = 3;
   } else if ((first & 0xE0) == 0xE0) {
      numExtraBytes = 2;
   } else if ((first & 0xC0) == 0xC0) {
      numExtraBytes = 1;
   } else if (first & 0x7F) {
      return first & 0x80 ? -1 : first;
   } else {
      return -1;
   }

   if (bytesLength < numExtraBytes + 1) {
      return -1;
   }

   int codePoint = ((int)first & (0x3F >> numExtraBytes)) << (6 * numExtraBytes);
   for (size_t idx = 1; idx < numExtraBytes + 1; idx++) {
      char byte = cString[idx];
      if ((byte & 0xC0) != 0x80) {
         return -1;
      }

      codePoint |= (byte & 0x3F) << (6 * (numExtraBytes - idx));
      (*bytesConsumed)++;
   }

   return codePoint > (int)_UNICODE_MAX_CODEPOINT ? -1 : codePoint;
}

const char* Utf8GetPreviousChar(const char* string, const char* offset) {
   if (string == offset) {
      return string;
   }

   const char* prev = offset - 1;
   while (prev > string) {
      if ((*prev & 0xC0) == 0x80)
         prev--;
      else
         break;
   }

   return prev;
}

size_t Utf8GetCharBytes(const char* cString, size_t bytes) {
   if (!cString) {
      return 0;
   }
   size_t bytesConsumed;
   Utf8GetCodePoint(cString, bytes, &bytesConsumed);
   return bytesConsumed;
}

size_t Utf8StringLength(const char* cString, size_t bytes) {
   if (!cString) {
      return 0;
   }
   size_t length    = 0;
   size_t byteIndex = 0;
   while (byteIndex < bytes) {
      size_t bytesConsumed;
      Utf8GetCodePoint(cString + byteIndex, bytes - byteIndex, &bytesConsumed);
      byteIndex += bytesConsumed;
      length++;

      assert(byteIndex <= bytes && "Overran the end of the string while counting the number of UTF-8 code points");
   }

   return length;
}

inline void _ui_advance_char(size_t& index, const char* text, size_t count) {
   assert(count >= index);
   index += Utf8GetCharBytes(text, count - index);
}

inline void _ui_skip_tab(size_t ti, const char* text, size_t bytesLeft, size_t tabSize) {
   int c = Utf8GetCodePoint(text, bytesLeft, nullptr);
   if (c == 't')
      while (ti % tabSize)
         ++ti;
}

template <class T>
inline void _ui_move_caret_backwards(T& caret, const char* text, size_t offset, size_t offset2) {
   const char* prev = Utf8GetPreviousChar(text, text + offset);
   caret            = prev - text - offset2;
}

template <class T>
inline void _ui_move_caret_forward(T& caret, std::string_view text, size_t offset) {
   caret += Utf8GetCharBytes(&text[caret], text.size() - offset);
}

inline bool _ui_move_caret_by_word(std::string_view text, size_t offset) {
   assert(offset <= text.size());
   const char* prev = Utf8GetPreviousChar(&text[0], &text[offset]);
   int         c1   = Utf8GetCodePoint(prev, text.size() - (prev - &text[0]), nullptr);
   int         c2   = Utf8GetCodePoint(&text[offset], text.size() - offset, nullptr);
   return UI::is_alnum_or_underscore(c1) != UI::is_alnum_or_underscore(c2);
}

#else

inline constexpr int max_glyphs = 128;

inline void _ui_advance_char(size_t& index, [[maybe_unused]] const char* text, [[maybe_unused]] size_t count) {
   ++index;
}

inline void _ui_skip_tab(size_t ti, const char* text, [[maybe_unused]] size_t bytesLeft, size_t tabSize) {
   if (*(text) == '\t')
      while (ti % tabSize)
         ++ti;
}

template <class T>
inline void _ui_move_caret_backwards(T& caret, [[maybe_unused]] const char* text, [[maybe_unused]] size_t offset,
                                     [[maybe_unused]] size_t offset2) {
   --caret;
}

template <class T>
inline void _ui_move_caret_forward(T& caret, [[maybe_unused]] std::string_view text, [[maybe_unused]] size_t offset) {
   ++caret;
}

inline bool _ui_move_caret_by_word(std::string_view text, size_t offset) {
   char c1 = (text)[offset - 1];
   char c2 = (text)[offset];
   return (UI::is_alnum_or_underscore(c1) != UI::is_alnum_or_underscore(c2));
}

#endif // UI_UNICODE


UIRectangle fit(const UIRectangle& parent, UIRectangle child, bool allowScalingUp) {
   auto [childWidth, childHeight]   = child.dims();
   auto [parentWidth, parentHeight] = parent.dims();

   if (childWidth < parentWidth && childHeight < parentHeight && !allowScalingUp) {
      return center(parent, child);
   }

   float childAspectRatio   = static_cast<float>(childWidth) / childHeight;
   int   childMaximumWidth  = static_cast<int>(parentHeight * childAspectRatio);
   int   childMaximumHeight = static_cast<int>(parentWidth / childAspectRatio);

   if (childMaximumWidth > parentWidth) {
      return center(parent, ui_rect_2s(parentWidth, childMaximumHeight));
   }
   return center(parent, ui_rect_2s(childMaximumWidth, parentHeight));
}

union _UIConvertFloatInteger {
   float    f;
   uint32_t i;
};

float _UIFloorFloat(float x) {
   _UIConvertFloatInteger convert  = {x};
   uint32_t               sign     = convert.i & 0x80000000;
   int                    exponent = static_cast<int>((convert.i >> 23) & 0xFF) - 0x7F;

   if (exponent >= 23) {
      // There aren't any bits representing a fractional part.
   } else if (exponent >= 0) {
      // Positive exponent.
      uint32_t mask = 0x7FFFFF >> exponent;
      if (!(mask & convert.i))
         return x; // Already an integer.
      if (sign)
         convert.i += mask;
      convert.i &= ~mask; // Mask out the fractional bits.
   } else if (exponent < 0) {
      // Negative exponent.
      return sign ? -1.0f : 0.0f;
   }

   return convert.f;
}

float inverse_lerp(float a, float b, float v) { return (v - a) / (b - a); }

float _UILinearMap(float value, float inFrom, float inTo, float outFrom, float outTo) {
   float normalisedValue = inverse_lerp(inFrom, inTo, value);
   return std::lerp(outFrom, outTo, normalisedValue);
}

bool UIColorToHSV(uint32_t rgb, float* hue, float* saturation, float* value) {
   float r = ui_color_red_f(rgb);
   float g = ui_color_green_f(rgb);
   float b = ui_color_blue_f(rgb);

   float maximum = (r > g && r > b) ? r : (g > b ? g : b), minimum = (r < g && r < b) ? r : (g < b ? g : b),
         difference = maximum - minimum;
   *value           = maximum;

   if (!difference) {
      *saturation = 0;
      return false;
   }
   if (r == maximum)
      *hue = (g - b) / difference + 0;
   if (g == maximum)
      *hue = (b - r) / difference + 2;
   if (b == maximum)
      *hue = (r - g) / difference + 4;
   if (*hue < 0)
      *hue += 6;
   *saturation = difference / maximum;
   return true;
}

void UIColorToRGB(float h, float s, float v, uint32_t* rgb) {
   float r, g, b;

   if (!s) {
      r = g = b = v;
   } else {
      int   h0 = (static_cast<int>(h)) % 6;
      float f  = h - _UIFloorFloat(h);
      float x = v * (1 - s), y = v * (1 - s * f), z = v * (1 - s * (1 - f));

      switch (h0) {
      case 0:
         r = v, g = z, b = x;
         break;
      case 1:
         r = y, g = v, b = x;
         break;
      case 2:
         r = x, g = v, b = z;
         break;
      case 3:
         r = x, g = y, b = v;
         break;
      case 4:
         r = z, g = x, b = v;
         break;
      default:
         r = v, g = x, b = y;
         break;
      }
   }

   *rgb = ui_color_from_rgb(r, g, b);
}

int UI::byte_to_column(std::string_view string, const size_t byte, const size_t tabSize) {
   size_t       ti = 0, i = 0;
   const size_t bytes = string.size();

   while (i < std::min(byte, bytes)) {
      ti++;
      _ui_skip_tab(ti, &string[i], bytes - i, tabSize);
      _ui_advance_char(i, &string[i], byte);
   }

   return static_cast<int>(ti);
}

int UI::column_to_byte(std::string_view string, const size_t column, const size_t tabSize) {
   size_t       byte = 0, ti = 0;
   const size_t bytes = string.size();

   while (byte < bytes) {
      ti++;
      _ui_skip_tab(ti, &string[byte], bytes - byte, tabSize);
      if (column < ti)
         break;

      _ui_advance_char(byte, &string[byte], bytes);
   }

   return static_cast<int>(byte);
}

// --------------------------------------------------
// INI File reader
// --------------------------------------------------
INI_Parser::iterator& INI_Parser::iterator::operator++() {
   // reads into `destination` until character `c1` found (or we reach the end of `_curr_pos`)
   auto ini_read = [this](const char*& destination, size_t& counter, char c1, char c2) {
      destination = _curr_pos, counter = 0;
      while (_remaining && *_curr_pos != c1 && *_curr_pos != c2) {
         ++counter;
         ++_curr_pos;
         --_remaining;
      }
      if (_remaining && *_curr_pos == c1) {
         ++_curr_pos;
         --_remaining;
      }
   };

   while (_remaining) {
      char c = *_curr_pos;

      if (c == ' ' || c == '\n' || c == '\r') {
         _curr_pos++, _remaining--;
         continue;
      }
      if (c == ';') {
         _value_bytes = 0;
         _curr_pos++, _remaining--;
         ini_read(_key, _key_bytes, '\n', 0);
      } else if (c == '[') {
         _key_bytes = _value_bytes = 0;
         _curr_pos++, _remaining--;
         ini_read(_section, _section_bytes, ']', 0);
      } else {
         ini_read(_key, _key_bytes, '=', '\n');
         ini_read(_value, _value_bytes, '\n', 0);
      }

      _parse_result = parse_result_t{._section = ui_trim({_section, _section_bytes}),
                                     ._key     = ui_trim({_key, _key_bytes}),
                                     ._value   = ui_trim({_value, _value_bytes})};
      // on the last [s, k, v], _curr_pos points to the terminating zero character, and iterator still not end()
      return *this;
   }

   // null _curr_pos is end iterator
   if (_remaining == 0 && _curr_pos)
      *this = iterator();
   return *this;
}

// --------------------------------------------------
// INI File updater
// --------------------------------------------------



// --------------------------------------------------
// Animations.
// --------------------------------------------------

bool UIElement::animate(bool stop) { return ui()->animate(this, stop); }

bool UI::animate(UIElement* el, bool stop) {
   if (stop) {
      return !!std::erase(_animating, el);
   }

   if (auto it = std::ranges::find(_animating, el); it != _animating.end())
      return true;

   _animating.push_back(el);
   assert(!el->has_flag(UIElement::destroy_flag));
   return true;
}

uint64_t UIAnimateClock() { return (uint64_t)UI_CLOCK() * 1000 / UI_CLOCKS_PER_SECOND; }

void UI::process_animations() {
   bool do_update = !_animating.empty();

   for (auto el : _animating)
      el->message(UIMessage::ANIMATE, 0, 0);

   if (do_update) {
      update();
   }
}

void UIWindow::write_clipboard_text(std::string_view text, sel_target_t t) { _ui->write_clipboard_text(text, this, t); }

std::string UIWindow::read_clipboard_text(sel_target_t t) { return _ui->read_clipboard_text(this, t); }


// --------------------------------------------------
// Rendering.
// --------------------------------------------------
UIPainter::UIPainter(UIWindow* w)
   : _ui(w->ui())
   , _clip(ui_rect_2s(w->width(), w->height()))
   , _bits(w->bits().data())
   , _width(w->width())
   , _height(w->height()) {}

UIPainter& UIPainter::draw_block(UIRectangle rectangle, uint32_t color) {
   rectangle = intersection(_clip, rectangle);

   if (!rectangle.valid()) {
      return *this;
   }

#ifdef UI_SSE2
   __m128i color4 = _mm_set_epi32(color, color, color, color);
#endif

   for (int line = rectangle.t; line < rectangle.b; line++) {
      uint32_t* bits  = _bits + static_cast<size_t>(line * _width + rectangle.l);
      int       count = rectangle.width();

#ifdef UI_SSE2
      while (count >= 4) {
         _mm_storeu_si128((__m128i*)bits, color4);
         bits += 4;
         count -= 4;
      }
#endif

      while (count--) {
         *bits++ = color;
      }
   }

#ifdef UI_DEBUG
   _fill_count += rectangle.width() * rectangle.height();
#endif
   return *this;
}

UIPainter& UIPainter::draw_line(int x0, int y0, int x1, int y1, uint32_t color) {
   // Apply the clip.

   UIRectangle c = _clip;
   if (!c.valid())
      return *this;
   int       dx = x1 - x0, dy = y1 - y0;
   const int p[4] = {-dx, dx, -dy, dy};
   const int q[4] = {x0 - c.l, c.r - 1 - x0, y0 - c.t, c.b - 1 - y0};
   float     t0 = 0.0f, t1 = 1.0f; // How far along the line the points end up.

   for (int i = 0; i < 4; i++) {
      if (!p[i] && q[i] < 0)
         return *this;
      float r = static_cast<float>(q[i]) / p[i];
      if (p[i] < 0 && r > t1)
         return *this;
      if (p[i] > 0 && r < t0)
         return *this;
      if (p[i] < 0 && r > t0)
         t0 = r;
      if (p[i] > 0 && r < t1)
         t1 = r;
   }

   x1 = x0 + static_cast<int>(t1 * dx);
   y1 = y0 + static_cast<int>(t1 * dy);
   x0 += static_cast<int>(t0 * dx);
   y0 += static_cast<int>(t0 * dy);

   // Calculate the delta X and delta Y.

   if (y1 < y0) {
      std::swap(x0, x1);
      std::swap(y0, y1);
   }

   dx = x1 - x0, dy = y1 - y0;
   int dxs = dx < 0 ? -1 : 1;
   if (dx < 0)
      dx = -dx;

   // Draw the line using Bresenham's line algorithm.

   uint32_t* bits = _bits + static_cast<size_t>(y0 * _width + x0);

   if (dy * dy < dx * dx) {
      int m = 2 * dy - dx;

      for (int i = 0; i < dx; i++, bits += dxs) {
         *bits = color;
         if (m > 0)
            bits += _width, m -= 2 * dx;
         m += 2 * dy;
      }
   } else {
      int m = 2 * dx - dy;

      for (int i = 0; i < dy; i++, bits += _width) {
         *bits = color;
         if (m > 0)
            bits += dxs, m -= 2 * dy;
         m += 2 * dx;
      }
   }

   return *this;
}

UIPainter& UIPainter::draw_circle(int cx, int cy, int radius, uint32_t fillColor, uint32_t outlineColor, bool hollow) {
   // TODO There's a hole missing at the bottom of the circle!
   // TODO This looks bad at small radii (< 20).

   float x = 0, y = -radius;
   float dx = radius, dy = 0;
   float step = 0.2f / radius;
   int   px = 0, py = cy + y;

   while (x >= 0) {
      x += dx * step;
      y += dy * step;
      dx += -x * step;
      dy += -y * step;

      int ix = x, iy = cy + y;

      while (py <= iy) {
         if (py >= _clip.t && py < _clip.b) {
            for (int s = 0; s <= ix || s <= px; s++) {
               bool inOutline = ((s <= ix) != (s <= px)) || ((ix == px) && (s == ix));
               if (hollow && !inOutline)
                  continue;
               bool clip0 = cx + s >= _clip.l && cx + s < _clip.r;
               bool clip1 = cx - s >= _clip.l && cx - s < _clip.r;
               if (clip0)
                  _bits[_width * py + cx + s] = inOutline ? outlineColor : fillColor;
               if (clip1)
                  _bits[_width * py + cx - s] = inOutline ? outlineColor : fillColor;
            }
         }

         px = ix, py++;
      }
   }
   return *this;
}

UIPainter& UIPainter::draw_triangle(int x0, int y0, int x1, int y1, int x2, int y2, uint32_t color) {
   // Step 1: Sort the points by their y-coordinate.
   if (y1 < y0) {
      int xt = x0;
      x0 = x1, x1 = xt;
      int yt = y0;
      y0 = y1, y1 = yt;
   }
   if (y2 < y1) {
      int xt = x1;
      x1 = x2, x2 = xt;
      int yt = y1;
      y1 = y2, y2 = yt;
   }
   if (y1 < y0) {
      int xt = x0;
      x0 = x1, x1 = xt;
      int yt = y0;
      y0 = y1, y1 = yt;
   }
   if (y2 == y0)
      return *this;

   // Step 2: Clip the triangle.
   if ((x0 < _clip.l && x1 < _clip.l && x2 < _clip.l) || (x0 >= _clip.r && x1 >= _clip.r && x2 >= _clip.r) ||
       (y2 < _clip.t || y0 >= _clip.b))
      return *this;

   bool needsXClip = x0 < _clip.l + 1 || x0 >= _clip.r - 1 || x1 < _clip.l + 1 || x1 >= _clip.r - 1 ||
                     x2 < _clip.l + 1 || x2 >= _clip.r - 1;
   bool needsYClip = y0 < _clip.t + 1 || y2 >= _clip.b - 1;

   auto apply_clip = [&](int& xf, int& xt, int xo, int yo, int yi) {
      if (needsYClip && (yi + yo < _clip.t || yi + yo >= _clip.b))
         return true;
      if (needsXClip) {
         if (xf + xo < _clip.l)
            xf = _clip.l - xo;
         if (xt + xo > _clip.r)
            xt = _clip.r - xo;
      }
      return false;
   };

   // Step 3: Split into 2 triangles with bases aligned with the x-axis.
   float xm0 = (x2 - x0) * (y1 - y0) / (y2 - y0), xm1 = x1 - x0;
   if (xm1 < xm0) {
      float xmt = xm0;
      xm0 = xm1, xm1 = xmt;
   }
   float xe0 = xm0 + x0 - x2, xe1 = xm1 + x0 - x2;
   int   ym = y1 - y0, ye = y2 - y1;
   float ymr = 1.0f / ym, yer = 1.0f / ye;

   // Step 4: Draw the top part.
   for (float y = 0; y < ym; y++) {
      int xf = xm0 * y * ymr, xt = xm1 * y * ymr, yi = static_cast<int>(y);
      if (apply_clip(xf, xt, x0, y0, yi))
         continue; // fully clipped
      uint32_t* b = &_bits[(yi + y0) * _width + x0];
      for (int x = xf; x < xt; x++)
         b[x] = color;
   }

   // Step 5: Draw the bottom part.
   for (float y = 0; y < ye; y++) {
      int xf = xe0 * (ye - y) * yer, xt = xe1 * (ye - y) * yer, yi = static_cast<int>(y);
      if (apply_clip(xf, xt, x2, y1, yi))
         continue; // fully clipped
      uint32_t* b = &_bits[(yi + y1) * _width + x2];
      for (int x = xf; x < xt; x++)
         b[x] = color;
   }
   return *this;
}

UIPainter& UIPainter::draw_triangle_outline(int x0, int y0, int x1, int y1, int x2, int y2, uint32_t color) {
   draw_line(x0, y0, x1, y1, color);
   draw_line(x1, y1, x2, y2, color);
   draw_line(x2, y2, x0, y0, color);
   return *this;
}

UIPainter& UIPainter::draw_invert(UIRectangle rectangle) {
   rectangle = intersection(_clip, rectangle);

   if (rectangle.valid()) {
      for (int line = rectangle.t; line < rectangle.b; line++) {
         uint32_t* bits  = _bits + static_cast<size_t>(line * _width + rectangle.l) ;
         int       count = rectangle.width();

         while (count--) {
            uint32_t in = *bits;
            *bits       = in ^ 0xFFFFFF;
            bits++;
         }
      }
   }
   return *this;
}

int UI::string_width(std::string_view string) const {
#ifdef UI_UNICODE
   return Utf8StringLength(string.data(), string.size()) * _active_font->_glyph_width;
#else
   return static_cast<int>(string.size()) * _active_font->_glyph_width;
#endif
}

UIPainter& UIPainter::draw_string(UIRectangle r, std::string_view string, uint32_t color, UIAlign align,
                                  UIStringSelection* selection) {
   if (string.empty() || !string[0])
      return *this;

   UIRectangle oldClip = _clip;
   _clip               = intersection(r, oldClip);

   if (!_clip.valid()) {
      _clip = oldClip;
      return *this;
   }

   UI* ui               = this->ui();
   auto [width, height] = ui->string_dims(string);
   int    x = align == UIAlign::center ? ((r.l + r.r - width) / 2) : align == UIAlign::right ? (r.r - width) : r.l;
   int    y = (r.t + r.b - height) / 2;
   int    i = 0;
   size_t j = 0;

   int selectFrom = -1, selectTo = -1;

   if (selection) {
      selectFrom = selection->carets[0];
      selectTo   = selection->carets[1];

      if (selectFrom > selectTo) {
         std::swap(selectFrom, selectTo);
      }
   }

   size_t bytes = string.size();
   while (j < bytes) {
      size_t bytesConsumed = 1;

#ifdef UI_UNICODE
      int c = Utf8GetCodePoint(string.data(), bytes - j, &bytesConsumed);
      assert(bytesConsumed > 0);
      string = string.substr(bytesConsumed);
#else
      char c = string[0];
      string = string.substr(1);
#endif
      uint32_t colorText   = color;
      UIFont*  active_font = ui->active_font();

      if (i >= selectFrom && i < selectTo) {
         int w = active_font->_glyph_width;
         if (c == '\t') {
            int ii = i;
            while (++ii & 3)
               w += active_font->_glyph_width;
         }
         draw_block(UIRectangle(x, x + w, y, y + height), selection->colorBackground);
         colorText = selection->colorText;
      }

      if (c != '\t') {
         draw_glyph(x, y, c, colorText);
      }

      if (selection && selection->carets[0] == i) {
         draw_invert(UIRectangle(x, x + 1, y, y + height));
      }

      x += active_font->_glyph_width, i++;

      if (c == '\t') {
         while (i & 3)
            x += active_font->_glyph_width, i++;
      }

      j += bytesConsumed;
   }

   if (selection && selection->carets[0] == i) {
      draw_invert(UIRectangle(x, x + 1, y, y + height));
   }

   _clip = oldClip;

   return *this;
}

UIPainter& UIPainter::draw_border(UIRectangle r, uint32_t borderColor, UIRectangle borderSize) {
   auto border_rects = r.border(borderSize);
   for (const auto& br : border_rects)
      draw_block(br, borderColor);
   return *this;
}

UIPainter& UIPainter::draw_rectangle(UIRectangle r, uint32_t mainColor, uint32_t borderColor, UIRectangle borderSize) {
   draw_border(r, borderColor, borderSize);
   draw_block(r.shrink(borderSize), mainColor);
   return *this;
}

auto ui_mdi_child_calculate_layout(const UIRectangle& bounds, float scale) {
   int         titleSize   = ui_size::mdi_child_title * scale;
   int         borderSize  = ui_size::mdi_child_border * scale;
   UIRectangle titleRect   = add(bounds, UIRectangle(borderSize, -borderSize, 0, 0));
   titleRect.b             = titleRect.t + titleSize;
   UIRectangle contentRect = add(bounds, UIRectangle(borderSize, -borderSize, titleSize, -borderSize));

   return std::tuple{titleSize, borderSize, titleRect, contentRect};
}

UIPainter& UIPainter::draw_control_default(UIRectangle bounds, uint32_t mode, std::string_view label, double position,
                                           float scale) {
   UI*      ui            = this->ui();
   bool     checked       = mode & UIControl::state_checked;
   bool     disabled      = mode & UIControl::state_disabled;
   bool     focused       = mode & UIControl::state_focused;
   bool     hovered       = mode & UIControl::state_hovered;
   bool     indeterminate = mode & UIControl::state_indeterminate;
   bool     pressed       = mode & UIControl::state_pressed;
   bool     selected      = mode & UIControl::state_selected;
   uint32_t which         = mode & UIControl::type_mask;

   auto& theme = ui->theme();

   uint32_t buttonColor     = disabled               ? theme.buttonDisabled
                              : (pressed && hovered) ? theme.buttonPressed
                              : (pressed || hovered) ? theme.buttonHovered
                              : focused              ? theme.selected
                                                     : theme.buttonNormal;
   uint32_t buttonTextColor = disabled                        ? theme.textDisabled
                              : buttonColor == theme.selected ? theme.textSelected
                                                              : theme.text;

   if (which == UIControl::checkbox) {
      uint32_t    color = buttonColor, textColor = buttonTextColor;
      int         midY      = (bounds.t + bounds.b) / 2;
      UIRectangle boxBounds = UIRectangle(bounds.l, bounds.l + ui_size::checkbox_box, midY - ui_size::checkbox_box / 2,
                                          midY + ui_size::checkbox_box / 2);
      draw_rectangle(boxBounds, color, theme.border, UIRectangle(1));
      draw_string(boxBounds + UIRectangle(1, 0, 0, 0),
                  checked         ? "*"
                  : indeterminate ? "-"
                                  : " ",
                  textColor, UIAlign::center, nullptr);
      draw_string(bounds + UIRectangle(ui_size::checkbox_box + ui_size::checkbox_gap, 0, 0, 0), label,
                  disabled ? theme.textDisabled : theme.text, UIAlign::left, nullptr);
   } else if (which == UIControl::menu_item || which == UIControl::drop_down || which == UIControl::push_button) {
      uint32_t color = buttonColor, textColor = buttonTextColor;
      int      borderSize = which == UIControl::menu_item ? 0 : scale;
      draw_rectangle(bounds, color, theme.border, UIRectangle(borderSize));

      if (checked && !focused) {
         draw_block(bounds + ui_rect_1i(static_cast<int>(ui_size::button_checked_area * scale)), theme.buttonPressed);
      }

      UIRectangle innerBounds = bounds + ui_rect_2i(static_cast<int>(ui_size::menu_item_margin * scale), 0);

      if (which == UIControl::menu_item) {
         int  tab        = 0;
         auto labelBytes = static_cast<int>(label.size());
         for (; tab < labelBytes && label[tab] != '\t'; tab++)
            ;

         draw_string(innerBounds, label.substr(0, tab), textColor, UIAlign::left, nullptr);

         if (labelBytes > tab) {
            draw_string(innerBounds, {label.data() + tab + 1, static_cast<size_t>(labelBytes - tab - 1)}, textColor,
                        UIAlign::right, nullptr);
         }
      } else if (which == UIControl::drop_down) {
         draw_string(innerBounds, label, textColor, UIAlign::left, nullptr);
         draw_string(innerBounds, "\x19", textColor, UIAlign::right, nullptr);
      } else {
         draw_string(bounds, label, textColor, UIAlign::center, nullptr);
      }
   } else if (which == UIControl::label) {
      draw_string(bounds, label, theme.text, UIAlign::left, nullptr);
   } else if (which == UIControl::splitter) {
      UIRectangle borders = (mode & UIControl::state_vertical) ? UIRectangle(0, 1) : UIRectangle(1, 0);
      draw_rectangle(bounds, theme.buttonNormal, theme.border, borders);
   } else if (which == UIControl::scroll_track) {
      if (disabled)
         draw_block(bounds, theme.panel1);
   } else if (which == UIControl::scroll_down || which == UIControl::scroll_up) {
      bool     isDown = which == UIControl::scroll_down;
      uint32_t color  = pressed ? theme.buttonPressed : hovered ? theme.buttonHovered : theme.panel2;
      draw_rectangle(bounds, color, theme.border, UIRectangle(0));
      UIFont* active_font = ui->active_font();

      if (mode & UIControl::state_vertical) {
         draw_glyph((bounds.l + bounds.r - active_font->_glyph_width) / 2 + 1,
                    isDown ? (bounds.b - active_font->_glyph_height - 2 * scale) : (bounds.t + 2 * scale),
                    isDown ? 25 : 24, theme.text);
      } else {
         draw_glyph(isDown ? (bounds.r - active_font->_glyph_width - 2 * scale) : (bounds.l + 2 * scale),
                    (bounds.t + bounds.b - active_font->_glyph_height) / 2, isDown ? 26 : 27, theme.text);
      }
   } else if (which == UIControl::scroll_thumb) {
      uint32_t color = pressed ? theme.buttonPressed : hovered ? theme.buttonHovered : theme.buttonNormal;
      draw_rectangle(bounds, color, theme.border, UIRectangle(2));
   } else if (which == UIControl::gauge) {
      draw_rectangle(bounds, theme.buttonNormal, theme.border, UIRectangle(1));
      UIRectangle filled = bounds + ui_rect_1i(1);
      if (mode & UIControl::state_vertical) {
         filled.t = filled.b - filled.height() * position;
      } else {
         filled.r = filled.l + filled.width() * position;
      }
      draw_block(filled, theme.selected);
   } else if (which == UIControl::slider) {
      bool vertical     = mode & UIControl::state_vertical;
      int  center       = vertical ? (bounds.l + bounds.r) / 2 : (bounds.t + bounds.b) / 2;
      int  trackSize    = ui_size::slider_track * scale;
      int  thumbSize    = ui_size::slider_thumb * scale;
      int thumbPosition = vertical ? (bounds.height() - thumbSize) * position : (bounds.width() - thumbSize) * position;
      UIRectangle track = vertical
                             ? UIRectangle(center - (trackSize + 1) / 2, center + trackSize / 2, bounds.t, bounds.b)
                             : UIRectangle(bounds.l, bounds.r, center - (trackSize + 1) / 2, center + trackSize / 2);

      draw_rectangle(track, disabled ? theme.buttonDisabled : theme.buttonNormal, theme.border, UIRectangle(1));
      uint32_t    color = disabled  ? theme.buttonDisabled
                          : pressed ? theme.buttonPressed
                          : hovered ? theme.buttonHovered
                                    : theme.buttonNormal;
      UIRectangle thumb = vertical ? UIRectangle(center - (thumbSize + 1) / 2, center + thumbSize / 2,
                                                 bounds.b - thumbPosition - thumbSize, bounds.b - thumbPosition)
                                   : UIRectangle(bounds.l + thumbPosition, bounds.l + thumbPosition + thumbSize,
                                                 center - (thumbSize + 1) / 2, center + thumbSize / 2);
      draw_rectangle(thumb, color, theme.border, UIRectangle(1));
   } else if (which == UIControl::textbox) {
      draw_rectangle(bounds,
                     disabled  ? theme.buttonDisabled
                     : focused ? theme.textboxFocused
                               : theme.textboxNormal,
                     theme.border, UIRectangle(1));
   } else if (which == UIControl::modal_popup) {
      UIRectangle bounds2 = bounds + ui_rect_1i(-1);
      draw_border(bounds2, theme.border, UIRectangle(1));
      draw_border(bounds2 + UIRectangle(1), theme.border, UIRectangle(1));
   } else if (which == UIControl::menu) {
      draw_block(bounds, theme.border);
   } else if (which == UIControl::table_row) {
      if (selected)
         draw_block(bounds, theme.selected);
      else if (hovered)
         draw_block(bounds, theme.buttonHovered);
   } else if (which == UIControl::table_cell) {
      uint32_t textColor = selected ? theme.textSelected : theme.text;
      draw_string(bounds, label, textColor, UIAlign::left, nullptr);
   } else if (which == UIControl::table_background) {
      draw_block(bounds, theme.panel2);
      draw_rectangle(UIRectangle(bounds.l, bounds.r, bounds.t, bounds.t + static_cast<int>(ui_size::table_header * scale)),
                     theme.panel1, theme.border, UIRectangle(0, 0, 0, 1));
   } else if (which == UIControl::table_header) {
      draw_string(bounds, label, theme.text, UIAlign::left, nullptr);
      if (selected)
         draw_invert(bounds);
   } else if (which == UIControl::mdi_child) {
      auto [titleSize, borderSize, titleRect, contentRect] = ui_mdi_child_calculate_layout(bounds, scale);
      UIRectangle borders                                  = UIRectangle(borderSize, borderSize, titleSize, borderSize);
      draw_border(bounds, theme.buttonNormal, borders);
      draw_border(bounds, theme.border, UIRectangle(static_cast<int>(scale)));
      draw_border(contentRect + ui_rect_1i(-1), theme.border, UIRectangle(static_cast<int>(scale)));
      draw_string(titleRect, label, theme.text, UIAlign::left, nullptr);
   } else if (which == UIControl::tab) {
      uint32_t    color = selected ? theme.buttonPressed : theme.buttonNormal;
      UIRectangle t     = bounds;
      if (selected)
         t.b++, t.t--;
      else
         t.t++;
      draw_rectangle(t, color, theme.border, UIRectangle(1));
      draw_string(bounds, label, theme.text, UIAlign::center, nullptr);
   } else if (which == UIControl::tab_band) {
      draw_rectangle(bounds, theme.panel1, theme.border, UIRectangle(0, 0, 0, 1));
   }
   return *this;
}

// --------------------------------------------------
// Element hierarchy.
// --------------------------------------------------

void UIElement::_destroy_descendents(bool topLevel) {
   for (auto child : _children) {
      if (!topLevel || !child->has_flag(non_client_flag))
         child->destroy();
   }

   if constexpr (UIInspector::enabled())
      ui()->inspector_refresh();
}

void UIElement::destroy_descendents() { _destroy_descendents(true); }

void UIElement::destroy() {
   if (has_flag(destroy_flag)) {
      return;
   }

   this->message(UIMessage::DESTROY, 0, nullptr);
   set_flag(destroy_flag | hide_flag);

   UIElement* ancestor = _parent;

   while (ancestor) {
      if (ancestor->has_flag(destroy_descendent))
         break;
      ancestor->set_flag(destroy_descendent);
      ancestor = ancestor->_parent;
   }

   _destroy_descendents(false);

   if (_parent) {
      _parent->relayout();
      _parent->repaint(&_bounds);
      _parent->measurements_changed(3);
   }
}

// returns 0 if message not processed
// ----------------------------------
int UIElement::message(UIMessage msg, int di, void* dp) {
   if (msg != UIMessage::DEALLOCATE && has_flag(destroy_flag)) {
      return 0;
   }

   if (msg >= UIMessage::INPUT_EVENTS_START && msg <= UIMessage::INPUT_EVENTS_END && is_disabled()) {
      return 0;
   }

   if (_user_proc) {
      if (int result = _user_proc(this, msg, di, dp))
         return result;
   }

   if (_class_proc)
      return _class_proc(this, msg, di, dp);

   return 0;
}

// --------------------------------------------------
// Modal dialogs.
// --------------------------------------------------

int _UIDialogWrapperMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::LAYOUT) {
      int         width  = el->_children[0]->message(UIMessage::GET_WIDTH, 0, nullptr);
      int         height = el->_children[0]->message(UIMessage::GET_HEIGHT, width, nullptr);
      int         cx     = (el->_bounds.l + el->_bounds.r) / 2;
      int         cy     = (el->_bounds.t + el->_bounds.b) / 2;
      UIRectangle bounds = UIRectangle(cx - (width + 1) / 2, cx + width / 2, cy - (height + 1) / 2, cy + height / 2);
      el->_children[0]->move(bounds, false);
      el->repaint(nullptr);
   } else if (msg == UIMessage::PAINT) {
      static_cast<UIPainter*>(dp)->draw_control(el->_children[0]->_bounds, UIControl::modal_popup, {}, 0,
                                                el->get_scale());
   } else if (msg == UIMessage::KEY_TYPED) {
      UI*   ui    = el->ui();
      auto* typed = static_cast<UIKeyTyped*>(dp);

      if (el->is_ctrl_on())
         return 0;
      if (el->is_shift_on())
         return 0;

      if (!ui->_dialog_can_exit) {
      } else if (!el->is_alt_on() && typed->code == UIKeycode::ESCAPE) {
         ui->_dialog_result = "__C";
         return 1;
      } else if (!el->is_alt_on() && typed->code == UIKeycode::ENTER) {
         ui->_dialog_result = "__D";
         return 1;
      }

      char c0 = 0, c1 = 0;

      if (typed->text.size() == 1 && typed->text[0] >= 'a' && typed->text[0] <= 'z') {
         c0 = typed->text[0], c1 = typed->text[0] - 'a' + 'A';
      } else {
         return 0;
      }

      UIElement* rowContainer = el->_children[0];
      UIElement* target       = nullptr;
      bool       duplicate    = false;

      for (auto row : rowContainer->_children) {
         for (auto item : row->_children) {

            if (item->_class_proc == UIButton::_ClassMessageProc) {
               UIButton* button = dynamic_cast<UIButton*>(item);
               auto      label  = button->label();
               if (!label.empty() && (label[0] == c0 || label[0] == c1)) {
                  if (!target) {
                     target = button;
                  } else {
                     duplicate = true;
                  }
               }
            }
         }
      }

      if (target) {
         if (duplicate) {
            target->focus();
         } else {
            target->message(UIMessage::CLICKED, 0, nullptr);
         }

         return 1;
      }
   }

   return 0;
}

// -------------------------------------------------------------------------------
// Set insertBefore to null to insert at the end.
// Returns the element it was before in its previous parent, or nullptr.
// -------------------------------------------------------------------------------
UIElement* UIElement::change_parent(UIElement* newParent, UIElement* insertBefore) {
   [[maybe_unused]] bool found     = false;
   UIElement*            oldBefore = nullptr;

   auto& from = _parent->_children;
   for (uint32_t i = 0; i < from.size(); i++) {
      if (from[i] == this) {
         from.erase(from.begin() + i);
         oldBefore = (i == from.size()) ? nullptr : from[i];
         found     = true;
         break;
      }
   }

   assert(found && !has_flag(destroy_flag));

   auto& to = newParent->_children;
   for (uint32_t i = 0; i <= to.size(); i++) {
      if (i == to.size() || to[i] == insertBefore) {
         to.insert(to.begin() + i, this);
         found = true;
         break;
      }
   }

   UIElement* oldParent = _parent;
   _parent              = newParent;
   _window              = newParent->_window;

   oldParent->measurements_changed(3);
   newParent->measurements_changed(3);

   return oldBefore;
}

void UIElement::set_disabled(bool disabled) {
   if (is_focused() && disabled) {
      _window->focus();
   }

   if (is_disabled() == disabled)
      return;

   if (disabled)
      set_flag(disabled_flag);
   else
      clear_flag(disabled_flag);

   message(UIMessage::UPDATE, UIUpdate::disabled, nullptr);
}

UIElement& UIElement::focus() {
   UIElement* previous = _window->focused();
   if (previous != this) {
      _window->set_focused(this);
      if (previous)
         previous->message(UIMessage::UPDATE, UIUpdate::focused, nullptr);
      this->message(UIMessage::UPDATE, UIUpdate::focused, nullptr);

      ui()->inspector_refresh();
   }
   return *this;
}

UIMDIChild& UIElement::add_mdichild(uint32_t flags, UIRectangle initialBounds, std::string_view title) {
   return *new UIMDIChild(this, flags, initialBounds, title);
}

UIMDIClient& UIElement::add_mdiclient(uint32_t flags) { return *new UIMDIClient(this, flags); }

UIPanel& UIElement::add_panel(uint32_t flags) { return *new UIPanel(this, flags); }

UILabel& UIElement::add_label(uint32_t flags, std::string_view label) { return *new UILabel(this, flags, label); }

UIButton& UIElement::add_button(uint32_t flags, std::string_view label) { return *new UIButton(this, flags, label); }

UIElement& UIElement::add_element(uint32_t flags, message_proc_t message_proc, const char* cClassName) {
   return *new UIElement(this, flags, message_proc, cClassName);
}

UICheckbox& UIElement::add_checkbox(uint32_t flags, std::string_view label) {
   return *new UICheckbox(this, flags, label);
}

UIScrollBar& UIElement::add_scrollbar(uint32_t flags) { return *new UIScrollBar(this, flags); }

UISlider& UIElement::add_slider(uint32_t flags) { return *new UISlider(this, flags); }

UISpacer& UIElement::add_spacer(uint32_t flags, int width, int height) {
   return *new UISpacer(this, flags, width, height);
}

UISplitPane& UIElement::add_splitpane(uint32_t flags, float weight) { return *new UISplitPane(this, flags, weight); }

UISplitter& UIElement::add_splitter(uint32_t flags) { return *new UISplitter(this, flags); }

UITabPane& UIElement::add_tabpane(uint32_t flags, const char* tabs) { return *new UITabPane(this, flags, tabs); }

UIWrapPanel& UIElement::add_wrappanel(uint32_t flags) { return *new UIWrapPanel(this, flags); }

UIGauge& UIElement::add_gauge(uint32_t flags) { return *new UIGauge(this, flags); }

UIImageDisplay& UIElement::add_imagedisplay(uint32_t flags, UIBitmapBits&& bb) {
   return *new UIImageDisplay(this, flags, std::move(bb));
}

UISwitcher& UIElement::add_switcher(uint32_t flags) { return *new UISwitcher(this, flags); }

UIMenu& UIElement::add_menu(uint32_t flags) { return *new UIMenu(_window->ui(), this, flags); }

UITextbox& UIElement::add_textbox(uint32_t flags) { return *new UITextbox(this, flags); }

UITable& UIElement::add_table(uint32_t flags, const char* columns) { return *new UITable(this, flags, columns); }

UICode& UIElement::add_code(uint32_t flags) { return *new UICode(this, flags); }

UIWindow& UI::create_window(UIWindow* owner, uint32_t flags, const char* cTitle, int width, int height) {
   return _platform_create_window(owner, flags, cTitle, width, height);
}

int _UIDialogDefaultButtonMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::PAINT && el->_window->focused()->_class_proc != UIButton::_ClassMessageProc) {
      el->set_flag(UIButton::CHECKED);
      el->_class_proc(el, msg, di, dp);
      el->clear_flag(UIButton::CHECKED);
      return 1;
   }

   return 0;
}

// --------------------------------------------------
// UIWindow
// --------------------------------------------------
static int _DialogTextboxMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   auto* textbox = dynamic_cast<UITextbox*>(el);

   if (msg == UIMessage::VALUE_CHANGED) {
      auto* buffer = static_cast<std::string*>(el->_cp);
      *buffer      = textbox->text();
   } else if (msg == UIMessage::UPDATE && di == UIUpdate::focused && el->is_focused()) {
      textbox->select_all();
      el->repaint(nullptr);
   }

   return 0;
}

std::string_view UIWindow::show_dialog(uint32_t flags, const char* format, ...) {
   // Create the dialog wrapper and panel.
   // ------------------------------------
   assert(!_dialog);
   _dialog        = &add_element(0, _UIDialogWrapperMessage, "DialogWrapper");
   UIPanel* panel = &_dialog->add_panel(UIPanel::MEDIUM_SPACING | UIPanel::COLOR_1)
                        .set_border(UIRectangle(ui_size::pane_medium_border * 2));
   _children[0]->set_flag(disabled_flag);

   // Create the dialog contents.
   // ---------------------------
   va_list arguments;
   va_start(arguments, format);
   UIPanel*   row           = nullptr;
   UIElement* focus         = nullptr;
   UIButton*  defaultButton = nullptr;
   UIButton*  cancelButton  = nullptr;
   uint32_t   buttonCount   = 0;

   for (int i = 0; format[i]; i++) {
      if (i == 0 || format[i - 1] == '\n') {
         row = &panel->add_panel(UIPanel::HORIZONTAL | h_fill).set_gap(ui_size::pane_small_gap);
      }

      if (format[i] == ' ' || format[i] == '\n') {
      } else if (format[i] == '%') {
         i++;

         if (format[i] == 'b' || // --> button
             format[i] == 'B' || // --> default button
             format[i] == 'C') { // --> cancel button
            const char* label  = va_arg(arguments, const char*);
            UIButton*   button = &row->add_button(0, label);
            if (!focus)
               focus = button;
            if (format[i] == 'B')
               defaultButton = button;
            if (format[i] == 'C')
               cancelButton = button;
            buttonCount++;
            button->on_click([&, label = label](UIButton&) { ui()->_dialog_result = label; });
            if (format[i] == 'B')
               button->set_user_proc(_UIDialogDefaultButtonMessage);
         } else if (format[i] == 's') { // --> label from string
            const char* label = va_arg(arguments, const char*);
            row->add_label(0, label);
         } else if (format[i] == 't') { // --> textbox
            std::string* buffer  = va_arg(arguments, std::string*);
            UITextbox*   textbox = &row->add_textbox(h_fill);
            if (!focus)
               focus = textbox;
            if (!buffer->empty())
               textbox->replace_text(*buffer, false);
            textbox->set_cp(buffer); // when the textbox text is updated, `*buffer` will contain a `char*` to the string
            textbox->set_user_proc(_DialogTextboxMessageProc);
         } else if (format[i] == 'f') { // --> horizontal fill
            row->add_spacer(h_fill, 0, 0);
         } else if (format[i] == 'l') { // --> horizontal line
            row->add_spacer(border_flag | h_fill, 0, 1);
         } else if (format[i] == 'u') { // --> user
            UIDialogUserCallback callback = va_arg(arguments, UIDialogUserCallback);
            callback(row);
         }
      } else {
         int j = i;
         while (format[j] && format[j] != '%' && format[j] != '\n')
            j++;
         row->add_label(0, {format + i, static_cast<size_t>(j - i)});
         i = j - 1;
      }
   }

   va_end(arguments);

   _dialog_old_focus = _focused;
   (focus ? focus : _dialog)->focus();

   // Run the modal message loop.
   // ---------------------------
   int result = 0;
   UI* ui               = this->ui();
   ui->_dialog_result   = nullptr;
   ui->_dialog_can_exit = buttonCount != 0;
   for (int i = 1; i <= 3; i++)
      set_pressed(nullptr, i);
   refresh();
   ui->update();
   while (!ui->_dialog_result && ui->platform_message_loop_single(&result))
      ;
   ui->_quit = !ui->_dialog_result;

   // Check for cancel/default action.

   if (buttonCount == 1 && defaultButton && !cancelButton) {
      cancelButton = defaultButton;
   }

   if (!ui->_dialog_result) {
   } else if (ui->_dialog_result[0] == '_' && ui->_dialog_result[1] == '_' && ui->_dialog_result[2] == 'C' &&
              ui->_dialog_result[3] == 0 && cancelButton) {
      ui->_dialog_result = static_cast<const char*>(cancelButton->_cp);
   } else if (ui->_dialog_result[0] == '_' && ui->_dialog_result[1] == '_' && ui->_dialog_result[2] == 'D' &&
              ui->_dialog_result[3] == 0 && defaultButton) {
      ui->_dialog_result = static_cast<const char*>(defaultButton->_cp);
   }

   // Destroy the dialog.
   // -------------------
   _children[0]->clear_flag(disabled_flag);
   _dialog->destroy();
   _dialog = nullptr;
   refresh();
   if (_dialog_old_focus)
      _dialog_old_focus->focus();
   return ui->_dialog_result ? ui->_dialog_result : "";
}

UIWindow& UIWindow::register_shortcut(const UIShortcut& shortcut) {
   _shortcuts.push_back(std::move(shortcut));
   return *this;
}

// --------------------------------------------------
// Update cycles.
// --------------------------------------------------

void UIElement::refresh() {
   relayout();
   repaint(nullptr);
}

void UIElement::relayout() {
   if (has_flag(relayout_flag)) {
      return;
   }

   set_flag(relayout_flag);
   UIElement* ancestor = _parent;

   while (ancestor) {
      ancestor->set_flag(relayout_descendent);
      ancestor = ancestor->_parent;
   }
}

void UIElement::measurements_changed(int which) {
   if (!_parent) {
      return; // This is the window element.
   }

   UIElement* el = this;

   while (true) {
      if (el->_parent->has_flag(destroy_flag))
         return;
      which &= ~el->_parent->message(UIMessage::GET_CHILD_STABILITY, which, el);
      if (!which)
         break;
      el->set_flag(relayout_flag);
      el = el->_parent;
   }

   el->relayout();
}

void UIElement::repaint(const UIRectangle* region) {
   if (!region) {
      region = &_bounds;
   }

   UIRectangle r = intersection(*region, _clip);

   if (!r.valid()) {
      return;
   }

   if (_window->update_region().valid()) {
      _window->set_update_region(bounding(_window->update_region(), r));
   } else {
      _window->set_update_region(r);
   }
}

void UIElement::move(UIRectangle new_bounds, bool layout) {
   UIRectangle new_clip = _parent ? intersection(_parent->_clip, new_bounds) : new_bounds;
   bool        moved    = _bounds != new_bounds || _clip != new_clip;

   if (moved) {
      layout = true;

      _window->repaint(&_clip);
      _window->repaint(&new_clip);

      _bounds = new_bounds;
      _clip   = new_clip;
   }

   if (has_flag(relayout_flag)) {
      layout = true;
   }

   if (layout) {
      message(UIMessage::LAYOUT, 0, nullptr);
   } else if (has_flag(relayout_descendent)) {
      for (auto child : _children)
         child->move(child->_bounds, false);
   }

   clear_flag(relayout_descendent | relayout_flag);
}

void UIElement::paint(UIPainter* painter) {
   if (has_flag(hide_flag)) {
      return;
   }

   // Clip painting to the element's clip.
   // ------------------------------------
   painter->_clip = intersection(_clip, painter->_clip);

   if (!painter->_clip.valid()) {
      return;
   }

   // Paint the element.
   // ------------------
   message(UIMessage::PAINT, 0, painter);

   // Paint its children.
   // -------------------
   UIRectangle previousClip = painter->_clip;

   for (auto child : _children) {
      painter->_clip = previousClip;
      child->paint(painter);
   }

   // Draw the foreground and border.
   // -------------------------------
   painter->_clip = previousClip;
   message(UIMessage::PAINT_FOREGROUND, 0, painter);

   if (has_flag(border_flag)) {
      painter->draw_border(_bounds, ui()->theme().border, UIRectangle(static_cast<int>(get_scale())));
   }
}

bool UIElement::_destroy() {
   if (has_flag(destroy_descendent)) {
      clear_flag(destroy_descendent);
#if 1
      auto num_children = static_cast<intptr_t>(_children.size());
      for (intptr_t i = 0; i < num_children; i++) {
         if (_children[i]->_destroy()) {
            _children.erase(_children.begin() + i);
            --num_children;
            --i;
         }
      }
#else
      // not sure why this does not work. crash when clicking on file in "Files" tab
      auto                    filtered     = _children | views::filter([](UIElement* c) { return !c->destroy(); });
      std::vector<UIElement*> new_children = {filtered.begin(), filtered.end()};
      _children                            = std::move(new_children);
#endif
   }

   if (has_flag(destroy_flag)) {
      message(UIMessage::DEALLOCATE, 0, nullptr);

      auto *win = _window;

      if (is_pressed()) {
         win->set_pressed(nullptr, 0);
      }

      if (is_hovered()) {
         win->set_hovered(win); // base of win, not `this` as before
      }

      if (is_focused()) {
         win->set_focused(nullptr);
      }

      if (win->dialog_old_focus() == this) {
         win->set_dialog_old_focus(nullptr);
      }

      animate(true);

      delete this;
      return true;
   }
   return false;
}

UIElement::UIElement(UIElement* parent, uint32_t flags, message_proc_t message_proc, const char* cClassName)
   : _flags(flags)
   , _class_name(cClassName)
   , _parent(parent)
   , _bounds(0)
   , _clip(0)
   , _class_proc(message_proc) {

   assert(has_flag(window_flag) || parent); // if `window_flag`, set, `_window` will be set by `_init_toplevel()`
   if (parent) {
      assert(!parent->has_flag(destroy_flag));
      _window = parent->_window;
      parent->_children.push_back(this);
      parent->relayout();
      parent->measurements_changed(3);
   }

   static uint32_t s_id = 0;
   _id                  = ++s_id;

   if constexpr (UIInspector::enabled())
      if (parent)
         parent->ui()->inspector_refresh();
}

UIElement::~UIElement() = default;

// --------------------------------------------------
// Panels.
// --------------------------------------------------

int UIPanel::_calculate_per_fill(int* _count, int hSpace, int vSpace, float scale) {
   bool horizontal = has_flag(UIPanel::HORIZONTAL);
   int  available  = horizontal ? hSpace : vSpace;
   int  count = 0, fill = 0, perFill = 0;

   for (auto child : _children) {
      if (child->has_flag(hide_flag) || child->has_flag(non_client_flag)) {
         continue;
      }

      count++;

      if (horizontal) {
         if (child->has_flag(h_fill)) {
            fill++;
         } else if (available > 0) {
            available -= child->message(UIMessage::GET_WIDTH, vSpace, 0);
         }
      } else {
         if (child->has_flag(v_fill)) {
            fill++;
         } else if (available > 0) {
            available -= child->message(UIMessage::GET_HEIGHT, hSpace, 0);
         }
      }
   }

   if (count) {
      available -= (count - 1) * static_cast<int>(_gap * scale);
   }

   if (available > 0 && fill) {
      perFill = available / fill;
   }

   if (_count) {
      *_count = count;
   }

   return perFill;
}

int UIPanel::_measure(int di) {
   bool horizontal = has_flag(UIPanel::HORIZONTAL);
   int  perFill    = _calculate_per_fill(nullptr, horizontal ? di : 0, horizontal ? 0 : di, get_scale());
   int  size       = 0;

   for (auto child : _children) {
      if (child->has_flag(hide_flag) || child->has_flag(non_client_flag))
         continue;
      int childSize = child->message(horizontal ? UIMessage::GET_HEIGHT : UIMessage::GET_WIDTH,
                                     child->has_flag(horizontal ? h_fill : v_fill) ? perFill : 0, 0);
      if (childSize > size)
         size = childSize;
   }

   int border = horizontal ? (_border.t + _border.b) : (_border.l + _border.r);
   return size + scale(border);
}

int UIPanel::_layout(UIRectangle bounds, bool measure) {
   bool horizontal = has_flag(UIPanel::HORIZONTAL);

   int position = scale(horizontal ? _border.l : _border.t);
   if (_scrollBar && !measure)
      position -= _scrollBar->position();
   int  hSpace        = bounds.width() - scale(_border.total_width());
   int  vSpace        = bounds.height() - scale(_border.total_height());
   int  count         = 0;
   int  perFill       = _calculate_per_fill(&count, hSpace, vSpace, get_scale());
   int  scaledBorder2 = scale(horizontal ? _border.t : _border.l);
   bool expand        = has_flag(UIPanel::EXPAND);

   for (auto child : _children) {
      if (child->has_flag(hide_flag) || child->has_flag(non_client_flag)) {
         continue;
      }

      if (horizontal) {
         int         height   = (child->has_flag(v_fill) || expand)
                                   ? vSpace
                                   : child->message(UIMessage::GET_HEIGHT, child->has_flag(h_fill) ? perFill : 0, 0);
         int         width    = child->has_flag(h_fill) ? perFill : child->message(UIMessage::GET_WIDTH, height, 0);
         UIRectangle relative = UIRectangle(position, position + width, scaledBorder2 + (vSpace - height) / 2,
                                            scaledBorder2 + (vSpace + height) / 2);
         if (!measure)
            child->move(translate(relative, bounds), false);
         position += width + scale(_gap);
      } else {
         int         width    = (child->has_flag(h_fill) || expand)
                                   ? hSpace
                                   : child->message(UIMessage::GET_WIDTH, child->has_flag(v_fill) ? perFill : 0, 0);
         int         height   = child->has_flag(v_fill) ? perFill : child->message(UIMessage::GET_HEIGHT, width, 0);
         UIRectangle relative = UIRectangle(scaledBorder2 + (hSpace - width) / 2, scaledBorder2 + (hSpace + width) / 2,
                                            position, position + height);
         if (!measure)
            child->move(translate(relative, bounds), false);
         position += height + scale(_gap);
      }
   }

   return position - scale(count ? _gap : 0) + scale(horizontal ? _border.r : _border.b);
}

int UIPanel::_class_message_proc(UIMessage msg, int di, void* dp) {
   bool horizontal = has_flag(UIPanel::HORIZONTAL);

   if (msg == UIMessage::LAYOUT) {
      int         scrollBarWidth = _scrollBar ? scale(ui_size::scroll_bar) : 0;
      UIRectangle bounds         = _bounds;
      bounds.r -= scrollBarWidth;

      if (_scrollBar) {
         UIRectangle scrollBarBounds = _bounds;
         scrollBarBounds.l           = scrollBarBounds.r - scrollBarWidth;
         _scrollBar->set_maximum(_layout(bounds, true));
         _scrollBar->set_page(_bounds.height());
         _scrollBar->move(scrollBarBounds, true);
      }

      _layout(bounds, false);
   } else if (msg == UIMessage::GET_WIDTH) {
      if (horizontal) {
         return _layout(UIRectangle(0, 0, 0, di), true);
      }
      return _measure(di);
   } else if (msg == UIMessage::GET_HEIGHT) {
      if (horizontal) {
         return _measure(di);
      }
      int width = di && _scrollBar ? (di - scale(ui_size::scroll_bar)) : di;
      return _layout(UIRectangle(0, width, 0, 0), true);
   } else if (msg == UIMessage::PAINT) {
      auto& theme = ui()->theme();

      if (has_flag(UIPanel::COLOR_1)) {
         static_cast<UIPainter*>(dp)->draw_block(_bounds, theme.panel1);
      } else if (has_flag(UIPanel::COLOR_2)) {
         static_cast<UIPainter*>(dp)->draw_block(_bounds, theme.panel2);
      }
   } else if (msg == UIMessage::MOUSE_WHEEL && _scrollBar) {
      return _scrollBar->message(msg, di, dp);
   } else if (msg == UIMessage::SCROLLED) {
      refresh();
   } else if (msg == UIMessage::GET_CHILD_STABILITY) {
      auto* child = static_cast<UIElement*>(dp);
      return (has_flag(UIPanel::EXPAND) ? (horizontal ? 2 : 1) : 0) | (child->has_flag(h_fill) ? 1 : 0) |
             (child->has_flag(v_fill) ? 2 : 0);
   }

   return 0;
}

UIPanel::UIPanel(UIElement* parent, uint32_t flags)
   : UIElementCast<UIPanel>(parent, flags, UIPanel::_ClassMessageProc, "Panel") {

   if (flags & UIPanel::LARGE_SPACING) {
      _border = UIRectangle(ui_size::pane_large_border);
      _gap    = ui_size::pane_large_gap;
   } else if (flags & UIPanel::MEDIUM_SPACING) {
      _border = UIRectangle(ui_size::pane_medium_border);
      _gap    = ui_size::pane_medium_gap;
   } else if (flags & UIPanel::SMALL_SPACING) {
      _border = UIRectangle(ui_size::pane_small_border);
      _gap    = ui_size::pane_small_gap;
   }

   if (flags & UIPanel::SCROLL) {
      _scrollBar = &add_scrollbar(non_client_flag);
   }
}

void UIWrapPanel::_layout_row(uint32_t rowStart, uint32_t rowEnd, int rowY, int rowHeight) {
   int rowPosition = 0;

   for (uint32_t i = rowStart; i < rowEnd; i++) {
      UIElement* child = _children[i];
      if (child->has_flag(UIElement::hide_flag))
         continue;
      int         height   = child->message(UIMessage::GET_HEIGHT, 0, nullptr);
      int         width    = child->message(UIMessage::GET_WIDTH, 0, nullptr);
      UIRectangle relative = UIRectangle(rowPosition, rowPosition + width, rowY + rowHeight / 2 - height / 2,
                                         rowY + rowHeight / 2 + height / 2);
      child->move(translate(relative, _bounds), false);
      rowPosition += width;
   }
}

int UIWrapPanel::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::LAYOUT || msg == UIMessage::GET_HEIGHT) {
      int totalHeight = 0;
      int rowPosition = 0;
      int rowHeight   = 0;
      int rowLimit    = msg == UIMessage::LAYOUT ? _bounds.width() : di;

      uint32_t rowStart = 0;

      for (uint32_t i = 0; i < _children.size(); i++) {
         UIElement* child = _children[i];
         if (child->has_flag(hide_flag))
            continue;

         int height = child->message(UIMessage::GET_HEIGHT, 0, nullptr);
         int width  = child->message(UIMessage::GET_WIDTH, 0, nullptr);

         if (rowLimit && rowPosition + width > rowLimit) {
            _layout_row(rowStart, i, totalHeight, rowHeight);
            totalHeight += rowHeight;
            rowPosition = 0;
            rowHeight   = 0;
            rowStart    = i;
         }

         if (height > rowHeight) {
            rowHeight = height;
         }

         rowPosition += width;
      }

      if (msg == UIMessage::GET_HEIGHT) {
         return totalHeight + rowHeight;
      }
      _layout_row(rowStart, static_cast<uint32_t>(_children.size()), totalHeight, rowHeight);
   }

   return 0;
}

UIWrapPanel::UIWrapPanel(UIElement* parent, uint32_t flags)
   : UIElementCast<UIWrapPanel>(parent, flags, UIWrapPanel::_ClassMessageProc, "Wrap Panel") {}

int UISwitcher::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (!_active) {
   } else if (msg == UIMessage::GET_WIDTH || msg == UIMessage::GET_HEIGHT) {
      return _active->message(msg, di, dp);
   } else if (msg == UIMessage::LAYOUT) {
      _active->move(_bounds, false);
   }

   return 0;
}

void UISwitcher::switch_to(UIElement* child) {
   for (auto sw_child : _children)
      sw_child->set_flag(UIElement::hide_flag);

   assert(child->_parent == this);
   child->clear_flag(UIElement::hide_flag);
   _active = child;
   measurements_changed(3);
   refresh();
}

UISwitcher::UISwitcher(UIElement* parent, uint32_t flags)
   : UIElementCast<UISwitcher>(parent, flags, UISwitcher::_ClassMessageProc, "Switcher") {}

// --------------------------------------------------
// Checkboxes and buttons.
// --------------------------------------------------

int UIButton::_class_message_proc(UIMessage msg, int di, void* dp) {
   bool isMenuItem = has_flag(UIButton::MENU_ITEM);
   bool isDropDown = has_flag(UIButton::DROP_DOWN);

   if (msg == UIMessage::GET_HEIGHT) {
      if (isMenuItem) {
         return scale(ui_size::menu_item_height);
      }
      return scale(ui_size::button_height);
   }

   if (msg == UIMessage::GET_WIDTH) {
      int labelSize  = ui()->string_width(_label);
      int paddedSize = labelSize + scale(ui_size::button_padding);
      if (isDropDown)
         paddedSize += ui()->active_font()->_glyph_width * 2;
      int minimumSize = scale(has_flag(UIButton::SMALL) ? 0
                              : isMenuItem              ? ui_size::menu_item_minimum_width
                                                        : ui_size::button_minimum_width);
      return paddedSize > minimumSize ? paddedSize : minimumSize;
   }

   if (msg == UIMessage::PAINT) {
      static_cast<UIPainter*>(dp)->draw_control(_bounds,
                                                (isMenuItem   ? UIControl::menu_item
                                                 : isDropDown ? UIControl::drop_down
                                                              : UIControl::push_button) |
                                                   (has_flag(UIButton::CHECKED) ? UIControl::state_checked : 0) |
                                                   state(),
                                                _label, 0, get_scale());
   } else if (msg == UIMessage::UPDATE) {
      repaint(nullptr);
   } else if (msg == UIMessage::DEALLOCATE) {
   } else if (msg == UIMessage::LEFT_DOWN) {
      if (has_flag(UIButton::CAN_FOCUS)) {
         focus();
      }
   } else if (msg == UIMessage::KEY_TYPED) {
      auto* m = static_cast<UIKeyTyped*>(dp);

      if ((m->text == " ") || m->code == UIKeycode::ENTER) {
         message(UIMessage::CLICKED, 0, nullptr);
         repaint(nullptr);
         return 1;
      }
   } else if (msg == UIMessage::CLICKED) {
      if (_on_click) {
         _on_click(*this);
      }
   }

   return 0;
}

UIButton& UIButton::set_label(std::string_view new_label) {
   if (ui_set(_label, new_label)) {
      measurements_changed(1);
      repaint(nullptr);
   }
   return *this;
}

UIButton::UIButton(UIElement* parent, uint32_t flags, std::string_view label)
   : UIElementCast<UIButton>(parent, flags | tab_stop_flag, UIButton::_ClassMessageProc, "Button")
   , _label(label) {}

// ------------------------------------------------------------------------------------------
int UICheckbox::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_HEIGHT) {
      return scale(ui_size::button_height);
   }
   if (msg == UIMessage::GET_WIDTH) {
      int labelSize = ui()->string_width(_label);
      return scale(labelSize + ui_size::checkbox_box + ui_size::checkbox_gap);
   }
   if (msg == UIMessage::PAINT) {
      static_cast<UIPainter*>(dp)->draw_control(
         _bounds, UIControl::checkbox | (checked() ? UIControl::state_checked : 0) | state(), _label, 0, get_scale());
   } else if (msg == UIMessage::UPDATE) {
      repaint(nullptr);
   } else if (msg == UIMessage::DEALLOCATE) {
   } else if (msg == UIMessage::KEY_TYPED) {
      auto* m = static_cast<UIKeyTyped*>(dp);

      if (m->text == " ") {
         message(UIMessage::CLICKED, 0, nullptr);
         repaint(nullptr);
      }
   } else if (msg == UIMessage::CLICKED) {
      set_checked(!checked());
      if (_on_click)
         _on_click(*this);
   }

   return 0;
}

UICheckbox& UICheckbox::set_label(std::string_view new_label) {
   if (ui_set(_label, new_label)) {
      this->measurements_changed(1);
      repaint(nullptr);
   }
   return *this;
}

UICheckbox::UICheckbox(UIElement* parent, uint32_t flags, std::string_view label)
   : UIElementCast<UICheckbox>(parent, flags | tab_stop_flag, UICheckbox::_ClassMessageProc, "Checkbox")
   , _checked_ptr(&_checked)
   , _label(label) {}


// --------------------------------------------------
// Labels.
// --------------------------------------------------

int UILabel::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_HEIGHT) {
      return ui()->string_height();
   }
   if (msg == UIMessage::GET_WIDTH) {
      return ui()->string_width(_label);
   }
   if (msg == UIMessage::PAINT) {
      static_cast<UIPainter*>(dp)->draw_control(_bounds, UIControl::label | state(), _label, 0, get_scale());
   } else if (msg == UIMessage::DEALLOCATE) {
   }

   return 0;
}

UILabel& UILabel::set_label(std::string_view new_label) {
   if (ui_set(_label, new_label)) {
      measurements_changed(1);
      repaint(nullptr);
   }
   return *this;
}

UILabel::UILabel(UIElement* parent, uint32_t flags, std::string_view label)
   : UIElementCast<UILabel>(parent, flags | tab_stop_flag, UILabel::_ClassMessageProc, "Label")
   , _label(label) {}

// --------------------------------------------------
// Split panes.
// --------------------------------------------------
int UISplitter::_class_message_proc(UIMessage msg, int di, void* dp) {
   auto* splitPane = dynamic_cast<UISplitPane*>(_parent);
   bool  vertical  = splitPane->has_flag(vertical_flag);

   if (msg == UIMessage::PAINT) {
      static_cast<UIPainter*>(dp)->draw_control(
         _bounds, UIControl::splitter | (vertical ? UIControl::state_vertical : 0) | state(), {}, 0, get_scale());
   } else if (msg == UIMessage::GET_CURSOR) {
      return vertical ? static_cast<uint32_t>(UICursor::split_v) : static_cast<uint32_t>(UICursor::split_h);
   } else if (msg == UIMessage::MOUSE_DRAG) {
      int   cursor       = vertical ? cursor_pos().y : cursor_pos().x;
      int   splitterSize = scale(ui_size::splitter);
      int   space        = (vertical ? splitPane->_bounds.height() : splitPane->_bounds.width()) - splitterSize;
      float oldWeight    = splitPane->weight();
      splitPane->set_weight(
         (cursor - static_cast<float>(splitterSize) / 2 - (vertical ? splitPane->_bounds.t : splitPane->_bounds.l)) / space);
      splitPane->set_weight(std::clamp(splitPane->weight(), 0.05f, 0.95f));

      if (splitPane->_children[2]->_class_proc == UISplitPane::_ClassMessageProc &&
          (splitPane->_children[2]->has_flag(vertical_flag)) == splitPane->has_flag(vertical_flag)) {
         auto* subSplitPane = dynamic_cast<UISplitPane*>(splitPane->_children[2]);
         subSplitPane->set_weight(
            (splitPane->weight() - oldWeight - subSplitPane->weight() + oldWeight * subSplitPane->weight()) /
            (-1 + splitPane->weight()));
         splitPane->set_weight(std::clamp(subSplitPane->weight(), 0.05f, 0.95f));
      }

      splitPane->refresh();
   }

   return 0;
}

int UISplitPane::_class_message_proc(UIMessage msg, int di, void* dp) {
   bool vertical = has_flag(vertical_flag);

   if (msg == UIMessage::LAYOUT) {
      assert(_children.size() >= 3);
      UIElement* splitter = _children[0];
      UIElement* left     = _children[1];
      UIElement* right    = _children[2];

      int splitterSize = scale(ui_size::splitter);
      int space        = (vertical ? _bounds.height() : _bounds.width()) - splitterSize;
      int leftSize     = space * _weight;
      int rightSize    = space - leftSize;

      if (vertical) {
         left->move(UIRectangle(_bounds.l, _bounds.r, _bounds.t, _bounds.t + leftSize), false);
         splitter->move(UIRectangle(_bounds.l, _bounds.r, _bounds.t + leftSize, _bounds.t + leftSize + splitterSize),
                        false);
         right->move(UIRectangle(_bounds.l, _bounds.r, _bounds.b - rightSize, _bounds.b), false);
      } else {
         left->move(UIRectangle(_bounds.l, _bounds.l + leftSize, _bounds.t, _bounds.b), false);
         splitter->move(UIRectangle(_bounds.l + leftSize, _bounds.l + leftSize + splitterSize, _bounds.t, _bounds.b),
                        false);
         right->move(UIRectangle(_bounds.r - rightSize, _bounds.r, _bounds.t, _bounds.b), false);
      }
   }

   return 0;
}

UISplitPane::UISplitPane(UIElement* parent, uint32_t flags, float weight)
   : UIElementCast<UISplitPane>(parent, flags, UISplitPane::_ClassMessageProc, "Split Pane")
   , _weight(weight) {
   add_splitter(0);
}

UISplitter::UISplitter(UIElement* parent, uint32_t flags)
   : UIElementCast<UISplitter>(parent, flags, UISplitter::_ClassMessageProc, "Splitter") {}

// --------------------------------------------------
// Tab panes.
// --------------------------------------------------

int UITabPane::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::PAINT) {
      auto*       painter = static_cast<UIPainter*>(dp);
      UIRectangle top     = _bounds;
      top.b               = top.t + scale(ui_size::button_height);
      painter->draw_control(top, UIControl::tab_band, {}, 0, get_scale());

      UIRectangle tab = top;
      tab.l += scale(ui_size::tab_pane_space_left);
      tab.t += scale(ui_size::tab_pane_space_top);

      for_each_tab([&](std::string_view tab_text, uint32_t index, bool active) {
         tab.r = tab.l + ui()->string_width(tab_text) + ui_size::button_padding;
         painter->draw_control(tab, UIControl::tab | (active ? UIControl::state_selected : 0), tab_text, 0,
                               get_scale());
         tab.l = tab.r - 1;
         return true;
      });

   } else if (msg == UIMessage::LEFT_DOWN) {
      UIRectangle tab = _bounds;
      tab.b           = tab.t + scale(ui_size::button_height);
      tab.l += scale(ui_size::tab_pane_space_left);
      tab.t += scale(ui_size::tab_pane_space_top);

      for_each_tab([&](std::string_view tab_text, uint32_t index, bool active) {
         tab.r = tab.l + ui()->string_width(tab_text) + ui_size::button_padding;
         if (tab.contains(_window->cursor_pos())) {
            set_active(index);
            relayout();
            repaint(nullptr);
            return false;
            ;
         }
         tab.l = tab.r - 1;
         return true;
      });
   } else if (msg == UIMessage::LAYOUT) {
      UIRectangle content = _bounds;
      content.t += scale(ui_size::button_height);

      for (uint32_t index = 0; index < _children.size(); index++) {
         UIElement* child = _children[index];

         if (get_active() == index) {
            child->clear_flag(hide_flag);
            child->move(content, false);
            child->message(UIMessage::TAB_SELECTED, 0, nullptr);
         } else {
            child->set_flag(hide_flag);
         }
      }
   } else if (msg == UIMessage::GET_HEIGHT) {
      int baseHeight = scale(ui_size::button_height);

      for (uint32_t index = 0; index < _children.size(); index++) {
         UIElement* child = _children[index];

         if (get_active() == index) {
            return baseHeight + child->message(UIMessage::GET_HEIGHT, di, dp);
         }
      }
   } else if (msg == UIMessage::DEALLOCATE) {
   }

   return 0;
}

UITabPane::UITabPane(UIElement* parent, uint32_t flags, const char* tabs)
   : UIElementCast<UITabPane>(parent, flags, UITabPane::_ClassMessageProc, "Tab Pane")
   , _tabs(tabs) {}

// --------------------------------------------------
// Spacers.
// --------------------------------------------------

int UISpacer::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_HEIGHT) {
      return scale(_height);
   }
   if (msg == UIMessage::GET_WIDTH) {
      return scale(_width);
   }

   return 0;
}

UISpacer::UISpacer(UIElement* parent, uint32_t flags, int width, int height)
   : UIElementCast<UISpacer>(parent, flags, UISpacer::_ClassMessageProc, "Spacer")
   , _width(width)
   , _height(height) {}

// --------------------------------------------------
// Scroll bars.
// --------------------------------------------------

int UIScrollBar::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_WIDTH || msg == UIMessage::GET_HEIGHT) {
      return scale(ui_size::scroll_bar);
   }
   if (msg == UIMessage::LAYOUT) {
      UIElement* up    = _children[0];
      UIElement* thumb = _children[1];
      UIElement* down  = _children[2];

      if (page() >= maximum() || maximum() <= 0 || page() <= 0) {
         up->set_flag(hide_flag);
         thumb->set_flag(hide_flag);
         down->set_flag(hide_flag);

         _position = 0;
      } else {
         up->clear_flag(hide_flag);
         thumb->clear_flag(hide_flag);
         down->clear_flag(hide_flag);

         int size      = _horizontal ? _bounds.width() : _bounds.height();
         int thumbSize = size * page() / maximum();

         if (thumbSize < scale(ui_size::scroll_minimum_thumb)) {
            thumbSize = scale(ui_size::scroll_minimum_thumb);
         }

         if (_position < 0) {
            _position = 0;
         } else if (_position > maximum() - page()) {
            _position = maximum() - page();
         }

         int thumbPosition = static_cast<int>(static_cast<double>(_position) / (maximum() - page()) * (size - thumbSize));

         if (_position == maximum() - page()) {
            thumbPosition = size - thumbSize;
         }

         if (_horizontal) {
            UIRectangle r = _bounds;
            r.r           = r.l + thumbPosition;
            up->move(r, false);
            r.l = r.r, r.r = r.l + thumbSize;
            thumb->move(r, false);
            r.l = r.r, r.r = _bounds.r;
            down->move(r, false);
         } else {
            UIRectangle r = _bounds;
            r.b           = r.t + thumbPosition;
            up->move(r, false);
            r.t = r.b, r.b = r.t + thumbSize;
            thumb->move(r, false);
            r.t = r.b, r.b = _bounds.b;
            down->move(r, false);
         }
      }
   } else if (msg == UIMessage::PAINT) {
      static_cast<UIPainter*>(dp)->draw_control(
         _bounds,
         UIControl::scroll_track |
            ((page() >= maximum() || maximum() <= 0 || page() <= 0) ? UIControl::state_disabled : 0),
         {}, 0, get_scale());
   } else if (msg == UIMessage::MOUSE_WHEEL) {
      _position += di;
      refresh();
      _parent->message(UIMessage::SCROLLED, 0, nullptr);
      return 1;
   }

   return 0;
}

int _UIScrollUpDownMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   auto* scrollBar = dynamic_cast<UIScrollBar*>(el->_parent);
   bool  isDown    = el->_cp;

   if (msg == UIMessage::PAINT) {
      static_cast<UIPainter*>(dp)->draw_control(el->_bounds,
                                                (isDown ? UIControl::scroll_down : UIControl::scroll_up) |
                                                   (scrollBar->_horizontal ? 0 : UIControl::state_vertical) |
                                                   el->state(),
                                                {}, 0, el->get_scale());
   } else if (msg == UIMessage::UPDATE) {
      el->repaint(nullptr);
   } else if (msg == UIMessage::LEFT_DOWN) {
      el->animate(false);
      scrollBar->_last_animate_time = UI_CLOCK();
   } else if (msg == UIMessage::LEFT_UP) {
      el->animate(true);
   } else if (msg == UIMessage::ANIMATE) {
      UI_CLOCK_T previous     = scrollBar->_last_animate_time;
      UI_CLOCK_T current      = UI_CLOCK();
      UI_CLOCK_T delta        = current - previous;
      double     deltaSeconds = (double)delta / UI_CLOCKS_PER_SECOND;
      if (deltaSeconds > 0.1)
         deltaSeconds = 0.1;
      double deltaPixels            = deltaSeconds * scrollBar->page() * 3;
      scrollBar->_last_animate_time = current;
      if (isDown)
         scrollBar->position() += deltaPixels;
      else
         scrollBar->position() -= deltaPixels;
      scrollBar->refresh();
      scrollBar->_parent->message(UIMessage::SCROLLED, 0, nullptr);
   }

   return 0;
}

int _UIScrollThumbMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   auto* scrollBar = dynamic_cast<UIScrollBar*>(el->_parent);

   if (msg == UIMessage::PAINT) {
      static_cast<UIPainter*>(dp)->draw_control(
         el->_bounds, UIControl::scroll_thumb | (scrollBar->_horizontal ? 0 : UIControl::state_vertical) | el->state(),
         {}, 0, el->get_scale());
   } else if (msg == UIMessage::UPDATE) {
      el->repaint(nullptr);
   } else if (msg == UIMessage::MOUSE_DRAG && el->pressed_button() == 1) {
      if (!scrollBar->_in_drag) {
         scrollBar->_in_drag = true;

         if (scrollBar->_horizontal) {
            scrollBar->set_drag_offset(el->_bounds.l - scrollBar->_bounds.l - el->cursor_pos().x);
         } else {
            scrollBar->set_drag_offset(el->_bounds.t - scrollBar->_bounds.t - el->cursor_pos().y);
         }
      }

      int thumbPosition = (scrollBar->_horizontal ? el->cursor_pos().x : el->cursor_pos().y) + scrollBar->drag_offset();
      int size          = scrollBar->_horizontal ? (scrollBar->_bounds.width() - el->_bounds.width())
                                                 : (scrollBar->_bounds.height() - el->_bounds.height());
      scrollBar->position() = static_cast<double>(thumbPosition) / size * (scrollBar->maximum() - scrollBar->page());
      scrollBar->refresh();
      scrollBar->_parent->message(UIMessage::SCROLLED, 0, nullptr);
   } else if (msg == UIMessage::LEFT_UP) {
      scrollBar->_in_drag = false;
   }

   return 0;
}

UIScrollBar::UIScrollBar(UIElement* parent, uint32_t flags)
   : UIElementCast<UIScrollBar>(parent, flags, UIScrollBar::_ClassMessageProc, "Scroll Bar")
   , _horizontal(flags & UIScrollBar::HORIZONTAL) {
   auto scrollup = new UIElement(this, flags, _UIScrollUpDownMessageProc, !_horizontal ? "Scroll Up" : "Scroll Left");
   scrollup->_cp = (void*)static_cast<uintptr_t>(0);

   new UIElement(this, flags, _UIScrollThumbMessageProc, "Scroll Thumb");

   auto scrolldown =
      new UIElement(this, flags, _UIScrollUpDownMessageProc, !_horizontal ? "Scroll Down" : "Scroll Right");
   scrolldown->_cp = (void*)static_cast<uintptr_t>(1);
}

// --------------------------------------------------
// Scrollbar pairs
// --------------------------------------------------
void UIScrollbarPair::layout_scrollbar_pair(int hSpace, int vSpace, int scrollBarSize, UIElement* el) {
   _vscroll->set_page(vSpace - (_hscroll->page() < _hscroll->maximum() ? scrollBarSize : 0));
   _hscroll->set_page(hSpace - (_vscroll->page() < _vscroll->maximum() ? scrollBarSize : 0));
   _vscroll->set_page(vSpace - (_hscroll->page() < _hscroll->maximum() ? scrollBarSize : 0));

   UIRectangle vScrollBarBounds = el->_bounds, hScrollBarBounds = el->_bounds;

   hScrollBarBounds.r = vScrollBarBounds.l =
      vScrollBarBounds.r - (_vscroll->page() < _vscroll->maximum() ? scrollBarSize : 0);
   vScrollBarBounds.b = hScrollBarBounds.t =
      hScrollBarBounds.b - (_hscroll->page() < _hscroll->maximum() ? scrollBarSize : 0);

   _vscroll->move(vScrollBarBounds, true);
   _hscroll->move(hScrollBarBounds, true);
}

inline void UIScrollbarPair::key_input_vscroll(UIKeyTyped* m, int rowHeight, int pageHeight, UIElement* el) {
   if (m->code == UIKeycode::UP)
      _vscroll->position() -= rowHeight;
   else if (m->code == UIKeycode::DOWN)
      _vscroll->position() += rowHeight;
   else if (m->code == UIKeycode::PAGE_UP)
      _vscroll->position() += pageHeight;
   else if (m->code == UIKeycode::PAGE_DOWN)
      _vscroll->position() -= pageHeight;
   else if (m->code == UIKeycode::HOME)
      _vscroll->position() = 0;
   else if (m->code == UIKeycode::END)
      _vscroll->position() = _vscroll->maximum();
   el->refresh();
}

// --------------------------------------------------
// Code views.
// --------------------------------------------------

UICode::buffer_ptr UICode::buffer_mgr::load_buffer(const std::string& path, std::optional<std::string_view> err /* = {} */) {
   if (auto it = _buffers.find(path); it != _buffers.end())
      return it->second;
   return {};
}

int UICode::column_to_byte(size_t ln, size_t column) const {
   return UI::column_to_byte(line(ln), column, tab_columns());
}

int UICode::byte_to_column(size_t ln, size_t byte) const { return UI::byte_to_column(line(ln), byte, tab_columns()); }

UICode& UICode::clear() {
   assert(!has_flag(UICode::MANAGE_BUFFER));
   _buffer->clear();
   _sel         = {};
   return *this;
}

UICode& UICode::load_file(const std::string& path, std::optional<std::string_view> err /* = {} */) {
   std::string buff = LoadFile(path);
   if (buff.empty())
      insert_content(err ? *err : std::format("The file '{}' could not be loaded.\n", path), true);
   else
      insert_content(buff, true);
   _current_line.reset();
   return *this;
}

UICode::code_pos_t UICode::code_pos_from_point(UIPoint pt) {
   with_font fnt(_font); // measure using _font
   UI* ui = this->ui();
   int      lineHeight   = ui->string_height();
   UIFont*  active_font  = ui->active_font();
   code_pos_t pos;
   pos.line = std::max(static_cast<int64_t>(0), (pt.y - _bounds.t + _vscroll->position()) / lineHeight);
   if (pos.line >= num_lines())
      pos.line = num_lines() - 1;
   int column = (pt.x - _bounds.l + _hscroll->position() + active_font->_glyph_width / 2) / active_font->_glyph_width;
   if (!has_flag(UICode::NO_MARGIN))
      column -= (ui->code_margin() + ui->code_margin_gap()) / active_font->_glyph_width;
   pos.offset = column_to_byte(pos.line, column);
   return pos;
}

// returns negative value if in margin
int UICode::hittest(int x, int y) {
   UI* ui = this->ui();
   x -= _bounds.l;

   if (x < 0 || x >= _vscroll->_bounds.l) {
      return 0;
   }

   y -= _bounds.t - _vscroll->position();

   with_font fnt(_font); // measure using _font
   int     lineHeight   = ui->string_height();
   bool    inMargin     = x < ui->code_margin() + ui->code_margin_gap() / 2 && !has_flag(UICode::NO_MARGIN);

   if (y < 0 || y >= lineHeight * static_cast<int>(num_lines())) {
      return 0;
   }

   int line = y / lineHeight + 1;
   return inMargin ? -line : line;
}

int UIPainter::draw_string_highlighted(UIRectangle lineBounds, std::string_view string, int tabSize,
                                       UIStringSelection* selection) {
   UI* ui = this->ui();
   if (string.size() > 10000)
      string = string.substr(0, 10000);

   enum _UICodeTokenType {
      UI_CODE_TOKEN_TYPE_DEFAULT,
      UI_CODE_TOKEN_TYPE_COMMENT,
      UI_CODE_TOKEN_TYPE_STRING,
      UI_CODE_TOKEN_TYPE_NUMBER,
      UI_CODE_TOKEN_TYPE_OPERATOR,
      UI_CODE_TOKEN_TYPE_PREPROCESSOR,
   };

   auto&    theme    = ui->theme();
   uint32_t colors[] = {
      theme.codeDefault, theme.codeComment,  theme.codeString,
      theme.codeNumber,  theme.codeOperator, theme.codePreprocessor,
   };

   int              lineHeight = ui->string_height();
   int              x          = lineBounds.l;
   int              y          = (lineBounds.t + lineBounds.b - lineHeight) / 2;
   int              ti         = 0;
   _UICodeTokenType tokenType  = UI_CODE_TOKEN_TYPE_DEFAULT;
   bool     inComment = false, inIdentifier = false, inChar = false, startedString = false, startedPreprocessor = false;
   uint32_t last = 0;
   int      j    = 0;

   size_t bytes = string.size();
   while (bytes) {
#ifdef UI_UNICODE
      size_t bytesConsumed;
      int    c = Utf8GetCodePoint(string.data(), bytes, &bytesConsumed);

      assert(bytesConsumed > 0);
      string = string.substr(bytesConsumed);
      bytes -= bytesConsumed;
#else
      char c = string[0];
      string = string.substr(1);
      bytes--;
#endif

      last <<= 8;
      last |= c & 0xFF;

      if (tokenType == UI_CODE_TOKEN_TYPE_PREPROCESSOR) {
         if (bytes && c == '/' && (string[0] == '/' || string[0] == '*')) {
            tokenType = UI_CODE_TOKEN_TYPE_DEFAULT;
         }
      } else if (tokenType == UI_CODE_TOKEN_TYPE_OPERATOR) {
         tokenType = UI_CODE_TOKEN_TYPE_DEFAULT;
      } else if (tokenType == UI_CODE_TOKEN_TYPE_COMMENT) {
         if ((last & 0xFF0000) == ('*' << 16) && (last & 0xFF00) == ('/' << 8) && inComment) {
            tokenType = startedPreprocessor ? UI_CODE_TOKEN_TYPE_PREPROCESSOR : UI_CODE_TOKEN_TYPE_DEFAULT;
            inComment = false;
         }
      } else if (tokenType == UI_CODE_TOKEN_TYPE_NUMBER) {
         if (!UI::is_alnum(c)) {
            tokenType = UI_CODE_TOKEN_TYPE_DEFAULT;
         }
      } else if (tokenType == UI_CODE_TOKEN_TYPE_STRING) {
         if (!startedString) {
            if (!inChar && ((last >> 8) & 0xFF) == '"' && ((last >> 16) & 0xFF) != '\\') {
               tokenType = UI_CODE_TOKEN_TYPE_DEFAULT;
            } else if (inChar && ((last >> 8) & 0xFF) == '\'' && ((last >> 16) & 0xFF) != '\\') {
               tokenType = UI_CODE_TOKEN_TYPE_DEFAULT;
            }
         }

         startedString = false;
      }

      if (tokenType == UI_CODE_TOKEN_TYPE_DEFAULT) {
         if (c == '#') {
            tokenType           = UI_CODE_TOKEN_TYPE_PREPROCESSOR;
            startedPreprocessor = true;
         } else if (bytes && c == '/' && string[0] == '/') {
            tokenType = UI_CODE_TOKEN_TYPE_COMMENT;
         } else if (bytes && c == '/' && string[0] == '*') {
            tokenType = UI_CODE_TOKEN_TYPE_COMMENT, inComment = true;
         } else if (c == '"') {
            tokenType     = UI_CODE_TOKEN_TYPE_STRING;
            inChar        = false;
            startedString = true;
         } else if (c == '\'') {
            tokenType     = UI_CODE_TOKEN_TYPE_STRING;
            inChar        = true;
            startedString = true;
         } else if (UI::is_digit(c) && !inIdentifier) {
            tokenType = UI_CODE_TOKEN_TYPE_NUMBER;
         } else if (!UI::is_alnum(c)) {
            tokenType    = UI_CODE_TOKEN_TYPE_OPERATOR;
            inIdentifier = false;
         } else {
            inIdentifier = true;
         }
      }

      int     oldX        = x;
      UIFont* active_font = ui->active_font();

      if (c == '\t') {
         x += active_font->_glyph_width, ti++;
         while (ti % tabSize)
            x += active_font->_glyph_width, ti++, j++;
      } else {
         draw_glyph(x, y, c, colors[tokenType]);
         x += active_font->_glyph_width, ti++;
      }

      if (selection && j >= selection->carets[0] && j < selection->carets[1]) {
         draw_block(UIRectangle(oldX, x, y, y + lineHeight), selection->colorBackground);
         if (c != '\t')
            draw_glyph(oldX, y, c, selection->colorText);
      }

      if (selection && selection->carets[0] == j) {
         draw_invert(UIRectangle(oldX, oldX + 1, y, y + lineHeight));
      }

      j++;
   }

   if (selection && selection->carets[0] == j) {
      draw_invert(UIRectangle(x, x + 1, y, y + lineHeight));
   }

   return x;
}

UICode& UICode::copy(sel_target_t t) {
   auto sel = selection();
   if (sel.size())
      _window->write_clipboard_text(sel, t);

   return *this;
}

UICode& UICode::update_selection() {
   bool swap      = _sel[3].line < _sel[2].line || (_sel[3].line == _sel[2].line && _sel[3].offset < _sel[2].offset);
   _sel[1 - swap] = _sel[3];
   _sel[0 + swap] = _sel[2];
   copy(sel_target_t::primary);
   _move_scroll_to_caret_next_layout = true;
   refresh();
   return *this;
}

UICode& UICode::_set_vertical_motion_column(bool restore) {
   if (restore) {
      _sel[3].offset = column_to_byte(_sel[3].line, _vertical_motion_column);
   } else if (!_use_vertical_motion_column) {
      _use_vertical_motion_column = true;
      _vertical_motion_column     = byte_to_column(_sel[3].line, _sel[3].offset);
   }
   return *this;
}

int UICode::_class_message_proc(UIMessage msg, int di, void* dp) {
   UI* ui = this->ui();
   if (msg == UIMessage::LAYOUT) {
      auto str_height = ui->string_height();

      with_font fnt(font()); // measure using font()
      int     scrollBarSize = scale(ui_size::scroll_bar);
      _vscroll->set_maximum(num_lines() * str_height);
      _hscroll->set_maximum(max_columns() * font()->_glyph_width); // TODO This doesn't take into account tab sizes!
      int vSpace = _bounds.height();
      int hSpace = _bounds.width();

      _vscroll->set_page(vSpace);
      _hscroll->set_page(hSpace);

      if (_move_scroll_to_caret_next_layout) {
         size_t top     = _sel[3].line * str_height;
         size_t bottom  = top + str_height;
         size_t context = str_height * 2;
         if (bottom > _vscroll->position() + vSpace - context)
            _vscroll->position() = bottom - vSpace + context;
         if (top < _vscroll->position() + context)
            _vscroll->position() = top - context;
         _move_scroll_to_caret_next_layout = _move_scroll_to_focus_next_layout = false;
         // TODO Horizontal scrolling.
      } else if (_move_scroll_to_focus_next_layout) {
         _vscroll->position() = (focus_line() + 0.5f) * str_height - _bounds.height() * 0.5f;
      }

      if (!has_flag(UICode::NO_MARGIN))
         hSpace -= ui->code_margin() + ui->code_margin_gap();
      layout_scrollbar_pair(hSpace, vSpace, scrollBarSize, this);

   } else if (msg == UIMessage::PAINT) {
      with_font fnt(font()); // measure using font()

      auto*       painter    = static_cast<UIPainter*>(dp);
      UIRectangle lineBounds = _bounds;

      lineBounds.r = _vscroll->_bounds.l;

      if (!has_flag(UICode::NO_MARGIN)) {
         lineBounds.l += ui->code_margin() + ui->code_margin_gap();
      }

      int lineHeight = ui->string_height();
      lineBounds.t -= _vscroll->position() % lineHeight;

      auto& theme = ui->theme();
      painter->draw_block(_bounds, theme.codeBackground);

      UIStringSelection selection = {};
      selection.colorBackground   = theme.selected;
      selection.colorText         = theme.textSelected;

      for (size_t i = _vscroll->position() / lineHeight; i < num_lines(); i++) {
         if (lineBounds.t > _clip.b) {
            break;
         }

         lineBounds.b = lineBounds.t + lineHeight;

         if (!has_flag(UICode::NO_MARGIN)) {
            char   string[16];
            size_t p          = 16;
            size_t lineNumber = i + 1;

            while (lineNumber) {
               string[--p] = (lineNumber % 10) + '0';
               lineNumber /= 10;
            }

            UIRectangle marginBounds = lineBounds;
            marginBounds.r           = marginBounds.l - ui->code_margin_gap();
            marginBounds.l -= ui->code_margin() + ui->code_margin_gap();

            uint32_t marginColor = message(UIMessage::CODE_GET_MARGIN_COLOR, static_cast<uint32_t>(i + 1), 0);

            if (marginColor) {
               painter->draw_block(marginBounds, marginColor);
            }

            painter->draw_string(marginBounds, {string + p, static_cast<size_t>(16 - p)},
                                 marginColor ? theme.codeDefault : theme.codeComment, UIAlign::right, nullptr);
         }

         if (focus_line() == i) {
            painter->draw_block(lineBounds, theme.codeFocused);
         }

         UIRectangle oldClip = painter->_clip;
         painter->_clip      = intersection(oldClip, lineBounds);
         if (_hscroll)
            lineBounds.l -= _hscroll->position();

         bool selected = is_focused() && i >= _sel[0].line && i <= _sel[1].line;
         if (selected) {
            selection.carets[0] = (i == _sel[0].line) ? byte_to_column(i, _sel[0].offset) : 0;
            selection.carets[1] = (i == _sel[1].line) ? byte_to_column(i, _sel[1].offset) : static_cast<int>(line(i).size());
         }

         int x = painter->draw_string_highlighted(lineBounds, line(i), tab_columns(), selected ? &selection : nullptr);
         int y = (lineBounds.t + lineBounds.b - ui->string_height()) / 2;

         if (selected && i < _sel[1].line) {
            painter->draw_block(ui_rect_4pd(x, y, font()->_glyph_width, font()->_glyph_height),
                                selection.colorBackground);
         }

         if (_hscroll)
            lineBounds.l += _hscroll->position();
         painter->_clip = oldClip;

         UICodeDecorateLine m;
         m.x = x, m.y = y, m.bounds = lineBounds, m.index = static_cast<int>(i), m.painter = painter;
         message(UIMessage::CODE_DECORATE_LINE, 0, &m);

         lineBounds.t += lineHeight;
      }
   } else if (msg == UIMessage::SCROLLED) {
      _move_scroll_to_focus_next_layout = false;
      refresh();
   } else if (msg == UIMessage::MOUSE_WHEEL) {
      return _vscroll->message(msg, di, dp);
   } else if (msg == UIMessage::GET_CURSOR) {
      if (hittest(cursor_pos()) < 0) {
         return static_cast<int>(UICursor::flipped_arrow);
      }

      if (has_flag(UICode::SELECTABLE)) {
         return static_cast<int>(UICursor::text);
      }
   } else if (msg == UIMessage::LEFT_UP) {
      animate(true);
   } else if (msg == UIMessage::LEFT_DOWN && !empty()) {
      int hitTest          = hittest(cursor_pos());
      _left_down_in_margin = hitTest < 0;

      if (hitTest > 0 && has_flag(UICode::SELECTABLE)) {
         _sel[2] = code_pos_from_point(cursor_pos());
         _class_message_proc(UIMessage::MOUSE_DRAG, 0, nullptr); // updates `_sel[3]` with last mouse position
         focus();
         animate(false);
         set_last_animate_time(UI_CLOCK());
      }
   } else if (msg == UIMessage::ANIMATE) {
      if (is_pressed() && _window->pressed_button() == 1 && !empty() && !_left_down_in_margin) {
         UI_CLOCK_T previous     = last_animate_time();
         UI_CLOCK_T current      = UI_CLOCK();
         UI_CLOCK_T deltaTicks   = current - previous;
         double     deltaSeconds = (double)deltaTicks / UI_CLOCKS_PER_SECOND;
         if (deltaSeconds > 0.1)
            deltaSeconds = 0.1;
         int delta = deltaSeconds * 800;
         if (!delta) {
            return 0;
         }
         set_last_animate_time(current);

         {
            with_font fnt(font()); // measure using font()
            auto      pos = cursor_pos();
            if (pos.x < _bounds.l + (has_flag(UICode::NO_MARGIN) ? ui->code_margin_gap()
                                                                 : (ui->code_margin() + ui->code_margin_gap() * 2))) {
               _hscroll->position() -= delta;
            } else if (pos.x >= _vscroll->_bounds.l - ui->code_margin_gap()) {
               _hscroll->position() += delta;
            }

            if (pos.y < _bounds.t + ui->code_margin_gap()) {
               _vscroll->position() -= delta;
            } else if (pos.y >= _hscroll->_bounds.t - ui->code_margin_gap()) {
               _vscroll->position() += delta;
            }

            _move_scroll_to_focus_next_layout = false;
         }
         UICode::_ClassMessageProc(this, UIMessage::MOUSE_DRAG, di, dp);
         refresh();
      }
   } else if (msg == UIMessage::MOUSE_DRAG && _window->pressed_button() == 1 && !empty() && !_left_down_in_margin) {
      // TODO Double-click and triple-click dragging for word and line granularity respectively.
      _sel[3] = code_pos_from_point(cursor_pos());
      update_selection();
      _move_scroll_to_focus_next_layout = _move_scroll_to_caret_next_layout = false;
      _use_vertical_motion_column                                           = false;
   } else if (msg == UIMessage::KEY_TYPED && !empty()) {
      auto* m = static_cast<UIKeyTyped*>(dp);

      if ((m->code == UI_KEYCODE_LETTER('C') || m->code == UI_KEYCODE_LETTER('X') || m->code == UIKeycode::INSERT) &&
          is_only_ctrl_on()) {
         copy(sel_target_t::clipboard);
      } else if ((m->code == UIKeycode::UP || m->code == UIKeycode::DOWN || m->code == UIKeycode::PAGE_UP ||
                  m->code == UIKeycode::PAGE_DOWN) &&
                 !is_ctrl_on() && !is_alt_on()) {
         with_font fnt(font()); // measure using font()
         int       lineHeight = ui->string_height();

         if (is_shift_on()) {
            if (m->code == UIKeycode::UP) {
               if (static_cast<int>(_sel[3].line) - 1 >= 0) {
                  _set_vertical_motion_column(false);
                  _sel[3].line--;
                  _set_vertical_motion_column(true);
               }
            } else if (m->code == UIKeycode::DOWN) {
               if (_sel[3].line + 1 < num_lines()) {
                  _set_vertical_motion_column(false);
                  _sel[3].line++;
                  _set_vertical_motion_column(true);
               }
            } else if (m->code == UIKeycode::PAGE_UP || m->code == UIKeycode::PAGE_DOWN) {
               _set_vertical_motion_column(false);
               int pageHeight = (_bounds.t - _hscroll->_bounds.t) / lineHeight * 4 / 5;
               _sel[3].line += m->code == UIKeycode::PAGE_UP ? pageHeight : -pageHeight;
               // if (selection[3].line < 0)
               //    selection[3].line = 0;
               if (_sel[3].line >= num_lines())
                  _sel[3].line = num_lines() - 1;
               _set_vertical_motion_column(true);
            }

            update_selection();
         } else {
            _move_scroll_to_focus_next_layout = false;
            key_input_vscroll(m, lineHeight,
                              (_bounds.t - _hscroll->_bounds.t) * 4 / 5 /* leave a few lines for context */, this);
         }
      } else if ((m->code == UIKeycode::HOME || m->code == UIKeycode::END) && !is_alt_on()) {
         if (is_shift_on()) {
            if (m->code == UIKeycode::HOME) {
               if (is_ctrl_on())
                  _sel[3].line = 0;
               _sel[3].offset              = 0;
               _use_vertical_motion_column = false;
            } else {
               if (is_ctrl_on())
                  _sel[3].line = num_lines() - 1;
               _sel[3].offset              = line(_sel[3].line).size();
               _use_vertical_motion_column = false;
            }

            update_selection();
         } else {
            _vscroll->position()              = m->code == UIKeycode::HOME ? 0 : _vscroll->maximum();
            _move_scroll_to_focus_next_layout = false;
            refresh();
         }
      } else if ((m->code == UIKeycode::LEFT || m->code == UIKeycode::RIGHT) && !is_alt_on()) {
         if (is_shift_on()) {
            move_caret(m->code == UIKeycode::LEFT, is_ctrl_on());
         } else if (!is_ctrl_on()) {
            UIFont* active_font = ui->active_font();
            _hscroll->position() += m->code == UIKeycode::LEFT ? -active_font->_glyph_width : active_font->_glyph_width;
            refresh();
         } else {
            return 0;
         }
      } else {
         return 0;
      }

      return 1;
   } else if (msg == UIMessage::RIGHT_DOWN) {
      int hitTest = hittest(cursor_pos());

      if (hitTest > 0 && has_flag(UICode::SELECTABLE)) {
         focus();
         size_t           from     = offset(selection(0));
         size_t           to       = offset(selection(1));
         int              disabled = from != to ? 0 : disabled_flag;
         std::string_view sel{&(*this)[from], to - from};

         auto& menu = ui->create_menu(_window, UIMenu::NO_SCROLL).add_item(disabled, "Copy", [this, sel](UIButton&) {
            _window->write_clipboard_text(sel, sel_target_t::clipboard);
         });
         for (const auto& mi : _menu_items) {
            menu.add_item(disabled, mi.label, [&invoke = mi.invoke, sel](UIButton&) { invoke(sel); });
         }
         menu.show();
      }
   } else if (msg == UIMessage::UPDATE) {
      repaint(nullptr);
   } else if (msg == UIMessage::DEALLOCATE) {
      clear();
   }

   return 0;
}

UICode& UICode::move_caret(bool backward, bool word) {
   while (true) {
      if (backward) {
         if (static_cast<int>(_sel[3].offset) - 1 < 0) {
            if (_sel[3].line > 0) {
               _sel[3].line--;
               _sel[3].offset = code_line(_sel[3].line).bytes;
            } else
               break;
         } else
            _ui_move_caret_backwards(_sel[3].offset, &(*_buffer)[0], offset(_sel[3]), code_line(_sel[3].line).offset);
      } else {
         if (_sel[3].offset + 1 > code_line(_sel[3].line).bytes) {
            if (_sel[3].line + 1 < num_lines()) {
               _sel[3].line++;
               _sel[3].offset = 0;
            } else
               break;
         } else
            _ui_move_caret_forward(_sel[3].offset, std::string_view(&(*_buffer)[0], size()), offset(_sel[3]));
      }

      if (!word)
         break;

      if (_sel[3].offset != 0 && _sel[3].offset != code_line(_sel[3].line).bytes) {
         if (_ui_move_caret_by_word(std::string_view{&(*_buffer)[0], size()}, offset(_sel[3])))
            break;
      }
   }

   _use_vertical_motion_column = false;
   update_selection();
   return *this;
}

// Line numbers are 1-indexed!!
UICode& UICode::set_focus_line(size_t index) {
   _focus_line                       = index;
   _move_scroll_to_focus_next_layout = true;
   refresh();
   return *this;
}

void UICode::buffer_t::insert_content(std::string_view new_content) {
   size_t sz = new_content.size();
   if (!sz)
      return;

   size_t orig_size           = _content.size();
   bool   append_to_last_line = (!_content.empty() && _content.back() != '\n');

   _content.resize(orig_size + sz);

   size_t num_new_lines = 0;
   for (size_t i = 0; i < sz; ++i) {
      _content[orig_size + i] = new_content[i];
      if (new_content[i] == '\n')
         ++num_new_lines;
   }

   size_t orig_lines = _lines.size();
   _lines.reserve(orig_lines + num_new_lines);

   for (size_t i = 0, offset = 0; i <= sz; ++i) {
      if (i == sz || new_content[i] == '\n') { // if `new_content` not `\n` terminated, still process the last line
         if (append_to_last_line) {
            assert(offset == 0);
            append_to_last_line = false;
            increase_last_line_length(i);
         } else
            emplace_back_line(orig_size + offset, i - offset);
         offset = i + 1;
         if (i == sz - 1)
            break; // we already finished the line with the `\n`
      }
   }

}

UICode& UICode::insert_content(std::string_view new_content, bool replace) {
   assert(!has_flag(UICode::MANAGE_BUFFER));
   constexpr size_t max_size = 1000000000;
   if (new_content.size() > max_size)
      new_content = new_content.substr(0, max_size);

   _use_vertical_motion_column = false;

   if (replace)
      clear();

   _buffer->insert_content(new_content);

   if (!replace) {
      with_font fnt(_font); // measure using _font
      _vscroll->position() = num_lines() * ui()->string_height();
   }

   repaint(nullptr);
   return *this;
}

UICode::UICode(UIElement* parent, uint32_t flags)
   : UIElementCast<UICode>(parent, flags, UICode::_ClassMessageProc, "Code")
   , UIScrollbarPair(this)
   , _font(parent->ui()->active_font()) {
   _buffer = std::make_shared<buffer_t>();
}

// --------------------------------------------------
// Gauges.
// --------------------------------------------------

int UIGauge::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_HEIGHT) {
      return scale(_vertical ? ui_size::gauge_width : ui_size::gauge_height);
   }
   if (msg == UIMessage::GET_WIDTH) {
      return scale(_vertical ? ui_size::gauge_height : ui_size::gauge_width);
   }
   if (msg == UIMessage::PAINT) {
      static_cast<UIPainter*>(dp)->draw_control(
         _bounds, UIControl::gauge | state() | (_vertical ? UIControl::state_vertical : 0), {}, _position, get_scale());
   }

   return 0;
}

UIGauge& UIGauge::set_position(double new_pos) {
   new_pos = std::clamp(new_pos, 0., 1.);
   if (ui_set(_position, new_pos)) {
      repaint(nullptr);
   }
   return *this;
}

UIGauge::UIGauge(UIElement* parent, uint32_t flags)
   : UIElementCast<UIGauge>(parent, flags, UIGauge::_ClassMessageProc, "Gauge")
   , _vertical(!!(flags & vertical_flag)) {}

// --------------------------------------------------
// Sliders.
// --------------------------------------------------

int UISlider::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_HEIGHT) {
      return scale(_vertical ? ui_size::slider_width : ui_size::slider_height);
   }
   if (msg == UIMessage::GET_WIDTH) {
      return scale(_vertical ? ui_size::slider_height : ui_size::slider_width);
   }
   if (msg == UIMessage::PAINT) {
      static_cast<UIPainter*>(dp)->draw_control(
         _bounds, UIControl::slider | state() | (_vertical ? UIControl::state_vertical : 0), {}, _position,
         get_scale());
   } else if (msg == UIMessage::LEFT_DOWN || (msg == UIMessage::MOUSE_DRAG && _window->pressed_button() == 1)) {
      UIRectangle bounds    = _bounds;
      int         thumbSize = scale(ui_size::slider_thumb);
      auto        pos       = cursor_pos();
      _position = _vertical ? 1 - (static_cast<float>(pos.y - thumbSize / 2 - bounds.t) / (bounds.height() - thumbSize))
                            : static_cast<double>(pos.x - thumbSize / 2 - bounds.l) / (bounds.width() - thumbSize);
      if (_steps > 1)
         _position = static_cast<int>(_position * (_steps - 1) + 0.5f) / static_cast<double>(_steps - 1);
      if (_position < 0)
         _position = 0;
      if (_position > 1)
         _position = 1;
      message(UIMessage::VALUE_CHANGED, 0, nullptr);
      repaint(nullptr);
   } else if (msg == UIMessage::UPDATE) {
      repaint(nullptr);
   } else if (msg == UIMessage::VALUE_CHANGED) {
      if (_on_value_changed) {
         _on_value_changed(*this);
      }
   }

   return 0;
}

UISlider& UISlider::set_position(double new_pos) {
   new_pos = std::clamp(new_pos, 0., 1.);
   if (ui_set(_position, new_pos)) {
      if (_steps > 1)
         _position = static_cast<int>(_position * (_steps - 1) + 0.5f) / static_cast<float>(_steps - 1);
      message(UIMessage::VALUE_CHANGED, 0, nullptr);
      repaint(nullptr);
   }
   return *this;
}

UISlider::UISlider(UIElement* parent, uint32_t flags)
   : UIElementCast<UISlider>(parent, flags, UISlider::_ClassMessageProc, "Slider")
   , _vertical(!!(flags & vertical_flag)) {}

// --------------------------------------------------
// Tables.
// --------------------------------------------------

int UITable::hittest(int x, int y) {
   x -= _bounds.l;

   if (x < 0 || x >= _vscroll->_bounds.l) {
      return -1;
   }

   y -= (_bounds.t + scale(ui_size::table_header)) - _vscroll->position();

   int rowHeight = scale(ui_size::table_row);

   if (y < 0 || y >= static_cast<int>(rowHeight * _num_items)) {
      return -1;
   }

   return y / rowHeight;
}

int UITable::header_hittest(int x, int y) {
   if (_column_widths.empty())
      return -1;
   UIRectangle header = _bounds;
   header.b           = header.t + scale(ui_size::table_header);
   header.l += scale(ui_size::table_column_gap);
   int position = 0, index = 0;

   while (true) {
      size_t end = column_end(position);
      header.r   = header.l + _column_widths[index];
      if (header.contains(x, y))
         return index;
      header.l += _column_widths[index] + scale(ui_size::table_column_gap);
      if (_columns[end] != '\t')
         break;
      position = end + 1;
      index++;
   }

   return -1;
}

bool UITable::ensure_visible(int index) {
   int rowHeight = scale(ui_size::table_row);
   int y         = index * rowHeight;
   y -= _vscroll->position();
   int height = _bounds.height() - scale(ui_size::table_header) - rowHeight;

   if (y < 0) {
      _vscroll->position() += y;
      refresh();
      return true;
   }
   if (y > height) {
      _vscroll->position() -= height - y;
      refresh();
      return true;
   }
   return false;
}

UITable& UITable::resize_columns() {
   size_t position = 0;
   size_t count    = 0;

   while (true) {
      size_t end = column_end(position);
      count++;
      if (_columns[end] == '\t')
         position = end + 1;
      else
         break;
   }

   _column_widths.resize(count);

   position = 0;

   UITableGetItem m(256);

   while (true) {
      size_t end     = column_end(position);
      int    longest = ui()->string_width(column(position, end));

      for (size_t i = 0; i < num_items(); i++) {
         m._row    = i;
         int bytes = message(UIMessage::TABLE_GET_ITEM, 0, &m);
         int width = ui()->string_width(m.buff(bytes));

         if (width > longest) {
            longest = width;
         }
      }

      _column_widths[m._column] = longest;
      m._column++;
      if (_columns[end] == '\t')
         position = end + 1;
      else
         break;
   }

   repaint(nullptr);
   return *this;
}

int UITable::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::PAINT) {
      auto*       painter = static_cast<UIPainter*>(dp);
      UIRectangle bounds  = _bounds;
      bounds.r            = _vscroll->_bounds.l;
      painter->draw_control(_bounds, UIControl::table_background | state(), {}, 0, get_scale());
      UIRectangle    row       = bounds;
      int            rowHeight = scale(ui_size::table_row);
      UITableGetItem m(256);
      row.t += scale(ui_size::table_header);
      row.t -= _vscroll->position() % rowHeight;
      int         hovered = hittest(cursor_pos());
      UIRectangle oldClip = painter->_clip;
      painter->_clip =
         intersection(oldClip, UIRectangle(bounds.l, bounds.r, bounds.t + scale(ui_size::table_header), bounds.b));

      for (int i = _vscroll->position() / rowHeight; i < static_cast<int>(num_items()); i++) {
         if (row.t > painter->_clip.b) {
            break;
         }

         row.b          = row.t + rowHeight;
         m._row         = i;
         m._is_selected = false;
         m._column      = 0;

         int bytes = message(UIMessage::TABLE_GET_ITEM, 0, &m);

         uint32_t rowFlags =
            (m._is_selected ? UIControl::state_selected : 0) | (hovered == i ? UIControl::state_hovered : 0);
         painter->draw_control(row, UIControl::table_row | rowFlags, {}, 0, get_scale());

         UIRectangle cell = row;
         cell.l += scale(ui_size::table_column_gap) - _hscroll->position();

         for (size_t j = 0; j < _column_widths.size(); j++) {
            if (j) {
               m._column = j;
               bytes     = message(UIMessage::TABLE_GET_ITEM, 0, &m);
            }

            cell.r = cell.l + _column_widths[j];
            if (static_cast<size_t>(bytes) > m.buff_size() && bytes > 0)
               bytes = m.buff_size();
            if (bytes > 0)
               painter->draw_control(cell, UIControl::table_cell | rowFlags, m.buff(bytes), 0, get_scale());
            cell.l += _column_widths[j] + scale(ui_size::table_column_gap);
         }

         row.t += rowHeight;
      }

      bounds         = _bounds;
      painter->_clip = intersection(oldClip, bounds);
      if (_hscroll)
         bounds.l -= _hscroll->position();

      UIRectangle header = bounds;
      header.b           = header.t + scale(ui_size::table_header);
      header.l += scale(ui_size::table_column_gap);

      size_t position = 0;
      size_t index    = 0;

      if (!_column_widths.empty()) {
         while (true) {
            size_t end = column_end(position);

            header.r = header.l + _column_widths[index];
            painter->draw_control(
               header, UIControl::table_header | (index == _column_highlight ? UIControl::state_selected : 0),
               column(position, end), 0, get_scale());
            header.l += _column_widths[index] + scale(ui_size::table_column_gap);

            if (_columns[end] == '\t') {
               position = end + 1;
               index++;
            } else {
               break;
            }
         }
      }
   } else if (msg == UIMessage::TABLE_GET_ITEM) {
      if (_on_getitem)
         return _on_getitem(*this, *static_cast<UITableGetItem*>(dp));
   } else if (msg == UIMessage::LAYOUT) {
      int scrollBarSize = scale(ui_size::scroll_bar);
      int columnGap     = scale(ui_size::table_column_gap);

      _vscroll->set_maximum(num_items() * scale(ui_size::table_row));
      _hscroll->set_maximum(columnGap);
      for (auto width : _column_widths) {
         _hscroll->set_maximum(_hscroll->maximum() + width + columnGap);
      }

      int vSpace = _bounds.height() - scale(ui_size::table_header);
      int hSpace = _bounds.width();

      _vscroll->set_page(vSpace);
      _hscroll->set_page(hSpace);

      layout_scrollbar_pair(hSpace, vSpace, scrollBarSize, this);
   } else if (msg == UIMessage::MOUSE_MOVE || msg == UIMessage::UPDATE) {
      repaint(nullptr);
   } else if (msg == UIMessage::SCROLLED) {
      refresh();
   } else if (msg == UIMessage::MOUSE_WHEEL) {
      return _vscroll->message(msg, di, dp);
   } else if (msg == UIMessage::LEFT_DOWN) {
      focus();
      if (_on_click)
         _on_click(*this);
   } else if (msg == UIMessage::KEY_TYPED) {
      auto* m = static_cast<UIKeyTyped*>(dp);

      if ((m->code == UIKeycode::UP || m->code == UIKeycode::DOWN || m->code == UIKeycode::PAGE_UP ||
           m->code == UIKeycode::PAGE_DOWN || m->code == UIKeycode::HOME || m->code == UIKeycode::END) &&
          !is_modifier_on()) {
         key_input_vscroll(m, scale(ui_size::table_row),
                           (_bounds.t - _hscroll->_bounds.t + ui_size::table_header) * 4 / 5, this);
         return 1;
      } if ((m->code == UIKeycode::LEFT || m->code == UIKeycode::RIGHT) && !is_modifier_on()) {
         UIFont* active_font = ui()->active_font();
         _hscroll->position() += m->code == UIKeycode::LEFT ? -active_font->_glyph_width : active_font->_glyph_width;
         refresh();
         return 1;
      }
   } else if (msg == UIMessage::DEALLOCATE) {
   }

   return 0;
}

UITable::UITable(UIElement* parent, uint32_t flags, const char* columns)
   : UIElementCast<UITable>(parent, flags, UITable::_ClassMessageProc, "Table")
   , UIScrollbarPair(this)
   , _columns(columns)
   , _column_highlight(static_cast<size_t>(-1)) {}

// --------------------------------------------------
// Textboxes.
// --------------------------------------------------

int UITextbox::_byte_to_column(std::string_view string, int byte) { return UI::byte_to_column(string, byte, 4); }

int UITextbox::_column_to_byte(std::string_view string, int column) { return UI::column_to_byte(string, column, 4); }

UITextbox& UITextbox::replace_text(std::string_view text, bool sendChangedMessage) {
   auto   sz         = _buffer.size();
   size_t deleteFrom = _carets[0];
   size_t deleteTo   = _carets[1];
   assert(deleteFrom <= sz && deleteTo <= sz);

   if (deleteFrom > deleteTo)
      std::swap(deleteFrom, deleteTo);

   // first remove the selection
   // --------------------------
   _buffer.erase(deleteFrom, deleteTo - deleteFrom);

   // then insert new text
   // --------------------
   _buffer.insert(deleteFrom, text);
   _carets[0] = deleteFrom + text.size();
   _carets[1] = _carets[0];

   if (sendChangedMessage)
      message(UIMessage::VALUE_CHANGED, 0, nullptr);
   _window->set_textbox_modified_flag(true);
   _save_state();
   repaint(nullptr);
   return *this;
}

UITextbox& UITextbox::clear(bool sendChangedMessage) {
   _carets[1] = 0;
   _carets[0] = text().size();
   return replace_text("", sendChangedMessage);
}

UITextbox& UITextbox::move_caret(bool backward, bool word) {
   scoped_guard _([this]() { _save_state(); }); // save undo state if needed

   while (true) {
      std::string_view cur_text = text();
      if (_carets[0] > 0 && backward) {
         _ui_move_caret_backwards(_carets[0], cur_text.data(), _carets[0], 0);
      } else if (_carets[0] < cur_text.size() && !backward) {
         _ui_move_caret_forward(_carets[0], cur_text, _carets[0]);
      } else {
         return *this;
      }

      if (!word) {
         return *this;
      } if (_carets[0] != cur_text.size() && _carets[0] != 0) {
         if (_ui_move_caret_by_word(cur_text, _carets[0]))
            break;
      }
   }

   repaint(nullptr);
   return *this;
}

UITextbox& UITextbox::copy() {
   size_t from = std::min(_carets[0], _carets[1]);
   size_t to   = std::max(_carets[0], _carets[1]);

   if (from != to) {
      auto cur_text = text();
      _window->write_clipboard_text(std::string_view{&cur_text[from], to - from}, sel_target_t::clipboard);
   }
   return *this;
}

UITextbox& UITextbox::paste(sel_target_t t) {
   std::string cur_text = _window->read_clipboard_text(t);

   if (!cur_text.empty()) {
      for (auto& c : cur_text)
         if (c == '\n')
            c = ' ';

      replace_text(cur_text, true);
   }
   return *this;
}

std::string_view UITextbox::_text() {
   std::string_view cur_text = text();
   if (has_flag(HIDE_CHARACTERS)) {
      if (_hidden_str.size() < cur_text.size())
         _hidden_str.resize(cur_text.size(), '*');
      return std::string_view(_hidden_str.data(), cur_text.size());
   }
   return cur_text;
}

int UITextbox::_class_message_proc(UIMessage msg, int di, void* dp) {
   UI* ui = this->ui();
   if (msg == UIMessage::GET_HEIGHT) {
      return scale(ui_size::textbox_height);
   }
   if (msg == UIMessage::GET_WIDTH) {
      return scale(ui_size::textbox_width);
   }
   if (msg == UIMessage::PAINT) {
      static_cast<UIPainter*>(dp)->draw_control(_bounds, UIControl::textbox | state(), {}, 0, get_scale());

      std::string_view cur_text = _text();

      int         scaledMargin = scale(ui_size::textbox_margin);
      int         totalWidth   = ui->string_width(cur_text) + scaledMargin * 2;
      UIRectangle textBounds   = _bounds + ui_rect_1i(scaledMargin);

      if (_scroll > totalWidth - textBounds.width()) {
         _scroll = totalWidth - textBounds.width();
      }

      if (_scroll < 0) {
         _scroll = 0;
      }
      int caretX = ui->string_width(cur_text.substr(0, _carets[0])) - _scroll;

      if (caretX < 0) {
         _scroll = caretX + _scroll;
      } else if (caretX > textBounds.width()) {
         _scroll = caretX - textBounds.width() + _scroll + 1;
      }

      auto&             theme     = ui->theme();
      UIStringSelection selection = {};
      selection.carets[0]         = _byte_to_column(cur_text, _carets[0]);
      selection.carets[1]         = _byte_to_column(cur_text, _carets[1]);
      selection.colorBackground   = theme.selected;
      selection.colorText         = theme.textSelected;
      textBounds.l -= _scroll;

      if (!cur_text.empty()) {
         static_cast<UIPainter*>(dp)->draw_string(textBounds, cur_text, is_disabled() ? theme.textDisabled : theme.text,
                                                  UIAlign::left, is_focused() ? &selection : nullptr);
      }
   } else if (msg == UIMessage::GET_CURSOR) {
      return static_cast<int>(UICursor::text);
   } else if (msg == UIMessage::LEFT_DOWN) {
      scoped_guard _([this]() { _save_state(); }); // save undo state if needed
      UIFont*      active_font = ui->active_font();
      int          column      = (_window->cursor_pos().x - _bounds.l + _scroll - scale(ui_size::textbox_margin) +
                    active_font->_glyph_width / 2) /
                   active_font->_glyph_width;
      _carets[0] = _carets[1] = column <= 0 ? 0 : _column_to_byte(_text(), column);
      focus();
   } else if (msg == UIMessage::UPDATE) {
      repaint(nullptr);
   } else if (msg == UIMessage::DEALLOCATE) {
      ;
   } else if (msg == UIMessage::KEY_TYPED) {
      scoped_guard _([this]() { _save_state(); }); // save undo state if needed

      auto* m         = static_cast<UIKeyTyped*>(dp);
      bool  handled   = true;
      bool  shift     = !!is_shift_on();
      bool  alt       = !!is_alt_on();
      bool  ctrl      = is_ctrl_on() && !alt;
      bool  modifier  = ctrl || alt || shift;
      bool  selection = _carets[0] != _carets[1];

      if (_reject_next_key) {
         _reject_next_key = false;
         handled          = false;
      } else if (m->code == UIKeycode::BACKSPACE || m->code == UIKeycode::DEL) {
         _delete_one(m->code == UIKeycode::BACKSPACE, ctrl);
      } else if (m->code == UIKeycode::LEFT || m->code == UIKeycode::RIGHT) {
         _move_one(m->code == UIKeycode::LEFT, shift, is_ctrl_on());
      } else if (_on_key_up_down && (m->code == UIKeycode::UP || m->code == UIKeycode::DOWN)) {
         handled = _on_key_up_down(*this, m->code);
      } else if (m->code == UIKeycode::HOME || m->code == UIKeycode::END) {
         _move_to_end(m->code == UIKeycode::HOME, shift);
      } else if (!m->text.empty() && !alt && !ctrl && m->text[0] >= 0x20) {
         replace_text(m->text, true);
      } else if ((m->is('c') || m->is('x') || m->code == UIKeycode::INSERT) && ctrl && !shift) {
         copy();
         if (m->is('x'))
            replace_text("", true);
      } else if (((m->is('v')) && ctrl && !shift) || (m->code == UIKeycode::INSERT && !modifier)) {
         paste(sel_target_t::clipboard);
      } else {
         // implement some readline key bindings - see https://readline.kablamo.org/emacs.html
         // ----------------------------------------------------------------------------------
         if (ctrl && (m->is('a') || m->is('e'))) {
            _move_to_end(m->is('a'), shift); // `ctrl-a` move to beg of line, `ctrl-e` to the end
         } else if ((ctrl ^ alt) && (m->is('f') || m->is('b'))) {
            _move_one(m->is('b'), shift, alt); // `ctrl-f` move forward, `ctrl-b` back, with `alt` by word
         } else if ((ctrl ^ alt) && !shift && m->is('d')) {
            if (selection)
               replace_text("", true); // if there is selected text, just delete it
            else
               _delete_one(false, alt); // `ctrl-d` deletes one char, `alt-d` one word
         } else if ((ctrl && !alt && !shift) && (m->is('u') || m->is('k'))) {
            if (!selection)
               _carets[0] = m->is('u') ? 0 : text().size();
            replace_text("", true); // if there is selected text, just delete it
         } else {
            // undo/redo
            // ---------
            if (ctrl && m->is('_')) {
               undo();
            } else {
               handled = false;
            }
            _.dismiss();
         }
      }

      if (handled) {
         repaint(nullptr);
         return 1;
      }
   } else if (msg == UIMessage::RIGHT_DOWN) {
      size_t c0 = _carets[0], c1 = _carets[1];
      _class_message_proc(UIMessage::LEFT_DOWN, di, dp);

      if (c0 < c1 ? (_carets[0] >= c0 && _carets[0] < c1) : (_carets[0] >= c1 && _carets[0] < c0)) {
         _carets[0] = c0,
         _carets[1] = c1; // Only move caret if clicking outside the existing selection.
         _save_state();
      }

      std::string paste_str = _window->read_clipboard_text(sel_target_t::clipboard);
      ui->create_menu(_window, UIMenu::NO_SCROLL)
         .add_item(_carets[0] == _carets[1] ? disabled_flag : 0, "Copy", [this](UIButton&) { copy(); })
         .add_item(paste_str.empty() ? disabled_flag : 0, "Paste",
                   [this](UIButton&) { paste(sel_target_t::clipboard); })
         .show();
   } else if (msg == UIMessage::MIDDLE_DOWN) {
      paste(sel_target_t::primary);
      repaint(nullptr);
      return 1;
   }

   return 0;
}

UITextbox::UITextbox(UIElement* parent, uint32_t flags)
   : UIElementCast<UITextbox>(parent, flags | tab_stop_flag, UITextbox::_ClassMessageProc, "Textbox") {}

void UITextbox::_delete_one(bool backwards, bool by_word) {
   scoped_guard _([this]() { _save_state(); }); // save undo state if needed
   if (_carets[0] == _carets[1]) {
      move_caret(backwards, by_word);
   }
   replace_text("", true);
}

void UITextbox::_move_one(bool backwards, bool select, bool by_word) {
   scoped_guard _([this]() { _save_state(); }); // save undo state if needed
   if (_carets[0] == _carets[1] || select) {
      move_caret(backwards, by_word);
      if (!select)
         _carets[1] = _carets[0];
   } else {
      _carets[1 - select] = _carets[select];
   }
}

void UITextbox::_move_to_end(bool backwards, bool select) {
   scoped_guard _([this]() { _save_state(); }); // save undo state if needed
   _carets[0] = (backwards ? 0ull : text().size());
   if (!select)
      _carets[1] = _carets[0];
}

UITextbox& UITextbox::select_all() {
   scoped_guard _([this]() { _save_state(); }); // save undo state if needed
   _carets[1] = 0;
   _carets[0] = text().size();
   return *this;
}

void UITextbox::_update_state(const textbox_state_t& s) {
   *static_cast<textbox_state_t*>(this) = s;
   _undo_states.push_back(s);
}

void UITextbox::_save_state() {
   assert(_undo_idx <= _undo_states.size());
   const auto& s = *static_cast<textbox_state_t*>(this);
   if (_undo_states.empty() || s != _undo_states.back()) {
      constexpr size_t max_size = 4096;
      if (_undo_states.size() > max_size)
         _undo_states.erase(_undo_states.cbegin(), _undo_states.cbegin() + (max_size / 2));
      _undo_states.push_back(s);
      _undo_idx = _undo_states.size() - 1;
   }
}

UITextbox& UITextbox::undo() {
   if (_undo_idx > 0) {
      assert(_undo_idx < _undo_states.size());
      --_undo_idx;
      _update_state(_undo_states[_undo_idx]);
   }
   return *this;
}

UITextbox& UITextbox::redo() {
   if (_undo_idx + 1 < _undo_states.size()) {
      ++_undo_idx;
      _update_state(_undo_states[_undo_idx]);
   }
   return *this;
}

// --------------------------------------------------
// MDI clients.
// --------------------------------------------------
int UIMDIChild::hittest(UIPoint pt) {
   auto [titleSize, borderSize, titleRect, contentRect] = ui_mdi_child_calculate_layout(_bounds, get_scale());
   int cornerSize                                       = scale(ui_size::mdi_child_corner);
   if (!_bounds.contains(pt.x, pt.y) || contentRect.contains(pt.x, pt.y))
      return -1;
   if (pt.x < _bounds.l + cornerSize && pt.y < _bounds.t + cornerSize)
      return 0b1010;
   if (pt.x > _bounds.r - cornerSize && pt.y < _bounds.t + cornerSize)
      return 0b0110;
   if (pt.x < _bounds.l + cornerSize && pt.y > _bounds.b - cornerSize)
      return 0b1001;
   if (pt.x > _bounds.r - cornerSize && pt.y > _bounds.b - cornerSize)
      return 0b0101;
   if (pt.x < _bounds.l + borderSize)
      return 0b1000;
   if (pt.x > _bounds.r - borderSize)
      return 0b0100;
   if (pt.y < _bounds.t + borderSize)
      return 0b0010;
   if (pt.y > _bounds.b - borderSize)
      return 0b0001;
   if (titleRect.contains(pt.x, pt.y))
      return 0b1111;
   return -1;
}

void _UIMDIChildCloseButton(void* _child) {
   auto* child = static_cast<UIElement*>(_child);

   if (!child->message(UIMessage::WINDOW_CLOSE, 0, nullptr)) {
      child->destroy();
      child->_parent->refresh();
   }
}

int UIMDIChild::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::PAINT) {
      static_cast<UIPainter*>(dp)->draw_control(_bounds, UIControl::mdi_child, _title, 0, get_scale());
   } else if (msg == UIMessage::GET_WIDTH) {
      UIElement* child = _children.empty() ? nullptr : _children.back();
      int        width = 2 * ui_size::mdi_child_border;
      width +=
         (child ? child->message(msg, di ? (di - ui_size::mdi_child_title + ui_size::mdi_child_border) : 0, dp) : 0);
      if (width < ui_size::mdi_child_minimum_width)
         width = ui_size::mdi_child_minimum_width;
      return width;
   } else if (msg == UIMessage::GET_HEIGHT) {
      UIElement* child  = _children.empty() ? nullptr : _children.back();
      int        height = ui_size::mdi_child_title + ui_size::mdi_child_border;
      height += (child ? child->message(msg, di ? (di - 2 * ui_size::mdi_child_border) : 0, dp) : 0);
      if (height < ui_size::mdi_child_minimum_height)
         height = ui_size::mdi_child_minimum_height;
      return height;
   } else if (msg == UIMessage::LAYOUT) {
      auto [titleSize, borderSize, titleRect, contentRect] = ui_mdi_child_calculate_layout(_bounds, get_scale());

      int position = titleRect.r;

      for (auto child : _children) {
         int width = child->message(UIMessage::GET_WIDTH, 0, 0);
         child->move(UIRectangle(position - width, position, titleRect.t, titleRect.b), false);
         position -= width;
      }

      UIElement* child = _children.empty() ? nullptr : _children.back();

      if (child) {
         child->move(contentRect, false);
      }
   } else if (msg == UIMessage::GET_CURSOR) {
      int hitTest = hittest(cursor_pos());
      if (hitTest == 0b1000)
         return static_cast<int>(UICursor::resize_left);
      if (hitTest == 0b0010)
         return static_cast<int>(UICursor::resize_up);
      if (hitTest == 0b0110)
         return static_cast<int>(UICursor::resize_up_right);
      if (hitTest == 0b1010)
         return static_cast<int>(UICursor::resize_up_left);
      if (hitTest == 0b0100)
         return static_cast<int>(UICursor::resize_right);
      if (hitTest == 0b0001)
         return static_cast<int>(UICursor::resize_down);
      if (hitTest == 0b1001)
         return static_cast<int>(UICursor::resize_down_left);
      if (hitTest == 0b0101)
         return static_cast<int>(UICursor::resize_down_right);
      return static_cast<int>(UICursor::arrow);
   } else if (msg == UIMessage::LEFT_DOWN) {
      _drag_hit_test = hittest(cursor_pos());
      _drag_offset   = _bounds + UIRectangle(-cursor_pos().x, -cursor_pos().y);
   } else if (msg == UIMessage::LEFT_UP) {
      if (_mdi_bounds.l < 0)
         _mdi_bounds.r -= _mdi_bounds.l, _mdi_bounds.l = 0;
      if (_mdi_bounds.t < 0)
         _mdi_bounds.b -= _mdi_bounds.t, _mdi_bounds.t = 0;
      _parent->refresh();
   } else if (msg == UIMessage::MOUSE_DRAG) {
      if (_drag_hit_test > 0) {

#define _UI_MDI_CHILD_MOVE_EDGE(bit, edge, cursor, size, opposite, negate, minimum, offset) \
   if (_drag_hit_test & (bit))                                                                \
      _mdi_bounds.edge = _drag_offset.edge + pos.cursor - _parent->_bounds.offset;          \
   if ((_drag_hit_test & (bit)) && _mdi_bounds.size() < (minimum))                              \
      _mdi_bounds.edge = _mdi_bounds.opposite negate minimum;

         auto pos = cursor_pos();
         _UI_MDI_CHILD_MOVE_EDGE(0b1000, l, x, width,  r, -, ui_size::mdi_child_minimum_width, l);
         _UI_MDI_CHILD_MOVE_EDGE(0b0100, r, x, width,  l, +, ui_size::mdi_child_minimum_width, l);
         _UI_MDI_CHILD_MOVE_EDGE(0b0010, t, y, height, b, -, ui_size::mdi_child_minimum_height, t);
         _UI_MDI_CHILD_MOVE_EDGE(0b0001, b, y, height, t, +, ui_size::mdi_child_minimum_height, t);
         _parent->refresh();
      }
   } else if (msg == UIMessage::DESTROY) {
      auto* client = dynamic_cast<UIMDIClient*>(_parent);
      if (client->_active == this) {
         client->_active = dynamic_cast<UIMDIChild*>(client->_children.size() == 1
                                            ? nullptr
                                            : client->_children[client->_children.size() - 2]); // todo: seems wrong
      }
   } else if (msg == UIMessage::DEALLOCATE) {
   }

   return 0;
}

int UIMDIClient::_class_message_proc(UIMessage msg, int di, void* dp) {
   UI*   ui    = this->ui();
   auto& theme = ui->theme();

   if (msg == UIMessage::PAINT) {
      if (!has_flag(UIMDIClient::_TRANSPARENT)) {
         static_cast<UIPainter*>(dp)->draw_block(_bounds, theme.panel2);
      }
   } else if (msg == UIMessage::LAYOUT) {
      for (auto child : _children) {
         UIMDIChild* mdiChild = dynamic_cast<UIMDIChild*>(child);
         assert(mdiChild->_class_proc == UIMDIChild::_ClassMessageProc);

         if (mdiChild->_mdi_bounds == UIRectangle(0)) {
            int width  = mdiChild->message(UIMessage::GET_WIDTH, 0, 0);
            int height = mdiChild->message(UIMessage::GET_HEIGHT, width, 0);
            if (_cascade + width > _bounds.r || _cascade + height > _bounds.b)
               _cascade = 0;
            mdiChild->_mdi_bounds = UIRectangle(_cascade, _cascade + width, _cascade, _cascade + height);
            _cascade += scale(ui_size::mdi_cascade);
         }

         UIRectangle bounds = mdiChild->_mdi_bounds + UIRectangle(_bounds.l, _bounds.t);
         mdiChild->move(bounds, false);
      }
   } else if (msg == UIMessage::PRESSED_DESCENDENT) {
      auto* child = static_cast<UIMDIChild*>(dp);

      if (child && child != _active) {
         for (uint32_t i = 0; i < _children.size(); i++) {
            if (_children[i] == child) {
               _children.erase(_children.begin() + i);
               _children.push_back(child);
               break;
            }
         }

         _active = child;
         refresh();
      }
   }

   return 0;
}

UIMDIChild::UIMDIChild(UIElement* parent, uint32_t flags, const UIRectangle& initialBounds, std::string_view title)
   : UIElementCast<UIMDIChild>(parent, flags, UIMDIChild::_ClassMessageProc, "MDIChild")
   , _mdi_bounds(initialBounds)
   , _title(title) {
   assert(parent->_class_proc == UIMDIClient::_ClassMessageProc);
   auto* mdiClient = dynamic_cast<UIMDIClient*>(parent);

   mdiClient->_active = this;

   if (flags & UIMDIChild::CLOSE_BUTTON) {
      add_button(UIButton::SMALL | non_client_flag, "X").on_click([this](UIButton&) { _UIMDIChildCloseButton(this); });
   }
}

UIMDIClient::UIMDIClient(UIElement* parent, uint32_t flags)
   : UIElementCast<UIMDIClient>(parent, flags, UIMDIClient::_ClassMessageProc, "MDIClient") {}

// --------------------------------------------------
// Image displays.
// --------------------------------------------------

UIImageDisplay& UIImageDisplay::_update_viewport() {
   UIRectangle bounds = _bounds;
   bounds.r -= bounds.l, bounds.b -= bounds.t;

   float minimumZoomX = 1, minimumZoomY = 1;
   if (static_cast<int>(_bb.width) > bounds.r)
      minimumZoomX = static_cast<float>(bounds.r) / _bb.width;
   if (static_cast<int>(_bb.height) > bounds.b)
      minimumZoomY = static_cast<float>(bounds.b) / _bb.height;
   float minimumZoom = minimumZoomX < minimumZoomY ? minimumZoomX : minimumZoomY;

   if (_zoom < minimumZoom || has_flag(UIImageDisplay::ZOOM_FIT)) {
      _zoom = minimumZoom;
      set_flag(UIImageDisplay::ZOOM_FIT);
   }

   if (_panX < 0)
      _panX = 0;
   if (_panY < 0)
      _panY = 0;
   if (_panX > _bb.width - bounds.r / _zoom)
      _panX = _bb.width - bounds.r / _zoom;
   if (_panY > _bb.height - bounds.b / _zoom)
      _panY = _bb.height - bounds.b / _zoom;

   if (bounds.r && _bb.width * _zoom <= bounds.r)
      _panX = _bb.width / 2 - bounds.r / _zoom / 2;
   if (bounds.b && _bb.height * _zoom <= bounds.b)
      _panY = _bb.height / 2 - bounds.b / _zoom / 2;
   return *this;
}

int UIImageDisplay::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_HEIGHT) {
      return _bb.height;
   } if (msg == UIMessage::GET_WIDTH) {
      return _bb.width;
   } if (msg == UIMessage::DEALLOCATE) {
   } else if (msg == UIMessage::PAINT) {
      auto* painter = static_cast<UIPainter*>(dp);

      int w = _bounds.width(), h = _bounds.height();
      int x = _UILinearMap(0, _panX, _panX + w / _zoom, 0, w) + _bounds.l;
      int y = _UILinearMap(0, _panY, _panY + h / _zoom, 0, h) + _bounds.t;

      UIRectangle image  = UIRectangle(x, x + static_cast<int>(_bb.width * _zoom), y, static_cast<int>(y + _bb.height * _zoom));
      UIRectangle bounds = intersection(painter->_clip, intersection(_bounds, image));
      if (!bounds.valid())
         return 0;

      if (_zoom == 1) {
         uint32_t*       lineStart = painter->_bits + static_cast<size_t>(bounds.t * painter->_width + bounds.l);
         const uint32_t* sourceLineStart =
            &_bb[bounds.l - image.l] + static_cast<size_t>(_bb.width * (bounds.t - image.t));

         for (int i = 0; i < bounds.b - bounds.t; i++, lineStart += painter->_width, sourceLineStart += _bb.width) {
            uint32_t*       destination = lineStart;
            const uint32_t* source      = sourceLineStart;
            int             j           = bounds.r - bounds.l;

            do {
               *destination = *source;
               destination++;
               source++;
            } while (--j);
         }
      } else {
         float zr          = 1.0f / _zoom;
         auto* destination = painter->_bits;

         for (int i = bounds.t; i < bounds.b; i++) {
            int ty = (i - image.t) * zr;

            for (int j = bounds.l; j < bounds.r; j++) {
               int tx                               = (j - image.l) * zr;
               destination[i * painter->_width + j] = _bb[ty * _bb.width + tx];
            }
         }
      }
   } else if (msg == UIMessage::MOUSE_WHEEL && has_flag(UIImageDisplay::INTERACTIVE)) {
      clear_flag(UIImageDisplay::ZOOM_FIT);
      int   divisions   = -di / 72;
      float factor      = 1;
      float perDivision = is_ctrl_on() ? 2.0f : is_alt_on() ? 1.01f : 1.2f;
      while (divisions > 0)
         factor *= perDivision, divisions--;
      while (divisions < 0)
         factor /= perDivision, divisions++;
      if (_zoom * factor > 64)
         factor = 64 / _zoom;
      int mx = cursor_pos().x - _bounds.l;
      int my = cursor_pos().y - _bounds.t;
      _zoom *= factor;
      _panX -= mx / _zoom * (1 - factor);
      _panY -= my / _zoom * (1 - factor);
      _update_viewport();
      repaint(nullptr);
   } else if (msg == UIMessage::LAYOUT && has_flag(UIImageDisplay::INTERACTIVE)) {
      UIRectangle bounds = _bounds;
      bounds.r -= bounds.l, bounds.b -= bounds.t;
      _panX -= (bounds.r - _previousWidth) / 2 / _zoom;
      _panY -= (bounds.b - _previousHeight) / 2 / _zoom;
      _previousWidth = bounds.r, _previousHeight = bounds.b;
      _update_viewport();
   } else if (msg == UIMessage::GET_CURSOR && has_flag(UIImageDisplay::INTERACTIVE) &&
              (_bounds.width() < _bb.width * _zoom || _bounds.height() < _bb.height * _zoom)) {
      return static_cast<int>(UICursor::hand);
   } else if (msg == UIMessage::MOUSE_DRAG) {
      _panX -= (cursor_pos().x - _previousPanPointX) / _zoom;
      _panY -= (cursor_pos().y - _previousPanPointY) / _zoom;
      _update_viewport();
      _previousPanPointX = cursor_pos().x;
      _previousPanPointY = cursor_pos().y;
      repaint(nullptr);
   } else if (msg == UIMessage::LEFT_DOWN) {
      clear_flag(UIImageDisplay::ZOOM_FIT);
      _previousPanPointX = cursor_pos().x;
      _previousPanPointY = cursor_pos().y;
   }

   return 0;
}

UIImageDisplay& UIImageDisplay::set_content(UIBitmapBits bb) {
   _bb = std::move(bb);
   measurements_changed(3);
   repaint(nullptr);
   return *this;
}

UIImageDisplay::UIImageDisplay(UIElement* parent, uint32_t flags, UIBitmapBits bb)
   : UIElementCast<UIImageDisplay>(parent, flags, UIImageDisplay::_ClassMessageProc, "ImageDisplay") {
   set_content(std::move(bb));
}

// --------------------------------------------------
// Menus (common).
// --------------------------------------------------

bool UI::close_menus() {
   bool anyClosed = false;

   UIWindow* window = _toplevel_windows;

   while (window) {
      if (window->has_flag(UIWindow::MENU)) {
         inspector_notify_destroyed_window(window);

         window->destroy();
         anyClosed = true;
      }

      window = window->next();
   }

   return anyClosed;
}

int UIMenu::_MenuItemMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::CLICKED) {
      el->ui()->close_menus();
   }

   return 0;
}

int UIMenu::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_WIDTH) {
      int width = 0;

      for (auto child : _children) {
         if (!child->has_flag(non_client_flag)) {
            int w = child->message(UIMessage::GET_WIDTH, 0, 0);
            if (w > width)
               width = w;
         }
      }

      return width + 4 + ui_size::scroll_bar;
   } if (msg == UIMessage::GET_HEIGHT) {
      int height = 0;

      for (auto child : _children) {
         if (!child->has_flag(non_client_flag)) {
            height += child->message(UIMessage::GET_HEIGHT, 0, 0);
         }
      }

      return height + 4;
   } if (msg == UIMessage::PAINT) {
      static_cast<UIPainter*>(dp)->draw_control(_bounds, UIControl::menu, {}, 0, get_scale());
   } else if (msg == UIMessage::LAYOUT) {
      int position      = _bounds.t + 2 - _vscroll->position();
      int totalHeight   = 0;
      int scrollBarSize = has_flag(NO_SCROLL) ? 0 : ui_size::scroll_bar;

      for (auto child : _children) {
         if (!child->has_flag(non_client_flag)) {
            int height = child->message(UIMessage::GET_HEIGHT, 0, 0);
            child->move(UIRectangle(_bounds.l + 2, _bounds.r - scrollBarSize - 2, position, position + height), false);
            position += height;
            totalHeight += height;
         }
      }

      UIRectangle scrollBarBounds = _bounds;
      scrollBarBounds.l           = scrollBarBounds.r - scale(scrollBarSize);
      _vscroll->set_maximum(totalHeight);
      _vscroll->set_page(_bounds.height());
      _vscroll->move(scrollBarBounds, true);
   } else if (msg == UIMessage::KEY_TYPED) {
      auto* m = static_cast<UIKeyTyped*>(dp);

      if (m->code == UIKeycode::ESCAPE) {
         ui()->close_menus();
         return 1;
      }
   } else if (msg == UIMessage::MOUSE_WHEEL) {
      return _vscroll->message(msg, di, dp);
   } else if (msg == UIMessage::SCROLLED) {
      refresh();
   }

   return 0;
}

UIMenu& UIMenu::add_item(uint32_t flags, std::string_view label, std::function<void(UIButton&)> invoke) {
   add_button(flags | UIButton::MENU_ITEM, label)
      .on_click(std::move(invoke))
      .set_user_proc(UIMenu::_MenuItemMessageProc);
   return *this;
}

void UIMenu::_prepare(int* width, int* height) {
   *width  = message(UIMessage::GET_WIDTH, 0, nullptr);
   *height = message(UIMessage::GET_HEIGHT, 0, nullptr);

   if (has_flag(PLACE_ABOVE)) {
      _point.y -= *height;
   }
}

UIMenu::UIMenu(UI* ui, UIElement* parent, uint32_t flags)
   : UIElementCast<UIMenu>(&ui->create_window(parent->_window, UIWindow::MENU, nullptr, 0, 0), flags,
                           UIMenu::_ClassMessageProc, "Menu")
   , _vscroll(&add_scrollbar(non_client_flag))
   , _parent_window(parent->_window) {
   if (parent->_parent) {
      UIRectangle screenBounds = parent->screen_bounds();
      _point.x                 = screenBounds.l;
      _point.y                 = (flags & PLACE_ABOVE) ? (screenBounds.t + 1) : (screenBounds.b - 1);
   } else {
      int x = 0, y = 0;
      parent->_window->get_screen_position(&x, &y);

      _point.x = parent->cursor_pos().x + x;
      _point.y = parent->cursor_pos().y + y;
   }
}

UIMenu& UI::create_menu(UIElement* parent, uint32_t flags) { return *new UIMenu(this, parent, flags); }

// --------------------------------------------------
// Miscellaneous core functions.
// --------------------------------------------------

UIRectangle UIElement::screen_bounds() {
   int x = 0, y = 0;
   _window->get_screen_position(&x, &y);
   return _bounds + UIRectangle(x, y);
}

UI::~UI() {
   _font_map.clear();
#ifdef UI_FREETYPE
   FT_Done_FreeType(_ft);
#endif
}

void UI::update() {
   UIWindow*  window = _toplevel_windows;
   UIWindow** link   = &_toplevel_windows;

   while (window) {
      UIWindow* next = window->next();

      window->message(UIMessage::WINDOW_UPDATE_START, 0, nullptr);
      window->message(UIMessage::WINDOW_UPDATE_BEFORE_DESTROY, 0, nullptr);

      if (window->_destroy()) {
         *link = next;
      } else {
         link = &window->_next;

         window->message(UIMessage::WINDOW_UPDATE_BEFORE_LAYOUT, 0, nullptr);
         window->move(window->_bounds, false);
         window->message(UIMessage::WINDOW_UPDATE_BEFORE_PAINT, 0, nullptr);

         if (window->_update_region.valid()) {
            UIPainter painter(window);
            painter._clip = intersection(painter._clip, window->_update_region);

            window->paint(&painter);
            window->endpaint(&painter);
#ifdef UI_DEBUG
            window->_last_full_fill_count =
               static_cast<float>(painter._fill_count) / (window->_update_region.width() * window->_update_region.height());
#endif
            window->_update_region = UIRectangle(0);
         }

         window->message(UIMessage::WINDOW_UPDATE_END, 0, nullptr);
      }

      window = next;
   }
}

// --------------------------------------------------
// Input event handling.
// --------------------------------------------------

UIWindow& UIWindow::set_pressed(UIElement* el, int button) {
   UIElement* previous = _pressed;
   _pressed            = el;
   _pressed_button     = button;
   if (previous)
      previous->message(UIMessage::UPDATE, UIUpdate::pressed, nullptr);
   if (el)
      el->message(UIMessage::UPDATE, UIUpdate::pressed, nullptr);

   UIElement* ancestor = el;
   UIElement* child    = nullptr;

   while (ancestor) {
      ancestor->message(UIMessage::PRESSED_DESCENDENT, 0, child);
      child    = ancestor;
      ancestor = ancestor->_parent;
   }
   return *this;
}

UIElement* UIElement::find_by_point(int x, int y) {
   for (uint32_t i = _children.size(); i > 0; i--) {
      UIElement* child = _children[i - 1];

      if (!child->has_flag(hide_flag) && child->_clip.contains(x, y)) {
         return child->find_by_point(x, y);
      }
   }

   return this;
}

bool UI::is_menu_open() const {
   UIWindow* win = this->_toplevel_windows;

   while (win) {
      if (win->has_flag(UIWindow::MENU)) {
         return true;
      }
      win = win->next();
   }

   return false;
}

UIElement* UIElement::next_or_previous_sibling(bool previous) {
   if (!_parent) {
      return nullptr;
   }

   auto& children = _parent->_children;
   for (size_t i = 0; i < children.size(); i++) {
      if (children[i] == this) {
         if (previous) {
            return i > 0 ? children[i - 1] : nullptr;
         }             return i < children.size() - 1 ? children[i + 1] : nullptr;
        
      }
   }

   assert(false);
   return nullptr;
}

bool UIWindow::input_event(UIMessage msg, int di, void* dp) {
   bool handled     = true;
   UI*  ui          = this->ui();
   bool multi_click = msg >= UIMessage::LEFT_DBLCLICK && msg <= UIMessage::RIGHT_TRIPLECLICK;

   if (_pressed) {
      if (msg == UIMessage::MOUSE_MOVE) {
         _pressed->message(UIMessage::MOUSE_DRAG, di, dp);
      } else if (msg == UIMessage::LEFT_UP && _pressed_button == 1) {
         if (_hovered == _pressed) {
            _pressed->message(UIMessage::CLICKED, di, dp);
            if (ui->_quit || ui->_dialog_result)
               goto end;
         }

         if (_pressed) {
            _pressed->message(UIMessage::LEFT_UP, di, dp);
            if (ui->_quit || ui->_dialog_result)
               goto end;
            set_pressed(nullptr, 1);
         }
      } else if (msg == UIMessage::MIDDLE_UP && _pressed_button == 2) {
         _pressed->message(UIMessage::MIDDLE_UP, di, dp);
         if (ui->_quit || ui->_dialog_result)
            goto end;
         set_pressed(nullptr, 2);
      } else if (msg == UIMessage::RIGHT_UP && _pressed_button == 3) {
         _pressed->message(UIMessage::RIGHT_UP, di, dp);
         if (ui->_quit || ui->_dialog_result)
            goto end;
         set_pressed(nullptr, 3);
      }
   }

   if (_pressed) {
      bool inside = _pressed->_clip.contains(_cursor);

      if (inside && _hovered == _window) {
         _hovered = _pressed;
         _pressed->message(UIMessage::UPDATE, UIUpdate::hovered, nullptr);
      } else if (!inside && _hovered == _pressed) {
         _hovered = _window;
         _pressed->message(UIMessage::UPDATE, UIUpdate::hovered, nullptr);
      }

      if (ui->_quit || ui->_dialog_result)
         goto end;
   }

   if (!_pressed) {
      UIElement* loc = _window->find_by_point(_cursor.x, _cursor.y);

      if (msg == UIMessage::MOUSE_MOVE) {
         loc->message(UIMessage::MOUSE_MOVE, di, dp);

         int cursor = _hovered->message(UIMessage::GET_CURSOR, di, dp);

         if (cursor != _cursor_style) {
            _cursor_style = cursor;
            set_cursor(cursor);
         }
      } else if (msg >= UIMessage::LEFT_DOWN && msg <= UIMessage::RIGHT_DOWN) {
         if (has_flag(UIWindow::MENU) || !ui->close_menus()) {
            int button = static_cast<int>(msg) - static_cast<int>(UIMessage::LEFT_DOWN) + 1;
            set_pressed(loc, button);
            loc->message(msg, di, dp);
         }
      } else if (multi_click) {
         if (has_flag(UIWindow::MENU) || !ui->close_menus()) {
            int button = static_cast<int>(msg) - static_cast<int>(UIMessage::LEFT_DBLCLICK) + 1;
            set_pressed(loc, button);
            loc->message(msg, di, dp);

            // Clear the `pressed` status as the window proc waits for and removes the ButtonRelease
            // necessary otherwise we can get `pressed` mousemoves before the release which change the selection
            set_pressed(nullptr, button);
         }
      } else if (msg == UIMessage::MOUSE_WHEEL) {
         UIElement* el = loc;

         while (el) {
            if (el->message(UIMessage::MOUSE_WHEEL, di, dp)) {
               break;
            }

            el = el->_parent;
         }
      } else if (msg == UIMessage::KEY_TYPED || msg == UIMessage::KEY_RELEASED) {
         handled = false;

         if (_focused) {
            UIElement* el = _focused;

            while (el) {
               if (el->message(msg, di, dp)) {
                  handled = true;
                  break;
               }

               el = el->_parent;
            }
         } else {
            if (message(msg, di, dp)) {
               handled = true;
            }
         }

         if (!handled && !ui->is_menu_open() && msg == UIMessage::KEY_TYPED) {
            auto* m = static_cast<UIKeyTyped*>(dp);

            if (m->code == UIKeycode::TAB && !_ctrl && !_alt) {
               UIElement* start = _focused ? _focused : _window;
               UIElement* el    = start;

               do {
                  if (!el->_children.empty() && !(el->has_flag(hide_flag) || el->has_flag(disabled_flag))) {
                     el = _shift ? el->_children.back() : el->_children[0];
                     continue;
                  }

                  while (el) {
                     UIElement* sibling = el->next_or_previous_sibling(_shift);
                     if (sibling) {
                        el = sibling;
                        break;
                     }
                     el = el->_parent;
                  }

                  if (!el) {
                     el = _window;
                  }
               } while (el != start &&
                        (!el->has_flag(tab_stop_flag) || el->has_flag(hide_flag) || el->has_flag(disabled_flag)));

               if (!el->has_flag(window_flag)) {
                  el->focus();
               }

               handled = true;
            } else if (!_dialog) {
               for (const auto& shortcut : views::reverse(_shortcuts)) {
                  if (shortcut.code == m->code && shortcut.ctrl == _ctrl && shortcut.shift == _shift &&
                      shortcut.alt == _alt) {
                     if (shortcut.invoke())
                        handled = true;
                     break;
                  }
               }
            } else if (_dialog) {
               _dialog->message(msg, di, dp);
            }
         }
      }

      if (ui->_quit || ui->_dialog_result)
         goto end;

      if (loc != _hovered) {
         UIElement* previous = _hovered;
         _hovered            = loc;
         previous->message(UIMessage::UPDATE, UIUpdate::hovered, nullptr);
         _hovered->message(UIMessage::UPDATE, UIUpdate::hovered, nullptr);
      }
   }

end:
   ui->update();
   return handled;
}

// --------------------------------------------------
// Font handling.
// --------------------------------------------------

// Taken from https://commons.wikimedia.org/wiki/File:Codepage-437.png
// Public domain.

const uint64_t _uiFont[] = {
   0x0000000000000000UL, 0x0000000000000000UL, 0xBD8181A5817E0000UL, 0x000000007E818199UL, 0xC3FFFFDBFF7E0000UL,
   0x000000007EFFFFE7UL, 0x7F7F7F3600000000UL, 0x00000000081C3E7FUL, 0x7F3E1C0800000000UL, 0x0000000000081C3EUL,
   0xE7E73C3C18000000UL, 0x000000003C1818E7UL, 0xFFFF7E3C18000000UL, 0x000000003C18187EUL, 0x3C18000000000000UL,
   0x000000000000183CUL, 0xC3E7FFFFFFFFFFFFUL, 0xFFFFFFFFFFFFE7C3UL, 0x42663C0000000000UL, 0x00000000003C6642UL,
   0xBD99C3FFFFFFFFFFUL, 0xFFFFFFFFFFC399BDUL, 0x331E4C5870780000UL, 0x000000001E333333UL, 0x3C666666663C0000UL,
   0x0000000018187E18UL, 0x0C0C0CFCCCFC0000UL, 0x00000000070F0E0CUL, 0xC6C6C6FEC6FE0000UL, 0x0000000367E7E6C6UL,
   0xE73CDB1818000000UL, 0x000000001818DB3CUL, 0x1F7F1F0F07030100UL, 0x000000000103070FUL, 0x7C7F7C7870604000UL,
   0x0000000040607078UL, 0x1818187E3C180000UL, 0x0000000000183C7EUL, 0x6666666666660000UL, 0x0000000066660066UL,
   0xD8DEDBDBDBFE0000UL, 0x00000000D8D8D8D8UL, 0x6363361C06633E00UL, 0x0000003E63301C36UL, 0x0000000000000000UL,
   0x000000007F7F7F7FUL, 0x1818187E3C180000UL, 0x000000007E183C7EUL, 0x1818187E3C180000UL, 0x0000000018181818UL,
   0x1818181818180000UL, 0x00000000183C7E18UL, 0x7F30180000000000UL, 0x0000000000001830UL, 0x7F060C0000000000UL,
   0x0000000000000C06UL, 0x0303000000000000UL, 0x0000000000007F03UL, 0xFF66240000000000UL, 0x0000000000002466UL,
   0x3E1C1C0800000000UL, 0x00000000007F7F3EUL, 0x3E3E7F7F00000000UL, 0x0000000000081C1CUL, 0x0000000000000000UL,
   0x0000000000000000UL, 0x18183C3C3C180000UL, 0x0000000018180018UL, 0x0000002466666600UL, 0x0000000000000000UL,
   0x36367F3636000000UL, 0x0000000036367F36UL, 0x603E0343633E1818UL, 0x000018183E636160UL, 0x1830634300000000UL,
   0x000000006163060CUL, 0x3B6E1C36361C0000UL, 0x000000006E333333UL, 0x000000060C0C0C00UL, 0x0000000000000000UL,
   0x0C0C0C0C18300000UL, 0x0000000030180C0CUL, 0x30303030180C0000UL, 0x000000000C183030UL, 0xFF3C660000000000UL,
   0x000000000000663CUL, 0x7E18180000000000UL, 0x0000000000001818UL, 0x0000000000000000UL, 0x0000000C18181800UL,
   0x7F00000000000000UL, 0x0000000000000000UL, 0x0000000000000000UL, 0x0000000018180000UL, 0x1830604000000000UL,
   0x000000000103060CUL, 0xDBDBC3C3663C0000UL, 0x000000003C66C3C3UL, 0x1818181E1C180000UL, 0x000000007E181818UL,
   0x0C183060633E0000UL, 0x000000007F630306UL, 0x603C6060633E0000UL, 0x000000003E636060UL, 0x7F33363C38300000UL,
   0x0000000078303030UL, 0x603F0303037F0000UL, 0x000000003E636060UL, 0x633F0303061C0000UL, 0x000000003E636363UL,
   0x18306060637F0000UL, 0x000000000C0C0C0CUL, 0x633E6363633E0000UL, 0x000000003E636363UL, 0x607E6363633E0000UL,
   0x000000001E306060UL, 0x0000181800000000UL, 0x0000000000181800UL, 0x0000181800000000UL, 0x000000000C181800UL,
   0x060C183060000000UL, 0x000000006030180CUL, 0x00007E0000000000UL, 0x000000000000007EUL, 0x6030180C06000000UL,
   0x00000000060C1830UL, 0x18183063633E0000UL, 0x0000000018180018UL, 0x7B7B63633E000000UL, 0x000000003E033B7BUL,
   0x7F6363361C080000UL, 0x0000000063636363UL, 0x663E6666663F0000UL, 0x000000003F666666UL, 0x03030343663C0000UL,
   0x000000003C664303UL, 0x66666666361F0000UL, 0x000000001F366666UL, 0x161E1646667F0000UL, 0x000000007F664606UL,
   0x161E1646667F0000UL, 0x000000000F060606UL, 0x7B030343663C0000UL, 0x000000005C666363UL, 0x637F636363630000UL,
   0x0000000063636363UL, 0x18181818183C0000UL, 0x000000003C181818UL, 0x3030303030780000UL, 0x000000001E333333UL,
   0x1E1E366666670000UL, 0x0000000067666636UL, 0x06060606060F0000UL, 0x000000007F664606UL, 0xC3DBFFFFE7C30000UL,
   0x00000000C3C3C3C3UL, 0x737B7F6F67630000UL, 0x0000000063636363UL, 0x63636363633E0000UL, 0x000000003E636363UL,
   0x063E6666663F0000UL, 0x000000000F060606UL, 0x63636363633E0000UL, 0x000070303E7B6B63UL, 0x363E6666663F0000UL,
   0x0000000067666666UL, 0x301C0663633E0000UL, 0x000000003E636360UL, 0x18181899DBFF0000UL, 0x000000003C181818UL,
   0x6363636363630000UL, 0x000000003E636363UL, 0xC3C3C3C3C3C30000UL, 0x00000000183C66C3UL, 0xDBC3C3C3C3C30000UL,
   0x000000006666FFDBUL, 0x18183C66C3C30000UL, 0x00000000C3C3663CUL, 0x183C66C3C3C30000UL, 0x000000003C181818UL,
   0x0C183061C3FF0000UL, 0x00000000FFC38306UL, 0x0C0C0C0C0C3C0000UL, 0x000000003C0C0C0CUL, 0x1C0E070301000000UL,
   0x0000000040607038UL, 0x30303030303C0000UL, 0x000000003C303030UL, 0x0000000063361C08UL, 0x0000000000000000UL,
   0x0000000000000000UL, 0x0000FF0000000000UL, 0x0000000000180C0CUL, 0x0000000000000000UL, 0x3E301E0000000000UL,
   0x000000006E333333UL, 0x66361E0606070000UL, 0x000000003E666666UL, 0x03633E0000000000UL, 0x000000003E630303UL,
   0x33363C3030380000UL, 0x000000006E333333UL, 0x7F633E0000000000UL, 0x000000003E630303UL, 0x060F0626361C0000UL,
   0x000000000F060606UL, 0x33336E0000000000UL, 0x001E33303E333333UL, 0x666E360606070000UL, 0x0000000067666666UL,
   0x18181C0018180000UL, 0x000000003C181818UL, 0x6060700060600000UL, 0x003C666660606060UL, 0x1E36660606070000UL,
   0x000000006766361EUL, 0x18181818181C0000UL, 0x000000003C181818UL, 0xDBFF670000000000UL, 0x00000000DBDBDBDBUL,
   0x66663B0000000000UL, 0x0000000066666666UL, 0x63633E0000000000UL, 0x000000003E636363UL, 0x66663B0000000000UL,
   0x000F06063E666666UL, 0x33336E0000000000UL, 0x007830303E333333UL, 0x666E3B0000000000UL, 0x000000000F060606UL,
   0x06633E0000000000UL, 0x000000003E63301CUL, 0x0C0C3F0C0C080000UL, 0x00000000386C0C0CUL, 0x3333330000000000UL,
   0x000000006E333333UL, 0xC3C3C30000000000UL, 0x00000000183C66C3UL, 0xC3C3C30000000000UL, 0x0000000066FFDBDBUL,
   0x3C66C30000000000UL, 0x00000000C3663C18UL, 0x6363630000000000UL, 0x001F30607E636363UL, 0x18337F0000000000UL,
   0x000000007F63060CUL, 0x180E181818700000UL, 0x0000000070181818UL, 0x1800181818180000UL, 0x0000000018181818UL,
   0x18701818180E0000UL, 0x000000000E181818UL, 0x000000003B6E0000UL, 0x0000000000000000UL, 0x63361C0800000000UL,
   0x00000000007F6363UL,
};

UIPainter& UIPainter::draw_glyph(int x0, int y0, int c, uint32_t color) {
#ifdef UI_FREETYPE
   UI* ui = this->ui();

   UIFont* font = ui->active_font();
   // std_print("font = {}\n", (void *)font);

   if (font->_is_freetype) {
      if (c < 0 || c >= max_glyphs)
         c = '?';

      if (c == '\r')
         c = ' ';

      if (!font->_glyphs_rendered[c]) {
         FT_Load_Char(font->_font,
                      c == 24   ? 0x2191
                      : c == 25 ? 0x2193
                      : c == 26 ? 0x2192
                      : c == 27 ? 0x2190
                                : c,
                      FT_LOAD_DEFAULT);
         FT_Render_Glyph(font->_font->glyph, ft_render_mode);

         FT_Bitmap_Copy(ui->_ft, &font->_font->glyph->bitmap, &font->_glyphs[c]);
         font->_glyphs_offsets_x[c] = font->_font->glyph->bitmap_left;
         font->_glyphs_offsets_y[c] = font->_font->size->metrics.ascender / 64 - font->_font->glyph->bitmap_top;
         font->_glyphs_rendered[c]  = true;
      }

      FT_Bitmap* bitmap = &font->_glyphs[c];
      x0 += font->_glyphs_offsets_x[c], y0 += font->_glyphs_offsets_y[c];

      for (int y = 0; y < (int)bitmap->rows; y++) {
         if (y0 + y < _clip.t)
            continue;
         if (y0 + y >= _clip.b)
            break;

         int width2 = bitmap->pixel_mode == FT_PIXEL_MODE_LCD ? bitmap->width / 3 : bitmap->width;

         for (int x = 0; x < width2; x++) {
            if (x0 + x < _clip.l)
               continue;
            if (x0 + x >= _clip.r)
               break;

            uint32_t* destination = _bits + (x0 + x) + (y0 + y) * _width;
            uint32_t  original    = *destination, ra, ga, ba;

            if (bitmap->pixel_mode == FT_PIXEL_MODE_LCD) {
               ra = ((uint8_t*)bitmap->buffer)[x * 3 + y * bitmap->pitch + 0];
               ga = ((uint8_t*)bitmap->buffer)[x * 3 + y * bitmap->pitch + 1];
               ba = ((uint8_t*)bitmap->buffer)[x * 3 + y * bitmap->pitch + 2];
               ra += (ga - ra) / 2, ba += (ga - ba) / 2; // TODO Gamma correct blending!
            } else if (bitmap->pixel_mode == FT_PIXEL_MODE_MONO) {
               ra = (((uint8_t*)bitmap->buffer)[(x >> 3) + y * bitmap->pitch] & (0x80 >> (x & 7))) ? 0xFF : 0;
               ga = ra, ba = ra;
            } else if (bitmap->pixel_mode == FT_PIXEL_MODE_GRAY) {
               ra = ((uint8_t*)bitmap->buffer)[x + y * bitmap->pitch];
               ga = ra, ba = ra;
            } else {
               ra = ga = ba = 0;
            }

            uint32_t r2 = (255 - ra) * ((original & 0x000000FF) >> 0);
            uint32_t g2 = (255 - ga) * ((original & 0x0000FF00) >> 8);
            uint32_t b2 = (255 - ba) * ((original & 0x00FF0000) >> 16);
            uint32_t r1 = ra * ((color & 0x000000FF) >> 0);
            uint32_t g1 = ga * ((color & 0x0000FF00) >> 8);
            uint32_t b1 = ba * ((color & 0x00FF0000) >> 16);

            uint32_t result = 0xFF000000 | (0x00FF0000 & ((b1 + b2) << 8)) | (0x0000FF00 & ((g1 + g2) << 0)) |
                              (0x000000FF & ((r1 + r2) >> 8));
            *destination = result;
         }
      }

      return *this;
   }
#endif // UI_FREETYPE

   if (c < 0 || c > 127)
      c = '?';

   assert(_bits != nullptr);
   UIRectangle rectangle = intersection(_clip, UIRectangle(x0, x0 + 8, y0, y0 + 16));

   const uint8_t* data = reinterpret_cast<const uint8_t*>(_uiFont) + static_cast<size_t>(c * 16);

   for (int i = rectangle.t; i < rectangle.b; i++) {
      uint32_t* bits = _bits + static_cast<size_t>(i * _width + rectangle.l);
      uint8_t   byte = data[i - y0];

      for (int j = rectangle.l; j < rectangle.r; j++) {
         if (byte & (1 << (j - x0))) {
            *bits = color;
         }

         bits++;
      }
   }
   return *this;
}

UIFont* UI::create_font(std::string_view cPath, uint32_t size) {
   if (cPath.empty())
      return nullptr;

   UIFontSpec spec{std::string{cPath}, size};

   if (auto it = _font_map.find(spec); it != _font_map.end())
      return it->second.get();

   unique_ptr<UIFont> font = make_unique<UIFont>();

   font->_ui           = this;
   font->_glyph_width  = 9;
   font->_glyph_height = 16;

#ifdef UI_FREETYPE
   font->_glyphs           = make_unique<FT_Bitmap[]>(max_glyphs);
   font->_glyphs_rendered  = make_unique<bool[]>(max_glyphs);
   font->_glyphs_offsets_x = make_unique<int[]>(max_glyphs);
   font->_glyphs_offsets_y = make_unique<int[]>(max_glyphs);

   int ret = FT_New_Face(_ft, cPath.data(), 0, &font->_font);
   if (ret == 0) {
      FT_Select_Charmap(font->_font, FT_ENCODING_UNICODE);
      if (FT_HAS_FIXED_SIZES(font->_font) && font->_font->num_fixed_sizes) {
         // Look for the smallest strike that's at least `size`.
         int j = 0;

         for (int i = 0; i < font->_font->num_fixed_sizes; i++) {
            if ((uint32_t)font->_font->available_sizes[i].height >= size &&
                font->_font->available_sizes[i].y_ppem < font->_font->available_sizes[j].y_ppem) {
               j = i;
            }
         }

         FT_Set_Pixel_Sizes(font->_font, font->_font->available_sizes[j].x_ppem / 64,
                            font->_font->available_sizes[j].y_ppem / 64);
      } else {
         FT_Set_Char_Size(font->_font, 0, size * 64, 100, 100);
      }

      FT_Load_Char(font->_font, 'a', FT_LOAD_DEFAULT);
      font->_glyph_width  = font->_font->glyph->advance.x / 64;
      font->_glyph_height = (font->_font->size->metrics.ascender - font->_font->size->metrics.descender) / 64;
      font->_is_freetype  = true;
   } else {
      std_print("Cannot load font {} : {}\n", cPath, ret);
      return nullptr;
   }
#endif // UI_FREETYPE

   UIFont* f = font.get();
   _font_map.emplace(std::move(spec), std::move(font));
   return f;
}

UIFont* UIFont::activate() {
   UIFont* previous = _ui->active_font();
   _ui->set_active_font(this);
   return previous;
}


UIFont::~UIFont() {
#ifdef UI_FREETYPE
   FT_Done_Face(_font);
   for (size_t i = 0; i < max_glyphs; ++i)
      FT_Bitmap_Done(_ui->_ft, &_glyphs[i]);
#endif
}


// --------------------------------------------------
// Debugging.
// --------------------------------------------------
std::pair<UIElement*, size_t> _UIInspectorFindNthElement(UIElement* el, int* index) {
   if (*index == 0) {
      return {el, 0};
   }

   *index = *index - 1;

   for (auto child : el->_children) {
      if (!child->has_flag(UIElement::destroy_flag) && !child->has_flag(UIElement::hide_flag)) {
         auto [result, depth] = _UIInspectorFindNthElement(child, index);
         if (result)
            return {result, depth + 1};
      }
   }

   return {nullptr, 0};
}

int _UIInspectorTableMessage(UIElement* table, UIMessage msg, int di, void* dp) {
   UI* ui = table->ui();
   if (!ui->_inspector->_target) {
      return 0;
   }

   if (msg == UIMessage::TABLE_GET_ITEM) {
      auto* m          = static_cast<UITableGetItem*>(dp);
      int   index      = m->_row;
      auto [el, depth] = _UIInspectorFindNthElement(ui->_inspector->_target, &index);
      if (!el)
         return 0;

      if (m->_column == 0) {
         return m->format_to("{}{}", std::string_view{"                ", depth * 2}, el->class_name());
      } if (m->_column == 1) {
         const auto& b = el->_bounds;
         return m->format_to("{}:{}, {}:{}", b.l, b.r, b.t, b.b);
      } if (m->_column == 2) {
         return m->format_to("{}{:c}", el->_id, el->is_focused() ? '*' : ' ');
      }
   } else if (msg == UIMessage::MOUSE_MOVE) {
      int        index = ui->_inspector->_table->hittest(table->cursor_pos());
      UIElement* el    = nullptr;
      if (index >= 0)
         el = _UIInspectorFindNthElement(ui->_inspector->_target, &index).first;
      UIWindow* window = ui->_inspector->_target;
      window->set_update_region(window->_bounds);

      UIPainter painter(window);

      auto& bits = window->bits();
      for (uint32_t i = 0; i < window->width() * window->height(); i++) {
         bits[i] = 0xFF00FF;
      }

      window->paint(&painter);
      painter._clip = ui_rect_2s(window->width(), window->height());

      if (el) {
         painter.draw_invert(el->_bounds);
         painter.draw_invert(el->_bounds + ui_rect_1i(4));
      }

      window->endpaint(&painter);
   }

   return 0;
}

UIInspector::UIInspector(UI* ui)
   : _ui(ui) {
   if (!_enabled)
      return;

   _inspector             = &ui->create_window(nullptr, UIWindow::INSPECTOR, "Inspector", 0, 0);
   UISplitPane& splitPane = _inspector->add_splitpane(0, 0.5f);
   _table                 = &splitPane.add_table(0, "Class\tBounds\tID").set_user_proc(_UIInspectorTableMessage);
   _log                   = &splitPane.add_code(0).set_font(ui->default_font());
}

int _UIInspectorCountElements(UIElement* el) {
   int count = 1;

   for (auto child : el->_children) {
      if (!(child->has_flag(UIElement::destroy_flag) || child->has_flag(UIElement::hide_flag))) {
         count += _UIInspectorCountElements(child);
      }
   }

   return count;
}

void UIInspector::refresh() {
   if (!_enabled || !_target || !_inspector || !_table)
      return;

   _table->set_num_items(_UIInspectorCountElements(_target));
   _table->resize_columns();
   _table->refresh();
}

void UIInspector::set_focused_window(UIWindow* window) {
   if (!_enabled || !_inspector || !_table)
      return;

   if (window->has_flag(UIWindow::INSPECTOR)) {
      return;
   }

   if (_target != window) {
      _target = window;
      window->ui()->inspector_refresh();
   }
}

void UIInspector::notify_destroyed_window(UIWindow* window) {
   if (!_enabled || _target != window)
      return;
   _table->set_num_items(0);
   _table->refresh();
   _target = nullptr;
}

// --------------------------------------------------
// Automation for tests.
// --------------------------------------------------

#ifdef UI_AUTOMATION_TESTS

int UI::automation_run_tests() { return 1; }

void UI::automation_process_message() {
   int result = 0;
   UI::platform_message_loop_single(&result);
}

void UI::automation_keyboard_type_single(int code, bool ctrl, bool shift, bool alt) {
   UIWindow*  window = _toplevel_windows; // TODO Get the focused window.
   UIKeyTyped m;
   m.code         = (UIKeycode)code;
   window->_ctrl  = ctrl;
   window->_alt   = alt;
   window->_shift = shift;
   window->input_event(UIMessage::KEY_TYPED, 0, &m);
   window->_ctrl  = false;
   window->_alt   = false;
   window->_shift = false;
}

void UI::automation_keyboard_type(const char* string) {
   UIWindow* window = _toplevel_windows; // TODO Get the focused window.

   UIKeyTyped m;
   char       c[2];

   c[1]   = 0;
   m.text = std::string_view{c, 1};

   for (int i = 0; string[i]; i++) {
      c[0]           = string[i];
      window->_ctrl  = false;
      window->_alt   = false;
      window->_shift = (c[0] >= 'A' && c[0] <= 'Z');
      m.code         = (c[0] >= 'A' && c[0] <= 'Z')   ? UI_KEYCODE_LETTER(c[0])
                       : c[0] == '\n'                 ? UIKeycode::ENTER
                       : c[0] == '\t'                 ? UIKeycode::TAB
                       : c[0] == ' '                  ? UIKeycode::SPACE
                       : (c[0] >= '0' && c[0] <= '9') ? UI_KEYCODE_DIGIT(c[0])
                                                      : (UIKeycode)0;
      window->input_event(UIMessage::KEY_TYPED, 0, &m);
   }

   window->_ctrl  = false;
   window->_alt   = false;
   window->_shift = false;
}

bool UI::automation_check_code_line_matches(UICode* code, size_t lineIndex, std::string_view input) {
   if (lineIndex < 1 || lineIndex > code->num_lines())
      return false;
   return input == code->line(lineIndex - 1);
}

bool UI::automation_check_table_item_matches(UITable* table, size_t row, size_t column, std::string_view input) {
   int bytes = 0;
   for (int i = 0; input[i]; i++)
      bytes++;
   if (row >= table->num_items() || column >= table->num_columns())
      return false;
   UITableGetItem m(bytes + 1, row, column);
   int            length = table->message(UIMessage::TABLE_GET_ITEM, 0, &m);
   return m.buff(length) == input;
}

#endif // UI_AUTOMATION_TESTS

// --------------------------------------------------
// Common platform layer functionality.
// --------------------------------------------------

void UI::_initialize_common(const UIConfig& cfg, const std::string& default_font_path) {
   _theme = ui_themes["ice"];

#ifdef UI_FREETYPE
   FT_Init_FreeType(&_ft);
#endif

   _default_font = create_font(default_font_path, cfg.default_font_size);
   _default_font->activate();
}

void UIWindow::_init_toplevel() {
   set_scale(1.0f);
   _window = this;

   UI* ui = this->ui(); // has to be after `_window` is updated
   set_hovered(this);
   UIWindow** head_ptr = ui->toplevel_windows_head();
   set_next(*head_ptr);
   *head_ptr = this;

   ui->inspector_refresh();
}

int UIWindow::_class_message_proc_common(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::LAYOUT && !_children.empty()) {
      _children[0]->move(_bounds, false);
      if (_window->_dialog)
         _window->_dialog->move(_bounds, false);
      repaint(nullptr);
   } else if (msg == UIMessage::GET_CHILD_STABILITY) {
      return 3; // Both width and height of the child el are ignored.
   }

   return 0;
}

int UI::message_loop() {
   update();
#if 0 && defined(UI_AUTOMATION_TESTS)
   return UI::automation_run_tests();
#else
   int result = 0;
   while (!_quit && platform_message_loop_single(&result))
      _dialog_result = nullptr;
   return result;
#endif
}

UIWindow& UIWindow::grab_focus() {
   ui()->set_focus(native_window());
   return *this;
}

UIWindow::UIWindow(UI* ui, UIElement* parent, uint32_t flags, message_proc_t message_proc, const char* cClassName)
   : UIElementCast<UIWindow>(parent, flags, message_proc, cClassName)
   , _ui(ui) {}

UIWindow::~UIWindow() {}

// --------------------------------------------------
// Platform layers.
// --------------------------------------------------

#ifdef UI_LINUX

enum atom_id_t {
   windowClosedID = 0,
   primaryID,
   uriListID,
   plainTextID,
   dndEnterID,
   dndPositionID,
   dndStatusID,
   dndActionCopyID,
   dndDropID,
   dndSelectionID,
   dndFinishedID,
   dndAwareID,
   clipboardID,
   xSelectionDataID,
   textID,
   targetID,
   incrID,
   atom_id_last
};

int UI::_platform_message_proc(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::DEALLOCATE) {
      UIWindow* window     = dynamic_cast<UIWindow*>(el);
      window->_image->data = nullptr;
      XDestroyImage(window->_image);
      XDestroyIC(window->_xic);
      XDestroyWindow(window->_ui->native_display(), window->_xwindow);
      return 0;
   }

   return UIWindow::_ClassMessageProcCommon(el, msg, di, dp);
}

UIWindow& UI::_platform_create_window(UIWindow* owner, uint32_t flags, const char* cTitle, int _width, int _height) {
   UI* ui = this;
   ui->close_menus();

   UIWindow* window = new UIWindow(ui, nullptr, flags | UIElement::window_flag, UI::_platform_message_proc, "Window");
   window->_init_toplevel();
   if (owner)
      window->set_scale(owner->_scale);

   bool is_popup_menu = !!(flags & UIWindow::MENU);

   int width  = is_popup_menu ? 1 : (_width ? _width : 800);
   int height = is_popup_menu ? 1 : (_height ? _height : 600);

   Display* dpy = ui->native_display();

   XSetWindowAttributes attributes = {};
   unsigned long        mask       = 0;

   if (is_popup_menu) {
      mask |= CWOverrideRedirect;
      attributes.override_redirect = is_popup_menu;
   }

   #if 0
      // causes window resizing to flicker on ubuntu/gnome/xwaylamd
      mask |= CWBorderPixel | CWBackPixel;
      attributes.border_pixel      = 0;
      attributes.background_pixel  = 0;
   #endif

   #if 0
      // doesn't seem to make a difference on my system ubuntu/gnome/xwaylamd
      if (is_menu) {
         mask |= CWSaveUnder;
         attributes.save_under = True;      // default is False
      }
   #endif
   // weird `hyprland` issue the border_width should not be 0
   window->_xwindow = XCreateWindow(dpy, DefaultRootWindow(dpy), 0, 0, width, height, 1, CopyFromParent, InputOutput,
                                    CopyFromParent, mask, &attributes);

   if (cTitle)
      XStoreName(dpy, window->_xwindow, cTitle);

   XSelectInput(dpy, window->_xwindow,
                SubstructureNotifyMask | ExposureMask | PointerMotionMask | ButtonPressMask | ButtonReleaseMask |
                   KeyPressMask | KeyReleaseMask | StructureNotifyMask | EnterWindowMask | LeaveWindowMask |
                   ButtonMotionMask | KeymapStateMask | FocusChangeMask | PropertyChangeMask);

   if (flags & UIWindow::MAXIMIZE) {
      Atom atoms[2] = {XInternAtom(dpy, "_NET_WM_STATE_MAXIMIZED_HORZ", 0),
                       XInternAtom(dpy, "_NET_WM_STATE_MAXIMIZED_VERT", 0)};
      XChangeProperty(dpy, window->_xwindow, XInternAtom(dpy, "_NET_WM_STATE", 0), XA_ATOM, 32, PropModeReplace,
                      (unsigned char*)atoms, 2);
   }

   if (~flags & UIWindow::MENU) {
      XMapRaised(dpy, window->_xwindow);
   }

   if (flags & UIWindow::CENTER_IN_OWNER) {
      int x = 0, y = 0;
      owner->get_screen_position(&x, &y);
      XMoveResizeWindow(dpy, window->_xwindow, x + owner->width() / 2 - width / 2, y + owner->height() / 2 - height / 2,
                        width, height);
   }

   XSetWMProtocols(dpy, window->_xwindow, &ui->_atoms[windowClosedID], 1);
   window->_image = XCreateImage(dpy, ui->_visual, 24, ZPixmap, 0, nullptr, 10, 10, 32, 0);

   window->_xic = XCreateIC(ui->_xim, XNInputStyle, XIMPreeditNothing | XIMStatusNothing, XNClientWindow,
                            window->_xwindow, XNFocusWindow, window->_xwindow, nullptr);

   int dndVersion = 4;
   XChangeProperty(dpy, window->_xwindow, ui->_atoms[dndAwareID], XA_ATOM, 32 /* bits */, PropModeReplace,
                   (uint8_t*)&dndVersion, 1);

   return *window;
}

UIWindow* UI::_find_x11_window(Window window) const {
   UIWindow* w = _toplevel_windows;
   while (w) {
      if (w->_xwindow == window) {
         return w;
      }
      w = w->next();
   }
   return nullptr;
}

void UI::write_clipboard_text(std::string_view text, UIWindow* w, sel_target_t t) {
   _sel[static_cast<int>(t)].write_clipboard_text(*this, text, w);
}

std::string UI::read_clipboard_text(UIWindow* w, sel_target_t t) {
   return _sel[static_cast<int>(t)].read_clipboard_text(*this, w);
}

void UI::selection::write_clipboard_text(UI& ui, std::string_view text, UIWindow* w) {
   _paste_text = text;
   XSetSelectionOwner(ui.native_display(), _atom, static_cast<Window>(w->native_window()), 0);
}

std::string UI::selection::read_clipboard_text(UI& ui, UIWindow* w) {
   auto   dpy            = ui.native_display();
   Window clipboardOwner = XGetSelectionOwner(dpy, _atom);

   if (clipboardOwner == None) {
      return {};
   }

   if (ui._find_x11_window(clipboardOwner)) {
      // one of our windows owns the selection
      return _paste_text;
   }

   XEvent event;
   XConvertSelection(dpy, _atom, XA_STRING, ui._atoms[xSelectionDataID], static_cast<Window>(w->native_window()),
                     CurrentTime);
   XSync(dpy, 0);
   XNextEvent(dpy, &event);

   // Hack to get around the fact that PropertyNotify arrives before SelectionNotify.
   // We need PropertyNotify for incremental transfers.
   while (event.type == PropertyNotify) {
      XNextEvent(dpy, &event);
   }

   if (event.type == SelectionNotify && event.xselection.selection == _atom && event.xselection.property) {
      Atom target = 0;
      // This `itemAmount` is actually `bytes_after_return`
      unsigned long size = 0, itemAmount = 0;
      char*         data = nullptr;
      int           format = 0;
      const auto&   sel_event = event.xselection;
      XGetWindowProperty(sel_event.display, sel_event.requestor, sel_event.property, 0L, ~0L, 0, AnyPropertyType,
                         &target, &format, &size, &itemAmount, (unsigned char**)&data);

      // non incremental transfer
      // ------------------------
      if (target != ui._atoms[incrID]) {
         std::string res;
         res.resize_and_overwrite(size, [&](char* p, size_t sz) {
            std::memcpy(p, data, sz);
            return sz;
         });
         res.resize(size); // shouldn't be necessary, but for some reason it is

         XFree(data);
         XDeleteProperty(sel_event.display, sel_event.requestor, sel_event.property);
         return res;
      }

      // incremental transfer
      // --------------------
      XFree(data);
      XDeleteProperty(dpy, sel_event.requestor, sel_event.property);
      XSync(dpy, 0);

      size = 0;
      std::string res;

      while (true) {
         // TODO Timeout.
         XNextEvent(dpy, &event);

         if (event.type == PropertyNotify) {
            // The other case - PropertyDelete would be caused by us and can be ignored
            if (event.xproperty.state == PropertyNewValue) {
               unsigned long chunkSize = 0;

               // Note that this call deletes the property.
               XGetWindowProperty(dpy, event.xproperty.window, event.xproperty.atom, 0L, ~0L, True, AnyPropertyType,
                                  &target, &format, &chunkSize, &itemAmount, (unsigned char**)&data);

               if (chunkSize == 0) {
                  return res;
               } else {
                  res.resize_and_overwrite(size + chunkSize, [&](char* p, size_t sz) {
                     std::memcpy(p + size, data, chunkSize);
                     return sz;
                  });
                  size += chunkSize;
                  res.resize(size); // shouldn't be necessary
               }

               XFree(data);
            }
         }
      }
   } else {
      // TODO What should happen in this case? Is the next event always going to be the selection event?
      return {};
   }
}

void UI::selection::process_selection_request(UI& ui, UIWindow* w, XSelectionRequestEvent& ev) {
   auto dpy = ui.native_display();

   if (XGetSelectionOwner(dpy, _atom) == w->_xwindow) {
      Atom utf8ID = XInternAtom(dpy, "UTF8_STRING", 1);
      if (utf8ID == None)
         utf8ID = XA_STRING;

      Atom type                = ev.target;
      type                     = (type == ui._atoms[textID]) ? XA_STRING : type;
      int changePropertyResult = 0;

      if (ev.target == XA_STRING || ev.target == ui._atoms[textID] || ev.target == utf8ID) {
         changePropertyResult = XChangeProperty(ev.display, ev.requestor, ev.property, type, 8, PropModeReplace,
                                                (const unsigned char*)_paste_text.c_str(), _paste_text.size());
      } else if (ev.target == ui._atoms[targetID]) {
         changePropertyResult = XChangeProperty(ev.display, ev.requestor, ev.property, XA_ATOM, 32, PropModeReplace,
                                                (unsigned char*)&utf8ID, 1);
      }

      if (changePropertyResult == 0 || changePropertyResult == 1) {
         XSelectionEvent sendEvent = {.type       = SelectionNotify,
                                      .serial     = ev.serial,
                                      .send_event = ev.send_event,
                                      .display    = ev.display,
                                      .requestor  = ev.requestor,
                                      .selection  = ev.selection,
                                      .target     = ev.target,
                                      .property   = ev.property,
                                      .time       = ev.time};

         XSendEvent(dpy, ev.requestor, 0, 0, (XEvent*)&sendEvent);
      }
   }
}

unique_ptr<UI> UI::initialise(const UIConfig& cfg) {
   unique_ptr<UI> ui(new UI);

   if (cfg._has_theme)
      ui->_theme = cfg._theme;

   std::string font_path = cfg.font_path;

   #ifdef UI_FREETYPE
   if (font_path.empty()) {
      // Ask fontconfig for a monospaced font. If this fails, the fallback font will be used.
      FILE* f = popen("fc-list | grep -F `fc-match mono | awk '{ print($1) }'` "
                      "| awk 'BEGIN { FS = \":\" } ; { print($1) }'",
                      "r");

      if (f) {
         font_path.resize_and_overwrite(PATH_MAX, [&](char* p, size_t sz) { return fread(p, 1, sz, f); });
         pclose(f);
         char* newline = strchr(&font_path[0], '\n');
         if (newline) {
            *newline = 0;
            font_path.resize(newline - &font_path[0]);
         }
         print(std::cerr, "Using font {}\n", font_path);
      }
   }
   #endif

   ui->_default_font_path = font_path;
   ui->_initialize_common(cfg, font_path);

   XInitThreads();

   Display* dpy = XOpenDisplay(nullptr);
   ui->_display = dpy;
   ui->_visual  = XDefaultVisual(dpy, 0);

   auto screen      = DefaultScreen(dpy);
   ui->_screen_size = {DisplayWidth(dpy, screen), DisplayHeight(dpy, screen)};
   // std_print("{}, {}\n", ui->_screen_size.x, ui->_screen_size.y);

   ui->_atoms[windowClosedID]   = XInternAtom(dpy, "WM_DELETE_WINDOW", 0);
   ui->_atoms[primaryID]        = XInternAtom(dpy, "PRIMARY", 0);
   ui->_atoms[dndEnterID]       = XInternAtom(dpy, "XdndEnter", 0);
   ui->_atoms[dndPositionID]    = XInternAtom(dpy, "XdndPosition", 0);
   ui->_atoms[dndStatusID]      = XInternAtom(dpy, "XdndStatus", 0);
   ui->_atoms[dndActionCopyID]  = XInternAtom(dpy, "XdndActionCopy", 0);
   ui->_atoms[dndDropID]        = XInternAtom(dpy, "XdndDrop", 0);
   ui->_atoms[dndSelectionID]   = XInternAtom(dpy, "XdndSelection", 0);
   ui->_atoms[dndFinishedID]    = XInternAtom(dpy, "XdndFinished", 0);
   ui->_atoms[dndAwareID]       = XInternAtom(dpy, "XdndAware", 0);
   ui->_atoms[uriListID]        = XInternAtom(dpy, "text/uri-list", 0);
   ui->_atoms[plainTextID]      = XInternAtom(dpy, "text/plain", 0);
   ui->_atoms[clipboardID]      = XInternAtom(dpy, "CLIPBOARD", 0);
   ui->_atoms[xSelectionDataID] = XInternAtom(dpy, "XSEL_DATA", 0);
   ui->_atoms[textID]           = XInternAtom(dpy, "TEXT", 0);
   ui->_atoms[targetID]         = XInternAtom(dpy, "TARGETS", 0);
   ui->_atoms[incrID]           = XInternAtom(dpy, "INCR", 0);

   ui->_sel[0]._atom = ui->_atoms[primaryID];
   ui->_sel[1]._atom = ui->_atoms[clipboardID];

   ui->_cursors[(uint32_t)UICursor::arrow]             = XCreateFontCursor(dpy, XC_left_ptr);
   ui->_cursors[(uint32_t)UICursor::text]              = XCreateFontCursor(dpy, XC_xterm);
   ui->_cursors[(uint32_t)UICursor::split_v]           = XCreateFontCursor(dpy, XC_sb_v_double_arrow);
   ui->_cursors[(uint32_t)UICursor::split_h]           = XCreateFontCursor(dpy, XC_sb_h_double_arrow);
   ui->_cursors[(uint32_t)UICursor::flipped_arrow]     = XCreateFontCursor(dpy, XC_right_ptr);
   ui->_cursors[(uint32_t)UICursor::cross_hair]        = XCreateFontCursor(dpy, XC_crosshair);
   ui->_cursors[(uint32_t)UICursor::hand]              = XCreateFontCursor(dpy, XC_hand1);
   ui->_cursors[(uint32_t)UICursor::resize_up]         = XCreateFontCursor(dpy, XC_top_side);
   ui->_cursors[(uint32_t)UICursor::resize_left]       = XCreateFontCursor(dpy, XC_left_side);
   ui->_cursors[(uint32_t)UICursor::resize_up_right]   = XCreateFontCursor(dpy, XC_top_right_corner);
   ui->_cursors[(uint32_t)UICursor::resize_up_left]    = XCreateFontCursor(dpy, XC_top_left_corner);
   ui->_cursors[(uint32_t)UICursor::resize_down]       = XCreateFontCursor(dpy, XC_bottom_side);
   ui->_cursors[(uint32_t)UICursor::resize_right]      = XCreateFontCursor(dpy, XC_right_side);
   ui->_cursors[(uint32_t)UICursor::resize_down_left]  = XCreateFontCursor(dpy, XC_bottom_left_corner);
   ui->_cursors[(uint32_t)UICursor::resize_down_right] = XCreateFontCursor(dpy, XC_bottom_right_corner);

   XSetLocaleModifiers("");

   ui->_xim = XOpenIM(dpy, 0, 0, 0);

   if (!ui->_xim) {
      XSetLocaleModifiers("@im=none");
      ui->_xim = XOpenIM(dpy, 0, 0, 0);
   }

   if constexpr (UIInspector::enabled())
      ui->_inspector.reset(new UIInspector(ui.get()));

   return ui;
}

UIWindow& UIWindow::set_cursor(int cursor) {
   XDefineCursor(_ui->native_display(), _xwindow, _ui->native_cursors()[cursor]);
   return *this;
}

   #if 0
Display* _UIX11GetDisplay() {
   return ui->x11_display();
}

void _UIX11ResetCursor(UIWindow* window) {
   XDefineCursor(window->ui()->x11_display(), window->_xwindow, window->ui()->native_cursors()[(uint32_t)UICursor::arrow]);
}

void UIWindowPack(UIWindow* window, int _width) {
   int width  = _width ? _width : window->_children[0]->message(UIMessage::GET_WIDTH, 0, 0);
   int height = window->_children[0]->message(UIMessage::GET_HEIGHT, width, 0);
   XResizeWindow(window->ui()->x11_display(), window->_xwindow, width, height);
}

   #endif

void UIWindow::endpaint(UIPainter* painter) const {
   (void)painter;
   Display*    dpy = _ui->native_display();
   const auto& ur  = _window->_update_region;
   XPutImage(dpy, _window->_xwindow, DefaultGC(dpy, 0), _window->_image, ur.l, ur.t, ur.l, ur.t,
             UI_RECT_SIZE(_window->_update_region));
}

void UIWindow::get_screen_position(int* _x, int* _y) const {
   Display* dpy = _ui->native_display();
   Window   child = 0;
   XTranslateCoordinates(dpy, _window->_xwindow, DefaultRootWindow(dpy), 0, 0, _x, _y, &child);
}

UIMenu& UIMenu::show() {
   UI*      ui  = this->ui();
   Display* dpy = ui->native_display();
   Window   child = 0;

   // Find the screen that contains the point the menu was created at.
   Screen* menuScreen = nullptr;
   int     screenX = 0, screenY = 0;

   for (int i = 0; i < ScreenCount(dpy); i++) {
      Screen* screen = ScreenOfDisplay(dpy, i);
      int     x = 0, y = 0;
      XTranslateCoordinates(dpy, screen->root, DefaultRootWindow(dpy), 0, 0, &x, &y, &child);

      if (_point.x >= x && _point.x < x + screen->width && _point.y >= y && _point.y < y + screen->height) {
         menuScreen = screen;
         screenX = x, screenY = y;
         break;
      }
   }

   int width = 0, height = 0;
   _prepare(&width, &height);

   {
      // Clamp the menu to the bounds of the window.
      // This step shouldn't be necessary with the screen clamping below, but there are some buggy X11 drivers that
      // report screen sizes incorrectly.
      int       wx = 0, wy = 0;
      UIWindow* parentWindow = this->_parent_window;
      XTranslateCoordinates(dpy, parentWindow->_xwindow, DefaultRootWindow(dpy), 0, 0, &wx, &wy, &child);
      if (_point.x + width > wx + (int)parentWindow->width())
         _point.x = wx + parentWindow->width() - width;
      if (_point.y + height > wy + (int)parentWindow->height())
         _point.y = wy + parentWindow->height() - height;
      if (_point.x < wx)
         _point.x = wx;
      if (_point.y < wy)
         _point.y = wy;
   }

   if (menuScreen) {
      // Clamp to the bounds of the screen.
      if (_point.x + width > screenX + menuScreen->width)
         _point.x = screenX + menuScreen->width - width;
      if (_point.y + height > screenY + menuScreen->height)
         _point.y = screenY + menuScreen->height - height;
      if (_point.x < screenX)
         _point.x = screenX;
      if (_point.y < screenY)
         _point.y = screenY;
      if (_point.x + width > screenX + menuScreen->width)
         width = screenX + menuScreen->width - _point.x;
      if (_point.y + height > screenY + menuScreen->height)
         height = screenY + menuScreen->height - _point.y;
   }

   Atom properties[] = {
      XInternAtom(dpy, "_NET_WM_WINDOW_TYPE", true),
      XInternAtom(dpy, "_NET_WM_WINDOW_TYPE_DROPDOWN_MENU", true),
      XInternAtom(dpy, "_MOTIF_WM_HINTS", true),
   };

   XChangeProperty(dpy, _window->_xwindow, properties[0], XA_ATOM, 32, PropModeReplace, (uint8_t*)properties, 2);
   XSetTransientForHint(dpy, _window->_xwindow, DefaultRootWindow(dpy));

   struct Hints {
      int flags;
      int functions;
      int decorations;
      int inputMode;
      int status;
   };

   struct Hints hints = {0};
   hints.flags        = 2;
   XChangeProperty(dpy, _window->_xwindow, properties[2], properties[2], 32, PropModeReplace, (uint8_t*)&hints, 5);

   XMapWindow(dpy, _window->_xwindow);
   XMoveResizeWindow(dpy, _window->_xwindow, _point.x, _point.y, width, height);
   return *this;
}

// return true if we should exit, normally return false
// ----------------------------------------------------
bool UI::_process_x11_event(Display* dpy, XEvent* event) {
   if (event->type == ClientMessage && (Atom)event->xclient.data.l[0] == _atoms[windowClosedID]) {
      UIWindow* window = _find_x11_window(event->xclient.window);
      if (!window)
         return false;
      bool exit = !window->message(UIMessage::WINDOW_CLOSE, 0, 0);
      if (exit)
         return true;
      update();
      return false;
   } else if (event->type == Expose) {
      UIWindow* window = _find_x11_window(event->xexpose.window);
      if (!window || event->xexpose.count > 0)
         return false;
      XPutImage(_display, window->_xwindow, DefaultGC(_display, 0), window->_image, 0, 0, 0, 0, window->width(),
                window->height());
   } else if (event->type == ConfigureNotify) {
      // remove any all the ConfigureNotify events for this window from the queue, leaving the last one found in event
      while (XCheckTypedWindowEvent(dpy, event->xconfigure.window, ConfigureNotify, event) == True)
         ;

      UIWindow* window = _find_x11_window(event->xconfigure.window);
      if (!window)
         return false;

      if ((int)window->width() != event->xconfigure.width || (int)window->height() != event->xconfigure.height) {
         auto& bits = window->bits();
         window->set_size(event->xconfigure.width, event->xconfigure.height);
         bits.resize(window->width() * window->height());

         window->_image->width          = window->width();
         window->_image->height         = window->height();
         window->_image->bytes_per_line = window->width() * 4;
         window->_image->data           = (char*)bits.data();
         window->_bounds                = ui_rect_2s(window->width(), window->height());
         window->_clip                  = ui_rect_2s(window->width(), window->height());
   #ifdef UI_DEBUG
         for (uint32_t i = 0; i < window->width() * window->height(); i++)
            bits[i] = 0xFF00FF;
   #endif
         window->relayout();
         update();
      }
   } else if (event->type == MotionNotify) {
      // remove any all the MotionNotify events for this window from the queue, leaving the last one found in event
      while (XCheckTypedWindowEvent(dpy, event->xmotion.window, MotionNotify, event) == True)
         ;

      UIWindow* window = _find_x11_window(event->xmotion.window);
      if (!window)
         return false;
      window->set_cursor_pos({event->xmotion.x, event->xmotion.y});
      window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
   } else if (event->type == LeaveNotify) {
      UIWindow* window = _find_x11_window(event->xcrossing.window);
      if (!window)
         return false;

      if (!window->pressed()) {
         window->set_cursor_pos({-1, -1});
      }
      window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
   } else if (event->type == ButtonPress || event->type == ButtonRelease) {
      UIWindow* window = _find_x11_window(event->xbutton.window);
      if (!window)
         return false;
      window->set_cursor_pos({event->xbutton.x, event->xbutton.y});

      // Button1 == 1 ...  Button3 == 3
      if (event->xbutton.button >= 1 && event->xbutton.button <= 3) {
         bool sent_multi_click = false;

         // check for double and triple clicks
         // ----------------------------------
         struct last_click_t {
            UIPoint   pos;
            Time      t   = 0; // milliseconds
            UIWindow* win = nullptr;

            bool closely_follows(const last_click_t& o) const {
               return win == o.win && t < (o.t + 500) && pos.approx_equal(o.pos, 8);
            }

            void print(std::string_view sv) {
               std_print("{} -> ({}, {}), t={}, win={}\n", sv, pos.x, pos.y, t, (void*)win);
            }
         };

         static std::array<last_click_t, 2> last_clicks;
         last_click_t        click{
                   .pos = {event->xbutton.x, event->xbutton.y},
                     .t = event->xbutton.time, .win = window
         };

         if (event->type == ButtonPress) {
            if (click.closely_follows(last_clicks[1])) {
               uint32_t ev = (uint32_t)UIMessage::LEFT_DBLCLICK;
               if (last_clicks[1].closely_follows(last_clicks[0]))
                  ev = (uint32_t)UIMessage::LEFT_TRIPLECLICK;
               window->input_event((UIMessage)((uint32_t)(ev + event->xbutton.button - 1)), 0, 0);
               sent_multi_click = true;
               // wait for ButtonRelease so we don't get MotionNotify in between
               while (!XCheckTypedWindowEvent(dpy, event->xbutton.window, ButtonRelease, event))
                  ;
            }
            last_clicks[0] = last_clicks[1];
            last_clicks[1] = click;
         }

         if (!sent_multi_click) {
            // if we didn't send a DBLCLICK or TRIPLECLICK, send normal message
            uint32_t ev = (uint32_t)UIMessage::LEFT_UP + event->xbutton.button - 1;
            if (event->type == ButtonPress) {
               ev += 3;
            }
            window->input_event((UIMessage)ev, 0, 0);
         }
      } else if (event->xbutton.button == Button4) {
         window->input_event(UIMessage::MOUSE_WHEEL, -72, 0);
      } else if (event->xbutton.button == Button5) {
         window->input_event(UIMessage::MOUSE_WHEEL, 72, 0);
      }

      inspector_set_focused_window(window);
   } else if (event->type == KeyPress) {
      UIWindow* window = _find_x11_window(event->xkey.window);
      if (!window)
         return false;

      if (event->xkey.x == 0x7123 && event->xkey.y == 0x7456) {
         // HACK! See UIWindowPostMessage.
         uintptr_t p =
            ((uintptr_t)(event->xkey.x_root & 0xFFFF) << 0) | ((uintptr_t)(event->xkey.y_root & 0xFFFF) << 16);
   #if INTPTR_MAX == INT64_MAX
         p |= (uintptr_t)(event->xkey.time & 0xFFFFFFFF) << 32;
   #endif
         window->message((UIMessage)event->xkey.state, 0, (void*)p);
         update();
      } else {
         char   text[32];
         KeySym symbol = NoSymbol;
         Status status = 0;
         // std_print("{}, {}\n", symbol, text);
         UIKeyTyped m;
         auto       sz = Xutf8LookupString(window->_xic, &event->xkey, text, sizeof(text) - 1, &symbol, &status);
         m.text        = {text, static_cast<size_t>(sz)};
         m.code        = (UIKeycode)XLookupKeysym(&event->xkey, 0);

         if (symbol == XK_Control_L || symbol == XK_Control_R) {
            window->_ctrl      = true;
            window->_ctrl_code = event->xkey.keycode;
            window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
         } else if (symbol == XK_Shift_L || symbol == XK_Shift_R) {
            window->_shift      = true;
            window->_shift_code = event->xkey.keycode;
            window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
         } else if (symbol == XK_Alt_L || symbol == XK_Alt_R) {
            window->_alt      = true;
            window->_alt_code = event->xkey.keycode;
            window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
         } else if (symbol == XK_KP_Left) {
            m.code = UIKeycode::LEFT;
         } else if (symbol == XK_KP_Right) {
            m.code = UIKeycode::RIGHT;
         } else if (symbol == XK_KP_Up) {
            m.code = UIKeycode::UP;
         } else if (symbol == XK_KP_Down) {
            m.code = UIKeycode::DOWN;
         } else if (symbol == XK_KP_Home) {
            m.code = UIKeycode::HOME;
         } else if (symbol == XK_KP_End) {
            m.code = UIKeycode::END;
         } else if (symbol == XK_KP_Enter) {
            m.code = UIKeycode::ENTER;
         } else if (symbol == XK_KP_Delete) {
            m.code = UIKeycode::DEL;
         } else if (symbol == XK_KP_Page_Up) {
            m.code = UIKeycode::UP;
         } else if (symbol == XK_KP_Page_Down) {
            m.code = UIKeycode::DOWN;
         } else if (symbol == XK_underscore) {
            m.code = UIKeycode::UNDERSCORE;
         }

         window->input_event(UIMessage::KEY_TYPED, 0, &m);
      }
   } else if (event->type == KeyRelease) {
      UIWindow* window = _find_x11_window(event->xkey.window);
      if (!window)
         return false;

      if (event->xkey.keycode == window->_ctrl_code) {
         window->_ctrl = false;
         window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
      } else if (event->xkey.keycode == window->_shift_code) {
         window->_shift = false;
         window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
      } else if (event->xkey.keycode == window->_alt_code) {
         window->_alt = false;
         window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
      } else {
         char       text[32];
         KeySym     symbol = NoSymbol;
         Status     status = 0;
         UIKeyTyped m;
         auto       sz = Xutf8LookupString(window->_xic, &event->xkey, text, sizeof(text) - 1, &symbol, &status);
         m.text        = {text, static_cast<size_t>(sz)};
         m.code        = (UIKeycode)XLookupKeysym(&event->xkey, 0);
         window->input_event(UIMessage::KEY_RELEASED, 0, &m);
      }
   } else if (event->type == FocusIn) {
      UIWindow* window = _find_x11_window(event->xfocus.window);
      if (!window)
         return false;
      window->_ctrl = window->_shift = window->_alt = false;
      window->message(UIMessage::WINDOW_ACTIVATE, 0, 0);
   } else if (event->type == FocusOut || event->type == ResizeRequest) {
      close_menus();
      update();
   } else if (event->type == ClientMessage && event->xclient.message_type == _atoms[dndEnterID]) {
      UIWindow* window = _find_x11_window(event->xclient.window);
      if (!window)
         return false;
      window->_drag_source = (Window)event->xclient.data.l[0];
   } else if (event->type == ClientMessage && event->xclient.message_type == _atoms[dndPositionID]) {
      UIWindow* window = _find_x11_window(event->xclient.window);
      if (!window)
         return false;
      XClientMessageEvent m = {0};
      m.type                = ClientMessage;
      m.display             = event->xclient.display;
      m.window              = (Window)event->xclient.data.l[0];
      m.message_type        = _atoms[dndStatusID];
      m.format              = 32;
      m.data.l[0]           = window->_xwindow;
      m.data.l[1]           = true;
      m.data.l[4]           = _atoms[dndActionCopyID];
      XSendEvent(_display, m.window, False, NoEventMask, (XEvent*)&m);
      XFlush(_display);
   } else if (event->type == ClientMessage && event->xclient.message_type == _atoms[dndDropID]) {
      UIWindow* window = _find_x11_window(event->xclient.window);
      if (!window)
         return false;

      // TODO Dropping text.

      if (!XConvertSelection(_display, _atoms[dndSelectionID], _atoms[uriListID], _atoms[primaryID], window->_xwindow,
                             event->xclient.data.l[2])) {
         XClientMessageEvent m = {0};
         m.type                = ClientMessage;
         m.display             = _display;
         m.window              = window->_drag_source;
         m.message_type        = _atoms[dndFinishedID];
         m.format              = 32;
         m.data.l[0]           = window->_xwindow;
         m.data.l[1]           = 0;
         m.data.l[2]           = _atoms[dndActionCopyID];
         XSendEvent(_display, m.window, False, NoEventMask, (XEvent*)&m);
         XFlush(_display);
      }
   } else if (event->type == SelectionNotify) {
      UIWindow* window = _find_x11_window(event->xselection.requestor);
      if (!window)
         return false;
      if (!window->_drag_source)
         return false;

      Atom          type   = None;
      int           format = 0;
      unsigned long count = 0, bytesLeft = 0;
      uint8_t*      data = nullptr;
      XGetWindowProperty(_display, window->_xwindow, _atoms[primaryID], 0, 65536, False, AnyPropertyType, &type,
                         &format, &count, &bytesLeft, &data);

      if (format == 8 /* bits per character */) {
         if (event->xselection.target == _atoms[uriListID]) {
            char* copy      = (char*)malloc(count);
            int   fileCount = 0;

            for (int i = 0; i < (int)count; i++) {
               copy[i] = data[i];

               if (i && data[i - 1] == '\r' && data[i] == '\n') {
                  fileCount++;
               }
            }

            char** files = (char**)malloc(sizeof(char*) * fileCount);
            fileCount    = 0;

            for (int i = 0; i < (int)count; i++) {
               char* s = copy + i;
               while (!(i && data[i - 1] == '\r' && data[i] == '\n' && i < (int)count))
                  i++;
               copy[i - 1] = 0;

               for (int j = 0; s[j]; j++) {
                  if (s[j] == '%' && s[j + 1] && s[j + 2]) {
                     char n[3];
                     n[0] = s[j + 1], n[1] = s[j + 2], n[2] = 0;
                     s[j] = strtol(n, nullptr, 16);
                     if (!s[j])
                        break;
                     std::memmove(s + j + 1, s + j + 3, strlen(s) - j - 2);
                  }
               }

               if (s[0] == 'f' && s[1] == 'i' && s[2] == 'l' && s[3] == 'e' && s[4] == ':' && s[5] == '/' &&
                   s[6] == '/') {
                  files[fileCount++] = s + 7;
               }
            }

            // works! Can drop files from the file manager and this sends a list of full paths to the
            // main application window (`windowMain` ib gf).
            window->message(UIMessage::WINDOW_DROP_FILES, fileCount, files);

            free(files);
            free(copy);
         } else if (event->xselection.target == _atoms[plainTextID]) {
            // TODO.
         }
      }

      XFree(data);

      XClientMessageEvent m = {0};
      m.type                = ClientMessage;
      m.display             = _display;
      m.window              = window->_drag_source;
      m.message_type        = _atoms[dndFinishedID];
      m.format              = 32;
      m.data.l[0]           = window->_xwindow;
      m.data.l[1]           = true;
      m.data.l[2]           = _atoms[dndActionCopyID];
      XSendEvent(_display, m.window, False, NoEventMask, (XEvent*)&m);
      XFlush(_display);

      window->_drag_source = 0; // Drag complete.
      update();
   } else if (event->type == SelectionRequest) {
      UIWindow* window = _find_x11_window(event->xclient.window);
      if (!window)
         return false;

      if (event->xselectionrequest.selection == _atoms[primaryID]) {
         _sel[0].process_selection_request(*this, window, event->xselectionrequest);
      } else if (event->xselectionrequest.selection == _atoms[clipboardID]) {
         _sel[1].process_selection_request(*this, window, event->xselectionrequest);
      }
   }

   return false;
}

// return true if events processed without problem, false otherwise
// ----------------------------------------------------------------
bool UI::platform_message_loop_single(int* result) {
   XEvent event;

   if (!_animating.empty()) {
      if (XPending(_display)) {
         XNextEvent(_display, &event);
      } else {
         process_animations();
         return true;
      }
   } else {
      XNextEvent(_display, &event);
   }

   return !_process_x11_event(_display, &event);
}

void UIWindow::post_message(UIMessage msg, void* _dp) const {
   // HACK! Xlib doesn't seem to have a nice way to do this,
   // so send a specially crafted key press event instead.
   // TODO Maybe ClientMessage is what this should use?
   Display* dpy = ui()->native_display();

   uintptr_t dp    = (uintptr_t)_dp;
   XKeyEvent event = {0};
   event.display   = dpy;
   event.window    = _xwindow;
   event.root      = DefaultRootWindow(dpy);
   event.subwindow = None;
   #if INTPTR_MAX == INT64_MAX
   event.time = dp >> 32;
   #endif
   event.x           = 0x7123;
   event.y           = 0x7456;
   event.x_root      = (dp >> 0) & 0xFFFF;
   event.y_root      = (dp >> 16) & 0xFFFF;
   event.same_screen = True;
   event.keycode     = 1;
   event.state       = (unsigned int)msg;
   event.type        = KeyPress;
   XSendEvent(dpy, _xwindow, True, KeyPressMask, (XEvent*)&event);
   XFlush(dpy);
}

UIWindow& UIWindow::set_name(std::string_view name) {
   XStoreName(ui()->native_display(), _xwindow, name.data());
   return *this;
}

UIWindow& UIWindow::set_urgent(bool urgent) {
   if (_urgent == urgent)
      return *this;

   _urgent      = urgent;
   Display* dpy = ui()->native_display();

   if (XWMHints* wmh = XGetWMHints(dpy, _xwindow)) {
      wmh->flags = urgent ? (wmh->flags | XUrgencyHint) : (wmh->flags & ~XUrgencyHint);
      XSetWMHints(dpy, _xwindow, wmh);
      XFree(wmh);
   }
   return *this;
}

void UI::set_focus(ui_handle window) const {
   Display* dpy = native_display();
   XSetInputFocus(dpy, static_cast<Window>(window), RevertToParent, CurrentTime);
}

ui_handle UI::get_focus() const {
   Window win = 0;
   int    revert_to_return = 0;
   XGetInputFocus(native_display(), &win, &revert_to_return);
   return static_cast<ui_handle>(win);
}

#endif // UI_LINUX

#ifdef UI_WINDOWS

int UI::_platform_message_proc(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::DEALLOCATE) {
      UIWindow* window = (UIWindow*)el;
      SetWindowLongPtr(window->_hwnd, GWLP_USERDATA, 0);
      DestroyWindow(window->_hwnd);
      return 0;
   }

   return UIWindow::_ClassMessageProcCommon(el, msg, di, dp);
}

LRESULT CALLBACK UIWindow::WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
   UIWindow* window = (UIWindow*)GetWindowLongPtr(hwnd, GWLP_USERDATA);

   if (!window) {
      return DefWindowProc(hwnd, msg, wParam, lParam);
   }

   UI* ui = window->ui();

   if (msg == WM_CLOSE) {
      if (window->message(UIMessage::WINDOW_CLOSE, 0, 0)) {
         ui->update();
         return 0;
      } else {
         PostQuitMessage(0);
      }
   } else if (msg == WM_SIZE) {
      RECT client;
      GetClientRect(hwnd, &client);
      window->set_size(client.right, client.bottom);
      window->bits().resize(window->width() * window->height());
      window->_bounds = ui_rect_2s(window->width(), window->height());
      window->_clip   = ui_rect_2s(window->width(), window->height());
      window->relayout();
      ui->update();
   } else if (msg == WM_MOUSEMOVE) {
      if (!window->_tracking_leave) {
         window->_tracking_leave = true;
         TRACKMOUSEEVENT leave   = {0};
         leave.cbSize            = sizeof(TRACKMOUSEEVENT);
         leave.dwFlags           = TME_LEAVE;
         leave.hwndTrack         = hwnd;
         TrackMouseEvent(&leave);
      }

      POINT cursor;
      GetCursorPos(&cursor);
      ScreenToClient(hwnd, &cursor);
      window->set_cursor_pos({cursor.x, cursor.y});
      window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
   } else if (msg == WM_MOUSELEAVE) {
      window->_tracking_leave = false;

      if (!window->pressed()) {
         window->set_cursor_pos({-1, -1});
      }

      window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
   } else if (msg == WM_LBUTTONDOWN) {
      SetCapture(hwnd);
      window->input_event(UIMessage::LEFT_DOWN, 0, 0);
   } else if (msg == WM_LBUTTONUP) {
      if (window->pressed_button() == 1)
         ReleaseCapture();
      window->input_event(UIMessage::LEFT_UP, 0, 0);
   } else if (msg == WM_MBUTTONDOWN) {
      SetCapture(hwnd);
      window->input_event(UIMessage::MIDDLE_DOWN, 0, 0);
   } else if (msg == WM_MBUTTONUP) {
      if (window->pressed_button() == 2)
         ReleaseCapture();
      window->input_event(UIMessage::MIDDLE_UP, 0, 0);
   } else if (msg == WM_RBUTTONDOWN) {
      SetCapture(hwnd);
      window->input_event(UIMessage::RIGHT_DOWN, 0, 0);
   } else if (msg == WM_RBUTTONUP) {
      if (window->pressed_button() == 3)
         ReleaseCapture();
      window->input_event(UIMessage::RIGHT_UP, 0, 0);
   } else if (msg == WM_MOUSEWHEEL) {
      int delta = (int)wParam >> 16;
      window->input_event(UIMessage::MOUSE_WHEEL, -delta, 0);
   } else if (msg == WM_KEYDOWN) {
      window->_ctrl  = GetKeyState(VK_CONTROL) & 0x8000;
      window->_shift = GetKeyState(VK_SHIFT) & 0x8000;
      window->_alt   = GetKeyState(VK_MENU) & 0x8000;

      UIKeyTyped m{.code = (UIKeycode)wParam};
      window->input_event(UIMessage::KEY_TYPED, 0, &m);
   } else if (msg == WM_CHAR) {
      UIKeyTyped m;
      char       c = (char)wParam;
      m.text       = {&c, 1};
      window->input_event(UIMessage::KEY_TYPED, 0, &m);
   } else if (msg == WM_PAINT) {
      PAINTSTRUCT      paint;
      HDC              dc   = BeginPaint(hwnd, &paint);
      BITMAPINFOHEADER info = {0};
      info.biSize           = sizeof(info);
      info.biWidth = window->width(), info.biHeight = -(int)window->height();
      info.biPlanes = 1, info.biBitCount = 32;
      StretchDIBits(dc, 0, 0, UI_RECT_SIZE(window->_bounds), 0, 0, UI_RECT_SIZE(window->_bounds), window->bits().data(),
                    (BITMAPINFO*)&info, DIB_RGB_COLORS, SRCCOPY);
      EndPaint(hwnd, &paint);
   } else if (msg == WM_SETCURSOR && LOWORD(lParam) == HTCLIENT) {
      ::SetCursor(ui->native_cursors()[window->cursor_style()]);
      return 1;
   } else if (msg == WM_SETFOCUS || msg == WM_KILLFOCUS) {
      ui->close_menus();

      if (msg == WM_SETFOCUS) {
         ui->inspector_set_focused_window(window);
         window->message(UIMessage::WINDOW_ACTIVATE, 0, 0);
      }
   } else if (msg == WM_MOUSEACTIVATE && window->has_flag(UIWindow::MENU)) {
      return MA_NOACTIVATE;
   } else if (msg == WM_DROPFILES) {
      HDROP  drop  = (HDROP)wParam;
      int    count = DragQueryFile(drop, 0xFFFFFFFF, nullptr, 0);
      char** files = (char**)malloc(sizeof(char*) * count);

      for (int i = 0; i < count; i++) {
         int length       = DragQueryFile(drop, i, nullptr, 0);
         files[i]         = (char*)malloc(length + 1);
         files[i][length] = 0;
         DragQueryFile(drop, i, files[i], length + 1);
      }

      window->message(UIMessage::WINDOW_DROP_FILES, count, files);
      for (int i = 0; i < count; i++)
         free(files[i]);
      free(files);
      DragFinish(drop);
      ui->update();
   } else if (msg == WM_APP + 1) {
      window->message((UIMessage)wParam, 0, (void*)lParam);
      ui->update();
   } else {
      if (msg == WM_NCLBUTTONDOWN || msg == WM_NCMBUTTONDOWN || msg == WM_NCRBUTTONDOWN) {
         if (!window->has_flag(UIWindow::MENU)) {
            ui->close_menus();
            ui->update();
         }
      }

      return DefWindowProc(hwnd, msg, wParam, lParam);
   }

   return 0;
}

unique_ptr<UI> UI::initialise(const UIConfig& cfg) {
   std::unique_ptr<UI> ui(new UI);

   ui->_screen_size = {GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN)};

   std::string font_path = cfg.font_path;
   if (font_path.empty())
      font_path = _UI_TO_STRING_2(UI_FONT_PATH);

   ui->_default_font_path = font_path;
   ui->_initialize_common(cfg, font_path);

   auto& cursors                                  = ui->native_cursors();
   cursors[(uint32_t)UICursor::arrow]             = LoadCursor(NULL, IDC_ARROW);
   cursors[(uint32_t)UICursor::text]              = LoadCursor(NULL, IDC_IBEAM);
   cursors[(uint32_t)UICursor::split_v]           = LoadCursor(NULL, IDC_SIZENS);
   cursors[(uint32_t)UICursor::split_h]           = LoadCursor(NULL, IDC_SIZEWE);
   cursors[(uint32_t)UICursor::flipped_arrow]     = LoadCursor(NULL, IDC_ARROW);
   cursors[(uint32_t)UICursor::cross_hair]        = LoadCursor(NULL, IDC_CROSS);
   cursors[(uint32_t)UICursor::hand]              = LoadCursor(NULL, IDC_HAND);
   cursors[(uint32_t)UICursor::resize_up]         = LoadCursor(NULL, IDC_SIZENS);
   cursors[(uint32_t)UICursor::resize_left]       = LoadCursor(NULL, IDC_SIZEWE);
   cursors[(uint32_t)UICursor::resize_up_right]   = LoadCursor(NULL, IDC_SIZENESW);
   cursors[(uint32_t)UICursor::resize_up_left]    = LoadCursor(NULL, IDC_SIZENWSE);
   cursors[(uint32_t)UICursor::resize_down]       = LoadCursor(NULL, IDC_SIZENS);
   cursors[(uint32_t)UICursor::resize_right]      = LoadCursor(NULL, IDC_SIZEWE);
   cursors[(uint32_t)UICursor::resize_down_left]  = LoadCursor(NULL, IDC_SIZENESW);
   cursors[(uint32_t)UICursor::resize_down_right] = LoadCursor(NULL, IDC_SIZENWSE);

   WNDCLASS windowClass      = {0};
   windowClass.lpfnWndProc   = UIWindow::WndProc;
   windowClass.lpszClassName = "normal";
   RegisterClass(&windowClass);
   windowClass.style |= CS_DROPSHADOW;
   windowClass.lpszClassName = "shadow";
   RegisterClass(&windowClass);

   ui->_inspector.reset(new UIInspector(ui.get()));

   return ui;
}

bool UI::platform_message_loop_single(int* result) {
   MSG msg = {0};

   if (!_animating.empty()) {
      if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
         if (msg.message == WM_QUIT) {
            *result = msg.wParam;
            return false;
         }

         TranslateMessage(&msg);
         DispatchMessage(&msg);
      } else {
         process_animations();
      }
   } else {
      if (!GetMessage(&msg, NULL, 0, 0)) {
         *result = msg.wParam;
         return false;
      }

      TranslateMessage(&msg);
      DispatchMessage(&msg);
   }

   return true;
}

UIMenu& UIMenu::show() {
   int width, height;
   _prepare(&width, &height);
   MoveWindow(_window->_hwnd, _point.x, _point.y, width, height, FALSE);
   ShowWindow(_window->_hwnd, SW_SHOWNOACTIVATE);
   return *this;
}

UIWindow& UI::_platform_create_window(UIWindow* owner, uint32_t flags, const char* cTitle, int width, int height) {
   UI* ui = this;
   close_menus();

   UIWindow* window = new UIWindow(ui, NULL, flags | UIElement::window_flag, UI::_platform_message_proc, "Window");
   window->_init_toplevel();
   if (owner)
      window->set_scale(owner->_scale);

   if (flags & UIWindow::MENU) {
      assert(owner);

      window->_hwnd = CreateWindowEx(WS_EX_TOPMOST | WS_EX_NOACTIVATE, "shadow", 0, WS_POPUP, 0, 0, 0, 0, owner->_hwnd,
                                     NULL, NULL, NULL);
   } else {
      window->_hwnd = CreateWindowEx(WS_EX_ACCEPTFILES, "normal", cTitle, WS_OVERLAPPEDWINDOW, CW_USEDEFAULT,
                                     CW_USEDEFAULT, width ? width : CW_USEDEFAULT, height ? height : CW_USEDEFAULT,
                                     owner ? owner->_hwnd : NULL, NULL, NULL, NULL);
   }

   SetWindowLongPtr(window->_hwnd, GWLP_USERDATA, (LONG_PTR)window);

   if (~flags & UIWindow::MENU) {
      ShowWindow(window->_hwnd, SW_SHOW);
      PostMessage(window->_hwnd, WM_SIZE, 0, 0);
   }

   return *window;
}

void UIWindow::endpaint(UIPainter* painter) const {
   HDC              dc   = GetDC(_hwnd);
   BITMAPINFOHEADER info = {0};
   info.biSize           = sizeof(info);
   info.biWidth = _width, info.biHeight = _height;
   info.biPlanes = 1, info.biBitCount = 32;
   StretchDIBits(dc, _update_region.l, _update_region.t, UI_RECT_SIZE(_window->_update_region), _update_region.l,
                 _update_region.b + 1, _update_region.width(), -_update_region.height(), _bits.data(),
                 (BITMAPINFO*)&info, DIB_RGB_COLORS, SRCCOPY);
   ReleaseDC(_hwnd, dc);
}

UIWindow& UIWindow::set_cursor(int cursor) {
   ::SetCursor(ui()->native_cursors()[cursor]);
   return *this;
}

void UIWindow::get_screen_position(int* _x, int* _y) const {
   POINT p;
   p.x = 0;
   p.y = 0;
   ClientToScreen(_window->_hwnd, &p);
   *_x = p.x;
   *_y = p.y;
}

void UIWindow::post_message(UIMessage msg, void* _dp) const {
   PostMessage(_hwnd, WM_APP + 1, (WPARAM)msg, (LPARAM)_dp);
}

UIWindow& UIWindow::set_name(std::string_view name) {
   SetWindowText(_hwnd, name.data());
   return *this;
}

void UI::set_focus(ui_handle window) const { SetFocus(reinterpret_cast<HWND>(window)); }

ui_handle UI::get_focus() const { return reinterpret_cast<ui_handle>(GetFocus()); }

void UI::write_clipboard_text(std::string_view text, UIWindow* w, sel_target_t) {
   HWND hwnd = reinterpret_cast<HWND>(w->native_window());
   if (OpenClipboard(hwnd)) {
      EmptyClipboard();
      HGLOBAL memory = GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT, text.size() + 1);
      char*   copy   = (char*)GlobalLock(memory);
      std::memcpy(copy, text.data(), text.size());
      GlobalUnlock(copy);
      SetClipboardData(CF_TEXT, memory);
      CloseClipboard();
   }
}

std::string UI::read_clipboard_text(UIWindow* w, sel_target_t) {
   HWND        hwnd = reinterpret_cast<HWND>(w->native_window());
   std::string res;

   if (!OpenClipboard(hwnd)) {
      return res;
   }

   HANDLE memory = GetClipboardData(CF_TEXT);

   if (!memory) {
      CloseClipboard();
      return res;
   }

   char* buffer = (char*)GlobalLock(memory);

   if (!buffer) {
      CloseClipboard();
      return res;
   }

   size_t byteCount = GlobalSize(memory);

   if (byteCount < 1) {
      GlobalUnlock(memory);
      CloseClipboard();
      return res;
   }

   res.resize_and_overwrite(byteCount, [&](char* p, size_t sz) {
      std::memcpy(p, buffer, sz);
      return sz;
   });
   res.resize(byteCount); // shouldn't be necessary

   GlobalUnlock(memory);
   CloseClipboard();

   return res;
}

#endif // UI_WINDOWS
