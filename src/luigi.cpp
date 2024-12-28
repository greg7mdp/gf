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

// --------------------------------------------------
// Themes.
// --------------------------------------------------

UITheme uiThemeClassic = {
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
};

UITheme uiThemeDark = {
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
};

// ---------------------------------------------------------------------------------------------
//                              Utilities
// ---------------------------------------------------------------------------------------------
std::string LoadFile(const char* path) {
   FILE* f = fopen(path, "rb");
   if (!f)
      return {};

   fseek(f, 0, SEEK_END);
   size_t bytes = ftell(f);
   fseek(f, 0, SEEK_SET);
   std::string s;
   s.resize_and_overwrite(bytes + 1, [](char*, size_t sz) { return sz; });

   fread(s.data(), 1, bytes, f);
   s[bytes] = 0;    // make sure it is null terminated
   s.resize(bytes); // return a string of the correct size
   fclose(f);

   return s;
}

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

int         Utf8GetCodePoint(const char* cString, size_t bytesLength, size_t* bytesConsumed);
const char* Utf8GetPreviousChar(const char* string, const char* offset);
size_t      Utf8GetCharBytes(const char* cString, size_t bytes);
size_t      Utf8StringLength(const char* cString, size_t bytes);

inline constexpr size_t _UNICODE_MAX_CODEPOINT = 0x10FFFF;
inline constexpr int    max_glyphs             = _UNICODE_MAX_CODEPOINT + 1;

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

   float childAspectRatio   = (float)childWidth / childHeight;
   int   childMaximumWidth  = static_cast<int>(parentHeight * childAspectRatio);
   int   childMaximumHeight = static_cast<int>(parentWidth / childAspectRatio);

   if (childMaximumWidth > parentWidth) {
      return center(parent, ui_rect_2s(parentWidth, childMaximumHeight));
   } else {
      return center(parent, ui_rect_2s(childMaximumWidth, parentHeight));
   }
}

union _UIConvertFloatInteger {
   float    f;
   uint32_t i;
};

float _UIFloorFloat(float x) {
   _UIConvertFloatInteger convert  = {x};
   uint32_t               sign     = convert.i & 0x80000000;
   int                    exponent = (int)((convert.i >> 23) & 0xFF) - 0x7F;

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

float inverse_lerp(float a, float b, float v) {
   return (v - a) / (b - a);
}

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
   } else {
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
}

void UIColorToRGB(float h, float s, float v, uint32_t* rgb) {
   float r, g, b;

   if (!s) {
      r = g = b = v;
   } else {
      int   h0 = ((int)h) % 6;
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

int UI::byte_to_column(std::string_view string, size_t byte, size_t tabSize) {
   size_t ti = 0, i = 0;
   size_t bytes = string.size();

   while (i < byte && i < bytes) {
      ti++;
      _ui_skip_tab(ti, &string[i], bytes - i, tabSize);
      _ui_advance_char(i, &string[i], byte);
   }

   return (int)ti;
}

int UI::column_to_byte(std::string_view string, size_t column, size_t tabSize) {
   size_t byte = 0, ti = 0;
   size_t bytes = string.size();

   while (byte < bytes) {
      ti++;
      _ui_skip_tab(ti, &string[byte], bytes - byte, tabSize);
      if (column < ti)
         break;

      _ui_advance_char(byte, &string[byte], bytes);
   }

   return (int)byte;
}

#ifdef UI_UNICODE

int Utf8GetCodePoint(const char* cString, size_t bytesLength, size_t* bytesConsumed) {
   UI_ASSERT(bytesLength > 0 && "Attempted to get UTF-8 code point from an empty string");

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

      UI_ASSERT(byteIndex <= bytes && "Overran the end of the string while counting the number of UTF-8 code points");
   }

   return length;
}

#endif

// --------------------------------------------------
// Animations.
// --------------------------------------------------

bool UIElement::animate(bool stop) {
   auto& animating = ui()->_animating;
   if (stop) {
      if (auto it = std::ranges::find(animating, this); it != animating.end()) {
         animating.erase(it);
         return true;
      }
      return false;
   } else {
      if (auto it = std::ranges::find(animating, this); it != animating.end())
         return true;

      animating.push_back(this);
      UI_ASSERT(~_flags & destroy_flag);
      return true;
   }
}

uint64_t UIAnimateClock() {
   return (uint64_t)UI_CLOCK() * 1000 / UI_CLOCKS_PER_SECOND;
}

void UI::process_animations() {
   bool do_update = !_animating.empty();

   for (auto el : _animating)
      el->message(UIMessage::ANIMATE, 0, 0);

   if (do_update) {
      update();
   }
}

// --------------------------------------------------
// Rendering.
// --------------------------------------------------
UIPainter::UIPainter(UIWindow* w)
   : _ui(w->ui())
   , _clip(ui_rect_2s(w->width(), w->height()))
   , _bits(w->bits().data())
   , _width(w->width())
   , _height(w->height()) {}

void UIDrawBlock(UIPainter* painter, UIRectangle rectangle, uint32_t color) {
   rectangle = intersection(painter->_clip, rectangle);

   if (!rectangle.valid()) {
      return;
   }

#ifdef UI_SSE2
   __m128i color4 = _mm_set_epi32(color, color, color, color);
#endif

   for (int line = rectangle.t; line < rectangle.b; line++) {
      uint32_t* bits  = painter->_bits + line * painter->_width + rectangle.l;
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
   painter->fillCount += rectangle.width() * rectangle.height();
#endif
}

bool UIDrawLine(UIPainter* painter, int x0, int y0, int x1, int y1, uint32_t color) {
   // Apply the clip.

   UIRectangle c = painter->_clip;
   if (!c.valid())
      return false;
   int       dx = x1 - x0, dy = y1 - y0;
   const int p[4] = {-dx, dx, -dy, dy};
   const int q[4] = {x0 - c.l, c.r - 1 - x0, y0 - c.t, c.b - 1 - y0};
   float     t0 = 0.0f, t1 = 1.0f; // How far along the line the points end up.

   for (int i = 0; i < 4; i++) {
      if (!p[i] && q[i] < 0)
         return false;
      float r = (float)q[i] / p[i];
      if (p[i] < 0 && r > t1)
         return false;
      if (p[i] > 0 && r < t0)
         return false;
      if (p[i] < 0 && r > t0)
         t0 = r;
      if (p[i] > 0 && r < t1)
         t1 = r;
   }

   x1 = x0 + (int)(t1 * dx);
   y1 = y0 + (int)(t1 * dy);
   x0 += (int)(t0 * dx);
   y0 += (int)(t0 * dy);

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

   uint32_t* bits = painter->_bits + y0 * painter->_width + x0;

   if (dy * dy < dx * dx) {
      int m = 2 * dy - dx;

      for (int i = 0; i < dx; i++, bits += dxs) {
         *bits = color;
         if (m > 0)
            bits += painter->_width, m -= 2 * dx;
         m += 2 * dy;
      }
   } else {
      int m = 2 * dx - dy;

      for (int i = 0; i < dy; i++, bits += painter->_width) {
         *bits = color;
         if (m > 0)
            bits += dxs, m -= 2 * dy;
         m += 2 * dx;
      }
   }

   return true;
}

void UIDrawCircle(UIPainter* painter, int cx, int cy, int radius, uint32_t fillColor, uint32_t outlineColor,
                  bool hollow) {
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
         if (py >= painter->_clip.t && py < painter->_clip.b) {
            for (int s = 0; s <= ix || s <= px; s++) {
               bool inOutline = ((s <= ix) != (s <= px)) || ((ix == px) && (s == ix));
               if (hollow && !inOutline)
                  continue;
               bool clip0 = cx + s >= painter->_clip.l && cx + s < painter->_clip.r;
               bool clip1 = cx - s >= painter->_clip.l && cx - s < painter->_clip.r;
               if (clip0)
                  painter->_bits[painter->_width * py + cx + s] = inOutline ? outlineColor : fillColor;
               if (clip1)
                  painter->_bits[painter->_width * py + cx - s] = inOutline ? outlineColor : fillColor;
            }
         }

         px = ix, py++;
      }
   }
}

void UIDrawTriangle(UIPainter* painter, int x0, int y0, int x1, int y1, int x2, int y2, uint32_t color) {
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
      return;

   // Step 2: Clip the triangle.
   if (x0 < painter->_clip.l && x1 < painter->_clip.l && x2 < painter->_clip.l)
      return;
   if (x0 >= painter->_clip.r && x1 >= painter->_clip.r && x2 >= painter->_clip.r)
      return;
   if (y2 < painter->_clip.t || y0 >= painter->_clip.b)
      return;
   bool needsXClip = x0 < painter->_clip.l + 1 || x0 >= painter->_clip.r - 1 || x1 < painter->_clip.l + 1 ||
                     x1 >= painter->_clip.r - 1 || x2 < painter->_clip.l + 1 || x2 >= painter->_clip.r - 1;
   bool needsYClip = y0 < painter->_clip.t + 1 || y2 >= painter->_clip.b - 1;

#define _UI_DRAW_TRIANGLE_APPLY_CLIP(xo, yo)                                      \
   if (needsYClip && (yi + yo < painter->_clip.t || yi + yo >= painter->_clip.b)) \
      continue;                                                                   \
   if (needsXClip && xf + xo < painter->_clip.l)                                  \
      xf = painter->_clip.l - xo;                                                 \
   if (needsXClip && xt + xo > painter->_clip.r)                                  \
      xt = painter->_clip.r - xo;

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
      int xf = xm0 * y * ymr, xt = xm1 * y * ymr, yi = (int)y;
      _UI_DRAW_TRIANGLE_APPLY_CLIP(x0, y0);
      uint32_t* b = &painter->_bits[(yi + y0) * painter->_width + x0];
      for (int x = xf; x < xt; x++)
         b[x] = color;
   }

   // Step 5: Draw the bottom part.
   for (float y = 0; y < ye; y++) {
      int xf = xe0 * (ye - y) * yer, xt = xe1 * (ye - y) * yer, yi = (int)y;
      _UI_DRAW_TRIANGLE_APPLY_CLIP(x2, y1);
      uint32_t* b = &painter->_bits[(yi + y1) * painter->_width + x2];
      for (int x = xf; x < xt; x++)
         b[x] = color;
   }
}

void UIDrawTriangleOutline(UIPainter* painter, int x0, int y0, int x1, int y1, int x2, int y2, uint32_t color) {
   UIDrawLine(painter, x0, y0, x1, y1, color);
   UIDrawLine(painter, x1, y1, x2, y2, color);
   UIDrawLine(painter, x2, y2, x0, y0, color);
}

void UIDrawInvert(UIPainter* painter, UIRectangle rectangle) {
   rectangle = intersection(painter->_clip, rectangle);

   if (!rectangle.valid()) {
      return;
   }

   for (int line = rectangle.t; line < rectangle.b; line++) {
      uint32_t* bits  = painter->_bits + line * painter->_width + rectangle.l;
      int       count = rectangle.width();

      while (count--) {
         uint32_t in = *bits;
         *bits       = in ^ 0xFFFFFF;
         bits++;
      }
   }
}

int UI::string_width(std::string_view string) const {
#ifdef UI_UNICODE
   return Utf8StringLength(string.data(), string.size()) * _active_font->_glyph_width;
#else
   return (int)string.size() * _active_font->_glyph_width;
#endif
}

void UIDrawString(UIPainter* painter, UIRectangle r, std::string_view string, uint32_t color, UIAlign align,
                  UIStringSelection* selection) {
   if (string.empty() || !string[0])
      return;

   UIRectangle oldClip = painter->_clip;
   painter->_clip       = intersection(r, oldClip);

   if (!painter->_clip.valid()) {
      painter->_clip = oldClip;
      return;
   }

   UI* ui               = painter->ui();
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
      UI_ASSERT(bytesConsumed > 0);
      string = string.substr(bytesConsumed);
#else
      char c = string[0];
      string = string.substr(1);
#endif
      uint32_t colorText = color;

      if (i >= selectFrom && i < selectTo) {
         int w = ui->_active_font->_glyph_width;
         if (c == '\t') {
            int ii = i;
            while (++ii & 3)
               w += ui->_active_font->_glyph_width;
         }
         UIDrawBlock(painter, UIRectangle(x, x + w, y, y + height), selection->colorBackground);
         colorText = selection->colorText;
      }

      if (c != '\t') {
         painter->draw_glyph(x, y, c, colorText);
      }

      if (selection && selection->carets[0] == i) {
         UIDrawInvert(painter, UIRectangle(x, x + 1, y, y + height));
      }

      x += ui->_active_font->_glyph_width, i++;

      if (c == '\t') {
         while (i & 3)
            x += ui->_active_font->_glyph_width, i++;
      }

      j += bytesConsumed;
   }

   if (selection && selection->carets[0] == i) {
      UIDrawInvert(painter, UIRectangle(x, x + 1, y, y + height));
   }

   painter->_clip = oldClip;
}

void UIDrawBorder(UIPainter* painter, UIRectangle r, uint32_t borderColor, UIRectangle borderSize) {
   auto border_rects = r.border(borderSize);
   for (const auto& br : border_rects)
      UIDrawBlock(painter, br, borderColor);
}

void UIDrawRectangle(UIPainter* painter, UIRectangle r, uint32_t mainColor, uint32_t borderColor,
                     UIRectangle borderSize) {
   UIDrawBorder(painter, r, borderColor, borderSize);
   UIDrawBlock(painter, r.shrink(borderSize), mainColor);
}

auto ui_mdi_child_calculate_layout(const UIRectangle& bounds, float scale) {
   int         titleSize   = ui_size::mdi_child_title * scale;
   int         borderSize  = ui_size::mdi_child_border * scale;
   UIRectangle titleRect   = add(bounds, UIRectangle(borderSize, -borderSize, 0, 0));
   titleRect.b             = titleRect.t + titleSize;
   UIRectangle contentRect = add(bounds, UIRectangle(borderSize, -borderSize, titleSize, -borderSize));

   return std::tuple{titleSize, borderSize, titleRect, contentRect};
}

void UIDrawControlDefault(UIPainter* painter, UIRectangle bounds, uint32_t mode, std::string_view label,
                          double position, float scale) {
   UI* ui = painter->ui();
   bool     checked       = mode & UIControl::state_checked;
   bool     disabled      = mode & UIControl::state_disabled;
   bool     focused       = mode & UIControl::state_focused;
   bool     hovered       = mode & UIControl::state_hovered;
   bool     indeterminate = mode & UIControl::state_indeterminate;
   bool     pressed       = mode & UIControl::state_pressed;
   bool     selected      = mode & UIControl::state_selected;
   uint32_t which         = mode & UIControl::type_mask;

   uint32_t buttonColor     = disabled               ? ui->_theme.buttonDisabled
                              : (pressed && hovered) ? ui->_theme.buttonPressed
                              : (pressed || hovered) ? ui->_theme.buttonHovered
                              : focused              ? ui->_theme.selected
                                                     : ui->_theme.buttonNormal;
   uint32_t buttonTextColor = disabled                            ? ui->_theme.textDisabled
                              : buttonColor == ui->_theme.selected ? ui->_theme.textSelected
                                                                  : ui->_theme.text;

   if (which == UIControl::checkbox) {
      uint32_t    color = buttonColor, textColor = buttonTextColor;
      int         midY      = (bounds.t + bounds.b) / 2;
      UIRectangle boxBounds = UIRectangle(bounds.l, bounds.l + ui_size::checkbox_box, midY - ui_size::checkbox_box / 2,
                                          midY + ui_size::checkbox_box / 2);
      UIDrawRectangle(painter, boxBounds, color, ui->_theme.border, UIRectangle(1));
      UIDrawString(painter, boxBounds + UIRectangle(1, 0, 0, 0),
                   checked         ? "*"
                   : indeterminate ? "-"
                                   : " ",
                   textColor, UIAlign::center, NULL);
      UIDrawString(painter, bounds + UIRectangle(ui_size::checkbox_box + ui_size::checkbox_gap, 0, 0, 0), label,
                   disabled ? ui->_theme.textDisabled : ui->_theme.text, UIAlign::left, NULL);
   } else if (which == UIControl::menu_item || which == UIControl::drop_down || which == UIControl::push_button) {
      uint32_t color = buttonColor, textColor = buttonTextColor;
      int      borderSize = which == UIControl::menu_item ? 0 : scale;
      UIDrawRectangle(painter, bounds, color, ui->_theme.border, UIRectangle(borderSize));

      if (checked && !focused) {
         UIDrawBlock(painter, bounds + ui_rect_1i((int)(ui_size::button_checked_area * scale)),
                     ui->_theme.buttonPressed);
      }

      UIRectangle innerBounds = bounds + ui_rect_2i((int)(ui_size::menu_item_margin * scale), 0);

      if (which == UIControl::menu_item) {
         int  tab        = 0;
         auto labelBytes = (int)label.size();
         for (; tab < labelBytes && label[tab] != '\t'; tab++)
            ;

         UIDrawString(painter, innerBounds, label.substr(0, tab), textColor, UIAlign::left, NULL);

         if (labelBytes > tab) {
            UIDrawString(painter, innerBounds, {label.data() + tab + 1, static_cast<size_t>(labelBytes - tab - 1)},
                         textColor, UIAlign::right, NULL);
         }
      } else if (which == UIControl::drop_down) {
         UIDrawString(painter, innerBounds, label, textColor, UIAlign::left, NULL);
         UIDrawString(painter, innerBounds, "\x19", textColor, UIAlign::right, NULL);
      } else {
         UIDrawString(painter, bounds, label, textColor, UIAlign::center, NULL);
      }
   } else if (which == UIControl::label) {
      UIDrawString(painter, bounds, label, ui->_theme.text, UIAlign::left, NULL);
   } else if (which == UIControl::splitter) {
      UIRectangle borders = (mode & UIControl::state_vertical) ? UIRectangle(0, 1) : UIRectangle(1, 0);
      UIDrawRectangle(painter, bounds, ui->_theme.buttonNormal, ui->_theme.border, borders);
   } else if (which == UIControl::scroll_track) {
      if (disabled)
         UIDrawBlock(painter, bounds, ui->_theme.panel1);
   } else if (which == UIControl::scroll_down || which == UIControl::scroll_up) {
      bool     isDown = which == UIControl::scroll_down;
      uint32_t color  = pressed ? ui->_theme.buttonPressed : hovered ? ui->_theme.buttonHovered : ui->_theme.panel2;
      UIDrawRectangle(painter, bounds, color, ui->_theme.border, UIRectangle(0));

      if (mode & UIControl::state_vertical) {
         painter->draw_glyph((bounds.l + bounds.r - ui->_active_font->_glyph_width) / 2 + 1,
                             isDown ? (bounds.b - ui->_active_font->_glyph_height - 2 * scale) : (bounds.t + 2 * scale),
                             isDown ? 25 : 24, ui->_theme.text);
      } else {
         painter->draw_glyph(isDown ? (bounds.r - ui->_active_font->_glyph_width - 2 * scale) : (bounds.l + 2 * scale),
                             (bounds.t + bounds.b - ui->_active_font->_glyph_height) / 2, isDown ? 26 : 27, ui->_theme.text);
      }
   } else if (which == UIControl::scroll_thumb) {
      uint32_t color = pressed ? ui->_theme.buttonPressed : hovered ? ui->_theme.buttonHovered : ui->_theme.buttonNormal;
      UIDrawRectangle(painter, bounds, color, ui->_theme.border, UIRectangle(2));
   } else if (which == UIControl::gauge) {
      UIDrawRectangle(painter, bounds, ui->_theme.buttonNormal, ui->_theme.border, UIRectangle(1));
      UIRectangle filled = bounds + ui_rect_1i(1);
      if (mode & UIControl::state_vertical) {
         filled.t = filled.b - filled.height() * position;
      } else {
         filled.r = filled.l + filled.width() * position;
      }
      UIDrawBlock(painter, filled, ui->_theme.selected);
   } else if (which == UIControl::slider) {
      bool vertical     = mode & UIControl::state_vertical;
      int  center       = vertical ? (bounds.l + bounds.r) / 2 : (bounds.t + bounds.b) / 2;
      int  trackSize    = ui_size::slider_track * scale;
      int  thumbSize    = ui_size::slider_thumb * scale;
      int thumbPosition = vertical ? (bounds.height() - thumbSize) * position : (bounds.width() - thumbSize) * position;
      UIRectangle track = vertical
                             ? UIRectangle(center - (trackSize + 1) / 2, center + trackSize / 2, bounds.t, bounds.b)
                             : UIRectangle(bounds.l, bounds.r, center - (trackSize + 1) / 2, center + trackSize / 2);

      UIDrawRectangle(painter, track, disabled ? ui->_theme.buttonDisabled : ui->_theme.buttonNormal, ui->_theme.border,
                      UIRectangle(1));
      uint32_t    color = disabled  ? ui->_theme.buttonDisabled
                          : pressed ? ui->_theme.buttonPressed
                          : hovered ? ui->_theme.buttonHovered
                                    : ui->_theme.buttonNormal;
      UIRectangle thumb = vertical ? UIRectangle(center - (thumbSize + 1) / 2, center + thumbSize / 2,
                                                 bounds.b - thumbPosition - thumbSize, bounds.b - thumbPosition)
                                   : UIRectangle(bounds.l + thumbPosition, bounds.l + thumbPosition + thumbSize,
                                                 center - (thumbSize + 1) / 2, center + thumbSize / 2);
      UIDrawRectangle(painter, thumb, color, ui->_theme.border, UIRectangle(1));
   } else if (which == UIControl::textbox) {
      UIDrawRectangle(painter, bounds,
                      disabled  ? ui->_theme.buttonDisabled
                      : focused ? ui->_theme.textboxFocused
                                : ui->_theme.textboxNormal,
                      ui->_theme.border, UIRectangle(1));
   } else if (which == UIControl::modal_popup) {
      UIRectangle bounds2 = bounds + ui_rect_1i(-1);
      UIDrawBorder(painter, bounds2, ui->_theme.border, UIRectangle(1));
      UIDrawBorder(painter, bounds2 + UIRectangle(1), ui->_theme.border, UIRectangle(1));
   } else if (which == UIControl::menu) {
      UIDrawBlock(painter, bounds, ui->_theme.border);
   } else if (which == UIControl::table_row) {
      if (selected)
         UIDrawBlock(painter, bounds, ui->_theme.selected);
      else if (hovered)
         UIDrawBlock(painter, bounds, ui->_theme.buttonHovered);
   } else if (which == UIControl::table_cell) {
      uint32_t textColor = selected ? ui->_theme.textSelected : ui->_theme.text;
      UIDrawString(painter, bounds, label, textColor, UIAlign::left, NULL);
   } else if (which == UIControl::table_background) {
      UIDrawBlock(painter, bounds, ui->_theme.panel2);
      UIDrawRectangle(painter,
                      UIRectangle(bounds.l, bounds.r, bounds.t, bounds.t + (int)(ui_size::table_header * scale)),
                      ui->_theme.panel1, ui->_theme.border, UIRectangle(0, 0, 0, 1));
   } else if (which == UIControl::table_header) {
      UIDrawString(painter, bounds, label, ui->_theme.text, UIAlign::left, NULL);
      if (selected)
         UIDrawInvert(painter, bounds);
   } else if (which == UIControl::mdi_child) {
      auto [titleSize, borderSize, titleRect, contentRect] = ui_mdi_child_calculate_layout(bounds, scale);
      UIRectangle borders                                  = UIRectangle(borderSize, borderSize, titleSize, borderSize);
      UIDrawBorder(painter, bounds, ui->_theme.buttonNormal, borders);
      UIDrawBorder(painter, bounds, ui->_theme.border, UIRectangle((int)scale));
      UIDrawBorder(painter, contentRect + ui_rect_1i(-1), ui->_theme.border, UIRectangle((int)scale));
      UIDrawString(painter, titleRect, label, ui->_theme.text, UIAlign::left, NULL);
   } else if (which == UIControl::tab) {
      uint32_t    color = selected ? ui->_theme.buttonPressed : ui->_theme.buttonNormal;
      UIRectangle t     = bounds;
      if (selected)
         t.b++, t.t--;
      else
         t.t++;
      UIDrawRectangle(painter, t, color, ui->_theme.border, UIRectangle(1));
      UIDrawString(painter, bounds, label, ui->_theme.text, UIAlign::center, NULL);
   } else if (which == UIControl::tab_band) {
      UIDrawRectangle(painter, bounds, ui->_theme.panel1, ui->_theme.border, UIRectangle(0, 0, 0, 1));
   }
}

// --------------------------------------------------
// Element hierarchy.
// --------------------------------------------------

void UIElement::_destroy_descendents(bool topLevel) {
   for (auto child : _children) {
      if (!topLevel || (~child->_flags & non_client_flag))
         child->destroy();
   }

   if constexpr (UIInspector::enabled())
      ui()->_inspector->refresh();
}

void UIElement::destroy_descendents() {
   _destroy_descendents(true);
}

void UIElement::destroy() {
   if (_flags & destroy_flag) {
      return;
   }

   this->message(UIMessage::DESTROY, 0, 0);
   _flags |= destroy_flag | hide_flag;

   UIElement* ancestor = _parent;

   while (ancestor) {
      if (ancestor->_flags & destroy_descendent_flag)
         break;
      ancestor->_flags |= destroy_descendent_flag;
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
   if (msg != UIMessage::DEALLOCATE && (_flags & destroy_flag)) {
      return 0;
   }

   if (msg >= UIMessage::INPUT_EVENTS_START && msg <= UIMessage::INPUT_EVENTS_END && is_disabled()) {
      return 0;
   }

   if (_user_proc) {
      int result = _user_proc(this, msg, di, dp);

      if (result) {
         return result;
      }
   }

   if (_class_proc) {
      return _class_proc(this, msg, di, dp);
   } else {
      return 0;
   }
}

// --------------------------------------------------
// Modal dialogs.
// --------------------------------------------------

int _UIDialogWrapperMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::LAYOUT) {
      int         width  = el->_children[0]->message(UIMessage::GET_WIDTH, 0, 0);
      int         height = el->_children[0]->message(UIMessage::GET_HEIGHT, width, 0);
      int         cx     = (el->_bounds.l + el->_bounds.r) / 2;
      int         cy     = (el->_bounds.t + el->_bounds.b) / 2;
      UIRectangle bounds = UIRectangle(cx - (width + 1) / 2, cx + width / 2, cy - (height + 1) / 2, cy + height / 2);
      el->_children[0]->move(bounds, false);
      el->repaint(nullptr);
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, el->_children[0]->_bounds, UIControl::modal_popup, {}, 0, el->_window->scale());
   } else if (msg == UIMessage::KEY_TYPED) {
      UI* ui = el->ui();
      UIKeyTyped* typed = (UIKeyTyped*)dp;

      if (el->_window->_ctrl)
         return 0;
      if (el->_window->_shift)
         return 0;

      if (!ui->_dialog_can_exit) {
      } else if (!el->_window->_alt && typed->code == UIKeycode::ESCAPE) {
         ui->_dialog_result = "__C";
         return 1;
      } else if (!el->_window->_alt && typed->code == UIKeycode::ENTER) {
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
      UIElement* target       = NULL;
      bool       duplicate    = false;

      for (auto row : rowContainer->_children) {
         for (auto item : row->_children) {

            if (item->_class_proc == UIButton::_ClassMessageProc) {
               UIButton* button = (UIButton*)item;
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
            target->message(UIMessage::CLICKED, 0, 0);
         }

         return 1;
      }
   }

   return 0;
}

// -------------------------------------------------------------------------------
// Set insertBefore to null to insert at the end.
// Returns the element it was before in its previous parent, or NULL.
// -------------------------------------------------------------------------------
UIElement* UIElement::change_parent(UIElement* newParent, UIElement* insertBefore) {
   [[maybe_unused]] bool found     = false;
   UIElement*            oldBefore = NULL;

   auto num_children = _parent->_children.size();
   for (uint32_t i = 0; i < num_children; i++) {
      if (_parent->_children[i] == this) {
         _parent->_children.erase(_parent->_children.begin() + i);
         oldBefore = i == _parent->_children.size() ? NULL : _parent->_children[i];
         found     = true;
         break;
      }
   }

   UI_ASSERT(found && (~_flags & destroy_flag));

   for (uint32_t i = 0; i <= newParent->_children.size(); i++) {
      if (i == newParent->_children.size() || newParent->_children[i] == insertBefore) {
         newParent->_children.insert(_parent->_children.begin() + i, this);
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
      _flags |= disabled_flag;
   else
      _flags &= ~disabled_flag;

   message(UIMessage::UPDATE, UIUpdate::disabled, 0);
}

UIElement& UIElement::focus() {
   UIElement* previous = _window->focused();
   if (previous != this) {
      _window->set_focused(this);
      if (previous)
         previous->message(UIMessage::UPDATE, UIUpdate::focused, 0);
      this->message(UIMessage::UPDATE, UIUpdate::focused, 0);

      if constexpr (UIInspector::enabled())
         ui()->_inspector->refresh();
   }
   return *this;
}

UIMDIChild& UIElement::add_mdichild(uint32_t flags, UIRectangle initialBounds, std::string_view title) {
   return *new UIMDIChild(this, flags, initialBounds, title);
}

UIMDIClient& UIElement::add_mdiclient(uint32_t flags) {
   return *new UIMDIClient(this, flags);
}

UIPanel& UIElement::add_panel(uint32_t flags) {
   return *new UIPanel(this, flags);
}

UILabel& UIElement::add_label(uint32_t flags, std::string_view label) {
   return *new UILabel(this, flags, label);
}

UIButton& UIElement::add_button(uint32_t flags, std::string_view label) {
   return *new UIButton(this, flags, label);
}

UICheckbox& UIElement::add_checkbox(uint32_t flags, std::string_view label) {
   return *new UICheckbox(this, flags, label);
}

UIScrollBar& UIElement::add_scrollbar(uint32_t flags) {
   return *new UIScrollBar(this, flags);
}

UISlider& UIElement::add_slider(uint32_t flags) {
   return *new UISlider(this, flags);
}

UISpacer& UIElement::add_spacer(uint32_t flags, int width, int height) {
   return *new UISpacer(this, flags, width, height);
}

UISplitPane& UIElement::add_splitpane(uint32_t flags, float weight) {
   return *new UISplitPane(this, flags, weight);
}

UITabPane& UIElement::add_tabpane(uint32_t flags, const char* tabs) {
   return *new UITabPane(this, flags, tabs);
}

UIWrapPanel& UIElement::add_wrappanel(uint32_t flags) {
   return *new UIWrapPanel(this, flags);
}

UIGauge& UIElement::add_gauge(uint32_t flags) {
   return *new UIGauge(this, flags);
}

UIImageDisplay& UIElement::add_imagedisplay(uint32_t flags, uint32_t* bits, size_t width, size_t height,
                                            size_t stride) {
   return *new UIImageDisplay(this, flags, bits, width, height, stride);
}

UISwitcher& UIElement::add_switcher(uint32_t flags) {
   return *new UISwitcher(this, flags);
}

UIMenu& UIElement::add_menu(uint32_t flags) {
   return *new UIMenu(_window->_ui, this, flags);
}

UITextbox& UIElement::add_textbox(uint32_t flags) {
   return *new UITextbox(this, flags);
}

UITable& UIElement::add_table(uint32_t flags, const char* columns) {
   return *new UITable(this, flags, columns);
}

UICode& UIElement::add_code(uint32_t flags) {
   return *new UICode(this, flags);
}

UIWindow& UI::create_window(UIWindow* owner, uint32_t flags, const char* cTitle, int width, int height) {
   return _platform_create_window(owner, flags, cTitle, width, height);
}

int _UIDialogDefaultButtonMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::PAINT && el->_window->focused()->_class_proc != UIButton::_ClassMessageProc) {
      el->_flags |= UIButton::CHECKED;
      el->_class_proc(el, msg, di, dp);
      el->_flags &= ~UIButton::CHECKED;
      return 1;
   }

   return 0;
}

int UITextbox::_DialogTextboxMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UITextbox*       textbox = (UITextbox*)el;
   std::string_view text    = textbox->text();

   if (msg == UIMessage::VALUE_CHANGED) {
      auto   sz     = text.size();
      char** buffer = (char**)el->_cp;
      *buffer       = (char*)realloc(*buffer, sz + 1); // update user pointer to hold the textbox text

      for (size_t i = 0; i < sz; i++)
         (*buffer)[i] = text[i];
      (*buffer)[sz] = 0;
   } else if (msg == UIMessage::UPDATE && di == UIUpdate::focused && el->is_focused()) {
      textbox->_carets[1] = 0;
      textbox->_carets[0] = text.size();
      el->repaint(nullptr);
   }

   return 0;
}

// --------------------------------------------------
// UIWindow
// --------------------------------------------------

const char* UIWindow::show_dialog(uint32_t flags, const char* format, ...) {
   // Create the dialog wrapper and panel.

   UI_ASSERT(!_dialog);
   _dialog        = UIElementCreate(sizeof(UIElement), this, 0, _UIDialogWrapperMessage, "DialogWrapper");
   UIPanel* panel = UIPanelCreate(_dialog, UIPanel::MEDIUM_SPACING | UIPanel::COLOR_1);
   panel->set_border(UIRectangle(ui_size::pane_medium_border * 2));
   _children[0]->_flags |= UIElement::disabled_flag;

   // Create the dialog contents.

   va_list arguments;
   va_start(arguments, format);
   UIPanel*   row           = NULL;
   UIElement* focus         = NULL;
   UIButton*  defaultButton = NULL;
   UIButton*  cancelButton  = NULL;
   uint32_t   buttonCount   = 0;

   for (int i = 0; format[i]; i++) {
      if (i == 0 || format[i - 1] == '\n') {
         row = UIPanelCreate(panel, UIPanel::HORIZONTAL | UIElement::h_fill);
         row->set_gap(ui_size::pane_small_gap);
      }

      if (format[i] == ' ' || format[i] == '\n') {
      } else if (format[i] == '%') {
         i++;

         if (format[i] == 'b' /* button */ || format[i] == 'B' /* default button */ ||
             format[i] == 'C' /* cancel button */) {
            const char* label  = va_arg(arguments, const char*);
            UIButton*   button = UIButtonCreate(row, 0, label);
            if (!focus)
               focus = button;
            if (format[i] == 'B')
               defaultButton = button;
            if (format[i] == 'C')
               cancelButton = button;
            buttonCount++;
            button->on_click([&, label=label](UIButton&) { ui()->_dialog_result = label; });
            if (format[i] == 'B')
               button->_user_proc = _UIDialogDefaultButtonMessage;
         } else if (format[i] == 's' /* label from string */) {
            const char* label = va_arg(arguments, const char*);
            UILabelCreate(row, 0, label);
         } else if (format[i] == 't' /* textbox */) {
            char**     buffer  = va_arg(arguments, char**);
            UITextbox* textbox = UITextboxCreate(row, UIElement::h_fill);
            if (!focus)
               focus = textbox;
            if (*buffer)
               textbox->replace_text(*buffer, false);
            textbox->_cp = buffer; // when the textbox text is updated, `*buffer` will contain a `char*` to the string
            textbox->_user_proc = UITextbox::_DialogTextboxMessageProc;
         } else if (format[i] == 'f' /* horizontal fill */) {
            UISpacerCreate(row, UIElement::h_fill, 0, 0);
         } else if (format[i] == 'l' /* horizontal line */) {
            UISpacerCreate(row, UIElement::border_flag | UIElement::h_fill, 0, 1);
         } else if (format[i] == 'u' /* user */) {
            UIDialogUserCallback callback = va_arg(arguments, UIDialogUserCallback);
            callback(row);
         }
      } else {
         int j = i;
         while (format[j] && format[j] != '%' && format[j] != '\n')
            j++;
         UILabelCreate(row, 0, {format + i, static_cast<size_t>(j - i)});
         i = j - 1;
      }
   }

   va_end(arguments);

   _dialog_old_focus = _focused;
   (focus ? focus : _dialog)->focus();

   // Run the modal message loop.

   int result;
   UI* ui            = this->ui();
   ui->_dialog_result  = nullptr;
   ui->_dialog_can_exit = buttonCount != 0;
   for (int i = 1; i <= 3; i++)
      set_pressed(NULL, i);
   refresh();
   ui->update();
   while (!ui->_dialog_result && ui->_platform_message_loop_single(&result))
      ;
   ui->_quit = !ui->_dialog_result;

   // Check for cancel/default action.

   if (buttonCount == 1 && defaultButton && !cancelButton) {
      cancelButton = defaultButton;
   }

   if (!ui->_dialog_result) {
   } else if (ui->_dialog_result[0] == '_' && ui->_dialog_result[1] == '_' && ui->_dialog_result[2] == 'C' &&
              ui->_dialog_result[3] == 0 && cancelButton) {
      ui->_dialog_result = (const char*)cancelButton->_cp;
   } else if (ui->_dialog_result[0] == '_' && ui->_dialog_result[1] == '_' && ui->_dialog_result[2] == 'D' &&
              ui->_dialog_result[3] == 0 && defaultButton) {
      ui->_dialog_result = (const char*)defaultButton->_cp;
   }

   // Destroy the dialog.

   _children[0]->_flags &= ~UIElement::disabled_flag;
   _dialog->destroy();
   _dialog = NULL;
   refresh();
   if (_dialog_old_focus)
      _dialog_old_focus->focus();
   return ui->_dialog_result ? ui->_dialog_result : "";
}

UIWindow& UIWindow::register_shortcut(UIShortcut shortcut) {
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
   if (_flags & relayout_flag) {
      return;
   }

   _flags |= relayout_flag;
   UIElement* ancestor = _parent;

   while (ancestor) {
      ancestor->_flags |= relayout_descendent_flag;
      ancestor = ancestor->_parent;
   }
}

void UIElement::measurements_changed(int which) {
   if (!_parent) {
      return; // This is the window element.
   }

   UIElement* el = this;

   while (true) {
      if (el->_parent->_flags & destroy_flag)
         return;
      which &= ~el->_parent->message(UIMessage::GET_CHILD_STABILITY, which, el);
      if (!which)
         break;
      el->_flags |= relayout_flag;
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

   if (_window->_update_region.valid()) {
      _window->_update_region = bounding(_window->_update_region, r);
   } else {
      _window->_update_region = r;
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

   if (_flags & relayout_flag) {
      layout = true;
   }

   if (layout) {
      message(UIMessage::LAYOUT, 0, 0);
   } else if (_flags & relayout_descendent_flag) {
      for (auto child : _children)
         child->move(child->_bounds, false);
   }

   _flags &= ~(relayout_descendent_flag | relayout_flag);
}

void UIElement::paint(UIPainter* painter) {
   if (_flags & hide_flag) {
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

   if (_flags & border_flag) {
      UIDrawBorder(painter, _bounds, ui()->_theme.border, UIRectangle((int)_window->scale()));
   }
}

bool UIElement::_destroy() {
   if (_flags & UIElement::destroy_descendent_flag) {
      _flags &= ~UIElement::destroy_descendent_flag;
#if 1
      intptr_t num_children = (intptr_t)_children.size();
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

   if (_flags & UIElement::destroy_flag) {
      message(UIMessage::DEALLOCATE, 0, 0);

      auto win = _window;

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
   } else {
      return false;
   }
}

UIElement::UIElement(UIElement* parent, uint32_t flags, message_proc_t message_proc, const char* cClassName)
   : _flags(flags)
   , _parent(parent)
   , _bounds(0)
   , _clip(0)
   , _class_name(cClassName) {

   _class_proc = message_proc;

   assert((flags & window_flag) || parent); // if `window_flag`, set, `_window` will be set by `_init_toplevel()`
   if (parent) {
      UI_ASSERT(~parent->_flags & destroy_flag);
      _window = parent->_window;
      parent->_children.push_back(this);
      parent->relayout();
      parent->measurements_changed(3);
   }

   static uint32_t s_id = 0;
   _id                  = ++s_id;

   if constexpr (UIInspector::enabled())
      parent->ui()->_inspector->refresh();
}

UIElement::~UIElement() {}

UIElement* UIElementCreate(size_t bytes, UIElement* parent, uint32_t flags, message_proc_t message_proc,
                           const char* cClassName) {
   UIElement* el = new UIElement(parent, flags, message_proc, cClassName);
   return el;
}

// --------------------------------------------------
// Panels.
// --------------------------------------------------

int UIPanel::_calculate_per_fill(int* _count, int hSpace, int vSpace, float scale) {
   bool horizontal = _flags & UIPanel::HORIZONTAL;
   int  available  = horizontal ? hSpace : vSpace;
   int  count = 0, fill = 0, perFill = 0;

   for (auto child : _children) {
      if (child->_flags & (UIElement::hide_flag | UIElement::non_client_flag)) {
         continue;
      }

      count++;

      if (horizontal) {
         if (child->_flags & UIElement::h_fill) {
            fill++;
         } else if (available > 0) {
            available -= child->message(UIMessage::GET_WIDTH, vSpace, 0);
         }
      } else {
         if (child->_flags & UIElement::v_fill) {
            fill++;
         } else if (available > 0) {
            available -= child->message(UIMessage::GET_HEIGHT, hSpace, 0);
         }
      }
   }

   if (count) {
      available -= (count - 1) * (int)(_gap * scale);
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
   bool horizontal = _flags & UIPanel::HORIZONTAL;
   int  perFill    = _calculate_per_fill(NULL, horizontal ? di : 0, horizontal ? 0 : di, _window->scale());
   int  size       = 0;

   for (auto child : _children) {
      if (child->_flags & (UIElement::hide_flag | UIElement::non_client_flag))
         continue;
      int childSize =
         child->message(horizontal ? UIMessage::GET_HEIGHT : UIMessage::GET_WIDTH,
                        (child->_flags & (horizontal ? UIElement::h_fill : UIElement::v_fill)) ? perFill : 0, 0);
      if (childSize > size)
         size = childSize;
   }

   int border = horizontal ? (_border.t + _border.b) : (_border.l + _border.r);
   return size + scale(border);
}

int UIPanel::_layout(UIRectangle bounds, bool measure) {
   bool horizontal = _flags & UIPanel::HORIZONTAL;

   int position = scale(horizontal ? _border.l : _border.t);
   if (_scrollBar && !measure)
      position -= _scrollBar->position();
   int  hSpace        = bounds.width() - scale(_border.total_width());
   int  vSpace        = bounds.height() - scale(_border.total_height());
   int  count         = 0;
   int  perFill       = _calculate_per_fill(&count, hSpace, vSpace, _window->scale());
   int  scaledBorder2 = scale(horizontal ? _border.t : _border.l);
   bool expand        = _flags & UIPanel::EXPAND;

   for (auto child : _children) {
      if (child->_flags & (UIElement::hide_flag | UIElement::non_client_flag)) {
         continue;
      }

      if (horizontal) {
         int height = ((child->_flags & UIElement::v_fill) || expand)
                         ? vSpace
                         : child->message(UIMessage::GET_HEIGHT, (child->_flags & UIElement::h_fill) ? perFill : 0, 0);
         int width  = (child->_flags & UIElement::h_fill) ? perFill : child->message(UIMessage::GET_WIDTH, height, 0);
         UIRectangle relative = UIRectangle(position, position + width, scaledBorder2 + (vSpace - height) / 2,
                                            scaledBorder2 + (vSpace + height) / 2);
         if (!measure)
            child->move(translate(relative, bounds), false);
         position += width + scale(_gap);
      } else {
         int width  = ((child->_flags & UIElement::h_fill) || expand)
                         ? hSpace
                         : child->message(UIMessage::GET_WIDTH, (child->_flags & UIElement::v_fill) ? perFill : 0, 0);
         int height = (child->_flags & UIElement::v_fill) ? perFill : child->message(UIMessage::GET_HEIGHT, width, 0);
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
   bool horizontal = _flags & UIPanel::HORIZONTAL;

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
      } else {
         return _measure(di);
      }
   } else if (msg == UIMessage::GET_HEIGHT) {
      if (horizontal) {
         return _measure(di);
      } else {
         int width = di && _scrollBar ? (di - scale(ui_size::scroll_bar)) : di;
         return _layout(UIRectangle(0, width, 0, 0), true);
      }
   } else if (msg == UIMessage::PAINT) {
      if (_flags & UIPanel::COLOR_1) {
         UIDrawBlock((UIPainter*)dp, _bounds, ui()->_theme.panel1);
      } else if (_flags & UIPanel::COLOR_2) {
         UIDrawBlock((UIPainter*)dp, _bounds, ui()->_theme.panel2);
      }
   } else if (msg == UIMessage::MOUSE_WHEEL && _scrollBar) {
      return _scrollBar->message(msg, di, dp);
   } else if (msg == UIMessage::SCROLLED) {
      refresh();
   } else if (msg == UIMessage::GET_CHILD_STABILITY) {
      UIElement* child = (UIElement*)dp;
      return ((_flags & UIPanel::EXPAND) ? (horizontal ? 2 : 1) : 0) | ((child->_flags & UIElement::h_fill) ? 1 : 0) |
             ((child->_flags & UIElement::v_fill) ? 2 : 0);
   }

   return 0;
}

UIPanel::UIPanel(UIElement* parent, uint32_t flags)
   : UIElementCast<UIPanel>(parent, flags, UIPanel::_ClassMessageProc, "Panel")
   , _scrollBar(nullptr)
   , _border(0)
   , _gap(0) {

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
      _scrollBar = UIScrollBarCreate(this, UIElement::non_client_flag);
   }
}

UIPanel* UIPanelCreate(UIElement* parent, uint32_t flags) {
   return new UIPanel(parent, flags);
}

void _UIWrapPanelLayoutRow(UIWrapPanel* panel, uint32_t rowStart, uint32_t rowEnd, int rowY, int rowHeight) {
   int rowPosition = 0;

   for (uint32_t i = rowStart; i < rowEnd; i++) {
      UIElement* child = panel->_children[i];
      if (child->_flags & UIElement::hide_flag)
         continue;
      int         height   = child->message(UIMessage::GET_HEIGHT, 0, 0);
      int         width    = child->message(UIMessage::GET_WIDTH, 0, 0);
      UIRectangle relative = UIRectangle(rowPosition, rowPosition + width, rowY + rowHeight / 2 - height / 2,
                                         rowY + rowHeight / 2 + height / 2);
      child->move(translate(relative, panel->_bounds), false);
      rowPosition += width;
   }
}

int UIWrapPanel::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UIWrapPanel* panel = (UIWrapPanel*)el;

   if (msg == UIMessage::LAYOUT || msg == UIMessage::GET_HEIGHT) {
      int totalHeight = 0;
      int rowPosition = 0;
      int rowHeight   = 0;
      int rowLimit    = msg == UIMessage::LAYOUT ? el->_bounds.width() : di;

      uint32_t rowStart = 0;

      for (uint32_t i = 0; i < panel->_children.size(); i++) {
         UIElement* child = panel->_children[i];
         if (child->_flags & UIElement::hide_flag)
            continue;

         int height = child->message(UIMessage::GET_HEIGHT, 0, 0);
         int width  = child->message(UIMessage::GET_WIDTH, 0, 0);

         if (rowLimit && rowPosition + width > rowLimit) {
            _UIWrapPanelLayoutRow(panel, rowStart, i, totalHeight, rowHeight);
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
      } else {
         _UIWrapPanelLayoutRow(panel, rowStart, (uint32_t)panel->_children.size(), totalHeight, rowHeight);
      }
   }

   return 0;
}

UIWrapPanel::UIWrapPanel(UIElement* parent, uint32_t flags)
   : UIElementCast<UIWrapPanel>(parent, flags, UIWrapPanel::_ClassMessageProc, "Wrap Panel") {}

UIWrapPanel* UIWrapPanelCreate(UIElement* parent, uint32_t flags) {
   return new UIWrapPanel(parent, flags);
}

int UISwitcher::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UISwitcher* switcher = (UISwitcher*)el;

   if (!switcher->_active) {
   } else if (msg == UIMessage::GET_WIDTH || msg == UIMessage::GET_HEIGHT) {
      return switcher->_active->message(msg, di, dp);
   } else if (msg == UIMessage::LAYOUT) {
      switcher->_active->move(el->_bounds, false);
   }

   return 0;
}

void UISwitcherSwitchTo(UISwitcher* switcher, UIElement* child) {
   for (auto sw_child : switcher->_children)
      sw_child->_flags |= UIElement::hide_flag;

   UI_ASSERT(child->_parent == switcher);
   child->_flags &= ~UIElement::hide_flag;
   switcher->_active = child;
   switcher->measurements_changed(3);
   switcher->refresh();
}

UISwitcher::UISwitcher(UIElement* parent, uint32_t flags)
   : UIElementCast<UISwitcher>(parent, flags, UISwitcher::_ClassMessageProc, "Switcher")
   , _active(nullptr) {}

UISwitcher* UISwitcherCreate(UIElement* parent, uint32_t flags) {
   return new UISwitcher(parent, flags);
}

// --------------------------------------------------
// Checkboxes and buttons.
// --------------------------------------------------

int UIButton::_class_message_proc(UIMessage msg, int di, void* dp) {
   bool isMenuItem = _flags & UIButton::MENU_ITEM;
   bool isDropDown = _flags & UIButton::DROP_DOWN;

   if (msg == UIMessage::GET_HEIGHT) {
      if (isMenuItem) {
         return scale(ui_size::menu_item_height);
      } else {
         return scale(ui_size::button_height);
      }
   } else if (msg == UIMessage::GET_WIDTH) {
      int labelSize  = ui()->string_width(_label);
      int paddedSize = labelSize + scale(ui_size::button_padding);
      if (isDropDown)
         paddedSize += ui()->_active_font->_glyph_width * 2;
      int minimumSize = scale((_flags & UIButton::SMALL) ? 0
                              : isMenuItem               ? ui_size::menu_item_minimum_width
                                                         : ui_size::button_minimum_width);
      return paddedSize > minimumSize ? paddedSize : minimumSize;
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, _bounds,
                    (isMenuItem   ? UIControl::menu_item
                     : isDropDown ? UIControl::drop_down
                                  : UIControl::push_button) |
                       ((_flags & UIButton::CHECKED) ? UIControl::state_checked : 0) | state(),
                    _label, 0, _window->scale());
   } else if (msg == UIMessage::UPDATE) {
      repaint(nullptr);
   } else if (msg == UIMessage::DEALLOCATE) {
   } else if (msg == UIMessage::LEFT_DOWN) {
      if (_flags & UIButton::CAN_FOCUS) {
         focus();
      }
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if ((m->text == " ") || m->code == UIKeycode::ENTER) {
         message(UIMessage::CLICKED, 0, 0);
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
   if (_label != new_label) {
      _label = new_label;
      measurements_changed(1);
      repaint(nullptr);
   }
   return *this;
}

UIButton::UIButton(UIElement* parent, uint32_t flags, std::string_view label)
   : UIElementCast<UIButton>(parent, flags | UIElement::tab_stop_flag, UIButton::_ClassMessageProc, "Button")
   , _label(label) {}

UIButton* UIButtonCreate(UIElement* parent, uint32_t flags, std::string_view label) {
   return new UIButton(parent, flags | UIElement::tab_stop_flag, label);
}

// ------------------------------------------------------------------------------------------
int UICheckbox::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_HEIGHT) {
      return scale(ui_size::button_height);
   } else if (msg == UIMessage::GET_WIDTH) {
      int labelSize = ui()->string_width(_label);
      return scale(labelSize + ui_size::checkbox_box + ui_size::checkbox_gap);
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, _bounds,
                    UIControl::checkbox |
                       (check() == indeterminate ? UIControl::state_indeterminate
                        : check() == checked     ? UIControl::state_checked
                                                 : 0) |
                       state(),
                    _label, 0, _window->scale());
   } else if (msg == UIMessage::UPDATE) {
      repaint(nullptr);
   } else if (msg == UIMessage::DEALLOCATE) {
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if (m->text == " ") {
         message(UIMessage::CLICKED, 0, 0);
         repaint(nullptr);
      }
   } else if (msg == UIMessage::CLICKED) {
      set_check((_check + 1) % ((_flags & allow_indeterminate) ? 3 : 2));
      if (_on_click)
         _on_click(*this);
   }

   return 0;
}

UICheckbox& UICheckbox::set_label(std::string_view new_label) {
   if (_label != new_label) {
      _label = new_label;
      this->measurements_changed(1);
      repaint(nullptr);
   }
   return *this;
}

UICheckbox::UICheckbox(UIElement* parent, uint32_t flags, std::string_view label)
   : UIElementCast<UICheckbox>(parent, flags | UIElement::tab_stop_flag, UICheckbox::_ClassMessageProc, "Checkbox")
   , _check(0)
   , _label(label) {}


UICheckbox* UICheckboxCreate(UIElement* parent, uint32_t flags, std::string_view label) {
   return new UICheckbox(parent, flags | UIElement::tab_stop_flag, label);
}

// --------------------------------------------------
// Labels.
// --------------------------------------------------

int UILabel::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_HEIGHT) {
      return ui()->string_height();
   } else if (msg == UIMessage::GET_WIDTH) {
      return ui()->string_width(_label);
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, _bounds, UIControl::label | state(), _label, 0, _window->scale());
   } else if (msg == UIMessage::DEALLOCATE) {
   }

   return 0;
}

UILabel& UILabel::set_label(std::string_view new_label) {
   if (_label != new_label) {
      _label = new_label;
      measurements_changed(1);
      repaint(nullptr);
   }
   return *this;
}

UILabel::UILabel(UIElement* parent, uint32_t flags, std::string_view label)
   : UIElementCast<UILabel>(parent, flags | UIElement::tab_stop_flag, UILabel::_ClassMessageProc, "Label")
   , _label(label) {}

UILabel* UILabelCreate(UIElement* parent, uint32_t flags, std::string_view label) {
   return new UILabel(parent, flags, label);
}

// --------------------------------------------------
// Split panes.
// --------------------------------------------------
int UISplitter::_class_message_proc(UIMessage msg, int di, void* dp) {
   UISplitPane* splitPane = (UISplitPane*)_parent;
   bool         vertical  = splitPane->_flags & UIElement::vertical_flag;

   if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, _bounds, UIControl::splitter | (vertical ? UIControl::state_vertical : 0) | state(),
                    {}, 0, _window->scale());
   } else if (msg == UIMessage::GET_CURSOR) {
      return vertical ? (uint32_t)UICursor::split_v : (uint32_t)UICursor::split_h;
   } else if (msg == UIMessage::MOUSE_DRAG) {
      int   cursor       = vertical ? cursor_pos().y : cursor_pos().x;
      int   splitterSize = scale(ui_size::splitter);
      int   space        = (vertical ? splitPane->_bounds.height() : splitPane->_bounds.width()) - splitterSize;
      float oldWeight    = splitPane->weight();
      splitPane->set_weight(
         (float)(cursor - (float)splitterSize / 2 - (vertical ? splitPane->_bounds.t : splitPane->_bounds.l)) / space);
      splitPane->set_weight(std::clamp(splitPane->weight(), 0.05f, 0.95f));

      if (splitPane->_children[2]->_class_proc == UISplitPane::_ClassMessageProc &&
          (splitPane->_children[2]->_flags & UIElement::vertical_flag) ==
             (splitPane->_flags & UIElement::vertical_flag)) {
         UISplitPane* subSplitPane = (UISplitPane*)splitPane->_children[2];
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
   bool vertical = _flags & UIElement::vertical_flag;

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
   UIElementCreate(sizeof(UIElement), this, 0, UISplitter::_ClassMessageProc, "Splitter");
}

UISplitPane* UISplitPaneCreate(UIElement* parent, uint32_t flags, float weight) {
   return new UISplitPane(parent, flags, weight);
}

// --------------------------------------------------
// Tab panes.
// --------------------------------------------------

int UITabPane::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::PAINT) {
      UIPainter*  painter = (UIPainter*)dp;
      UIRectangle top     = _bounds;
      top.b               = top.t + scale(ui_size::button_height);
      UIDrawControl(painter, top, UIControl::tab_band, {}, 0, _window->scale());

      UIRectangle tab = top;
      tab.l += scale(ui_size::tab_pane_space_left);
      tab.t += scale(ui_size::tab_pane_space_top);

      for_each_tab([&](std::string_view tab_text, uint32_t index, bool active) {
         tab.r = tab.l + ui()->string_width(tab_text) + ui_size::button_padding;
         UIDrawControl(painter, tab, UIControl::tab | (active ? UIControl::state_selected : 0), tab_text, 0,
                       _window->scale());
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
            child->_flags &= ~UIElement::hide_flag;
            child->move(content, false);
            child->message(UIMessage::TAB_SELECTED, 0, 0);
         } else {
            child->_flags |= UIElement::hide_flag;
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
   , _tabs(tabs)
   , _active(0) {}

UITabPane* UITabPaneCreate(UIElement* parent, uint32_t flags, const char* tabs) {
   return new UITabPane(parent, flags, tabs);
}

// --------------------------------------------------
// Spacers.
// --------------------------------------------------

int UISpacer::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_HEIGHT) {
      return scale(_height);
   } else if (msg == UIMessage::GET_WIDTH) {
      return scale(_width);
   }

   return 0;
}

UISpacer::UISpacer(UIElement* parent, uint32_t flags, int width, int height)
   : UIElementCast<UISpacer>(parent, flags, UISpacer::_ClassMessageProc, "Spacer")
   , _width(width)
   , _height(height) {}

UISpacer* UISpacerCreate(UIElement* parent, uint32_t flags, int width, int height) {
   return new UISpacer(parent, flags, width, height);
}

// --------------------------------------------------
// Scroll bars.
// --------------------------------------------------

int UIScrollBar::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_WIDTH || msg == UIMessage::GET_HEIGHT) {
      return scale(ui_size::scroll_bar);
   } else if (msg == UIMessage::LAYOUT) {
      UIElement* up    = _children[0];
      UIElement* thumb = _children[1];
      UIElement* down  = _children[2];

      if (page() >= maximum() || maximum() <= 0 || page() <= 0) {
         up->_flags |= UIElement::hide_flag;
         thumb->_flags |= UIElement::hide_flag;
         down->_flags |= UIElement::hide_flag;

         _position = 0;
      } else {
         up->_flags &= ~UIElement::hide_flag;
         thumb->_flags &= ~UIElement::hide_flag;
         down->_flags &= ~UIElement::hide_flag;

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

         int thumbPosition = (int)((double)_position / (maximum() - page()) * (size - thumbSize));

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
      UIDrawControl((UIPainter*)dp, _bounds,
                    UIControl::scroll_track |
                       ((page() >= maximum() || maximum() <= 0 || page() <= 0) ? UIControl::state_disabled : 0),
                    {}, 0, _window->scale());
   } else if (msg == UIMessage::MOUSE_WHEEL) {
      _position += di;
      refresh();
      _parent->message(UIMessage::SCROLLED, 0, 0);
      return 1;
   }

   return 0;
}

int _UIScrollUpDownMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UIScrollBar* scrollBar = (UIScrollBar*)el->_parent;
   bool         isDown    = el->_cp;

   if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, el->_bounds,
                    (isDown ? UIControl::scroll_down : UIControl::scroll_up) |
                       (scrollBar->_horizontal ? 0 : UIControl::state_vertical) | el->state(),
                    {}, 0, el->_window->scale());
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
      scrollBar->_parent->message(UIMessage::SCROLLED, 0, 0);
   }

   return 0;
}

int _UIScrollThumbMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UIScrollBar* scrollBar = (UIScrollBar*)el->_parent;

   if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, el->_bounds,
                    UIControl::scroll_thumb | (scrollBar->_horizontal ? 0 : UIControl::state_vertical) | el->state(),
                    {}, 0, el->_window->scale());
   } else if (msg == UIMessage::UPDATE) {
      el->repaint(nullptr);
   } else if (msg == UIMessage::MOUSE_DRAG && el->_window->pressed_button() == 1) {
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
      scrollBar->position() = (double)thumbPosition / size * (scrollBar->maximum() - scrollBar->page());
      scrollBar->refresh();
      scrollBar->_parent->message(UIMessage::SCROLLED, 0, 0);
   } else if (msg == UIMessage::LEFT_UP) {
      scrollBar->_in_drag = false;
   }

   return 0;
}

UIScrollBar::UIScrollBar(UIElement* parent, uint32_t flags)
   : UIElementCast<UIScrollBar>(parent, flags, UIScrollBar::_ClassMessageProc, "Scroll Bar")
   , _maximum(0)
   , _page(0)
   , _drag_offset(0)
   , _position(0)
   , _last_animate_time(0)
   , _in_drag(false)
   , _horizontal(flags & UIScrollBar::HORIZONTAL) {
   auto scrollup = new UIElement(this, flags, _UIScrollUpDownMessageProc, !_horizontal ? "Scroll Up" : "Scroll Left");
   scrollup->_cp = (void*)(uintptr_t)0;

   new UIElement(this, flags, _UIScrollThumbMessageProc, "Scroll Thumb");

   auto scrolldown =
      new UIElement(this, flags, _UIScrollUpDownMessageProc, !_horizontal ? "Scroll Down" : "Scroll Right");
   scrolldown->_cp = (void*)(uintptr_t)1;
}

UIScrollBar* UIScrollBarCreate(UIElement* parent, uint32_t flags) {
   return new UIScrollBar(parent, flags);
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

int UICode::column_to_byte(size_t ln, size_t column) const {
   return UI::column_to_byte(line(ln), column, tab_columns());
}

int UICode::byte_to_column(size_t ln, size_t byte) const {
   return UI::byte_to_column(line(ln), byte, tab_columns());
}

UICode& UICode::clear() {
   _content.clear();
   _lines.clear();
   _max_columns = 0;
   _selection   = {};
   return *this;
}

UICode& UICode::load_file(const char* path, std::optional<std::string_view> err /* = {} */) {
   std::string buff = LoadFile(path);
   if (buff.empty())
      insert_content(err ? *err : std::format("The file '{}' could not be loaded.", path), true);
   else
      insert_content(buff, true);
   _current_line.reset();
   return *this;
}

UICode& UICode::position_to_byte(int x, int y, size_t* line, size_t* byte) {
   UI*     ui           = this->ui();
   UIFont* previousFont = _font->activate();
   int     lineHeight   = ui->string_height();
   *line                = std::max((int64_t)0, (y - _bounds.t + _vscroll->position()) / lineHeight);
   if (*line >= num_lines())
      *line = num_lines() - 1;
   int column = (x - _bounds.l + _hscroll->position() + ui->_active_font->_glyph_width / 2) / ui->_active_font->_glyph_width;
   if (~_flags & UICode::NO_MARGIN)
      column -= (ui->code_margin() + ui->code_margin_gap()) / ui->_active_font->_glyph_width;
   previousFont->activate();
   *byte = column_to_byte(*line, column);
   return *this;
}

int UICode::hittest(int x, int y) {
   UI* ui = this->ui();
   x -= _bounds.l;

   if (x < 0 || x >= _vscroll->_bounds.l) {
      return 0;
   }

   y -= _bounds.t - _vscroll->position();

   UIFont* previousFont = _font->activate();
   int     lineHeight   = ui->string_height();
   bool    inMargin     = x < ui->code_margin() + ui->code_margin_gap() / 2 && (~_flags & UICode::NO_MARGIN);
   previousFont->activate();

   if (y < 0 || y >= lineHeight * (int)num_lines()) {
      return 0;
   }

   int line = y / lineHeight + 1;
   return inMargin ? -line : line;
}

int UIDrawStringHighlighted(UIPainter* painter, UIRectangle lineBounds, std::string_view string, int tabSize,
                            UIStringSelection* selection) {
   UI* ui = painter->ui();
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

   uint32_t colors[] = {
      ui->_theme.codeDefault, ui->_theme.codeComment,  ui->_theme.codeString,
      ui->_theme.codeNumber,  ui->_theme.codeOperator, ui->_theme.codePreprocessor,
   };

   int              lineHeight = painter->ui()->string_height();
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

      UI_ASSERT(bytesConsumed > 0);
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

      int oldX = x;

      if (c == '\t') {
         x += ui->_active_font->_glyph_width, ti++;
         while (ti % tabSize)
            x += ui->_active_font->_glyph_width, ti++, j++;
      } else {
         painter->draw_glyph(x, y, c, colors[tokenType]);
         x += ui->_active_font->_glyph_width, ti++;
      }

      if (selection && j >= selection->carets[0] && j < selection->carets[1]) {
         UIDrawBlock(painter, UIRectangle(oldX, x, y, y + lineHeight), selection->colorBackground);
         if (c != '\t')
            painter->draw_glyph(oldX, y, c, selection->colorText);
      }

      if (selection && selection->carets[0] == j) {
         UIDrawInvert(painter, UIRectangle(oldX, oldX + 1, y, y + lineHeight));
      }

      j++;
   }

   if (selection && selection->carets[0] == j) {
      UIDrawInvert(painter, UIRectangle(x, x + 1, y, y + lineHeight));
   }

   return x;
}

UICode& UICode::copy(sel_target_t t) {
   size_t from = offset(selection(0));
   size_t to   = offset(selection(1));

   if (from != to)
      _window->write_clipboard_text(std::string_view{&(*this)[from], to - from}, t);

   return *this;
}

UICode& UICode::_update_selection() {
   bool swap = _selection[3].line < _selection[2].line ||
               (_selection[3].line == _selection[2].line && _selection[3].offset < _selection[2].offset);
   _selection[1 - swap] = _selection[3];
   _selection[0 + swap] = _selection[2];
   copy(sel_target_t::primary);
   _move_scroll_to_caret_next_layout = true;
   refresh();
   return *this;
}

UICode& UICode::_set_vertical_motion_column(bool restore) {
   if (restore) {
      _selection[3].offset = column_to_byte(_selection[3].line, _vertical_motion_column);
   } else if (!_use_vertical_motion_column) {
      _use_vertical_motion_column = true;
      _vertical_motion_column     = byte_to_column(_selection[3].line, _selection[3].offset);
   }
   return *this;
}

int UICode::_class_message_proc(UIMessage msg, int di, void* dp) {
   UI* ui = this->ui();
   if (msg == UIMessage::LAYOUT) {
      auto str_height = ui->string_height();

      UIFont* previousFont  = font()->activate();
      int     scrollBarSize = scale(ui_size::scroll_bar);
      _vscroll->set_maximum(num_lines() * str_height);
      _hscroll->set_maximum(max_columns() * font()->_glyph_width); // TODO This doesn't take into account tab sizes!
      int vSpace = _bounds.height();
      int hSpace = _bounds.width();

      _vscroll->set_page(vSpace);
      _hscroll->set_page(hSpace);

      if (_move_scroll_to_caret_next_layout) {
         size_t top     = _selection[3].line * str_height;
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

      if (!(_flags & UICode::NO_MARGIN))
         hSpace -= ui->code_margin() + ui->code_margin_gap();
      layout_scrollbar_pair(hSpace, vSpace, scrollBarSize, this);

      previousFont->activate();
   } else if (msg == UIMessage::PAINT) {
      UIFont* previousFont = font()->activate();

      UIPainter*  painter    = (UIPainter*)dp;
      UIRectangle lineBounds = _bounds;

      lineBounds.r = _vscroll->_bounds.l;

      if (~_flags & UICode::NO_MARGIN) {
         lineBounds.l += ui->code_margin() + ui->code_margin_gap();
      }

      int lineHeight = ui->string_height();
      lineBounds.t -= (int64_t)_vscroll->position() % lineHeight;

      UIDrawBlock(painter, _bounds, ui->_theme.codeBackground);

      UIStringSelection selection = {};
      selection.colorBackground   = ui->_theme.selected;
      selection.colorText         = ui->_theme.textSelected;

      for (size_t i = _vscroll->position() / lineHeight; i < num_lines(); i++) {
         if (lineBounds.t > _clip.b) {
            break;
         }

         lineBounds.b = lineBounds.t + lineHeight;

         if (~_flags & UICode::NO_MARGIN) {
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

            uint32_t marginColor = message(UIMessage::CODE_GET_MARGIN_COLOR, (uint32_t)(i + 1), 0);

            if (marginColor) {
               UIDrawBlock(painter, marginBounds, marginColor);
            }

            UIDrawString(painter, marginBounds, {string + p, static_cast<size_t>(16 - p)},
                         marginColor ? ui->_theme.codeDefault : ui->_theme.codeComment, UIAlign::right, NULL);
         }

         if (focus_line() == i) {
            UIDrawBlock(painter, lineBounds, ui->_theme.codeFocused);
         }

         UIRectangle oldClip = painter->_clip;
         painter->_clip       = intersection(oldClip, lineBounds);
         if (_hscroll)
            lineBounds.l -= (int64_t)_hscroll->position();
         selection.carets[0] = i == _selection[0].line ? byte_to_column(i, _selection[0].offset) : 0;
         selection.carets[1] = i == _selection[1].line ? byte_to_column(i, _selection[1].offset) : (int)line(i).size();

         bool selected = is_focused() && i >= _selection[0].line && i <= _selection[1].line;
         int  x = UIDrawStringHighlighted(painter, lineBounds, line(i), tab_columns(), selected ? &selection : nullptr);
         int  y = (lineBounds.t + lineBounds.b - ui->string_height()) / 2;

         if (is_focused() && i >= _selection[0].line && i < _selection[1].line) {
            UIDrawBlock(painter, ui_rect_4pd(x, y, font()->_glyph_width, font()->_glyph_height), selection.colorBackground);
         }

         if (_hscroll)
            lineBounds.l += (int64_t)_hscroll->position();
         painter->_clip = oldClip;

         UICodeDecorateLine m;
         m.x = x, m.y = y, m.bounds = lineBounds, m.index = (int)i, m.painter = painter;
         message(UIMessage::CODE_DECORATE_LINE, 0, &m);

         lineBounds.t += lineHeight;
      }

      previousFont->activate();
   } else if (msg == UIMessage::SCROLLED) {
      _move_scroll_to_focus_next_layout = false;
      refresh();
   } else if (msg == UIMessage::MOUSE_WHEEL) {
      return _vscroll->message(msg, di, dp);
   } else if (msg == UIMessage::GET_CURSOR) {
      if (hittest(cursor_pos()) < 0) {
         return (int)UICursor::flipped_arrow;
      }

      if (_flags & UICode::SELECTABLE) {
         return (int)UICursor::text;
      }
   } else if (msg == UIMessage::LEFT_UP) {
      animate(true);
   } else if (msg == UIMessage::LEFT_DOWN && num_lines()) {
      int hitTest          = hittest(cursor_pos());
      _left_down_in_margin = hitTest < 0;

      if (hitTest > 0 && (_flags & UICode::SELECTABLE)) {
         position_to_byte(cursor_pos(), &_selection[2].line, &_selection[2].offset);
         _class_message_proc(UIMessage::MOUSE_DRAG, di, dp);
         focus();
         animate(false);
         set_last_animate_time(UI_CLOCK());
      }
   } else if (msg == UIMessage::ANIMATE) {
      if (is_pressed() && _window->pressed_button() == 1 && num_lines() && !_left_down_in_margin) {
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

         UIFont* previousFont = font()->activate();
         auto    pos          = cursor_pos();
         if (pos.x < _bounds.l + ((_flags & UICode::NO_MARGIN) ? ui->code_margin_gap()
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
         previousFont->activate();
         UICode::_ClassMessageProc(this, UIMessage::MOUSE_DRAG, di, dp);
         refresh();
      }
   } else if (msg == UIMessage::MOUSE_DRAG && _window->pressed_button() == 1 && num_lines() && !_left_down_in_margin) {
      // TODO Double-click and triple-click dragging for word and line granularity respectively.
      position_to_byte(cursor_pos(), &_selection[3].line, &_selection[3].offset);
      _update_selection();
      _move_scroll_to_focus_next_layout = _move_scroll_to_caret_next_layout = false;
      _use_vertical_motion_column                                           = false;
   } else if (msg == UIMessage::KEY_TYPED && num_lines()) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if ((m->code == UI_KEYCODE_LETTER('C') || m->code == UI_KEYCODE_LETTER('X') || m->code == UIKeycode::INSERT) &&
          _window->_ctrl && !_window->_alt && !_window->_shift) {
         copy(sel_target_t::clipboard);
      } else if ((m->code == UIKeycode::UP || m->code == UIKeycode::DOWN || m->code == UIKeycode::PAGE_UP ||
                  m->code == UIKeycode::PAGE_DOWN) &&
                 !_window->_ctrl && !_window->_alt) {
         UIFont* previousFont = font()->activate();
         int     lineHeight   = ui->string_height();

         if (_window->_shift) {
            if (m->code == UIKeycode::UP) {
               if ((int)_selection[3].line - 1 >= 0) {
                  _set_vertical_motion_column(false);
                  _selection[3].line--;
                  _set_vertical_motion_column(true);
               }
            } else if (m->code == UIKeycode::DOWN) {
               if (_selection[3].line + 1 < num_lines()) {
                  _set_vertical_motion_column(false);
                  _selection[3].line++;
                  _set_vertical_motion_column(true);
               }
            } else if (m->code == UIKeycode::PAGE_UP || m->code == UIKeycode::PAGE_DOWN) {
               _set_vertical_motion_column(false);
               int pageHeight = (_bounds.t - _hscroll->_bounds.t) / lineHeight * 4 / 5;
               _selection[3].line += m->code == UIKeycode::PAGE_UP ? pageHeight : -pageHeight;
               // if (selection[3].line < 0)
               //    selection[3].line = 0;
               if (_selection[3].line >= num_lines())
                  _selection[3].line = num_lines() - 1;
               _set_vertical_motion_column(true);
            }

            _update_selection();
         } else {
            _move_scroll_to_focus_next_layout = false;
            key_input_vscroll(m, lineHeight,
                              (_bounds.t - _hscroll->_bounds.t) * 4 / 5 /* leave a few lines for context */, this);
         }

         previousFont->activate();
      } else if ((m->code == UIKeycode::HOME || m->code == UIKeycode::END) && !_window->_alt) {
         if (_window->_shift) {
            if (m->code == UIKeycode::HOME) {
               if (_window->_ctrl)
                  _selection[3].line = 0;
               _selection[3].offset        = 0;
               _use_vertical_motion_column = false;
            } else {
               if (_window->_ctrl)
                  _selection[3].line = num_lines() - 1;
               _selection[3].offset        = line(_selection[3].line).size();
               _use_vertical_motion_column = false;
            }

            _update_selection();
         } else {
            _vscroll->position()              = m->code == UIKeycode::HOME ? 0 : _vscroll->maximum();
            _move_scroll_to_focus_next_layout = false;
            refresh();
         }
      } else if ((m->code == UIKeycode::LEFT || m->code == UIKeycode::RIGHT) && !_window->_alt) {
         if (_window->_shift) {
            move_caret(m->code == UIKeycode::LEFT, _window->_ctrl);
         } else if (!_window->_ctrl) {
            _hscroll->position() +=
               m->code == UIKeycode::LEFT ? -ui->_active_font->_glyph_width : ui->_active_font->_glyph_width;
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

      if (hitTest > 0 && (_flags & UICode::SELECTABLE)) {
         focus();
         ui->create_menu(_window, UIMenu::NO_SCROLL)
            .add_item((_selection[0].line == _selection[1].line && _selection[0].offset == _selection[1].offset)
                         ? disabled_flag
                         : 0,
                      "Copy", [this](UIButton&) { copy(sel_target_t::clipboard); })
            .show();
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
         if ((int)_selection[3].offset - 1 < 0) {
            if (_selection[3].line > 0) {
               _selection[3].line--;
               _selection[3].offset = _lines[_selection[3].line].bytes;
            } else
               break;
         } else
            _ui_move_caret_backwards(_selection[3].offset, &_content[0], offset(_selection[3]),
                                     _lines[_selection[3].line].offset);
      } else {
         if (_selection[3].offset + 1 > _lines[_selection[3].line].bytes) {
            if (_selection[3].line + 1 < num_lines()) {
               _selection[3].line++;
               _selection[3].offset = 0;
            } else
               break;
         } else
            _ui_move_caret_forward(_selection[3].offset, std::string_view(&_content[0], size()), offset(_selection[3]));
      }

      if (!word)
         break;

      if (_selection[3].offset != 0 && _selection[3].offset != _lines[_selection[3].line].bytes) {
         if (_ui_move_caret_by_word(std::string_view{&_content[0], size()}, offset(_selection[3])))
            break;
      }
   }

   _use_vertical_motion_column = false;
   _update_selection();
   return *this;
}

// Line numbers are 1-indexed!!
UICode& UICode::set_focus_line(size_t index) {
   _focus_line                       = index;
   _move_scroll_to_focus_next_layout = true;
   refresh();
   return *this;
}

UICode& UICode::insert_content(std::string_view new_content, bool replace) {
   constexpr size_t max_size = 1000000000;
   if (new_content.size() > max_size)
      new_content = new_content.substr(0, max_size);

   _use_vertical_motion_column = false;

   UIFont* previousFont = _font->activate();

   if (replace)
      clear();

   if (new_content.empty())
      return *this;

   size_t sz        = new_content.size();
   size_t orig_size = _content.size();

   _content.resize(orig_size + sz);

   size_t lineCount = new_content.back() != '\n';

   for (size_t i = 0; i < sz; ++i) {
      _content[orig_size + i] = new_content[i];

      if (new_content[i] == '\n')
         lineCount++;
   }

   size_t orig_lines = _lines.size();
   _lines.reserve(orig_lines + lineCount);

   for (size_t i = 0, offset = 0; i <= sz; ++i) {
      if (i == sz || new_content[i] == '\n') {
         emplace_back_line(orig_size + offset, i - offset);
         offset = i + 1;
      }
   }

   if (!replace) {
      _vscroll->position() = _lines.size() * ui()->string_height();
   }

   previousFont->activate();
   repaint(nullptr);
   return *this;
}

UICode::UICode(UIElement* parent, uint32_t flags)
   : UIElementCast<UICode>(parent, flags, UICode::_ClassMessageProc, "Code")
   , UIScrollbarPair(this)
   , _font(parent->ui()->_active_font) {}

UICode* UICodeCreate(UIElement* parent, uint32_t flags) {
   return new UICode(parent, flags);
}

// --------------------------------------------------
// Gauges.
// --------------------------------------------------

int UIGauge::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_HEIGHT) {
      return scale(_vertical ? ui_size::gauge_width : ui_size::gauge_height);
   } else if (msg == UIMessage::GET_WIDTH) {
      return scale(_vertical ? ui_size::gauge_height : ui_size::gauge_width);
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, _bounds, UIControl::gauge | state() | (_vertical ? UIControl::state_vertical : 0),
                    {}, _position, _window->scale());
   }

   return 0;
}

UIGauge& UIGauge::set_position(double new_pos) {
   new_pos = std::clamp(new_pos, 0., 1.);
   if (new_pos != _position) {
      _position = new_pos;
      repaint(nullptr);
   }
   return *this;
}

UIGauge::UIGauge(UIElement* parent, uint32_t flags)
   : UIElementCast<UIGauge>(parent, flags, UIGauge::_ClassMessageProc, "Gauge")
   , _position(0)
   , _vertical(!!(flags & vertical_flag)) {}

UIGauge* UIGaugeCreate(UIElement* parent, uint32_t flags) {
   return new UIGauge(parent, flags);
}

// --------------------------------------------------
// Sliders.
// --------------------------------------------------

int UISlider::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_HEIGHT) {
      return scale(_vertical ? ui_size::slider_width : ui_size::slider_height);
   } else if (msg == UIMessage::GET_WIDTH) {
      return scale(_vertical ? ui_size::slider_height : ui_size::slider_width);
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, _bounds, UIControl::slider | state() | (_vertical ? UIControl::state_vertical : 0),
                    {}, _position, _window->scale());
   } else if (msg == UIMessage::LEFT_DOWN || (msg == UIMessage::MOUSE_DRAG && _window->pressed_button() == 1)) {
      UIRectangle bounds    = _bounds;
      int         thumbSize = scale(ui_size::slider_thumb);
      auto        pos       = cursor_pos();
      _position = _vertical ? 1 - ((float)(pos.y - thumbSize / 2 - bounds.t) / (bounds.height() - thumbSize))
                            : (double)(pos.x - thumbSize / 2 - bounds.l) / (bounds.width() - thumbSize);
      if (_steps > 1)
         _position = (int)(_position * (_steps - 1) + 0.5f) / (double)(_steps - 1);
      if (_position < 0)
         _position = 0;
      if (_position > 1)
         _position = 1;
      message(UIMessage::VALUE_CHANGED, 0, 0);
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
   if (new_pos != _position) {
      if (_steps > 1)
         _position = (int)(_position * (_steps - 1) + 0.5f) / (float)(_steps - 1);
      message(UIMessage::VALUE_CHANGED, 0, 0);
      repaint(nullptr);
   }
   return *this;
}

UISlider::UISlider(UIElement* parent, uint32_t flags)
   : UIElementCast<UISlider>(parent, flags, UISlider::_ClassMessageProc, "Slider")
   , _position(0)
   , _steps(0)
   , _vertical(!!(flags & vertical_flag)) {}

UISlider* UISliderCreate(UIElement* parent, uint32_t flags) {
   return new UISlider(parent, flags);
}

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

   if (y < 0 || y >= (int)(rowHeight * _num_items)) {
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
   } else if (y > height) {
      _vscroll->position() -= height - y;
      refresh();
      return true;
   } else {
      return false;
   }
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
         m.index   = i;
         int bytes = message(UIMessage::TABLE_GET_ITEM, 0, &m);
         int width = ui()->string_width(m.buff(bytes));

         if (width > longest) {
            longest = width;
         }
      }

      _column_widths[m.column] = longest;
      m.column++;
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
      UIPainter*  painter = (UIPainter*)dp;
      UIRectangle bounds  = _bounds;
      bounds.r            = _vscroll->_bounds.l;
      UIDrawControl(painter, _bounds, UIControl::table_background | state(), {}, 0, _window->scale());
      UIRectangle    row       = bounds;
      int            rowHeight = scale(ui_size::table_row);
      UITableGetItem m(256);
      row.t += scale(ui_size::table_header);
      row.t -= (int64_t)_vscroll->position() % rowHeight;
      int         hovered = hittest(cursor_pos());
      UIRectangle oldClip = painter->_clip;
      painter->_clip =
         intersection(oldClip, UIRectangle(bounds.l, bounds.r, bounds.t + scale(ui_size::table_header), bounds.b));

      for (int i = _vscroll->position() / rowHeight; i < (int)num_items(); i++) {
         if (row.t > painter->_clip.b) {
            break;
         }

         row.b        = row.t + rowHeight;
         m.index      = i;
         m.isSelected = false;
         m.column     = 0;

         int bytes = message(UIMessage::TABLE_GET_ITEM, 0, &m);

         uint32_t rowFlags =
            (m.isSelected ? UIControl::state_selected : 0) | (hovered == i ? UIControl::state_hovered : 0);
         UIDrawControl(painter, row, UIControl::table_row | rowFlags, {}, 0, _window->scale());

         UIRectangle cell = row;
         cell.l += scale(ui_size::table_column_gap) - (int64_t)_hscroll->position();

         for (size_t j = 0; j < _column_widths.size(); j++) {
            if (j) {
               m.column = j;
               bytes    = message(UIMessage::TABLE_GET_ITEM, 0, &m);
            }

            cell.r = cell.l + _column_widths[j];
            if ((size_t)bytes > m.buff_size() && bytes > 0)
               bytes = m.buff_size();
            if (bytes > 0)
               UIDrawControl(painter, cell, UIControl::table_cell | rowFlags, m.buff(bytes), 0, _window->scale());
            cell.l += _column_widths[j] + scale(ui_size::table_column_gap);
         }

         row.t += rowHeight;
      }

      bounds        = _bounds;
      painter->_clip = intersection(oldClip, bounds);
      if (_hscroll)
         bounds.l -= (int64_t)_hscroll->position();

      UIRectangle header = bounds;
      header.b           = header.t + scale(ui_size::table_header);
      header.l += scale(ui_size::table_column_gap);

      size_t position = 0;
      size_t index    = 0;

      if (!_column_widths.empty()) {
         while (true) {
            size_t end = column_end(position);

            header.r = header.l + _column_widths[index];
            UIDrawControl(painter, header,
                          UIControl::table_header | (index == _column_highlight ? UIControl::state_selected : 0),
                          column(position, end), 0, _window->scale());
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
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if ((m->code == UIKeycode::UP || m->code == UIKeycode::DOWN || m->code == UIKeycode::PAGE_UP ||
           m->code == UIKeycode::PAGE_DOWN || m->code == UIKeycode::HOME || m->code == UIKeycode::END) &&
          !_window->_ctrl && !_window->_alt && !_window->_shift) {
         key_input_vscroll(m, scale(ui_size::table_row),
                           (_bounds.t - _hscroll->_bounds.t + ui_size::table_header) * 4 / 5, this);
         return 1;
      } else if ((m->code == UIKeycode::LEFT || m->code == UIKeycode::RIGHT) && !_window->_ctrl && !_window->_alt &&
                 !_window->_shift) {
         _hscroll->position() +=
            m->code == UIKeycode::LEFT ? -ui()->_active_font->_glyph_width : ui()->_active_font->_glyph_width;
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
   , _num_items(0)
   , _columns(columns)
   , _column_highlight((size_t)-1) {}

UITable* UITableCreate(UIElement* parent, uint32_t flags, const char* columns) {
   return new UITable(parent, flags, columns);
}

// --------------------------------------------------
// Textboxes.
// --------------------------------------------------

int UITextbox::byte_to_column(std::string_view string, int byte) {
   return UI::byte_to_column(string, byte, 4);
}

int UITextbox::column_to_byte(std::string_view string, int column) {
   return UI::column_to_byte(string, column, 4);
}

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
      message(UIMessage::VALUE_CHANGED, 0, 0);
   _window->set_textbox_modified_flag(true);
   repaint(nullptr);
   return *this;
}

UITextbox& UITextbox::clear(bool sendChangedMessage) {
   _carets[1] = 0;
   _carets[0] = text().size();
   return replace_text("", sendChangedMessage);
}

UITextbox& UITextbox::move_caret(bool backward, bool word) {
   while (true) {
      std::string_view cur_text = text();
      if (_carets[0] > 0 && backward) {
         _ui_move_caret_backwards(_carets[0], cur_text.data(), _carets[0], 0);
      } else if (_carets[0] < (int)cur_text.size() && !backward) {
         _ui_move_caret_forward(_carets[0], cur_text, _carets[0]);
      } else {
         return *this;
      }

      if (!word) {
         return *this;
      } else if (_carets[0] != (int)cur_text.size() && _carets[0] != 0) {
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

int UITextbox::_class_message_proc(UIMessage msg, int di, void* dp) {
   UI* ui = this->ui();
   if (msg == UIMessage::GET_HEIGHT) {
      return scale(ui_size::textbox_height);
   } else if (msg == UIMessage::GET_WIDTH) {
      return scale(ui_size::textbox_width);
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, _bounds, UIControl::textbox | state(), {}, 0, _window->scale());

      int         scaledMargin = scale(ui_size::textbox_margin);
      int         totalWidth   = ui->string_width(text()) + scaledMargin * 2;
      UIRectangle textBounds   = _bounds + ui_rect_1i(scaledMargin);

      if (_scroll > totalWidth - textBounds.width()) {
         _scroll = totalWidth - textBounds.width();
      }

      if (_scroll < 0) {
         _scroll = 0;
      }

      int caretX = ui->string_width(text().substr(0, _carets[0])) - _scroll;

      if (caretX < 0) {
         _scroll = caretX + _scroll;
      } else if (caretX > textBounds.width()) {
         _scroll = caretX - textBounds.width() + _scroll + 1;
      }

      UIStringSelection selection = {};
      selection.carets[0]         = byte_to_column(text(), _carets[0]);
      selection.carets[1]         = byte_to_column(text(), _carets[1]);
      selection.colorBackground   = ui->_theme.selected;
      selection.colorText         = ui->_theme.textSelected;
      textBounds.l -= _scroll;

      auto cur_text = text();
      if (!cur_text.empty()) {
         UIDrawString((UIPainter*)dp, textBounds, cur_text, is_disabled() ? ui->_theme.textDisabled : ui->_theme.text,
                      UIAlign::left, is_focused() ? &selection : NULL);
      }
   } else if (msg == UIMessage::GET_CURSOR) {
      return (int)UICursor::text;
   } else if (msg == UIMessage::LEFT_DOWN) {
      int column = (_window->cursor_pos().x - _bounds.l + _scroll - scale(ui_size::textbox_margin) +
                    ui->_active_font->_glyph_width / 2) /
                   ui->_active_font->_glyph_width;
      _carets[0] = _carets[1] = column <= 0 ? 0 : column_to_byte(text(), column);
      focus();
   } else if (msg == UIMessage::UPDATE) {
      repaint(nullptr);
   } else if (msg == UIMessage::DEALLOCATE) {
      ;
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m       = (UIKeyTyped*)dp;
      bool        handled = true;

      if (_reject_next_key) {
         _reject_next_key = false;
         handled          = false;
      } else if (m->code == UIKeycode::BACKSPACE || m->code == UIKeycode::DEL) {
         if (_carets[0] == _carets[1]) {
            move_caret(m->code == UIKeycode::BACKSPACE, _window->_ctrl);
         }

         replace_text("", true);
      } else if (m->code == UIKeycode::LEFT || m->code == UIKeycode::RIGHT) {
         if (_carets[0] == _carets[1] || _window->_shift) {
            move_caret(m->code == UIKeycode::LEFT, _window->_ctrl);
            if (!_window->_shift)
               _carets[1] = _carets[0];
         } else {
            _carets[1 - _window->_shift] = _carets[_window->_shift];
         }
      } else if (m->code == UIKeycode::HOME || m->code == UIKeycode::END) {
         if (m->code == UIKeycode::HOME) {
            _carets[0] = 0;
         } else {
            _carets[0] = text().size();
         }

         if (!_window->_shift) {
            _carets[1] = _carets[0];
         }
      } else if (m->code == UI_KEYCODE_LETTER('A') && _window->_ctrl) {
         _carets[1] = 0;
         _carets[0] = text().size();
      } else if (m->text.size() && !_window->_alt && !_window->_ctrl && m->text[0] >= 0x20) {
         replace_text(m->text, true);
      } else if ((m->code == UI_KEYCODE_LETTER('C') || m->code == UI_KEYCODE_LETTER('X') ||
                  m->code == UIKeycode::INSERT) &&
                 _window->_ctrl && !_window->_alt && !_window->_shift) {
         copy();

         if (m->code == UI_KEYCODE_LETTER('X')) {
            replace_text("", true);
         }
      } else if ((m->code == UI_KEYCODE_LETTER('V') && _window->_ctrl && !_window->_alt && !_window->_shift) ||
                 (m->code == UIKeycode::INSERT && !_window->_ctrl && !_window->_alt && _window->_shift)) {
         paste(sel_target_t::clipboard);
      } else {
         handled = false;
      }

      if (handled) {
         repaint(nullptr);
         return 1;
      }
   } else if (msg == UIMessage::RIGHT_DOWN) {
      int c0 = _carets[0], c1 = _carets[1];
      _class_message_proc(UIMessage::LEFT_DOWN, di, dp);

      if (c0 < c1 ? (_carets[0] >= c0 && _carets[0] < c1) : (_carets[0] >= c1 && _carets[0] < c0)) {
         _carets[0] = c0,
         _carets[1] = c1; // Only move caret if clicking outside the existing selection.
      }

      std::string paste_str = _window->read_clipboard_text(sel_target_t::clipboard);
      ui->create_menu(_window, UIMenu::NO_SCROLL)
         .add_item(_carets[0] == _carets[1] ? UIElement::disabled_flag : 0, "Copy", [this](UIButton&) { copy(); })
         .add_item(paste_str.empty() ? UIElement::disabled_flag : 0, "Paste",
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
   : UIElementCast<UITextbox>(parent, flags | tab_stop_flag, UITextbox::_ClassMessageProc, "Textbox")
   , _carets({0, 0})
   , _scroll(0)
   , _reject_next_key(false) {}

UITextbox* UITextboxCreate(UIElement* parent, uint32_t flags) {
   return new UITextbox(parent, flags);
}

// --------------------------------------------------
// MDI clients.
// --------------------------------------------------
int _UIMDIChildHitTest(UIMDIChild* mdiChild, UIPoint pt) {
   UIElement* el = mdiChild;
   auto [titleSize, borderSize, titleRect, contentRect] =
      ui_mdi_child_calculate_layout(el->_bounds, el->_window->scale());
   int cornerSize = el->scale(ui_size::mdi_child_corner);
   if (!el->_bounds.contains(pt.x, pt.y) || contentRect.contains(pt.x, pt.y))
      return -1;
   else if (pt.x < el->_bounds.l + cornerSize && pt.y < el->_bounds.t + cornerSize)
      return 0b1010;
   else if (pt.x > el->_bounds.r - cornerSize && pt.y < el->_bounds.t + cornerSize)
      return 0b0110;
   else if (pt.x < el->_bounds.l + cornerSize && pt.y > el->_bounds.b - cornerSize)
      return 0b1001;
   else if (pt.x > el->_bounds.r - cornerSize && pt.y > el->_bounds.b - cornerSize)
      return 0b0101;
   else if (pt.x < el->_bounds.l + borderSize)
      return 0b1000;
   else if (pt.x > el->_bounds.r - borderSize)
      return 0b0100;
   else if (pt.y < el->_bounds.t + borderSize)
      return 0b0010;
   else if (pt.y > el->_bounds.b - borderSize)
      return 0b0001;
   else if (titleRect.contains(pt.x, pt.y))
      return 0b1111;
   else
      return -1;
}

void _UIMDIChildCloseButton(void* _child) {
   UIElement* child = (UIElement*)_child;

   if (!child->message(UIMessage::WINDOW_CLOSE, 0, 0)) {
      child->destroy();
      child->_parent->refresh();
   }
}

int UIMDIChild::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UIMDIChild* mdiChild = (UIMDIChild*)el;

   if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, el->_bounds, UIControl::mdi_child, mdiChild->_title, 0, el->_window->scale());
   } else if (msg == UIMessage::GET_WIDTH) {
      UIElement* child = el->_children.empty() ? nullptr : el->_children.back();
      int        width = 2 * ui_size::mdi_child_border;
      width +=
         (child ? child->message(msg, di ? (di - ui_size::mdi_child_title + ui_size::mdi_child_border) : 0, dp) : 0);
      if (width < ui_size::mdi_child_minimum_width)
         width = ui_size::mdi_child_minimum_width;
      return width;
   } else if (msg == UIMessage::GET_HEIGHT) {
      UIElement* child  = el->_children.empty() ? nullptr : el->_children.back();
      int        height = ui_size::mdi_child_title + ui_size::mdi_child_border;
      height += (child ? child->message(msg, di ? (di - 2 * ui_size::mdi_child_border) : 0, dp) : 0);
      if (height < ui_size::mdi_child_minimum_height)
         height = ui_size::mdi_child_minimum_height;
      return height;
   } else if (msg == UIMessage::LAYOUT) {
      auto [titleSize, borderSize, titleRect, contentRect] =
         ui_mdi_child_calculate_layout(el->_bounds, el->_window->scale());

      int position = titleRect.r;

      for (auto child : el->_children) {
         int width = child->message(UIMessage::GET_WIDTH, 0, 0);
         child->move(UIRectangle(position - width, position, titleRect.t, titleRect.b), false);
         position -= width;
      }

      UIElement* child = el->_children.empty() ? nullptr : el->_children.back();

      if (child) {
         child->move(contentRect, false);
      }
   } else if (msg == UIMessage::GET_CURSOR) {
      int hitTest = _UIMDIChildHitTest(mdiChild, el->cursor_pos());
      if (hitTest == 0b1000)
         return (int)UICursor::resize_left;
      if (hitTest == 0b0010)
         return (int)UICursor::resize_up;
      if (hitTest == 0b0110)
         return (int)UICursor::resize_up_right;
      if (hitTest == 0b1010)
         return (int)UICursor::resize_up_left;
      if (hitTest == 0b0100)
         return (int)UICursor::resize_right;
      if (hitTest == 0b0001)
         return (int)UICursor::resize_down;
      if (hitTest == 0b1001)
         return (int)UICursor::resize_down_left;
      if (hitTest == 0b0101)
         return (int)UICursor::resize_down_right;
      return (int)UICursor::arrow;
   } else if (msg == UIMessage::LEFT_DOWN) {
      mdiChild->_drag_hit_test = _UIMDIChildHitTest(mdiChild, el->cursor_pos());
      mdiChild->_drag_offset   = el->_bounds + UIRectangle(-el->cursor_pos().x, -el->cursor_pos().y);
   } else if (msg == UIMessage::LEFT_UP) {
      if (mdiChild->_mdi_bounds.l < 0)
         mdiChild->_mdi_bounds.r -= mdiChild->_mdi_bounds.l, mdiChild->_mdi_bounds.l = 0;
      if (mdiChild->_mdi_bounds.t < 0)
         mdiChild->_mdi_bounds.b -= mdiChild->_mdi_bounds.t, mdiChild->_mdi_bounds.t = 0;
      el->_parent->refresh();
   } else if (msg == UIMessage::MOUSE_DRAG) {
      if (mdiChild->_drag_hit_test > 0) {

#define _UI_MDI_CHILD_MOVE_EDGE(bit, edge, cursor, size, opposite, negate, minimum, offset)                \
   if (mdiChild->_drag_hit_test & bit)                                                                     \
      mdiChild->_mdi_bounds.edge = mdiChild->_drag_offset.edge + pos.cursor - el->_parent->_bounds.offset; \
   if ((mdiChild->_drag_hit_test & bit) && mdiChild->_mdi_bounds.size() < minimum)                         \
      mdiChild->_mdi_bounds.edge = mdiChild->_mdi_bounds.opposite negate minimum;

         auto pos = el->cursor_pos();
         _UI_MDI_CHILD_MOVE_EDGE(0b1000, l, x, width, r, -, ui_size::mdi_child_minimum_width, l);
         _UI_MDI_CHILD_MOVE_EDGE(0b0100, r, x, width, l, +, ui_size::mdi_child_minimum_width, l);
         _UI_MDI_CHILD_MOVE_EDGE(0b0010, t, y, height, b, -, ui_size::mdi_child_minimum_height, t);
         _UI_MDI_CHILD_MOVE_EDGE(0b0001, b, y, height, t, +, ui_size::mdi_child_minimum_height, t);
         el->_parent->refresh();
      }
   } else if (msg == UIMessage::DESTROY) {
      UIMDIClient* client = (UIMDIClient*)el->_parent;
      if (client->_active == mdiChild) {
         client->_active = (UIMDIChild*)(client->_children.size() == 1
                                            ? NULL
                                            : client->_children[client->_children.size() - 2]); // todo: seems wrong
      }
   } else if (msg == UIMessage::DEALLOCATE) {
   }

   return 0;
}

int UIMDIClient::_class_message_proc(UIMessage msg, int di, void* dp) {
   UI* ui = this->ui();

   if (msg == UIMessage::PAINT) {
      if (~_flags & UIMDIClient::_TRANSPARENT) {
         UIDrawBlock((UIPainter*)dp, _bounds, ui->_theme.panel2);
      }
   } else if (msg == UIMessage::LAYOUT) {
      for (auto child : _children) {
         UIMDIChild* mdiChild = (UIMDIChild*)child;
         UI_ASSERT(mdiChild->_class_proc == UIMDIChild::_ClassMessageProc);

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
      UIMDIChild* child = (UIMDIChild*)dp;

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
   , _title(title)
   , _drag_hit_test(0)
   , _drag_offset(0) {
   UI_ASSERT(parent->_class_proc == UIMDIClient::_ClassMessageProc);
   UIMDIClient* mdiClient = (UIMDIClient*)parent;

   mdiClient->_active = this;

   if (flags & UIMDIChild::CLOSE_BUTTON) {
      add_button(UIButton::SMALL | non_client_flag, "X").on_click([this](UIButton&) { _UIMDIChildCloseButton(this); });
   }
}

UIMDIChild* UIMDIChildCreate(UIElement* parent, uint32_t flags, UIRectangle initialBounds, std::string_view title) {
   return new UIMDIChild(parent, flags, initialBounds, title);
}

UIMDIClient::UIMDIClient(UIElement* parent, uint32_t flags)
   : UIElementCast<UIMDIClient>(parent, flags, UIMDIClient::_ClassMessageProc, "MDIClient")
   , _active(nullptr)
   , _cascade(0) {}

UIMDIClient* UIMDIClientCreate(UIElement* parent, uint32_t flags) {
   return new UIMDIClient(parent, flags);
}

// --------------------------------------------------
// Image displays.
// --------------------------------------------------

UIImageDisplay& UIImageDisplay::_update_viewport() {
   UIRectangle bounds = _bounds;
   bounds.r -= bounds.l, bounds.b -= bounds.t;

   float minimumZoomX = 1, minimumZoomY = 1;
   if ((int)_width > bounds.r)
      minimumZoomX = (float)bounds.r / _width;
   if ((int)_height > bounds.b)
      minimumZoomY = (float)bounds.b / _height;
   float minimumZoom = minimumZoomX < minimumZoomY ? minimumZoomX : minimumZoomY;

   if (_zoom < minimumZoom || (_flags & UIImageDisplay::ZOOM_FIT)) {
      _zoom = minimumZoom;
      _flags |= UIImageDisplay::ZOOM_FIT;
   }

   if (_panX < 0)
      _panX = 0;
   if (_panY < 0)
      _panY = 0;
   if (_panX > _width - bounds.r / _zoom)
      _panX = _width - bounds.r / _zoom;
   if (_panY > _height - bounds.b / _zoom)
      _panY = _height - bounds.b / _zoom;

   if (bounds.r && _width * _zoom <= bounds.r)
      _panX = _width / 2 - bounds.r / _zoom / 2;
   if (bounds.b && _height * _zoom <= bounds.b)
      _panY = _height / 2 - bounds.b / _zoom / 2;
   return *this;
}

int UIImageDisplay::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_HEIGHT) {
      return _height;
   } else if (msg == UIMessage::GET_WIDTH) {
      return _width;
   } else if (msg == UIMessage::DEALLOCATE) {
      free(_bits);
   } else if (msg == UIMessage::PAINT) {
      UIPainter* painter = (UIPainter*)dp;

      int w = _bounds.width(), h = _bounds.height();
      int x = _UILinearMap(0, _panX, _panX + w / _zoom, 0, w) + _bounds.l;
      int y = _UILinearMap(0, _panY, _panY + h / _zoom, 0, h) + _bounds.t;

      UIRectangle image =
         UIRectangle(x, x + (int)(_width * _zoom), y, (int)(y + _height * _zoom));
      UIRectangle bounds = intersection(painter->_clip, intersection(_bounds, image));
      if (!bounds.valid())
         return 0;

      if (_zoom == 1) {
         uint32_t* lineStart       = (uint32_t*)painter->_bits + bounds.t * painter->_width + bounds.l;
         uint32_t* sourceLineStart = _bits + (bounds.l - image.l) + _width * (bounds.t - image.t);

         for (int i = 0; i < bounds.b - bounds.t; i++, lineStart += painter->_width, sourceLineStart += _width) {
            uint32_t* destination = lineStart;
            uint32_t* source      = sourceLineStart;
            int       j           = bounds.r - bounds.l;

            do {
               *destination = *source;
               destination++;
               source++;
            } while (--j);
         }
      } else {
         float     zr          = 1.0f / _zoom;
         uint32_t* destination = (uint32_t*)painter->_bits;

         for (int i = bounds.t; i < bounds.b; i++) {
            int ty = (i - image.t) * zr;

            for (int j = bounds.l; j < bounds.r; j++) {
               int tx                              = (j - image.l) * zr;
               destination[i * painter->_width + j] = _bits[ty * _width + tx];
            }
         }
      }
   } else if (msg == UIMessage::MOUSE_WHEEL && (_flags & UIImageDisplay::INTERACTIVE)) {
      _flags &= ~UIImageDisplay::ZOOM_FIT;
      int   divisions   = -di / 72;
      float factor      = 1;
      float perDivision = _window->_ctrl ? 2.0f : _window->_alt ? 1.01f : 1.2f;
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
   } else if (msg == UIMessage::LAYOUT && (_flags & UIImageDisplay::INTERACTIVE)) {
      UIRectangle bounds = _bounds;
      bounds.r -= bounds.l, bounds.b -= bounds.t;
      _panX -= (bounds.r - _previousWidth) / 2 / _zoom;
      _panY -= (bounds.b - _previousHeight) / 2 / _zoom;
      _previousWidth = bounds.r, _previousHeight = bounds.b;
      _update_viewport();
   } else if (msg == UIMessage::GET_CURSOR && (_flags & UIImageDisplay::INTERACTIVE) &&
              (_bounds.width() < _width * _zoom ||
               _bounds.height() < _height * _zoom)) {
      return (int)UICursor::hand;
   } else if (msg == UIMessage::MOUSE_DRAG) {
      _panX -= (cursor_pos().x - _previousPanPointX) / _zoom;
      _panY -= (cursor_pos().y - _previousPanPointY) / _zoom;
      _update_viewport();
      _previousPanPointX = cursor_pos().x;
      _previousPanPointY = cursor_pos().y;
      repaint(nullptr);
   } else if (msg == UIMessage::LEFT_DOWN) {
      _flags &= ~UIImageDisplay::ZOOM_FIT;
      _previousPanPointX = cursor_pos().x;
      _previousPanPointY = cursor_pos().y;
   }

   return 0;
}

UIImageDisplay& UIImageDisplay::set_content(uint32_t* bits, size_t w, size_t h, size_t stride) {
   free(bits);
   bits   = (uint32_t*)malloc(w * h * 4);
   _width  = w;
   _height = h;

   uint32_t* destination = bits;
   uint32_t* source      = bits;

   for (size_t row = 0; row < _height; row++, source += stride / 4) {
      for (size_t i = 0; i < _width; i++) {
         *destination++ = source[i];
      }
   }

   measurements_changed(3);
   repaint(nullptr);
   return *this;
}

UIImageDisplay::UIImageDisplay(UIElement* parent, uint32_t flags, uint32_t* bits, size_t width, size_t height,
                               size_t stride)
   : UIElementCast<UIImageDisplay>(parent, flags, UIImageDisplay::_ClassMessageProc, "ImageDisplay")
   , _previousWidth(0)
   , _previousHeight(0)
   , _previousPanPointX(0)
   , _previousPanPointY(0)
   , _bits(bits)
   , _width(width)
   , _height(height)
   , _panX(0)
   , _panY(0)
   , _zoom(1)
{
   set_content(bits, width, height, stride);
}

UIImageDisplay* UIImageDisplayCreate(UIElement* parent, uint32_t flags, uint32_t* bits, size_t width, size_t height,
                                     size_t stride) {
   return new UIImageDisplay(parent, flags, bits, width, height, stride);
}

// --------------------------------------------------
// Menus (common).
// --------------------------------------------------

bool UI::_close_menus() {
   bool anyClosed = false;

   UIWindow* window = _toplevel_windows;

   while (window) {
      if (window->_flags & UIWindow::MENU) {
         if constexpr (UIInspector::enabled())
            _inspector->notify_destroyed_window(window);

         window->destroy();
         anyClosed = true;
      }

      window = window->next();
   }

   return anyClosed;
}

int UIMenu::_MenuItemMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::CLICKED) {
      el->ui()->_close_menus();
   }

   return 0;
}

int UIMenu::_class_message_proc(UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::GET_WIDTH) {
      int width = 0;

      for (auto child : _children) {
         if (~child->_flags & UIElement::non_client_flag) {
            int w = child->message(UIMessage::GET_WIDTH, 0, 0);
            if (w > width)
               width = w;
         }
      }

      return width + 4 + ui_size::scroll_bar;
   } else if (msg == UIMessage::GET_HEIGHT) {
      int height = 0;

      for (auto child : _children) {
         if (~child->_flags & UIElement::non_client_flag) {
            height += child->message(UIMessage::GET_HEIGHT, 0, 0);
         }
      }

      return height + 4;
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, _bounds, UIControl::menu, {}, 0, _window->scale());
   } else if (msg == UIMessage::LAYOUT) {
      int position      = _bounds.t + 2 - _vscroll->position();
      int totalHeight   = 0;
      int scrollBarSize = (_flags & NO_SCROLL) ? 0 : ui_size::scroll_bar;

      for (auto child : _children) {
         if (~child->_flags & UIElement::non_client_flag) {
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
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if (m->code == UIKeycode::ESCAPE) {
         ui()->_close_menus();
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
   *width  = message(UIMessage::GET_WIDTH, 0, 0);
   *height = message(UIMessage::GET_HEIGHT, 0, 0);

   if (_flags & PLACE_ABOVE) {
      _point.y -= *height;
   }
}

UIMenu::UIMenu(UI* ui, UIElement* parent, uint32_t flags)
   : UIElementCast<UIMenu>(&ui->create_window(parent->_window, UIWindow::MENU, 0, 0, 0), flags,
                           UIMenu::_ClassMessageProc, "Menu")
   , _vscroll(UIScrollBarCreate(this, non_client_flag))
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

UIMenu& UI::create_menu(UIElement* parent, uint32_t flags) {
   return *new UIMenu(this, parent, flags);
}

// --------------------------------------------------
// Miscellaneous core functions.
// --------------------------------------------------

UIRectangle UIElement::screen_bounds() {
   int x = 0, y = 0;
   _window->get_screen_position(&x, &y);
   return _bounds + UIRectangle(x, y);
}

void UI::update() {
   UIWindow*  window = _toplevel_windows;
   UIWindow** link   = &_toplevel_windows;

   while (window) {
      UIWindow* next = window->next();

      window->message(UIMessage::WINDOW_UPDATE_START, 0, 0);
      window->message(UIMessage::WINDOW_UPDATE_BEFORE_DESTROY, 0, 0);

      if (window->_destroy()) {
         *link = next;
      } else {
         link = &window->_next;

         window->message(UIMessage::WINDOW_UPDATE_BEFORE_LAYOUT, 0, 0);
         window->move(window->_bounds, false);
         window->message(UIMessage::WINDOW_UPDATE_BEFORE_PAINT, 0, 0);

         if (window->_update_region.valid()) {
            UIPainter painter(this,
                              intersection(ui_rect_2s(window->width(), window->height()), window->_update_region));
            painter._bits   = window->bits().data();
            painter._width  = window->width();
            painter._height = window->height();

            window->paint(&painter);
            window->endpaint(&painter);
            window->_update_region = UIRectangle(0);

#ifdef UI_DEBUG
            window->_last_full_fill_count =
               (float)painter.fillCount / (window->_update_region.width() * window->_update_region.height());
#endif
         }

         window->message(UIMessage::WINDOW_UPDATE_END, 0, 0);
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
      previous->message(UIMessage::UPDATE, UIUpdate::pressed, 0);
   if (el)
      el->message(UIMessage::UPDATE, UIUpdate::pressed, 0);

   UIElement* ancestor = el;
   UIElement* child    = NULL;

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

      if ((~child->_flags & hide_flag) && child->_clip.contains(x, y)) {
         return child->find_by_point(x, y);
      }
   }

   return this;
}

bool UI::is_menu_open() const {
   UIWindow* win = this->_toplevel_windows;

   while (win) {
      if (win->_flags & UIWindow::MENU) {
         return true;
      }
      win = win->next();
   }

   return false;
}

UIElement* UIElement::next_or_previous_sibling(bool previous) {
   if (!_parent) {
      return NULL;
   }

   for (uint32_t i = 0; i < _parent->_children.size(); i++) {
      if (_parent->_children[i] == this) {
         if (previous) {
            return i > 0 ? _parent->_children[i - 1] : NULL;
         } else {
            return i < _parent->_children.size() - 1 ? _parent->_children[i + 1] : NULL;
         }
      }
   }

   UI_ASSERT(false);
   return NULL;
}

bool UIWindow::input_event(UIMessage msg, int di, void* dp) {
   bool handled = true;
   UI*  ui      = this->ui();

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
            set_pressed(NULL, 1);
         }
      } else if (msg == UIMessage::MIDDLE_UP && _pressed_button == 2) {
         _pressed->message(UIMessage::MIDDLE_UP, di, dp);
         if (ui->_quit || ui->_dialog_result)
            goto end;
         set_pressed(NULL, 2);
      } else if (msg == UIMessage::RIGHT_UP && _pressed_button == 3) {
         _pressed->message(UIMessage::RIGHT_UP, di, dp);
         if (ui->_quit || ui->_dialog_result)
            goto end;
         set_pressed(NULL, 3);
      }
   }

   if (_pressed) {
      bool inside = _pressed->_clip.contains(_cursor);

      if (inside && _hovered == _window) {
         _hovered = _pressed;
         _pressed->message(UIMessage::UPDATE, UIUpdate::hovered, 0);
      } else if (!inside && _hovered == _pressed) {
         _hovered = _window;
         _pressed->message(UIMessage::UPDATE, UIUpdate::hovered, 0);
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
      } else if (msg == UIMessage::LEFT_DOWN) {
         if ((_flags & UIWindow::MENU) || !ui->_close_menus()) {
            set_pressed(loc, 1);
            loc->message(UIMessage::LEFT_DOWN, di, dp);
         }
      } else if (msg == UIMessage::MIDDLE_DOWN) {
         if ((_flags & UIWindow::MENU) || !ui->_close_menus()) {
            set_pressed(loc, 2);
            loc->message(UIMessage::MIDDLE_DOWN, di, dp);
         }
      } else if (msg == UIMessage::RIGHT_DOWN) {
         if ((_flags & UIWindow::MENU) || !ui->_close_menus()) {
            set_pressed(loc, 3);
            loc->message(UIMessage::RIGHT_DOWN, di, dp);
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
            UIKeyTyped* m = (UIKeyTyped*)dp;

            if (m->code == UIKeycode::TAB && !_ctrl && !_alt) {
               UIElement* start = _focused ? _focused : _window;
               UIElement* el    = start;

               do {
                  if (!el->_children.empty() && !(el->_flags & (UIElement::hide_flag | UIElement::disabled_flag))) {
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
               } while (el != start && ((~el->_flags & UIElement::tab_stop_flag) ||
                                        (el->_flags & (UIElement::hide_flag | UIElement::disabled_flag))));

               if (~el->_flags & UIElement::window_flag) {
                  el->focus();
               }

               handled = true;
            } else if (!_dialog) {
               for (const auto& shortcut : views::reverse(_shortcuts)) {
                  if (shortcut.code == m->code && shortcut.ctrl == _ctrl && shortcut.shift == _shift &&
                      shortcut.alt == _alt) {
                     shortcut.invoke();
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
         previous->message(UIMessage::UPDATE, UIUpdate::hovered, 0);
         _hovered->message(UIMessage::UPDATE, UIUpdate::hovered, 0);
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
   UI *ui = this->ui();
   
   UIFont* font = ui->_active_font;
   //std_print("font = {}\n", (void *)font);

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
         font->_glyphs_offsets_x[c]  = font->_font->glyph->bitmap_left;
         font->_glyphs_offsets_y[c]  = font->_font->size->metrics.ascender / 64 - font->_font->glyph->bitmap_top;
         font->_glyphs_rendered[c] = true;
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

   UIRectangle rectangle = intersection(_clip, UIRectangle(x0, x0 + 8, y0, y0 + 16));

   const uint8_t* data = (const uint8_t*)_uiFont + c * 16;

   for (int i = rectangle.t; i < rectangle.b; i++) {
      uint32_t* bits = _bits + i * _width + rectangle.l;
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

   if (auto it = font_map.find(spec); it != font_map.end())
      return it->second.get();

   unique_ptr<UIFont> font = make_unique<UIFont>();

   font->_ui = this;
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
   font_map.emplace(std::move(spec), std::move(font));
   return f;
}

UIFont* UIFont::activate() {
   UIFont* previous  = _ui->_active_font;
   _ui->_active_font = this;
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
      if (!(child->_flags & (UIElement::destroy_flag | UIElement::hide_flag))) {
         auto [result, depth] = _UIInspectorFindNthElement(child, index);
         if (result)
            return {result, depth + 1};
      }
   }

   return {nullptr, 0};
}

int _UIInspectorTableMessage(UIElement* table, UIMessage msg, int di, void* dp) {
   UI *ui = table->ui();
   if (!ui->_inspector->_target) {
      return 0;
   }

   if (msg == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m     = (UITableGetItem*)dp;
      int             index = m->index;
      auto [el, depth]      = _UIInspectorFindNthElement(ui->_inspector->_target, &index);
      if (!el)
         return 0;

      if (m->column == 0) {
         return m->format_to("{}{}", std::string_view{"                ", depth * 2}, el->_class_name);
      } else if (m->column == 1) {
         const auto& b = el->_bounds;
         return m->format_to("{}:{}, {}:{}", b.l, b.r, b.t, b.b);
      } else if (m->column == 2) {
         return m->format_to("{}{:c}", el->_id, el->is_focused() ? '*' : ' ');
      }
   } else if (msg == UIMessage::MOUSE_MOVE) {
      int        index = ui->_inspector->_table->hittest(table->cursor_pos());
      UIElement* el    = NULL;
      if (index >= 0)
         el = _UIInspectorFindNthElement(ui->_inspector->_target, &index).first;
      UIWindow* window  = ui->_inspector->_target;
      UIPainter painter = {0};
      window->set_update_region(window->_bounds);
      painter._bits   = window->bits().data();
      painter._width  = window->width();
      painter._height = window->height();
      painter._clip   = ui_rect_2s(window->width(), window->height());

      auto& bits = window->bits();
      for (uint32_t i = 0; i < window->width() * window->height(); i++) {
         bits[i] = 0xFF00FF;
      }

      window->paint(&painter);
      painter._clip = ui_rect_2s(window->width(), window->height());

      if (el) {
         UIDrawInvert(&painter, el->_bounds);
         UIDrawInvert(&painter, el->_bounds + ui_rect_1i(4));
      }

      window->endpaint(&painter);
   }

   return 0;
}

UIInspector::UIInspector(UI* ui)
   : _ui(ui) {
   if (!_enabled)
      return;

   _inspector             = &ui->create_window(0, UIWindow::INSPECTOR, "Inspector", 0, 0);
   UISplitPane* splitPane = UISplitPaneCreate(_inspector, 0, 0.5f);
   _table                 = UITableCreate(splitPane, 0, "Class\tBounds\tID");
   _table->_user_proc     = _UIInspectorTableMessage;
   _log                   = UICodeCreate(splitPane, 0);
   _log->set_font(ui->_default_font);
}

int _UIInspectorCountElements(UIElement* el) {
   int count = 1;

   for (auto child : el->_children) {
      if (!(child->_flags & (UIElement::destroy_flag | UIElement::hide_flag))) {
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

   if (window->_flags & UIWindow::INSPECTOR) {
      return;
   }

   if (_target != window) {
      _target = window;
      window->ui()->_inspector->refresh();
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

int UIAutomationRunTests();

void UIAutomationProcessMessage() {
   int result;
   UI::_platform_message_loop_single(&result);
}

void UIAutomationKeyboardTypeSingle(intptr_t code, bool ctrl, bool shift, bool alt) {
   UIWindow*  window = ui->_toplevel_windows; // TODO Get the focused window.
   UIKeyTyped m      = {0};
   m.code            = code;
   window->ctrl      = ctrl;
   window->alt       = alt;
   window->shift     = shift;
   window->imput_event(UIMessage::KEY_TYPED, 0, &m);
   window->ctrl  = false;
   window->alt   = false;
   window->shift = false;
}

void UIAutomationKeyboardType(const char* string) {
   UIWindow* window = ui->_toplevel_windows; // TODO Get the focused window.

   UIKeyTyped m;
   char       c[2];

   c[1]   = 0;
   m.text = std::string_view{c, 1};

   for (int i = 0; string[i]; i++) {
      c[0]          = string[i];
      window->ctrl  = false;
      window->alt   = false;
      window->shift = (c[0] >= 'A' && c[0] <= 'Z');
      m.code        = (c[0] >= 'A' && c[0] <= 'Z')   ? UI_KEYCODE_LETTER(c[0])
                      : c[0] == '\n'                 ? UIKeycode::ENTER
                      : c[0] == '\t'                 ? UIKeycode::TAB
                      : c[0] == ' '                  ? UIKeycode::SPACE
                      : (c[0] >= '0' && c[0] <= '9') ? UI_KEYCODE_DIGIT(c[0])
                                                     : 0;
      window->input_event(UIMessage::KEY_TYPED, 0, &m);
   }

   window->ctrl  = false;
   window->alt   = false;
   window->shift = false;
}

bool UIAutomationCheckCodeLineMatches(UICode* code, int lineIndex, const char* input) {
   if (lineIndex < 1 || lineIndex > code->lineCount)
      return false;
   int bytes = 0;
   for (int i = 0; input[i]; i++)
      bytes++;
   if (bytes != code->lines[lineIndex - 1].bytes)
      return false;
   for (int i = 0; input[i]; i++)
      if (code->content[code->lines[lineIndex - 1].offset + i] != input[i])
         return false;
   return true;
}

bool UIAutomationCheckTableItemMatches(UITable* table, int row, int column, const char* input) {
   int bytes = 0;
   for (int i = 0; input[i]; i++)
      bytes++;
   if (row < 0 || row >= table->num_items())
      return false;
   if (column < 0 || column >= table->columnCount)
      return false;
   UITableGetItem m(bytes + 1);
   m.column   = column;
   m.index    = row;
   int length = &table->e->message(UIMessage::TABLE_GET_ITEM, 0, &m);
   if (length != bytes)
      return false;
   auto buffer = m.buff(length);
   for (int i = 0; input[i]; i++)
      if (buffer[i] != input[i])
         return false;
   return true;
}

#endif // UI_AUTOMATION_TESTS

// --------------------------------------------------
// Common platform layer functionality.
// --------------------------------------------------

void UI::_initialize_common(const UIConfig& cfg, const std::string& default_font_path) {
   _theme = uiThemeClassic;

#ifdef UI_FREETYPE
   FT_Init_FreeType(&_ft);
#endif

   _default_font = create_font(default_font_path, cfg.default_font_size);
   _default_font->activate();
}

void UIWindow::_init_toplevel() {
   set_scale(1.0f);
   _window = this;
   set_hovered(this);
   set_next(ui()->_toplevel_windows);
   ui()->_toplevel_windows = this;
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
#ifdef UI_AUTOMATION_TESTS
   return UIAutomationRunTests();
#else
   int result = 0;
   while (!_quit && _platform_message_loop_single(&result))
      _dialog_result = NULL;
   return result;
#endif
}

UIWindow::UIWindow(UI* ui, UIElement* parent, uint32_t flags, message_proc_t message_proc, const char* cClassName)
   : UIElementCast<UIWindow>(parent, flags, message_proc, cClassName)
   , _dialog(nullptr)
   , _scale(0)
   , _width(0)
   , _height(0)
   , _next(nullptr)
   , _hovered(nullptr)
   , _pressed(nullptr)
   , _focused(nullptr)
   , _dialog_old_focus(nullptr)
   , _pressed_button(0)
   , _cursor_style(0)
   , _textbox_modified_flag(false)
   , _update_region(0)
   , _ui(ui)
   , _ctrl(false)
   , _shift(false)
   , _alt(false) {}

UIWindow::~UIWindow() {}

// --------------------------------------------------
// Platform layers.
// --------------------------------------------------

#ifdef UI_LINUX

int UI::_platform_message_proc(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::DEALLOCATE) {
      UIWindow* window     = (UIWindow*)el;
      window->_image->data = NULL;
      XDestroyImage(window->_image);
      XDestroyIC(window->_xic);
      XDestroyWindow(window->_ui->_display, window->_xwindow);
      return 0;
   }

   return UIWindow::_ClassMessageProcCommon(el, msg, di, dp);
}

UIWindow& UI::_platform_create_window(UIWindow* owner, uint32_t flags, const char* cTitle, int _width, int _height) {
   UI* ui = this;
   ui->_close_menus();

   UIWindow* window = new UIWindow(ui, NULL, flags | UIElement::window_flag, UI::_platform_message_proc, "Window");
   window->_init_toplevel();
   if (owner)
      window->set_scale(owner->_scale);

   int width  = (flags & UIWindow::MENU) ? 1 : _width ? _width : 800;
   int height = (flags & UIWindow::MENU) ? 1 : _height ? _height : 600;

   XSetWindowAttributes attributes = {};
   attributes.override_redirect    = flags & UIWindow::MENU;

   window->_xwindow = XCreateWindow(ui->_display, DefaultRootWindow(ui->_display), 0, 0, width, height, 0, 0, InputOutput,
                                    CopyFromParent, CWOverrideRedirect, &attributes);
   if (cTitle)
      XStoreName(ui->_display, window->_xwindow, cTitle);
   XSelectInput(ui->_display, window->_xwindow,
                SubstructureNotifyMask | ExposureMask | PointerMotionMask | ButtonPressMask | ButtonReleaseMask |
                   KeyPressMask | KeyReleaseMask | StructureNotifyMask | EnterWindowMask | LeaveWindowMask |
                   ButtonMotionMask | KeymapStateMask | FocusChangeMask | PropertyChangeMask);

   if (flags & UIWindow::MAXIMIZE) {
      Atom atoms[2] = {XInternAtom(ui->_display, "_NET_WM_STATE_MAXIMIZED_HORZ", 0),
                       XInternAtom(ui->_display, "_NET_WM_STATE_MAXIMIZED_VERT", 0)};
      XChangeProperty(ui->_display, window->_xwindow, XInternAtom(ui->_display, "_NET_WM_STATE", 0), XA_ATOM, 32,
                      PropModeReplace, (unsigned char*)atoms, 2);
   }

   if (~flags & UIWindow::MENU) {
      XMapRaised(ui->_display, window->_xwindow);
   }

   if (flags & UIWindow::CENTER_IN_OWNER) {
      int x = 0, y = 0;
      owner->get_screen_position(&x, &y);
      XMoveResizeWindow(ui->_display, window->_xwindow, x + owner->width() / 2 - width / 2,
                        y + owner->height() / 2 - height / 2, width, height);
   }

   XSetWMProtocols(ui->_display, window->_xwindow, &ui->windowClosedID, 1);
   window->_image = XCreateImage(ui->_display, ui->_visual, 24, ZPixmap, 0, NULL, 10, 10, 32, 0);

   window->_xic = XCreateIC(ui->_xim, XNInputStyle, XIMPreeditNothing | XIMStatusNothing, XNClientWindow,
                            window->_xwindow, XNFocusWindow, window->_xwindow, nullptr);

   int dndVersion = 4;
   XChangeProperty(ui->_display, window->_xwindow, ui->dndAwareID, XA_ATOM, 32 /* bits */, PropModeReplace,
                   (uint8_t*)&dndVersion, 1);

   return *window;
}

UIWindow* _UIFindWindow(UI *ui, Window window) {
   UIWindow* w = ui->_toplevel_windows;
   while (w) {
      if (w->_xwindow == window) {
         return w;
      }
      w = w->next();
   }
   return NULL;
}

void UIWindow::write_clipboard_text(std::string_view text, sel_target_t t) {
   _ui->_paste_text = text;
   Atom atom      = (t == sel_target_t::clipboard) ? _ui->clipboardID : _ui->primaryID;
   XSetSelectionOwner(_ui->_display, atom, _xwindow, 0);
}

std::string UIWindow::read_clipboard_text(sel_target_t t) {
   Atom atom = (t == sel_target_t::clipboard) ? _ui->clipboardID : _ui->primaryID;

   Window clipboardOwner = XGetSelectionOwner(_ui->_display, atom);

   if (clipboardOwner == None) {
      return {};
   }

   if (_UIFindWindow(ui(), clipboardOwner)) {
      return _ui->_paste_text;
   }

   XConvertSelection(_ui->_display, atom, XA_STRING, _ui->xSelectionDataID, _xwindow, CurrentTime);
   XSync(_ui->_display, 0);
   XNextEvent(_ui->_display, &_ui->_copy_event);

   // Hack to get around the fact that PropertyNotify arrives before SelectionNotify.
   // We need PropertyNotify for incremental transfers.
   while (_ui->_copy_event.type == PropertyNotify) {
      XNextEvent(_ui->_display, &_ui->_copy_event);
   }

   if (_ui->_copy_event.type == SelectionNotify && _ui->_copy_event.xselection.selection == atom &&
       _ui->_copy_event.xselection.property) {
      Atom target;
      // This `itemAmount` is actually `bytes_after_return`
      unsigned long size, itemAmount;
      char*         data;
      int           format;
      XGetWindowProperty(_ui->_copy_event.xselection.display, _ui->_copy_event.xselection.requestor,
                         _ui->_copy_event.xselection.property, 0L, ~0L, 0, AnyPropertyType, &target, &format, &size,
                         &itemAmount, (unsigned char**)&data);

      // non incremental transfer
      // ------------------------
      if (target != _ui->incrID) {
         std::string res;
         res.resize(size);
         memcpy(res.data(), data, size);
         XFree(data);
         XDeleteProperty(_ui->_copy_event.xselection.display, _ui->_copy_event.xselection.requestor,
                         _ui->_copy_event.xselection.property);
         return res;
      }

      // incremental transfer
      // --------------------
      XFree(data);
      XDeleteProperty(_ui->_display, _ui->_copy_event.xselection.requestor, _ui->_copy_event.xselection.property);
      XSync(_ui->_display, 0);

      size = 0;
      std::string res;

      while (true) {
         // TODO Timeout.
         XNextEvent(_ui->_display, &_ui->_copy_event);

         if (_ui->_copy_event.type == PropertyNotify) {
            // The other case - PropertyDelete would be caused by us and can be ignored
            if (_ui->_copy_event.xproperty.state == PropertyNewValue) {
               unsigned long chunkSize;

               // Note that this call deletes the property.
               XGetWindowProperty(_ui->_display, _ui->_copy_event.xproperty.window, _ui->_copy_event.xproperty.atom, 0L, ~0L,
                                  True, AnyPropertyType, &target, &format, &chunkSize, &itemAmount,
                                  (unsigned char**)&data);

               if (chunkSize == 0) {
                  return res;
               } else {
                  res.resize(size + chunkSize);
                  memcpy(res.data() + size, data, chunkSize);
                  size += chunkSize;
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

unique_ptr<UI> UI::initialise(const UIConfig& cfg) {
   unique_ptr<UI> ui( new UI);
   
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
         font_path.resize(PATH_MAX + 1);
         auto cnt = fread(&font_path[0], 1, PATH_MAX, f);
         font_path.resize(cnt);
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

   ui->_display = XOpenDisplay(NULL);
   ui->_visual  = XDefaultVisual(ui->_display, 0);

   ui->windowClosedID   = XInternAtom(ui->_display, "WM_DELETE_WINDOW", 0);
   ui->primaryID        = XInternAtom(ui->_display, "PRIMARY", 0);
   ui->dndEnterID       = XInternAtom(ui->_display, "XdndEnter", 0);
   ui->dndPositionID    = XInternAtom(ui->_display, "XdndPosition", 0);
   ui->dndStatusID      = XInternAtom(ui->_display, "XdndStatus", 0);
   ui->dndActionCopyID  = XInternAtom(ui->_display, "XdndActionCopy", 0);
   ui->dndDropID        = XInternAtom(ui->_display, "XdndDrop", 0);
   ui->dndSelectionID   = XInternAtom(ui->_display, "XdndSelection", 0);
   ui->dndFinishedID    = XInternAtom(ui->_display, "XdndFinished", 0);
   ui->dndAwareID       = XInternAtom(ui->_display, "XdndAware", 0);
   ui->uriListID        = XInternAtom(ui->_display, "text/uri-list", 0);
   ui->plainTextID      = XInternAtom(ui->_display, "text/plain", 0);
   ui->clipboardID      = XInternAtom(ui->_display, "CLIPBOARD", 0);
   ui->xSelectionDataID = XInternAtom(ui->_display, "XSEL_DATA", 0);
   ui->textID           = XInternAtom(ui->_display, "TEXT", 0);
   ui->targetID         = XInternAtom(ui->_display, "TARGETS", 0);
   ui->incrID           = XInternAtom(ui->_display, "INCR", 0);

   ui->_cursors[(uint32_t)UICursor::arrow]             = XCreateFontCursor(ui->_display, XC_left_ptr);
   ui->_cursors[(uint32_t)UICursor::text]              = XCreateFontCursor(ui->_display, XC_xterm);
   ui->_cursors[(uint32_t)UICursor::split_v]           = XCreateFontCursor(ui->_display, XC_sb_v_double_arrow);
   ui->_cursors[(uint32_t)UICursor::split_h]           = XCreateFontCursor(ui->_display, XC_sb_h_double_arrow);
   ui->_cursors[(uint32_t)UICursor::flipped_arrow]     = XCreateFontCursor(ui->_display, XC_right_ptr);
   ui->_cursors[(uint32_t)UICursor::cross_hair]        = XCreateFontCursor(ui->_display, XC_crosshair);
   ui->_cursors[(uint32_t)UICursor::hand]              = XCreateFontCursor(ui->_display, XC_hand1);
   ui->_cursors[(uint32_t)UICursor::resize_up]         = XCreateFontCursor(ui->_display, XC_top_side);
   ui->_cursors[(uint32_t)UICursor::resize_left]       = XCreateFontCursor(ui->_display, XC_left_side);
   ui->_cursors[(uint32_t)UICursor::resize_up_right]   = XCreateFontCursor(ui->_display, XC_top_right_corner);
   ui->_cursors[(uint32_t)UICursor::resize_up_left]    = XCreateFontCursor(ui->_display, XC_top_left_corner);
   ui->_cursors[(uint32_t)UICursor::resize_down]       = XCreateFontCursor(ui->_display, XC_bottom_side);
   ui->_cursors[(uint32_t)UICursor::resize_right]      = XCreateFontCursor(ui->_display, XC_right_side);
   ui->_cursors[(uint32_t)UICursor::resize_down_left]  = XCreateFontCursor(ui->_display, XC_bottom_left_corner);
   ui->_cursors[(uint32_t)UICursor::resize_down_right] = XCreateFontCursor(ui->_display, XC_bottom_right_corner);

   XSetLocaleModifiers("");

   ui->_xim = XOpenIM(ui->_display, 0, 0, 0);

   if (!ui->_xim) {
      XSetLocaleModifiers("@im=none");
      ui->_xim = XOpenIM(ui->_display, 0, 0, 0);
   }

   ui->_inspector.reset(new UIInspector(ui.get()));

   return ui;
}

UIWindow& UIWindow::set_cursor(int cursor) {
   XDefineCursor(_ui->_display, _xwindow, _ui->_cursors[cursor]);
   return *this;
}

#if 0
Display* _UIX11GetDisplay() {
   return ui->_display;
}

void _UIX11ResetCursor(UIWindow* window) {
   XDefineCursor(window->ui()->_display, window->_xwindow, window->ui()->_cursors[(uint32_t)UICursor::arrow]);
}

void UIWindowPack(UIWindow* window, int _width) {
   int width  = _width ? _width : window->_children[0]->message(UIMessage::GET_WIDTH, 0, 0);
   int height = window->_children[0]->message(UIMessage::GET_HEIGHT, width, 0);
   XResizeWindow(window->ui()->_display, window->_xwindow, width, height);
}

#endif

void UIWindow::endpaint(UIPainter* painter) const{
   (void)painter;
   const auto& ur = _window->_update_region;
   XPutImage(_ui->_display, _window->_xwindow, DefaultGC(_ui->_display, 0), _window->_image, ur.l, ur.t, ur.l, ur.t,
             UI_RECT_SIZE(_window->_update_region));
}

void UIWindow::get_screen_position(int* _x, int* _y) const {
   Window child;
   XTranslateCoordinates(_ui->_display, _window->_xwindow, DefaultRootWindow(_ui->_display), 0, 0, _x, _y, &child);
}

UIMenu& UIMenu::show() {
   UI*    ui = this->ui();
   Window child;

   // Find the screen that contains the point the menu was created at.
   Screen* menuScreen = NULL;
   int     screenX, screenY;

   for (int i = 0; i < ScreenCount(ui->_display); i++) {
      Screen* screen = ScreenOfDisplay(ui->_display, i);
      int     x, y;
      XTranslateCoordinates(ui->_display, screen->root, DefaultRootWindow(ui->_display), 0, 0, &x, &y, &child);

      if (_point.x >= x && _point.x < x + screen->width && _point.y >= y && _point.y < y + screen->height) {
         menuScreen = screen;
         screenX = x, screenY = y;
         break;
      }
   }

   int width, height;
   _prepare(&width, &height);

   {
      // Clamp the menu to the bounds of the window.
      // This step shouldn't be necessary with the screen clamping below, but there are some buggy X11 drivers that
      // report screen sizes incorrectly.
      int       wx, wy;
      UIWindow* parentWindow = this->_parent_window;
      XTranslateCoordinates(ui->_display, parentWindow->_xwindow, DefaultRootWindow(ui->_display), 0, 0, &wx, &wy,
                            &child);
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
      XInternAtom(ui->_display, "_NET_WM_WINDOW_TYPE", true),
      XInternAtom(ui->_display, "_NET_WM_WINDOW_TYPE_DROPDOWN_MENU", true),
      XInternAtom(ui->_display, "_MOTIF_WM_HINTS", true),
   };

   XChangeProperty(ui->_display, _window->_xwindow, properties[0], XA_ATOM, 32, PropModeReplace, (uint8_t*)properties,
                   2);
   XSetTransientForHint(ui->_display, _window->_xwindow, DefaultRootWindow(ui->_display));

   struct Hints {
      int flags;
      int functions;
      int decorations;
      int inputMode;
      int status;
   };

   struct Hints hints = {0};
   hints.flags        = 2;
   XChangeProperty(ui->_display, _window->_xwindow, properties[2], properties[2], 32, PropModeReplace, (uint8_t*)&hints,
                   5);

   XMapWindow(ui->_display, _window->_xwindow);
   XMoveResizeWindow(ui->_display, _window->_xwindow, _point.x, _point.y, width, height);
   return *this;
}

// return true if we should exit, normally return false
// ----------------------------------------------------
bool UI::_process_x11_event(void* x_event) {
   XEvent* event = (XEvent*)x_event;
   if (event->type == ClientMessage && (Atom)event->xclient.data.l[0] == windowClosedID) {
      UIWindow* window = _UIFindWindow(this, event->xclient.window);
      if (!window)
         return false;
      bool exit = !window->message(UIMessage::WINDOW_CLOSE, 0, 0);
      if (exit)
         return true;
      update();
      return false;
   } else if (event->type == Expose) {
      UIWindow* window = _UIFindWindow(this, event->xexpose.window);
      if (!window)
         return false;
      XPutImage(_display, window->_xwindow, DefaultGC(_display, 0), window->_image, 0, 0, 0, 0, window->width(),
                window->height());
   } else if (event->type == ConfigureNotify) {
      UIWindow* window = _UIFindWindow(this, event->xconfigure.window);
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
      UIWindow* window = _UIFindWindow(this, event->xmotion.window);
      if (!window)
         return false;
      window->set_cursor_pos({event->xmotion.x, event->xmotion.y});
      window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
   } else if (event->type == LeaveNotify) {
      UIWindow* window = _UIFindWindow(this, event->xcrossing.window);
      if (!window)
         return false;

      if (!window->pressed()) {
         window->set_cursor_pos({-1, -1});
      }

      window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
   } else if (event->type == ButtonPress || event->type == ButtonRelease) {
      UIWindow* window = _UIFindWindow(this, event->xbutton.window);
      if (!window)
         return false;
      window->set_cursor_pos({event->xbutton.x, event->xbutton.y});

      if (event->xbutton.button >= 1 && event->xbutton.button <= 3) {
         window->input_event(
            (UIMessage)((uint32_t)(event->type == ButtonPress ? UIMessage::LEFT_DOWN : UIMessage::LEFT_UP) +
                        event->xbutton.button * 2 - 2),
            0, 0);
      } else if (event->xbutton.button == 4) {
         window->input_event(UIMessage::MOUSE_WHEEL, -72, 0);
      } else if (event->xbutton.button == 5) {
         window->input_event(UIMessage::MOUSE_WHEEL, 72, 0);
      }

      if constexpr (UIInspector::enabled())
         _inspector->set_focused_window(window);
   } else if (event->type == KeyPress) {
      UIWindow* window = _UIFindWindow(this, event->xkey.window);
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
         Status status;
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
         }

         window->input_event(UIMessage::KEY_TYPED, 0, &m);
      }
   } else if (event->type == KeyRelease) {
      UIWindow* window = _UIFindWindow(this, event->xkey.window);
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
         Status     status;
         UIKeyTyped m;
         auto       sz = Xutf8LookupString(window->_xic, &event->xkey, text, sizeof(text) - 1, &symbol, &status);
         m.text        = {text, static_cast<size_t>(sz)};
         m.code        = (UIKeycode)XLookupKeysym(&event->xkey, 0);
         window->input_event(UIMessage::KEY_RELEASED, 0, &m);
      }
   } else if (event->type == FocusIn) {
      UIWindow* window = _UIFindWindow(this, event->xfocus.window);
      if (!window)
         return false;
      window->_ctrl = window->_shift = window->_alt = false;
      window->message(UIMessage::WINDOW_ACTIVATE, 0, 0);
   } else if (event->type == FocusOut || event->type == ResizeRequest) {
      _close_menus();
      update();
   } else if (event->type == ClientMessage && event->xclient.message_type == dndEnterID) {
      UIWindow* window = _UIFindWindow(this, event->xclient.window);
      if (!window)
         return false;
      window->_drag_source = (Window)event->xclient.data.l[0];
   } else if (event->type == ClientMessage && event->xclient.message_type == dndPositionID) {
      UIWindow* window = _UIFindWindow(this, event->xclient.window);
      if (!window)
         return false;
      XClientMessageEvent m = {0};
      m.type                = ClientMessage;
      m.display             = event->xclient.display;
      m.window              = (Window)event->xclient.data.l[0];
      m.message_type        = dndStatusID;
      m.format              = 32;
      m.data.l[0]           = window->_xwindow;
      m.data.l[1]           = true;
      m.data.l[4]           = dndActionCopyID;
      XSendEvent(_display, m.window, False, NoEventMask, (XEvent*)&m);
      XFlush(_display);
   } else if (event->type == ClientMessage && event->xclient.message_type == dndDropID) {
      UIWindow* window = _UIFindWindow(this, event->xclient.window);
      if (!window)
         return false;

      // TODO Dropping text.

      if (!XConvertSelection(_display, dndSelectionID, uriListID, primaryID, window->_xwindow,
                             event->xclient.data.l[2])) {
         XClientMessageEvent m = {0};
         m.type                = ClientMessage;
         m.display             = _display;
         m.window              = window->_drag_source;
         m.message_type        = dndFinishedID;
         m.format              = 32;
         m.data.l[0]           = window->_xwindow;
         m.data.l[1]           = 0;
         m.data.l[2]           = dndActionCopyID;
         XSendEvent(_display, m.window, False, NoEventMask, (XEvent*)&m);
         XFlush(_display);
      }
   } else if (event->type == SelectionNotify) {
      UIWindow* window = _UIFindWindow(this, event->xselection.requestor);
      if (!window)
         return false;
      if (!window->_drag_source)
         return false;

      Atom          type   = None;
      int           format = 0;
      unsigned long count = 0, bytesLeft = 0;
      uint8_t*      data = NULL;
      XGetWindowProperty(_display, window->_xwindow, primaryID, 0, 65536, False, AnyPropertyType, &type, &format,
                         &count, &bytesLeft, &data);

      if (format == 8 /* bits per character */) {
         if (event->xselection.target == uriListID) {
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
                     s[j] = strtol(n, NULL, 16);
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

            window->message(UIMessage::WINDOW_DROP_FILES, fileCount, files);

            free(files);
            free(copy);
         } else if (event->xselection.target == plainTextID) {
            // TODO.
         }
      }

      XFree(data);

      XClientMessageEvent m = {0};
      m.type                = ClientMessage;
      m.display             = _display;
      m.window              = window->_drag_source;
      m.message_type        = dndFinishedID;
      m.format              = 32;
      m.data.l[0]           = window->_xwindow;
      m.data.l[1]           = true;
      m.data.l[2]           = dndActionCopyID;
      XSendEvent(_display, m.window, False, NoEventMask, (XEvent*)&m);
      XFlush(_display);

      window->_drag_source = 0; // Drag complete.
      update();
   } else if (event->type == SelectionRequest) {
      UIWindow* window = _UIFindWindow(this, event->xclient.window);
      if (!window)
         return false;

      if ((XGetSelectionOwner(_display, clipboardID) == window->_xwindow) &&
          (event->xselectionrequest.selection == clipboardID)) {
         XSelectionRequestEvent requestEvent = event->xselectionrequest;
         Atom                   utf8ID       = XInternAtom(_display, "UTF8_STRING", 1);
         if (utf8ID == None)
            utf8ID = XA_STRING;

         Atom type                = requestEvent.target;
         type                     = (type == textID) ? XA_STRING : type;
         int changePropertyResult = 0;

         if (requestEvent.target == XA_STRING || requestEvent.target == textID || requestEvent.target == utf8ID) {
            changePropertyResult =
               XChangeProperty(requestEvent.display, requestEvent.requestor, requestEvent.property, type, 8,
                               PropModeReplace, (const unsigned char*)_paste_text.c_str(), _paste_text.size());
         } else if (requestEvent.target == targetID) {
            changePropertyResult = XChangeProperty(requestEvent.display, requestEvent.requestor, requestEvent.property,
                                                   XA_ATOM, 32, PropModeReplace, (unsigned char*)&utf8ID, 1);
         }

         if (changePropertyResult == 0 || changePropertyResult == 1) {
            XSelectionEvent sendEvent = {.type       = SelectionNotify,
                                         .serial     = requestEvent.serial,
                                         .send_event = requestEvent.send_event,
                                         .display    = requestEvent.display,
                                         .requestor  = requestEvent.requestor,
                                         .selection  = requestEvent.selection,
                                         .target     = requestEvent.target,
                                         .property   = requestEvent.property,
                                         .time       = requestEvent.time};

            XSendEvent(_display, requestEvent.requestor, 0, 0, (XEvent*)&sendEvent);
         }
      }
   }

   return false;
}

// return true if events processed without problem, false otherwise
// ----------------------------------------------------------------
bool UI::_platform_message_loop_single(int* result) {
   XEvent events[64];

   if (!_animating.empty()) {
      if (XPending(_display)) {
         XNextEvent(_display, events + 0);
      } else {
         process_animations();
         return true;
      }
   } else {
      XNextEvent(_display, events + 0);
   }

   int cur_idx = 1;

   auto merge_events = [&](int a, int last_seen_idx) {
      if (events[cur_idx].type == a) {
         if (last_seen_idx != -1)
            events[last_seen_idx].type = 0;
         last_seen_idx = cur_idx;
      }
   };

   int configureIndex = -1, motionIndex = -1, exposeIndex = -1;

   while (cur_idx < 64 && XPending(_display)) {
      XNextEvent(_display, events + cur_idx);

      merge_events(ConfigureNotify, configureIndex);
      merge_events(MotionNotify, motionIndex);
      merge_events(Expose, exposeIndex);

      ++cur_idx;
   }

   for (int i = 0; i < cur_idx; i++) {
      if (!events[i].type) {
         continue;
      }

      if (_process_x11_event(events + i)) {
         return false;
      }
   }

   return true;
}

void UIWindow::post_message(UIMessage msg, void* _dp) const {
   // HACK! Xlib doesn't seem to have a nice way to do this,
   // so send a specially crafted key press event instead.
   // TODO Maybe ClientMessage is what this should use?
   Display* dpy = ui()->_display;

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
   XStoreName(ui()->_display, _xwindow, name.data());
   return *this;
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

LRESULT CALLBACK _UIWindowProcedure(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
   UIWindow* window = (UIWindow*)GetWindowLongPtr(hwnd, GWLP_USERDATA);

   if (!window) {
      return DefWindowProc(hwnd, msg, wParam, lParam);
   }

   if (msg == WM_CLOSE) {
      if (window->message(UIMessage::WINDOW_CLOSE, 0, 0)) {
          window->ui()->update();
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
      window->ui()->update();
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
      ::SetCursor( window->ui()->_cursors[window->cursor_style()]);
      return 1;
   } else if (msg == WM_SETFOCUS || msg == WM_KILLFOCUS) {
      window->ui()->_close_menus();

      if (msg == WM_SETFOCUS) {
          window->ui()->_inspector->set_focused_window(window);
         window->message(UIMessage::WINDOW_ACTIVATE, 0, 0);
      }
   } else if (msg == WM_MOUSEACTIVATE && (window->_flags & UIWindow::MENU)) {
      return MA_NOACTIVATE;
   } else if (msg == WM_DROPFILES) {
      HDROP  drop  = (HDROP)wParam;
      int    count = DragQueryFile(drop, 0xFFFFFFFF, NULL, 0);
      char** files = (char**)malloc(sizeof(char*) * count);

      for (int i = 0; i < count; i++) {
         int length       = DragQueryFile(drop, i, NULL, 0);
         files[i]         = (char*)malloc(length + 1);
         files[i][length] = 0;
         DragQueryFile(drop, i, files[i], length + 1);
      }

      window->message(UIMessage::WINDOW_DROP_FILES, count, files);
      for (int i = 0; i < count; i++)
         free(files[i]);
      free(files);
      DragFinish(drop);
       window->ui()->update();
   } else if (msg == WM_APP + 1) {
      window->message((UIMessage)wParam, 0, (void*)lParam);
       window->ui()->update();
   } else {
      if (msg == WM_NCLBUTTONDOWN || msg == WM_NCMBUTTONDOWN || msg == WM_NCRBUTTONDOWN) {
         if (~window->_flags & UIWindow::MENU) {
             window->ui()->_close_menus();
             window->ui()->update();
         }
      }

      return DefWindowProc(hwnd, msg, wParam, lParam);
   }

   return 0;
}

unique_ptr<UI> UI::initialise(const UIConfig& cfg) {
   std::unique_ptr<UI> ui(new UI);

   std::string font_path = cfg.font_path;
   if (font_path.empty())
      font_path = _UI_TO_STRING_2(UI_FONT_PATH);

   ui->_default_font_path = font_path;
   ui->_initialize_common(cfg, font_path);

   ui->_cursors[(uint32_t)UICursor::arrow]             = LoadCursor(NULL, IDC_ARROW);
   ui->_cursors[(uint32_t)UICursor::text]              = LoadCursor(NULL, IDC_IBEAM);
   ui->_cursors[(uint32_t)UICursor::split_v]           = LoadCursor(NULL, IDC_SIZENS);
   ui->_cursors[(uint32_t)UICursor::split_h]           = LoadCursor(NULL, IDC_SIZEWE);
   ui->_cursors[(uint32_t)UICursor::flipped_arrow]     = LoadCursor(NULL, IDC_ARROW);
   ui->_cursors[(uint32_t)UICursor::cross_hair]        = LoadCursor(NULL, IDC_CROSS);
   ui->_cursors[(uint32_t)UICursor::hand]              = LoadCursor(NULL, IDC_HAND);
   ui->_cursors[(uint32_t)UICursor::resize_up]         = LoadCursor(NULL, IDC_SIZENS);
   ui->_cursors[(uint32_t)UICursor::resize_left]       = LoadCursor(NULL, IDC_SIZEWE);
   ui->_cursors[(uint32_t)UICursor::resize_up_right]   = LoadCursor(NULL, IDC_SIZENESW);
   ui->_cursors[(uint32_t)UICursor::resize_up_left]    = LoadCursor(NULL, IDC_SIZENWSE);
   ui->_cursors[(uint32_t)UICursor::resize_down]       = LoadCursor(NULL, IDC_SIZENS);
   ui->_cursors[(uint32_t)UICursor::resize_right]      = LoadCursor(NULL, IDC_SIZEWE);
   ui->_cursors[(uint32_t)UICursor::resize_down_left]  = LoadCursor(NULL, IDC_SIZENESW);
   ui->_cursors[(uint32_t)UICursor::resize_down_right] = LoadCursor(NULL, IDC_SIZENWSE);

   WNDCLASS windowClass      = {0};
   windowClass.lpfnWndProc   = _UIWindowProcedure;
   windowClass.lpszClassName = "normal";
   RegisterClass(&windowClass);
   windowClass.style |= CS_DROPSHADOW;
   windowClass.lpszClassName = "shadow";
   RegisterClass(&windowClass);

   ui->_inspector.reset(new UIInspector(ui.get()));

   return ui;
}

bool UI::_platform_message_loop_single(int* result) {
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
   _close_menus();

   UIWindow* window = new UIWindow(ui, NULL, flags | UIElement::window_flag, UI::_platform_message_proc, "Window");
   window->_init_toplevel();
   if (owner)
       window->set_scale(owner->_scale);

   if (flags & UIWindow::MENU) {
      UI_ASSERT(owner);

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
   ::SetCursor(ui()->_cursors[cursor]);
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

void UIWindow::write_clipboard_text(std::string_view text, sel_target_t) {
   if (OpenClipboard(_hwnd)) {
      EmptyClipboard();
      HGLOBAL memory = GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT, text.size() + 1);
      char*   copy   = (char*)GlobalLock(memory);
      std::memcpy(copy, text.data(), text.size());
      GlobalUnlock(copy);
      SetClipboardData(CF_TEXT, memory);
      CloseClipboard();
   }
}

std::string UIWindow::read_clipboard_text(sel_target_t) {
   std::string res;

   if (!OpenClipboard(_hwnd)) {
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

   res.resize(byteCount);
   for (uintptr_t i = 0; i < byteCount; i++)
      res[i] = buffer[i];

   GlobalUnlock(memory);
   CloseClipboard();

   return res;
}

#endif // UI_WINDOWS
