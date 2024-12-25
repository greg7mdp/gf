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
// Global variables.
// --------------------------------------------------
UI* ui = nullptr; // global pointer to the UIInitialise return value

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
   return (((_flags & disabled_flag) ? UIControl::state_disabled : 0) |
           ((_window->_hovered == this) ? UIControl::state_hovered : 0) |
           ((_window->_focused == this) ? UIControl::state_focused : 0) |
           ((_window->_pressed == this) ? UIControl::state_pressed : 0));
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

ptrdiff_t _UIStringLength(const char* cString) {
   if (!cString)
      return 0;
   ptrdiff_t length;
   for (length = 0; cString[length]; length++)
      ;
   return length;
}

char* UIStringCopy(const char* in, ptrdiff_t inBytes) {
   if (inBytes == -1) {
      inBytes = _UIStringLength(in);
   }

   char* buffer = (char*)malloc(inBytes + 1);

   for (intptr_t i = 0; i < inBytes; i++) {
      buffer[i] = in[i];
   }

   buffer[inBytes] = 0;
   return buffer;
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
   if (stop) {
      if (auto it = std::ranges::find(ui->animating, this); it != ui->animating.end()) {
         ui->animating.erase(it);
         return true;
      }
      return false;
   } else {
      if (auto it = std::ranges::find(ui->animating, this); it != ui->animating.end())
         return true;

      ui->animating.push_back(this);
      UI_ASSERT(~_flags & destroy_flag);
      return true;
   }
}

uint64_t UIAnimateClock() {
   return (uint64_t)UI_CLOCK() * 1000 / UI_CLOCKS_PER_SECOND;
}

void UI::ProcessAnimations() {
   bool update = !ui->animating.empty();

   for (auto el : ui->animating)
      el->message(UIMessage::ANIMATE, 0, 0);

   if (update) {
      UI::Update();
   }
}

// --------------------------------------------------
// Rendering.
// --------------------------------------------------

void UIDrawBlock(UIPainter* painter, UIRectangle rectangle, uint32_t color) {
   rectangle = intersection(painter->clip, rectangle);

   if (!rectangle.valid()) {
      return;
   }

#ifdef UI_SSE2
   __m128i color4 = _mm_set_epi32(color, color, color, color);
#endif

   for (int line = rectangle.t; line < rectangle.b; line++) {
      uint32_t* bits  = painter->bits + line * painter->width + rectangle.l;
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

   UIRectangle c = painter->clip;
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

   uint32_t* bits = painter->bits + y0 * painter->width + x0;

   if (dy * dy < dx * dx) {
      int m = 2 * dy - dx;

      for (int i = 0; i < dx; i++, bits += dxs) {
         *bits = color;
         if (m > 0)
            bits += painter->width, m -= 2 * dx;
         m += 2 * dy;
      }
   } else {
      int m = 2 * dx - dy;

      for (int i = 0; i < dy; i++, bits += painter->width) {
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
         if (py >= painter->clip.t && py < painter->clip.b) {
            for (int s = 0; s <= ix || s <= px; s++) {
               bool inOutline = ((s <= ix) != (s <= px)) || ((ix == px) && (s == ix));
               if (hollow && !inOutline)
                  continue;
               bool clip0 = cx + s >= painter->clip.l && cx + s < painter->clip.r;
               bool clip1 = cx - s >= painter->clip.l && cx - s < painter->clip.r;
               if (clip0)
                  painter->bits[painter->width * py + cx + s] = inOutline ? outlineColor : fillColor;
               if (clip1)
                  painter->bits[painter->width * py + cx - s] = inOutline ? outlineColor : fillColor;
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
   if (x0 < painter->clip.l && x1 < painter->clip.l && x2 < painter->clip.l)
      return;
   if (x0 >= painter->clip.r && x1 >= painter->clip.r && x2 >= painter->clip.r)
      return;
   if (y2 < painter->clip.t || y0 >= painter->clip.b)
      return;
   bool needsXClip = x0 < painter->clip.l + 1 || x0 >= painter->clip.r - 1 || x1 < painter->clip.l + 1 ||
                     x1 >= painter->clip.r - 1 || x2 < painter->clip.l + 1 || x2 >= painter->clip.r - 1;
   bool needsYClip = y0 < painter->clip.t + 1 || y2 >= painter->clip.b - 1;

#define _UI_DRAW_TRIANGLE_APPLY_CLIP(xo, yo)                                    \
   if (needsYClip && (yi + yo < painter->clip.t || yi + yo >= painter->clip.b)) \
      continue;                                                                 \
   if (needsXClip && xf + xo < painter->clip.l)                                 \
      xf = painter->clip.l - xo;                                                \
   if (needsXClip && xt + xo > painter->clip.r)                                 \
      xt = painter->clip.r - xo;

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
      uint32_t* b = &painter->bits[(yi + y0) * painter->width + x0];
      for (int x = xf; x < xt; x++)
         b[x] = color;
   }

   // Step 5: Draw the bottom part.
   for (float y = 0; y < ye; y++) {
      int xf = xe0 * (ye - y) * yer, xt = xe1 * (ye - y) * yer, yi = (int)y;
      _UI_DRAW_TRIANGLE_APPLY_CLIP(x2, y1);
      uint32_t* b = &painter->bits[(yi + y1) * painter->width + x2];
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
   rectangle = intersection(painter->clip, rectangle);

   if (!rectangle.valid()) {
      return;
   }

   for (int line = rectangle.t; line < rectangle.b; line++) {
      uint32_t* bits  = painter->bits + line * painter->width + rectangle.l;
      int       count = rectangle.width();

      while (count--) {
         uint32_t in = *bits;
         *bits       = in ^ 0xFFFFFF;
         bits++;
      }
   }
}

int UIMeasureStringWidth(std::string_view string) {
#ifdef UI_UNICODE
   return Utf8StringLength(string.data(), string.size()) * ui->activeFont->glyphWidth;
#else
   return (int)string.size() * ui->activeFont->glyphWidth;
#endif
}

int UIMeasureStringHeight() {
   return ui->activeFont->glyphHeight;
}

void UIDrawString(UIPainter* painter, UIRectangle r, std::string_view string, uint32_t color, UIAlign align,
                  UIStringSelection* selection) {
   if (string.empty() || !string[0])
      return;

   UIRectangle oldClip = painter->clip;
   painter->clip       = intersection(r, oldClip);

   if (!painter->clip.valid()) {
      painter->clip = oldClip;
      return;
   }

   int    width  = UIMeasureStringWidth(string);
   int    height = UIMeasureStringHeight();
   int    x      = align == UIAlign::center ? ((r.l + r.r - width) / 2) : align == UIAlign::right ? (r.r - width) : r.l;
   int    y      = (r.t + r.b - height) / 2;
   int    i      = 0;
   size_t j      = 0;

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
         int w = ui->activeFont->glyphWidth;
         if (c == '\t') {
            int ii = i;
            while (++ii & 3)
               w += ui->activeFont->glyphWidth;
         }
         UIDrawBlock(painter, UIRectangle(x, x + w, y, y + height), selection->colorBackground);
         colorText = selection->colorText;
      }

      if (c != '\t') {
         UIDrawGlyph(painter, x, y, c, colorText);
      }

      if (selection && selection->carets[0] == i) {
         UIDrawInvert(painter, UIRectangle(x, x + 1, y, y + height));
      }

      x += ui->activeFont->glyphWidth, i++;

      if (c == '\t') {
         while (i & 3)
            x += ui->activeFont->glyphWidth, i++;
      }

      j += bytesConsumed;
   }

   if (selection && selection->carets[0] == i) {
      UIDrawInvert(painter, UIRectangle(x, x + 1, y, y + height));
   }

   painter->clip = oldClip;
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
   bool     checked       = mode & UIControl::state_checked;
   bool     disabled      = mode & UIControl::state_disabled;
   bool     focused       = mode & UIControl::state_focused;
   bool     hovered       = mode & UIControl::state_hovered;
   bool     indeterminate = mode & UIControl::state_indeterminate;
   bool     pressed       = mode & UIControl::state_pressed;
   bool     selected      = mode & UIControl::state_selected;
   uint32_t which         = mode & UIControl::type_mask;

   uint32_t buttonColor     = disabled               ? ui->theme.buttonDisabled
                              : (pressed && hovered) ? ui->theme.buttonPressed
                              : (pressed || hovered) ? ui->theme.buttonHovered
                              : focused              ? ui->theme.selected
                                                     : ui->theme.buttonNormal;
   uint32_t buttonTextColor = disabled                            ? ui->theme.textDisabled
                              : buttonColor == ui->theme.selected ? ui->theme.textSelected
                                                                  : ui->theme.text;

   if (which == UIControl::checkbox) {
      uint32_t    color = buttonColor, textColor = buttonTextColor;
      int         midY      = (bounds.t + bounds.b) / 2;
      UIRectangle boxBounds = UIRectangle(bounds.l, bounds.l + ui_size::checkbox_box, midY - ui_size::checkbox_box / 2,
                                          midY + ui_size::checkbox_box / 2);
      UIDrawRectangle(painter, boxBounds, color, ui->theme.border, UIRectangle(1));
      UIDrawString(painter, boxBounds + UIRectangle(1, 0, 0, 0),
                   checked         ? "*"
                   : indeterminate ? "-"
                                   : " ",
                   textColor, UIAlign::center, NULL);
      UIDrawString(painter, bounds + UIRectangle(ui_size::checkbox_box + ui_size::checkbox_gap, 0, 0, 0), label,
                   disabled ? ui->theme.textDisabled : ui->theme.text, UIAlign::left, NULL);
   } else if (which == UIControl::menu_item || which == UIControl::drop_down || which == UIControl::push_button) {
      uint32_t color = buttonColor, textColor = buttonTextColor;
      int      borderSize = which == UIControl::menu_item ? 0 : scale;
      UIDrawRectangle(painter, bounds, color, ui->theme.border, UIRectangle(borderSize));

      if (checked && !focused) {
         UIDrawBlock(painter, bounds + ui_rect_1i((int)(ui_size::button_checked_area * scale)),
                     ui->theme.buttonPressed);
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
      UIDrawString(painter, bounds, label, ui->theme.text, UIAlign::left, NULL);
   } else if (which == UIControl::splitter) {
      UIRectangle borders = (mode & UIControl::state_vertical) ? UIRectangle(0, 1) : UIRectangle(1, 0);
      UIDrawRectangle(painter, bounds, ui->theme.buttonNormal, ui->theme.border, borders);
   } else if (which == UIControl::scroll_track) {
      if (disabled)
         UIDrawBlock(painter, bounds, ui->theme.panel1);
   } else if (which == UIControl::scroll_down || which == UIControl::scroll_up) {
      bool     isDown = which == UIControl::scroll_down;
      uint32_t color  = pressed ? ui->theme.buttonPressed : hovered ? ui->theme.buttonHovered : ui->theme.panel2;
      UIDrawRectangle(painter, bounds, color, ui->theme.border, UIRectangle(0));

      if (mode & UIControl::state_vertical) {
         UIDrawGlyph(painter, (bounds.l + bounds.r - ui->activeFont->glyphWidth) / 2 + 1,
                     isDown ? (bounds.b - ui->activeFont->glyphHeight - 2 * scale) : (bounds.t + 2 * scale),
                     isDown ? 25 : 24, ui->theme.text);
      } else {
         UIDrawGlyph(painter, isDown ? (bounds.r - ui->activeFont->glyphWidth - 2 * scale) : (bounds.l + 2 * scale),
                     (bounds.t + bounds.b - ui->activeFont->glyphHeight) / 2, isDown ? 26 : 27, ui->theme.text);
      }
   } else if (which == UIControl::scroll_thumb) {
      uint32_t color = pressed ? ui->theme.buttonPressed : hovered ? ui->theme.buttonHovered : ui->theme.buttonNormal;
      UIDrawRectangle(painter, bounds, color, ui->theme.border, UIRectangle(2));
   } else if (which == UIControl::gauge) {
      UIDrawRectangle(painter, bounds, ui->theme.buttonNormal, ui->theme.border, UIRectangle(1));
      UIRectangle filled = bounds + ui_rect_1i(1);
      if (mode & UIControl::state_vertical) {
         filled.t = filled.b - filled.height() * position;
      } else {
         filled.r = filled.l + filled.width() * position;
      }
      UIDrawBlock(painter, filled, ui->theme.selected);
   } else if (which == UIControl::slider) {
      bool vertical     = mode & UIControl::state_vertical;
      int  center       = vertical ? (bounds.l + bounds.r) / 2 : (bounds.t + bounds.b) / 2;
      int  trackSize    = ui_size::slider_track * scale;
      int  thumbSize    = ui_size::slider_thumb * scale;
      int thumbPosition = vertical ? (bounds.height() - thumbSize) * position : (bounds.width() - thumbSize) * position;
      UIRectangle track = vertical
                             ? UIRectangle(center - (trackSize + 1) / 2, center + trackSize / 2, bounds.t, bounds.b)
                             : UIRectangle(bounds.l, bounds.r, center - (trackSize + 1) / 2, center + trackSize / 2);

      UIDrawRectangle(painter, track, disabled ? ui->theme.buttonDisabled : ui->theme.buttonNormal, ui->theme.border,
                      UIRectangle(1));
      uint32_t    color = disabled  ? ui->theme.buttonDisabled
                          : pressed ? ui->theme.buttonPressed
                          : hovered ? ui->theme.buttonHovered
                                    : ui->theme.buttonNormal;
      UIRectangle thumb = vertical ? UIRectangle(center - (thumbSize + 1) / 2, center + thumbSize / 2,
                                                 bounds.b - thumbPosition - thumbSize, bounds.b - thumbPosition)
                                   : UIRectangle(bounds.l + thumbPosition, bounds.l + thumbPosition + thumbSize,
                                                 center - (thumbSize + 1) / 2, center + thumbSize / 2);
      UIDrawRectangle(painter, thumb, color, ui->theme.border, UIRectangle(1));
   } else if (which == UIControl::textbox) {
      UIDrawRectangle(painter, bounds,
                      disabled  ? ui->theme.buttonDisabled
                      : focused ? ui->theme.textboxFocused
                                : ui->theme.textboxNormal,
                      ui->theme.border, UIRectangle(1));
   } else if (which == UIControl::modal_popup) {
      UIRectangle bounds2 = bounds + ui_rect_1i(-1);
      UIDrawBorder(painter, bounds2, ui->theme.border, UIRectangle(1));
      UIDrawBorder(painter, bounds2 + UIRectangle(1), ui->theme.border, UIRectangle(1));
   } else if (which == UIControl::menu) {
      UIDrawBlock(painter, bounds, ui->theme.border);
   } else if (which == UIControl::table_row) {
      if (selected)
         UIDrawBlock(painter, bounds, ui->theme.selected);
      else if (hovered)
         UIDrawBlock(painter, bounds, ui->theme.buttonHovered);
   } else if (which == UIControl::table_cell) {
      uint32_t textColor = selected ? ui->theme.textSelected : ui->theme.text;
      UIDrawString(painter, bounds, label, textColor, UIAlign::left, NULL);
   } else if (which == UIControl::table_background) {
      UIDrawBlock(painter, bounds, ui->theme.panel2);
      UIDrawRectangle(painter,
                      UIRectangle(bounds.l, bounds.r, bounds.t, bounds.t + (int)(ui_size::table_header * scale)),
                      ui->theme.panel1, ui->theme.border, UIRectangle(0, 0, 0, 1));
   } else if (which == UIControl::table_header) {
      UIDrawString(painter, bounds, label, ui->theme.text, UIAlign::left, NULL);
      if (selected)
         UIDrawInvert(painter, bounds);
   } else if (which == UIControl::mdi_child) {
      auto [titleSize, borderSize, titleRect, contentRect] = ui_mdi_child_calculate_layout(bounds, scale);
      UIRectangle borders                                  = UIRectangle(borderSize, borderSize, titleSize, borderSize);
      UIDrawBorder(painter, bounds, ui->theme.buttonNormal, borders);
      UIDrawBorder(painter, bounds, ui->theme.border, UIRectangle((int)scale));
      UIDrawBorder(painter, contentRect + ui_rect_1i(-1), ui->theme.border, UIRectangle((int)scale));
      UIDrawString(painter, titleRect, label, ui->theme.text, UIAlign::left, NULL);
   } else if (which == UIControl::tab) {
      uint32_t    color = selected ? ui->theme.buttonPressed : ui->theme.buttonNormal;
      UIRectangle t     = bounds;
      if (selected)
         t.b++, t.t--;
      else
         t.t++;
      UIDrawRectangle(painter, t, color, ui->theme.border, UIRectangle(1));
      UIDrawString(painter, bounds, label, ui->theme.text, UIAlign::center, NULL);
   } else if (which == UIControl::tab_band) {
      UIDrawRectangle(painter, bounds, ui->theme.panel1, ui->theme.border, UIRectangle(0, 0, 0, 1));
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
      ui->inspector.refresh();
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

   if (msg >= UIMessage::INPUT_EVENTS_START && msg <= UIMessage::INPUT_EVENTS_END && (_flags & disabled_flag)) {
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
   if (_window->_focused == this && disabled) {
      _window->focus();
   }

   if ((_flags & disabled_flag) && disabled)
      return;
   if ((~_flags & disabled_flag) && !disabled)
      return;

   if (disabled)
      _flags |= disabled_flag;
   else
      _flags &= ~disabled_flag;

   message(UIMessage::UPDATE, UIUpdate::disabled, 0);
}

void UIElement::focus() {
   UIElement* previous = _window->_focused;
   if (previous == this)
      return;
   _window->_focused = this;
   if (previous)
      previous->message(UIMessage::UPDATE, UIUpdate::focused, 0);
   this->message(UIMessage::UPDATE, UIUpdate::focused, 0);

   if constexpr (UIInspector::enabled())
      ui->inspector.refresh();
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
   return *new UIMenu(this, flags);
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

UIWindow& UI::add_window(UIWindow* owner, uint32_t flags, const char* cTitle, int width, int height) {
   return *UIWindowCreate(owner, flags, cTitle, width, height);
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
   painter->clip = intersection(_clip, painter->clip);

   if (!painter->clip.valid()) {
      return;
   }

   // Paint the element.
   // ------------------
   message(UIMessage::PAINT, 0, painter);

   // Paint its children.
   // -------------------
   UIRectangle previousClip = painter->clip;

   for (auto child : _children) {
      painter->clip = previousClip;
      child->paint(painter);
   }

   // Draw the foreground and border.
   // -------------------------------
   painter->clip = previousClip;
   message(UIMessage::PAINT_FOREGROUND, 0, painter);

   if (_flags & border_flag) {
      UIDrawBorder(painter, _bounds, ui->theme.border, UIRectangle((int)_window->_scale));
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

      if (win->_pressed == this) {
         win->set_pressed(NULL, 0);
      }

      if (win->_hovered == this) {
         win->_hovered = win;
      }

      if (win->_focused == this) {
         win->_focused = NULL;
      }

      if (win->_dialog_old_focus == this) {
         win->_dialog_old_focus = NULL;
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

   if (!parent && (~flags & window_flag)) {
      UI_ASSERT(ui->parentStackCount);
      parent = ui->parentStack[ui->parentStackCount - 1];
   }

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
      ui->inspector.refresh();

   if (flags & parent_push_flag) {
      UIParentPush(this);
   }
}

UIElement::~UIElement() {}

UIElement* UIElementCreate(size_t bytes, UIElement* parent, uint32_t flags, message_proc_t message_proc,
                           const char* cClassName) {
   UIElement* el = new UIElement(parent, flags, message_proc, cClassName);
   return el;
}

UIElement* UIParentPush(UIElement* el) {
   UI_ASSERT(ui->parentStackCount != sizeof(ui->parentStack) / sizeof(ui->parentStack[0]));
   ui->parentStack[ui->parentStackCount++] = el;
   return el;
}

UIElement* UIParentPop() {
   UI_ASSERT(ui->parentStackCount);
   ui->parentStackCount--;
   return ui->parentStack[ui->parentStackCount];
}

// --------------------------------------------------
// Panels.
// --------------------------------------------------

int _UIPanelCalculatePerFill(UIPanel* panel, int* _count, int hSpace, int vSpace, float scale) {
   bool horizontal = panel->_flags & UIPanel::HORIZONTAL;
   int  available  = horizontal ? hSpace : vSpace;
   int  count = 0, fill = 0, perFill = 0;

   for (auto child : panel->_children) {
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
      available -= (count - 1) * (int)(panel->_gap * scale);
   }

   if (available > 0 && fill) {
      perFill = available / fill;
   }

   if (_count) {
      *_count = count;
   }

   return perFill;
}

int _UIPanelMeasure(UIPanel* panel, int di) {
   bool horizontal = panel->_flags & UIPanel::HORIZONTAL;
   int  perFill =
      _UIPanelCalculatePerFill(panel, NULL, horizontal ? di : 0, horizontal ? 0 : di, panel->_window->_scale);
   int size = 0;

   for (auto child : panel->_children) {
      if (child->_flags & (UIElement::hide_flag | UIElement::non_client_flag))
         continue;
      int childSize =
         child->message(horizontal ? UIMessage::GET_HEIGHT : UIMessage::GET_WIDTH,
                        (child->_flags & (horizontal ? UIElement::h_fill : UIElement::v_fill)) ? perFill : 0, 0);
      if (childSize > size)
         size = childSize;
   }

   int border = horizontal ? (panel->_border.t + panel->_border.b) : (panel->_border.l + panel->_border.r);
   return size + panel->scale(border);
}

int _UIPanelLayout(UIPanel* panel, UIRectangle bounds, bool measure) {
   bool horizontal = panel->_flags & UIPanel::HORIZONTAL;

   int position = panel->scale(horizontal ? panel->_border.l : panel->_border.t);
   if (panel->_scrollBar && !measure)
      position -= panel->_scrollBar->_position;
   int  hSpace        = bounds.width() - panel->scale(panel->_border.total_width());
   int  vSpace        = bounds.height() - panel->scale(panel->_border.total_height());
   int  count         = 0;
   int  perFill       = _UIPanelCalculatePerFill(panel, &count, hSpace, vSpace, panel->_window->_scale);
   int  scaledBorder2 = panel->scale(horizontal ? panel->_border.t : panel->_border.l);
   bool expand        = panel->_flags & UIPanel::EXPAND;

   for (auto child : panel->_children) {
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
         position += width + panel->scale(panel->_gap);
      } else {
         int width  = ((child->_flags & UIElement::h_fill) || expand)
                         ? hSpace
                         : child->message(UIMessage::GET_WIDTH, (child->_flags & UIElement::v_fill) ? perFill : 0, 0);
         int height = (child->_flags & UIElement::v_fill) ? perFill : child->message(UIMessage::GET_HEIGHT, width, 0);
         UIRectangle relative = UIRectangle(scaledBorder2 + (hSpace - width) / 2, scaledBorder2 + (hSpace + width) / 2,
                                            position, position + height);
         if (!measure)
            child->move(translate(relative, bounds), false);
         position += height + panel->scale(panel->_gap);
      }
   }

   return position - panel->scale(count ? panel->_gap : 0) +
          panel->scale(horizontal ? panel->_border.r : panel->_border.b);
}

int UIPanel::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UIPanel* panel      = (UIPanel*)el;
   bool     horizontal = el->_flags & UIPanel::HORIZONTAL;

   if (msg == UIMessage::LAYOUT) {
      int         scrollBarWidth = panel->_scrollBar ? el->scale(ui_size::scroll_bar) : 0;
      UIRectangle bounds         = el->_bounds;
      bounds.r -= scrollBarWidth;

      if (panel->_scrollBar) {
         UIRectangle scrollBarBounds = el->_bounds;
         scrollBarBounds.l           = scrollBarBounds.r - scrollBarWidth;
         panel->_scrollBar->set_maximum(_UIPanelLayout(panel, bounds, true));
         panel->_scrollBar->set_page(el->_bounds.height());
         panel->_scrollBar->move(scrollBarBounds, true);
      }

      _UIPanelLayout(panel, bounds, false);
   } else if (msg == UIMessage::GET_WIDTH) {
      if (horizontal) {
         return _UIPanelLayout(panel, UIRectangle(0, 0, 0, di), true);
      } else {
         return _UIPanelMeasure(panel, di);
      }
   } else if (msg == UIMessage::GET_HEIGHT) {
      if (horizontal) {
         return _UIPanelMeasure(panel, di);
      } else {
         int width = di && panel->_scrollBar ? (di - el->scale(ui_size::scroll_bar)) : di;
         return _UIPanelLayout(panel, UIRectangle(0, width, 0, 0), true);
      }
   } else if (msg == UIMessage::PAINT) {
      if (el->_flags & UIPanel::COLOR_1) {
         UIDrawBlock((UIPainter*)dp, el->_bounds, ui->theme.panel1);
      } else if (el->_flags & UIPanel::COLOR_2) {
         UIDrawBlock((UIPainter*)dp, el->_bounds, ui->theme.panel2);
      }
   } else if (msg == UIMessage::MOUSE_WHEEL && panel->_scrollBar) {
      return panel->_scrollBar->message(msg, di, dp);
   } else if (msg == UIMessage::SCROLLED) {
      el->refresh();
   } else if (msg == UIMessage::GET_CHILD_STABILITY) {
      UIElement* child = (UIElement*)dp;
      return ((el->_flags & UIPanel::EXPAND) ? (horizontal ? 2 : 1) : 0) |
             ((child->_flags & UIElement::h_fill) ? 1 : 0) | ((child->_flags & UIElement::v_fill) ? 2 : 0);
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

   if (!switcher->active) {
   } else if (msg == UIMessage::GET_WIDTH || msg == UIMessage::GET_HEIGHT) {
      return switcher->active->message(msg, di, dp);
   } else if (msg == UIMessage::LAYOUT) {
      switcher->active->move(el->_bounds, false);
   }

   return 0;
}

void UISwitcherSwitchTo(UISwitcher* switcher, UIElement* child) {
   for (auto sw_child : switcher->_children)
      sw_child->_flags |= UIElement::hide_flag;

   UI_ASSERT(child->_parent == switcher);
   child->_flags &= ~UIElement::hide_flag;
   switcher->active = child;
   switcher->measurements_changed(3);
   switcher->refresh();
}

UISwitcher::UISwitcher(UIElement* parent, uint32_t flags)
   : UIElementCast<UISwitcher>(parent, flags, UISwitcher::_ClassMessageProc, "Switcher")
   , active(nullptr) {}

UISwitcher* UISwitcherCreate(UIElement* parent, uint32_t flags) {
   return new UISwitcher(parent, flags);
}

// --------------------------------------------------
// Checkboxes and buttons.
// --------------------------------------------------

int UIButton::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UIButton* button     = (UIButton*)el;
   bool      isMenuItem = el->_flags & UIButton::MENU_ITEM;
   bool      isDropDown = el->_flags & UIButton::DROP_DOWN;

   if (msg == UIMessage::GET_HEIGHT) {
      if (isMenuItem) {
         return el->scale(ui_size::menu_item_height);
      } else {
         return el->scale(ui_size::button_height);
      }
   } else if (msg == UIMessage::GET_WIDTH) {
      int labelSize  = UIMeasureStringWidth(button->label);
      int paddedSize = labelSize + el->scale(ui_size::button_padding);
      if (isDropDown)
         paddedSize += ui->activeFont->glyphWidth * 2;
      int minimumSize = el->scale((el->_flags & UIButton::SMALL) ? 0
                                  : isMenuItem                   ? ui_size::menu_item_minimum_width
                                                                 : ui_size::button_minimum_width);
      return paddedSize > minimumSize ? paddedSize : minimumSize;
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, el->_bounds,
                    (isMenuItem   ? UIControl::menu_item
                     : isDropDown ? UIControl::drop_down
                                  : UIControl::push_button) |
                       ((el->_flags & UIButton::CHECKED) ? UIControl::state_checked : 0) | el->state(),
                    button->label, 0, el->_window->_scale);
   } else if (msg == UIMessage::UPDATE) {
      el->repaint(NULL);
   } else if (msg == UIMessage::DEALLOCATE) {
   } else if (msg == UIMessage::LEFT_DOWN) {
      if (el->_flags & UIButton::CAN_FOCUS) {
         el->focus();
      }
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if ((m->text == " ") || m->code == UIKeycode::ENTER) {
         el->message(UIMessage::CLICKED, 0, 0);
         el->repaint(NULL);
         return 1;
      }
   } else if (msg == UIMessage::CLICKED) {
      if (button->invoke) {
         button->invoke();
      }
   }

   return 0;
}

void UIButtonSetLabel(UIButton* button, std::string_view string) {
   button->label = string;
   button->measurements_changed(1);
   button->repaint(NULL);
}

UIButton::UIButton(UIElement* parent, uint32_t flags, std::string_view label)
   : UIElementCast<UIButton>(parent, flags | UIElement::tab_stop_flag, UIButton::_ClassMessageProc, "Button")
   , label(label) {}

UIButton* UIButtonCreate(UIElement* parent, uint32_t flags, std::string_view label) {
   return new UIButton(parent, flags | UIElement::tab_stop_flag, label);
}

int UICheckbox::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UICheckbox* box = (UICheckbox*)el;

   if (msg == UIMessage::GET_HEIGHT) {
      return el->scale(ui_size::button_height);
   } else if (msg == UIMessage::GET_WIDTH) {
      int labelSize = UIMeasureStringWidth(box->label);
      return el->scale(labelSize + ui_size::checkbox_box + ui_size::checkbox_gap);
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, el->_bounds,
                    UIControl::checkbox |
                       (box->check == UICheckbox::INDETERMINATE ? UIControl::state_indeterminate
                        : box->check == UICheckbox::CHECKED     ? UIControl::state_checked
                                                                : 0) |
                       el->state(),
                    box->label, 0, el->_window->_scale);
   } else if (msg == UIMessage::UPDATE) {
      el->repaint(NULL);
   } else if (msg == UIMessage::DEALLOCATE) {
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if (m->text == " ") {
         el->message(UIMessage::CLICKED, 0, 0);
         el->repaint(NULL);
      }
   } else if (msg == UIMessage::CLICKED) {
      box->check = (box->check + 1) % ((el->_flags & UICheckbox::ALLOW_INDETERMINATE) ? 3 : 2);
      el->repaint(NULL);
      if (box->invoke)
         box->invoke();
   }

   return 0;
}

void UICheckbox::set_label(std::string_view new_label) {
   label = new_label;
   this->measurements_changed(1);
   repaint(NULL);
}

UICheckbox::UICheckbox(UIElement* parent, uint32_t flags, std::string_view label)
   : UIElementCast<UICheckbox>(parent, flags | UIElement::tab_stop_flag, UICheckbox::_ClassMessageProc, "Checkbox")
   , check(0)
   , label(label) {}


UICheckbox* UICheckboxCreate(UIElement* parent, uint32_t flags, std::string_view label) {
   return new UICheckbox(parent, flags | UIElement::tab_stop_flag, label);
}

// --------------------------------------------------
// Labels.
// --------------------------------------------------

int UILabel::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UILabel* label = (UILabel*)el;

   if (msg == UIMessage::GET_HEIGHT) {
      return UIMeasureStringHeight();
   } else if (msg == UIMessage::GET_WIDTH) {
      return UIMeasureStringWidth(label->_label);
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, el->_bounds, UIControl::label | el->state(), label->_label, 0, el->_window->_scale);
   } else if (msg == UIMessage::DEALLOCATE) {
   }

   return 0;
}

void UILabelSetContent(UILabel* label, std::string_view str) {
   label->_label = str;
   label->measurements_changed(1);
   label->repaint(NULL);
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
int UISplitter::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UISplitPane* splitPane = (UISplitPane*)el->_parent;
   bool         vertical  = splitPane->_flags & UIElement::vertical_flag;

   if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, el->_bounds,
                    UIControl::splitter | (vertical ? UIControl::state_vertical : 0) | el->state(), {}, 0,
                    el->_window->_scale);
   } else if (msg == UIMessage::GET_CURSOR) {
      return vertical ? (uint32_t)UICursor::split_v : (uint32_t)UICursor::split_h;
   } else if (msg == UIMessage::MOUSE_DRAG) {
      int   cursor       = vertical ? el->_window->_cursor.y : el->_window->_cursor.x;
      int   splitterSize = el->scale(ui_size::splitter);
      int   space        = (vertical ? splitPane->_bounds.height() : splitPane->_bounds.width()) - splitterSize;
      float oldWeight    = splitPane->_weight;
      splitPane->_weight =
         (float)(cursor - (float)splitterSize / 2 - (vertical ? splitPane->_bounds.t : splitPane->_bounds.l)) / space;
      splitPane->_weight = std::clamp(splitPane->_weight, 0.05f, 0.95f);

      if (splitPane->_children[2]->_class_proc == UISplitPane::_ClassMessageProc &&
          (splitPane->_children[2]->_flags & UIElement::vertical_flag) ==
             (splitPane->_flags & UIElement::vertical_flag)) {
         UISplitPane* subSplitPane = (UISplitPane*)splitPane->_children[2];
         subSplitPane->_weight =
            (splitPane->_weight - oldWeight - subSplitPane->_weight + oldWeight * subSplitPane->_weight) /
            (-1 + splitPane->_weight);
         splitPane->_weight = std::clamp(subSplitPane->_weight, 0.05f, 0.95f);
      }

      splitPane->refresh();
   }

   return 0;
}

int UISplitPane::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UISplitPane* splitPane = (UISplitPane*)el;
   bool         vertical  = splitPane->_flags & UIElement::vertical_flag;

   if (msg == UIMessage::LAYOUT) {
      assert(el->_children.size() >= 3);
      UIElement* splitter = el->_children[0];
      UIElement* left     = el->_children[1];
      UIElement* right    = el->_children[2];

      int splitterSize = el->scale(ui_size::splitter);
      int space        = (vertical ? el->_bounds.height() : el->_bounds.width()) - splitterSize;
      int leftSize     = space * splitPane->_weight;
      int rightSize    = space - leftSize;

      if (vertical) {
         left->move(UIRectangle(el->_bounds.l, el->_bounds.r, el->_bounds.t, el->_bounds.t + leftSize), false);
         splitter->move(UIRectangle(el->_bounds.l, el->_bounds.r, el->_bounds.t + leftSize,
                                    el->_bounds.t + leftSize + splitterSize),
                        false);
         right->move(UIRectangle(el->_bounds.l, el->_bounds.r, el->_bounds.b - rightSize, el->_bounds.b), false);
      } else {
         left->move(UIRectangle(el->_bounds.l, el->_bounds.l + leftSize, el->_bounds.t, el->_bounds.b), false);
         splitter->move(UIRectangle(el->_bounds.l + leftSize, el->_bounds.l + leftSize + splitterSize, el->_bounds.t,
                                    el->_bounds.b),
                        false);
         right->move(UIRectangle(el->_bounds.r - rightSize, el->_bounds.r, el->_bounds.t, el->_bounds.b), false);
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

int UITabPane::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UITabPane* tabPane = (UITabPane*)el;

   if (msg == UIMessage::PAINT) {
      UIPainter*  painter = (UIPainter*)dp;
      UIRectangle top     = el->_bounds;
      top.b               = top.t + el->scale(ui_size::button_height);
      UIDrawControl(painter, top, UIControl::tab_band, {}, 0, el->_window->_scale);

      UIRectangle tab = top;
      tab.l += el->scale(ui_size::tab_pane_space_left);
      tab.t += el->scale(ui_size::tab_pane_space_top);

      tabPane->for_each_tab([&](std::string_view tab_text, uint32_t index, bool active) {
         tab.r = tab.l + UIMeasureStringWidth(tab_text) + ui_size::button_padding;
         UIDrawControl(painter, tab, UIControl::tab | (active ? UIControl::state_selected : 0), tab_text, 0,
                       el->_window->_scale);
         tab.l = tab.r - 1;
         return true;
      });

   } else if (msg == UIMessage::LEFT_DOWN) {
      UIRectangle tab = el->_bounds;
      tab.b           = tab.t + el->scale(ui_size::button_height);
      tab.l += el->scale(ui_size::tab_pane_space_left);
      tab.t += el->scale(ui_size::tab_pane_space_top);

      tabPane->for_each_tab([&](std::string_view tab_text, uint32_t index, bool active) {
         tab.r = tab.l + UIMeasureStringWidth(tab_text) + ui_size::button_padding;
         if (tab.contains(el->_window->_cursor)) {
            tabPane->set_active(index);
            el->relayout();
            el->repaint(NULL);
            return false;
            ;
         }
         tab.l = tab.r - 1;
         return true;
      });
   } else if (msg == UIMessage::LAYOUT) {
      UIRectangle content = el->_bounds;
      content.t += el->scale(ui_size::button_height);

      for (uint32_t index = 0; index < el->_children.size(); index++) {
         UIElement* child = el->_children[index];

         if (tabPane->get_active() == index) {
            child->_flags &= ~UIElement::hide_flag;
            child->move(content, false);
            child->message(UIMessage::TAB_SELECTED, 0, 0);
         } else {
            child->_flags |= UIElement::hide_flag;
         }
      }
   } else if (msg == UIMessage::GET_HEIGHT) {
      int baseHeight = el->scale(ui_size::button_height);

      for (uint32_t index = 0; index < el->_children.size(); index++) {
         UIElement* child = el->_children[index];

         if (tabPane->get_active() == index) {
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

int UISpacer::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UISpacer* spacer = (UISpacer*)el;

   if (msg == UIMessage::GET_HEIGHT) {
      return el->scale(spacer->_height);
   } else if (msg == UIMessage::GET_WIDTH) {
      return el->scale(spacer->_width);
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

int UIScrollBar::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UIScrollBar* scrollBar = (UIScrollBar*)el;

   if (msg == UIMessage::GET_WIDTH || msg == UIMessage::GET_HEIGHT) {
      return el->scale(ui_size::scroll_bar);
   } else if (msg == UIMessage::LAYOUT) {
      UIElement* up    = el->_children[0];
      UIElement* thumb = el->_children[1];
      UIElement* down  = el->_children[2];

      if (scrollBar->page() >= scrollBar->maximum() || scrollBar->maximum() <= 0 || scrollBar->page() <= 0) {
         up->_flags |= UIElement::hide_flag;
         thumb->_flags |= UIElement::hide_flag;
         down->_flags |= UIElement::hide_flag;

         scrollBar->_position = 0;
      } else {
         up->_flags &= ~UIElement::hide_flag;
         thumb->_flags &= ~UIElement::hide_flag;
         down->_flags &= ~UIElement::hide_flag;

         int size      = scrollBar->_horizontal ? el->_bounds.width() : el->_bounds.height();
         int thumbSize = size * scrollBar->page() / scrollBar->maximum();

         if (thumbSize < el->scale(ui_size::scroll_minimum_thumb)) {
            thumbSize = el->scale(ui_size::scroll_minimum_thumb);
         }

         if (scrollBar->_position < 0) {
            scrollBar->_position = 0;
         } else if (scrollBar->_position > scrollBar->maximum() - scrollBar->page()) {
            scrollBar->_position = scrollBar->maximum() - scrollBar->page();
         }

         int thumbPosition =
            (int)((double)scrollBar->_position / (scrollBar->maximum() - scrollBar->page()) * (size - thumbSize));

         if (scrollBar->_position == scrollBar->maximum() - scrollBar->page()) {
            thumbPosition = size - thumbSize;
         }

         if (scrollBar->_horizontal) {
            UIRectangle r = el->_bounds;
            r.r           = r.l + thumbPosition;
            up->move(r, false);
            r.l = r.r, r.r = r.l + thumbSize;
            thumb->move(r, false);
            r.l = r.r, r.r = el->_bounds.r;
            down->move(r, false);
         } else {
            UIRectangle r = el->_bounds;
            r.b           = r.t + thumbPosition;
            up->move(r, false);
            r.t = r.b, r.b = r.t + thumbSize;
            thumb->move(r, false);
            r.t = r.b, r.b = el->_bounds.b;
            down->move(r, false);
         }
      }
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, el->_bounds,
                    UIControl::scroll_track | ((scrollBar->page() >= scrollBar->maximum() ||
                                                scrollBar->maximum() <= 0 || scrollBar->page() <= 0)
                                                  ? UIControl::state_disabled
                                                  : 0),
                    {}, 0, el->_window->_scale);
   } else if (msg == UIMessage::MOUSE_WHEEL) {
      scrollBar->_position += di;
      el->refresh();
      el->_parent->message(UIMessage::SCROLLED, 0, 0);
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
                    {}, 0, el->_window->_scale);
   } else if (msg == UIMessage::UPDATE) {
      el->repaint(NULL);
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
         scrollBar->_position += deltaPixels;
      else
         scrollBar->_position -= deltaPixels;
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
                    {}, 0, el->_window->_scale);
   } else if (msg == UIMessage::UPDATE) {
      el->repaint(NULL);
   } else if (msg == UIMessage::MOUSE_DRAG && el->_window->_pressed_button == 1) {
      if (!scrollBar->_in_drag) {
         scrollBar->_in_drag = true;

         if (scrollBar->_horizontal) {
            scrollBar->set_drag_offset(el->_bounds.l - scrollBar->_bounds.l - el->_window->_cursor.x);
         } else {
            scrollBar->set_drag_offset(el->_bounds.t - scrollBar->_bounds.t - el->_window->_cursor.y);
         }
      }

      int thumbPosition =
         (scrollBar->_horizontal ? el->_window->_cursor.x : el->_window->_cursor.y) + scrollBar->drag_offset();
      int size             = scrollBar->_horizontal ? (scrollBar->_bounds.width() - el->_bounds.width())
                                                    : (scrollBar->_bounds.height() - el->_bounds.height());
      scrollBar->_position = (double)thumbPosition / size * (scrollBar->maximum() - scrollBar->page());
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
      _vscroll->_position -= rowHeight;
   else if (m->code == UIKeycode::DOWN)
      _vscroll->_position += rowHeight;
   else if (m->code == UIKeycode::PAGE_UP)
      _vscroll->_position += pageHeight;
   else if (m->code == UIKeycode::PAGE_DOWN)
      _vscroll->_position -= pageHeight;
   else if (m->code == UIKeycode::HOME)
      _vscroll->_position = 0;
   else if (m->code == UIKeycode::END)
      _vscroll->_position = _vscroll->maximum();
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
   UIFont* previousFont = UIFontActivate(_font);
   int     lineHeight   = UIMeasureStringHeight();
   *line                = std::max((int64_t)0, (y - _bounds.t + _vscroll->_position) / lineHeight);
   if (*line >= num_lines())
      *line = num_lines() - 1;
   int column = (x - _bounds.l + _hscroll->_position + ui->activeFont->glyphWidth / 2) / ui->activeFont->glyphWidth;
   if (~_flags & UICode::NO_MARGIN)
      column -= (ui->code_margin() + ui->code_margin_gap()) / ui->activeFont->glyphWidth;
   UIFontActivate(previousFont);
   *byte = column_to_byte(*line, column);
   return *this;
}

int UICode::hittest(int x, int y) {
   x -= _bounds.l;

   if (x < 0 || x >= _vscroll->_bounds.l) {
      return 0;
   }

   y -= _bounds.t - _vscroll->_position;

   UIFont* previousFont = UIFontActivate(_font);
   int     lineHeight   = UIMeasureStringHeight();
   bool    inMargin     = x < ui->code_margin() + ui->code_margin_gap() / 2 && (~_flags & UICode::NO_MARGIN);
   UIFontActivate(previousFont);

   if (y < 0 || y >= lineHeight * (int)num_lines()) {
      return 0;
   }

   int line = y / lineHeight + 1;
   return inMargin ? -line : line;
}

int UIDrawStringHighlighted(UIPainter* painter, UIRectangle lineBounds, std::string_view string, int tabSize,
                            UIStringSelection* selection) {
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
      ui->theme.codeDefault, ui->theme.codeComment,  ui->theme.codeString,
      ui->theme.codeNumber,  ui->theme.codeOperator, ui->theme.codePreprocessor,
   };

   int              lineHeight = UIMeasureStringHeight();
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
         x += ui->activeFont->glyphWidth, ti++;
         while (ti % tabSize)
            x += ui->activeFont->glyphWidth, ti++, j++;
      } else {
         UIDrawGlyph(painter, x, y, c, colors[tokenType]);
         x += ui->activeFont->glyphWidth, ti++;
      }

      if (selection && j >= selection->carets[0] && j < selection->carets[1]) {
         UIDrawBlock(painter, UIRectangle(oldX, x, y, y + lineHeight), selection->colorBackground);
         if (c != '\t')
            UIDrawGlyph(painter, oldX, y, c, selection->colorText);
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

UICode& UICode::copy_text(sel_target_t t) {
   size_t from = offset(selection(0));
   size_t to   = offset(selection(1));

   if (from != to) {
      std::string pasteText;
      pasteText.resize(to - from);
      for (size_t i = from; i < to; i++)
         pasteText[i - from] = (*this)[i];
      UI::ClipboardWriteText(_window, std::move(pasteText), t);
   }
   return *this;
}

UICode& UICode::_update_selection() {
   bool swap = _selection[3].line < _selection[2].line ||
               (_selection[3].line == _selection[2].line && _selection[3].offset < _selection[2].offset);
   _selection[1 - swap] = _selection[3];
   _selection[0 + swap] = _selection[2];
   copy_text(sel_target_t::primary);
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
   if (msg == UIMessage::LAYOUT) {
      UIFont* previousFont  = UIFontActivate(font());
      int     scrollBarSize = scale(ui_size::scroll_bar);
      _vscroll->set_maximum(num_lines() * UIMeasureStringHeight());
      _hscroll->set_maximum(max_columns() * font()->glyphWidth); // TODO This doesn't take into account tab sizes!
      int vSpace = _bounds.height();
      int hSpace = _bounds.width();

      _vscroll->set_page(vSpace);
      _hscroll->set_page(hSpace);

      if (_move_scroll_to_caret_next_layout) {
         size_t top     = _selection[3].line * UIMeasureStringHeight();
         size_t bottom  = top + UIMeasureStringHeight();
         size_t context = UIMeasureStringHeight() * 2;
         if (bottom > _vscroll->_position + vSpace - context)
            _vscroll->_position = bottom - vSpace + context;
         if (top < _vscroll->_position + context)
            _vscroll->_position = top - context;
         _move_scroll_to_caret_next_layout = _move_scroll_to_focus_next_layout = false;
         // TODO Horizontal scrolling.
      } else if (_move_scroll_to_focus_next_layout) {
         _vscroll->_position = (focus_line() + 0.5f) * UIMeasureStringHeight() - _bounds.height() * 0.5f;
      }

      if (!(_flags & UICode::NO_MARGIN))
         hSpace -= ui->code_margin() + ui->code_margin_gap();
      layout_scrollbar_pair(hSpace, vSpace, scrollBarSize, this);

      UIFontActivate(previousFont);
   } else if (msg == UIMessage::PAINT) {
      UIFont* previousFont = UIFontActivate(font());

      UIPainter*  painter    = (UIPainter*)dp;
      UIRectangle lineBounds = _bounds;

      lineBounds.r = _vscroll->_bounds.l;

      if (~_flags & UICode::NO_MARGIN) {
         lineBounds.l += ui->code_margin() + ui->code_margin_gap();
      }

      int lineHeight = UIMeasureStringHeight();
      lineBounds.t -= (int64_t)_vscroll->_position % lineHeight;

      UIDrawBlock(painter, _bounds, ui->theme.codeBackground);

      UIStringSelection selection = {};
      selection.colorBackground   = ui->theme.selected;
      selection.colorText         = ui->theme.textSelected;

      for (size_t i = _vscroll->_position / lineHeight; i < num_lines(); i++) {
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
                         marginColor ? ui->theme.codeDefault : ui->theme.codeComment, UIAlign::right, NULL);
         }

         if (focus_line() == i) {
            UIDrawBlock(painter, lineBounds, ui->theme.codeFocused);
         }

         UIRectangle oldClip = painter->clip;
         painter->clip       = intersection(oldClip, lineBounds);
         if (_hscroll)
            lineBounds.l -= (int64_t)_hscroll->_position;
         selection.carets[0] = i == _selection[0].line ? byte_to_column(i, _selection[0].offset) : 0;
         selection.carets[1] = i == _selection[1].line ? byte_to_column(i, _selection[1].offset) : (int)line(i).size();

         bool selected = _window->_focused == this && i >= _selection[0].line && i <= _selection[1].line;
         int  x = UIDrawStringHighlighted(painter, lineBounds, line(i), tab_columns(), selected ? &selection : nullptr);
         int  y = (lineBounds.t + lineBounds.b - UIMeasureStringHeight()) / 2;

         if (_window->_focused == this && i >= _selection[0].line && i < _selection[1].line) {
            UIDrawBlock(painter, ui_rect_4pd(x, y, font()->glyphWidth, font()->glyphHeight), selection.colorBackground);
         }

         if (_hscroll)
            lineBounds.l += (int64_t)_hscroll->_position;
         painter->clip = oldClip;

         UICodeDecorateLine m;
         m.x = x, m.y = y, m.bounds = lineBounds, m.index = (int)i, m.painter = painter;
         message(UIMessage::CODE_DECORATE_LINE, 0, &m);

         lineBounds.t += lineHeight;
      }

      UIFontActivate(previousFont);
   } else if (msg == UIMessage::SCROLLED) {
      _move_scroll_to_focus_next_layout = false;
      refresh();
   } else if (msg == UIMessage::MOUSE_WHEEL) {
      return _vscroll->message(msg, di, dp);
   } else if (msg == UIMessage::GET_CURSOR) {
      if (hittest(_window->_cursor.x, _window->_cursor.y) < 0) {
         return (int)UICursor::flipped_arrow;
      }

      if (_flags & UICode::SELECTABLE) {
         return (int)UICursor::text;
      }
   } else if (msg == UIMessage::LEFT_UP) {
      animate(true);
   } else if (msg == UIMessage::LEFT_DOWN && num_lines()) {
      int hitTest          = hittest(_window->_cursor.x, _window->_cursor.y);
      _left_down_in_margin = hitTest < 0;

      if (hitTest > 0 && (_flags & UICode::SELECTABLE)) {
         position_to_byte(_window->_cursor.x, _window->_cursor.y, &_selection[2].line, &_selection[2].offset);
         _class_message_proc(UIMessage::MOUSE_DRAG, di, dp);
         focus();
         animate(false);
         set_last_animate_time(UI_CLOCK());
      }
   } else if (msg == UIMessage::ANIMATE) {
      if (_window->_pressed == this && _window->_pressed_button == 1 && num_lines() && !_left_down_in_margin) {
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

         UIFont* previousFont = UIFontActivate(font());

         if (_window->_cursor.x < _bounds.l + ((_flags & UICode::NO_MARGIN)
                                                  ? ui->code_margin_gap()
                                                  : (ui->code_margin() + ui->code_margin_gap() * 2))) {
            _hscroll->_position -= delta;
         } else if (_window->_cursor.x >= _vscroll->_bounds.l - ui->code_margin_gap()) {
            _hscroll->_position += delta;
         }

         if (_window->_cursor.y < _bounds.t + ui->code_margin_gap()) {
            _vscroll->_position -= delta;
         } else if (_window->_cursor.y >= _hscroll->_bounds.t - ui->code_margin_gap()) {
            _vscroll->_position += delta;
         }

         _move_scroll_to_focus_next_layout = false;
         UIFontActivate(previousFont);
         UICode::_ClassMessageProc(this, UIMessage::MOUSE_DRAG, di, dp);
         refresh();
      }
   } else if (msg == UIMessage::MOUSE_DRAG && _window->_pressed_button == 1 && num_lines() && !_left_down_in_margin) {
      // TODO Double-click and triple-click dragging for word and line granularity respectively.
      position_to_byte(_window->_cursor.x, _window->_cursor.y, &_selection[3].line, &_selection[3].offset);
      _update_selection();
      _move_scroll_to_focus_next_layout = _move_scroll_to_caret_next_layout = false;
      _use_vertical_motion_column                                           = false;
   } else if (msg == UIMessage::KEY_TYPED && num_lines()) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if ((m->code == UI_KEYCODE_LETTER('C') || m->code == UI_KEYCODE_LETTER('X') || m->code == UIKeycode::INSERT) &&
          _window->_ctrl && !_window->_alt && !_window->_shift) {
         copy_text(sel_target_t::clipboard);
      } else if ((m->code == UIKeycode::UP || m->code == UIKeycode::DOWN || m->code == UIKeycode::PAGE_UP ||
                  m->code == UIKeycode::PAGE_DOWN) &&
                 !_window->_ctrl && !_window->_alt) {
         UIFont* previousFont = UIFontActivate(font());
         int     lineHeight   = UIMeasureStringHeight();

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

         UIFontActivate(previousFont);
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
            _vscroll->_position               = m->code == UIKeycode::HOME ? 0 : _vscroll->maximum();
            _move_scroll_to_focus_next_layout = false;
            refresh();
         }
      } else if ((m->code == UIKeycode::LEFT || m->code == UIKeycode::RIGHT) && !_window->_alt) {
         if (_window->_shift) {
            move_caret(m->code == UIKeycode::LEFT, _window->_ctrl);
         } else if (!_window->_ctrl) {
            _hscroll->_position +=
               m->code == UIKeycode::LEFT ? -ui->activeFont->glyphWidth : ui->activeFont->glyphWidth;
            refresh();
         } else {
            return 0;
         }
      } else {
         return 0;
      }

      return 1;
   } else if (msg == UIMessage::RIGHT_DOWN) {
      int hitTest = hittest(_window->_cursor.x, _window->_cursor.y);

      if (hitTest > 0 && (_flags & UICode::SELECTABLE)) {
         focus();
         UIMenu* menu = UIMenuCreate(_window, UIMenu::NO_SCROLL);
         UIMenuAddItem(menu,
                       (_selection[0].line == _selection[1].line && _selection[0].offset == _selection[1].offset)
                          ? disabled_flag
                          : 0,
                       "Copy", [this]() { copy_text(sel_target_t::clipboard); });
         UIMenuShow(menu);
      }
   } else if (msg == UIMessage::UPDATE) {
      repaint(NULL);
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

   UIFont* previousFont = UIFontActivate(_font);

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
      _vscroll->_position = _lines.size() * UIMeasureStringHeight();
   }

   UIFontActivate(previousFont);
   repaint(NULL);
   return *this;
}

UICode::UICode(UIElement* parent, uint32_t flags)
   : UIElementCast<UICode>(parent, flags, UICode::_ClassMessageProc, "Code")
   , UIScrollbarPair(this)
   , _font(ui->activeFont) {}

UICode* UICodeCreate(UIElement* parent, uint32_t flags) {
   return new UICode(parent, flags);
}

// --------------------------------------------------
// Gauges.
// --------------------------------------------------

int _UIGaugeMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   UIGauge* gauge = (UIGauge*)el;

   if (msg == UIMessage::GET_HEIGHT) {
      return el->scale(gauge->_vertical ? ui_size::gauge_width : ui_size::gauge_height);
   } else if (msg == UIMessage::GET_WIDTH) {
      return el->scale(gauge->_vertical ? ui_size::gauge_height : ui_size::gauge_width);
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, el->_bounds,
                    UIControl::gauge | el->state() | (gauge->_vertical ? UIControl::state_vertical : 0), {},
                    gauge->_position, el->_window->_scale);
   }

   return 0;
}

UIGauge& UIGauge::set_position(double new_pos) {
   new_pos = std::clamp(new_pos, 0., 1.);
   if (new_pos != _position) {
      _position = new_pos;
      repaint(NULL);
   }
   return *this;
}

UIGauge::UIGauge(UIElement* parent, uint32_t flags)
   : UIElementCast<UIGauge>(parent, flags, _UIGaugeMessage, "Gauge")
   , _position(0)
   , _vertical(!!(flags & vertical_flag)) {}

UIGauge* UIGaugeCreate(UIElement* parent, uint32_t flags) {
   return new UIGauge(parent, flags);
}

// --------------------------------------------------
// Sliders.
// --------------------------------------------------

int _UISliderMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   UISlider* slider = (UISlider*)el;

   if (msg == UIMessage::GET_HEIGHT) {
      return el->scale(slider->_vertical ? ui_size::slider_width : ui_size::slider_height);
   } else if (msg == UIMessage::GET_WIDTH) {
      return el->scale(slider->_vertical ? ui_size::slider_height : ui_size::slider_width);
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, el->_bounds,
                    UIControl::slider | el->state() | (slider->_vertical ? UIControl::state_vertical : 0), {},
                    slider->_position, el->_window->_scale);
   } else if (msg == UIMessage::LEFT_DOWN || (msg == UIMessage::MOUSE_DRAG && el->_window->_pressed_button == 1)) {
      UIRectangle bounds    = el->_bounds;
      int         thumbSize = el->scale(ui_size::slider_thumb);
      slider->_position =
         slider->_vertical
            ? 1 - ((float)(el->_window->_cursor.y - thumbSize / 2 - bounds.t) / (bounds.height() - thumbSize))
            : (double)(el->_window->_cursor.x - thumbSize / 2 - bounds.l) / (bounds.width() - thumbSize);
      if (slider->_steps > 1)
         slider->_position = (int)(slider->_position * (slider->_steps - 1) + 0.5f) / (double)(slider->_steps - 1);
      if (slider->_position < 0)
         slider->_position = 0;
      if (slider->_position > 1)
         slider->_position = 1;
      el->message(UIMessage::VALUE_CHANGED, 0, 0);
      el->repaint(NULL);
   } else if (msg == UIMessage::UPDATE) {
      el->repaint(NULL);
   }

   return 0;
}

UISlider& UISlider::set_position(double new_pos) {
   new_pos = std::clamp(new_pos, 0., 1.);
   if (new_pos != _position) {
      if (_steps > 1)
         _position = (int)(_position * (_steps - 1) + 0.5f) / (float)(_steps - 1);
      message(UIMessage::VALUE_CHANGED, 0, 0);
      repaint(NULL);
   }
   return *this;
}

UISlider::UISlider(UIElement* parent, uint32_t flags)
   : UIElementCast<UISlider>(parent, flags, _UISliderMessage, "Slider")
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

   y -= (_bounds.t + scale(ui_size::table_header)) - _vscroll->_position;

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
   y -= _vscroll->_position;
   int height = _bounds.height() - scale(ui_size::table_header) - rowHeight;

   if (y < 0) {
      _vscroll->_position += y;
      refresh();
      return true;
   } else if (y > height) {
      _vscroll->_position -= height - y;
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
      int    longest = UIMeasureStringWidth(column(position, end));

      for (size_t i = 0; i < num_items(); i++) {
         m.index   = i;
         int bytes = message(UIMessage::TABLE_GET_ITEM, 0, &m);
         int width = UIMeasureStringWidth(m.buff(bytes));

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

   repaint(NULL);
   return *this;
}

int UITable::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UITable* table = (UITable*)el;

   if (msg == UIMessage::PAINT) {
      UIPainter*  painter = (UIPainter*)dp;
      UIRectangle bounds  = el->_bounds;
      bounds.r            = table->_vscroll->_bounds.l;
      UIDrawControl(painter, el->_bounds, UIControl::table_background | el->state(), {}, 0, el->_window->_scale);
      UIRectangle    row       = bounds;
      int            rowHeight = el->scale(ui_size::table_row);
      UITableGetItem m(256);
      row.t += table->scale(ui_size::table_header);
      row.t -= (int64_t)table->_vscroll->_position % rowHeight;
      int         hovered = table->hittest(el->_window->_cursor.x, el->_window->_cursor.y);
      UIRectangle oldClip = painter->clip;
      painter->clip =
         intersection(oldClip, UIRectangle(bounds.l, bounds.r, bounds.t + el->scale(ui_size::table_header), bounds.b));

      for (int i = table->_vscroll->_position / rowHeight; i < (int)table->num_items(); i++) {
         if (row.t > painter->clip.b) {
            break;
         }

         row.b        = row.t + rowHeight;
         m.index      = i;
         m.isSelected = false;
         m.column     = 0;
         int bytes    = el->message(UIMessage::TABLE_GET_ITEM, 0, &m);

         uint32_t rowFlags =
            (m.isSelected ? UIControl::state_selected : 0) | (hovered == i ? UIControl::state_hovered : 0);
         UIDrawControl(painter, row, UIControl::table_row | rowFlags, {}, 0, el->_window->_scale);

         UIRectangle cell = row;
         cell.l += table->scale(ui_size::table_column_gap) - (int64_t)table->_hscroll->_position;

         for (size_t j = 0; j < table->_column_widths.size(); j++) {
            if (j) {
               m.column = j;
               bytes    = el->message(UIMessage::TABLE_GET_ITEM, 0, &m);
            }

            cell.r = cell.l + table->_column_widths[j];
            if ((size_t)bytes > m.buff_size() && bytes > 0)
               bytes = m.buff_size();
            if (bytes > 0)
               UIDrawControl(painter, cell, UIControl::table_cell | rowFlags, m.buff(bytes), 0, el->_window->_scale);
            cell.l += table->_column_widths[j] + table->scale(ui_size::table_column_gap);
         }

         row.t += rowHeight;
      }

      bounds        = el->_bounds;
      painter->clip = intersection(oldClip, bounds);
      if (table->_hscroll)
         bounds.l -= (int64_t)table->_hscroll->_position;

      UIRectangle header = bounds;
      header.b           = header.t + table->scale(ui_size::table_header);
      header.l += table->scale(ui_size::table_column_gap);

      size_t position = 0;
      size_t index    = 0;

      if (!table->_column_widths.empty()) {
         while (true) {
            size_t end = table->column_end(position);

            header.r = header.l + table->_column_widths[index];
            UIDrawControl(painter, header,
                          UIControl::table_header | (index == table->_column_highlight ? UIControl::state_selected : 0),
                          table->column(position, end), 0, el->_window->_scale);
            header.l += table->_column_widths[index] + table->scale(ui_size::table_column_gap);

            if (table->_columns[end] == '\t') {
               position = end + 1;
               index++;
            } else {
               break;
            }
         }
      }
   } else if (msg == UIMessage::LAYOUT) {
      int scrollBarSize = table->scale(ui_size::scroll_bar);
      int columnGap     = table->scale(ui_size::table_column_gap);

      table->_vscroll->set_maximum(table->num_items() * el->scale(ui_size::table_row));
      table->_hscroll->set_maximum(columnGap);
      for (auto width : table->_column_widths) {
         table->_hscroll->set_maximum(table->_hscroll->maximum() + width + columnGap);
      }

      int vSpace = el->_bounds.height() - el->scale(ui_size::table_header);
      int hSpace = el->_bounds.width();

      table->_vscroll->set_page(vSpace);
      table->_hscroll->set_page(hSpace);

      table->layout_scrollbar_pair(hSpace, vSpace, scrollBarSize, table);
   } else if (msg == UIMessage::MOUSE_MOVE || msg == UIMessage::UPDATE) {
      el->repaint(NULL);
   } else if (msg == UIMessage::SCROLLED) {
      el->refresh();
   } else if (msg == UIMessage::MOUSE_WHEEL) {
      return table->_vscroll->message(msg, di, dp);
   } else if (msg == UIMessage::LEFT_DOWN) {
      el->focus();
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if ((m->code == UIKeycode::UP || m->code == UIKeycode::DOWN || m->code == UIKeycode::PAGE_UP ||
           m->code == UIKeycode::PAGE_DOWN || m->code == UIKeycode::HOME || m->code == UIKeycode::END) &&
          !el->_window->_ctrl && !el->_window->_alt && !el->_window->_shift) {
         table->key_input_vscroll(m, el->scale(ui_size::table_row),
                                  (el->_bounds.t - table->_hscroll->_bounds.t + ui_size::table_header) * 4 / 5, table);
         return 1;
      } else if ((m->code == UIKeycode::LEFT || m->code == UIKeycode::RIGHT) && !el->_window->_ctrl &&
                 !el->_window->_alt && !el->_window->_shift) {
         table->_hscroll->_position +=
            m->code == UIKeycode::LEFT ? -ui->activeFont->glyphWidth : ui->activeFont->glyphWidth;
         table->refresh();
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

int _UITextboxByteToColumn(std::string_view string, int byte) {
   return UI::byte_to_column(string, byte, 4);
}

int _UITextboxColumnToByte(std::string_view string, int column) {
   return UI::column_to_byte(string, column, 4);
}

void UITextboxReplace(UITextbox* textbox, std::string_view text, bool sendChangedMessage) {
   auto   sz         = textbox->buffer.size();
   size_t deleteFrom = textbox->carets[0], deleteTo = textbox->carets[1];
   assert(deleteFrom <= sz && deleteTo <= sz);
   if (deleteFrom > deleteTo)
      std::swap(deleteFrom, deleteTo);

   // first remove the selection
   // --------------------------
   textbox->buffer.erase(deleteFrom, deleteTo - deleteFrom);

   // then insert new text
   // --------------------
   textbox->buffer.insert(deleteFrom, text);
   textbox->carets[0] = deleteFrom + text.size();
   textbox->carets[1] = textbox->carets[0];

   if (sendChangedMessage)
      textbox->message(UIMessage::VALUE_CHANGED, 0, 0);
   textbox->_window->_textbox_modified_flag = true;
   textbox->repaint(NULL);
}

void UITextboxClear(UITextbox* textbox, bool sendChangedMessage) {
   textbox->carets[1] = 0;
   textbox->carets[0] = textbox->text().size();
   UITextboxReplace(textbox, "", sendChangedMessage);
}

void UITextboxMoveCaret(UITextbox* textbox, bool backward, bool word) {
   while (true) {
      std::string_view text = textbox->text();
      if (textbox->carets[0] > 0 && backward) {
         _ui_move_caret_backwards(textbox->carets[0], text.data(), textbox->carets[0], 0);
      } else if (textbox->carets[0] < (int)text.size() && !backward) {
         _ui_move_caret_forward(textbox->carets[0], text, textbox->carets[0]);
      } else {
         return;
      }

      if (!word) {
         return;
      } else if (textbox->carets[0] != (int)text.size() && textbox->carets[0] != 0) {
         if (_ui_move_caret_by_word(text, textbox->carets[0]))
            break;
      }
   }

   textbox->repaint(NULL);
}

void UITextboxCopyText(void* cp) {
   UITextbox* textbox = (UITextbox*)cp;

   int from = std::min(textbox->carets[0], textbox->carets[1]);
   int to   = std::max(textbox->carets[0], textbox->carets[1]);

   if (from != to) {
      auto        text = textbox->text();
      std::string pasteText;
      pasteText.resize(to - from);
      for (int i = from; i < to; i++)
         pasteText[i - from] = text[i];
      UI::ClipboardWriteText(textbox->_window, std::move(pasteText), sel_target_t::clipboard);
   }
}

void UITextboxPasteText(void* cp, sel_target_t t) {
   UITextbox*  textbox = (UITextbox*)cp;
   std::string text    = UI::ClipboardReadText(textbox->_window, t);

   if (!text.empty()) {
      for (auto& c : text)
         if (c == '\n')
            c = ' ';

      UITextboxReplace(textbox, text, true);
   }
}

int _UITextboxMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   UITextbox* textbox = (UITextbox*)el;

   if (msg == UIMessage::GET_HEIGHT) {
      return el->scale(ui_size::textbox_height);
   } else if (msg == UIMessage::GET_WIDTH) {
      return el->scale(ui_size::textbox_width);
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, el->_bounds, UIControl::textbox | el->state(), {}, 0, el->_window->_scale);

      int         scaledMargin = el->scale(ui_size::textbox_margin);
      int         totalWidth   = UIMeasureStringWidth(textbox->text()) + scaledMargin * 2;
      UIRectangle textBounds   = el->_bounds + ui_rect_1i(scaledMargin);

      if (textbox->scroll > totalWidth - textBounds.width()) {
         textbox->scroll = totalWidth - textBounds.width();
      }

      if (textbox->scroll < 0) {
         textbox->scroll = 0;
      }

      int caretX = UIMeasureStringWidth(textbox->text().substr(0, textbox->carets[0])) - textbox->scroll;

      if (caretX < 0) {
         textbox->scroll = caretX + textbox->scroll;
      } else if (caretX > textBounds.width()) {
         textbox->scroll = caretX - textBounds.width() + textbox->scroll + 1;
      }

      UIStringSelection selection = {};
      selection.carets[0]         = _UITextboxByteToColumn(textbox->text(), textbox->carets[0]);
      selection.carets[1]         = _UITextboxByteToColumn(textbox->text(), textbox->carets[1]);
      selection.colorBackground   = ui->theme.selected;
      selection.colorText         = ui->theme.textSelected;
      textBounds.l -= textbox->scroll;

      auto text = textbox->text();
      if (!text.empty())
         UIDrawString((UIPainter*)dp, textBounds, text,
                      (el->_flags & UIElement::disabled_flag) ? ui->theme.textDisabled : ui->theme.text, UIAlign::left,
                      el->_window->_focused == el ? &selection : NULL);
   } else if (msg == UIMessage::GET_CURSOR) {
      return (int)UICursor::text;
   } else if (msg == UIMessage::LEFT_DOWN) {
      int column = (el->_window->_cursor.x - el->_bounds.l + textbox->scroll - el->scale(ui_size::textbox_margin) +
                    ui->activeFont->glyphWidth / 2) /
                   ui->activeFont->glyphWidth;
      textbox->carets[0] = textbox->carets[1] = column <= 0 ? 0 : _UITextboxColumnToByte(textbox->text(), column);
      el->focus();
   } else if (msg == UIMessage::UPDATE) {
      el->repaint(NULL);
   } else if (msg == UIMessage::DEALLOCATE) {
      ;
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m       = (UIKeyTyped*)dp;
      bool        handled = true;

      if (textbox->rejectNextKey) {
         textbox->rejectNextKey = false;
         handled                = false;
      } else if (m->code == UIKeycode::BACKSPACE || m->code == UIKeycode::DEL) {
         if (textbox->carets[0] == textbox->carets[1]) {
            UITextboxMoveCaret(textbox, m->code == UIKeycode::BACKSPACE, el->_window->_ctrl);
         }

         UITextboxReplace(textbox, "", true);
      } else if (m->code == UIKeycode::LEFT || m->code == UIKeycode::RIGHT) {
         if (textbox->carets[0] == textbox->carets[1] || el->_window->_shift) {
            UITextboxMoveCaret(textbox, m->code == UIKeycode::LEFT, el->_window->_ctrl);
            if (!el->_window->_shift)
               textbox->carets[1] = textbox->carets[0];
         } else {
            textbox->carets[1 - el->_window->_shift] = textbox->carets[el->_window->_shift];
         }
      } else if (m->code == UIKeycode::HOME || m->code == UIKeycode::END) {
         if (m->code == UIKeycode::HOME) {
            textbox->carets[0] = 0;
         } else {
            textbox->carets[0] = textbox->text().size();
         }

         if (!el->_window->_shift) {
            textbox->carets[1] = textbox->carets[0];
         }
      } else if (m->code == UI_KEYCODE_LETTER('A') && el->_window->_ctrl) {
         textbox->carets[1] = 0;
         textbox->carets[0] = textbox->text().size();
      } else if (m->text.size() && !el->_window->_alt && !el->_window->_ctrl && m->text[0] >= 0x20) {
         UITextboxReplace(textbox, m->text, true);
      } else if ((m->code == UI_KEYCODE_LETTER('C') || m->code == UI_KEYCODE_LETTER('X') ||
                  m->code == UIKeycode::INSERT) &&
                 el->_window->_ctrl && !el->_window->_alt && !el->_window->_shift) {
         UITextboxCopyText(textbox);

         if (m->code == UI_KEYCODE_LETTER('X')) {
            UITextboxReplace(textbox, "", true);
         }
      } else if ((m->code == UI_KEYCODE_LETTER('V') && el->_window->_ctrl && !el->_window->_alt &&
                  !el->_window->_shift) ||
                 (m->code == UIKeycode::INSERT && !el->_window->_ctrl && !el->_window->_alt && el->_window->_shift)) {
         UITextboxPasteText(textbox, sel_target_t::clipboard);
      } else {
         handled = false;
      }

      if (handled) {
         el->repaint(NULL);
         return 1;
      }
   } else if (msg == UIMessage::RIGHT_DOWN) {
      int c0 = textbox->carets[0], c1 = textbox->carets[1];
      _UITextboxMessage(el, UIMessage::LEFT_DOWN, di, dp);

      if (c0 < c1 ? (textbox->carets[0] >= c0 && textbox->carets[0] < c1)
                  : (textbox->carets[0] >= c1 && textbox->carets[0] < c0)) {
         textbox->carets[0] = c0,
         textbox->carets[1] = c1; // Only move caret if clicking outside the existing selection.
      }

      UIMenu* menu = UIMenuCreate(el->_window, UIMenu::NO_SCROLL);
      UIMenuAddItem(menu, textbox->carets[0] == textbox->carets[1] ? UIElement::disabled_flag : 0, "Copy",
                    [=]() { UITextboxCopyText(textbox); });
      std::string paste = UI::ClipboardReadText(textbox->_window, sel_target_t::clipboard);
      UIMenuAddItem(menu, paste.empty() ? UIElement::disabled_flag : 0, "Paste",
                    [=]() { UITextboxPasteText(textbox, sel_target_t::clipboard); });
      UIMenuShow(menu);
   } else if (msg == UIMessage::MIDDLE_DOWN) {
      UITextboxPasteText(textbox, sel_target_t::primary);
      el->repaint(NULL);
      return 1;
   }

   return 0;
}

UITextbox::UITextbox(UIElement* parent, uint32_t flags)
   : UIElementCast<UITextbox>(parent, flags | tab_stop_flag, _UITextboxMessage, "Textbox")
   , carets({0, 0})
   , scroll(0)
   , rejectNextKey(false) {}

UITextbox* UITextboxCreate(UIElement* parent, uint32_t flags) {
   return new UITextbox(parent, flags);
}

// --------------------------------------------------
// MDI clients.
// --------------------------------------------------
int _UIMDIChildHitTest(UIMDIChild* mdiChild, int x, int y) {
   UIElement* el = mdiChild;
   auto [titleSize, borderSize, titleRect, contentRect] =
      ui_mdi_child_calculate_layout(el->_bounds, el->_window->_scale);
   int cornerSize = el->scale(ui_size::mdi_child_corner);
   if (!el->_bounds.contains(x, y) || contentRect.contains(x, y))
      return -1;
   else if (x < el->_bounds.l + cornerSize && y < el->_bounds.t + cornerSize)
      return 0b1010;
   else if (x > el->_bounds.r - cornerSize && y < el->_bounds.t + cornerSize)
      return 0b0110;
   else if (x < el->_bounds.l + cornerSize && y > el->_bounds.b - cornerSize)
      return 0b1001;
   else if (x > el->_bounds.r - cornerSize && y > el->_bounds.b - cornerSize)
      return 0b0101;
   else if (x < el->_bounds.l + borderSize)
      return 0b1000;
   else if (x > el->_bounds.r - borderSize)
      return 0b0100;
   else if (y < el->_bounds.t + borderSize)
      return 0b0010;
   else if (y > el->_bounds.b - borderSize)
      return 0b0001;
   else if (titleRect.contains(x, y))
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
      UIDrawControl((UIPainter*)dp, el->_bounds, UIControl::mdi_child, mdiChild->_title, 0, el->_window->_scale);
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
         ui_mdi_child_calculate_layout(el->_bounds, el->_window->_scale);

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
      int hitTest = _UIMDIChildHitTest(mdiChild, el->_window->_cursor.x, el->_window->_cursor.y);
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
      mdiChild->_drag_hit_test = _UIMDIChildHitTest(mdiChild, el->_window->_cursor.x, el->_window->_cursor.y);
      mdiChild->_drag_offset   = el->_bounds + UIRectangle(-el->_window->_cursor.x, -el->_window->_cursor.y);
   } else if (msg == UIMessage::LEFT_UP) {
      if (mdiChild->_mdi_bounds.l < 0)
         mdiChild->_mdi_bounds.r -= mdiChild->_mdi_bounds.l, mdiChild->_mdi_bounds.l = 0;
      if (mdiChild->_mdi_bounds.t < 0)
         mdiChild->_mdi_bounds.b -= mdiChild->_mdi_bounds.t, mdiChild->_mdi_bounds.t = 0;
      el->_parent->refresh();
   } else if (msg == UIMessage::MOUSE_DRAG) {
      if (mdiChild->_drag_hit_test > 0) {

#define _UI_MDI_CHILD_MOVE_EDGE(bit, edge, cursor, size, opposite, negate, minimum, offset)                         \
   if (mdiChild->_drag_hit_test & bit)                                                                              \
      mdiChild->_mdi_bounds.edge = mdiChild->_drag_offset.edge + el->_window->cursor - el->_parent->_bounds.offset; \
   if ((mdiChild->_drag_hit_test & bit) && mdiChild->_mdi_bounds.size() < minimum)                                  \
      mdiChild->_mdi_bounds.edge = mdiChild->_mdi_bounds.opposite negate minimum;

         _UI_MDI_CHILD_MOVE_EDGE(0b1000, l, _cursor.x, width, r, -, ui_size::mdi_child_minimum_width, l);
         _UI_MDI_CHILD_MOVE_EDGE(0b0100, r, _cursor.x, width, l, +, ui_size::mdi_child_minimum_width, l);
         _UI_MDI_CHILD_MOVE_EDGE(0b0010, t, _cursor.y, height, b, -, ui_size::mdi_child_minimum_height, t);
         _UI_MDI_CHILD_MOVE_EDGE(0b0001, b, _cursor.y, height, t, +, ui_size::mdi_child_minimum_height, t);
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

int UIMDIClient::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   UIMDIClient* client = (UIMDIClient*)el;

   if (msg == UIMessage::PAINT) {
      if (~el->_flags & UIMDIClient::_TRANSPARENT) {
         UIDrawBlock((UIPainter*)dp, el->_bounds, ui->theme.panel2);
      }
   } else if (msg == UIMessage::LAYOUT) {
      for (auto child : el->_children) {
         UIMDIChild* mdiChild = (UIMDIChild*)child;
         UI_ASSERT(mdiChild->_class_proc == UIMDIChild::_ClassMessageProc);

         if (mdiChild->_mdi_bounds == UIRectangle(0)) {
            int width  = mdiChild->message(UIMessage::GET_WIDTH, 0, 0);
            int height = mdiChild->message(UIMessage::GET_HEIGHT, width, 0);
            if (client->_cascade + width > el->_bounds.r || client->_cascade + height > el->_bounds.b)
               client->_cascade = 0;
            mdiChild->_mdi_bounds =
               UIRectangle(client->_cascade, client->_cascade + width, client->_cascade, client->_cascade + height);
            client->_cascade += el->scale(ui_size::mdi_cascade);
         }

         UIRectangle bounds = mdiChild->_mdi_bounds + UIRectangle(el->_bounds.l, el->_bounds.t);
         mdiChild->move(bounds, false);
      }
   } else if (msg == UIMessage::PRESSED_DESCENDENT) {
      UIMDIChild* child = (UIMDIChild*)dp;

      if (child && child != client->_active) {
         for (uint32_t i = 0; i < el->_children.size(); i++) {
            if (el->_children[i] == child) {
               el->_children.erase(el->_children.begin() + i);
               el->_children.push_back(child);
               break;
            }
         }

         client->_active = child;
         el->refresh();
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
      UIButton* closeButton = UIButtonCreate(this, UIButton::SMALL | non_client_flag, "X");
      closeButton->invoke   = [this]() { _UIMDIChildCloseButton(this); };
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

void _UIImageDisplayUpdateViewport(UIImageDisplay* display) {
   UIRectangle bounds = display->_bounds;
   bounds.r -= bounds.l, bounds.b -= bounds.t;

   float minimumZoomX = 1, minimumZoomY = 1;
   if (display->width > bounds.r)
      minimumZoomX = (float)bounds.r / display->width;
   if (display->height > bounds.b)
      minimumZoomY = (float)bounds.b / display->height;
   float minimumZoom = minimumZoomX < minimumZoomY ? minimumZoomX : minimumZoomY;

   if (display->zoom < minimumZoom || (display->_flags & UIImageDisplay::ZOOM_FIT)) {
      display->zoom = minimumZoom;
      display->_flags |= UIImageDisplay::ZOOM_FIT;
   }

   if (display->panX < 0)
      display->panX = 0;
   if (display->panY < 0)
      display->panY = 0;
   if (display->panX > display->width - bounds.r / display->zoom)
      display->panX = display->width - bounds.r / display->zoom;
   if (display->panY > display->height - bounds.b / display->zoom)
      display->panY = display->height - bounds.b / display->zoom;

   if (bounds.r && display->width * display->zoom <= bounds.r)
      display->panX = display->width / 2 - bounds.r / display->zoom / 2;
   if (bounds.b && display->height * display->zoom <= bounds.b)
      display->panY = display->height / 2 - bounds.b / display->zoom / 2;
}

int _UIImageDisplayMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   UIImageDisplay* display = (UIImageDisplay*)el;

   if (msg == UIMessage::GET_HEIGHT) {
      return display->height;
   } else if (msg == UIMessage::GET_WIDTH) {
      return display->width;
   } else if (msg == UIMessage::DEALLOCATE) {
      free(display->bits);
   } else if (msg == UIMessage::PAINT) {
      UIPainter* painter = (UIPainter*)dp;

      int w = el->_bounds.width(), h = el->_bounds.height();
      int x = _UILinearMap(0, display->panX, display->panX + w / display->zoom, 0, w) + el->_bounds.l;
      int y = _UILinearMap(0, display->panY, display->panY + h / display->zoom, 0, h) + el->_bounds.t;

      UIRectangle image =
         UIRectangle(x, x + (int)(display->width * display->zoom), y, (int)(y + display->height * display->zoom));
      UIRectangle bounds = intersection(painter->clip, intersection(display->_bounds, image));
      if (!bounds.valid())
         return 0;

      if (display->zoom == 1) {
         uint32_t* lineStart       = (uint32_t*)painter->bits + bounds.t * painter->width + bounds.l;
         uint32_t* sourceLineStart = display->bits + (bounds.l - image.l) + display->width * (bounds.t - image.t);

         for (int i = 0; i < bounds.b - bounds.t; i++, lineStart += painter->width, sourceLineStart += display->width) {
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
         float     zr          = 1.0f / display->zoom;
         uint32_t* destination = (uint32_t*)painter->bits;

         for (int i = bounds.t; i < bounds.b; i++) {
            int ty = (i - image.t) * zr;

            for (int j = bounds.l; j < bounds.r; j++) {
               int tx                              = (j - image.l) * zr;
               destination[i * painter->width + j] = display->bits[ty * display->width + tx];
            }
         }
      }
   } else if (msg == UIMessage::MOUSE_WHEEL && (el->_flags & UIImageDisplay::INTERACTIVE)) {
      display->_flags &= ~UIImageDisplay::ZOOM_FIT;
      int   divisions   = -di / 72;
      float factor      = 1;
      float perDivision = el->_window->_ctrl ? 2.0f : el->_window->_alt ? 1.01f : 1.2f;
      while (divisions > 0)
         factor *= perDivision, divisions--;
      while (divisions < 0)
         factor /= perDivision, divisions++;
      if (display->zoom * factor > 64)
         factor = 64 / display->zoom;
      int mx = el->_window->_cursor.x - el->_bounds.l;
      int my = el->_window->_cursor.y - el->_bounds.t;
      display->zoom *= factor;
      display->panX -= mx / display->zoom * (1 - factor);
      display->panY -= my / display->zoom * (1 - factor);
      _UIImageDisplayUpdateViewport(display);
      display->repaint(NULL);
   } else if (msg == UIMessage::LAYOUT && (el->_flags & UIImageDisplay::INTERACTIVE)) {
      UIRectangle bounds = display->_bounds;
      bounds.r -= bounds.l, bounds.b -= bounds.t;
      display->panX -= (bounds.r - display->previousWidth) / 2 / display->zoom;
      display->panY -= (bounds.b - display->previousHeight) / 2 / display->zoom;
      display->previousWidth = bounds.r, display->previousHeight = bounds.b;
      _UIImageDisplayUpdateViewport(display);
   } else if (msg == UIMessage::GET_CURSOR && (el->_flags & UIImageDisplay::INTERACTIVE) &&
              (el->_bounds.width() < display->width * display->zoom ||
               el->_bounds.height() < display->height * display->zoom)) {
      return (int)UICursor::hand;
   } else if (msg == UIMessage::MOUSE_DRAG) {
      display->panX -= (el->_window->_cursor.x - display->previousPanPointX) / display->zoom;
      display->panY -= (el->_window->_cursor.y - display->previousPanPointY) / display->zoom;
      _UIImageDisplayUpdateViewport(display);
      display->previousPanPointX = el->_window->_cursor.x;
      display->previousPanPointY = el->_window->_cursor.y;
      el->repaint(NULL);
   } else if (msg == UIMessage::LEFT_DOWN) {
      display->_flags &= ~UIImageDisplay::ZOOM_FIT;
      display->previousPanPointX = el->_window->_cursor.x;
      display->previousPanPointY = el->_window->_cursor.y;
   }

   return 0;
}

void UIImageDisplaySetContent(UIImageDisplay* display, uint32_t* bits, size_t width, size_t height, size_t stride) {
   free(display->bits);

   display->bits   = (uint32_t*)malloc(width * height * 4);
   display->width  = width;
   display->height = height;

   uint32_t* destination = display->bits;
   uint32_t* source      = bits;

   for (uintptr_t row = 0; row < height; row++, source += stride / 4) {
      for (uintptr_t i = 0; i < width; i++) {
         *destination++ = source[i];
      }
   }

   display->measurements_changed(3);
   display->repaint(NULL);
}

UIImageDisplay::UIImageDisplay(UIElement* parent, uint32_t flags, uint32_t* bits, size_t width, size_t height,
                               size_t stride)
   : UIElementCast<UIImageDisplay>(parent, flags, _UIImageDisplayMessage, "ImageDisplay")
   , bits(bits)
   , width(width)
   , height(height)
   , panX(0)
   , panY(0)
   , zoom(1)
   , previousWidth(0)
   , previousHeight(0)
   , previousPanPointX(0)
   , previousPanPointY(0) {
   UIImageDisplaySetContent(this, bits, width, height, stride);
}

UIImageDisplay* UIImageDisplayCreate(UIElement* parent, uint32_t flags, uint32_t* bits, size_t width, size_t height,
                                     size_t stride) {
   return new UIImageDisplay(parent, flags, bits, width, height, stride);
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
      el->repaint(NULL);
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, el->_children[0]->_bounds, UIControl::modal_popup, {}, 0, el->_window->_scale);
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* typed = (UIKeyTyped*)dp;

      if (el->_window->_ctrl)
         return 0;
      if (el->_window->_shift)
         return 0;

      if (!ui->dialogCanExit) {
      } else if (!el->_window->_alt && typed->code == UIKeycode::ESCAPE) {
         ui->dialogResult = "__C";
         return 1;
      } else if (!el->_window->_alt && typed->code == UIKeycode::ENTER) {
         ui->dialogResult = "__D";
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

               if (!button->label.empty() && (button->label[0] == c0 || button->label[0] == c1)) {
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

void _UIDialogButtonInvoke(const char* label) {
   ui->dialogResult = label;
}

int _UIDialogDefaultButtonMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::PAINT && el->_window->_focused->_class_proc != UIButton::_ClassMessageProc) {
      el->_flags |= UIButton::CHECKED;
      el->_class_proc(el, msg, di, dp);
      el->_flags &= ~UIButton::CHECKED;
      return 1;
   }

   return 0;
}

int _UIDialogTextboxMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   UITextbox*       textbox = (UITextbox*)el;
   std::string_view text    = textbox->text();

   if (msg == UIMessage::VALUE_CHANGED) {
      auto   sz     = text.size();
      char** buffer = (char**)el->_cp;
      *buffer       = (char*)realloc(*buffer, sz + 1); // update user pointer to hold the textbox text

      for (size_t i = 0; i < sz; i++)
         (*buffer)[i] = text[i];
      (*buffer)[sz] = 0;
   } else if (msg == UIMessage::UPDATE && di == UIUpdate::focused && el->_window->_focused == el) {
      textbox->carets[1] = 0;
      textbox->carets[0] = text.size();
      el->repaint(NULL);
   }

   return 0;
}

const char* UIDialogShow(UIWindow* window, uint32_t flags, const char* format, ...) {
   // Create the dialog wrapper and panel.

   UI_ASSERT(!window->_dialog);
   window->_dialog = UIElementCreate(sizeof(UIElement), window, 0, _UIDialogWrapperMessage, "DialogWrapper");
   UIPanel* panel  = UIPanelCreate(window->_dialog, UIPanel::MEDIUM_SPACING | UIPanel::COLOR_1);
   panel->_border  = UIRectangle(ui_size::pane_medium_border * 2);
   window->_children[0]->_flags |= UIElement::disabled_flag;

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
         row       = UIPanelCreate(panel, UIPanel::HORIZONTAL | UIElement::h_fill);
         row->_gap = ui_size::pane_small_gap;
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
            button->invoke = [label]() { _UIDialogButtonInvoke(label); };
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
               UITextboxReplace(textbox, *buffer, false);
            textbox->_cp = buffer; // when the textbox text is updated, `*buffer` will contain a `char*` to the string
            textbox->_user_proc = _UIDialogTextboxMessage;
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

   window->_dialog_old_focus = window->_focused;
   (focus ? focus : window->_dialog)->focus();

   // Run the modal message loop.

   int result;
   ui->dialogResult  = NULL;
   ui->dialogCanExit = buttonCount != 0;
   for (int i = 1; i <= 3; i++)
      window->set_pressed(NULL, i);
   window->refresh();
   UI::Update();
   while (!ui->dialogResult && UI::MessageLoopSingle(&result))
      ;
   ui->quit = !ui->dialogResult;

   // Check for cancel/default action.

   if (buttonCount == 1 && defaultButton && !cancelButton) {
      cancelButton = defaultButton;
   }

   if (!ui->dialogResult) {
   } else if (ui->dialogResult[0] == '_' && ui->dialogResult[1] == '_' && ui->dialogResult[2] == 'C' &&
              ui->dialogResult[3] == 0 && cancelButton) {
      ui->dialogResult = (const char*)cancelButton->_cp;
   } else if (ui->dialogResult[0] == '_' && ui->dialogResult[1] == '_' && ui->dialogResult[2] == 'D' &&
              ui->dialogResult[3] == 0 && defaultButton) {
      ui->dialogResult = (const char*)defaultButton->_cp;
   }

   // Destroy the dialog.

   window->_children[0]->_flags &= ~UIElement::disabled_flag;
   window->_dialog->destroy();
   window->_dialog = NULL;
   window->refresh();
   if (window->_dialog_old_focus)
      window->_dialog_old_focus->focus();
   return ui->dialogResult ? ui->dialogResult : "";
}

// --------------------------------------------------
// Menus (common).
// --------------------------------------------------

bool _UIMenusClose() {
   bool anyClosed = false;

   if (ui) {
      UIWindow* window = ui->windows;

      while (window) {
         if (window->_flags & UIWindow::MENU) {
            if constexpr (UIInspector::enabled())
               ui->inspector.notify_destroyed_window(window);

            window->destroy();
            anyClosed = true;
         }

         window = window->_next;
      }
   }

   return anyClosed;
}

int _UIMenuItemMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::CLICKED) {
      _UIMenusClose();
   }

   return 0;
}

int _UIMenuMessage(UIElement* el, UIMessage msg, int di, void* dp) {
   UIMenu* menu = (UIMenu*)el;

   if (msg == UIMessage::GET_WIDTH) {
      int width = 0;

      for (auto child : el->_children) {
         if (~child->_flags & UIElement::non_client_flag) {
            int w = child->message(UIMessage::GET_WIDTH, 0, 0);
            if (w > width)
               width = w;
         }
      }

      return width + 4 + ui_size::scroll_bar;
   } else if (msg == UIMessage::GET_HEIGHT) {
      int height = 0;

      for (auto child : el->_children) {
         if (~child->_flags & UIElement::non_client_flag) {
            height += child->message(UIMessage::GET_HEIGHT, 0, 0);
         }
      }

      return height + 4;
   } else if (msg == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, el->_bounds, UIControl::menu, {}, 0, el->_window->_scale);
   } else if (msg == UIMessage::LAYOUT) {
      int position      = el->_bounds.t + 2 - menu->vScroll->_position;
      int totalHeight   = 0;
      int scrollBarSize = (menu->_flags & UIMenu::NO_SCROLL) ? 0 : ui_size::scroll_bar;

      for (auto child : el->_children) {
         if (~child->_flags & UIElement::non_client_flag) {
            int height = child->message(UIMessage::GET_HEIGHT, 0, 0);
            child->move(UIRectangle(el->_bounds.l + 2, el->_bounds.r - scrollBarSize - 2, position, position + height),
                        false);
            position += height;
            totalHeight += height;
         }
      }

      UIRectangle scrollBarBounds = el->_bounds;
      scrollBarBounds.l           = scrollBarBounds.r - el->scale(scrollBarSize);
      menu->vScroll->set_maximum(totalHeight);
      menu->vScroll->set_page(el->_bounds.height());
      menu->vScroll->move(scrollBarBounds, true);
   } else if (msg == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if (m->code == UIKeycode::ESCAPE) {
         _UIMenusClose();
         return 1;
      }
   } else if (msg == UIMessage::MOUSE_WHEEL) {
      return menu->vScroll->message(msg, di, dp);
   } else if (msg == UIMessage::SCROLLED) {
      el->refresh();
   }

   return 0;
}

void UIMenuAddItem(UIMenu* menu, uint32_t flags, std::string_view label, std::function<void()> invoke) {
   UIButton* button   = UIButtonCreate(menu, flags | UIButton::MENU_ITEM, label);
   button->invoke     = std::move(invoke);
   button->_user_proc = _UIMenuItemMessage;
}

void _UIMenuPrepare(UIMenu* menu, int* width, int* height) {
   *width  = menu->message(UIMessage::GET_WIDTH, 0, 0);
   *height = menu->message(UIMessage::GET_HEIGHT, 0, 0);

   if (menu->_flags & UIMenu::PLACE_ABOVE) {
      menu->pointY -= *height;
   }
}

UIMenu::UIMenu(UIElement* parent, uint32_t flags)
   : UIElementCast<UIMenu>(UIWindowCreate(parent->_window, UIWindow::MENU, 0, 0, 0), flags, _UIMenuMessage, "Menu")
   , vScroll(UIScrollBarCreate(this, non_client_flag))
   , parentWindow(parent->_window) {
   if (parent->_parent) {
      UIRectangle screenBounds = parent->screen_bounds();
      pointX                   = screenBounds.l;
      pointY                   = (flags & UIMenu::PLACE_ABOVE) ? (screenBounds.t + 1) : (screenBounds.b - 1);
   } else {
      int x = 0, y = 0;
      parent->_window->get_screen_position(&x, &y);

      pointX = parent->_window->_cursor.x + x;
      pointY = parent->_window->_cursor.y + y;
   }
}

UIMenu* UIMenuCreate(UIElement* parent, uint32_t flags) {
   return new UIMenu(parent, flags);
}

// --------------------------------------------------
// Miscellaneous core functions.
// --------------------------------------------------

UIRectangle UIElement::screen_bounds() {
   int x = 0, y = 0;
   _window->get_screen_position(&x, &y);
   return _bounds + UIRectangle(x, y);
}

void UIWindowRegisterShortcut(UIWindow* window, UIShortcut shortcut) {
   window->_shortcuts.push_back(std::move(shortcut));
}

void UI::Update() {
   UIWindow*  window = ui->windows;
   UIWindow** link   = &ui->windows;

   while (window) {
      UIWindow* next = window->_next;

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
            UIPainter painter = {.clip =
                                    intersection(ui_rect_2s(window->_width, window->_height), window->_update_region),
                                 .bits   = window->_bits.data(),
                                 .width  = window->_width,
                                 .height = window->_height};
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

void UIWindow::set_pressed(UIElement* el, int button) {
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

bool UIMenusOpen() {
   UIWindow* window = ui->windows;

   while (window) {
      if (window->_flags & UIWindow::MENU) {
         return true;
      }

      window = window->_next;
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

   if (_pressed) {
      if (msg == UIMessage::MOUSE_MOVE) {
         _pressed->message(UIMessage::MOUSE_DRAG, di, dp);
      } else if (msg == UIMessage::LEFT_UP && _pressed_button == 1) {
         if (_hovered == _pressed) {
            _pressed->message(UIMessage::CLICKED, di, dp);
            if (ui->quit || ui->dialogResult)
               goto end;
         }

         if (_pressed) {
            _pressed->message(UIMessage::LEFT_UP, di, dp);
            if (ui->quit || ui->dialogResult)
               goto end;
            set_pressed(NULL, 1);
         }
      } else if (msg == UIMessage::MIDDLE_UP && _pressed_button == 2) {
         _pressed->message(UIMessage::MIDDLE_UP, di, dp);
         if (ui->quit || ui->dialogResult)
            goto end;
         set_pressed(NULL, 2);
      } else if (msg == UIMessage::RIGHT_UP && _pressed_button == 3) {
         _pressed->message(UIMessage::RIGHT_UP, di, dp);
         if (ui->quit || ui->dialogResult)
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

      if (ui->quit || ui->dialogResult)
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
         if ((_flags & UIWindow::MENU) || !_UIMenusClose()) {
            set_pressed(loc, 1);
            loc->message(UIMessage::LEFT_DOWN, di, dp);
         }
      } else if (msg == UIMessage::MIDDLE_DOWN) {
         if ((_flags & UIWindow::MENU) || !_UIMenusClose()) {
            set_pressed(loc, 2);
            loc->message(UIMessage::MIDDLE_DOWN, di, dp);
         }
      } else if (msg == UIMessage::RIGHT_DOWN) {
         if ((_flags & UIWindow::MENU) || !_UIMenusClose()) {
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

         if (!handled && !UIMenusOpen() && msg == UIMessage::KEY_TYPED) {
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

      if (ui->quit || ui->dialogResult)
         goto end;

      if (loc != _hovered) {
         UIElement* previous = _hovered;
         _hovered            = loc;
         previous->message(UIMessage::UPDATE, UIUpdate::hovered, 0);
         _hovered->message(UIMessage::UPDATE, UIUpdate::hovered, 0);
      }
   }

end:
   UI::Update();
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

void UIDrawGlyph(UIPainter* painter, int x0, int y0, int c, uint32_t color) {
#ifdef UI_FREETYPE
   UIFont* font = ui->activeFont;

   if (font->isFreeType) {
      if (c < 0 || c >= max_glyphs)
         c = '?';

      if (c == '\r')
         c = ' ';

      if (!font->glyphsRendered[c]) {
         FT_Load_Char(font->font,
                      c == 24   ? 0x2191
                      : c == 25 ? 0x2193
                      : c == 26 ? 0x2192
                      : c == 27 ? 0x2190
                                : c,
                      FT_LOAD_DEFAULT);
         FT_Render_Glyph(font->font->glyph, ft_render_mode);

         FT_Bitmap_Copy(ui->ft, &font->font->glyph->bitmap, &font->glyphs[c]);
         font->glyphOffsetsX[c]  = font->font->glyph->bitmap_left;
         font->glyphOffsetsY[c]  = font->font->size->metrics.ascender / 64 - font->font->glyph->bitmap_top;
         font->glyphsRendered[c] = true;
      }

      FT_Bitmap* bitmap = &font->glyphs[c];
      x0 += font->glyphOffsetsX[c], y0 += font->glyphOffsetsY[c];

      for (int y = 0; y < (int)bitmap->rows; y++) {
         if (y0 + y < painter->clip.t)
            continue;
         if (y0 + y >= painter->clip.b)
            break;

         int width = bitmap->pixel_mode == FT_PIXEL_MODE_LCD ? bitmap->width / 3 : bitmap->width;

         for (int x = 0; x < width; x++) {
            if (x0 + x < painter->clip.l)
               continue;
            if (x0 + x >= painter->clip.r)
               break;

            uint32_t* destination = painter->bits + (x0 + x) + (y0 + y) * painter->width;
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

      return;
   }
#endif // UI_FREETYPE

   if (c < 0 || c > 127)
      c = '?';

   UIRectangle rectangle = intersection(painter->clip, UIRectangle(x0, x0 + 8, y0, y0 + 16));

   const uint8_t* data = (const uint8_t*)_uiFont + c * 16;

   for (int i = rectangle.t; i < rectangle.b; i++) {
      uint32_t* bits = painter->bits + i * painter->width + rectangle.l;
      uint8_t   byte = data[i - y0];

      for (int j = rectangle.l; j < rectangle.r; j++) {
         if (byte & (1 << (j - x0))) {
            *bits = color;
         }

         bits++;
      }
   }
}

UIFont* UIFontCreate(const char* cPath, uint32_t size) {
   if (!cPath || !*cPath) {
      return nullptr;
   }

   UIFontSpec spec{cPath, size};

   if (auto it = ui->font_map.find(spec); it != ui->font_map.end())
      return it->second.get();

   unique_ptr<UIFont> font = make_unique<UIFont>();

   font->glyphWidth  = 9;
   font->glyphHeight = 16;

#ifdef UI_FREETYPE
   font->glyphs         = make_unique<FT_Bitmap[]>(max_glyphs);
   font->glyphsRendered = make_unique<bool[]>(max_glyphs);
   font->glyphOffsetsX  = make_unique<int[]>(max_glyphs);
   font->glyphOffsetsY  = make_unique<int[]>(max_glyphs);
   if (cPath) {
      int ret = FT_New_Face(ui->ft, cPath, 0, &font->font);
      if (ret == 0) {
         FT_Select_Charmap(font->font, FT_ENCODING_UNICODE);
         if (FT_HAS_FIXED_SIZES(font->font) && font->font->num_fixed_sizes) {
            // Look for the smallest strike that's at least `size`.
            int j = 0;

            for (int i = 0; i < font->font->num_fixed_sizes; i++) {
               if ((uint32_t)font->font->available_sizes[i].height >= size &&
                   font->font->available_sizes[i].y_ppem < font->font->available_sizes[j].y_ppem) {
                  j = i;
               }
            }

            FT_Set_Pixel_Sizes(font->font, font->font->available_sizes[j].x_ppem / 64,
                               font->font->available_sizes[j].y_ppem / 64);
         } else {
            FT_Set_Char_Size(font->font, 0, size * 64, 100, 100);
         }

         FT_Load_Char(font->font, 'a', FT_LOAD_DEFAULT);
         font->glyphWidth  = font->font->glyph->advance.x / 64;
         font->glyphHeight = (font->font->size->metrics.ascender - font->font->size->metrics.descender) / 64;
         font->isFreeType  = true;
      } else {
         std_print("Cannot load font {} : {}\n", cPath, ret);
         return nullptr;
      }
   }
#endif // UI_FREETYPE
   UIFont* f = font.get();
   ui->font_map.emplace(std::move(spec), std::move(font));
   return f;
}

UIFont* UIFontActivate(UIFont* font) {
   UIFont* previous = ui->activeFont;
   ui->activeFont   = font;
   return previous;
}


UIFont::~UIFont() {
#ifdef UI_FREETYPE
   FT_Done_Face(font);
   for (size_t i = 0; i < max_glyphs; ++i)
      FT_Bitmap_Done(ui->ft, &glyphs[i]);
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
   if (!ui->inspector._target) {
      return 0;
   }

   if (msg == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m     = (UITableGetItem*)dp;
      int             index = m->index;
      auto [el, depth]      = _UIInspectorFindNthElement(ui->inspector._target, &index);
      if (!el)
         return 0;

      if (m->column == 0) {
         return m->format_to("{}{}", std::string_view{"                ", depth * 2}, el->_class_name);
      } else if (m->column == 1) {
         const auto& b = el->_bounds;
         return m->format_to("{}:{}, {}:{}", b.l, b.r, b.t, b.b);
      } else if (m->column == 2) {
         return m->format_to("{}{:c}", el->_id, el->_window->_focused == el ? '*' : ' ');
      }
   } else if (msg == UIMessage::MOUSE_MOVE) {
      int        index = ui->inspector._table->hittest(table->_window->_cursor.x, table->_window->_cursor.y);
      UIElement* el    = NULL;
      if (index >= 0)
         el = _UIInspectorFindNthElement(ui->inspector._target, &index).first;
      UIWindow* window       = ui->inspector._target;
      UIPainter painter      = {0};
      window->_update_region = window->_bounds;
      painter.bits           = window->_bits.data();
      painter.width          = window->_width;
      painter.height         = window->_height;
      painter.clip           = ui_rect_2s(window->_width, window->_height);

      for (uint32_t i = 0; i < window->_width * window->_height; i++) {
         window->_bits[i] = 0xFF00FF;
      }

      window->paint(&painter);
      painter.clip = ui_rect_2s(window->_width, window->_height);

      if (el) {
         UIDrawInvert(&painter, el->_bounds);
         UIDrawInvert(&painter, el->_bounds + ui_rect_1i(4));
      }

      window->endpaint(&painter);
   }

   return 0;
}

void UIInspector::create() {
   if (!_enabled || _inspector)
      return;

   _inspector             = UIWindowCreate(0, UIWindow::INSPECTOR, "Inspector", 0, 0);
   UISplitPane* splitPane = UISplitPaneCreate(_inspector, 0, 0.5f);
   _table                 = UITableCreate(splitPane, 0, "Class\tBounds\tID");
   _table->_user_proc     = _UIInspectorTableMessage;
   _log                   = UICodeCreate(splitPane, 0);
   _log->set_font(ui->defaultFont);
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
      ui->inspector.refresh();
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
   UI::MessageLoopSingle(&result);
}

void UIAutomationKeyboardTypeSingle(intptr_t code, bool ctrl, bool shift, bool alt) {
   UIWindow*  window = ui->windows; // TODO Get the focused window.
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
   UIWindow* window = ui->windows; // TODO Get the focused window.

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

void _UIWindowDestroyCommon(UIWindow* window) {}

void _UIInitialiseCommon(const UIConfig& cfg, const std::string& default_font_path) {
   ui->theme = uiThemeClassic;

#ifdef UI_FREETYPE
   FT_Init_FreeType(&ui->ft);
#endif

   ui->defaultFont = UIFontCreate(default_font_path.c_str(), cfg.default_font_size);
   UIFontActivate(ui->defaultFont);
}



void _UIWindowAdd(UIWindow* window) {
   window->_scale   = 1.0f;
   window->_window  = window;
   window->_hovered = window;
   window->_next    = ui->windows;
   ui->windows      = window;
}

int UIWindow::_ClassMessageProcCommon(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::LAYOUT && !el->_children.empty()) {
      el->_children[0]->move(el->_bounds, false);
      if (el->_window->_dialog)
         el->_window->_dialog->move(el->_bounds, false);
      el->repaint(NULL);
   } else if (msg == UIMessage::GET_CHILD_STABILITY) {
      return 3; // Both width and height of the child el are ignored.
   }

   return 0;
}

int UIMessageLoop() {
   UI::Update();
#ifdef UI_AUTOMATION_TESTS
   return UIAutomationRunTests();
#else
   int result = 0;
   while (!ui->quit && UI::MessageLoopSingle(&result))
      ui->dialogResult = NULL;
   return result;
#endif
}

UIWindow::UIWindow(UIElement* parent, uint32_t flags, message_proc_t message_proc, const char* cClassName)
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
   , _ctrl(false)
   , _shift(false)
   , _alt(false)
   , _update_region(0) {}

UIWindow::~UIWindow() {}

// --------------------------------------------------
// Platform layers.
// --------------------------------------------------

#ifdef UI_LINUX

int UIWindow::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::DEALLOCATE) {
      UIWindow* window = (UIWindow*)el;
      _UIWindowDestroyCommon(window);
      window->_image->data = NULL;
      XDestroyImage(window->_image);
      XDestroyIC(window->_xic);
      XDestroyWindow(ui->display, ((UIWindow*)el)->_xwindow);
      return 0;
   }

   return UIWindow::_ClassMessageProcCommon(el, msg, di, dp);
}

UIWindow* UIWindowCreate(UIWindow* owner, uint32_t flags, const char* cTitle, int _width, int _height) {
   _UIMenusClose();

   UIWindow* window = new UIWindow(NULL, flags | UIElement::window_flag, UIWindow::_ClassMessageProc, "Window");
   _UIWindowAdd(window);
   if (owner)
      window->_scale = owner->_scale;

   int width  = (flags & UIWindow::MENU) ? 1 : _width ? _width : 800;
   int height = (flags & UIWindow::MENU) ? 1 : _height ? _height : 600;

   XSetWindowAttributes attributes = {};
   attributes.override_redirect    = flags & UIWindow::MENU;

   window->_xwindow = XCreateWindow(ui->display, DefaultRootWindow(ui->display), 0, 0, width, height, 0, 0, InputOutput,
                                    CopyFromParent, CWOverrideRedirect, &attributes);
   if (cTitle)
      XStoreName(ui->display, window->_xwindow, cTitle);
   XSelectInput(ui->display, window->_xwindow,
                SubstructureNotifyMask | ExposureMask | PointerMotionMask | ButtonPressMask | ButtonReleaseMask |
                   KeyPressMask | KeyReleaseMask | StructureNotifyMask | EnterWindowMask | LeaveWindowMask |
                   ButtonMotionMask | KeymapStateMask | FocusChangeMask | PropertyChangeMask);

   if (flags & UIWindow::MAXIMIZE) {
      Atom atoms[2] = {XInternAtom(ui->display, "_NET_WM_STATE_MAXIMIZED_HORZ", 0),
                       XInternAtom(ui->display, "_NET_WM_STATE_MAXIMIZED_VERT", 0)};
      XChangeProperty(ui->display, window->_xwindow, XInternAtom(ui->display, "_NET_WM_STATE", 0), XA_ATOM, 32,
                      PropModeReplace, (unsigned char*)atoms, 2);
   }

   if (~flags & UIWindow::MENU) {
      XMapRaised(ui->display, window->_xwindow);
   }

   if (flags & UIWindow::CENTER_IN_OWNER) {
      int x = 0, y = 0;
      owner->get_screen_position(&x, &y);
      XMoveResizeWindow(ui->display, window->_xwindow, x + owner->_width / 2 - width / 2,
                        y + owner->_height / 2 - height / 2, width, height);
   }

   XSetWMProtocols(ui->display, window->_xwindow, &ui->windowClosedID, 1);
   window->_image = XCreateImage(ui->display, ui->visual, 24, ZPixmap, 0, NULL, 10, 10, 32, 0);

   window->_xic = XCreateIC(ui->xim, XNInputStyle, XIMPreeditNothing | XIMStatusNothing, XNClientWindow,
                            window->_xwindow, XNFocusWindow, window->_xwindow, nullptr);

   int dndVersion = 4;
   XChangeProperty(ui->display, window->_xwindow, ui->dndAwareID, XA_ATOM, 32 /* bits */, PropModeReplace,
                   (uint8_t*)&dndVersion, 1);

   return window;
}

Display* _UIX11GetDisplay() {
   return ui->display;
}

UIWindow* _UIFindWindow(Window window) {
   UIWindow* w = ui->windows;

   while (w) {
      if (w->_xwindow == window) {
         return w;
      }

      w = w->_next;
   }

   return NULL;
}

void UI::ClipboardWriteText(UIWindow* window, std::string text, sel_target_t t) {
   ui->pasteText = std::move(text);
   Atom atom     = (t == sel_target_t::clipboard) ? ui->clipboardID : ui->primaryID;
   XSetSelectionOwner(ui->display, atom, window->_xwindow, 0);
}

std::string UI::ClipboardReadText(UIWindow* window, sel_target_t t) {
   Atom atom = (t == sel_target_t::clipboard) ? ui->clipboardID : ui->primaryID;

   Window clipboardOwner = XGetSelectionOwner(ui->display, atom);

   if (clipboardOwner == None) {
      return {};
   }

   if (_UIFindWindow(clipboardOwner)) {
      return ui->pasteText;
   }

   XConvertSelection(ui->display, atom, XA_STRING, ui->xSelectionDataID, window->_xwindow, CurrentTime);
   XSync(ui->display, 0);
   XNextEvent(ui->display, &ui->copyEvent);

   // Hack to get around the fact that PropertyNotify arrives before SelectionNotify.
   // We need PropertyNotify for incremental transfers.
   while (ui->copyEvent.type == PropertyNotify) {
      XNextEvent(ui->display, &ui->copyEvent);
   }

   if (ui->copyEvent.type == SelectionNotify && ui->copyEvent.xselection.selection == atom &&
       ui->copyEvent.xselection.property) {
      Atom target;
      // This `itemAmount` is actually `bytes_after_return`
      unsigned long size, itemAmount;
      char*         data;
      int           format;
      XGetWindowProperty(ui->copyEvent.xselection.display, ui->copyEvent.xselection.requestor,
                         ui->copyEvent.xselection.property, 0L, ~0L, 0, AnyPropertyType, &target, &format, &size,
                         &itemAmount, (unsigned char**)&data);

      // non incremental transfer
      // ------------------------
      if (target != ui->incrID) {
         std::string res;
         res.resize(size);
         memcpy(res.data(), data, size);
         XFree(data);
         XDeleteProperty(ui->copyEvent.xselection.display, ui->copyEvent.xselection.requestor,
                         ui->copyEvent.xselection.property);
         return res;
      }

      // incremental transfer
      // --------------------
      XFree(data);
      XDeleteProperty(ui->display, ui->copyEvent.xselection.requestor, ui->copyEvent.xselection.property);
      XSync(ui->display, 0);

      size = 0;
      std::string res;

      while (true) {
         // TODO Timeout.
         XNextEvent(ui->display, &ui->copyEvent);

         if (ui->copyEvent.type == PropertyNotify) {
            // The other case - PropertyDelete would be caused by us and can be ignored
            if (ui->copyEvent.xproperty.state == PropertyNewValue) {
               unsigned long chunkSize;

               // Note that this call deletes the property.
               XGetWindowProperty(ui->display, ui->copyEvent.xproperty.window, ui->copyEvent.xproperty.atom, 0L, ~0L,
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

unique_ptr<UI> UIInitialise(const UIConfig& cfg) {
   ui = new UI;

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

   ui->default_font_path = font_path;
   _UIInitialiseCommon(cfg, font_path);

   XInitThreads();

   ui->display = XOpenDisplay(NULL);
   ui->visual  = XDefaultVisual(ui->display, 0);

   ui->windowClosedID   = XInternAtom(ui->display, "WM_DELETE_WINDOW", 0);
   ui->primaryID        = XInternAtom(ui->display, "PRIMARY", 0);
   ui->dndEnterID       = XInternAtom(ui->display, "XdndEnter", 0);
   ui->dndPositionID    = XInternAtom(ui->display, "XdndPosition", 0);
   ui->dndStatusID      = XInternAtom(ui->display, "XdndStatus", 0);
   ui->dndActionCopyID  = XInternAtom(ui->display, "XdndActionCopy", 0);
   ui->dndDropID        = XInternAtom(ui->display, "XdndDrop", 0);
   ui->dndSelectionID   = XInternAtom(ui->display, "XdndSelection", 0);
   ui->dndFinishedID    = XInternAtom(ui->display, "XdndFinished", 0);
   ui->dndAwareID       = XInternAtom(ui->display, "XdndAware", 0);
   ui->uriListID        = XInternAtom(ui->display, "text/uri-list", 0);
   ui->plainTextID      = XInternAtom(ui->display, "text/plain", 0);
   ui->clipboardID      = XInternAtom(ui->display, "CLIPBOARD", 0);
   ui->xSelectionDataID = XInternAtom(ui->display, "XSEL_DATA", 0);
   ui->textID           = XInternAtom(ui->display, "TEXT", 0);
   ui->targetID         = XInternAtom(ui->display, "TARGETS", 0);
   ui->incrID           = XInternAtom(ui->display, "INCR", 0);

   ui->cursors[(uint32_t)UICursor::arrow]             = XCreateFontCursor(ui->display, XC_left_ptr);
   ui->cursors[(uint32_t)UICursor::text]              = XCreateFontCursor(ui->display, XC_xterm);
   ui->cursors[(uint32_t)UICursor::split_v]           = XCreateFontCursor(ui->display, XC_sb_v_double_arrow);
   ui->cursors[(uint32_t)UICursor::split_h]           = XCreateFontCursor(ui->display, XC_sb_h_double_arrow);
   ui->cursors[(uint32_t)UICursor::flipped_arrow]     = XCreateFontCursor(ui->display, XC_right_ptr);
   ui->cursors[(uint32_t)UICursor::cross_hair]        = XCreateFontCursor(ui->display, XC_crosshair);
   ui->cursors[(uint32_t)UICursor::hand]              = XCreateFontCursor(ui->display, XC_hand1);
   ui->cursors[(uint32_t)UICursor::resize_up]         = XCreateFontCursor(ui->display, XC_top_side);
   ui->cursors[(uint32_t)UICursor::resize_left]       = XCreateFontCursor(ui->display, XC_left_side);
   ui->cursors[(uint32_t)UICursor::resize_up_right]   = XCreateFontCursor(ui->display, XC_top_right_corner);
   ui->cursors[(uint32_t)UICursor::resize_up_left]    = XCreateFontCursor(ui->display, XC_top_left_corner);
   ui->cursors[(uint32_t)UICursor::resize_down]       = XCreateFontCursor(ui->display, XC_bottom_side);
   ui->cursors[(uint32_t)UICursor::resize_right]      = XCreateFontCursor(ui->display, XC_right_side);
   ui->cursors[(uint32_t)UICursor::resize_down_left]  = XCreateFontCursor(ui->display, XC_bottom_left_corner);
   ui->cursors[(uint32_t)UICursor::resize_down_right] = XCreateFontCursor(ui->display, XC_bottom_right_corner);

   XSetLocaleModifiers("");

   ui->xim = XOpenIM(ui->display, 0, 0, 0);

   if (!ui->xim) {
      XSetLocaleModifiers("@im=none");
      ui->xim = XOpenIM(ui->display, 0, 0, 0);
   }

   ui->inspector.create();

   return unique_ptr<UI>{ui};
}

void UIWindow::set_cursor(int cursor) {
   XDefineCursor(ui->display, _xwindow, ui->cursors[cursor]);
}

void _UIX11ResetCursor(UIWindow* window) {
   XDefineCursor(ui->display, window->_xwindow, ui->cursors[(uint32_t)UICursor::arrow]);
}

void UIWindow::endpaint(UIPainter* painter) {
   (void)painter;
   const auto& ur = _window->_update_region;
   XPutImage(ui->display, _window->_xwindow, DefaultGC(ui->display, 0), _window->_image, ur.l, ur.t, ur.l, ur.t,
             UI_RECT_SIZE(_window->_update_region));
}

void UIWindow::get_screen_position(int* _x, int* _y) {
   Window child;
   XTranslateCoordinates(ui->display, _window->_xwindow, DefaultRootWindow(ui->display), 0, 0, _x, _y, &child);
}

void UIMenuShow(UIMenu* menu) {
   Window child;

   // Find the screen that contains the point the menu was created at.
   Screen* menuScreen = NULL;
   int     screenX, screenY;

   for (int i = 0; i < ScreenCount(ui->display); i++) {
      Screen* screen = ScreenOfDisplay(ui->display, i);
      int     x, y;
      XTranslateCoordinates(ui->display, screen->root, DefaultRootWindow(ui->display), 0, 0, &x, &y, &child);

      if (menu->pointX >= x && menu->pointX < x + screen->width && menu->pointY >= y &&
          menu->pointY < y + screen->height) {
         menuScreen = screen;
         screenX = x, screenY = y;
         break;
      }
   }

   int width, height;
   _UIMenuPrepare(menu, &width, &height);

   {
      // Clamp the menu to the bounds of the window.
      // This step shouldn't be necessary with the screen clamping below, but there are some buggy X11 drivers that
      // report screen sizes incorrectly.
      int       wx, wy;
      UIWindow* parentWindow = menu->parentWindow;
      XTranslateCoordinates(ui->display, parentWindow->_xwindow, DefaultRootWindow(ui->display), 0, 0, &wx, &wy,
                            &child);
      if (menu->pointX + width > wx + (int)parentWindow->_width)
         menu->pointX = wx + parentWindow->_width - width;
      if (menu->pointY + height > wy + (int)parentWindow->_height)
         menu->pointY = wy + parentWindow->_height - height;
      if (menu->pointX < wx)
         menu->pointX = wx;
      if (menu->pointY < wy)
         menu->pointY = wy;
   }

   if (menuScreen) {
      // Clamp to the bounds of the screen.
      if (menu->pointX + width > screenX + menuScreen->width)
         menu->pointX = screenX + menuScreen->width - width;
      if (menu->pointY + height > screenY + menuScreen->height)
         menu->pointY = screenY + menuScreen->height - height;
      if (menu->pointX < screenX)
         menu->pointX = screenX;
      if (menu->pointY < screenY)
         menu->pointY = screenY;
      if (menu->pointX + width > screenX + menuScreen->width)
         width = screenX + menuScreen->width - menu->pointX;
      if (menu->pointY + height > screenY + menuScreen->height)
         height = screenY + menuScreen->height - menu->pointY;
   }

   Atom properties[] = {
      XInternAtom(ui->display, "_NET_WM_WINDOW_TYPE", true),
      XInternAtom(ui->display, "_NET_WM_WINDOW_TYPE_DROPDOWN_MENU", true),
      XInternAtom(ui->display, "_MOTIF_WM_HINTS", true),
   };

   XChangeProperty(ui->display, menu->_window->_xwindow, properties[0], XA_ATOM, 32, PropModeReplace,
                   (uint8_t*)properties, 2);
   XSetTransientForHint(ui->display, menu->_window->_xwindow, DefaultRootWindow(ui->display));

   struct Hints {
      int flags;
      int functions;
      int decorations;
      int inputMode;
      int status;
   };

   struct Hints hints = {0};
   hints.flags        = 2;
   XChangeProperty(ui->display, menu->_window->_xwindow, properties[2], properties[2], 32, PropModeReplace,
                   (uint8_t*)&hints, 5);

   XMapWindow(ui->display, menu->_window->_xwindow);
   XMoveResizeWindow(ui->display, menu->_window->_xwindow, menu->pointX, menu->pointY, width, height);
}

void UIWindowPack(UIWindow* window, int _width) {
   int width  = _width ? _width : window->_children[0]->message(UIMessage::GET_WIDTH, 0, 0);
   int height = window->_children[0]->message(UIMessage::GET_HEIGHT, width, 0);
   XResizeWindow(ui->display, window->_xwindow, width, height);
}

// return true if we should exit, normally return false
// ----------------------------------------------------
bool _UIProcessEvent(XEvent* event) {
   if (event->type == ClientMessage && (Atom)event->xclient.data.l[0] == ui->windowClosedID) {
      UIWindow* window = _UIFindWindow(event->xclient.window);
      if (!window)
         return false;
      bool exit = !window->message(UIMessage::WINDOW_CLOSE, 0, 0);
      if (exit)
         return true;
      UI::Update();
      return false;
   } else if (event->type == Expose) {
      UIWindow* window = _UIFindWindow(event->xexpose.window);
      if (!window)
         return false;
      XPutImage(ui->display, window->_xwindow, DefaultGC(ui->display, 0), window->_image, 0, 0, 0, 0, window->_width,
                window->_height);
   } else if (event->type == ConfigureNotify) {
      UIWindow* window = _UIFindWindow(event->xconfigure.window);
      if (!window)
         return false;

      if ((int)window->_width != event->xconfigure.width || (int)window->_height != event->xconfigure.height) {
         window->_width  = event->xconfigure.width;
         window->_height = event->xconfigure.height;
         window->_bits.resize(window->_width * window->_height);

         window->_image->width          = window->_width;
         window->_image->height         = window->_height;
         window->_image->bytes_per_line = window->_width * 4;
         window->_image->data           = (char*)window->_bits.data();
         window->_bounds                = ui_rect_2s(window->_width, window->_height);
         window->_clip                  = ui_rect_2s(window->_width, window->_height);
   #ifdef UI_DEBUG
         for (uint32_t i = 0; i < window->_width * window->_height; i++)
            window->_bits[i] = 0xFF00FF;
   #endif
         window->relayout();
         UI::Update();
      }
   } else if (event->type == MotionNotify) {
      UIWindow* window = _UIFindWindow(event->xmotion.window);
      if (!window)
         return false;
      window->_cursor.x = event->xmotion.x;
      window->_cursor.y = event->xmotion.y;
      window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
   } else if (event->type == LeaveNotify) {
      UIWindow* window = _UIFindWindow(event->xcrossing.window);
      if (!window)
         return false;

      if (!window->_pressed) {
         window->_cursor.x = -1;
         window->_cursor.y = -1;
      }

      window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
   } else if (event->type == ButtonPress || event->type == ButtonRelease) {
      UIWindow* window = _UIFindWindow(event->xbutton.window);
      if (!window)
         return false;
      window->_cursor.x = event->xbutton.x;
      window->_cursor.y = event->xbutton.y;

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
         ui->inspector.set_focused_window(window);
   } else if (event->type == KeyPress) {
      UIWindow* window = _UIFindWindow(event->xkey.window);
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
         UI::Update();
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
      UIWindow* window = _UIFindWindow(event->xkey.window);
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
      UIWindow* window = _UIFindWindow(event->xfocus.window);
      if (!window)
         return false;
      window->_ctrl = window->_shift = window->_alt = false;
      window->message(UIMessage::WINDOW_ACTIVATE, 0, 0);
   } else if (event->type == FocusOut || event->type == ResizeRequest) {
      _UIMenusClose();
      UI::Update();
   } else if (event->type == ClientMessage && event->xclient.message_type == ui->dndEnterID) {
      UIWindow* window = _UIFindWindow(event->xclient.window);
      if (!window)
         return false;
      window->_drag_source = (Window)event->xclient.data.l[0];
   } else if (event->type == ClientMessage && event->xclient.message_type == ui->dndPositionID) {
      UIWindow* window = _UIFindWindow(event->xclient.window);
      if (!window)
         return false;
      XClientMessageEvent m = {0};
      m.type                = ClientMessage;
      m.display             = event->xclient.display;
      m.window              = (Window)event->xclient.data.l[0];
      m.message_type        = ui->dndStatusID;
      m.format              = 32;
      m.data.l[0]           = window->_xwindow;
      m.data.l[1]           = true;
      m.data.l[4]           = ui->dndActionCopyID;
      XSendEvent(ui->display, m.window, False, NoEventMask, (XEvent*)&m);
      XFlush(ui->display);
   } else if (event->type == ClientMessage && event->xclient.message_type == ui->dndDropID) {
      UIWindow* window = _UIFindWindow(event->xclient.window);
      if (!window)
         return false;

      // TODO Dropping text.

      if (!XConvertSelection(ui->display, ui->dndSelectionID, ui->uriListID, ui->primaryID, window->_xwindow,
                             event->xclient.data.l[2])) {
         XClientMessageEvent m = {0};
         m.type                = ClientMessage;
         m.display             = ui->display;
         m.window              = window->_drag_source;
         m.message_type        = ui->dndFinishedID;
         m.format              = 32;
         m.data.l[0]           = window->_xwindow;
         m.data.l[1]           = 0;
         m.data.l[2]           = ui->dndActionCopyID;
         XSendEvent(ui->display, m.window, False, NoEventMask, (XEvent*)&m);
         XFlush(ui->display);
      }
   } else if (event->type == SelectionNotify) {
      UIWindow* window = _UIFindWindow(event->xselection.requestor);
      if (!window)
         return false;
      if (!window->_drag_source)
         return false;

      Atom          type   = None;
      int           format = 0;
      unsigned long count = 0, bytesLeft = 0;
      uint8_t*      data = NULL;
      XGetWindowProperty(ui->display, window->_xwindow, ui->primaryID, 0, 65536, False, AnyPropertyType, &type, &format,
                         &count, &bytesLeft, &data);

      if (format == 8 /* bits per character */) {
         if (event->xselection.target == ui->uriListID) {
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
         } else if (event->xselection.target == ui->plainTextID) {
            // TODO.
         }
      }

      XFree(data);

      XClientMessageEvent m = {0};
      m.type                = ClientMessage;
      m.display             = ui->display;
      m.window              = window->_drag_source;
      m.message_type        = ui->dndFinishedID;
      m.format              = 32;
      m.data.l[0]           = window->_xwindow;
      m.data.l[1]           = true;
      m.data.l[2]           = ui->dndActionCopyID;
      XSendEvent(ui->display, m.window, False, NoEventMask, (XEvent*)&m);
      XFlush(ui->display);

      window->_drag_source = 0; // Drag complete.
      UI::Update();
   } else if (event->type == SelectionRequest) {
      UIWindow* window = _UIFindWindow(event->xclient.window);
      if (!window)
         return false;

      if ((XGetSelectionOwner(ui->display, ui->clipboardID) == window->_xwindow) &&
          (event->xselectionrequest.selection == ui->clipboardID)) {
         XSelectionRequestEvent requestEvent = event->xselectionrequest;
         Atom                   utf8ID       = XInternAtom(ui->display, "UTF8_STRING", 1);
         if (utf8ID == None)
            utf8ID = XA_STRING;

         Atom type                = requestEvent.target;
         type                     = (type == ui->textID) ? XA_STRING : type;
         int changePropertyResult = 0;

         if (requestEvent.target == XA_STRING || requestEvent.target == ui->textID || requestEvent.target == utf8ID) {
            changePropertyResult =
               XChangeProperty(requestEvent.display, requestEvent.requestor, requestEvent.property, type, 8,
                               PropModeReplace, (const unsigned char*)ui->pasteText.c_str(), ui->pasteText.size());
         } else if (requestEvent.target == ui->targetID) {
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

            XSendEvent(ui->display, requestEvent.requestor, 0, 0, (XEvent*)&sendEvent);
         }
      }
   }

   return false;
}

// return true if events processed without problem, false otherwise
// ----------------------------------------------------------------
bool UI::MessageLoopSingle(int* result) {
   XEvent events[64];

   if (!ui->animating.empty()) {
      if (XPending(ui->display)) {
         XNextEvent(ui->display, events + 0);
      } else {
         UI::ProcessAnimations();
         return true;
      }
   } else {
      XNextEvent(ui->display, events + 0);
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

   while (cur_idx < 64 && XPending(ui->display)) {
      XNextEvent(ui->display, events + cur_idx);

      merge_events(ConfigureNotify, configureIndex);
      merge_events(MotionNotify, motionIndex);
      merge_events(Expose, exposeIndex);

      ++cur_idx;
   }

   for (int i = 0; i < cur_idx; i++) {
      if (!events[i].type) {
         continue;
      }

      if (_UIProcessEvent(events + i)) {
         return false;
      }
   }

   return true;
}

void UIWindowPostMessage(UIWindow* window, UIMessage msg, void* _dp) {
   // HACK! Xlib doesn't seem to have a nice way to do this,
   // so send a specially crafted key press event instead.
   // TODO Maybe ClientMessage is what this should use?
   uintptr_t dp    = (uintptr_t)_dp;
   XKeyEvent event = {0};
   event.display   = ui->display;
   event.window    = window->_xwindow;
   event.root      = DefaultRootWindow(ui->display);
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
   XSendEvent(ui->display, window->_xwindow, True, KeyPressMask, (XEvent*)&event);
   XFlush(ui->display);
}

#endif // UI_LINUX

#ifdef UI_WINDOWS

int UIWindow::_ClassMessageProc(UIElement* el, UIMessage msg, int di, void* dp) {
   if (msg == UIMessage::DEALLOCATE) {
      UIWindow* window = (UIWindow*)el;
      _UIWindowDestroyCommon(window);
      SetWindowLongPtr(window->_hwnd, GWLP_USERDATA, 0);
      DestroyWindow(window->_hwnd);
      return 0;
   }

   return UIWindow::_ClassMessageProcCommon(el, msg, di, dp);
}

LRESULT CALLBACK _UIWindowProcedure(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
   UIWindow* window = (UIWindow*)GetWindowLongPtr(hwnd, GWLP_USERDATA);

   if (!window || ui->assertionFailure) {
      return DefWindowProc(hwnd, msg, wParam, lParam);
   }

   if (msg == WM_CLOSE) {
      if (window->message(UIMessage::WINDOW_CLOSE, 0, 0)) {
         UI::Update();
         return 0;
      } else {
         PostQuitMessage(0);
      }
   } else if (msg == WM_SIZE) {
      RECT client;
      GetClientRect(hwnd, &client);
      window->_width  = client.right;
      window->_height = client.bottom;
      window->_bits.resize(window->_width * window->_height);
      window->_bounds = ui_rect_2s(window->_width, window->_height);
      window->_clip   = ui_rect_2s(window->_width, window->_height);
      window->relayout();
      UI::Update();
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
      window->_cursor.x = cursor.x;
      window->_cursor.y = cursor.y;
      window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
   } else if (msg == WM_MOUSELEAVE) {
      window->_tracking_leave = false;

      if (!window->_pressed) {
         window->_cursor.x = -1;
         window->_cursor.y = -1;
      }

      window->input_event(UIMessage::MOUSE_MOVE, 0, 0);
   } else if (msg == WM_LBUTTONDOWN) {
      SetCapture(hwnd);
      window->input_event(UIMessage::LEFT_DOWN, 0, 0);
   } else if (msg == WM_LBUTTONUP) {
      if (window->_pressed_button == 1)
         ReleaseCapture();
      window->input_event(UIMessage::LEFT_UP, 0, 0);
   } else if (msg == WM_MBUTTONDOWN) {
      SetCapture(hwnd);
      window->input_event(UIMessage::MIDDLE_DOWN, 0, 0);
   } else if (msg == WM_MBUTTONUP) {
      if (window->_pressed_button == 2)
         ReleaseCapture();
      window->input_event(UIMessage::MIDDLE_UP, 0, 0);
   } else if (msg == WM_RBUTTONDOWN) {
      SetCapture(hwnd);
      window->input_event(UIMessage::RIGHT_DOWN, 0, 0);
   } else if (msg == WM_RBUTTONUP) {
      if (window->_pressed_button == 3)
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
      info.biWidth = window->_width, info.biHeight = -(int)window->_height;
      info.biPlanes = 1, info.biBitCount = 32;
      StretchDIBits(dc, 0, 0, UI_RECT_SIZE(window->_bounds), 0, 0, UI_RECT_SIZE(window->_bounds), window->_bits.data(),
                    (BITMAPINFO*)&info, DIB_RGB_COLORS, SRCCOPY);
      EndPaint(hwnd, &paint);
   } else if (msg == WM_SETCURSOR && LOWORD(lParam) == HTCLIENT) {
      ::SetCursor(ui->cursors[window->_cursor_style]);
      return 1;
   } else if (msg == WM_SETFOCUS || msg == WM_KILLFOCUS) {
      _UIMenusClose();

      if (msg == WM_SETFOCUS) {
         ui->inspector.set_focused_window(window);
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
      UI::Update();
   } else if (msg == WM_APP + 1) {
      window->message((UIMessage)wParam, 0, (void*)lParam);
      UI::Update();
   } else {
      if (msg == WM_NCLBUTTONDOWN || msg == WM_NCMBUTTONDOWN || msg == WM_NCRBUTTONDOWN) {
         if (~window->_flags & UIWindow::MENU) {
            _UIMenusClose();
            UI::Update();
         }
      }

      return DefWindowProc(hwnd, msg, wParam, lParam);
   }

   return 0;
}

unique_ptr<UI> UIInitialise(const UIConfig& cfg) {
   ui       = new UI;
   ui->heap = GetProcessHeap();

   std::string font_path = cfg.font_path;
   if (font_path.empty())
      font_path = _UI_TO_STRING_2(UI_FONT_PATH);

   ui->default_font_path = font_path;
   _UIInitialiseCommon(cfg, font_path);

   ui->cursors[(uint32_t)UICursor::arrow]             = LoadCursor(NULL, IDC_ARROW);
   ui->cursors[(uint32_t)UICursor::text]              = LoadCursor(NULL, IDC_IBEAM);
   ui->cursors[(uint32_t)UICursor::split_v]           = LoadCursor(NULL, IDC_SIZENS);
   ui->cursors[(uint32_t)UICursor::split_h]           = LoadCursor(NULL, IDC_SIZEWE);
   ui->cursors[(uint32_t)UICursor::flipped_arrow]     = LoadCursor(NULL, IDC_ARROW);
   ui->cursors[(uint32_t)UICursor::cross_hair]        = LoadCursor(NULL, IDC_CROSS);
   ui->cursors[(uint32_t)UICursor::hand]              = LoadCursor(NULL, IDC_HAND);
   ui->cursors[(uint32_t)UICursor::resize_up]         = LoadCursor(NULL, IDC_SIZENS);
   ui->cursors[(uint32_t)UICursor::resize_left]       = LoadCursor(NULL, IDC_SIZEWE);
   ui->cursors[(uint32_t)UICursor::resize_up_right]   = LoadCursor(NULL, IDC_SIZENESW);
   ui->cursors[(uint32_t)UICursor::resize_up_left]    = LoadCursor(NULL, IDC_SIZENWSE);
   ui->cursors[(uint32_t)UICursor::resize_down]       = LoadCursor(NULL, IDC_SIZENS);
   ui->cursors[(uint32_t)UICursor::resize_right]      = LoadCursor(NULL, IDC_SIZEWE);
   ui->cursors[(uint32_t)UICursor::resize_down_left]  = LoadCursor(NULL, IDC_SIZENESW);
   ui->cursors[(uint32_t)UICursor::resize_down_right] = LoadCursor(NULL, IDC_SIZENWSE);

   WNDCLASS windowClass      = {0};
   windowClass.lpfnWndProc   = _UIWindowProcedure;
   windowClass.lpszClassName = "normal";
   RegisterClass(&windowClass);
   windowClass.style |= CS_DROPSHADOW;
   windowClass.lpszClassName = "shadow";
   RegisterClass(&windowClass);

   ui->inspector.create();

   return unique_ptr<UI>{ui};
}

bool UI::MessageLoopSingle(int* result) {
   MSG msg = {0};

   if (!ui->animating.empty()) {
      if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
         if (msg.message == WM_QUIT) {
            *result = msg.wParam;
            return false;
         }

         TranslateMessage(&msg);
         DispatchMessage(&msg);
      } else {
         UI::ProcessAnimations();
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

void UIMenuShow(UIMenu* menu) {
   int width, height;
   _UIMenuPrepare(menu, &width, &height);
   MoveWindow(menu->_window->_hwnd, menu->pointX, menu->pointY, width, height, FALSE);
   ShowWindow(menu->_window->_hwnd, SW_SHOWNOACTIVATE);
}

UIWindow* UIWindowCreate(UIWindow* owner, uint32_t flags, const char* cTitle, int width, int height) {
   _UIMenusClose();

   UIWindow* window = new UIWindow(NULL, flags | UIElement::window_flag, UIWindow::_ClassMessageProc, "Window");
   _UIWindowAdd(window);
   if (owner)
      window->_scale = owner->_scale;

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

   return window;
}

void UIWindow::endpaint(UIPainter* painter) {
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

void UIWindow::set_cursor(int cursor) {
   ::SetCursor(ui->cursors[cursor]);
}

void UIWindow::get_screen_position(int* _x, int* _y) {
   POINT p;
   p.x = 0;
   p.y = 0;
   ClientToScreen(_window->_hwnd, &p);
   *_x = p.x;
   *_y = p.y;
}

void UIWindowPostMessage(UIWindow* window, UIMessage msg, void* _dp) {
   PostMessage(window->_hwnd, WM_APP + 1, (WPARAM)msg, (LPARAM)_dp);
}

void UI::ClipboardWriteText(UIWindow* window, std::string text, sel_target_t) {
   if (OpenClipboard(window->_hwnd)) {
      EmptyClipboard();
      HGLOBAL memory = GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT, text.size() + 1);
      char*   copy   = (char*)GlobalLock(memory);
      std::memcpy(copy, text.c_str(), text.size());
      GlobalUnlock(copy);
      SetClipboardData(CF_TEXT, memory);
      CloseClipboard();
   }
}

std::string UI::ClipboardReadText(UIWindow* window, sel_target_t) {
   std::string res;

   if (!OpenClipboard(window->_hwnd)) {
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
