#include "luigi.hpp"

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
UI* ui = nullptr;           // global pointer to the UIInitialise return value

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
   s[bytes] = 0;        // make sure it is null terminated
   s.resize(bytes);     // return a string of the correct size
   fclose(f);

   return s;
}

// --------------------------------------------------
// Member functions.
// --------------------------------------------------
uint32_t UIElement::state() const {
   return (((flags & UIElement::DISABLED) ? UI_DRAW_CONTROL_STATE_DISABLED : 0) |
           ((window->hovered == this) ? UI_DRAW_CONTROL_STATE_HOVERED : 0) |
           ((window->focused == this) ? UI_DRAW_CONTROL_STATE_FOCUSED : 0) |
           ((window->pressed == this) ? UI_DRAW_CONTROL_STATE_PRESSED : 0));
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
inline constexpr int max_glyphs             = _UNICODE_MAX_CODEPOINT + 1;

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
   return _UICharIsAlphaOrDigitOrUnderscore(c1) != _UICharIsAlphaOrDigitOrUnderscore(c2);
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
   return (_UICharIsAlphaOrDigitOrUnderscore(c1) != _UICharIsAlphaOrDigitOrUnderscore(c2));
}

#endif // UI_UNICODE


UIRectangle fit(const UIRectangle& parent, UIRectangle child, bool allowScalingUp) {
   auto [childWidth, childHeight] = child.dims();
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

int _UIByteToColumn(std::string_view string, size_t byte, size_t tabSize) {
   size_t ti = 0, i = 0;
   size_t bytes = string.size();

   while (i < byte && i < bytes) {
      ti++;
      _ui_skip_tab(ti, &string[i], bytes - i, tabSize);
      _ui_advance_char(i, &string[i], byte);
   }

   return (int)ti;
}

int _UIColumnToByte(std::string_view string, size_t column, size_t tabSize) {
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

   size_t numExtraBytes;
   uint8_t   first = cString[0];

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

bool UIElement::Animate(bool stop) {
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
      UI_ASSERT(~flags & UIElement::DESTROY);
      return true;
   }
}

uint64_t UIAnimateClock() {
   return (uint64_t)UI_CLOCK() * 1000 / UI_CLOCKS_PER_SECOND;
}

void _UIProcessAnimations() {
   bool update = !ui->animating.empty();

   for (auto element : ui->animating)
      element->Message(UIMessage::ANIMATE, 0, 0);

   if (update) {
      _UIUpdate();
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
   return string.size() * ui->activeFont->glyphWidth;
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

   int width  = UIMeasureStringWidth(string);
   int height = UIMeasureStringHeight();
   int x      = align == UIAlign::center ? ((r.l + r.r - width) / 2) : align == UIAlign::right ? (r.r - width) : r.l;
   int y      = (r.t + r.b - height) / 2;
   int i = 0;
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
   for (const auto& r : border_rects)
      UIDrawBlock(painter, r, borderColor);
}

void UIDrawRectangle(UIPainter* painter, UIRectangle r, uint32_t mainColor, uint32_t borderColor,
                     UIRectangle borderSize) {
   UIDrawBorder(painter, r, borderColor, borderSize);
   UIDrawBlock(painter, r.shrink(borderSize), mainColor);
}

auto ui_mdi_child_calculate_layout(const UIRectangle& bounds, float scale) {
   int         titleSize   = ui_size::MDI_CHILD_TITLE * scale;
   int         borderSize  = ui_size::MDI_CHILD_BORDER * scale;
   UIRectangle titleRect   = add(bounds, UIRectangle(borderSize, -borderSize, 0, 0));
   titleRect.b             = titleRect.t + titleSize;
   UIRectangle contentRect = add(bounds, UIRectangle(borderSize, -borderSize, titleSize, -borderSize));

   return std::tuple{titleSize, borderSize, titleRect, contentRect};
}

void UIDrawControlDefault(UIPainter* painter, UIRectangle bounds, uint32_t mode, std::string_view label,
                          double position, float scale) {
   bool     checked       = mode & UI_DRAW_CONTROL_STATE_CHECKED;
   bool     disabled      = mode & UI_DRAW_CONTROL_STATE_DISABLED;
   bool     focused       = mode & UI_DRAW_CONTROL_STATE_FOCUSED;
   bool     hovered       = mode & UI_DRAW_CONTROL_STATE_HOVERED;
   bool     indeterminate = mode & UI_DRAW_CONTROL_STATE_INDETERMINATE;
   bool     pressed       = mode & UI_DRAW_CONTROL_STATE_PRESSED;
   bool     selected      = mode & UI_DRAW_CONTROL_STATE_SELECTED;
   uint32_t which         = mode & UI_DRAW_CONTROL_TYPE_MASK;

   uint32_t buttonColor     = disabled               ? ui->theme.buttonDisabled
                              : (pressed && hovered) ? ui->theme.buttonPressed
                              : (pressed || hovered) ? ui->theme.buttonHovered
                              : focused              ? ui->theme.selected
                                                     : ui->theme.buttonNormal;
   uint32_t buttonTextColor = disabled                           ? ui->theme.textDisabled
                              : buttonColor == ui->theme.selected ? ui->theme.textSelected
                                                                 : ui->theme.text;

   if (which == UI_DRAW_CONTROL_CHECKBOX) {
      uint32_t    color = buttonColor, textColor = buttonTextColor;
      int         midY      = (bounds.t + bounds.b) / 2;
      UIRectangle boxBounds = UIRectangle(bounds.l, bounds.l + ui_size::CHECKBOX_BOX, midY - ui_size::CHECKBOX_BOX / 2,
                                          midY + ui_size::CHECKBOX_BOX / 2);
      UIDrawRectangle(painter, boxBounds, color, ui->theme.border, UIRectangle(1));
      UIDrawString(painter, boxBounds + UIRectangle(1, 0, 0, 0),
                   checked         ? "*"
                   : indeterminate ? "-"
                                   : " ",
                   textColor, UIAlign::center, NULL);
      UIDrawString(painter, bounds + UIRectangle(ui_size::CHECKBOX_BOX + ui_size::CHECKBOX_GAP, 0, 0, 0),
                   label, disabled ? ui->theme.textDisabled : ui->theme.text, UIAlign::left, NULL);
   } else if (which == UI_DRAW_CONTROL_MENU_ITEM || which == UI_DRAW_CONTROL_DROP_DOWN ||
              which == UI_DRAW_CONTROL_PUSH_BUTTON) {
      uint32_t color = buttonColor, textColor = buttonTextColor;
      int      borderSize = which == UI_DRAW_CONTROL_MENU_ITEM ? 0 : scale;
      UIDrawRectangle(painter, bounds, color, ui->theme.border, UIRectangle(borderSize));

      if (checked && !focused) {
         UIDrawBlock(painter, bounds + ui_rect_1i((int)(ui_size::BUTTON_CHECKED_AREA * scale)),
                     ui->theme.buttonPressed);
      }

      UIRectangle innerBounds = bounds + ui_rect_2i((int)(ui_size::MENU_ITEM_MARGIN * scale), 0);

      if (which == UI_DRAW_CONTROL_MENU_ITEM) {
         int tab = 0;
         auto labelBytes = (int)label.size();
         for (; tab < labelBytes && label[tab] != '\t'; tab++)
            ;

         UIDrawString(painter, innerBounds, label.substr(0, tab), textColor, UIAlign::left, NULL);

         if (labelBytes > tab) {
            UIDrawString(painter, innerBounds, {label.data() + tab + 1, static_cast<size_t>(labelBytes - tab - 1)},
                         textColor, UIAlign::right, NULL);
         }
      } else if (which == UI_DRAW_CONTROL_DROP_DOWN) {
         UIDrawString(painter, innerBounds, label, textColor, UIAlign::left, NULL);
         UIDrawString(painter, innerBounds, "\x19", textColor, UIAlign::right, NULL);
      } else {
         UIDrawString(painter, bounds, label, textColor, UIAlign::center, NULL);
      }
   } else if (which == UI_DRAW_CONTROL_LABEL) {
      UIDrawString(painter, bounds, label, ui->theme.text, UIAlign::left, NULL);
   } else if (which == UI_DRAW_CONTROL_SPLITTER) {
      UIRectangle borders = (mode & UI_DRAW_CONTROL_STATE_VERTICAL) ? UIRectangle(0, 1) : UIRectangle(1, 0);
      UIDrawRectangle(painter, bounds, ui->theme.buttonNormal, ui->theme.border, borders);
   } else if (which == UI_DRAW_CONTROL_SCROLL_TRACK) {
      if (disabled)
         UIDrawBlock(painter, bounds, ui->theme.panel1);
   } else if (which == UI_DRAW_CONTROL_SCROLL_DOWN || which == UI_DRAW_CONTROL_SCROLL_UP) {
      bool     isDown = which == UI_DRAW_CONTROL_SCROLL_DOWN;
      uint32_t color  = pressed ? ui->theme.buttonPressed : hovered ? ui->theme.buttonHovered : ui->theme.panel2;
      UIDrawRectangle(painter, bounds, color, ui->theme.border, UIRectangle(0));

      if (mode & UI_DRAW_CONTROL_STATE_VERTICAL) {
         UIDrawGlyph(painter, (bounds.l + bounds.r - ui->activeFont->glyphWidth) / 2 + 1,
                     isDown ? (bounds.b - ui->activeFont->glyphHeight - 2 * scale) : (bounds.t + 2 * scale),
                     isDown ? 25 : 24, ui->theme.text);
      } else {
         UIDrawGlyph(painter, isDown ? (bounds.r - ui->activeFont->glyphWidth - 2 * scale) : (bounds.l + 2 * scale),
                     (bounds.t + bounds.b - ui->activeFont->glyphHeight) / 2, isDown ? 26 : 27, ui->theme.text);
      }
   } else if (which == UI_DRAW_CONTROL_SCROLL_THUMB) {
      uint32_t color = pressed ? ui->theme.buttonPressed : hovered ? ui->theme.buttonHovered : ui->theme.buttonNormal;
      UIDrawRectangle(painter, bounds, color, ui->theme.border, UIRectangle(2));
   } else if (which == UI_DRAW_CONTROL_GAUGE) {
      UIDrawRectangle(painter, bounds, ui->theme.buttonNormal, ui->theme.border, UIRectangle(1));
      UIRectangle filled = bounds + ui_rect_1i(1);
      if (mode & UI_DRAW_CONTROL_STATE_VERTICAL) {
         filled.t = filled.b - filled.height() * position;
      } else {
         filled.r = filled.l + filled.width() * position;
      }
      UIDrawBlock(painter, filled, ui->theme.selected);
   } else if (which == UI_DRAW_CONTROL_SLIDER) {
      bool vertical     = mode & UI_DRAW_CONTROL_STATE_VERTICAL;
      int  center       = vertical ? (bounds.l + bounds.r) / 2 : (bounds.t + bounds.b) / 2;
      int  trackSize    = ui_size::SLIDER_TRACK * scale;
      int  thumbSize    = ui_size::SLIDER_THUMB * scale;
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
   } else if (which == UI_DRAW_CONTROL_TEXTBOX) {
      UIDrawRectangle(painter, bounds,
                      disabled  ? ui->theme.buttonDisabled
                      : focused ? ui->theme.textboxFocused
                                : ui->theme.textboxNormal,
                      ui->theme.border, UIRectangle(1));
   } else if (which == UI_DRAW_CONTROL_MODAL_POPUP) {
      UIRectangle bounds2 = bounds + ui_rect_1i(-1);
      UIDrawBorder(painter, bounds2, ui->theme.border, UIRectangle(1));
      UIDrawBorder(painter, bounds2 + UIRectangle(1), ui->theme.border, UIRectangle(1));
   } else if (which == UI_DRAW_CONTROL_MENU) {
      UIDrawBlock(painter, bounds, ui->theme.border);
   } else if (which == UI_DRAW_CONTROL_TABLE_ROW) {
      if (selected)
         UIDrawBlock(painter, bounds, ui->theme.selected);
      else if (hovered)
         UIDrawBlock(painter, bounds, ui->theme.buttonHovered);
   } else if (which == UI_DRAW_CONTROL_TABLE_CELL) {
      uint32_t textColor = selected ? ui->theme.textSelected : ui->theme.text;
      UIDrawString(painter, bounds, label, textColor, UIAlign::left, NULL);
   } else if (which == UI_DRAW_CONTROL_TABLE_BACKGROUND) {
      UIDrawBlock(painter, bounds, ui->theme.panel2);
      UIDrawRectangle(painter, UIRectangle(bounds.l, bounds.r, bounds.t, bounds.t + (int)(ui_size::TABLE_HEADER * scale)),
                      ui->theme.panel1, ui->theme.border, UIRectangle(0, 0, 0, 1));
   } else if (which == UI_DRAW_CONTROL_TABLE_HEADER) {
      UIDrawString(painter, bounds, label, ui->theme.text, UIAlign::left, NULL);
      if (selected)
         UIDrawInvert(painter, bounds);
   } else if (which == UI_DRAW_CONTROL_MDI_CHILD) {
      auto [titleSize, borderSize, titleRect, contentRect] = ui_mdi_child_calculate_layout(bounds, scale);
      UIRectangle borders                                  = UIRectangle(borderSize, borderSize, titleSize, borderSize);
      UIDrawBorder(painter, bounds, ui->theme.buttonNormal, borders);
      UIDrawBorder(painter, bounds, ui->theme.border, UIRectangle((int)scale));
      UIDrawBorder(painter, contentRect + ui_rect_1i(-1), ui->theme.border, UIRectangle((int)scale));
      UIDrawString(painter, titleRect, label, ui->theme.text, UIAlign::left, NULL);
   } else if (which == UI_DRAW_CONTROL_TAB) {
      uint32_t    color = selected ? ui->theme.buttonPressed : ui->theme.buttonNormal;
      UIRectangle t     = bounds;
      if (selected)
         t.b++, t.t--;
      else
         t.t++;
      UIDrawRectangle(painter, t, color, ui->theme.border, UIRectangle(1));
      UIDrawString(painter, bounds, label, ui->theme.text, UIAlign::center, NULL);
   } else if (which == UI_DRAW_CONTROL_TAB_BAND) {
      UIDrawRectangle(painter, bounds, ui->theme.panel1, ui->theme.border, UIRectangle(0, 0, 0, 1));
   }
}

// --------------------------------------------------
// Element hierarchy.
// --------------------------------------------------

void UIElement::_DestroyDescendents(bool topLevel) {
   for (auto child : children) {
      if (!topLevel || (~child->flags & UIElement::NON_CLIENT))
         child->Destroy();
   }

   _UIInspectorRefresh();
}

void UIElement::DestroyDescendents() {
   _DestroyDescendents(true);
}

void UIElement::Destroy() {
   if (flags & UIElement::DESTROY) {
      return;
   }

   this->Message(UIMessage::DESTROY, 0, 0);
   flags |= UIElement::DESTROY | UIElement::HIDE;

   UIElement* ancestor = parent;

   while (ancestor) {
      if (ancestor->flags & UIElement::DESTROY_DESCENDENT)
         break;
      ancestor->flags |= UIElement::DESTROY_DESCENDENT;
      ancestor = ancestor->parent;
   }

   _DestroyDescendents(false);

   if (parent) {
      parent->Relayout();
      parent->Repaint(&bounds);
      UIElementMeasurementsChanged(parent, 3);
   }
}

// returns 0 if message not processed
// ----------------------------------
int UIElement::Message(UIMessage message, int di, void* dp) {
   if (message != UIMessage::DEALLOCATE && (flags & UIElement::DESTROY)) {
      return 0;
   }

   if (message >= UIMessage::INPUT_EVENTS_START && message <= UIMessage::INPUT_EVENTS_END &&
       (flags & UIElement::DISABLED)) {
      return 0;
   }

   if (messageUser) {
      int result = messageUser(this, message, di, dp);

      if (result) {
         return result;
      }
   }

   if (messageClass) {
      return messageClass(this, message, di, dp);
   } else {
      return 0;
   }
}

// -------------------------------------------------------------------------------
// Set insertBefore to null to insert at the end.
// Returns the element it was before in its previous parent, or NULL.
// -------------------------------------------------------------------------------
UIElement* UIElement::ChangeParent(UIElement* newParent, UIElement* insertBefore) {
   [[maybe_unused]] bool found     = false;
   UIElement*            oldBefore = NULL;

   auto num_children = parent->children.size();
   for (uint32_t i = 0; i < num_children; i++) {
      if (parent->children[i] == this) {
         parent->children.erase(parent->children.begin() + i);
         oldBefore = i == parent->children.size() ? NULL : parent->children[i];
         found     = true;
         break;
      }
   }

   UI_ASSERT(found && (~flags & UIElement::DESTROY));

   for (uint32_t i = 0; i <= newParent->children.size(); i++) {
      if (i == newParent->children.size() || newParent->children[i] == insertBefore) {
         newParent->children.insert(parent->children.begin() + i, this);
         found = true;
         break;
      }
   }

   UIElement* oldParent = parent;
   parent               = newParent;
   window               = newParent->window;

   UIElementMeasurementsChanged(oldParent, 3);
   UIElementMeasurementsChanged(newParent, 3);

   return oldBefore;
}

void UIElement::SetDisabled(bool disabled) {
   if (window->focused == this && disabled) {
      window->Focus();
   }

   if ((flags & UIElement::DISABLED) && disabled)
      return;
   if ((~flags & UIElement::DISABLED) && !disabled)
      return;

   if (disabled)
      flags |= UIElement::DISABLED;
   else
      flags &= ~UIElement::DISABLED;

   Message(UIMessage::UPDATE, UIUpdate::DISABLED, 0);
}

void UIElement::Focus() {
   UIElement* previous = window->focused;
   if (previous == this)
      return;
   window->focused = this;
   if (previous)
      previous->Message(UIMessage::UPDATE, UIUpdate::FOCUSED, 0);
   this->Message(UIMessage::UPDATE, UIUpdate::FOCUSED, 0);

   _UIInspectorRefresh();
}

// --------------------------------------------------
// Update cycles.
// --------------------------------------------------

void UIElement::Refresh() {
   Relayout();
   Repaint(nullptr);
}

void UIElement::Relayout() {
   if (flags & UIElement::RELAYOUT) {
      return;
   }

   flags |= UIElement::RELAYOUT;
   UIElement* ancestor = parent;

   while (ancestor) {
      ancestor->flags |= UIElement::RELAYOUT_DESCENDENT;
      ancestor = ancestor->parent;
   }
}

void UIElementMeasurementsChanged(UIElement* element, int which) {
   if (!element->parent) {
      return; // This is the window element.
   }

   while (true) {
      if (element->parent->flags & UIElement::DESTROY)
         return;
      which &= ~element->parent->Message(UIMessage::GET_CHILD_STABILITY, which, element);
      if (!which)
         break;
      element->flags |= UIElement::RELAYOUT;
      element = element->parent;
   }

   element->Relayout();
}

void UIElement::Repaint(const UIRectangle* region) {
   if (!region) {
      region = &bounds;
   }

   UIRectangle r = intersection(*region, clip);

   if (!r.valid()) {
      return;
   }

   if (window->updateRegion.valid()) {
      window->updateRegion = bounding(window->updateRegion, r);
   } else {
      window->updateRegion = r;
   }
}

void UIElement::Move(UIRectangle new_bounds, bool layout) {
   UIRectangle new_clip  = parent ? intersection(parent->clip, new_bounds) : new_bounds;
   bool        moved     = bounds != new_bounds || clip != new_clip;

   if (moved) {
      layout = true;

      window->Repaint(&clip);
      window->Repaint(&new_clip);

      bounds = new_bounds;
      clip   = new_clip;
   }

   if (flags & UIElement::RELAYOUT) {
      layout = true;
   }

   if (layout) {
      Message(UIMessage::LAYOUT, 0, 0);
   } else if (flags & UIElement::RELAYOUT_DESCENDENT) {
      for (auto child : children)
         child->Move(child->bounds, false);
   }

   flags &= ~(UIElement::RELAYOUT_DESCENDENT | UIElement::RELAYOUT);
}

void UIElement::Paint(UIPainter* painter) {
   if (flags & UIElement::HIDE) {
      return;
   }

   // Clip painting to the element's clip.
   // ------------------------------------
   painter->clip = intersection(clip, painter->clip);

   if (!painter->clip.valid()) {
      return;
   }

   // Paint the element.
   // ------------------
   Message(UIMessage::PAINT, 0, painter);

   // Paint its children.
   // -------------------
   UIRectangle previousClip = painter->clip;

   for (auto child : children) {
      painter->clip = previousClip;
      child->Paint(painter);
   }

   // Draw the foreground and border.
   // -------------------------------
   painter->clip = previousClip;
   Message(UIMessage::PAINT_FOREGROUND, 0, painter);

   if (flags & UIElement::BORDER) {
      UIDrawBorder(painter, bounds, ui->theme.border, UIRectangle((int)window->scale));
   }
}

bool _UIDestroy(UIElement* element) {
   if (element->flags & UIElement::DESTROY_DESCENDENT) {
      element->flags &= ~UIElement::DESTROY_DESCENDENT;
#if 1
      intptr_t num_children = (intptr_t)element->children.size();
      for (intptr_t i = 0; i < num_children; i++) {
         if (_UIDestroy(element->children[i])) {
            element->children.erase(element->children.begin() + i);
            --num_children; --i;
         }
      }
#else
      // not sure why this does not work. crash when clicking on file in "Files" tab
      auto filtered = element->children | views::filter([](UIElement* c) { return !_UIDestroy(c); });
      std::vector<UIElement*> new_children = { filtered.begin(), filtered.end() };
      element->children = std::move(new_children);
#endif
   }

   if (element->flags & UIElement::DESTROY) {
      element->Message(UIMessage::DEALLOCATE, 0, 0);

      if (element->window->pressed == element) {
         element->window->SetPressed(NULL, 0);
      }

      if (element->window->hovered == element) {
         element->window->hovered = element->window;
      }

      if (element->window->focused == element) {
         element->window->focused = NULL;
      }

      if (element->window->dialogOldFocus == element) {
         element->window->dialogOldFocus = NULL;
      }

      element->Animate(true);
      delete element;
      return true;
   } else {
      return false;
   }
}


UIElement::UIElement(UIElement* parent, uint32_t flags, MsgFn message, const char* cClassName)
   : flags(flags)
   , parent(parent)
   , bounds(0)
   , clip(0)
   , cClassName(cClassName) {

   messageClass = message;

   if (!parent && (~flags & UIElement::WINDOW)) {
      UI_ASSERT(ui->parentStackCount);
      parent = ui->parentStack[ui->parentStackCount - 1];
   }

   if (parent) {
      UI_ASSERT(~parent->flags & UIElement::DESTROY);
      window           = parent->window;
      parent->children.push_back(this);
      parent->Relayout();
      UIElementMeasurementsChanged(parent, 3);
   }

   static uint32_t s_id = 0;

   id = ++s_id;

   _UIInspectorRefresh();

   if (flags & UIElement::PARENT_PUSH) {
      UIParentPush(this);
   }
}

UIElement::~UIElement() {
}

UIElement* UIElementCreate(size_t bytes, UIElement* parent, uint32_t flags, MsgFn message, const char* cClassName) {
   UIElement* element = new UIElement(parent, flags, message, cClassName);
   return element;
}

UIElement* UIParentPush(UIElement* element) {
   UI_ASSERT(ui->parentStackCount != sizeof(ui->parentStack) / sizeof(ui->parentStack[0]));
   ui->parentStack[ui->parentStackCount++] = element;
   return element;
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
   bool horizontal = panel->flags & UIPanel::HORIZONTAL;
   int  available  = horizontal ? hSpace : vSpace;
   int  count = 0, fill = 0, perFill = 0;

   for (auto child : panel->children) {
      if (child->flags & (UIElement::HIDE | UIElement::NON_CLIENT)) {
         continue;
      }

      count++;

      if (horizontal) {
         if (child->flags & UIElement::H_FILL) {
            fill++;
         } else if (available > 0) {
            available -= child->Message(UIMessage::GET_WIDTH, vSpace, 0);
         }
      } else {
         if (child->flags & UIElement::V_FILL) {
            fill++;
         } else if (available > 0) {
            available -= child->Message(UIMessage::GET_HEIGHT, hSpace, 0);
         }
      }
   }

   if (count) {
      available -= (count - 1) * (int)(panel->gap * scale);
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
   bool horizontal = panel->flags & UIPanel::HORIZONTAL;
   int  perFill =
      _UIPanelCalculatePerFill(panel, NULL, horizontal ? di : 0, horizontal ? 0 : di, panel->window->scale);
   int size = 0;

   for (auto child : panel->children) {
      if (child->flags & (UIElement::HIDE | UIElement::NON_CLIENT))
         continue;
      int childSize =
         child->Message(horizontal ? UIMessage::GET_HEIGHT : UIMessage::GET_WIDTH,
                          (child->flags & (horizontal ? UIElement::H_FILL : UIElement::V_FILL)) ? perFill : 0, 0);
      if (childSize > size)
         size = childSize;
   }

   int border = horizontal ? (panel->border.t + panel->border.b) : (panel->border.l + panel->border.r);
   return size + panel->scale(border);
}

int _UIPanelLayout(UIPanel* panel, UIRectangle bounds, bool measure) {
   bool  horizontal = panel->flags & UIPanel::HORIZONTAL;

   int   position   = panel->scale(horizontal ? panel->border.l : panel->border.t);
   if (panel->scrollBar && !measure)
      position -= panel->scrollBar->position;
   int  hSpace        = bounds.width() - panel->scale(panel->border.total_width());
   int  vSpace        = bounds.height() - panel->scale(panel->border.total_height());
   int  count         = 0;
   int  perFill       = _UIPanelCalculatePerFill(panel, &count, hSpace, vSpace, panel->window->scale);
   int  scaledBorder2 = panel->scale(horizontal ? panel->border.t : panel->border.l);
   bool expand        = panel->flags & UIPanel::EXPAND;

   for (auto child : panel->children) {
      if (child->flags & (UIElement::HIDE | UIElement::NON_CLIENT)) {
         continue;
      }

      if (horizontal) {
         int height =
            ((child->flags & UIElement::V_FILL) || expand)
               ? vSpace
               : child->Message(UIMessage::GET_HEIGHT, (child->flags & UIElement::H_FILL) ? perFill : 0, 0);
         int width =
            (child->flags & UIElement::H_FILL) ? perFill : child->Message(UIMessage::GET_WIDTH, height, 0);
         UIRectangle relative = UIRectangle(position, position + width, scaledBorder2 + (vSpace - height) / 2,
                                            scaledBorder2 + (vSpace + height) / 2);
         if (!measure)
            child->Move(translate(relative, bounds), false);
         position += width + panel->scale(panel->gap);
      } else {
         int width =
            ((child->flags & UIElement::H_FILL) || expand)
               ? hSpace
               : child->Message(UIMessage::GET_WIDTH, (child->flags & UIElement::V_FILL) ? perFill : 0, 0);
         int height =
            (child->flags & UIElement::V_FILL) ? perFill : child->Message(UIMessage::GET_HEIGHT, width, 0);
         UIRectangle relative = UIRectangle(scaledBorder2 + (hSpace - width) / 2, scaledBorder2 + (hSpace + width) / 2,
                                            position, position + height);
         if (!measure)
            child->Move(translate(relative, bounds), false);
         position += height + panel->scale(panel->gap);
      }
   }

   return position - panel->scale(count ? panel->gap : 0) +
          panel->scale(horizontal ? panel->border.r : panel->border.b);
}

int _UIPanelMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UIPanel* panel      = (UIPanel*)element;
   bool     horizontal = element->flags & UIPanel::HORIZONTAL;

   if (message == UIMessage::LAYOUT) {
      int         scrollBarWidth = panel->scrollBar ? element->scale(ui_size::SCROLL_BAR) : 0;
      UIRectangle bounds         = element->bounds;
      bounds.r -= scrollBarWidth;

      if (panel->scrollBar) {
         UIRectangle scrollBarBounds = element->bounds;
         scrollBarBounds.l           = scrollBarBounds.r - scrollBarWidth;
         panel->scrollBar->maximum   = _UIPanelLayout(panel, bounds, true);
         panel->scrollBar->page      = element->bounds.height();
         panel->scrollBar->Move(scrollBarBounds, true);
      }

      _UIPanelLayout(panel, bounds, false);
   } else if (message == UIMessage::GET_WIDTH) {
      if (horizontal) {
         return _UIPanelLayout(panel, UIRectangle(0, 0, 0, di), true);
      } else {
         return _UIPanelMeasure(panel, di);
      }
   } else if (message == UIMessage::GET_HEIGHT) {
      if (horizontal) {
         return _UIPanelMeasure(panel, di);
      } else {
         int width = di && panel->scrollBar ? (di -  element->scale(ui_size::SCROLL_BAR)) : di;
         return _UIPanelLayout(panel, UIRectangle(0, width, 0, 0), true);
      }
   } else if (message == UIMessage::PAINT) {
      if (element->flags & UIPanel::COLOR_1) {
         UIDrawBlock((UIPainter*)dp, element->bounds, ui->theme.panel1);
      } else if (element->flags & UIPanel::COLOR_2) {
         UIDrawBlock((UIPainter*)dp, element->bounds, ui->theme.panel2);
      }
   } else if (message == UIMessage::MOUSE_WHEEL && panel->scrollBar) {
      return panel->scrollBar->Message(message, di, dp);
   } else if (message == UIMessage::SCROLLED) {
      element->Refresh();
   } else if (message == UIMessage::GET_CHILD_STABILITY) {
      UIElement* child = (UIElement*)dp;
      return ((element->flags & UIPanel::EXPAND) ? (horizontal ? 2 : 1) : 0) |
             ((child->flags & UIElement::H_FILL) ? 1 : 0) | ((child->flags & UIElement::V_FILL) ? 2 : 0);
   }

   return 0;
}

UIPanel::UIPanel(UIElement* parent, uint32_t flags)
   : UIElement(parent, flags, _UIPanelMessage, "Panel")
   , scrollBar(nullptr)
   , border(0)
   , gap(0) {

   if (flags & UIPanel::LARGE_SPACING) {
      border = UIRectangle(ui_size::PANE_LARGE_BORDER);
      gap    = ui_size::PANE_LARGE_GAP;
   } else if (flags & UIPanel::MEDIUM_SPACING) {
      border = UIRectangle(ui_size::PANE_MEDIUM_BORDER);
      gap    = ui_size::PANE_MEDIUM_GAP;
   } else if (flags & UIPanel::SMALL_SPACING) {
      border = UIRectangle(ui_size::PANE_SMALL_BORDER);
      gap    = ui_size::PANE_SMALL_GAP;
   }

   if (flags & UIPanel::SCROLL) {
      scrollBar = UIScrollBarCreate(this, UIElement::NON_CLIENT);
   }
}

UIPanel* UIPanelCreate(UIElement* parent, uint32_t flags) {
   return new UIPanel(parent, flags);
}

void _UIWrapPanelLayoutRow(UIWrapPanel* panel, uint32_t rowStart, uint32_t rowEnd, int rowY, int rowHeight) {
   int rowPosition = 0;

   for (uint32_t i = rowStart; i < rowEnd; i++) {
      UIElement* child = panel->children[i];
      if (child->flags & UIElement::HIDE)
         continue;
      int         height   = child->Message(UIMessage::GET_HEIGHT, 0, 0);
      int         width    = child->Message(UIMessage::GET_WIDTH, 0, 0);
      UIRectangle relative = UIRectangle(rowPosition, rowPosition + width, rowY + rowHeight / 2 - height / 2,
                                       rowY + rowHeight / 2 + height / 2);
      child->Move(translate(relative, panel->bounds), false);
      rowPosition += width;
   }
}

int _UIWrapPanelMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UIWrapPanel* panel = (UIWrapPanel*)element;

   if (message == UIMessage::LAYOUT || message == UIMessage::GET_HEIGHT) {
      int totalHeight = 0;
      int rowPosition = 0;
      int rowHeight   = 0;
      int rowLimit    = message == UIMessage::LAYOUT ? element->bounds.width() : di;

      uint32_t rowStart = 0;

      for (uint32_t i = 0; i < panel->children.size(); i++) {
         UIElement* child = panel->children[i];
         if (child->flags & UIElement::HIDE)
            continue;

         int height = child->Message(UIMessage::GET_HEIGHT, 0, 0);
         int width  = child->Message(UIMessage::GET_WIDTH, 0, 0);

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

      if (message == UIMessage::GET_HEIGHT) {
         return totalHeight + rowHeight;
      } else {
         _UIWrapPanelLayoutRow(panel, rowStart, panel->children.size(), totalHeight, rowHeight);
      }
   }

   return 0;
}

UIWrapPanel::UIWrapPanel(UIElement* parent, uint32_t flags) :
   UIElement(parent, flags, _UIWrapPanelMessage, "Wrap Panel")
{}

UIWrapPanel* UIWrapPanelCreate(UIElement* parent, uint32_t flags) {
   return new UIWrapPanel(parent, flags);
}

int _UISwitcherMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UISwitcher* switcher = (UISwitcher*)element;

   if (!switcher->active) {
   } else if (message == UIMessage::GET_WIDTH || message == UIMessage::GET_HEIGHT) {
      return switcher->active->Message(message, di, dp);
   } else if (message == UIMessage::LAYOUT) {
      switcher->active->Move(element->bounds, false);
   }

   return 0;
}

void UISwitcherSwitchTo(UISwitcher* switcher, UIElement* child) {
   for (auto child : switcher->children)
      child->flags |= UIElement::HIDE;

   UI_ASSERT(child->parent == switcher);
   child->flags &= ~UIElement::HIDE;
   switcher->active = child;
   UIElementMeasurementsChanged(switcher, 3);
   switcher->Refresh();
}

UISwitcher::UISwitcher(UIElement* parent, uint32_t flags) :
   UIElement(parent, flags, _UISwitcherMessage, "Switcher"),
   active(nullptr)
{}

UISwitcher* UISwitcherCreate(UIElement* parent, uint32_t flags) {
   return new UISwitcher(parent, flags);
}

// --------------------------------------------------
// Checkboxes and buttons.
// --------------------------------------------------

int _UIButtonMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UIButton* button     = (UIButton*)element;
   bool      isMenuItem = element->flags & UIButton::MENU_ITEM;
   bool      isDropDown = element->flags & UIButton::DROP_DOWN;

   if (message == UIMessage::GET_HEIGHT) {
      if (isMenuItem) {
         return element->scale(ui_size::MENU_ITEM_HEIGHT);
      } else {
         return element->scale(ui_size::BUTTON_HEIGHT);
      }
   } else if (message == UIMessage::GET_WIDTH) {
      int labelSize  = UIMeasureStringWidth(button->label);
      int paddedSize = labelSize + element->scale(ui_size::BUTTON_PADDING);
      if (isDropDown)
         paddedSize += ui->activeFont->glyphWidth * 2;
      int minimumSize = element->scale((element->flags & UIButton::SMALL) ? 0
                                       : isMenuItem                       ? ui_size::MENU_ITEM_MINIMUM_WIDTH
                                                                          : ui_size::BUTTON_MINIMUM_WIDTH);
      return paddedSize > minimumSize ? paddedSize : minimumSize;
   } else if (message == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, element->bounds,
                    (isMenuItem   ? UI_DRAW_CONTROL_MENU_ITEM
                     : isDropDown ? UI_DRAW_CONTROL_DROP_DOWN
                                  : UI_DRAW_CONTROL_PUSH_BUTTON) |
                       ((element->flags & UIButton::CHECKED) ? UI_DRAW_CONTROL_STATE_CHECKED : 0) | element->state(),
                    button->label, 0, element->window->scale);
   } else if (message == UIMessage::UPDATE) {
      element->Repaint(NULL);
   } else if (message == UIMessage::DEALLOCATE) {
   } else if (message == UIMessage::LEFT_DOWN) {
      if (element->flags & UIButton::CAN_FOCUS) {
         element->Focus();
      }
   } else if (message == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if ((m->text == " ") || m->code == UIKeycode::ENTER) {
         element->Message(UIMessage::CLICKED, 0, 0);
         element->Repaint(NULL);
         return 1;
      }
   } else if (message == UIMessage::CLICKED) {
      if (button->invoke) {
         button->invoke();
      }
   }

   return 0;
}

void UIButtonSetLabel(UIButton* button, std::string_view string) {
   button->label = string;
   UIElementMeasurementsChanged(button, 1);
   button->Repaint(NULL);
}

UIButton::UIButton(UIElement* parent, uint32_t flags, std::string_view label)
   : UIElement(parent, flags | UIElement::TAB_STOP, _UIButtonMessage, "Button")
   , label(label) {}

UIButton* UIButtonCreate(UIElement* parent, uint32_t flags, std::string_view label) {
   return new UIButton(parent, flags | UIElement::TAB_STOP, label);
}

int _UICheckboxMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UICheckbox* box = (UICheckbox*)element;

   if (message == UIMessage::GET_HEIGHT) {
      return element->scale(ui_size::BUTTON_HEIGHT);
   } else if (message == UIMessage::GET_WIDTH) {
      int labelSize = UIMeasureStringWidth(box->label);
      return element->scale(labelSize + ui_size::CHECKBOX_BOX + ui_size::CHECKBOX_GAP);
   } else if (message == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, element->bounds,
                    UI_DRAW_CONTROL_CHECKBOX |
                       (box->check == UICheckbox::INDETERMINATE ? UI_DRAW_CONTROL_STATE_INDETERMINATE
                        : box->check == UICheckbox::CHECKED     ? UI_DRAW_CONTROL_STATE_CHECKED
                                                                : 0) |
                       element->state(),
                    box->label, 0, element->window->scale);
   } else if (message == UIMessage::UPDATE) {
      element->Repaint(NULL);
   } else if (message == UIMessage::DEALLOCATE) {
   } else if (message == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if (m->text == " ") {
         element->Message(UIMessage::CLICKED, 0, 0);
         element->Repaint(NULL);
      }
   } else if (message == UIMessage::CLICKED) {
      box->check = (box->check + 1) % ((element->flags & UICheckbox::ALLOW_INDETERMINATE) ? 3 : 2);
      element->Repaint(NULL);
      if (box->invoke)
         box->invoke();
   }

   return 0;
}

void UICheckbox::SetLabel(std::string_view new_label) {
   label = new_label;
   UIElementMeasurementsChanged(this, 1);
   Repaint(NULL);
}

UICheckbox::UICheckbox(UIElement* parent, uint32_t flags, std::string_view label)
   : UIElement(parent, flags | UIElement::TAB_STOP, _UICheckboxMessage, "Checkbox")
   , check(0)
   , label(label) {}


UICheckbox* UICheckboxCreate(UIElement* parent, uint32_t flags, std::string_view label) {
   return new UICheckbox(parent, flags | UIElement::TAB_STOP, label);
}

// --------------------------------------------------
// Labels.
// --------------------------------------------------

int _UILabelMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UILabel* label = (UILabel*)element;

   if (message == UIMessage::GET_HEIGHT) {
      return UIMeasureStringHeight();
   } else if (message == UIMessage::GET_WIDTH) {
      return UIMeasureStringWidth(label->label);
   } else if (message == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, element->bounds, UI_DRAW_CONTROL_LABEL | element->state(), label->label,
                    0, element->window->scale);
   } else if (message == UIMessage::DEALLOCATE) {
   }

   return 0;
}

void UILabelSetContent(UILabel* label, std::string_view str) {
   label->label = str;
   UIElementMeasurementsChanged(label, 1);
   label->Repaint(NULL);
}

UILabel::UILabel(UIElement* parent, uint32_t flags, std::string_view label)
   : UIElement(parent, flags | UIElement::TAB_STOP, _UILabelMessage, "Label")
   , label(label) {}

UILabel* UILabelCreate(UIElement* parent, uint32_t flags, std::string_view label) {
   return new UILabel(parent, flags, label);
}

// --------------------------------------------------
// Split panes.
// --------------------------------------------------

int _UISplitPaneMessage(UIElement* element, UIMessage message, int di, void* dp);

int _UISplitterMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UISplitPane* splitPane = (UISplitPane*)element->parent;
   bool         vertical  = splitPane->flags & UIElement::VERTICAL;

   if (message == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, element->bounds,
                    UI_DRAW_CONTROL_SPLITTER | (vertical ? UI_DRAW_CONTROL_STATE_VERTICAL : 0) | element->state(), {},
                    0, element->window->scale);
   } else if (message == UIMessage::GET_CURSOR) {
      return vertical ? (uint32_t)UICursor::split_v : (uint32_t)UICursor::split_h;
   } else if (message == UIMessage::MOUSE_DRAG) {
      int cursor       = vertical ? element->window->cursor.y : element->window->cursor.x;
      int splitterSize = element->scale(ui_size::SPLITTER);
      int space = (vertical ? splitPane->bounds.height() : splitPane->bounds.width()) - splitterSize;
      float oldWeight = splitPane->weight;
      splitPane->weight =
         (float)(cursor - (float)splitterSize / 2 - (vertical ? splitPane->bounds.t : splitPane->bounds.l)) / space;
      std::clamp(splitPane->weight, 0.05f, 0.95f);

      if (splitPane->children[2]->messageClass == _UISplitPaneMessage &&
          (splitPane->children[2]->flags & UIElement::VERTICAL) == (splitPane->flags & UIElement::VERTICAL)) {
         UISplitPane* subSplitPane = (UISplitPane*)splitPane->children[2];
         subSplitPane->weight =
            (splitPane->weight - oldWeight - subSplitPane->weight + oldWeight * subSplitPane->weight) /
            (-1 + splitPane->weight);
         std::clamp(subSplitPane->weight, 0.05f, 0.95f);
      }

      splitPane->Refresh();
   }

   return 0;
}

int _UISplitPaneMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UISplitPane* splitPane = (UISplitPane*)element;
   bool         vertical  = splitPane->flags & UIElement::VERTICAL;

   if (message == UIMessage::LAYOUT) {
      assert(element->children.size() >= 3);
      UIElement* splitter = element->children[0];
      UIElement* left     = element->children[1];
      UIElement* right    = element->children[2];

      int splitterSize = element->scale(ui_size::SPLITTER);
      int space        = (vertical ? element->bounds.height() : element->bounds.width()) - splitterSize;
      int leftSize     = space * splitPane->weight;
      int rightSize    = space - leftSize;

      if (vertical) {
         left->Move(UIRectangle(element->bounds.l, element->bounds.r, element->bounds.t, element->bounds.t + leftSize),
                    false);
         splitter->Move(UIRectangle(element->bounds.l, element->bounds.r, element->bounds.t + leftSize,
                                    element->bounds.t + leftSize + splitterSize),
                        false);
         right->Move(
            UIRectangle(element->bounds.l, element->bounds.r, element->bounds.b - rightSize, element->bounds.b), false);
      } else {
         left->Move(UIRectangle(element->bounds.l, element->bounds.l + leftSize, element->bounds.t, element->bounds.b),
                    false);
         splitter->Move(UIRectangle(element->bounds.l + leftSize, element->bounds.l + leftSize + splitterSize,
                                    element->bounds.t, element->bounds.b),
                        false);
         right->Move(
            UIRectangle(element->bounds.r - rightSize, element->bounds.r, element->bounds.t, element->bounds.b), false);
      }
   }

   return 0;
}

UISplitPane::UISplitPane(UIElement* parent, uint32_t flags, float weight)
   : UIElement(parent, flags, _UISplitPaneMessage, "Split Pane")
   , weight(weight) {
   UIElementCreate(sizeof(UIElement), this, 0, _UISplitterMessage, "Splitter");
}

UISplitPane* UISplitPaneCreate(UIElement* parent, uint32_t flags, float weight) {
   return new UISplitPane(parent, flags, weight);
}

// --------------------------------------------------
// Tab panes.
// --------------------------------------------------

int _UITabPaneMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UITabPane* tabPane = (UITabPane*)element;

   if (message == UIMessage::PAINT) {
      UIPainter*  painter = (UIPainter*)dp;
      UIRectangle top     = element->bounds;
      top.b               = top.t + element->scale(ui_size::BUTTON_HEIGHT);
      UIDrawControl(painter, top, UI_DRAW_CONTROL_TAB_BAND, {}, 0, element->window->scale);

      UIRectangle tab = top;
      tab.l += element->scale(ui_size::TAB_PANE_SPACE_LEFT);
      tab.t += element->scale(ui_size::TAB_PANE_SPACE_TOP);

      tabPane->for_each_tab([&](std::string_view tab_text, uint32_t index, bool active) {
         tab.r = tab.l + UIMeasureStringWidth(tab_text) + ui_size::BUTTON_PADDING;
         UIDrawControl(painter, tab, UI_DRAW_CONTROL_TAB | (active ? UI_DRAW_CONTROL_STATE_SELECTED : 0), tab_text, 0,
                       element->window->scale);
         tab.l = tab.r - 1;
         return true;
      });

   } else if (message == UIMessage::LEFT_DOWN) {
      UIRectangle tab = element->bounds;
      tab.b           = tab.t + element->scale(ui_size::BUTTON_HEIGHT);
      tab.l += element->scale(ui_size::TAB_PANE_SPACE_LEFT);
      tab.t += element->scale(ui_size::TAB_PANE_SPACE_TOP);

      tabPane->for_each_tab([&](std::string_view tab_text, uint32_t index, bool active) {
         tab.r = tab.l + UIMeasureStringWidth(tab_text) + ui_size::BUTTON_PADDING;
         if (tab.contains(element->window->cursor)) {
            tabPane->set_active(index);
            element->Relayout();
            element->Repaint(NULL);
            return false;;
         }
         tab.l = tab.r - 1;
         return true;
      });
   } else if (message == UIMessage::LAYOUT) {
      UIRectangle content = element->bounds;
      content.t += element->scale(ui_size::BUTTON_HEIGHT);

      for (uint32_t index = 0; index < element->children.size(); index++) {
         UIElement* child = element->children[index];

         if (tabPane->get_active() == index) {
            child->flags &= ~UIElement::HIDE;
            child->Move(content, false);
            child->Message(UIMessage::TAB_SELECTED, 0, 0);
         } else {
            child->flags |= UIElement::HIDE;
         }
      }
   } else if (message == UIMessage::GET_HEIGHT) {
      int baseHeight = element->scale(ui_size::BUTTON_HEIGHT);

      for (uint32_t index = 0; index < element->children.size(); index++) {
         UIElement* child = element->children[index];

         if (tabPane->get_active() == index) {
            return baseHeight + child->Message(UIMessage::GET_HEIGHT, di, dp);
         }
      }
   } else if (message == UIMessage::DEALLOCATE) {
   }

   return 0;
}

UITabPane::UITabPane(UIElement* parent, uint32_t flags, const char* tabs)
   : UIElement(parent, flags, _UITabPaneMessage, "Tab Pane")
   , tabs(tabs)
   , active(0) {}

UITabPane* UITabPaneCreate(UIElement* parent, uint32_t flags, const char* tabs) {
   return new UITabPane(parent, flags, tabs);
}

// --------------------------------------------------
// Spacers.
// --------------------------------------------------

int _UISpacerMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UISpacer* spacer = (UISpacer*)element;

   if (message == UIMessage::GET_HEIGHT) {
      return element->scale(spacer->height);
   } else if (message == UIMessage::GET_WIDTH) {
      return element->scale(spacer->width);
   }

   return 0;
}

UISpacer::UISpacer(UIElement* parent, uint32_t flags, int width, int height)
   : UIElement(parent, flags, _UISpacerMessage, "Spacer")
   , width(width)
   , height(height) {}

UISpacer* UISpacerCreate(UIElement* parent, uint32_t flags, int width, int height) {
   return new UISpacer(parent, flags, width, height);
}

// --------------------------------------------------
// Scroll bars.
// --------------------------------------------------

int _UIScrollBarMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UIScrollBar* scrollBar = (UIScrollBar*)element;

   if (message == UIMessage::GET_WIDTH || message == UIMessage::GET_HEIGHT) {
      return element->scale(ui_size::SCROLL_BAR);
   } else if (message == UIMessage::LAYOUT) {
      UIElement* up    = element->children[0];
      UIElement* thumb = element->children[1];
      UIElement* down  = element->children[2];

      if (scrollBar->page >= scrollBar->maximum || scrollBar->maximum <= 0 || scrollBar->page <= 0) {
         up->flags |= UIElement::HIDE;
         thumb->flags |= UIElement::HIDE;
         down->flags |= UIElement::HIDE;

         scrollBar->position = 0;
      } else {
         up->flags &= ~UIElement::HIDE;
         thumb->flags &= ~UIElement::HIDE;
         down->flags &= ~UIElement::HIDE;

         int size      = scrollBar->horizontal ? element->bounds.width() : element->bounds.height();
         int thumbSize = size * scrollBar->page / scrollBar->maximum;

         if (thumbSize < element->scale(ui_size::SCROLL_MINIMUM_THUMB)) {
            thumbSize = element->scale(ui_size::SCROLL_MINIMUM_THUMB);
         }

         if (scrollBar->position < 0) {
            scrollBar->position = 0;
         } else if (scrollBar->position > scrollBar->maximum - scrollBar->page) {
            scrollBar->position = scrollBar->maximum - scrollBar->page;
         }

         int thumbPosition = scrollBar->position / (scrollBar->maximum - scrollBar->page) * (size - thumbSize);

         if (scrollBar->position == scrollBar->maximum - scrollBar->page) {
            thumbPosition = size - thumbSize;
         }

         if (scrollBar->horizontal) {
            UIRectangle r = element->bounds;
            r.r           = r.l + thumbPosition;
            up->Move(r, false);
            r.l = r.r, r.r = r.l + thumbSize;
            thumb->Move(r, false);
            r.l = r.r, r.r = element->bounds.r;
            down->Move(r, false);
         } else {
            UIRectangle r = element->bounds;
            r.b           = r.t + thumbPosition;
            up->Move(r, false);
            r.t = r.b, r.b = r.t + thumbSize;
            thumb->Move(r, false);
            r.t = r.b, r.b = element->bounds.b;
            down->Move(r, false);
         }
      }
   } else if (message == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, element->bounds,
                    UI_DRAW_CONTROL_SCROLL_TRACK |
                       ((scrollBar->page >= scrollBar->maximum || scrollBar->maximum <= 0 || scrollBar->page <= 0)
                           ? UI_DRAW_CONTROL_STATE_DISABLED
                           : 0),
                    {}, 0, element->window->scale);
   } else if (message == UIMessage::MOUSE_WHEEL) {
      scrollBar->position += di;
      element->Refresh();
      element->parent->Message(UIMessage::SCROLLED, 0, 0);
      return 1;
   }

   return 0;
}

int _UIScrollUpDownMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UIScrollBar* scrollBar = (UIScrollBar*)element->parent;
   bool         isDown    = element->cp;

   if (message == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, element->bounds,
                    (isDown ? UI_DRAW_CONTROL_SCROLL_DOWN : UI_DRAW_CONTROL_SCROLL_UP) |
                       (scrollBar->horizontal ? 0 : UI_DRAW_CONTROL_STATE_VERTICAL) | element->state(),
                    {}, 0, element->window->scale);
   } else if (message == UIMessage::UPDATE) {
      element->Repaint(NULL);
   } else if (message == UIMessage::LEFT_DOWN) {
      element->Animate(false);
      scrollBar->lastAnimateTime = UI_CLOCK();
   } else if (message == UIMessage::LEFT_UP) {
      element->Animate(true);
   } else if (message == UIMessage::ANIMATE) {
      UI_CLOCK_T previous     = scrollBar->lastAnimateTime;
      UI_CLOCK_T current      = UI_CLOCK();
      UI_CLOCK_T delta        = current - previous;
      double     deltaSeconds = (double)delta / UI_CLOCKS_PER_SECOND;
      if (deltaSeconds > 0.1)
         deltaSeconds = 0.1;
      double deltaPixels         = deltaSeconds * scrollBar->page * 3;
      scrollBar->lastAnimateTime = current;
      if (isDown)
         scrollBar->position += deltaPixels;
      else
         scrollBar->position -= deltaPixels;
      scrollBar->Refresh();
      scrollBar->parent->Message(UIMessage::SCROLLED, 0, 0);
   }

   return 0;
}

int _UIScrollThumbMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UIScrollBar* scrollBar = (UIScrollBar*)element->parent;

   if (message == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, element->bounds,
                    UI_DRAW_CONTROL_SCROLL_THUMB | (scrollBar->horizontal ? 0 : UI_DRAW_CONTROL_STATE_VERTICAL) |
                       element->state(),
                    {}, 0, element->window->scale);
   } else if (message == UIMessage::UPDATE) {
      element->Repaint(NULL);
   } else if (message == UIMessage::MOUSE_DRAG && element->window->pressedButton == 1) {
      if (!scrollBar->inDrag) {
         scrollBar->inDrag = true;

         if (scrollBar->horizontal) {
            scrollBar->dragOffset = element->bounds.l - scrollBar->bounds.l - element->window->cursor.x;
         } else {
            scrollBar->dragOffset = element->bounds.t - scrollBar->bounds.t - element->window->cursor.y;
         }
      }

      int thumbPosition =
         (scrollBar->horizontal ? element->window->cursor.x : element->window->cursor.y) + scrollBar->dragOffset;
      int size = scrollBar->horizontal ? (scrollBar->bounds.width() - element->bounds.width())
                                       : (scrollBar->bounds.height() - element->bounds.height());
      scrollBar->position = (double)thumbPosition / size * (scrollBar->maximum - scrollBar->page);
      scrollBar->Refresh();
      scrollBar->parent->Message(UIMessage::SCROLLED, 0, 0);
   } else if (message == UIMessage::LEFT_UP) {
      scrollBar->inDrag = false;
   }

   return 0;
}

UIScrollBar::UIScrollBar(UIElement* parent, uint32_t flags)
   : UIElement(parent, flags, _UIScrollBarMessage, "Scroll Bar")
   , maximum(0)
   , page(0)
   , dragOffset(0)
   , position(0)
   , lastAnimateTime(0)
   , inDrag(false)
   , horizontal(flags & UIScrollBar::HORIZONTAL) {
   auto scrollup = new UIElement(this, flags, _UIScrollUpDownMessage, !horizontal ? "Scroll Up" : "Scroll Left");
   scrollup->cp  = (void*)(uintptr_t)0;

   new UIElement(this, flags, _UIScrollThumbMessage, "Scroll Thumb");

   auto scrolldown = new UIElement(this, flags, _UIScrollUpDownMessage, !horizontal ? "Scroll Down" : "Scroll Right");
   scrolldown->cp  = (void*)(uintptr_t)1;
}

UIScrollBar* UIScrollBarCreate(UIElement* parent, uint32_t flags) {
   return new UIScrollBar(parent, flags);
}

// --------------------------------------------------
// Code views.
// --------------------------------------------------

int _UICodeColumnToByte(UICode* code, int line, int column) {
   return _UIColumnToByte(code->line(line), column, code->tabSize);
}

int _UICodeByteToColumn(UICode* code, int line, int byte) {
   return _UIByteToColumn(code->line(line), byte, code->tabSize);
}

void UICode::clear() {
   content.clear();
   lines.clear();
   columns   = 0;
   selection = {};
}

void UICode::load_file(const char* path, std::optional<std::string_view> err /* = {} */) {
   std::string buff = LoadFile(path);
   if (buff.empty())
      insert_content(err ? *err : std::format("The file '{}' could not be loaded.", path), true);
   else
      insert_content(buff, true);
}

void UICode::position_to_byte(int x, int y, size_t* line, size_t* byte) {
   UIFont* previousFont = UIFontActivate(font);
   int     lineHeight   = UIMeasureStringHeight();
   *line                = (size_t)std::max(0., (y - bounds.t + vScroll->position) / lineHeight);
   if (*line >= num_lines())
      *line = num_lines() - 1;
   int column =
      (x - bounds.l + hScroll->position + ui->activeFont->glyphWidth / 2) / ui->activeFont->glyphWidth;
   if (~flags & UICode::NO_MARGIN)
      column -= (ui->code_margin() + ui->code_margin_gap()) / ui->activeFont->glyphWidth;
   UIFontActivate(previousFont);
   *byte = _UICodeColumnToByte(this, *line, column);
}

int UICode::hittest(int x, int y) {
   x -= bounds.l;

   if (x < 0 || x >= vScroll->bounds.l) {
      return 0;
   }

   y -= bounds.t - vScroll->position;

   UIFont* previousFont = UIFontActivate(font);
   int     lineHeight   = UIMeasureStringHeight();
   bool    inMargin     = x < ui->code_margin() + ui->code_margin_gap() / 2 && (~flags & UICode::NO_MARGIN);
   UIFontActivate(previousFont);

   if (y < 0 || y >= lineHeight * (int)num_lines()) {
      return 0;
   }

   int line = y / lineHeight + 1;
   return inMargin ? -line : line;
}

int UIDrawStringHighlighted(UIPainter* painter, UIRectangle lineBounds, std::string_view string,
                            int tabSize, UIStringSelection* selection) {
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
      int       c = Utf8GetCodePoint(string.data(), bytes, &bytesConsumed);

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
         if (!_UICharIsAlpha(c) && !_UICharIsDigit(c)) {
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
         } else if (_UICharIsDigit(c) && !inIdentifier) {
            tokenType = UI_CODE_TOKEN_TYPE_NUMBER;
         } else if (!_UICharIsAlpha(c) && !_UICharIsDigit(c)) {
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

void _UICodeCopyText(void* cp, sel_target_t t) {
   UICode* code = (UICode*)cp;

   int from = code->lines[code->selection[0].line].offset + code->selection[0].offset;
   int to   = code->lines[code->selection[1].line].offset + code->selection[1].offset;

   if (from != to) {
      std::string pasteText;
      pasteText.resize(to - from);
      for (int i = from; i < to; i++)
         pasteText[i - from] = code->content[i];
      _UIClipboardWriteText(code->window, std::move(pasteText), t);
   }
}

void _UICodeUpdateSelection(UICode* code) {
   bool swap =
      code->selection[3].line < code->selection[2].line ||
      (code->selection[3].line == code->selection[2].line && code->selection[3].offset < code->selection[2].offset);
   code->selection[1 - swap]         = code->selection[3];
   code->selection[0 + swap]         = code->selection[2];
   _UICodeCopyText(code, sel_target_t::primary);
   code->moveScrollToCaretNextLayout = true;
   code->Refresh();
}

void _UICodeSetVerticalMotionColumn(UICode* code, bool restore) {
   if (restore) {
      code->selection[3].offset = _UICodeColumnToByte(code, code->selection[3].line, code->verticalMotionColumn);
   } else if (!code->useVerticalMotionColumn) {
      code->useVerticalMotionColumn = true;
      code->verticalMotionColumn    = _UICodeByteToColumn(code, code->selection[3].line, code->selection[3].offset);
   }
}

int _UICodeMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UICode* code = (UICode*)element;

   if (message == UIMessage::LAYOUT) {
      UIFont* previousFont   = UIFontActivate(code->font);
      int     scrollBarSize  = element->scale(ui_size::SCROLL_BAR);
      code->vScroll->maximum = code->num_lines() * UIMeasureStringHeight();
      code->hScroll->maximum = code->columns * code->font->glyphWidth; // TODO This doesn't take into account tab sizes!
      int vSpace = code->vScroll->page = element->bounds.height();
      int hSpace = code->hScroll->page = element->bounds.width();

      if (code->moveScrollToCaretNextLayout) {
         int top     = code->selection[3].line * UIMeasureStringHeight();
         int bottom  = top + UIMeasureStringHeight();
         int context = UIMeasureStringHeight() * 2;
         if (bottom > code->vScroll->position + vSpace - context)
            code->vScroll->position = bottom - vSpace + context;
         if (top < code->vScroll->position + context)
            code->vScroll->position = top - context;
         code->moveScrollToCaretNextLayout = code->moveScrollToFocusNextLayout = false;
         // TODO Horizontal scrolling.
      } else if (code->moveScrollToFocusNextLayout) {
         code->vScroll->position = (code->focused + 0.5) * UIMeasureStringHeight() - code->bounds.height() / 2;
      }

      if (!(code->flags & UICode::NO_MARGIN))
         hSpace -= ui->code_margin() + ui->code_margin_gap();
      UILayoutScrollbarPair(code, hSpace, vSpace, scrollBarSize);

      UIFontActivate(previousFont);
   } else if (message == UIMessage::PAINT) {
      UIFont* previousFont = UIFontActivate(code->font);

      UIPainter*  painter    = (UIPainter*)dp;
      UIRectangle lineBounds = element->bounds;

      lineBounds.r = code->vScroll->bounds.l;

      if (~code->flags & UICode::NO_MARGIN) {
         lineBounds.l += ui->code_margin() + ui->code_margin_gap();
      }

      int lineHeight = UIMeasureStringHeight();
      lineBounds.t -= (int64_t)code->vScroll->position % lineHeight;

      UIDrawBlock(painter, element->bounds, ui->theme.codeBackground);

      UIStringSelection selection = {};
      selection.colorBackground   = ui->theme.selected;
      selection.colorText         = ui->theme.textSelected;

      for (size_t i = code->vScroll->position / lineHeight; i < code->num_lines(); i++) {
         if (lineBounds.t > element->clip.b) {
            break;
         }

         lineBounds.b = lineBounds.t + lineHeight;

         if (~code->flags & UICode::NO_MARGIN) {
            char string[16];
            int  p          = 16;
            int  lineNumber = i + 1;

            while (lineNumber) {
               string[--p] = (lineNumber % 10) + '0';
               lineNumber /= 10;
            }

            UIRectangle marginBounds = lineBounds;
            marginBounds.r           = marginBounds.l - ui->code_margin_gap();
            marginBounds.l -= ui->code_margin() + ui->code_margin_gap();

            uint32_t marginColor = element->Message(UIMessage::CODE_GET_MARGIN_COLOR, i + 1, 0);

            if (marginColor) {
               UIDrawBlock(painter, marginBounds, marginColor);
            }

            UIDrawString(painter, marginBounds, {string + p, static_cast<size_t>(16 - p)},
                         marginColor ? ui->theme.codeDefault : ui->theme.codeComment, UIAlign::right, NULL);
         }

         if ((size_t)code->focused == i) {
            UIDrawBlock(painter, lineBounds, ui->theme.codeFocused);
         }

         UIRectangle oldClip = painter->clip;
         painter->clip       = intersection(oldClip, lineBounds);
         if (code->hScroll)
            lineBounds.l -= (int64_t)code->hScroll->position;
         selection.carets[0] =
            i == code->selection[0].line ? _UICodeByteToColumn(code, i, code->selection[0].offset) : 0;
         selection.carets[1] = i == code->selection[1].line ? _UICodeByteToColumn(code, i, code->selection[1].offset)
                                                            : code->lines[i].bytes;

         bool selected =
            element->window->focused == element && i >= code->selection[0].line && i <= code->selection[1].line;
         int x =
            UIDrawStringHighlighted(painter, lineBounds, code->line(i), code->tabSize, selected ? &selection : nullptr);
         int y = (lineBounds.t + lineBounds.b - UIMeasureStringHeight()) / 2;

         if (element->window->focused == element && i >= code->selection[0].line && i < code->selection[1].line) {
            UIDrawBlock(painter, ui_rect_4pd(x, y, code->font->glyphWidth, code->font->glyphHeight),
                        selection.colorBackground);
         }

         if (code->hScroll)
            lineBounds.l += (int64_t)code->hScroll->position;
         painter->clip = oldClip;

         UICodeDecorateLine m;
         m.x = x, m.y = y, m.bounds = lineBounds, m.index = i + 1, m.painter = painter;
         element->Message(UIMessage::CODE_DECORATE_LINE, 0, &m);

         lineBounds.t += lineHeight;
      }

      UIFontActivate(previousFont);
   } else if (message == UIMessage::SCROLLED) {
      code->moveScrollToFocusNextLayout = false;
      element->Refresh();
   } else if (message == UIMessage::MOUSE_WHEEL) {
      return code->vScroll->Message(message, di, dp);
   } else if (message == UIMessage::GET_CURSOR) {
      if (code->hittest(element->window->cursor.x, element->window->cursor.y) < 0) {
         return (int)UICursor::flipped_arrow;
      }

      if (element->flags & UICode::SELECTABLE) {
         return (int)UICursor::text;
      }
   } else if (message == UIMessage::LEFT_UP) {
      element->Animate(true);
   } else if (message == UIMessage::LEFT_DOWN && code->num_lines()) {
      int hitTest            = code->hittest(element->window->cursor.x, element->window->cursor.y);
      code->leftDownInMargin = hitTest < 0;

      if (hitTest > 0 && (element->flags & UICode::SELECTABLE)) {
         code->position_to_byte(element->window->cursor.x, element->window->cursor.y, &code->selection[2].line,
                                &code->selection[2].offset);
         _UICodeMessage(element, UIMessage::MOUSE_DRAG, di, dp);
         element->Focus();
         element->Animate(false);
         code->lastAnimateTime = UI_CLOCK();
      }
   } else if (message == UIMessage::ANIMATE) {
      if (element->window->pressed == element && element->window->pressedButton == 1 && code->num_lines() &&
          !code->leftDownInMargin) {
         UI_CLOCK_T previous     = code->lastAnimateTime;
         UI_CLOCK_T current      = UI_CLOCK();
         UI_CLOCK_T deltaTicks   = current - previous;
         double     deltaSeconds = (double)deltaTicks / UI_CLOCKS_PER_SECOND;
         if (deltaSeconds > 0.1)
            deltaSeconds = 0.1;
         int delta = deltaSeconds * 800;
         if (!delta) {
            return 0;
         }
         code->lastAnimateTime = current;

         UIFont* previousFont = UIFontActivate(code->font);

         if (element->window->cursor.x <
             element->bounds.l + ((element->flags & UICode::NO_MARGIN)
                                     ? ui->code_margin_gap()
                                     : (ui->code_margin() + ui->code_margin_gap() * 2))) {
            code->hScroll->position -= delta;
         } else if (element->window->cursor.x >= code->vScroll->bounds.l - ui->code_margin_gap()) {
            code->hScroll->position += delta;
         }

         if (element->window->cursor.y < element->bounds.t + ui->code_margin_gap()) {
            code->vScroll->position -= delta;
         } else if (element->window->cursor.y >= code->hScroll->bounds.t - ui->code_margin_gap()) {
            code->vScroll->position += delta;
         }

         code->moveScrollToFocusNextLayout = false;
         UIFontActivate(previousFont);
         _UICodeMessage(element, UIMessage::MOUSE_DRAG, di, dp);
         element->Refresh();
      }
   } else if (message == UIMessage::MOUSE_DRAG && element->window->pressedButton == 1 && code->num_lines() &&
              !code->leftDownInMargin) {
      // TODO Double-click and triple-click dragging for word and line granularity respectively.
      code->position_to_byte(element->window->cursor.x, element->window->cursor.y, &code->selection[3].line,
                             &code->selection[3].offset);
      _UICodeUpdateSelection(code);
      code->moveScrollToFocusNextLayout = code->moveScrollToCaretNextLayout = false;
      code->useVerticalMotionColumn                                         = false;
   } else if (message == UIMessage::KEY_TYPED && code->num_lines()) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if ((m->code == UI_KEYCODE_LETTER('C') || m->code == UI_KEYCODE_LETTER('X') || m->code == UIKeycode::INSERT) &&
          element->window->ctrl && !element->window->alt && !element->window->shift) {
         _UICodeCopyText(code, sel_target_t::clipboard);
      } else if ((m->code == UIKeycode::UP || m->code == UIKeycode::DOWN || m->code == UIKeycode::PAGE_UP ||
                  m->code == UIKeycode::PAGE_DOWN) &&
                 !element->window->ctrl && !element->window->alt) {
         UIFont* previousFont = UIFontActivate(code->font);
         int     lineHeight   = UIMeasureStringHeight();

         if (element->window->shift) {
            if (m->code == UIKeycode::UP) {
               if ((int)code->selection[3].line - 1 >= 0) {
                  _UICodeSetVerticalMotionColumn(code, false);
                  code->selection[3].line--;
                  _UICodeSetVerticalMotionColumn(code, true);
               }
            } else if (m->code == UIKeycode::DOWN) {
               if (code->selection[3].line + 1 < code->num_lines()) {
                  _UICodeSetVerticalMotionColumn(code, false);
                  code->selection[3].line++;
                  _UICodeSetVerticalMotionColumn(code, true);
               }
            } else if (m->code == UIKeycode::PAGE_UP || m->code == UIKeycode::PAGE_DOWN) {
               _UICodeSetVerticalMotionColumn(code, false);
               int pageHeight = (element->bounds.t - code->hScroll->bounds.t) / lineHeight * 4 / 5;
               code->selection[3].line += m->code == UIKeycode::PAGE_UP ? pageHeight : -pageHeight;
               //if (code->selection[3].line < 0)
               //   code->selection[3].line = 0;
               if (code->selection[3].line >= code->num_lines())
                  code->selection[3].line = code->num_lines() - 1;
               _UICodeSetVerticalMotionColumn(code, true);
            }

            _UICodeUpdateSelection(code);
         } else {
            code->moveScrollToFocusNextLayout = false;
            UIKeyInputVScroll(code, m, lineHeight,
                                  (element->bounds.t - code->hScroll->bounds.t) * 4 /
                                     5 /* leave a few lines for context */);
         }

         UIFontActivate(previousFont);
      } else if ((m->code == UIKeycode::HOME || m->code == UIKeycode::END) && !element->window->alt) {
         if (element->window->shift) {
            if (m->code == UIKeycode::HOME) {
               if (element->window->ctrl)
                  code->selection[3].line = 0;
               code->selection[3].offset     = 0;
               code->useVerticalMotionColumn = false;
            } else {
               if (element->window->ctrl)
                  code->selection[3].line = code->num_lines() - 1;
               code->selection[3].offset     = code->lines[code->selection[3].line].bytes;
               code->useVerticalMotionColumn = false;
            }

            _UICodeUpdateSelection(code);
         } else {
            code->vScroll->position           = m->code == UIKeycode::HOME ? 0 : code->vScroll->maximum;
            code->moveScrollToFocusNextLayout = false;
            code->Refresh();
         }
      } else if ((m->code == UIKeycode::LEFT || m->code == UIKeycode::RIGHT) && !element->window->alt) {
         if (element->window->shift) {
            code->move_caret(m->code == UIKeycode::LEFT, element->window->ctrl);
         } else if (!element->window->ctrl) {
            code->hScroll->position +=
               m->code == UIKeycode::LEFT ? -ui->activeFont->glyphWidth : ui->activeFont->glyphWidth;
            code->Refresh();
         } else {
            return 0;
         }
      } else {
         return 0;
      }

      return 1;
   } else if (message == UIMessage::RIGHT_DOWN) {
      int hitTest = code->hittest(element->window->cursor.x, element->window->cursor.y);

      if (hitTest > 0 && (element->flags & UICode::SELECTABLE)) {
         element->Focus();
         UIMenu* menu = UIMenuCreate(element->window, UIMenu::NO_SCROLL);
         UIMenuAddItem(menu,
                       (code->selection[0].line == code->selection[1].line &&
                        code->selection[0].offset == code->selection[1].offset)
                          ? UIElement::DISABLED
                          : 0,
                       "Copy", [=]() { _UICodeCopyText(code, sel_target_t::clipboard); });
         UIMenuShow(menu);
      }
   } else if (message == UIMessage::UPDATE) {
      element->Repaint(NULL);
   } else if (message == UIMessage::DEALLOCATE) {
      code->clear();
   }

   return 0;
}

void UICode::move_caret(bool backward, bool word) {
   while (true) {
      if (backward) {
         if ((int)selection[3].offset - 1 < 0) {
            if (selection[3].line > 0) {
               selection[3].line--;
               selection[3].offset = lines[selection[3].line].bytes;
            } else
               break;
         } else
            _ui_move_caret_backwards(selection[3].offset, &content[0],
                                     lines[selection[3].line].offset + selection[3].offset,
                                     lines[selection[3].line].offset);
      } else {
         if (selection[3].offset + 1 > lines[selection[3].line].bytes) {
            if (selection[3].line + 1 < num_lines()) {
               selection[3].line++;
               selection[3].offset = 0;
            } else
               break;
         } else
            _ui_move_caret_forward(selection[3].offset,  std::string_view(&content[0], size()),
                                   lines[selection[3].line].offset + selection[3].offset);
      }

      if (!word)
         break;

      if (selection[3].offset != 0 && selection[3].offset != lines[selection[3].line].bytes) {
         if (_ui_move_caret_by_word(std::string_view{&content[0], size()},
                                    lines[selection[3].line].offset + selection[3].offset))
            break;
      }
   }

   useVerticalMotionColumn = false;
   _UICodeUpdateSelection(this);
}

// Line numbers are 1-indexed!!
void UICode::focus_line(int index) {
   focused                     = index - 1;
   moveScrollToFocusNextLayout = true;
   Refresh();
}

void UICode::insert_content(std::string_view new_content, bool replace) {
   constexpr size_t max_size = 1000000000;
   if (new_content.size() > max_size)
      new_content = new_content.substr(0, max_size);
   
   useVerticalMotionColumn = false;

   UIFont* previousFont = UIFontActivate(font);

   if (replace) 
      clear();

   if (new_content.empty())
      return;

   size_t sz        = new_content.size();
   size_t orig_size = content.size();

   content.resize(orig_size + sz);

   size_t lineCount = new_content.back() != '\n';

   for (size_t i = 0; i < sz; ++i) {
      content[orig_size + i] = new_content[i];
      
      if (new_content[i] == '\n')
         lineCount++;
   }

   size_t orig_lines = lines.size();
   lines.reserve(orig_lines + lineCount);

   for (size_t i = 0, offset = 0; i <= sz; ++i) {
      if (i == sz || new_content[i] == '\n') {
         emplace_back_line(orig_size + offset, i - offset);
         offset = i + 1;
      }
   }

   if (!replace) {
      vScroll->position = lines.size() * UIMeasureStringHeight();
   }

   UIFontActivate(previousFont);
   Repaint(NULL);
}

UICode::UICode(UIElement* parent, uint32_t flags) :
   UIElement(parent, flags, _UICodeMessage, "Code"),
   font(ui->activeFont),
   focused(-1),
   moveScrollToFocusNextLayout(0),
   leftDownInMargin(0),
   tabSize(4),
   columns(0),
   lastAnimateTime(0),
   verticalMotionColumn(0),
   useVerticalMotionColumn(false),
   moveScrollToCaretNextLayout(false)
{
   vScroll = new UIScrollBar(this, 0);
   hScroll = new UIScrollBar(this, UIScrollBar::HORIZONTAL);
}

UICode* UICodeCreate(UIElement* parent, uint32_t flags) {
   return new UICode(parent, flags);
}

// --------------------------------------------------
// Gauges.
// --------------------------------------------------

int _UIGaugeMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UIGauge* gauge = (UIGauge*)element;

   if (message == UIMessage::GET_HEIGHT) {
      return element->scale(gauge->vertical ? ui_size::GAUGE_WIDTH : ui_size::GAUGE_HEIGHT);
   } else if (message == UIMessage::GET_WIDTH) {
      return element->scale(gauge->vertical ? ui_size::GAUGE_HEIGHT : ui_size::GAUGE_WIDTH);
   } else if (message == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, element->bounds,
                    UI_DRAW_CONTROL_GAUGE | element->state() | (gauge->vertical ? UI_DRAW_CONTROL_STATE_VERTICAL : 0),
                    {}, gauge->position, element->window->scale);
   }

   return 0;
}

void UIGauge::SetPosition(double new_pos) {
   new_pos = std::clamp(new_pos, 0., 1.);
   if (new_pos == position)
      return;
   position = new_pos;
   Repaint(NULL);
}

UIGauge::UIGauge(UIElement* parent, uint32_t flags)
   : UIElement(parent, flags, _UIGaugeMessage, "Gauge")
   , position(0)
   , vertical(!!(flags & UIElement::VERTICAL)) {}

UIGauge* UIGaugeCreate(UIElement* parent, uint32_t flags) {
   return new UIGauge(parent, flags);
}

// --------------------------------------------------
// Sliders.
// --------------------------------------------------

int _UISliderMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UISlider* slider = (UISlider*)element;

   if (message == UIMessage::GET_HEIGHT) {
      return element->scale(slider->vertical ? ui_size::SLIDER_WIDTH : ui_size::SLIDER_HEIGHT);
   } else if (message == UIMessage::GET_WIDTH) {
      return element->scale(slider->vertical ? ui_size::SLIDER_HEIGHT : ui_size::SLIDER_WIDTH);
   } else if (message == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, element->bounds,
                    UI_DRAW_CONTROL_SLIDER | element->state() | (slider->vertical ? UI_DRAW_CONTROL_STATE_VERTICAL : 0),
                    {}, slider->position, element->window->scale);
   } else if (message == UIMessage::LEFT_DOWN || (message == UIMessage::MOUSE_DRAG && element->window->pressedButton == 1)) {
      UIRectangle bounds    = element->bounds;
      int         thumbSize = element->scale(ui_size::SLIDER_THUMB);
      slider->position = slider->vertical ?
          1-((float) (element->window->cursor.y - thumbSize / 2 - bounds.t) / (bounds.height() - thumbSize)) :
          (double) (element->window->cursor.x - thumbSize / 2 - bounds.l) / (bounds.width() - thumbSize);
      if (slider->steps > 1)
         slider->position = (int)(slider->position * (slider->steps - 1) + 0.5f) / (double)(slider->steps - 1);
      if (slider->position < 0)
         slider->position = 0;
      if (slider->position > 1)
         slider->position = 1;
      element->Message(UIMessage::VALUE_CHANGED, 0, 0);
      element->Repaint(NULL);
   } else if (message == UIMessage::UPDATE) {
      element->Repaint(NULL);
   }

   return 0;
}

void UISlider::SetPosition(double new_pos) {
   new_pos = std::clamp(new_pos, 0., 1.);
   if (new_pos == position) return;
   if (steps > 1) position = (int) (position * (steps - 1) + 0.5f) / (float) (steps - 1);
   Message(UIMessage::VALUE_CHANGED, 0, 0);
   Repaint(NULL);
}

UISlider::UISlider(UIElement* parent, uint32_t flags)
   : UIElement(parent, flags, _UISliderMessage, "Slider")
   , position(0)
   , steps(0)
   , vertical(!!(flags & UIElement::VERTICAL)) {}

UISlider* UISliderCreate(UIElement* parent, uint32_t flags) {
   return new UISlider(parent, flags);
}

// --------------------------------------------------
// Tables.
// --------------------------------------------------

int UITableHitTest(UITable* table, int x, int y) {
   x -= table->bounds.l;

   if (x < 0 || x >= table->vScroll->bounds.l) {
      return -1;
   }

   y -= (table->bounds.t + table->scale(ui_size::TABLE_HEADER)) - table->vScroll->position;

   int rowHeight = table->scale(ui_size::TABLE_ROW);

   if (y < 0 || y >= (int)(rowHeight * table->itemCount)) {
      return -1;
   }

   return y / rowHeight;
}

int UITableHeaderHitTest(UITable* table, int x, int y) {
   if (table->columnWidths.empty())
      return -1;
   UIRectangle header = table->bounds;
   header.b           = header.t + table->scale(ui_size::TABLE_HEADER);
   header.l += table->scale(ui_size::TABLE_COLUMN_GAP);
   int position = 0, index = 0;

   while (true) {
      size_t end = table->column_end(position);
      header.r = header.l + table->columnWidths[index];
      if (header.contains(x, y))
         return index;
      header.l += table->columnWidths[index] + table->scale(ui_size::TABLE_COLUMN_GAP);
      if (table->columns[end] != '\t')
         break;
      position = end + 1;
      index++;
   }

   return -1;
}

bool UITableEnsureVisible(UITable* table, int index) {
   int rowHeight = table->scale(ui_size::TABLE_ROW);
   int y         = index * rowHeight;
   y -= table->vScroll->position;
   int height = table->bounds.height() - table->scale(ui_size::TABLE_HEADER) - rowHeight;

   if (y < 0) {
      table->vScroll->position += y;
      table->Refresh();
      return true;
   } else if (y > height) {
      table->vScroll->position -= height - y;
      table->Refresh();
      return true;
   } else {
      return false;
   }
}

void UITableResizeColumns(UITable* table) {
   size_t position = 0;
   size_t count    = 0;

   while (true) {
      size_t end = table->column_end(position);
      count++;
      if (table->columns[end] == '\t')
         position = end + 1;
      else
         break;
   }

   table->columnWidths.resize(count);

   position = 0;

   UITableGetItem m(256);

   while (true) {
      size_t end = table->column_end(position);
      int longest = UIMeasureStringWidth(table->column(position, end));

      for (size_t i = 0; i < (size_t)table->itemCount; i++) {
         m.index   = i;
         int bytes = table->Message(UIMessage::TABLE_GET_ITEM, 0, &m);
         int width = UIMeasureStringWidth(m.buff(bytes));

         if (width > longest) {
            longest = width;
         }
      }

      table->columnWidths[m.column] = longest;
      m.column++;
      if (table->columns[end] == '\t')
         position = end + 1;
      else
         break;
   }

   table->Repaint(NULL);
}

int _UITableMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UITable* table = (UITable*)element;

   if (message == UIMessage::PAINT) {
      UIPainter*  painter = (UIPainter*)dp;
      UIRectangle bounds  = element->bounds;
      bounds.r            = table->vScroll->bounds.l;
      UIDrawControl(painter, element->bounds, UI_DRAW_CONTROL_TABLE_BACKGROUND | element->state(), {}, 0,
                    element->window->scale);
      UIRectangle    row       = bounds;
      int            rowHeight = element->scale(ui_size::TABLE_ROW);
      UITableGetItem m(256);
      row.t += table->scale(ui_size::TABLE_HEADER);
      row.t -= (int64_t)table->vScroll->position % rowHeight;
      int         hovered = UITableHitTest(table, element->window->cursor.x, element->window->cursor.y);
      UIRectangle oldClip = painter->clip;
      painter->clip       = intersection(
         oldClip, UIRectangle(bounds.l, bounds.r, bounds.t + element->scale(ui_size::TABLE_HEADER), bounds.b));

      for (int i = table->vScroll->position / rowHeight; i < (int)table->itemCount; i++) {
         if (row.t > painter->clip.b) {
            break;
         }

         row.b        = row.t + rowHeight;
         m.index      = i;
         m.isSelected = false;
         m.column     = 0;
         int bytes    = element->Message(UIMessage::TABLE_GET_ITEM, 0, &m);

         uint32_t rowFlags =
            (m.isSelected ? UI_DRAW_CONTROL_STATE_SELECTED : 0) | (hovered == i ? UI_DRAW_CONTROL_STATE_HOVERED : 0);
         UIDrawControl(painter, row, UI_DRAW_CONTROL_TABLE_ROW | rowFlags, {}, 0, element->window->scale);

         UIRectangle cell = row;
         cell.l += table->scale(ui_size::TABLE_COLUMN_GAP) - (int64_t)table->hScroll->position;

         for (size_t j = 0; j < table->columnWidths.size(); j++) {
            if (j) {
               m.column = j;
               bytes    = element->Message(UIMessage::TABLE_GET_ITEM, 0, &m);
            }

            cell.r = cell.l + table->columnWidths[j];
            if ((size_t)bytes > m.buff_size() && bytes > 0)
               bytes = m.buff_size();
            if (bytes > 0)
               UIDrawControl(painter, cell, UI_DRAW_CONTROL_TABLE_CELL | rowFlags, m.buff(bytes), 0,
                             element->window->scale);
            cell.l += table->columnWidths[j] + table->scale(ui_size::TABLE_COLUMN_GAP);
         }

         row.t += rowHeight;
      }

      bounds        = element->bounds;
      painter->clip = intersection(oldClip, bounds);
      if (table->hScroll)
         bounds.l -= (int64_t)table->hScroll->position;

      UIRectangle header = bounds;
      header.b           = header.t + table->scale(ui_size::TABLE_HEADER);
      header.l += table->scale(ui_size::TABLE_COLUMN_GAP);

      size_t position = 0;
      int index    = 0;

      if (!table->columnWidths.empty()) {
         while (true) {
            size_t end = table->column_end(position);

            header.r = header.l + table->columnWidths[index];
            UIDrawControl(painter, header,
                          UI_DRAW_CONTROL_TABLE_HEADER |
                             (index == table->columnHighlight ? UI_DRAW_CONTROL_STATE_SELECTED : 0),
                          table->column(position, end), 0, element->window->scale);
            header.l += table->columnWidths[index] + table->scale(ui_size::TABLE_COLUMN_GAP);

            if (table->columns[end] == '\t') {
               position = end + 1;
               index++;
            } else {
               break;
            }
         }
      }
   } else if (message == UIMessage::LAYOUT) {
      int scrollBarSize = table->scale(ui_size::SCROLL_BAR);
      int columnGap     = table->scale(ui_size::TABLE_COLUMN_GAP);

      table->vScroll->maximum = table->itemCount * element->scale(ui_size::TABLE_ROW);
      table->hScroll->maximum = columnGap;
      for (auto width : table->columnWidths) {
         table->hScroll->maximum += width + columnGap;
      }

      int vSpace = table->vScroll->page =
         element->bounds.height() - element->scale(ui_size::TABLE_HEADER);
      int hSpace = table->hScroll->page = element->bounds.width();
      UILayoutScrollbarPair(table, hSpace, vSpace, scrollBarSize);
   } else if (message == UIMessage::MOUSE_MOVE || message == UIMessage::UPDATE) {
      element->Repaint(NULL);
   } else if (message == UIMessage::SCROLLED) {
      element->Refresh();
   } else if (message == UIMessage::MOUSE_WHEEL) {
      return table->vScroll->Message(message, di, dp);
   } else if (message == UIMessage::LEFT_DOWN) {
      element->Focus();
   } else if (message == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if ((m->code == UIKeycode::UP || m->code == UIKeycode::DOWN || m->code == UIKeycode::PAGE_UP ||
           m->code == UIKeycode::PAGE_DOWN || m->code == UIKeycode::HOME || m->code == UIKeycode::END) &&
          !element->window->ctrl && !element->window->alt && !element->window->shift) {
         UIKeyInputVScroll(table, m, element->scale(ui_size::TABLE_ROW),
                           (element->bounds.t - table->hScroll->bounds.t + ui_size::TABLE_HEADER) * 4 / 5);
         return 1;
      } else if ((m->code == UIKeycode::LEFT || m->code == UIKeycode::RIGHT) && !element->window->ctrl &&
                 !element->window->alt && !element->window->shift) {
         table->hScroll->position +=
            m->code == UIKeycode::LEFT ? -ui->activeFont->glyphWidth : ui->activeFont->glyphWidth;
         table->Refresh();
         return 1;
      }
   } else if (message == UIMessage::DEALLOCATE) {
   }

   return 0;
}

UITable::UITable(UIElement* parent, uint32_t flags, const char* columns) :
   UIElement(parent, flags, _UITableMessage, "Table"),
   vScroll(new UIScrollBar(this, 0)),
   hScroll(new UIScrollBar(this, UIScrollBar::HORIZONTAL)),
   itemCount(0),
   columns(columns),
   columnHighlight(-1)
{}

UITable* UITableCreate(UIElement* parent, uint32_t flags, const char* columns) {
   return new UITable(parent, flags, columns);
}

// --------------------------------------------------
// Textboxes.
// --------------------------------------------------

int _UITextboxByteToColumn(std::string_view string, int byte) {
   return _UIByteToColumn(string, byte, 4);
}

int _UITextboxColumnToByte(std::string_view string, int column) {
   return _UIColumnToByte(string, column, 4);
}

void UITextboxReplace(UITextbox* textbox, std::string_view text, bool sendChangedMessage) {
   auto sz = textbox->buffer.size();
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
      textbox->Message(UIMessage::VALUE_CHANGED, 0, 0);
   textbox->window->textboxModifiedFlag = true;
   textbox->Repaint(NULL);
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

   textbox->Repaint(NULL);
}

void UITextboxCopyText(void* cp) {
   UITextbox* textbox = (UITextbox*)cp;

   int from = std::min(textbox->carets[0], textbox->carets[1]);
   int to   = std::max(textbox->carets[0], textbox->carets[1]);

   if (from != to) {
      auto text = textbox->text();
      std::string pasteText;
      pasteText.resize(to - from);
      for (int i = from; i < to; i++)
         pasteText[i - from] = text[i];
      _UIClipboardWriteText(textbox->window, std::move(pasteText), sel_target_t::clipboard);
   }
}

void UITextboxPasteText(void* cp, sel_target_t t) {
   UITextbox*  textbox = (UITextbox*)cp;
   std::string text = _UIClipboardReadText(textbox->window, t);

   if (!text.empty()) {
      for (auto& c : text)
         if (c == '\n')
            c = ' ';

      UITextboxReplace(textbox, text, true);
   }
}

int _UITextboxMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UITextbox* textbox = (UITextbox*)element;

   if (message == UIMessage::GET_HEIGHT) {
      return element->scale(ui_size::TEXTBOX_HEIGHT);
   } else if (message == UIMessage::GET_WIDTH) {
      return element->scale(ui_size::TEXTBOX_WIDTH);
   } else if (message == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, element->bounds, UI_DRAW_CONTROL_TEXTBOX | element->state(), {}, 0,
                    element->window->scale);

      int         scaledMargin = element->scale(ui_size::TEXTBOX_MARGIN);
      int         totalWidth   = UIMeasureStringWidth(textbox->text()) + scaledMargin * 2;
      UIRectangle textBounds   = element->bounds + ui_rect_1i(scaledMargin);

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
                      (element->flags & UIElement::DISABLED) ? ui->theme.textDisabled : ui->theme.text, UIAlign::left,
                      element->window->focused == element ? &selection : NULL);
   } else if (message == UIMessage::GET_CURSOR) {
      return (int)UICursor::text;
   } else if (message == UIMessage::LEFT_DOWN) {
      int column = (element->window->cursor.x - element->bounds.l + textbox->scroll -
                    element->scale(ui_size::TEXTBOX_MARGIN) + ui->activeFont->glyphWidth / 2) /
                   ui->activeFont->glyphWidth;
      textbox->carets[0] = textbox->carets[1] = column <= 0 ? 0 : _UITextboxColumnToByte(textbox->text(), column);
      element->Focus();
   } else if (message == UIMessage::UPDATE) {
      element->Repaint(NULL);
   } else if (message == UIMessage::DEALLOCATE) {
      ;
   } else if (message == UIMessage::KEY_TYPED) {
      UIKeyTyped* m       = (UIKeyTyped*)dp;
      bool        handled = true;

      if (textbox->rejectNextKey) {
         textbox->rejectNextKey = false;
         handled                = false;
      } else if (m->code == UIKeycode::BACKSPACE || m->code == UIKeycode::DEL) {
         if (textbox->carets[0] == textbox->carets[1]) {
            UITextboxMoveCaret(textbox, m->code == UIKeycode::BACKSPACE, element->window->ctrl);
         }

         UITextboxReplace(textbox, "", true);
      } else if (m->code == UIKeycode::LEFT || m->code == UIKeycode::RIGHT) {
         if (textbox->carets[0] == textbox->carets[1] || element->window->shift) {
            UITextboxMoveCaret(textbox, m->code == UIKeycode::LEFT, element->window->ctrl);
            if (!element->window->shift)
               textbox->carets[1] = textbox->carets[0];
         } else {
            textbox->carets[1 - element->window->shift] = textbox->carets[element->window->shift];
         }
      } else if (m->code == UIKeycode::HOME || m->code == UIKeycode::END) {
         if (m->code == UIKeycode::HOME) {
            textbox->carets[0] = 0;
         } else {
            textbox->carets[0] = textbox->text().size();
         }

         if (!element->window->shift) {
            textbox->carets[1] = textbox->carets[0];
         }
      } else if (m->code == UI_KEYCODE_LETTER('A') && element->window->ctrl) {
         textbox->carets[1] = 0;
         textbox->carets[0] = textbox->text().size();
      } else if (m->text.size() && !element->window->alt && !element->window->ctrl && m->text[0] >= 0x20) {
         UITextboxReplace(textbox, m->text, true);
      } else if ((m->code == UI_KEYCODE_LETTER('C') || m->code == UI_KEYCODE_LETTER('X') ||
                  m->code == UIKeycode::INSERT) &&
                 element->window->ctrl && !element->window->alt && !element->window->shift) {
         UITextboxCopyText(textbox);

         if (m->code == UI_KEYCODE_LETTER('X')) {
            UITextboxReplace(textbox, "", true);
         }
      } else if ((m->code == UI_KEYCODE_LETTER('V') && element->window->ctrl && !element->window->alt &&
                  !element->window->shift) ||
                 (m->code == UIKeycode::INSERT && !element->window->ctrl && !element->window->alt &&
                  element->window->shift)) {
         UITextboxPasteText(textbox, sel_target_t::clipboard);
      } else {
         handled = false;
      }

      if (handled) {
         element->Repaint(NULL);
         return 1;
      }
   } else if (message == UIMessage::RIGHT_DOWN) {
      int c0 = textbox->carets[0], c1 = textbox->carets[1];
      _UITextboxMessage(element, UIMessage::LEFT_DOWN, di, dp);

      if (c0 < c1 ? (textbox->carets[0] >= c0 && textbox->carets[0] < c1)
                  : (textbox->carets[0] >= c1 && textbox->carets[0] < c0)) {
         textbox->carets[0] = c0,
         textbox->carets[1] = c1; // Only move caret if clicking outside the existing selection.
      }

      UIMenu* menu = UIMenuCreate(element->window, UIMenu::NO_SCROLL);
      UIMenuAddItem(menu, textbox->carets[0] == textbox->carets[1] ? UIElement::DISABLED : 0, "Copy",
                    [=]() { UITextboxCopyText(textbox); });
      std::string paste = _UIClipboardReadText(textbox->window, sel_target_t::clipboard);
      UIMenuAddItem(menu, paste.empty() ? UIElement::DISABLED : 0, "Paste",
                    [=]() { UITextboxPasteText(textbox, sel_target_t::clipboard); });
      UIMenuShow(menu);
   } else if (message == UIMessage::MIDDLE_DOWN) {
      UITextboxPasteText(textbox, sel_target_t::primary);
      element->Repaint(NULL);
      return 1;
   }

   return 0;
}

UITextbox::UITextbox(UIElement* parent, uint32_t flags) :
   UIElement( parent, flags | UIElement::TAB_STOP, _UITextboxMessage, "Textbox"),
   carets({0, 0}),
   scroll(0),
   rejectNextKey(false)
{}

UITextbox* UITextboxCreate(UIElement* parent, uint32_t flags) {
   return new UITextbox(parent, flags);
}

// --------------------------------------------------
// MDI clients.
// --------------------------------------------------
int _UIMDIChildHitTest(UIMDIChild* mdiChild, int x, int y) {
   UIElement* element = mdiChild;
   auto [titleSize, borderSize, titleRect, contentRect] =
      ui_mdi_child_calculate_layout(element->bounds, element->window->scale);
   int cornerSize = element->scale(ui_size::MDI_CHILD_CORNER);
   if (!element->bounds.contains(x, y) || contentRect.contains(x, y))
      return -1;
   else if (x < element->bounds.l + cornerSize && y < element->bounds.t + cornerSize)
      return 0b1010;
   else if (x > element->bounds.r - cornerSize && y < element->bounds.t + cornerSize)
      return 0b0110;
   else if (x < element->bounds.l + cornerSize && y > element->bounds.b - cornerSize)
      return 0b1001;
   else if (x > element->bounds.r - cornerSize && y > element->bounds.b - cornerSize)
      return 0b0101;
   else if (x < element->bounds.l + borderSize)
      return 0b1000;
   else if (x > element->bounds.r - borderSize)
      return 0b0100;
   else if (y < element->bounds.t + borderSize)
      return 0b0010;
   else if (y > element->bounds.b - borderSize)
      return 0b0001;
   else if (titleRect.contains(x, y))
      return 0b1111;
   else
      return -1;
}

void _UIMDIChildCloseButton(void* _child) {
   UIElement* child = (UIElement*)_child;

   if (!child->Message(UIMessage::WINDOW_CLOSE, 0, 0)) {
      child->Destroy();
      child->parent->Refresh();
   }
}

int _UIMDIChildMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UIMDIChild* mdiChild = (UIMDIChild*)element;

   if (message == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, element->bounds, UI_DRAW_CONTROL_MDI_CHILD, mdiChild->title,
                    0, element->window->scale);
   } else if (message == UIMessage::GET_WIDTH) {
      UIElement* child = element->children.empty() ? nullptr : element->children.back();
      int        width = 2 * ui_size::MDI_CHILD_BORDER;
      width += (child ? child->Message(message,
                                         di ? (di - ui_size::MDI_CHILD_TITLE + ui_size::MDI_CHILD_BORDER) : 0, dp)
                      : 0);
      if (width < ui_size::MDI_CHILD_MINIMUM_WIDTH)
         width = ui_size::MDI_CHILD_MINIMUM_WIDTH;
      return width;
   } else if (message == UIMessage::GET_HEIGHT) {
      UIElement* child  = element->children.empty() ? nullptr : element->children.back();
      int        height = ui_size::MDI_CHILD_TITLE + ui_size::MDI_CHILD_BORDER;
      height += (child ? child->Message(message, di ? (di - 2 * ui_size::MDI_CHILD_BORDER) : 0, dp) : 0);
      if (height < ui_size::MDI_CHILD_MINIMUM_HEIGHT)
         height = ui_size::MDI_CHILD_MINIMUM_HEIGHT;
      return height;
   } else if (message == UIMessage::LAYOUT) {
      auto [titleSize, borderSize, titleRect, contentRect] =
         ui_mdi_child_calculate_layout(element->bounds, element->window->scale);

      int position = titleRect.r;

      for (auto child : element->children) {
         int        width = child->Message(UIMessage::GET_WIDTH, 0, 0);
         child->Move(UIRectangle(position - width, position, titleRect.t, titleRect.b), false);
         position -= width;
      }

      UIElement* child = element->children.empty() ? nullptr : element->children.back();

      if (child) {
         child->Move(contentRect, false);
      }
   } else if (message == UIMessage::GET_CURSOR) {
      int hitTest = _UIMDIChildHitTest(mdiChild, element->window->cursor.x, element->window->cursor.y);
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
   } else if (message == UIMessage::LEFT_DOWN) {
      mdiChild->dragHitTest = _UIMDIChildHitTest(mdiChild, element->window->cursor.x, element->window->cursor.y);
      mdiChild->dragOffset =
         element->bounds + UIRectangle(-element->window->cursor.x, -element->window->cursor.y);
   } else if (message == UIMessage::LEFT_UP) {
      if (mdiChild->bounds.l < 0)
         mdiChild->bounds.r -= mdiChild->bounds.l, mdiChild->bounds.l = 0;
      if (mdiChild->bounds.t < 0)
         mdiChild->bounds.b -= mdiChild->bounds.t, mdiChild->bounds.t = 0;
      element->parent->Refresh();
   } else if (message == UIMessage::MOUSE_DRAG) {
      if (mdiChild->dragHitTest > 0) {

#define _UI_MDI_CHILD_MOVE_EDGE(bit, edge, cursor, size, opposite, negate, minimum, offset)                         \
   if (mdiChild->dragHitTest & bit)                                                                                 \
      mdiChild->bounds.edge = mdiChild->dragOffset.edge + element->window->cursor - element->parent->bounds.offset; \
   if ((mdiChild->dragHitTest & bit) && mdiChild->bounds.size() < minimum)                                          \
      mdiChild->bounds.edge = mdiChild->bounds.opposite negate minimum;

         _UI_MDI_CHILD_MOVE_EDGE(0b1000, l, cursor.x, width, r, -, ui_size::MDI_CHILD_MINIMUM_WIDTH, l);
         _UI_MDI_CHILD_MOVE_EDGE(0b0100, r, cursor.x, width, l, +, ui_size::MDI_CHILD_MINIMUM_WIDTH, l);
         _UI_MDI_CHILD_MOVE_EDGE(0b0010, t, cursor.y, height, b, -, ui_size::MDI_CHILD_MINIMUM_HEIGHT, t);
         _UI_MDI_CHILD_MOVE_EDGE(0b0001, b, cursor.y, height, t, +, ui_size::MDI_CHILD_MINIMUM_HEIGHT, t);
         element->parent->Refresh();
      }
   } else if (message == UIMessage::DESTROY) {
      UIMDIClient* client = (UIMDIClient*)element->parent;
      if (client->active == mdiChild) {
         client->active =
            (UIMDIChild*)(client->children.size() == 1 ? NULL : client->children[client->children.size() - 2]); // todo: seems wrong
      }
   } else if (message == UIMessage::DEALLOCATE) {
   }

   return 0;
}

int _UIMDIClientMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UIMDIClient* client = (UIMDIClient*)element;

   if (message == UIMessage::PAINT) {
      if (~element->flags & UIMDIClient::_TRANSPARENT) {
         UIDrawBlock((UIPainter*)dp, element->bounds, ui->theme.panel2);
      }
   } else if (message == UIMessage::LAYOUT) {
      for (auto mdiChild : element->children) {
         UI_ASSERT(mdiChild->messageClass == _UIMDIChildMessage);

         if (mdiChild->bounds == UIRectangle(0)) {
            int width  = mdiChild->Message(UIMessage::GET_WIDTH, 0, 0);
            int height = mdiChild->Message(UIMessage::GET_HEIGHT, width, 0);
            if (client->cascade + width > element->bounds.r || client->cascade + height > element->bounds.b)
               client->cascade = 0;
            mdiChild->bounds =
               UIRectangle(client->cascade, client->cascade + width, client->cascade, client->cascade + height);
            client->cascade += element->scale(ui_size::MDI_CASCADE);
         }

         UIRectangle bounds = mdiChild->bounds + UIRectangle(element->bounds.l, element->bounds.t);
         mdiChild->Move(bounds, false);
      }
   } else if (message == UIMessage::PRESSED_DESCENDENT) {
      UIMDIChild* child = (UIMDIChild*)dp;

      if (child && child != client->active) {
         for (uint32_t i = 0; i < element->children.size(); i++) {
            if (element->children[i] == child) {
               element->children.erase(element->children.begin() + i);
               element->children.push_back(child);
               break;
            }
         }

         client->active = child;
         element->Refresh();
      }
   }

   return 0;
}

UIMDIChild::UIMDIChild(UIElement* parent, uint32_t flags, const UIRectangle& initialBounds, std::string_view title)
   : UIElement(parent, flags, _UIMDIChildMessage, "MDIChild")
   , bounds(initialBounds)
   , title(title)
   , dragHitTest(0)
   , dragOffset(0) {
   UI_ASSERT(parent->messageClass == _UIMDIClientMessage);
   UIMDIClient* mdiClient = (UIMDIClient*)parent;

   mdiClient->active = this;

   if (flags & UIMDIChild::CLOSE_BUTTON) {
      UIButton* closeButton = UIButtonCreate(this, UIButton::SMALL | UIElement::NON_CLIENT, "X");
      closeButton->invoke   = [this]() { _UIMDIChildCloseButton(this); };
   }
}

UIMDIChild* UIMDIChildCreate(UIElement* parent, uint32_t flags, UIRectangle initialBounds, std::string_view title) {
   return new UIMDIChild(parent, flags, initialBounds, title);
}

UIMDIClient::UIMDIClient(UIElement* parent, uint32_t flags) :
   UIElement(parent, flags, _UIMDIClientMessage, "MDIClient"),
   active(nullptr),
   cascade(0)
{}

UIMDIClient* UIMDIClientCreate(UIElement* parent, uint32_t flags) {
   return new UIMDIClient(parent, flags);
}

// --------------------------------------------------
// Image displays.
// --------------------------------------------------

void _UIImageDisplayUpdateViewport(UIImageDisplay* display) {
   UIRectangle bounds = display->bounds;
   bounds.r -= bounds.l, bounds.b -= bounds.t;

   float minimumZoomX = 1, minimumZoomY = 1;
   if (display->width > bounds.r)
      minimumZoomX = (float)bounds.r / display->width;
   if (display->height > bounds.b)
      minimumZoomY = (float)bounds.b / display->height;
   float minimumZoom = minimumZoomX < minimumZoomY ? minimumZoomX : minimumZoomY;

   if (display->zoom < minimumZoom || (display->flags & UIImageDisplay::ZOOM_FIT)) {
      display->zoom = minimumZoom;
      display->flags |= UIImageDisplay::ZOOM_FIT;
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

int _UIImageDisplayMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UIImageDisplay* display = (UIImageDisplay*)element;

   if (message == UIMessage::GET_HEIGHT) {
      return display->height;
   } else if (message == UIMessage::GET_WIDTH) {
      return display->width;
   } else if (message == UIMessage::DEALLOCATE) {
      free(display->bits);
   } else if (message == UIMessage::PAINT) {
      UIPainter* painter = (UIPainter*)dp;

      int w = element->bounds.width(), h = element->bounds.height();
      int x = _UILinearMap(0, display->panX, display->panX + w / display->zoom, 0, w) + element->bounds.l;
      int y = _UILinearMap(0, display->panY, display->panY + h / display->zoom, 0, h) + element->bounds.t;

      UIRectangle image =
         UIRectangle(x, x + (int)(display->width * display->zoom), y, (int)(y + display->height * display->zoom));
      UIRectangle bounds = intersection(painter->clip, intersection(display->bounds, image));
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
   } else if (message == UIMessage::MOUSE_WHEEL && (element->flags & UIImageDisplay::INTERACTIVE)) {
      display->flags &= ~UIImageDisplay::ZOOM_FIT;
      int   divisions   = -di / 72;
      float factor      = 1;
      float perDivision = element->window->ctrl ? 2.0f : element->window->alt ? 1.01f : 1.2f;
      while (divisions > 0)
         factor *= perDivision, divisions--;
      while (divisions < 0)
         factor /= perDivision, divisions++;
      if (display->zoom * factor > 64)
         factor = 64 / display->zoom;
      int mx = element->window->cursor.x - element->bounds.l;
      int my = element->window->cursor.y - element->bounds.t;
      display->zoom *= factor;
      display->panX -= mx / display->zoom * (1 - factor);
      display->panY -= my / display->zoom * (1 - factor);
      _UIImageDisplayUpdateViewport(display);
      display->Repaint(NULL);
   } else if (message == UIMessage::LAYOUT && (element->flags & UIImageDisplay::INTERACTIVE)) {
      UIRectangle bounds = display->bounds;
      bounds.r -= bounds.l, bounds.b -= bounds.t;
      display->panX -= (bounds.r - display->previousWidth) / 2 / display->zoom;
      display->panY -= (bounds.b - display->previousHeight) / 2 / display->zoom;
      display->previousWidth = bounds.r, display->previousHeight = bounds.b;
      _UIImageDisplayUpdateViewport(display);
   } else if (message == UIMessage::GET_CURSOR && (element->flags & UIImageDisplay::INTERACTIVE) &&
              (element->bounds.width() < display->width * display->zoom ||
               element->bounds.height() < display->height * display->zoom)) {
      return (int)UICursor::hand;
   } else if (message == UIMessage::MOUSE_DRAG) {
      display->panX -= (element->window->cursor.x - display->previousPanPointX) / display->zoom;
      display->panY -= (element->window->cursor.y - display->previousPanPointY) / display->zoom;
      _UIImageDisplayUpdateViewport(display);
      display->previousPanPointX = element->window->cursor.x;
      display->previousPanPointY = element->window->cursor.y;
      element->Repaint(NULL);
   } else if (message == UIMessage::LEFT_DOWN) {
      display->flags &= ~UIImageDisplay::ZOOM_FIT;
      display->previousPanPointX = element->window->cursor.x;
      display->previousPanPointY = element->window->cursor.y;
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

   UIElementMeasurementsChanged(display, 3);
   display->Repaint(NULL);
}

UIImageDisplay::UIImageDisplay(UIElement* parent, uint32_t flags, uint32_t* bits, size_t width, size_t height,
                               size_t stride) :
   UIElement(parent, flags, _UIImageDisplayMessage, "ImageDisplay"),
   bits(bits),
   width(width),
   height(height),
   panX(0),
   panY(0),
   zoom(1),
   previousWidth(0),
   previousHeight(0),
   previousPanPointX(0),
   previousPanPointY(0)
{
   UIImageDisplaySetContent(this, bits, width, height, stride);
}

UIImageDisplay* UIImageDisplayCreate(UIElement* parent, uint32_t flags, uint32_t* bits, size_t width, size_t height,
                                     size_t stride) {
   return new UIImageDisplay(parent, flags, bits, width, height, stride);
}

// --------------------------------------------------
// Modal dialogs.
// --------------------------------------------------

int _UIDialogWrapperMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::LAYOUT) {
      int         width  = element->children[0]->Message(UIMessage::GET_WIDTH, 0, 0);
      int         height = element->children[0]->Message(UIMessage::GET_HEIGHT, width, 0);
      int         cx     = (element->bounds.l + element->bounds.r) / 2;
      int         cy     = (element->bounds.t + element->bounds.b) / 2;
      UIRectangle bounds = UIRectangle(cx - (width + 1) / 2, cx + width / 2, cy - (height + 1) / 2, cy + height / 2);
      element->children[0]->Move(bounds, false);
      element->Repaint(NULL);
   } else if (message == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, element->children[0]->bounds, UI_DRAW_CONTROL_MODAL_POPUP, {}, 0,
                    element->window->scale);
   } else if (message == UIMessage::KEY_TYPED) {
      UIKeyTyped* typed = (UIKeyTyped*)dp;

      if (element->window->ctrl)
         return 0;
      if (element->window->shift)
         return 0;

      if (!ui->dialogCanExit) {
      } else if (!element->window->alt && typed->code == UIKeycode::ESCAPE) {
         ui->dialogResult = "__C";
         return 1;
      } else if (!element->window->alt && typed->code == UIKeycode::ENTER) {
         ui->dialogResult = "__D";
         return 1;
      }

      char c0 = 0, c1 = 0;

      if (typed->text.size() == 1 && typed->text[0] >= 'a' && typed->text[0] <= 'z') {
         c0 = typed->text[0], c1 = typed->text[0] - 'a' + 'A';
      } else {
         return 0;
      }

      UIElement* rowContainer = element->children[0];
      UIElement* target       = NULL;
      bool       duplicate    = false;

      for (auto row : rowContainer->children) {
         for (auto item : row->children) {

            if (item->messageClass == _UIButtonMessage) {
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
            target->Focus();
         } else {
            target->Message(UIMessage::CLICKED, 0, 0);
         }

         return 1;
      }
   }

   return 0;
}

void _UIDialogButtonInvoke(const char* label) {
   ui->dialogResult = label;
}

int _UIDialogDefaultButtonMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::PAINT && element->window->focused->messageClass != _UIButtonMessage) {
      element->flags |= UIButton::CHECKED;
      element->messageClass(element, message, di, dp);
      element->flags &= ~UIButton::CHECKED;
      return 1;
   }

   return 0;
}

int _UIDialogTextboxMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UITextbox* textbox = (UITextbox*)element;
   std::string_view text = textbox->text();

   if (message == UIMessage::VALUE_CHANGED) {
      auto sz = text.size();
      char** buffer             = (char**)element->cp;
      *buffer                   = (char*)realloc(*buffer, sz + 1); // update user pointer to hold the textbox text

      for (size_t i = 0; i < sz; i++)
         (*buffer)[i] = text[i];
      (*buffer)[sz] = 0;
   } else if (message == UIMessage::UPDATE && di == UIUpdate::FOCUSED && element->window->focused == element) {
      textbox->carets[1] = 0;
      textbox->carets[0] = text.size();
      element->Repaint(NULL);
   }

   return 0;
}

const char* UIDialogShow(UIWindow* window, uint32_t flags, const char* format, ...) {
   // Create the dialog wrapper and panel.

   UI_ASSERT(!window->dialog);
   window->dialog = UIElementCreate(sizeof(UIElement), window, 0, _UIDialogWrapperMessage, "DialogWrapper");
   UIPanel* panel = UIPanelCreate(window->dialog, UIPanel::MEDIUM_SPACING | UIPanel::COLOR_1);
   panel->border  = UIRectangle(ui_size::PANE_MEDIUM_BORDER * 2);
   window->children[0]->flags |= UIElement::DISABLED;

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
         row      = UIPanelCreate(panel, UIPanel::HORIZONTAL | UIElement::H_FILL);
         row->gap = ui_size::PANE_SMALL_GAP;
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
               button->messageUser = _UIDialogDefaultButtonMessage;
         } else if (format[i] == 's' /* label from string */) {
            const char* label = va_arg(arguments, const char*);
            UILabelCreate(row, 0, label);
         } else if (format[i] == 't' /* textbox */) {
            char**     buffer  = va_arg(arguments, char**);
            UITextbox* textbox = UITextboxCreate(row, UIElement::H_FILL);
            if (!focus)
               focus = textbox;
            if (*buffer)
               UITextboxReplace(textbox, *buffer, false);
            textbox->cp          = buffer; // when the textbox text is updated, `*buffer` will contain a `char*` to the string
            textbox->messageUser = _UIDialogTextboxMessage;
         } else if (format[i] == 'f' /* horizontal fill */) {
            UISpacerCreate(row, UIElement::H_FILL, 0, 0);
         } else if (format[i] == 'l' /* horizontal line */) {
            UISpacerCreate(row, UIElement::BORDER | UIElement::H_FILL, 0, 1);
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

   window->dialogOldFocus = window->focused;
   (focus ? focus : window->dialog)->Focus();

   // Run the modal message loop.

   int result;
   ui->dialogResult  = NULL;
   ui->dialogCanExit = buttonCount != 0;
   for (int i = 1; i <= 3; i++)
      window->SetPressed(NULL, i);
   window->Refresh();
   _UIUpdate();
   while (!ui->dialogResult && _UIMessageLoopSingle(&result))
      ;
   ui->quit = !ui->dialogResult;

   // Check for cancel/default action.

   if (buttonCount == 1 && defaultButton && !cancelButton) {
      cancelButton = defaultButton;
   }

   if (!ui->dialogResult) {
   } else if (ui->dialogResult[0] == '_' && ui->dialogResult[1] == '_' && ui->dialogResult[2] == 'C' &&
              ui->dialogResult[3] == 0 && cancelButton) {
      ui->dialogResult = (const char*)cancelButton->cp;
   } else if (ui->dialogResult[0] == '_' && ui->dialogResult[1] == '_' && ui->dialogResult[2] == 'D' &&
              ui->dialogResult[3] == 0 && defaultButton) {
      ui->dialogResult = (const char*)defaultButton->cp;
   }

   // Destroy the dialog.

   window->children[0]->flags &= ~UIElement::DISABLED;
   window->dialog->Destroy();
   window->dialog = NULL;
   window->Refresh();
   if (window->dialogOldFocus)
      window->dialogOldFocus->Focus();
   return ui->dialogResult ? ui->dialogResult : "";
}

// --------------------------------------------------
// Menus (common).
// --------------------------------------------------

bool _UIMenusClose() {
   UIWindow* window    = ui->windows;
   bool      anyClosed = false;

   while (window) {
      if (window->flags & UIWindow::MENU) {
         window->Destroy();
         anyClosed = true;
      }

      window = window->next;
   }

   return anyClosed;
}

int _UIMenuItemMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::CLICKED) {
      _UIMenusClose();
   }

   return 0;
}

int _UIMenuMessage(UIElement* element, UIMessage message, int di, void* dp) {
   UIMenu* menu = (UIMenu*)element;

   if (message == UIMessage::GET_WIDTH) {
      int width = 0;

      for (auto child : element->children) {
         if (~child->flags & UIElement::NON_CLIENT) {
            int w = child->Message(UIMessage::GET_WIDTH, 0, 0);
            if (w > width)
               width = w;
         }
      }

      return width + 4 + ui_size::SCROLL_BAR;
   } else if (message == UIMessage::GET_HEIGHT) {
      int height = 0;

      for (auto child : element->children) {
         if (~child->flags & UIElement::NON_CLIENT) {
            height += child->Message(UIMessage::GET_HEIGHT, 0, 0);
         }
      }

      return height + 4;
   } else if (message == UIMessage::PAINT) {
      UIDrawControl((UIPainter*)dp, element->bounds, UI_DRAW_CONTROL_MENU, {}, 0, element->window->scale);
   } else if (message == UIMessage::LAYOUT) {
      int position      = element->bounds.t + 2 - menu->vScroll->position;
      int totalHeight   = 0;
      int scrollBarSize = (menu->flags & UIMenu::NO_SCROLL) ? 0 : ui_size::SCROLL_BAR;

      for (auto child : element->children) {
         if (~child->flags & UIElement::NON_CLIENT) {
            int height = child->Message(UIMessage::GET_HEIGHT, 0, 0);
            child->Move(
               UIRectangle(element->bounds.l + 2, element->bounds.r - scrollBarSize - 2, position, position + height),
               false);
            position += height;
            totalHeight += height;
         }
      }

      UIRectangle scrollBarBounds = element->bounds;
      scrollBarBounds.l           = scrollBarBounds.r - element->scale(scrollBarSize);
      menu->vScroll->maximum      = totalHeight;
      menu->vScroll->page         = element->bounds.height();
      menu->vScroll->Move(scrollBarBounds, true);
   } else if (message == UIMessage::KEY_TYPED) {
      UIKeyTyped* m = (UIKeyTyped*)dp;

      if (m->code == UIKeycode::ESCAPE) {
         _UIMenusClose();
         return 1;
      }
   } else if (message == UIMessage::MOUSE_WHEEL) {
      return menu->vScroll->Message(message, di, dp);
   } else if (message == UIMessage::SCROLLED) {
      element->Refresh();
   }

   return 0;
}

void UIMenuAddItem(UIMenu* menu, uint32_t flags, std::string_view label, std::function<void ()> invoke) {
   UIButton* button      = UIButtonCreate(menu, flags | UIButton::MENU_ITEM, label);
   button->invoke        = std::move(invoke);
   button->messageUser = _UIMenuItemMessage;
}

void _UIMenuPrepare(UIMenu* menu, int* width, int* height) {
   *width  = menu->Message(UIMessage::GET_WIDTH, 0, 0);
   *height = menu->Message(UIMessage::GET_HEIGHT, 0, 0);

   if (menu->flags & UIMenu::PLACE_ABOVE) {
      menu->pointY -= *height;
   }
}

UIMenu::UIMenu(UIElement* parent, uint32_t flags)
   : UIElement(UIWindowCreate(parent->window, UIWindow::MENU, 0, 0, 0), flags, _UIMenuMessage, "Menu")
   , vScroll(UIScrollBarCreate(this, UIElement::NON_CLIENT))
   , parentWindow(parent->window) {
   if (parent->parent) {
      UIRectangle screenBounds = parent->ScreenBounds();
      pointX                   = screenBounds.l;
      pointY                   = (flags & UIMenu::PLACE_ABOVE) ? (screenBounds.t + 1) : (screenBounds.b - 1);
   } else {
      int x = 0, y = 0;
      parent->window->GetScreenPosition(&x, &y);

      pointX = parent->window->cursor.x + x;
      pointY = parent->window->cursor.y + y;
   }
}

UIMenu* UIMenuCreate(UIElement* parent, uint32_t flags) {
   return new UIMenu(parent, flags);
}

// --------------------------------------------------
// Miscellaneous core functions.
// --------------------------------------------------

UIRectangle UIElement::ScreenBounds() {
   int x = 0, y = 0;
   window->GetScreenPosition(&x, &y);
   return bounds + UIRectangle(x, y);
}

void UIWindowRegisterShortcut(UIWindow* window, UIShortcut shortcut) {
   window->shortcuts.push_back(std::move(shortcut));
}

void _UIUpdate() {
   UIWindow*  window = ui->windows;
   UIWindow** link   = &ui->windows;

   while (window) {
      UIWindow* next = window->next;

      window->Message(UIMessage::WINDOW_UPDATE_START, 0, 0);
      window->Message(UIMessage::WINDOW_UPDATE_BEFORE_DESTROY, 0, 0);

      if (_UIDestroy(window)) {
         *link = next;
      } else {
         link = &window->next;

         window->Message(UIMessage::WINDOW_UPDATE_BEFORE_LAYOUT, 0, 0);
         window->Move(window->bounds, false);
         window->Message(UIMessage::WINDOW_UPDATE_BEFORE_PAINT, 0, 0);

         if (window->updateRegion.valid()) {
            UIPainter painter = {
               .clip   = intersection(ui_rect_2s(window->width, window->height), window->updateRegion),
               .bits   = window->bits.data(),
               .width  = window->width,
               .height = window->height};
            window->Paint(&painter);
            window->EndPaint(&painter);
            window->updateRegion = UIRectangle(0);

#ifdef UI_DEBUG
            window->lastFullFillCount =
               (float)painter.fillCount / (window->updateRegion.width() * window->updateRegion.height());
#endif
         }

         window->Message(UIMessage::WINDOW_UPDATE_END, 0, 0);
      }

      window = next;
   }
}

// --------------------------------------------------
// Input event handling.
// --------------------------------------------------

void UIWindow::SetPressed(UIElement* element, int button) {
   UIElement* previous   = pressed;
   pressed       = element;
   pressedButton = button;
   if (previous)
      previous->Message(UIMessage::UPDATE, UIUpdate::PRESSED, 0);
   if (element)
      element->Message(UIMessage::UPDATE, UIUpdate::PRESSED, 0);

   UIElement* ancestor = element;
   UIElement* child    = NULL;

   while (ancestor) {
      ancestor->Message(UIMessage::PRESSED_DESCENDENT, 0, child);
      child    = ancestor;
      ancestor = ancestor->parent;
   }
}

UIElement* UIElement::FindByPoint(int x, int y) {
   for (uint32_t i = children.size(); i > 0; i--) {
      UIElement* child = children[i - 1];

      if ((~child->flags & UIElement::HIDE) && child->clip.contains(x, y)) {
         return child->FindByPoint(x, y);
      }
   }

   return this;
}

bool UIMenusOpen() {
   UIWindow* window = ui->windows;

   while (window) {
      if (window->flags & UIWindow::MENU) {
         return true;
      }

      window = window->next;
   }

   return false;
}

UIElement* UIElement::NextOrPreviousSibling(bool previous) {
   if (!parent) {
      return NULL;
   }

   for (uint32_t i = 0; i < parent->children.size(); i++) {
      if (parent->children[i] == this) {
         if (previous) {
            return i > 0 ? parent->children[i - 1] : NULL;
         } else {
            return i < parent->children.size() - 1 ? parent->children[i + 1] : NULL;
         }
      }
   }

   UI_ASSERT(false);
   return NULL;
}

bool UIWindow::InputEvent(UIMessage message, int di, void* dp) {
   bool handled = true;

   if (pressed) {
      if (message == UIMessage::MOUSE_MOVE) {
         pressed->Message(UIMessage::MOUSE_DRAG, di, dp);
      } else if (message == UIMessage::LEFT_UP && pressedButton == 1) {
         if (hovered == pressed) {
            pressed->Message(UIMessage::CLICKED, di, dp);
            if (ui->quit || ui->dialogResult)
               goto end;
         }

         if (pressed) {
            pressed->Message(UIMessage::LEFT_UP, di, dp);
            if (ui->quit || ui->dialogResult)
               goto end;
            SetPressed(NULL, 1);
         }
      } else if (message == UIMessage::MIDDLE_UP && pressedButton == 2) {
         pressed->Message(UIMessage::MIDDLE_UP, di, dp);
         if (ui->quit || ui->dialogResult)
            goto end;
         SetPressed(NULL, 2);
      } else if (message == UIMessage::RIGHT_UP && pressedButton == 3) {
         pressed->Message(UIMessage::RIGHT_UP, di, dp);
         if (ui->quit || ui->dialogResult)
            goto end;
         SetPressed(NULL, 3);
      }
   }

   if (pressed) {
      bool inside = pressed->clip.contains(cursor);

      if (inside && hovered == window) {
         hovered = pressed;
         pressed->Message(UIMessage::UPDATE, UIUpdate::HOVERED, 0);
      } else if (!inside && hovered == pressed) {
         hovered = window;
         pressed->Message(UIMessage::UPDATE, UIUpdate::HOVERED, 0);
      }

      if (ui->quit || ui->dialogResult)
         goto end;
   }

   if (!pressed) {
      UIElement* loc = window->FindByPoint(cursor.x, cursor.y);

      if (message == UIMessage::MOUSE_MOVE) {
         loc->Message(UIMessage::MOUSE_MOVE, di, dp);

         int cursor = hovered->Message(UIMessage::GET_CURSOR, di, dp);

         if (cursor != cursorStyle) {
            cursorStyle = cursor;
            SetCursor(cursor);
         }
      } else if (message == UIMessage::LEFT_DOWN) {
         if ((flags & UIWindow::MENU) || !_UIMenusClose()) {
            SetPressed(loc, 1);
            loc->Message(UIMessage::LEFT_DOWN, di, dp);
         }
      } else if (message == UIMessage::MIDDLE_DOWN) {
         if ((flags & UIWindow::MENU) || !_UIMenusClose()) {
            SetPressed(loc, 2);
            loc->Message(UIMessage::MIDDLE_DOWN, di, dp);
         }
      } else if (message == UIMessage::RIGHT_DOWN) {
         if ((flags & UIWindow::MENU) || !_UIMenusClose()) {
            SetPressed(loc, 3);
            loc->Message(UIMessage::RIGHT_DOWN, di, dp);
         }
      } else if (message == UIMessage::MOUSE_WHEEL) {
         UIElement* element = loc;

         while (element) {
            if (element->Message(UIMessage::MOUSE_WHEEL, di, dp)) {
               break;
            }

            element = element->parent;
         }
      } else if (message == UIMessage::KEY_TYPED || message == UIMessage::KEY_RELEASED) {
         handled = false;

         if (focused) {
            UIElement* element = focused;

            while (element) {
               if (element->Message(message, di, dp)) {
                  handled = true;
                  break;
               }

               element = element->parent;
            }
         } else {
            if (Message(message, di, dp)) {
               handled = true;
            }
         }

         if (!handled && !UIMenusOpen() && message == UIMessage::KEY_TYPED) {
            UIKeyTyped* m = (UIKeyTyped*)dp;

            if (m->code == UIKeycode::TAB && !ctrl && !alt) {
               UIElement* start   = focused ? focused : window;
               UIElement* element = start;

               do {
                  if (!element->children.empty() && !(element->flags & (UIElement::HIDE | UIElement::DISABLED))) {
                     element = shift ? element->children.back() : element->children[0];
                     continue;
                  }

                  while (element) {
                     UIElement* sibling = element->NextOrPreviousSibling(shift);
                     if (sibling) {
                        element = sibling;
                        break;
                     }
                     element = element->parent;
                  }

                  if (!element) {
                     element = window;
                  }
               } while (element != start && ((~element->flags & UIElement::TAB_STOP) ||
                                             (element->flags & (UIElement::HIDE | UIElement::DISABLED))));

               if (~element->flags & UIElement::WINDOW) {
                  element->Focus();
               }

               handled = true;
            } else if (!dialog) {
               for (const auto& shortcut : views::reverse(shortcuts)) {
                  if (shortcut.code == m->code && shortcut.ctrl == ctrl && shortcut.shift == shift &&
                      shortcut.alt == alt) {
                     shortcut.invoke();
                     handled = true;
                     break;
                  }
               }
            } else if (dialog) {
               dialog->Message(message, di, dp);
            }
         }
      }

      if (ui->quit || ui->dialogResult)
         goto end;

      if (loc != hovered) {
         UIElement* previous = hovered;
         hovered     = loc;
         previous->Message(UIMessage::UPDATE, UIUpdate::HOVERED, 0);
         hovered->Message(UIMessage::UPDATE, UIUpdate::HOVERED, 0);
      }
   }

end:
   _UIUpdate();
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
   if (!cPath)
      return nullptr;

   UIFontSpec spec { cPath, size };

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
   UIFont *f = font.get();
   ui->font_map.emplace(std::move(spec), std::move(font));
   return f;
}

UIFont* UIFontActivate(UIFont* font) {
   UIFont* previous = ui->activeFont;
   ui->activeFont    = font;
   return previous;
}


UIFont::~UIFont() {
#ifdef UI_FREETYPE
   FT_Done_Face(font);
   for (size_t i=0; i<max_glyphs; ++i)
      FT_Bitmap_Done(ui->ft, &glyphs[i]);
#endif
}


// --------------------------------------------------
// Debugging.
// --------------------------------------------------

#ifdef UI_DEBUG

UIElement* _UIInspectorFindNthElement(UIElement* element, int* index, int* depth) {
   if (*index == 0) {
      return element;
   }

   *index = *index - 1;

   for (auto child : element->children) {
      if (!(child->flags & (UIElement::DESTROY | UIElement::HIDE))) {
         UIElement* result = _UIInspectorFindNthElement(child, index, depth);

         if (result) {
            if (depth) {
               *depth = *depth + 1;
            }

            return result;
         }
      }
   }

   return NULL;
}

int _UIInspectorTableMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (!ui->inspectorTarget) {
      return 0;
   }

   if (message == UIMessage::TABLE_GET_ITEM) {
      UITableGetItem* m       = (UITableGetItem*)dp;
      int             index   = m->index;
      int             depth   = 0;
      UIElement*      element = _UIInspectorFindNthElement(&ui->inspectorTarget->e, &index, &depth);
      if (!element)
         return 0;

      if (m->column == 0) {
         return m->format_to("{:.{}}{}", "                ", depth * 2, element->cClassName);
      } else if (m->column == 1) {
         const auto& b = element->bounds;
         return m->format_to("{}:{}, {}:{}", b.l, b.r, b.t, b.b);
      } else if (m->column == 2) {
         return m->format_to("{}{:c}", element->id, element->window->focused == element ? '*' : ' ');
      }
   } else if (message == UIMessage::MOUSE_MOVE) {
      int        index   = UITableHitTest(ui->inspectorTable, element->window->cursor.x, element->window->cursor.y);
      UIElement* element = NULL;
      if (index >= 0)
         element = _UIInspectorFindNthElement(&ui->inspectorTarget->e, &index, NULL);
      UIWindow* window     = ui->inspectorTarget;
      UIPainter painter    = {0};
      window->updateRegion = window->bounds;
      painter.bits         = window->bits;
      painter.width        = window->width;
      painter.height       = window->height;
      painter.clip         = ui_rect_2s(window->width, window->height);

      for (int i = 0; i < window->width * window->height; i++) {
         window->bits[i] = 0xFF00FF;
      }

      window->Paint(&painter);
      painter.clip = ui_rect_2s(window->width, window->height);

      if (element) {
         UIDrawInvert(&painter, element->bounds);
         UIDrawInvert(&painter, element->bounds + UI_RECT_1I(4));
      }

      window->EndPaint(&painter);
   }

   return 0;
}

void _UIInspectorCreate() {
   ui->inspector                   = UIWindowCreate(0, UIWindow::INSPECTOR, "Inspector", 0, 0);
   UISplitPane* splitPane         = UISplitPaneCreate(ui->inspector, 0, 0.5f);
   ui->inspectorTable              = UITableCreate(splitPane, 0, "Class\tBounds\tID");
   ui->inspectorTable->messageUser = _UIInspectorTableMessage;
   ui->inspectorLog                = UICodeCreate(splitPane, 0);
}

int _UIInspectorCountElements(UIElement* element) {
   int count = 1;

   for (auto child : element->children) {
      if (!(child->flags & (UIElement::DESTROY | UIElement::HIDE))) {
         count += _UIInspectorCountElements(child);
      }
   }

   return count;
}

void _UIInspectorRefresh() {
   if (!ui->inspectorTarget || !ui->inspector || !ui->inspectorTable)
      return;
   ui->inspectorTable->itemCount = _UIInspectorCountElements(&ui->inspectorTarget->e);
   UITableResizeColumns(ui->inspectorTable);
   &ui->inspectorTable->e->Refresh();
}

void _UIInspectorSetFocusedWindow(UIWindow* window) {
   if (!ui->inspector || !ui->inspectorTable)
      return;

   if (window->flags & UIWindow::INSPECTOR) {
      return;
   }

   if (ui->inspectorTarget != window) {
      ui->inspectorTarget = window;
      _UIInspectorRefresh();
   }
}

#else

void _UIInspectorCreate() {}
void _UIInspectorSetFocusedWindow(UIWindow* window) {}
void _UIInspectorRefresh() {}

#endif // UI_DEBUG

// --------------------------------------------------
// Automation for tests.
// --------------------------------------------------

#ifdef UI_AUTOMATION_TESTS

int UIAutomationRunTests();

void UIAutomationProcessMessage() {
   int result;
   _UIMessageLoopSingle(&result);
}

void UIAutomationKeyboardTypeSingle(intptr_t code, bool ctrl, bool shift, bool alt) {
   UIWindow*  window = ui->windows; // TODO Get the focused window.
   UIKeyTyped m      = {0};
   m.code            = code;
   window->ctrl      = ctrl;
   window->alt       = alt;
   window->shift     = shift;
   window->InputEvent(UIMessage::KEY_TYPED, 0, &m);
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
      window->InputEvent(UIMessage::KEY_TYPED, 0, &m);
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
   if (row < 0 || row >= table->itemCount)
      return false;
   if (column < 0 || column >= table->columnCount)
      return false;
   UITableGetItem m(bytes + 1);
   m.column              = column;
   m.index               = row;
   int length            = &table->e->Message(UIMessage::TABLE_GET_ITEM, 0, &m);
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

void _UIWindowDestroyCommon(UIWindow* window) {
}

void _UIInitialiseCommon() {
   ui->theme = uiThemeClassic;

#ifdef UI_FREETYPE
   FT_Init_FreeType(&ui->ft);
   UIFontActivate(UIFontCreate(_UI_TO_STRING_2(UI_FONT_PATH), 11));
#else
   UIFontActivate(UIFontCreate(0, 0));
#endif
}



void _UIWindowAdd(UIWindow* window) {
   window->scale    = 1.0f;
   window->window   = window;
   window->hovered  = window;
   window->next     = ui->windows;
   ui->windows       = window;
}

int _UIWindowMessageCommon(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::LAYOUT && !element->children.empty()) {
      element->children[0]->Move(element->bounds, false);
      if (element->window->dialog)
         element->window->dialog->Move(element->bounds, false);
      element->Repaint(NULL);
   } else if (message == UIMessage::GET_CHILD_STABILITY) {
      return 3; // Both width and height of the child element are ignored.
   }

   return 0;
}

int UIMessageLoop() {
   _UIInspectorCreate();
   _UIUpdate();
#ifdef UI_AUTOMATION_TESTS
   return UIAutomationRunTests();
#else
   int result = 0;
   while (!ui->quit && _UIMessageLoopSingle(&result))
      ui->dialogResult = NULL;
   return result;
#endif
}

UIWindow::UIWindow(UIElement* parent, uint32_t flags, MsgFn message, const char* cClassName) :
   UIElement(parent, flags, message, cClassName),
   dialog(nullptr),
   scale(0),
   width(0),
   height(0),
   next(nullptr),
   hovered(nullptr),
   pressed(nullptr),
   focused(nullptr),
   dialogOldFocus(nullptr),
   pressedButton(0),
   cursorStyle(0),
   textboxModifiedFlag(false),
   ctrl(false),
   shift(false),
   alt(false),
   updateRegion(0)
{
}

UIWindow::~UIWindow() {
}

// --------------------------------------------------
// Platform layers.
// --------------------------------------------------

#ifdef UI_LINUX

int _UIWindowMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::DEALLOCATE) {
      UIWindow* window = (UIWindow*)element;
      _UIWindowDestroyCommon(window);
      window->image->data = NULL;
      XDestroyImage(window->image);
      XDestroyIC(window->xic);
      XDestroyWindow(ui->display, ((UIWindow*)element)->xwindow);
      return 0;
   }

   return _UIWindowMessageCommon(element, message, di, dp);
}

UIWindow* UIWindowCreate(UIWindow* owner, uint32_t flags, const char* cTitle, int _width, int _height) {
   _UIMenusClose();

   UIWindow* window = new UIWindow(NULL, flags | UIElement::WINDOW, _UIWindowMessage, "Window");
   _UIWindowAdd(window);
   if (owner)
      window->scale = owner->scale;

   int width  = (flags & UIWindow::MENU) ? 1 : _width ? _width : 800;
   int height = (flags & UIWindow::MENU) ? 1 : _height ? _height : 600;

   XSetWindowAttributes attributes = {};
   attributes.override_redirect    = flags & UIWindow::MENU;

   window->xwindow = XCreateWindow(ui->display, DefaultRootWindow(ui->display), 0, 0, width, height, 0, 0, InputOutput,
                                   CopyFromParent, CWOverrideRedirect, &attributes);
   if (cTitle)
      XStoreName(ui->display, window->xwindow, cTitle);
   XSelectInput(ui->display, window->xwindow,
                SubstructureNotifyMask | ExposureMask | PointerMotionMask | ButtonPressMask | ButtonReleaseMask |
                   KeyPressMask | KeyReleaseMask | StructureNotifyMask | EnterWindowMask | LeaveWindowMask |
                   ButtonMotionMask | KeymapStateMask | FocusChangeMask | PropertyChangeMask);

   if (flags & UIWindow::MAXIMIZE) {
      Atom atoms[2] = {XInternAtom(ui->display, "_NET_WM_STATE_MAXIMIZED_HORZ", 0),
                       XInternAtom(ui->display, "_NET_WM_STATE_MAXIMIZED_VERT", 0)};
      XChangeProperty(ui->display, window->xwindow, XInternAtom(ui->display, "_NET_WM_STATE", 0), XA_ATOM, 32,
                      PropModeReplace, (unsigned char*)atoms, 2);
   }

   if (~flags & UIWindow::MENU) {
      XMapRaised(ui->display, window->xwindow);
   }

   if (flags & UIWindow::CENTER_IN_OWNER) {
      int x = 0, y = 0;
      owner->GetScreenPosition(&x, &y);
      XMoveResizeWindow(ui->display, window->xwindow, x + owner->width / 2 - width / 2,
                        y + owner->height / 2 - height / 2, width, height);
   }

   XSetWMProtocols(ui->display, window->xwindow, &ui->windowClosedID, 1);
   window->image = XCreateImage(ui->display, ui->visual, 24, ZPixmap, 0, NULL, 10, 10, 32, 0);

   window->xic = XCreateIC(ui->xim, XNInputStyle, XIMPreeditNothing | XIMStatusNothing, XNClientWindow, window->xwindow,
                           XNFocusWindow, window->xwindow, nullptr);

   int dndVersion = 4;
   XChangeProperty(ui->display, window->xwindow, ui->dndAwareID, XA_ATOM, 32 /* bits */, PropModeReplace,
                   (uint8_t*)&dndVersion, 1);

   return window;
}

Display* _UIX11GetDisplay() {
   return ui->display;
}

UIWindow* _UIFindWindow(Window window) {
   UIWindow* w = ui->windows;

   while (w) {
      if (w->xwindow == window) {
         return w;
      }

      w = w->next;
   }

   return NULL;
}

void _UIClipboardWriteText(UIWindow* window, std::string text, sel_target_t t) {
   ui->pasteText = std::move(text);
   Atom atom = (t == sel_target_t::clipboard) ? ui->clipboardID :  ui->primaryID;
   XSetSelectionOwner(ui->display, atom, window->xwindow, 0);
}

std::string _UIClipboardReadText(UIWindow* window, sel_target_t t) {
   Atom atom = (t == sel_target_t::clipboard) ? ui->clipboardID :  ui->primaryID;

   Window clipboardOwner = XGetSelectionOwner(ui->display, atom);

   if (clipboardOwner == None) {
      return {};
   }

   if (_UIFindWindow(clipboardOwner)) {
      return ui->pasteText;
   }

   XConvertSelection(ui->display, atom, XA_STRING, ui->xSelectionDataID, window->xwindow, CurrentTime);
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
               XGetWindowProperty(ui->display, ui->copyEvent.xproperty.window, ui->copyEvent.xproperty.atom, 0L, ~0L, True,
                                  AnyPropertyType, &target, &format, &chunkSize, &itemAmount, (unsigned char**)&data);

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
   _UIInitialiseCommon();

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
   return unique_ptr<UI>{ui};
}

void UIWindow::SetCursor(int cursor) {
   XDefineCursor(ui->display, xwindow, ui->cursors[cursor]);
}

void _UIX11ResetCursor(UIWindow* window) {
   XDefineCursor(ui->display, window->xwindow, ui->cursors[(uint32_t)UICursor::arrow]);
}

void UIWindow::EndPaint(UIPainter* painter) {
   (void)painter;
   const auto& ur = window->updateRegion;
   XPutImage(ui->display, window->xwindow, DefaultGC(ui->display, 0), window->image, ur.l, ur.t, ur.l, ur.t,
             UI_RECT_SIZE(window->updateRegion));
}

void UIWindow::GetScreenPosition(int* _x, int* _y) {
   Window child;
   XTranslateCoordinates(ui->display, window->xwindow, DefaultRootWindow(ui->display), 0, 0, _x, _y, &child);
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
      XTranslateCoordinates(ui->display, parentWindow->xwindow, DefaultRootWindow(ui->display), 0, 0, &wx, &wy, &child);
      if (menu->pointX + width > wx + (int)parentWindow->width)
         menu->pointX = wx + parentWindow->width - width;
      if (menu->pointY + height > wy + (int)parentWindow->height)
         menu->pointY = wy + parentWindow->height - height;
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

   XChangeProperty(ui->display, menu->window->xwindow, properties[0], XA_ATOM, 32, PropModeReplace,
                   (uint8_t*)properties, 2);
   XSetTransientForHint(ui->display, menu->window->xwindow, DefaultRootWindow(ui->display));

   struct Hints {
      int flags;
      int functions;
      int decorations;
      int inputMode;
      int status;
   };

   struct Hints hints = {0};
   hints.flags        = 2;
   XChangeProperty(ui->display, menu->window->xwindow, properties[2], properties[2], 32, PropModeReplace,
                   (uint8_t*)&hints, 5);

   XMapWindow(ui->display, menu->window->xwindow);
   XMoveResizeWindow(ui->display, menu->window->xwindow, menu->pointX, menu->pointY, width, height);
}

void UIWindowPack(UIWindow* window, int _width) {
   int width  = _width ? _width : window->children[0]->Message(UIMessage::GET_WIDTH, 0, 0);
   int height = window->children[0]->Message(UIMessage::GET_HEIGHT, width, 0);
   XResizeWindow(ui->display, window->xwindow, width, height);
}

// return true if we should exit, normally return false
// ----------------------------------------------------
bool _UIProcessEvent(XEvent* event) {
   if (event->type == ClientMessage && (Atom)event->xclient.data.l[0] == ui->windowClosedID) {
      UIWindow* window = _UIFindWindow(event->xclient.window);
      if (!window)
         return false;
      bool exit = !window->Message(UIMessage::WINDOW_CLOSE, 0, 0);
      if (exit)
         return true;
      _UIUpdate();
      return false;
   } else if (event->type == Expose) {
      UIWindow* window = _UIFindWindow(event->xexpose.window);
      if (!window)
         return false;
      XPutImage(ui->display, window->xwindow, DefaultGC(ui->display, 0), window->image, 0, 0, 0, 0, window->width,
                window->height);
   } else if (event->type == ConfigureNotify) {
      UIWindow* window = _UIFindWindow(event->xconfigure.window);
      if (!window)
         return false;

      if ((int)window->width != event->xconfigure.width || (int)window->height != event->xconfigure.height) {
         window->width                 = event->xconfigure.width;
         window->height                = event->xconfigure.height;
         window->bits.resize(window->width * window->height);

         window->image->width          = window->width;
         window->image->height         = window->height;
         window->image->bytes_per_line = window->width * 4;
         window->image->data           = (char*)window->bits.data();
         window->bounds                = ui_rect_2s(window->width, window->height);
         window->clip                  = ui_rect_2s(window->width, window->height);
   #ifdef UI_DEBUG
         for (int i = 0; i < window->width * window->height; i++)
            window->bits[i] = 0xFF00FF;
   #endif
         window->Relayout();
         _UIUpdate();
      }
   } else if (event->type == MotionNotify) {
      UIWindow* window = _UIFindWindow(event->xmotion.window);
      if (!window)
         return false;
      window->cursor.x = event->xmotion.x;
      window->cursor.y = event->xmotion.y;
      window->InputEvent(UIMessage::MOUSE_MOVE, 0, 0);
   } else if (event->type == LeaveNotify) {
      UIWindow* window = _UIFindWindow(event->xcrossing.window);
      if (!window)
         return false;

      if (!window->pressed) {
         window->cursor.x = -1;
         window->cursor.y = -1;
      }

      window->InputEvent(UIMessage::MOUSE_MOVE, 0, 0);
   } else if (event->type == ButtonPress || event->type == ButtonRelease) {
      UIWindow* window = _UIFindWindow(event->xbutton.window);
      if (!window)
         return false;
      window->cursor.x = event->xbutton.x;
      window->cursor.y = event->xbutton.y;

      if (event->xbutton.button >= 1 && event->xbutton.button <= 3) {
         window->InputEvent((UIMessage)((uint32_t)(event->type == ButtonPress ? UIMessage::LEFT_DOWN : UIMessage::LEFT_UP) +
                                        event->xbutton.button * 2 - 2),
                            0, 0);
      } else if (event->xbutton.button == 4) {
         window->InputEvent(UIMessage::MOUSE_WHEEL, -72, 0);
      } else if (event->xbutton.button == 5) {
         window->InputEvent(UIMessage::MOUSE_WHEEL, 72, 0);
      }

      _UIInspectorSetFocusedWindow(window);
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
         window->Message((UIMessage)event->xkey.state, 0, (void*)p);
         _UIUpdate();
      } else {
         char   text[32];
         KeySym symbol = NoSymbol;
         Status status;
         // std_print("{}, {}\n", symbol, text);
         UIKeyTyped m;
         auto       sz = Xutf8LookupString(window->xic, &event->xkey, text, sizeof(text) - 1, &symbol, &status);
         m.text        = {text, static_cast<size_t>(sz)};
         m.code        = (UIKeycode)XLookupKeysym(&event->xkey, 0);

         if (symbol == XK_Control_L || symbol == XK_Control_R) {
            window->ctrl     = true;
            window->ctrlCode = event->xkey.keycode;
            window->InputEvent(UIMessage::MOUSE_MOVE, 0, 0);
         } else if (symbol == XK_Shift_L || symbol == XK_Shift_R) {
            window->shift     = true;
            window->shiftCode = event->xkey.keycode;
            window->InputEvent(UIMessage::MOUSE_MOVE, 0, 0);
         } else if (symbol == XK_Alt_L || symbol == XK_Alt_R) {
            window->alt     = true;
            window->altCode = event->xkey.keycode;
            window->InputEvent(UIMessage::MOUSE_MOVE, 0, 0);
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

         window->InputEvent(UIMessage::KEY_TYPED, 0, &m);
      }
   } else if (event->type == KeyRelease) {
      UIWindow* window = _UIFindWindow(event->xkey.window);
      if (!window)
         return false;

      if (event->xkey.keycode == window->ctrlCode) {
         window->ctrl = false;
         window->InputEvent(UIMessage::MOUSE_MOVE, 0, 0);
      } else if (event->xkey.keycode == window->shiftCode) {
         window->shift = false;
         window->InputEvent(UIMessage::MOUSE_MOVE, 0, 0);
      } else if (event->xkey.keycode == window->altCode) {
         window->alt = false;
         window->InputEvent(UIMessage::MOUSE_MOVE, 0, 0);
      } else {
         char       text[32];
         KeySym     symbol = NoSymbol;
         Status     status;
         UIKeyTyped m;
         auto sz = Xutf8LookupString(window->xic, &event->xkey, text, sizeof(text) - 1, &symbol, &status);
         m.text       = { text, static_cast<size_t>(sz) };
         m.code       = (UIKeycode)XLookupKeysym(&event->xkey, 0);
         window->InputEvent(UIMessage::KEY_RELEASED, 0, &m);
      }
   } else if (event->type == FocusIn) {
      UIWindow* window = _UIFindWindow(event->xfocus.window);
      if (!window)
         return false;
      window->ctrl = window->shift = window->alt = false;
      window->Message(UIMessage::WINDOW_ACTIVATE, 0, 0);
   } else if (event->type == FocusOut || event->type == ResizeRequest) {
      _UIMenusClose();
      _UIUpdate();
   } else if (event->type == ClientMessage && event->xclient.message_type == ui->dndEnterID) {
      UIWindow* window = _UIFindWindow(event->xclient.window);
      if (!window)
         return false;
      window->dragSource = (Window)event->xclient.data.l[0];
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
      m.data.l[0]           = window->xwindow;
      m.data.l[1]           = true;
      m.data.l[4]           = ui->dndActionCopyID;
      XSendEvent(ui->display, m.window, False, NoEventMask, (XEvent*)&m);
      XFlush(ui->display);
   } else if (event->type == ClientMessage && event->xclient.message_type == ui->dndDropID) {
      UIWindow* window = _UIFindWindow(event->xclient.window);
      if (!window)
         return false;

      // TODO Dropping text.

      if (!XConvertSelection(ui->display, ui->dndSelectionID, ui->uriListID, ui->primaryID, window->xwindow,
                             event->xclient.data.l[2])) {
         XClientMessageEvent m = {0};
         m.type                = ClientMessage;
         m.display             = ui->display;
         m.window              = window->dragSource;
         m.message_type        = ui->dndFinishedID;
         m.format              = 32;
         m.data.l[0]           = window->xwindow;
         m.data.l[1]           = 0;
         m.data.l[2]           = ui->dndActionCopyID;
         XSendEvent(ui->display, m.window, False, NoEventMask, (XEvent*)&m);
         XFlush(ui->display);
      }
   } else if (event->type == SelectionNotify) {
      UIWindow* window = _UIFindWindow(event->xselection.requestor);
      if (!window)
         return false;
      if (!window->dragSource)
         return false;

      Atom          type   = None;
      int           format = 0;
      unsigned long count = 0, bytesLeft = 0;
      uint8_t*      data = NULL;
      XGetWindowProperty(ui->display, window->xwindow, ui->primaryID, 0, 65536, False, AnyPropertyType, &type, &format,
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

            window->Message(UIMessage::WINDOW_DROP_FILES, fileCount, files);

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
      m.window              = window->dragSource;
      m.message_type        = ui->dndFinishedID;
      m.format              = 32;
      m.data.l[0]           = window->xwindow;
      m.data.l[1]           = true;
      m.data.l[2]           = ui->dndActionCopyID;
      XSendEvent(ui->display, m.window, False, NoEventMask, (XEvent*)&m);
      XFlush(ui->display);

      window->dragSource = 0; // Drag complete.
      _UIUpdate();
   } else if (event->type == SelectionRequest) {
      UIWindow* window = _UIFindWindow(event->xclient.window);
      if (!window)
         return false;

      if ((XGetSelectionOwner(ui->display, ui->clipboardID) == window->xwindow) &&
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
                               PropModeReplace, (const unsigned char *)ui->pasteText.c_str(), ui->pasteText.size());
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
bool _UIMessageLoopSingle(int* result) {
   XEvent events[64];

   if (!ui->animating.empty()) {
      if (XPending(ui->display)) {
         XNextEvent(ui->display, events + 0);
      } else {
         _UIProcessAnimations();
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

void UIWindowPostMessage(UIWindow* window, UIMessage message, void* _dp) {
   // HACK! Xlib doesn't seem to have a nice way to do this,
   // so send a specially crafted key press event instead.
   // TODO Maybe ClientMessage is what this should use?
   uintptr_t dp    = (uintptr_t)_dp;
   XKeyEvent event = {0};
   event.display   = ui->display;
   event.window    = window->xwindow;
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
   event.state       = (unsigned int)message;
   event.type        = KeyPress;
   XSendEvent(ui->display, window->xwindow, True, KeyPressMask, (XEvent*)&event);
   XFlush(ui->display);
}

#endif // UI_LINUX

#ifdef UI_WINDOWS

int _UIWindowMessage(UIElement* element, UIMessage message, int di, void* dp) {
   if (message == UIMessage::DEALLOCATE) {
      UIWindow* window = (UIWindow*)element;
      _UIWindowDestroyCommon(window);
      SetWindowLongPtr(window->hwnd, GWLP_USERDATA, 0);
      DestroyWindow(window->hwnd);
      return 0;
   }

   return _UIWindowMessageCommon(element, message, di, dp);
}

LRESULT CALLBACK _UIWindowProcedure(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam) {
   UIWindow* window = (UIWindow*)GetWindowLongPtr(hwnd, GWLP_USERDATA);

   if (!window || ui->assertionFailure) {
      return DefWindowProc(hwnd, message, wParam, lParam);
   }

   if (message == WM_CLOSE) {
      if (window->Message(UIMessage::WINDOW_CLOSE, 0, 0)) {
         _UIUpdate();
         return 0;
      } else {
         PostQuitMessage(0);
      }
   } else if (message == WM_SIZE) {
      RECT client;
      GetClientRect(hwnd, &client);
      window->width  = client.right;
      window->height = client.bottom;
      window->bits.resize(window->width * window->height);
      window->bounds = ui_rect_2s(window->width, window->height);
      window->clip   = ui_rect_2s(window->width, window->height);
      window->Relayout();
      _UIUpdate();
   } else if (message == WM_MOUSEMOVE) {
      if (!window->trackingLeave) {
         window->trackingLeave = true;
         TRACKMOUSEEVENT leave = {0};
         leave.cbSize          = sizeof(TRACKMOUSEEVENT);
         leave.dwFlags         = TME_LEAVE;
         leave.hwndTrack       = hwnd;
         TrackMouseEvent(&leave);
      }

      POINT cursor;
      GetCursorPos(&cursor);
      ScreenToClient(hwnd, &cursor);
      window->cursor.x = cursor.x;
      window->cursor.y = cursor.y;
      window->InputEvent(UIMessage::MOUSE_MOVE, 0, 0);
   } else if (message == WM_MOUSELEAVE) {
      window->trackingLeave = false;

      if (!window->pressed) {
         window->cursor.x = -1;
         window->cursor.y = -1;
      }

      window->InputEvent(UIMessage::MOUSE_MOVE, 0, 0);
   } else if (message == WM_LBUTTONDOWN) {
      SetCapture(hwnd);
      window->InputEvent(UIMessage::LEFT_DOWN, 0, 0);
   } else if (message == WM_LBUTTONUP) {
      if (window->pressedButton == 1)
         ReleaseCapture();
      window->InputEvent(UIMessage::LEFT_UP, 0, 0);
   } else if (message == WM_MBUTTONDOWN) {
      SetCapture(hwnd);
      window->InputEvent(UIMessage::MIDDLE_DOWN, 0, 0);
   } else if (message == WM_MBUTTONUP) {
      if (window->pressedButton == 2)
         ReleaseCapture();
      window->InputEvent(UIMessage::MIDDLE_UP, 0, 0);
   } else if (message == WM_RBUTTONDOWN) {
      SetCapture(hwnd);
      window->InputEvent(UIMessage::RIGHT_DOWN, 0, 0);
   } else if (message == WM_RBUTTONUP) {
      if (window->pressedButton == 3)
         ReleaseCapture();
      window->InputEvent(UIMessage::RIGHT_UP, 0, 0);
   } else if (message == WM_MOUSEWHEEL) {
      int delta = (int)wParam >> 16;
      window->InputEvent(UIMessage::MOUSE_WHEEL, -delta, 0);
   } else if (message == WM_KEYDOWN) {
      window->ctrl  = GetKeyState(VK_CONTROL) & 0x8000;
      window->shift = GetKeyState(VK_SHIFT) & 0x8000;
      window->alt   = GetKeyState(VK_MENU) & 0x8000;

      UIKeyTyped m { .code = (UIKeycode)wParam} ;
      window->InputEvent(UIMessage::KEY_TYPED, 0, &m);
   } else if (message == WM_CHAR) {
      UIKeyTyped m;
      char       c = wParam;
      m.text       = {&c, 1};
      window->InputEvent(UIMessage::KEY_TYPED, 0, &m);
   } else if (message == WM_PAINT) {
      PAINTSTRUCT      paint;
      HDC              dc   = BeginPaint(hwnd, &paint);
      BITMAPINFOHEADER info = {0};
      info.biSize           = sizeof(info);
      info.biWidth = window->width, info.biHeight = -window->height;
      info.biPlanes = 1, info.biBitCount = 32;
      StretchDIBits(dc, 0, 0, UI_RECT_SIZE(window->bounds), 0, 0, UI_RECT_SIZE(window->bounds), window->bits.data(),
                    (BITMAPINFO*)&info, DIB_RGB_COLORS, SRCCOPY);
      EndPaint(hwnd, &paint);
   } else if (message == WM_SETCURSOR && LOWORD(lParam) == HTCLIENT) {
      ::SetCursor(ui->cursors[window->cursorStyle]);
      return 1;
   } else if (message == WM_SETFOCUS || message == WM_KILLFOCUS) {
      _UIMenusClose();

      if (message == WM_SETFOCUS) {
         _UIInspectorSetFocusedWindow(window);
         window->Message(UIMessage::WINDOW_ACTIVATE, 0, 0);
      }
   } else if (message == WM_MOUSEACTIVATE && (window->flags & UIWindow::MENU)) {
      return MA_NOACTIVATE;
   } else if (message == WM_DROPFILES) {
      HDROP  drop  = (HDROP)wParam;
      int    count = DragQueryFile(drop, 0xFFFFFFFF, NULL, 0);
      char** files = (char**)malloc(sizeof(char*) * count);

      for (int i = 0; i < count; i++) {
         int length       = DragQueryFile(drop, i, NULL, 0);
         files[i]         = (char*)malloc(length + 1);
         files[i][length] = 0;
         DragQueryFile(drop, i, files[i], length + 1);
      }

      window->Message(UIMessage::WINDOW_DROP_FILES, count, files);
      for (int i = 0; i < count; i++)
         free(files[i]);
      free(files);
      DragFinish(drop);
      _UIUpdate();
   } else if (message == WM_APP + 1) {
      window->Message((UIMessage)wParam, 0, (void*)lParam);
      _UIUpdate();
   } else {
      if (message == WM_NCLBUTTONDOWN || message == WM_NCMBUTTONDOWN || message == WM_NCRBUTTONDOWN) {
         if (~window->flags & UIWindow::MENU) {
            _UIMenusClose();
            _UIUpdate();
         }
      }

      return DefWindowProc(hwnd, message, wParam, lParam);
   }

   return 0;
}

unique_ptr<UI> UIInitialise(const UIConfig& cfg) {
   ui = new UI;
   ui->heap = GetProcessHeap();

   _UIInitialiseCommon();

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
   return unique_ptr<UI>{ui};
}

bool _UIMessageLoopSingle(int* result) {
   MSG message = {0};

   if (!ui->animating.empty()) {
      if (PeekMessage(&message, NULL, 0, 0, PM_REMOVE)) {
         if (message.message == WM_QUIT) {
            *result = message.wParam;
            return false;
         }

         TranslateMessage(&message);
         DispatchMessage(&message);
      } else {
         _UIProcessAnimations();
      }
   } else {
      if (!GetMessage(&message, NULL, 0, 0)) {
         *result = message.wParam;
         return false;
      }

      TranslateMessage(&message);
      DispatchMessage(&message);
   }

   return true;
}

void UIMenuShow(UIMenu* menu) {
   int width, height;
   _UIMenuPrepare(menu, &width, &height);
   MoveWindow(menu->window->hwnd, menu->pointX, menu->pointY, width, height, FALSE);
   ShowWindow(menu->window->hwnd, SW_SHOWNOACTIVATE);
}

UIWindow* UIWindowCreate(UIWindow* owner, uint32_t flags, const char* cTitle, int width, int height) {
   _UIMenusClose();

   UIWindow* window = new UIWindow(NULL, flags | UIElement::WINDOW, _UIWindowMessage, "Window");
   _UIWindowAdd(window);
   if (owner)
      window->scale = owner->scale;

   if (flags & UIWindow::MENU) {
      UI_ASSERT(owner);

      window->hwnd = CreateWindowEx(WS_EX_TOPMOST | WS_EX_NOACTIVATE, "shadow", 0, WS_POPUP, 0, 0, 0, 0, owner->hwnd,
                                    NULL, NULL, NULL);
   } else {
      window->hwnd = CreateWindowEx(WS_EX_ACCEPTFILES, "normal", cTitle, WS_OVERLAPPEDWINDOW, CW_USEDEFAULT,
                                    CW_USEDEFAULT, width ? width : CW_USEDEFAULT, height ? height : CW_USEDEFAULT,
                                    owner ? owner->hwnd : NULL, NULL, NULL, NULL);
   }

   SetWindowLongPtr(window->hwnd, GWLP_USERDATA, (LONG_PTR)window);

   if (~flags & UIWindow::MENU) {
      ShowWindow(window->hwnd, SW_SHOW);
      PostMessage(window->hwnd, WM_SIZE, 0, 0);
   }

   return window;
}

void UIWindow::EndPaint(UIPainter* painter) {
   HDC              dc   = GetDC(hwnd);
   BITMAPINFOHEADER info = {0};
   info.biSize           = sizeof(info);
   info.biWidth = width, info.biHeight = height;
   info.biPlanes = 1, info.biBitCount = 32;
   StretchDIBits(dc, updateRegion.l, updateRegion.t, UI_RECT_SIZE(window->updateRegion),
                 updateRegion.l, updateRegion.b + 1,
                 updateRegion.width(), -updateRegion.height(),
                 bits.data(), (BITMAPINFO*)&info, DIB_RGB_COLORS, SRCCOPY);
   ReleaseDC(hwnd, dc);
}

void UIWindow::SetCursor(int cursor) {
   ::SetCursor(ui->cursors[cursor]);
}

void UIWindow::GetScreenPosition(int* _x, int* _y) {
   POINT p;
   p.x = 0;
   p.y = 0;
   ClientToScreen(window->hwnd, &p);
   *_x = p.x;
   *_y = p.y;
}

void UIWindowPostMessage(UIWindow* window, UIMessage message, void* _dp) {
   PostMessage(window->hwnd, WM_APP + 1, (WPARAM)message, (LPARAM)_dp);
}

void _UIClipboardWriteText(UIWindow* window, std::string text, sel_target_t) {
   if (OpenClipboard(window->hwnd)) {
      EmptyClipboard();
      HGLOBAL memory = GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT, text.size() + 1);
      char*   copy   = (char*)GlobalLock(memory);
      std::memcpy(copy, text.c_str(), text.size());
      GlobalUnlock(copy);
      SetClipboardData(CF_TEXT, memory);
      CloseClipboard();
   }
}

std::string _UIClipboardReadText(UIWindow* window, sel_target_t) {
   std::string res;

   if (!OpenClipboard(window->hwnd)) {
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
