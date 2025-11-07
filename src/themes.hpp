#pragma once

#include "luigi.hpp"

// --------------------------------------------------
// Themes.
// --------------------------------------------------
static UITheme dark_github_theme{
   .panel1           = 0x161B22, // GitHub dark canvas subtle
   .panel2           = 0x191D23, // GitHub dark canvas default (from theme)
   .selected         = 0x3392FF, // Selection blue
   .border           = 0x30363D, // GitHub dark border default
   .text             = 0xE1E4E8, // #E1E4E8 - default foreground
   .textDisabled     = 0x6A737D, // #6A737D - comments/disabled
   .textSelected     = 0x24292E, // Dark text on light selection
   .buttonNormal     = 0x21262D, // GitHub dark canvas inset
   .buttonHovered    = 0x30363D, // Lighter on hover
   .buttonPressed    = 0x161B22, // Darker when pressed
   .buttonDisabled   = 0x24292E, // Disabled appearance
   .textboxFocused   = 0x2F363D, // Slightly lighter when focused
   .codeFocused      = 0x2B3036, // Subtle focused line
   .codeBackground   = 0x191D23, // #191D23 - background
   .codeDefault      = 0xE1E4E8, // #E1E4E8 - default text
   .codeComment      = 0x6A737D, // #6A737D - comment
   .codeString       = 0x9ECBFF, // #9ECBFF - string
   .codeNumber       = 0x79B8FF, // #79B8FF - constant
   .codeOperator     = 0xE1E4E8, // #E1E4E8 - operators (default)
   .codePreprocessor = 0xF97583, // #F97583 - storage/keyword (preprocessor-like)
   .codeKeyword      = 0xF97583, // #F97583 - keyword
   .codeType         = 0xB392F0, // #B392F0 - entity/entity.name (types)
   .codeVariable     = 0xFFAB70, // #FFAB70 - variable
   .codeFunction     = 0xB392F0, // #B392F0 - entity.name (functions)
   .codeBracket      = 0xD1D5DA, // #D1D5DA - brackethighlighter
   .accent1          = 0xF44747, // #F44747 - token.error-token
   .accent2          = 0x85E89D, // #85E89D - entity.name.tag (success green)
};

static UITheme dark_github_colorblind_theme{
   .panel1           = 0x161B22, // GitHub dark canvas subtle
   .panel2           = 0x0D1117, // GitHub dark dimmed background
   .selected         = 0x3392FF, // Selection blue
   .border           = 0x30363D, // GitHub dark border default
   .text             = 0xC9D1D9, // #C9D1D9 - default foreground
   .textDisabled     = 0x8B949E, // #8B949E - comments/disabled
   .textSelected     = 0x0D1117, // Dark text on light selection
   .buttonNormal     = 0x21262D, // GitHub dark canvas inset
   .buttonHovered    = 0x30363D, // Lighter on hover
   .buttonPressed    = 0x161B22, // Darker when pressed
   .buttonDisabled   = 0x0D1117, // Disabled appearance
   .textboxNormal    = 0x161B22, // Dark input
   .textboxFocused   = 0x21262D, // Slightly lighter when focused
   .codeFocused      = 0x1C2128, // Subtle focused line
   .codeBackground   = 0x0D1117, // #0D1117 - GitHub dark dimmed background
   .codeDefault      = 0xC9D1D9, // #C9D1D9 - meta.embedded, default text
   .codeComment      = 0x8B949E, // #8B949E - comment (muted gray)
   .codeString       = 0xA5D6FF, // #A5D6FF - string (light blue)
   .codeNumber       = 0x79C0FF, // #79C0FF - constant (bright blue)
   .codeOperator     = 0xC9D1D9, // #C9D1D9 - operators (default)
   .codePreprocessor = 0xEC8E2C, // #EC8E2C - storage/keyword (orange)
   .codeKeyword      = 0xEC8E2C, // #EC8E2C - keyword (orange)
   .codeType         = 0x79C0FF, // #79C0FF - support/support.type (bright blue)
   .codeVariable     = 0xFDAC54, // #FDAC54 - variable (orange/gold)
   .codeFunction     = 0xD2A8FF, // #D2A8FF - entity.name.function (light purple)
   .codeBracket      = 0x8B949E, // #8B949E - brackethighlighter (muted gray)
   .accent1          = 0xF44747, // #F44747 - token.error-token (red)
   .accent2          = 0xA5D6FF, // #A5D6FF - success (light blue)
};

static UITheme dark_green_tea_theme{
   .panel1           = 0x2B2B2B, // Dark green tea panel
   .panel2           = 0x1E1E1E, // Dark background
   .selected         = 0x4A6EA5, // Blue selection
   .border           = 0x505050, // Medium gray border
   .text             = 0xFCC7DA, // #DAC7FC - light purple default text
   .textDisabled     = 0x808080, // Gray disabled
   .textSelected     = 0xFFFFFF, // White on selection
   .buttonNormal     = 0x2B2B2B, // Dark button
   .buttonHovered    = 0x3A3A3A, // Lighter on hover
   .buttonPressed    = 0x1E1E1E, // Darker when pressed
   .buttonDisabled   = 0x252525, // Dark disabled
   .textboxNormal    = 0x1E1E1E, // Dark input
   .textboxFocused   = 0x2B2B2B, // Slightly lighter when focused
   .codeFocused      = 0x282828, // Subtle focused line
   .codeBackground   = 0x1E1E1E, // #1E1E1E - dark background
   .codeDefault      = 0xFCC7DA, // #DAC7FC - light purple default text
   .codeComment      = 0x6FAF5D, // #5DAF6F - green comment
   .codeString       = 0xB7E2A1, // #A1E2B7 - light green string
   .codeNumber       = 0xA6BBFF, // #FFBBA6 - peach number
   .codeOperator     = 0x99CEEB, // #EBCE99 - cream/tan operator
   .codePreprocessor = 0xFF89AE, // #AE89FF - purple preprocessor
   .codeKeyword      = 0xFF89AE, // #AE89FF - purple keyword (storage)
   .codeType         = 0x99CEEB, // #EBCE99 - cream type (support.type, entity.name.type)
   .codeVariable     = 0xFFA6F0, // #F0A6FF - pink-purple variable (parameter)
   .codeFunction     = 0xFFB994, // #94B9FF - light blue function (entity.name.function)
   .codeBracket      = 0xA666FF, // #FF66A6 - pink bracket/punctuation
   .accent1          = 0x4747F4, // #F44747 - error token (red)
   .accent2          = 0xB7E2A1, // #A1E2B7 - success (light green)
};

static UITheme dark_high_contrast_theme{
   .panel1           = 0x1C1C1C, // Very dark panel - high contrast
   .panel2           = 0x000000, // Pure black background
   .selected         = 0x3A6EA5, // Bright blue selection
   .border           = 0x6B6B6B, // Light gray border - high contrast
   .text             = 0xFFFFFF, // #FFFFFF - pure white text
   .textDisabled     = 0x808080, // Gray disabled
   .textSelected     = 0xFFFFFF, // White on selection
   .buttonNormal     = 0x1C1C1C, // Very dark button
   .buttonHovered    = 0x2D2D30, // Lighter on hover
   .buttonPressed    = 0x0E0E0E, // Darker when pressed
   .buttonDisabled   = 0x000000, // Black disabled
   .textboxNormal    = 0x0D0D0D, // Very dark input
   .textboxFocused   = 0x1C1C1C, // Slightly lighter when focused
   .codeFocused      = 0x1A1A1A, // Subtle focused line
   .codeBackground   = 0x000000, // #000000 - pure black background
   .codeDefault      = 0xFFFFFF, // #FFFFFF - pure white default text
   .codeComment      = 0x7CA668, // #7CA668 - comment (light green)
   .codeString       = 0xCE9178, // #CE9178 - string (orange/tan)
   .codeNumber       = 0xB5CEA8, // #B5CEA8 - constant.numeric (light green)
   .codeOperator     = 0xD4D4D4, // #D4D4D4 - keyword.operator (light gray)
   .codePreprocessor = 0x569CD6, // #569CD6 - meta.preprocessor (blue)
   .codeKeyword      = 0xC586C0, // #C586C0 - keyword.control (purple)
   .codeType         = 0x4EC9B0, // #4EC9B0 - entity.name.type (cyan/turquoise)
   .codeVariable     = 0x9CDCFE, // #9CDCFE - variable (light blue)
   .codeFunction     = 0xDCDCAA, // #DCDCAA - entity.name.function (yellow/gold)
   .codeBracket      = 0xD4D4D4, // #D4D4D4 - punctuation (light gray)
   .accent1          = 0xFF0000, // #FF0000 - token.error-token (bright red)
   .accent2          = 0x008000, // #008000 - token.warn-token (green)
};

UITheme dark_jacaranda_theme{
   .panel1           = 0x2B2B2B, // Dark jacaranda panel
   .panel2           = 0x1E1E1E, // Dark background
   .selected         = 0x4A6EA5, // Blue selection
   .border           = 0x505050, // Medium gray border
   .text             = 0xFCC7DA, // #DAC7FC - light purple default text
   .textDisabled     = 0x808080, // Gray disabled
   .textSelected     = 0xFFFFFF, // White on selection
   .buttonNormal     = 0x2B2B2B, // Dark button
   .buttonHovered    = 0x3A3A3A, // Lighter on hover
   .buttonPressed    = 0x1E1E1E, // Darker when pressed
   .buttonDisabled   = 0x252525, // Dark disabled
   .textboxNormal    = 0x1E1E1E, // Dark input
   .textboxFocused   = 0x2B2B2B, // Slightly lighter when focused
   .codeFocused      = 0x282828, // Subtle focused line
   .codeBackground   = 0x1E1E1E, // #1E1E1E - dark background
   .codeDefault      = 0xFCC7DA, // #DAC7FC - light purple default text
   .codeComment      = 0x6FAF5D, // #5DAF6F - green comment
   .codeString       = 0xB7E2A1, // #A1E2B7 - light green string
   .codeNumber       = 0xA6BBFF, // #FFBBA6 - peach/light coral number
   .codeOperator     = 0x99CEEB, // #EBCE99 - cream/tan operator
   .codePreprocessor = 0xFF89AE, // #AE89FF - purple preprocessor
   .codeKeyword      = 0xFF89AE, // #AE89FF - purple keyword (storage.type)
   .codeType         = 0x99CEEB, // #EBCE99 - cream type
   .codeVariable     = 0xFFA6F0, // #F0A6FF - pink-purple variable (parameter)
   .codeFunction     = 0xFFB994, // #94B9FF - light blue function
   .codeBracket      = 0xA666FF, // #FF66A6 - pink bracket/punctuation
   .accent1          = 0xCD3131, // #CD3131 - error (red)
   .accent2          = 0xB7E2A1, // #A1E2B7 - success (light green)
};

static UITheme dark_magnolia_theme{
   .panel1           = 0x2B2B2B, // Dark magnolia panel
   .panel2           = 0x1E1E1E, // Dark background
   .selected         = 0x4A6EA5, // Blue selection
   .border           = 0x505050, // Medium gray border
   .text             = 0xFFFFFF, // #FFFFFF - white default text (inverted from #000000)
   .textDisabled     = 0x808080, // Gray disabled
   .textSelected     = 0xFFFFFF, // White on selection
   .buttonNormal     = 0x2B2B2B, // Dark button
   .buttonHovered    = 0x3A3A3A, // Lighter on hover
   .buttonPressed    = 0x1E1E1E, // Darker when pressed
   .buttonDisabled   = 0x252525, // Dark disabled
   .textboxNormal    = 0x1E1E1E, // Dark input
   .textboxFocused   = 0x2B2B2B, // Slightly lighter when focused
   .codeFocused      = 0x282828, // Subtle focused line
   .codeBackground   = 0x1E1E1E, // #1E1E1E - dark background
   .codeDefault      = 0xFFFFFF, // #FFFFFF - white default text (bold)
   .codeComment      = 0x0078D7, // #0078D7 - comment (blue, italic)
   .codeString       = 0x5C9FFF, // Brightened #0000FF - bright blue string (bold)
   .codeNumber       = 0xB366B3, // Brightened #800080 - light purple (from purple)
   .codeOperator     = 0xFF4040, // Brightened #FF0000 - bright red operators (bold!)
   .codePreprocessor = 0x4D994D, // Brightened #008000 - light green preprocessor
   .codeKeyword      = 0xFFFFFF, // #FFFFFF - white keywords (bold, from black)
   .codeType         = 0xFFFFFF, // #FFFFFF - white types (bold, from black)
   .codeVariable     = 0xFFFFFF, // #FFFFFF - white variables (from black)
   .codeFunction     = 0xFFFFFF, // #FFFFFF - white functions (from black)
   .codeBracket      = 0xFF4040, // Brightened #FF0000 - bright red brackets (bold!)
   .accent1          = 0xCD3131, // #CD3131 - token.error-token (red)
   .accent2          = 0x4D994D, // Brightened green - success
};

static UITheme dark_modern_theme{
   .panel1           = 0x252526, // VS dark modern panel
   .panel2           = 0x1E1E1E, // VS dark modern background
   .selected         = 0x264F78, // VS dark selection
   .border           = 0x3E3E42, // VS dark border
   .text             = 0xD4D4D4, // #D4D4D4 - default foreground
   .textDisabled     = 0x808080, // Gray disabled
   .textSelected     = 0xFFFFFF, // White on selection
   .buttonNormal     = 0x333337, // Dark button
   .buttonHovered    = 0x3E3E42, // Lighter on hover
   .buttonPressed    = 0x252526, // Darker when pressed
   .buttonDisabled   = 0x2D2D30, // Disabled appearance
   .textboxNormal    = 0x2D2D30, // Dark input
   .textboxFocused   = 0x3F3F46, // Slightly lighter when focused
   .codeFocused      = 0x2A2D2E, // Subtle focused line
   .codeBackground   = 0x1E1E1E, // #1E1E1E - VS dark modern background
   .codeDefault      = 0xD4D4D4, // #D4D4D4 - meta.embedded, default text
   .codeComment      = 0x6A9955, // #6A9955 - comment (green)
   .codeString       = 0xCE9178, // #CE9178 - string (orange/tan)
   .codeNumber       = 0xB5CEA8, // #B5CEA8 - constant.numeric (light green)
   .codeOperator     = 0xD4D4D4, // #D4D4D4 - keyword.operator (default)
   .codePreprocessor = 0x569CD6, // #569CD6 - meta.preprocessor (blue)
   .codeKeyword      = 0xC586C0, // #C586C0 - keyword.control (purple)
   .codeType         = 0x4EC9B0, // #4EC9B0 - entity.name.type (cyan/turquoise)
   .codeVariable     = 0x9CDCFE, // #9CDCFE - variable (light blue)
   .codeFunction     = 0xDCDCAA, // #DCDCAA - entity.name.function (yellow/gold)
   .codeBracket      = 0xD4D4D4, // #D4D4D4 - punctuation (default)
   .accent1          = 0xF44747, // #F44747 - token.error-token (red)
   .accent2          = 0xB5CEA8, // #B5CEA8 - success (light green)
};

static UITheme dark_one_dark_pro_theme{
   .panel1           = 0x252B31, // Dark gray-blue - primary panel
   .panel2           = 0x14181E, // Darker gray-blue - secondary panel
   .selected         = 0x94BEFE, // Light blue - visible selection
   .border           = 0x000000, // Black - strong border
   .text             = 0xFFFFFF, // White - high contrast on dark
   .textDisabled     = 0x787D81, // Medium gray - visibly disabled
   .textSelected     = 0x000000, // Black - readable on selection
   .buttonNormal     = 0x383D41, // Dark gray - subtle button
   .buttonHovered    = 0x4B5874, // Blue-gray - visible hover
   .buttonPressed    = 0x0D0D0F, // Very dark - clear pressed
   .buttonDisabled   = 0x1B1F23, // Dark gray - disabled appearance
   .textboxNormal    = 0x31353C, // Dark gray - input field
   .textboxFocused   = 0x4D4D59, // Purple-gray - focused input
   .codeFocused      = 0x505055, // Medium gray - focused code area
   .codeBackground   = 0x212126, // Dark purple-gray - code background
   .codeDefault      = 0xABB2BF, // Light gray - One Dark Pro foreground
   .codeComment      = 0xB4B4B4, // Gray - subdued, readable
   .codeString       = 0x98C379, // Green - One Dark Pro strings
   .codeNumber       = 0xD19A66, // Orange - One Dark Pro numbers
   .codeOperator     = 0x56B6C2, // Cyan - One Dark Pro operators
   .codePreprocessor = 0xC678DD, // Purple - One Dark Pro macros
   .codeKeyword      = 0xC678DD, // Purple - One Dark Pro keywords
   .codeType         = 0xE5C07B, // Yellow - One Dark Pro types/classes
   .codeVariable     = 0xE06C75, // Red - One Dark Pro variables
   .codeFunction     = 0x61AFEF, // Blue - One Dark Pro functions
   .codeBracket      = 0xABB2BF, // Light gray - matches default
   .accent1          = 0xF01231, // Red - alert or error
   .accent2          = 0x45F94E, // Green - success or go
};

static UITheme dark_tsoding_theme{
   .panel1           = 0x202020, // Dark tsoding panel
   .panel2           = 0x1A1A1A, // Very dark background
   .selected         = 0x3A3A3A, // Muted gray selection
   .border           = 0x404040, // Dark gray border
   .text             = 0xBBBBBB, // #BBBBBB - default foreground
   .textDisabled     = 0x555555, // #555555 - disabled/muted
   .textSelected     = 0xFFFFFF, // White on selection
   .buttonNormal     = 0x2A2A2A, // Dark button
   .buttonHovered    = 0x353535, // Lighter on hover
   .buttonPressed    = 0x1A1A1A, // Darker when pressed
   .buttonDisabled   = 0x252525, // Disabled appearance
   .textboxNormal    = 0x1A1A1A, // Very dark input
   .textboxFocused   = 0x2A2A2A, // Slightly lighter when focused
   .codeFocused      = 0x252525, // Subtle focused line
   .codeBackground   = 0x1A1A1A, // Very dark background
   .codeDefault      = 0xBBBBBB, // #BBBBBB - default text
   .codeComment      = 0x899C7D, // #7D9C89 - green/sage comment
   .codeString       = 0x9393BD, // #BD9393 - muted pink/mauve string
   .codeNumber       = 0xBBBBBB, // #BBBBBB - constant.numeric (default gray)
   .codeOperator     = 0xBBBBBB, // #BBBBBB - keyword.operator (default gray)
   .codePreprocessor = 0xE8BD99, // #99BDE8 - blue preprocessor directive
   .codeKeyword      = 0xB9E2EA, // #EAE2B9 - cream/tan keyword
   .codeType         = 0xB9E2EA, // #EAE2B9 - cream/tan type (support.type.builtin)
   .codeVariable     = 0xBBBBBB, // #BBBBBB - variable (default gray)
   .codeFunction     = 0xBBBBBB, // #BBBBBB - function (default gray)
   .codeBracket      = 0xBFC5BF, // #BFC5BF - meta.brace (light gray-green)
   .accent1          = 0x4747F4, // #F44747 - error token (red)
   .accent2          = 0xE696E6, // #6796E6 - info/success (blue)
};

static UITheme dark_vs_cpp_theme{
   .panel1           = 0x252526, // VS dark panel
   .panel2           = 0x1E1E1E, // VS dark background
   .selected         = 0x264F78, // VS dark selection
   .border           = 0x3E3E42, // VS dark border
   .text             = 0xD4D4D4, // #D4D4D4 - default foreground
   .textDisabled     = 0x808080, // Gray disabled
   .textSelected     = 0xFFFFFF, // White on selection
   .buttonNormal     = 0x333337, // Dark button
   .buttonHovered    = 0x3E3E42, // Lighter on hover
   .buttonPressed    = 0x252526, // Darker when pressed
   .buttonDisabled   = 0x2D2D30, // Disabled appearance
   .textboxNormal    = 0x2D2D30, // Dark input
   .textboxFocused   = 0x3F3F46, // Slightly lighter when focused
   .codeFocused      = 0x2A2D2E, // Subtle focused line
   .codeBackground   = 0x1E1E1E, // #1E1E1E - VS dark background
   .codeDefault      = 0xD4D4D4, // #D4D4D4 - meta.embedded, default text
   .codeComment      = 0x57A64A, // #57A64A - comment (green)
   .codeString       = 0xCE9178, // #CE9178 - string (orange/tan)
   .codeNumber       = 0xB5CEA8, // #B5CEA8 - constant.numeric (light green)
   .codeOperator     = 0xB4B4B4, // #B4B4B4 - punctuation/operators (gray)
   .codePreprocessor = 0x569CD6, // #569CD6 - meta.preprocessor (blue)
   .codeKeyword      = 0xD8A0DF, // #D8A0DF - keyword.control (light purple)
   .codeType         = 0x4EC9B0, // #4EC9B0 - entity.name.type (cyan/turquoise)
   .codeVariable     = 0x9CDCFE, // #9CDCFE - variable (light blue)
   .codeFunction     = 0xDCDCAA, // #DCDCAA - entity.name.function (yellow/gold)
   .codeBracket      = 0xB4B4B4, // #B4B4B4 - punctuation (gray)
   .accent1          = 0xF44747, // #F44747 - token.error-token (red)
   .accent2          = 0xB5CEA8, // #B5CEA8 - success (light green)
};

static UITheme light_dev_cpp_theme{
   .panel1           = 0xF5F5F5, // Light gray - Dev-C++ panel
   .panel2           = 0xFFFFFF, // White - clean background
   .selected         = 0xD8E8FF, // Light blue selection
   .border           = 0xCCCCCC, // Light gray border
   .text             = 0x000000, // #000000 - default black text
   .textDisabled     = 0x808080, // Gray disabled
   .textSelected     = 0x000000, // Black on selection
   .buttonNormal     = 0xF5F5F5, // Light gray button
   .buttonHovered    = 0xE5E5E5, // Slightly darker on hover
   .buttonPressed    = 0xD0D0D0, // Darker when pressed
   .buttonDisabled   = 0xFFFFFF, // White disabled
   .textboxNormal    = 0xFFFFFF, // White input
   .textboxFocused   = 0xFFFFFF, // White focused
   .codeFocused      = 0xF5F5F5, // Light gray focused line
   .codeBackground   = 0xFFFFFF, // #FFFFFF - white background
   .codeDefault      = 0x000000, // #000000 - default text
   .codeComment      = 0x0078D7, // #0078D7 - comment (blue, italic)
   .codeString       = 0x0000FF, // #0000FF - string (bright blue, bold)
   .codeNumber       = 0x800080, // #800080 - constant.numeric (purple)
   .codeOperator     = 0xFF0000, // #FF0000 - keyword.operator (red, bold!)
   .codePreprocessor = 0x008000, // #008000 - meta.preprocessor (green)
   .codeKeyword      = 0x000000, // #000000 - keyword (black, bold)
   .codeType         = 0x000000, // #000000 - storage.type (black, bold)
   .codeVariable     = 0x000000, // #000000 - variable (black)
   .codeFunction     = 0x000000, // #000000 - entity.name.function (black)
   .codeBracket      = 0xFF0000, // #FF0000 - punctuation (red, bold!)
   .accent1          = 0xCD3131, // #CD3131 - token.error-token (red)
   .accent2          = 0x098658, // #098658 - success (teal green)
};

static UITheme light_github_theme{
   .panel1           = 0xF6F8FA, // GitHub light canvas subtle
   .panel2           = 0xFFFFFF, // GitHub light canvas default (white)
   .selected         = 0xFFD8B5, // Warm selection
   .border           = 0xD1D5DA, // GitHub light border default
   .text             = 0x24292E, // #24292E - default foreground
   .textDisabled     = 0x6A737D, // #6A737D - comments/disabled
   .textSelected     = 0x24292E, // Dark text on light selection
   .buttonNormal     = 0xFAFBFC, // GitHub light canvas inset
   .buttonHovered    = 0xF3F4F6, // Slightly darker on hover
   .buttonPressed    = 0xEDEFF2, // Darker when pressed
   .buttonDisabled   = 0xFFFFFF, // Disabled appearance
   .textboxNormal    = 0xFFFFFF, // White input
   .textboxFocused   = 0xFFFFFF, // White focused
   .codeFocused      = 0xF6F8FA, // Subtle focused line
   .codeBackground   = 0xFFFFFF, // #FFFFFF - white background
   .codeDefault      = 0x24292E, // #24292E - default text
   .codeComment      = 0x6A737D, // #6A737D - comment
   .codeString       = 0x032F62, // #032F62 - string (dark blue)
   .codeNumber       = 0x005CC5, // #005CC5 - constant (blue)
   .codeOperator     = 0x24292E, // #24292E - operators (default)
   .codePreprocessor = 0xD73A49, // #D73A49 - storage/keyword (preprocessor-like)
   .codeKeyword      = 0xD73A49, // #D73A49 - keyword (red/pink)
   .codeType         = 0x6F42C1, // #6F42C1 - entity/entity.name (purple)
   .codeVariable     = 0xE36209, // #E36209 - variable (orange)
   .codeFunction     = 0x6F42C1, // #6F42C1 - entity.name (purple)
   .codeBracket      = 0x586069, // #586069 - brackethighlighter (gray)
   .accent1          = 0xB31D28, // #B31D28 - invalid/errors (red)
   .accent2          = 0x22863A, // #22863A - success (green from entity.name.tag)
};

static UITheme light_high_contrast_theme{
   .panel1           = 0xF0F0F0, // Very light gray - high contrast panel
   .panel2           = 0xFFFFFF, // White - clean background
   .selected         = 0xDAE8FF, // Light blue selection
   .border           = 0x858585, // Dark gray border - high contrast
   .text             = 0x0E1116, // #0E1116 - very dark default text
   .textDisabled     = 0x66707B, // #66707B - medium gray disabled
   .textSelected     = 0x0E1116, // Very dark on selection
   .buttonNormal     = 0xF0F0F0, // Light gray button
   .buttonHovered    = 0xE0E0E0, // Slightly darker on hover
   .buttonPressed    = 0xD0D0D0, // Darker when pressed
   .buttonDisabled   = 0xFFFFFF, // White disabled
   .textboxNormal    = 0xFFFFFF, // White input
   .textboxFocused   = 0xFFFFFF, // White focused
   .codeFocused      = 0xF0F0F0, // Light gray focused line
   .codeBackground   = 0xFFFFFF, // #FFFFFF - white background
   .codeDefault      = 0x0E1116, // #0E1116 - very dark default text
   .codeComment      = 0x66707B, // #66707B - comment (medium gray)
   .codeString       = 0x032563, // #032563 - string (dark blue)
   .codeNumber       = 0x023B95, // #023B95 - constant (bright blue)
   .codeOperator     = 0x0E1116, // #0E1116 - operators (default)
   .codePreprocessor = 0xA0111F, // #A0111F - storage/keyword (dark red)
   .codeKeyword      = 0xA0111F, // #A0111F - keyword (dark red)
   .codeType         = 0x023B95, // #023B95 - support/support.type (bright blue)
   .codeVariable     = 0x702C00, // #702C00 - variable (dark brown/orange)
   .codeFunction     = 0x622CBC, // #622CBC - entity.name.function (dark purple)
   .codeBracket      = 0x4B535D, // #4B535D - brackethighlighter (dark gray)
   .accent1          = 0x6E011A, // #6E011A - invalid/errors (dark red)
   .accent2          = 0x024C1A, // #024C1A - success (dark green)
};

static UITheme light_quiet_theme{
   .panel1           = 0xF5F5F5, // Light gray - quiet panel
   .panel2           = 0xFFFFFF, // White - clean background
   .selected         = 0xC9D0D9, // Soft blue-gray selection
   .border           = 0xCCCCCC, // Light gray border
   .text             = 0x333333, // #333333 - default text (dark gray)
   .textDisabled     = 0xAAAAAA, // #AAAAAA - disabled text
   .textSelected     = 0x333333, // Dark gray on selection
   .buttonNormal     = 0xF5F5F5, // Light gray button
   .buttonHovered    = 0xE5E5E5, // Slightly darker on hover
   .buttonPressed    = 0xD0D0D0, // Darker when pressed
   .buttonDisabled   = 0xFFFFFF, // White disabled
   .textboxNormal    = 0xFFFFFF, // White input
   .textboxFocused   = 0xFFFFFF, // White focused
   .codeFocused      = 0xF5F5F5, // Light gray focused line
   .codeBackground   = 0xFFFFFF, // #FFFFFF - white background
   .codeDefault      = 0x333333, // #333333 - meta.embedded, default text
   .codeComment      = 0xAAAAAA, // #AAAAAA - comment (light gray)
   .codeString       = 0x448C27, // #448C27 - string (green)
   .codeNumber       = 0x9C5D27, // #9C5D27 - constant.numeric (brown/orange)
   .codeOperator     = 0x777777, // #777777 - keyword.operator (gray)
   .codePreprocessor = 0x4B69C6, // #4B69C6 - preprocessor (blue, like keywords)
   .codeKeyword      = 0x4B69C6, // #4B69C6 - keyword/storage (blue)
   .codeType         = 0x7A3E9D, // #7A3E9D - storage.type/support.type (purple)
   .codeVariable     = 0x7A3E9D, // #7A3E9D - variable (purple)
   .codeFunction     = 0xAA3731, // #AA3731 - entity.name.function (red/brown)
   .codeBracket      = 0x777777, // #777777 - punctuation (gray)
   .accent1          = 0xCD3131, // #CD3131 - token.error-token (red)
   .accent2          = 0x448C27, // #448C27 - success (green)
};

static UITheme light_solarized_theme{
   .panel1           = 0xEEE8D5, // Solarized base2 - warm beige panel
   .panel2           = 0xFDF6E3, // Solarized base3 - lightest background
   .selected         = 0xD3CBB7, // Warm tan - solarized selection
   .border           = 0x93A1A1, // Solarized base1 - subtle border
   .text             = 0x657B83, // Solarized base00 - body text
   .textDisabled     = 0x93A1A1, // Solarized base1 - disabled text
   .textSelected     = 0x002B36, // Solarized base03 - dark on selection
   .buttonNormal     = 0xEEE8D5, // Solarized base2 - button
   .buttonHovered    = 0xD3CBB7, // Darker beige - hover
   .buttonPressed    = 0xC9C0AD, // Even darker - pressed
   .buttonDisabled   = 0xFDF6E3, // Solarized base3 - disabled
   .textboxNormal    = 0xFDF6E3, // Solarized base3 - input
   .textboxFocused   = 0xFFFBEE, // Slightly brighter - focused
   .codeFocused      = 0xEEE8D5, // Solarized base2 - focused line
   .codeBackground   = 0xFDF6E3, // Solarized base3 - code background
   .codeDefault      = 0x657B83, // #657B83 - meta.embedded, default text
   .codeComment      = 0x93A1A1, // #93A1A1 - comment
   .codeString       = 0x2AA198, // #2AA198 - string (cyan)
   .codeNumber       = 0xD33682, // #D33682 - constant.numeric (magenta)
   .codeOperator     = 0x657B83, // #657B83 - operators (default)
   .codePreprocessor = 0xB58900, // #B58900 - meta.preprocessor (yellow)
   .codeKeyword      = 0x859900, // #859900 - keyword (green)
   .codeType         = 0xCB4B16, // #CB4B16 - entity.name.class/type (orange)
   .codeVariable     = 0x268BD2, // #268BD2 - variable.language/other (blue)
   .codeFunction     = 0x268BD2, // #268BD2 - entity.name.function (blue)
   .codeBracket      = 0x657B83, // #657B83 - brackets (default)
   .accent1          = 0xDC322F, // #DC322F - invalid, errors (red)
   .accent2          = 0x859900, // #859900 - success (green)
};

static UITheme light_vs_2017_cpp_theme{
   .panel1           = 0xF5F5F5, // Light gray - VS panel
   .panel2           = 0xFFFFFF, // White - clean background
   .selected         = 0xADD6FF, // Light blue - VS selection
   .border           = 0xCCCEDB, // Light gray border
   .text             = 0x000000, // #000000 - default black text
   .textDisabled     = 0x808080, // #808080 - gray disabled
   .textSelected     = 0x000000, // Black on selection
   .buttonNormal     = 0xF5F5F5, // Light gray button
   .buttonHovered    = 0xE5E5E5, // Slightly darker on hover
   .buttonPressed    = 0xD0D0D0, // Darker when pressed
   .buttonDisabled   = 0xFFFFFF, // White disabled
   .textboxNormal    = 0xFFFFFF, // White input
   .textboxFocused   = 0xFFFFFF, // White focused
   .codeFocused      = 0xF5F5F5, // Light gray focused line
   .codeBackground   = 0xFFFFFF, // #FFFFFF - white background
   .codeDefault      = 0x000000, // #000000 - default text
   .codeComment      = 0x008000, // #008000 - comment (green)
   .codeString       = 0xA31515, // #A31515 - string (dark red)
   .codeNumber       = 0x098658, // #098658 - constant.numeric (teal)
   .codeOperator     = 0x000000, // #000000 - keyword.operator (black)
   .codePreprocessor = 0x0000FF, // #0000FF - meta.preprocessor (blue)
   .codeKeyword      = 0x0000FF, // #0000FF - keyword (blue)
   .codeType         = 0x2B91AF, // #2B91AF - entity.name.type/class (VS blue)
   .codeVariable     = 0x000000, // #000000 - variable (black)
   .codeFunction     = 0x000000, // #000000 - entity.name (black)
   .codeBracket      = 0x000000, // #000000 - brackets (black)
   .accent1          = 0xCD3131, // #CD3131 - invalid/token.error-token (red)
   .accent2          = 0x098658, // #098658 - success (teal green)
};

static UITheme light_vs_cpp_theme{
   .panel1           = 0xF5F5F5, // Light gray - VS panel
   .panel2           = 0xFFFFFF, // White - clean background
   .selected         = 0xADD6FF, // Light blue - VS selection
   .border           = 0xCCCEDB, // Light gray border
   .text             = 0x000000, // #000000 - default black text
   .textDisabled     = 0x808080, // #808080 - gray disabled
   .textSelected     = 0x000000, // Black on selection
   .buttonNormal     = 0xF5F5F5, // Light gray button
   .buttonHovered    = 0xE5E5E5, // Slightly darker on hover
   .buttonPressed    = 0xD0D0D0, // Darker when pressed
   .buttonDisabled   = 0xFFFFFF, // White disabled
   .textboxNormal    = 0xFFFFFF, // White input
   .textboxFocused   = 0xFFFFFF, // White focused
   .codeFocused      = 0xF5F5F5, // Light gray focused line
   .codeBackground   = 0xFFFFFF, // #FFFFFF - white background
   .codeDefault      = 0x000000, // #000000 - meta.embedded, default text
   .codeComment      = 0x008000, // #008000 - comment (green)
   .codeString       = 0xA31515, // #A31515 - string (dark red)
   .codeNumber       = 0x098658, // #098658 - constant.numeric (teal)
   .codeOperator     = 0x000000, // #000000 - keyword.operator (black)
   .codePreprocessor = 0x0000FF, // #0000FF - meta.preprocessor (blue)
   .codeKeyword      = 0x8F08C4, // #8F08C4 - keyword.control (purple)
   .codeType         = 0x2B91AF, // #2B91AF - entity.name.type/support.type (VS blue)
   .codeVariable     = 0x1F377F, // #1F377F - variable (medium blue)
   .codeFunction     = 0x74531F, // #74531F - entity.name.function (brown)
   .codeBracket      = 0x000000, // #000000 - brackets (black)
   .accent1          = 0xCD3131, // #CD3131 - token.error-token (red)
   .accent2          = 0x098658, // #098658 - success (teal green)
};

// --------------------------------------------------
// Unused Themes.
// --------------------------------------------------
static UITheme classic_theme{
   .panel1           = 0xF0F0F0, // Light gray - soft panel background
   .panel2           = 0xFFFFFF, // White - clean secondary panel
   .selected         = 0x94BEFE, // Light blue - classic selection color
   .border           = 0x404040, // Dark gray - subtle border
   .text             = 0x000000, // Black - high contrast text
   .textDisabled     = 0x404040, // Dark gray - visibly disabled
   .textSelected     = 0x000000, // Black - readable on selection
   .buttonNormal     = 0xE0E0E0, // Light gray - subtle button
   .buttonHovered    = 0xF0F0F0, // Lighter gray - visible hover
   .buttonPressed    = 0xA0A0A0, // Medium gray - clear pressed state
   .buttonDisabled   = 0xF0F0F0, // Light gray - disabled appearance
   .textboxNormal    = 0xF8F8F8, // Very light gray - input field
   .textboxFocused   = 0xFFFFFF, // White - focused input
   .codeFocused      = 0xE0E0E0, // Light gray - focused code area
   .codeBackground   = 0xFFFFFF, // White - clean code background
   .codeDefault      = 0x000000, // Black - high contrast
   .codeComment      = 0xA11F20, // Dark red - muted, readable
   .codeString       = 0x037E01, // Green - classic string color
   .codeNumber       = 0x213EF1, // Blue - stands out as literal
   .codeOperator     = 0x7F0480, // Purple - distinct punctuation
   .codePreprocessor = 0x545D70, // Gray-blue - subdued
   .codeKeyword      = 0x0000FF, // Blue - classic keyword color
   .codeType         = 0x2B91AF, // Teal/cyan - distinguishes types
   .codeVariable     = 0x1E1E1E, // Very dark gray - subtle distinction
   .codeFunction     = 0x1E1E1E, // Very dark gray - same as variable
   .codeBracket      = 0x1E1E1E, // Very dark gray
   .accent1          = 0xFF0000, // Red - alert or error
   .accent2          = 0x00FF00, // Green - success or go
};

static UITheme classic2_theme{
   .panel1           = 0xF0F0F0, // Light gray - soft panel background
   .panel2           = 0xFFFFFF, // White - clean secondary panel
   .selected         = 0x94BEFE, // Light blue - classic selection color
   .border           = 0x404040, // Dark gray - subtle border
   .text             = 0x000000, // Black - high contrast text
   .textDisabled     = 0x404040, // Dark gray - visibly disabled
   .textSelected     = 0x000000, // Black - readable on selection
   .buttonNormal     = 0xE0E0E0, // Light gray - subtle button
   .buttonHovered    = 0xF0F0F0, // Lighter gray - visible hover
   .buttonPressed    = 0xA0A0A0, // Medium gray - clear pressed state
   .buttonDisabled   = 0xF0F0F0, // Light gray - disabled appearance
   .textboxNormal    = 0xF8F8F8, // Very light gray - input field
   .textboxFocused   = 0xFFFFFF, // White - focused input
   .codeFocused      = 0xE0E0E0, // Light gray - focused code area
   .codeBackground   = 0xFFFFFF, // White - clean code background
   .codeDefault      = 0x545454, // Dark gray - a11y compliant default
   .codeComment      = 0x802200, // Brown-red - a11y compliant, readable
   .codeString       = 0x008000, // Green - classic, accessible
   .codeNumber       = 0x326BAD, // Blue - accessible contrast
   .codeOperator     = 0x696969, // Medium gray - subtle operators
   .codePreprocessor = 0x9400D3, // Purple - distinct preprocessor
   .codeKeyword      = 0x0000FF, // Bright blue - control keywords stand out
   .codeType         = 0x267F99, // Teal - types distinct from keywords
   .codeVariable     = 0x545454, // Dark gray - matches default
   .codeFunction     = 0xA85D00, // Orange - functions clearly visible
   .codeBracket      = 0x545454, // Dark gray
   .accent1          = 0xFF0000, // Red - alert or error
   .accent2          = 0x00FF00, // Green - success or go
};

static UITheme dark_theme{
   .panel1           = 0x252B31, // Dark gray-blue - primary panel
   .panel2           = 0x14181E, // Darker gray-blue - secondary panel
   .selected         = 0x94BEFE, // Light blue - visible selection
   .border           = 0x000000, // Black - strong border
   .text             = 0xFFFFFF, // White - high contrast on dark
   .textDisabled     = 0x787D81, // Medium gray - visibly disabled
   .textSelected     = 0x000000, // Black - readable on selection
   .buttonNormal     = 0x383D41, // Dark gray - subtle button
   .buttonHovered    = 0x4B5874, // Blue-gray - visible hover
   .buttonPressed    = 0x0D0D0F, // Very dark - clear pressed
   .buttonDisabled   = 0x1B1F23, // Dark gray - disabled appearance
   .textboxNormal    = 0x31353C, // Dark gray - input field
   .textboxFocused   = 0x4D4D59, // Purple-gray - focused input
   .codeFocused      = 0x505055, // Medium gray - focused code area
   .codeBackground   = 0x212126, // Dark purple-gray - code background
   .codeDefault      = 0xFFFFFF, // White - high contrast on dark
   .codeComment      = 0xB4B4B4, // Gray - subdued, readable
   .codeString       = 0xF5DDD1, // Peach - warm, soft on dark
   .codeNumber       = 0xC3F5D3, // Mint green - fresh literal color
   .codeOperator     = 0xF5D499, // Gold - subtle punctuation
   .codePreprocessor = 0xF5F3D1, // Cream - muted directive color
   .codeKeyword      = 0xCF8E6D, // Soft orange/coral - stands out
   .codeType         = 0x8CDCFE, // Light cyan - VS Code dark theme style
   .codeVariable     = 0x9CDCFE, // Light blue - VS Code variable color
   .codeFunction     = 0x9CDCFE, // Light blue - same as variable
   .codeBracket      = 0x9CDCFE, // Light blue
   .accent1          = 0xF01231, // Red - alert or error
   .accent2          = 0x45F94E, // Green - success or go
};

static UITheme hero_theme{
   .panel1           = 0x202020, // Dark gray - gruvbox dark panel
   .panel2           = 0x202020, // Dark gray - unified panels
   .selected         = 0x3C3836, // Brown-gray - gruvbox selection
   .border           = 0x404040, // Medium gray - subtle border
   .text             = 0xDDDDDD, // Light gray - readable text
   .textDisabled     = 0x787D81, // Gray - visibly disabled
   .textSelected     = 0xFFFFFF, // White - high contrast selection
   .buttonNormal     = 0x202020, // Dark gray - flat button
   .buttonHovered    = 0x4B5874, // Blue-gray - visible hover
   .buttonPressed    = 0x0D0D0F, // Very dark - clear pressed
   .buttonDisabled   = 0x1B1F23, // Dark gray - disabled appearance
   .textboxNormal    = 0x202020, // Dark gray - input field
   .textboxFocused   = 0x3C3836, // Brown-gray - focused input
   .codeFocused      = 0x3C3836, // Brown-gray - focused code area
   .codeBackground   = 0x202020, // Dark gray - gruvbox code background
   .codeDefault      = 0xBDA175, // Tan - gruvbox fg, warm
   .codeComment      = 0xA8A5A2, // Gray - subdued but visible
   .codeString       = 0xB3B54A, // Yellow-green - gruvbox string
   .codeNumber       = 0xD3869B, // Pink - gruvbox number/literal
   .codeOperator     = 0xBDA175, // Tan - blends with default
   .codePreprocessor = 0xA0B8A0, // Sage green - gruvbox directive
   .codeKeyword      = 0xFB6542, // Warm orange/red - gruvbox inspired
   .codeType         = 0x83A598, // Muted aqua/teal - gruvbox aqua
   .codeVariable     = 0xD8C09A, // Light tan - gruvbox fg
   .codeFunction     = 0xD8C09A, // Light tan - same as variable
   .codeBracket      = 0xD8C09A, // Light tan
   .accent1          = 0xFF0000, // Red - alert or error
   .accent2          = 0x00FF00, // Green - success or go
};

static UITheme ice_theme{
   .panel1           = 0xF1F4FF, // Pale blue - cool panel background
   .panel2           = 0xFFFFFF, // White - clean secondary panel
   .selected         = 0xB5D0FE, // Sky blue - icy selection color
   .border           = 0x000000, // Black - crisp border
   .text             = 0x000000, // Black - high contrast text
   .textDisabled     = 0x787D81, // Gray - visibly disabled
   .textSelected     = 0x000000, // Black - readable on selection
   .buttonNormal     = 0xEAEDFF, // Light blue - subtle button
   .buttonHovered    = 0xF0F8FF, // Azure - visible hover
   .buttonPressed    = 0xB6C5FB, // Medium blue - clear pressed
   .buttonDisabled   = 0x1B1F23, // Dark gray - disabled appearance
   .textboxNormal    = 0xFFFFFF, // White - clean input field
   .textboxFocused   = 0xFFFFFF, // White - focused input
   .codeFocused      = 0x9CD6FF, // Light blue - focused code area
   .codeBackground   = 0xE8F2FF, // Ice blue - cool code background
   .codeDefault      = 0x000000, // Black - crisp on ice blue
   .codeComment      = 0xAB6F81, // Mauve - soft, non-distracting
   .codeString       = 0x0F7D32, // Forest green - natural contrast
   .codeNumber       = 0x0058F5, // Bright blue - pops as literal
   .codeOperator     = 0x720EE7, // Violet - distinctive punctuation
   .codePreprocessor = 0x900092, // Magenta - strong directive marker
   .codeKeyword      = 0x0040FF, // Bright blue - pops on ice background
   .codeType         = 0x267F99, // Deep teal - cool tone fits ice theme
   .codeVariable     = 0x001080, // Dark blue - readable on light
   .codeFunction     = 0x001080, // Dark blue - same as variable
   .codeBracket      = 0x001080, // Dark blue
   .accent1          = 0xFF0000, // Red - alert or error
   .accent2          = 0x00FF00, // Green - success or go
};

static UITheme lotus_theme{
   .panel1           = 0xFFF1F4, // Pale pink - soft panel background
   .panel2           = 0xFFFFFF, // White - clean secondary panel
   .selected         = 0xFEB5D0, // Pink - lotus blossom selection
   .border           = 0x000000, // Black - crisp border
   .text             = 0x000000, // Black - high contrast text
   .textDisabled     = 0x81787D, // Gray - visibly disabled
   .textSelected     = 0x000000, // Black - readable on selection
   .buttonNormal     = 0xFFEAED, // Light pink - subtle button
   .buttonHovered    = 0xFFF0F8, // Lighter pink - visible hover
   .buttonPressed    = 0xFBB6C5, // Rose pink - clear pressed
   .buttonDisabled   = 0x231B1F, // Dark gray - disabled appearance
   .textboxNormal    = 0xFFFFFF, // White - clean input field
   .textboxFocused   = 0xFFFFFF, // White - focused input
   .codeFocused      = 0xFCBAFF, // Light purple - focused code area
   .codeBackground   = 0xFFE8F2, // Pale pink - warm code background
   .codeDefault      = 0x000000, // Black - clean on pink
   .codeComment      = 0x817B6F, // Brown-gray - earthy, subdued
   .codeString       = 0x704697, // Purple - harmonizes with lotus
   .codeNumber       = 0xC21140, // Rose red - fits pink theme
   .codeOperator     = 0xC76716, // Burnt orange - warm punctuation
   .codePreprocessor = 0x7A7092, // Dusty purple - muted directive
   .codeKeyword      = 0xD13B9E, // Pink/magenta - fits lotus pink theme
   .codeType         = 0x8B4789, // Purple - harmonizes with pink
   .codeVariable     = 0x624C62, // Muted purple - fits lotus theme
   .codeFunction     = 0x624C62, // Muted purple - same as variable
   .codeBracket      = 0x624C62, // Muted purple
   .accent1          = 0xFF0000, // Red - alert or error
   .accent2          = 0x00FF00, // Green - success or go
};



// --------------------------------------------------
// --------------------------------------------------
static std::map<std::string, UITheme> ui_themes{
   {"dark_github",            dark_github_theme           },
   {"dark_github_colorblind", dark_github_colorblind_theme},
   {"dark_green_tea",         dark_green_tea_theme        },
   {"dark_high_contrast",     dark_high_contrast_theme    },
   {"dark_jacaranda",         dark_jacaranda_theme        },
   {"dark_magnolia",          dark_magnolia_theme         },
   {"dark_modern",            dark_modern_theme           },
   {"dark_one_dark_pro",      dark_one_dark_pro_theme     },
   {"dark_tsoding",           dark_tsoding_theme          },
   {"dark_vs_cpp",            dark_vs_cpp_theme           },
   {"light_dev_cpp",          light_dev_cpp_theme         },
   {"light_github",           light_github_theme          },
   {"light_high_contrast",    light_high_contrast_theme   },
   {"light_quiet",            light_quiet_theme           },
   {"light_solarized",        light_solarized_theme       },
   {"light_vs_2017_cpp",      light_vs_2017_cpp_theme     },
   {"light_vs_cpp",           light_vs_cpp_theme          }
};
