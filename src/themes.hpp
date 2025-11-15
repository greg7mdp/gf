#pragma once

#include "luigi.hpp"

// --------------------------------------------------
// Themes.
// --------------------------------------------------
static UITheme dark_github_theme{
   .panel1            = 0x161B22, // GitHub dark canvas subtle
   .panel2            = 0x191D23, // GitHub dark canvas default (from theme)
   .selected          = 0x3392FF, // Selection blue
   .border            = 0x30363D, // GitHub dark border default
   .text              = 0xE1E4E8, // #E1E4E8 - default foreground
   .text_disabled     = 0x6A737D, // #6A737D - comments/disabled
   .text_selected     = 0x24292E, // Dark text on light selection
   .button_normal     = 0x21262D, // GitHub dark canvas inset
   .button_hovered    = 0x30363D, // Lighter on hover
   .button_pressed    = 0x161B22, // Darker when pressed
   .button_disabled   = 0x24292E, // Disabled appearance
   .textbox_focused   = 0x2F363D, // Slightly lighter when focused
   .code_focused      = 0x2B3036, // Subtle focused line
   .code_background   = 0x191D23, // #191D23 - background
   .code_default      = 0xE1E4E8, // #E1E4E8 - default text
   .code_comment      = 0x6A737D, // #6A737D - comment
   .code_string       = 0x9ECBFF, // #9ECBFF - string
   .code_number       = 0x79B8FF, // #79B8FF - constant
   .code_operator     = 0xE1E4E8, // #E1E4E8 - operators (default)
   .code_preprocessor = 0xF97583, // #F97583 - storage/keyword (preprocessor-like)
   .code_keyword      = 0xF97583, // #F97583 - keyword
   .code_type         = 0xB392F0, // #B392F0 - entity/entity.name (types)
   .code_variable     = 0xFFAB70, // #FFAB70 - variable
   .code_function     = 0xB392F0, // #B392F0 - entity.name (functions)
   .code_bracket      = 0xD1D5DA, // #D1D5DA - brackethighlighter
   .accent1           = 0xF44747, // #F44747 - token.error-token
   .accent2           = 0x85E89D, // #85E89D - entity.name.tag (success green)
};

static UITheme dark_github_colorblind_theme{
   .panel1            = 0x161B22, // GitHub dark canvas subtle
   .panel2            = 0x0D1117, // GitHub dark dimmed background
   .selected          = 0x3392FF, // Selection blue
   .border            = 0x30363D, // GitHub dark border default
   .text              = 0xC9D1D9, // #C9D1D9 - default foreground
   .text_disabled     = 0x8B949E, // #8B949E - comments/disabled
   .text_selected     = 0x0D1117, // Dark text on light selection
   .button_normal     = 0x21262D, // GitHub dark canvas inset
   .button_hovered    = 0x30363D, // Lighter on hover
   .button_pressed    = 0x161B22, // Darker when pressed
   .button_disabled   = 0x0D1117, // Disabled appearance
   .textbox_normal    = 0x161B22, // Dark input
   .textbox_focused   = 0x21262D, // Slightly lighter when focused
   .code_focused      = 0x1C2128, // Subtle focused line
   .code_background   = 0x0D1117, // #0D1117 - GitHub dark dimmed background
   .code_default      = 0xC9D1D9, // #C9D1D9 - meta.embedded, default text
   .code_comment      = 0x8B949E, // #8B949E - comment (muted gray)
   .code_string       = 0xA5D6FF, // #A5D6FF - string (light blue)
   .code_number       = 0x79C0FF, // #79C0FF - constant (bright blue)
   .code_operator     = 0xC9D1D9, // #C9D1D9 - operators (default)
   .code_preprocessor = 0xEC8E2C, // #EC8E2C - storage/keyword (orange)
   .code_keyword      = 0xEC8E2C, // #EC8E2C - keyword (orange)
   .code_type         = 0x79C0FF, // #79C0FF - support/support.type (bright blue)
   .code_variable     = 0xFDAC54, // #FDAC54 - variable (orange/gold)
   .code_function     = 0xD2A8FF, // #D2A8FF - entity.name.function (light purple)
   .code_bracket      = 0x8B949E, // #8B949E - brackethighlighter (muted gray)
   .accent1           = 0xF44747, // #F44747 - token.error-token (red)
   .accent2           = 0xA5D6FF, // #A5D6FF - success (light blue)
};

static UITheme dark_green_tea_theme{
   .panel1            = 0x2B2B2B, // Dark green tea panel
   .panel2            = 0x1E1E1E, // Dark background
   .selected          = 0x4A6EA5, // Blue selection
   .border            = 0x505050, // Medium gray border
   .text              = 0xFCC7DA, // #DAC7FC - light purple default text
   .text_disabled     = 0x808080, // Gray disabled
   .text_selected     = 0xFFFFFF, // White on selection
   .button_normal     = 0x2B2B2B, // Dark button
   .button_hovered    = 0x3A3A3A, // Lighter on hover
   .button_pressed    = 0x1E1E1E, // Darker when pressed
   .button_disabled   = 0x252525, // Dark disabled
   .textbox_normal    = 0x1E1E1E, // Dark input
   .textbox_focused   = 0x2B2B2B, // Slightly lighter when focused
   .code_focused      = 0x282828, // Subtle focused line
   .code_background   = 0x1E1E1E, // #1E1E1E - dark background
   .code_default      = 0xFCC7DA, // #DAC7FC - light purple default text
   .code_comment      = 0x6FAF5D, // #5DAF6F - green comment
   .code_string       = 0xB7E2A1, // #A1E2B7 - light green string
   .code_number       = 0xA6BBFF, // #FFBBA6 - peach number
   .code_operator     = 0x99CEEB, // #EBCE99 - cream/tan operator
   .code_preprocessor = 0xFF89AE, // #AE89FF - purple preprocessor
   .code_keyword      = 0xFF89AE, // #AE89FF - purple keyword (storage)
   .code_type         = 0x99CEEB, // #EBCE99 - cream type (support.type, entity.name.type)
   .code_variable     = 0xFFA6F0, // #F0A6FF - pink-purple variable (parameter)
   .code_function     = 0xFFB994, // #94B9FF - light blue function (entity.name.function)
   .code_bracket      = 0xA666FF, // #FF66A6 - pink bracket/punctuation
   .accent1           = 0x4747F4, // #F44747 - error token (red)
   .accent2           = 0xB7E2A1, // #A1E2B7 - success (light green)
};

static UITheme dark_high_contrast_theme{
   .panel1            = 0x1C1C1C, // Very dark panel - high contrast
   .panel2            = 0x000000, // Pure black background
   .selected          = 0x3A6EA5, // Bright blue selection
   .border            = 0x6B6B6B, // Light gray border - high contrast
   .text              = 0xFFFFFF, // #FFFFFF - pure white text
   .text_disabled     = 0x808080, // Gray disabled
   .text_selected     = 0xFFFFFF, // White on selection
   .button_normal     = 0x1C1C1C, // Very dark button
   .button_hovered    = 0x2D2D30, // Lighter on hover
   .button_pressed    = 0x0E0E0E, // Darker when pressed
   .button_disabled   = 0x000000, // Black disabled
   .textbox_normal    = 0x0D0D0D, // Very dark input
   .textbox_focused   = 0x1C1C1C, // Slightly lighter when focused
   .code_focused      = 0x1A1A1A, // Subtle focused line
   .code_background   = 0x000000, // #000000 - pure black background
   .code_default      = 0xFFFFFF, // #FFFFFF - pure white default text
   .code_comment      = 0x7CA668, // #7CA668 - comment (light green)
   .code_string       = 0xCE9178, // #CE9178 - string (orange/tan)
   .code_number       = 0xB5CEA8, // #B5CEA8 - constant.numeric (light green)
   .code_operator     = 0xD4D4D4, // #D4D4D4 - keyword.operator (light gray)
   .code_preprocessor = 0x569CD6, // #569CD6 - meta.preprocessor (blue)
   .code_keyword      = 0xC586C0, // #C586C0 - keyword.control (purple)
   .code_type         = 0x4EC9B0, // #4EC9B0 - entity.name.type (cyan/turquoise)
   .code_variable     = 0x9CDCFE, // #9CDCFE - variable (light blue)
   .code_function     = 0xDCDCAA, // #DCDCAA - entity.name.function (yellow/gold)
   .code_bracket      = 0xD4D4D4, // #D4D4D4 - punctuation (light gray)
   .accent1           = 0xFF0000, // #FF0000 - token.error-token (bright red)
   .accent2           = 0x008000, // #008000 - token.warn-token (green)
};

UITheme dark_jacaranda_theme{
   .panel1            = 0x2B2B2B, // Dark jacaranda panel
   .panel2            = 0x1E1E1E, // Dark background
   .selected          = 0x4A6EA5, // Blue selection
   .border            = 0x505050, // Medium gray border
   .text              = 0xFCC7DA, // #DAC7FC - light purple default text
   .text_disabled     = 0x808080, // Gray disabled
   .text_selected     = 0xFFFFFF, // White on selection
   .button_normal     = 0x2B2B2B, // Dark button
   .button_hovered    = 0x3A3A3A, // Lighter on hover
   .button_pressed    = 0x1E1E1E, // Darker when pressed
   .button_disabled   = 0x252525, // Dark disabled
   .textbox_normal    = 0x1E1E1E, // Dark input
   .textbox_focused   = 0x2B2B2B, // Slightly lighter when focused
   .code_focused      = 0x282828, // Subtle focused line
   .code_background   = 0x1E1E1E, // #1E1E1E - dark background
   .code_default      = 0xFCC7DA, // #DAC7FC - light purple default text
   .code_comment      = 0x6FAF5D, // #5DAF6F - green comment
   .code_string       = 0xB7E2A1, // #A1E2B7 - light green string
   .code_number       = 0xA6BBFF, // #FFBBA6 - peach/light coral number
   .code_operator     = 0x99CEEB, // #EBCE99 - cream/tan operator
   .code_preprocessor = 0xFF89AE, // #AE89FF - purple preprocessor
   .code_keyword      = 0xFF89AE, // #AE89FF - purple keyword (storage.type)
   .code_type         = 0x99CEEB, // #EBCE99 - cream type
   .code_variable     = 0xFFA6F0, // #F0A6FF - pink-purple variable (parameter)
   .code_function     = 0xFFB994, // #94B9FF - light blue function
   .code_bracket      = 0xA666FF, // #FF66A6 - pink bracket/punctuation
   .accent1           = 0xCD3131, // #CD3131 - error (red)
   .accent2           = 0xB7E2A1, // #A1E2B7 - success (light green)
};

static UITheme dark_magnolia_theme{
   .panel1            = 0x2B2B2B, // Dark magnolia panel
   .panel2            = 0x1E1E1E, // Dark background
   .selected          = 0x4A6EA5, // Blue selection
   .border            = 0x505050, // Medium gray border
   .text              = 0xFFFFFF, // #FFFFFF - white default text (inverted from #000000)
   .text_disabled     = 0x808080, // Gray disabled
   .text_selected     = 0xFFFFFF, // White on selection
   .button_normal     = 0x2B2B2B, // Dark button
   .button_hovered    = 0x3A3A3A, // Lighter on hover
   .button_pressed    = 0x1E1E1E, // Darker when pressed
   .button_disabled   = 0x252525, // Dark disabled
   .textbox_normal    = 0x1E1E1E, // Dark input
   .textbox_focused   = 0x2B2B2B, // Slightly lighter when focused
   .code_focused      = 0x282828, // Subtle focused line
   .code_background   = 0x1E1E1E, // #1E1E1E - dark background
   .code_default      = 0xFFFFFF, // #FFFFFF - white default text (bold)
   .code_comment      = 0x0078D7, // #0078D7 - comment (blue, italic)
   .code_string       = 0x5C9FFF, // Brightened #0000FF - bright blue string (bold)
   .code_number       = 0xB366B3, // Brightened #800080 - light purple (from purple)
   .code_operator     = 0xFF4040, // Brightened #FF0000 - bright red operators (bold!)
   .code_preprocessor = 0x4D994D, // Brightened #008000 - light green preprocessor
   .code_keyword      = 0xFFFFFF, // #FFFFFF - white keywords (bold, from black)
   .code_type         = 0xFFFFFF, // #FFFFFF - white types (bold, from black)
   .code_variable     = 0xFFFFFF, // #FFFFFF - white variables (from black)
   .code_function     = 0xFFFFFF, // #FFFFFF - white functions (from black)
   .code_bracket      = 0xFF4040, // Brightened #FF0000 - bright red brackets (bold!)
   .accent1           = 0xCD3131, // #CD3131 - token.error-token (red)
   .accent2           = 0x4D994D, // Brightened green - success
};

static UITheme dark_modern_theme{
   .panel1            = 0x252526, // VS dark modern panel
   .panel2            = 0x1E1E1E, // VS dark modern background
   .selected          = 0x264F78, // VS dark selection
   .border            = 0x3E3E42, // VS dark border
   .text              = 0xD4D4D4, // #D4D4D4 - default foreground
   .text_disabled     = 0x808080, // Gray disabled
   .text_selected     = 0xFFFFFF, // White on selection
   .button_normal     = 0x333337, // Dark button
   .button_hovered    = 0x3E3E42, // Lighter on hover
   .button_pressed    = 0x252526, // Darker when pressed
   .button_disabled   = 0x2D2D30, // Disabled appearance
   .textbox_normal    = 0x2D2D30, // Dark input
   .textbox_focused   = 0x3F3F46, // Slightly lighter when focused
   .code_focused      = 0x2A2D2E, // Subtle focused line
   .code_background   = 0x1E1E1E, // #1E1E1E - VS dark modern background
   .code_default      = 0xD4D4D4, // #D4D4D4 - meta.embedded, default text
   .code_comment      = 0x6A9955, // #6A9955 - comment (green)
   .code_string       = 0xCE9178, // #CE9178 - string (orange/tan)
   .code_number       = 0xB5CEA8, // #B5CEA8 - constant.numeric (light green)
   .code_operator     = 0xD4D4D4, // #D4D4D4 - keyword.operator (default)
   .code_preprocessor = 0x569CD6, // #569CD6 - meta.preprocessor (blue)
   .code_keyword      = 0xC586C0, // #C586C0 - keyword.control (purple)
   .code_type         = 0x4EC9B0, // #4EC9B0 - entity.name.type (cyan/turquoise)
   .code_variable     = 0x9CDCFE, // #9CDCFE - variable (light blue)
   .code_function     = 0xDCDCAA, // #DCDCAA - entity.name.function (yellow/gold)
   .code_bracket      = 0xD4D4D4, // #D4D4D4 - punctuation (default)
   .accent1           = 0xF44747, // #F44747 - token.error-token (red)
   .accent2           = 0xB5CEA8, // #B5CEA8 - success (light green)
};

static UITheme dark_one_dark_pro_theme{
   .panel1            = 0x252B31, // Dark gray-blue - primary panel
   .panel2            = 0x14181E, // Darker gray-blue - secondary panel
   .selected          = 0x94BEFE, // Light blue - visible selection
   .border            = 0x000000, // Black - strong border
   .text              = 0xFFFFFF, // White - high contrast on dark
   .text_disabled     = 0x787D81, // Medium gray - visibly disabled
   .text_selected     = 0x000000, // Black - readable on selection
   .button_normal     = 0x383D41, // Dark gray - subtle button
   .button_hovered    = 0x4B5874, // Blue-gray - visible hover
   .button_pressed    = 0x0D0D0F, // Very dark - clear pressed
   .button_disabled   = 0x1B1F23, // Dark gray - disabled appearance
   .textbox_normal    = 0x31353C, // Dark gray - input field
   .textbox_focused   = 0x4D4D59, // Purple-gray - focused input
   .code_focused      = 0x505055, // Medium gray - focused code area
   .code_background   = 0x212126, // Dark purple-gray - code background
   .code_default      = 0xABB2BF, // Light gray - One Dark Pro foreground
   .code_comment      = 0xB4B4B4, // Gray - subdued, readable
   .code_string       = 0x98C379, // Green - One Dark Pro strings
   .code_number       = 0xD19A66, // Orange - One Dark Pro numbers
   .code_operator     = 0x56B6C2, // Cyan - One Dark Pro operators
   .code_preprocessor = 0xC678DD, // Purple - One Dark Pro macros
   .code_keyword      = 0xC678DD, // Purple - One Dark Pro keywords
   .code_type         = 0xE5C07B, // Yellow - One Dark Pro types/classes
   .code_variable     = 0xE06C75, // Red - One Dark Pro variables
   .code_function     = 0x61AFEF, // Blue - One Dark Pro functions
   .code_bracket      = 0xABB2BF, // Light gray - matches default
   .accent1           = 0xF01231, // Red - alert or error
   .accent2           = 0x45F94E, // Green - success or go
};

static UITheme dark_tsoding_theme{
   .panel1            = 0x202020, // Dark tsoding panel
   .panel2            = 0x1A1A1A, // Very dark background
   .selected          = 0x3A3A3A, // Muted gray selection
   .border            = 0x404040, // Dark gray border
   .text              = 0xBBBBBB, // #BBBBBB - default foreground
   .text_disabled     = 0x555555, // #555555 - disabled/muted
   .text_selected     = 0xFFFFFF, // White on selection
   .button_normal     = 0x2A2A2A, // Dark button
   .button_hovered    = 0x353535, // Lighter on hover
   .button_pressed    = 0x1A1A1A, // Darker when pressed
   .button_disabled   = 0x252525, // Disabled appearance
   .textbox_normal    = 0x1A1A1A, // Very dark input
   .textbox_focused   = 0x2A2A2A, // Slightly lighter when focused
   .code_focused      = 0x252525, // Subtle focused line
   .code_background   = 0x1A1A1A, // Very dark background
   .code_default      = 0xBBBBBB, // #BBBBBB - default text
   .code_comment      = 0x899C7D, // #7D9C89 - green/sage comment
   .code_string       = 0x9393BD, // #BD9393 - muted pink/mauve string
   .code_number       = 0xBBBBBB, // #BBBBBB - constant.numeric (default gray)
   .code_operator     = 0xBBBBBB, // #BBBBBB - keyword.operator (default gray)
   .code_preprocessor = 0xE8BD99, // #99BDE8 - blue preprocessor directive
   .code_keyword      = 0xB9E2EA, // #EAE2B9 - cream/tan keyword
   .code_type         = 0xB9E2EA, // #EAE2B9 - cream/tan type (support.type.builtin)
   .code_variable     = 0xBBBBBB, // #BBBBBB - variable (default gray)
   .code_function     = 0xBBBBBB, // #BBBBBB - function (default gray)
   .code_bracket      = 0xBFC5BF, // #BFC5BF - meta.brace (light gray-green)
   .accent1           = 0x4747F4, // #F44747 - error token (red)
   .accent2           = 0xE696E6, // #6796E6 - info/success (blue)
};

static UITheme dark_vs_cpp_theme{
   .panel1            = 0x252526, // VS dark panel
   .panel2            = 0x1E1E1E, // VS dark background
   .selected          = 0x264F78, // VS dark selection
   .border            = 0x3E3E42, // VS dark border
   .text              = 0xD4D4D4, // #D4D4D4 - default foreground
   .text_disabled     = 0x808080, // Gray disabled
   .text_selected     = 0xFFFFFF, // White on selection
   .button_normal     = 0x333337, // Dark button
   .button_hovered    = 0x3E3E42, // Lighter on hover
   .button_pressed    = 0x252526, // Darker when pressed
   .button_disabled   = 0x2D2D30, // Disabled appearance
   .textbox_normal    = 0x2D2D30, // Dark input
   .textbox_focused   = 0x3F3F46, // Slightly lighter when focused
   .code_focused      = 0x2A2D2E, // Subtle focused line
   .code_background   = 0x1E1E1E, // #1E1E1E - VS dark background
   .code_default      = 0xD4D4D4, // #D4D4D4 - meta.embedded, default text
   .code_comment      = 0x57A64A, // #57A64A - comment (green)
   .code_string       = 0xCE9178, // #CE9178 - string (orange/tan)
   .code_number       = 0xB5CEA8, // #B5CEA8 - constant.numeric (light green)
   .code_operator     = 0xB4B4B4, // #B4B4B4 - punctuation/operators (gray)
   .code_preprocessor = 0x569CD6, // #569CD6 - meta.preprocessor (blue)
   .code_keyword      = 0xD8A0DF, // #D8A0DF - keyword.control (light purple)
   .code_type         = 0x4EC9B0, // #4EC9B0 - entity.name.type (cyan/turquoise)
   .code_variable     = 0x9CDCFE, // #9CDCFE - variable (light blue)
   .code_function     = 0xDCDCAA, // #DCDCAA - entity.name.function (yellow/gold)
   .code_bracket      = 0xB4B4B4, // #B4B4B4 - punctuation (gray)
   .accent1           = 0xF44747, // #F44747 - token.error-token (red)
   .accent2           = 0xB5CEA8, // #B5CEA8 - success (light green)
};

static UITheme light_dev_cpp_theme{
   .panel1            = 0xF5F5F5, // Light gray - Dev-C++ panel
   .panel2            = 0xFFFFFF, // White - clean background
   .selected          = 0xD8E8FF, // Light blue selection
   .border            = 0xCCCCCC, // Light gray border
   .text              = 0x000000, // #000000 - default black text
   .text_disabled     = 0x808080, // Gray disabled
   .text_selected     = 0x000000, // Black on selection
   .button_normal     = 0xF5F5F5, // Light gray button
   .button_hovered    = 0xE5E5E5, // Slightly darker on hover
   .button_pressed    = 0xD0D0D0, // Darker when pressed
   .button_disabled   = 0xFFFFFF, // White disabled
   .textbox_normal    = 0xFFFFFF, // White input
   .textbox_focused   = 0xFFFFFF, // White focused
   .code_focused      = 0xF5F5F5, // Light gray focused line
   .code_background   = 0xFFFFFF, // #FFFFFF - white background
   .code_default      = 0x000000, // #000000 - default text
   .code_comment      = 0x0078D7, // #0078D7 - comment (blue, italic)
   .code_string       = 0x0000FF, // #0000FF - string (bright blue, bold)
   .code_number       = 0x800080, // #800080 - constant.numeric (purple)
   .code_operator     = 0xFF0000, // #FF0000 - keyword.operator (red, bold!)
   .code_preprocessor = 0x008000, // #008000 - meta.preprocessor (green)
   .code_keyword      = 0x000000, // #000000 - keyword (black, bold)
   .code_type         = 0x000000, // #000000 - storage.type (black, bold)
   .code_variable     = 0x000000, // #000000 - variable (black)
   .code_function     = 0x000000, // #000000 - entity.name.function (black)
   .code_bracket      = 0xFF0000, // #FF0000 - punctuation (red, bold!)
   .accent1           = 0xCD3131, // #CD3131 - token.error-token (red)
   .accent2           = 0x098658, // #098658 - success (teal green)
};

static UITheme light_github_theme{
   .panel1            = 0xF6F8FA, // GitHub light canvas subtle
   .panel2            = 0xFFFFFF, // GitHub light canvas default (white)
   .selected          = 0xFFD8B5, // Warm selection
   .border            = 0xD1D5DA, // GitHub light border default
   .text              = 0x24292E, // #24292E - default foreground
   .text_disabled     = 0x6A737D, // #6A737D - comments/disabled
   .text_selected     = 0x24292E, // Dark text on light selection
   .button_normal     = 0xFAFBFC, // GitHub light canvas inset
   .button_hovered    = 0xF3F4F6, // Slightly darker on hover
   .button_pressed    = 0xEDEFF2, // Darker when pressed
   .button_disabled   = 0xFFFFFF, // Disabled appearance
   .textbox_normal    = 0xFFFFFF, // White input
   .textbox_focused   = 0xFFFFFF, // White focused
   .code_focused      = 0xF6F8FA, // Subtle focused line
   .code_background   = 0xFFFFFF, // #FFFFFF - white background
   .code_default      = 0x24292E, // #24292E - default text
   .code_comment      = 0x6A737D, // #6A737D - comment
   .code_string       = 0x032F62, // #032F62 - string (dark blue)
   .code_number       = 0x005CC5, // #005CC5 - constant (blue)
   .code_operator     = 0x24292E, // #24292E - operators (default)
   .code_preprocessor = 0xD73A49, // #D73A49 - storage/keyword (preprocessor-like)
   .code_keyword      = 0xD73A49, // #D73A49 - keyword (red/pink)
   .code_type         = 0x6F42C1, // #6F42C1 - entity/entity.name (purple)
   .code_variable     = 0xE36209, // #E36209 - variable (orange)
   .code_function     = 0x6F42C1, // #6F42C1 - entity.name (purple)
   .code_bracket      = 0x586069, // #586069 - brackethighlighter (gray)
   .accent1           = 0xB31D28, // #B31D28 - invalid/errors (red)
   .accent2           = 0x22863A, // #22863A - success (green from entity.name.tag)
};

static UITheme light_high_contrast_theme{
   .panel1            = 0xF0F0F0, // Very light gray - high contrast panel
   .panel2            = 0xFFFFFF, // White - clean background
   .selected          = 0xDAE8FF, // Light blue selection
   .border            = 0x858585, // Dark gray border - high contrast
   .text              = 0x0E1116, // #0E1116 - very dark default text
   .text_disabled     = 0x66707B, // #66707B - medium gray disabled
   .text_selected     = 0x0E1116, // Very dark on selection
   .button_normal     = 0xF0F0F0, // Light gray button
   .button_hovered    = 0xE0E0E0, // Slightly darker on hover
   .button_pressed    = 0xD0D0D0, // Darker when pressed
   .button_disabled   = 0xFFFFFF, // White disabled
   .textbox_normal    = 0xFFFFFF, // White input
   .textbox_focused   = 0xFFFFFF, // White focused
   .code_focused      = 0xF0F0F0, // Light gray focused line
   .code_background   = 0xFFFFFF, // #FFFFFF - white background
   .code_default      = 0x0E1116, // #0E1116 - very dark default text
   .code_comment      = 0x66707B, // #66707B - comment (medium gray)
   .code_string       = 0x032563, // #032563 - string (dark blue)
   .code_number       = 0x023B95, // #023B95 - constant (bright blue)
   .code_operator     = 0x0E1116, // #0E1116 - operators (default)
   .code_preprocessor = 0xA0111F, // #A0111F - storage/keyword (dark red)
   .code_keyword      = 0xA0111F, // #A0111F - keyword (dark red)
   .code_type         = 0x023B95, // #023B95 - support/support.type (bright blue)
   .code_variable     = 0x702C00, // #702C00 - variable (dark brown/orange)
   .code_function     = 0x622CBC, // #622CBC - entity.name.function (dark purple)
   .code_bracket      = 0x4B535D, // #4B535D - brackethighlighter (dark gray)
   .accent1           = 0xCE312A, // #CE312A - invalid/errors (red)
   .accent2           = 0x024C1A, // #024C1A - success (dark green)
};

static UITheme light_quiet_theme{
   .panel1            = 0xF5F5F5, // Light gray - quiet panel
   .panel2            = 0xFFFFFF, // White - clean background
   .selected          = 0xC9D0D9, // Soft blue-gray selection
   .border            = 0xCCCCCC, // Light gray border
   .text              = 0x333333, // #333333 - default text (dark gray)
   .text_disabled     = 0xAAAAAA, // #AAAAAA - disabled text
   .text_selected     = 0x333333, // Dark gray on selection
   .button_normal     = 0xF5F5F5, // Light gray button
   .button_hovered    = 0xE5E5E5, // Slightly darker on hover
   .button_pressed    = 0xD0D0D0, // Darker when pressed
   .button_disabled   = 0xFFFFFF, // White disabled
   .textbox_normal    = 0xFFFFFF, // White input
   .textbox_focused   = 0xFFFFFF, // White focused
   .code_focused      = 0xF5F5F5, // Light gray focused line
   .code_background   = 0xFFFFFF, // #FFFFFF - white background
   .code_default      = 0x333333, // #333333 - meta.embedded, default text
   .code_comment      = 0xAAAAAA, // #AAAAAA - comment (light gray)
   .code_string       = 0x448C27, // #448C27 - string (green)
   .code_number       = 0x9C5D27, // #9C5D27 - constant.numeric (brown/orange)
   .code_operator     = 0x777777, // #777777 - keyword.operator (gray)
   .code_preprocessor = 0x4B69C6, // #4B69C6 - preprocessor (blue, like keywords)
   .code_keyword      = 0x4B69C6, // #4B69C6 - keyword/storage (blue)
   .code_type         = 0x7A3E9D, // #7A3E9D - storage.type/support.type (purple)
   .code_variable     = 0x7A3E9D, // #7A3E9D - variable (purple)
   .code_function     = 0xAA3731, // #AA3731 - entity.name.function (red/brown)
   .code_bracket      = 0x777777, // #777777 - punctuation (gray)
   .accent1           = 0xCD3131, // #CD3131 - token.error-token (red)
   .accent2           = 0x448C27, // #448C27 - success (green)
};

static UITheme light_solarized_theme{
   .panel1            = 0xEEE8D5, // Solarized base2 - warm beige panel
   .panel2            = 0xFDF6E3, // Solarized base3 - lightest background
   .selected          = 0xD3CBB7, // Warm tan - solarized selection
   .border            = 0x93A1A1, // Solarized base1 - subtle border
   .text              = 0x657B83, // Solarized base00 - body text
   .text_disabled     = 0x93A1A1, // Solarized base1 - disabled text
   .text_selected     = 0x002B36, // Solarized base03 - dark on selection
   .button_normal     = 0xEEE8D5, // Solarized base2 - button
   .button_hovered    = 0xD3CBB7, // Darker beige - hover
   .button_pressed    = 0xC9C0AD, // Even darker - pressed
   .button_disabled   = 0xFDF6E3, // Solarized base3 - disabled
   .textbox_normal    = 0xFDF6E3, // Solarized base3 - input
   .textbox_focused   = 0xFFFBEE, // Slightly brighter - focused
   .code_focused      = 0xEEE8D5, // Solarized base2 - focused line
   .code_background   = 0xFDF6E3, // Solarized base3 - code background
   .code_default      = 0x657B83, // #657B83 - meta.embedded, default text
   .code_comment      = 0x93A1A1, // #93A1A1 - comment
   .code_string       = 0x2AA198, // #2AA198 - string (cyan)
   .code_number       = 0xD33682, // #D33682 - constant.numeric (magenta)
   .code_operator     = 0x657B83, // #657B83 - operators (default)
   .code_preprocessor = 0xB58900, // #B58900 - meta.preprocessor (yellow)
   .code_keyword      = 0x859900, // #859900 - keyword (green)
   .code_type         = 0xCB4B16, // #CB4B16 - entity.name.class/type (orange)
   .code_variable     = 0x268BD2, // #268BD2 - variable.language/other (blue)
   .code_function     = 0x268BD2, // #268BD2 - entity.name.function (blue)
   .code_bracket      = 0x657B83, // #657B83 - brackets (default)
   .accent1           = 0xDC322F, // #DC322F - invalid, errors (red)
   .accent2           = 0x859900, // #859900 - success (green)
};

static UITheme light_vs_2017_cpp_theme{
   .panel1            = 0xF5F5F5, // Light gray - VS panel
   .panel2            = 0xFFFFFF, // White - clean background
   .selected          = 0xADD6FF, // Light blue - VS selection
   .border            = 0xCCCEDB, // Light gray border
   .text              = 0x000000, // #000000 - default black text
   .text_disabled     = 0x808080, // #808080 - gray disabled
   .text_selected     = 0x000000, // Black on selection
   .button_normal     = 0xF5F5F5, // Light gray button
   .button_hovered    = 0xE5E5E5, // Slightly darker on hover
   .button_pressed    = 0xD0D0D0, // Darker when pressed
   .button_disabled   = 0xFFFFFF, // White disabled
   .textbox_normal    = 0xFFFFFF, // White input
   .textbox_focused   = 0xFFFFFF, // White focused
   .code_focused      = 0xF5F5F5, // Light gray focused line
   .code_background   = 0xFFFFFF, // #FFFFFF - white background
   .code_default      = 0x000000, // #000000 - default text
   .code_comment      = 0x008000, // #008000 - comment (green)
   .code_string       = 0xA31515, // #A31515 - string (dark red)
   .code_number       = 0x098658, // #098658 - constant.numeric (teal)
   .code_operator     = 0x000000, // #000000 - keyword.operator (black)
   .code_preprocessor = 0x0000FF, // #0000FF - meta.preprocessor (blue)
   .code_keyword      = 0x0000FF, // #0000FF - keyword (blue)
   .code_type         = 0x2B91AF, // #2B91AF - entity.name.type/class (VS blue)
   .code_variable     = 0x000000, // #000000 - variable (black)
   .code_function     = 0x000000, // #000000 - entity.name (black)
   .code_bracket      = 0x000000, // #000000 - brackets (black)
   .accent1           = 0xCD3131, // #CD3131 - invalid/token.error-token (red)
   .accent2           = 0x098658, // #098658 - success (teal green)
};

static UITheme light_vs_cpp_theme{
   .panel1            = 0xF5F5F5, // Light gray - VS panel
   .panel2            = 0xFFFFFF, // White - clean background
   .selected          = 0xADD6FF, // Light blue - VS selection
   .border            = 0xCCCEDB, // Light gray border
   .text              = 0x000000, // #000000 - default black text
   .text_disabled     = 0x808080, // #808080 - gray disabled
   .text_selected     = 0x000000, // Black on selection
   .button_normal     = 0xF5F5F5, // Light gray button
   .button_hovered    = 0xE5E5E5, // Slightly darker on hover
   .button_pressed    = 0xD0D0D0, // Darker when pressed
   .button_disabled   = 0xFFFFFF, // White disabled
   .textbox_normal    = 0xFFFFFF, // White input
   .textbox_focused   = 0xFFFFFF, // White focused
   .code_focused      = 0xF5F5F5, // Light gray focused line
   .code_background   = 0xFFFFFF, // #FFFFFF - white background
   .code_default      = 0x000000, // #000000 - meta.embedded, default text
   .code_comment      = 0x008000, // #008000 - comment (green)
   .code_string       = 0xA31515, // #A31515 - string (dark red)
   .code_number       = 0x098658, // #098658 - constant.numeric (teal)
   .code_operator     = 0x000000, // #000000 - keyword.operator (black)
   .code_preprocessor = 0x0000FF, // #0000FF - meta.preprocessor (blue)
   .code_keyword      = 0x8F08C4, // #8F08C4 - keyword.control (purple)
   .code_type         = 0x2B91AF, // #2B91AF - entity.name.type/support.type (VS blue)
   .code_variable     = 0x1F377F, // #1F377F - variable (medium blue)
   .code_function     = 0x74531F, // #74531F - entity.name.function (brown)
   .code_bracket      = 0x000000, // #000000 - brackets (black)
   .accent1           = 0xCD3131, // #CD3131 - token.error-token (red)
   .accent2           = 0x098658, // #098658 - success (teal green)
};

// --------------------------------------------------
// Unused Themes.
// --------------------------------------------------
static UITheme classic_theme{
   .panel1            = 0xF0F0F0, // Light gray - soft panel background
   .panel2            = 0xFFFFFF, // White - clean secondary panel
   .selected          = 0x94BEFE, // Light blue - classic selection color
   .border            = 0x404040, // Dark gray - subtle border
   .text              = 0x000000, // Black - high contrast text
   .text_disabled     = 0x404040, // Dark gray - visibly disabled
   .text_selected     = 0x000000, // Black - readable on selection
   .button_normal     = 0xE0E0E0, // Light gray - subtle button
   .button_hovered    = 0xF0F0F0, // Lighter gray - visible hover
   .button_pressed    = 0xA0A0A0, // Medium gray - clear pressed state
   .button_disabled   = 0xF0F0F0, // Light gray - disabled appearance
   .textbox_normal    = 0xF8F8F8, // Very light gray - input field
   .textbox_focused   = 0xFFFFFF, // White - focused input
   .code_focused      = 0xE0E0E0, // Light gray - focused code area
   .code_background   = 0xFFFFFF, // White - clean code background
   .code_default      = 0x000000, // Black - high contrast
   .code_comment      = 0xA11F20, // Dark red - muted, readable
   .code_string       = 0x037E01, // Green - classic string color
   .code_number       = 0x213EF1, // Blue - stands out as literal
   .code_operator     = 0x7F0480, // Purple - distinct punctuation
   .code_preprocessor = 0x545D70, // Gray-blue - subdued
   .code_keyword      = 0x0000FF, // Blue - classic keyword color
   .code_type         = 0x2B91AF, // Teal/cyan - distinguishes types
   .code_variable     = 0x1E1E1E, // Very dark gray - subtle distinction
   .code_function     = 0x1E1E1E, // Very dark gray - same as variable
   .code_bracket      = 0x1E1E1E, // Very dark gray
   .accent1           = 0xFF0000, // Red - alert or error
   .accent2           = 0x00FF00, // Green - success or go
};

static UITheme classic2_theme{
   .panel1            = 0xF0F0F0, // Light gray - soft panel background
   .panel2            = 0xFFFFFF, // White - clean secondary panel
   .selected          = 0x94BEFE, // Light blue - classic selection color
   .border            = 0x404040, // Dark gray - subtle border
   .text              = 0x000000, // Black - high contrast text
   .text_disabled     = 0x404040, // Dark gray - visibly disabled
   .text_selected     = 0x000000, // Black - readable on selection
   .button_normal     = 0xE0E0E0, // Light gray - subtle button
   .button_hovered    = 0xF0F0F0, // Lighter gray - visible hover
   .button_pressed    = 0xA0A0A0, // Medium gray - clear pressed state
   .button_disabled   = 0xF0F0F0, // Light gray - disabled appearance
   .textbox_normal    = 0xF8F8F8, // Very light gray - input field
   .textbox_focused   = 0xFFFFFF, // White - focused input
   .code_focused      = 0xE0E0E0, // Light gray - focused code area
   .code_background   = 0xFFFFFF, // White - clean code background
   .code_default      = 0x545454, // Dark gray - a11y compliant default
   .code_comment      = 0x802200, // Brown-red - a11y compliant, readable
   .code_string       = 0x008000, // Green - classic, accessible
   .code_number       = 0x326BAD, // Blue - accessible contrast
   .code_operator     = 0x696969, // Medium gray - subtle operators
   .code_preprocessor = 0x9400D3, // Purple - distinct preprocessor
   .code_keyword      = 0x0000FF, // Bright blue - control keywords stand out
   .code_type         = 0x267F99, // Teal - types distinct from keywords
   .code_variable     = 0x545454, // Dark gray - matches default
   .code_function     = 0xA85D00, // Orange - functions clearly visible
   .code_bracket      = 0x545454, // Dark gray
   .accent1           = 0xFF0000, // Red - alert or error
   .accent2           = 0x00FF00, // Green - success or go
};

static UITheme dark_theme{
   .panel1            = 0x252B31, // Dark gray-blue - primary panel
   .panel2            = 0x14181E, // Darker gray-blue - secondary panel
   .selected          = 0x94BEFE, // Light blue - visible selection
   .border            = 0x000000, // Black - strong border
   .text              = 0xFFFFFF, // White - high contrast on dark
   .text_disabled     = 0x787D81, // Medium gray - visibly disabled
   .text_selected     = 0x000000, // Black - readable on selection
   .button_normal     = 0x383D41, // Dark gray - subtle button
   .button_hovered    = 0x4B5874, // Blue-gray - visible hover
   .button_pressed    = 0x0D0D0F, // Very dark - clear pressed
   .button_disabled   = 0x1B1F23, // Dark gray - disabled appearance
   .textbox_normal    = 0x31353C, // Dark gray - input field
   .textbox_focused   = 0x4D4D59, // Purple-gray - focused input
   .code_focused      = 0x505055, // Medium gray - focused code area
   .code_background   = 0x212126, // Dark purple-gray - code background
   .code_default      = 0xFFFFFF, // White - high contrast on dark
   .code_comment      = 0xB4B4B4, // Gray - subdued, readable
   .code_string       = 0xF5DDD1, // Peach - warm, soft on dark
   .code_number       = 0xC3F5D3, // Mint green - fresh literal color
   .code_operator     = 0xF5D499, // Gold - subtle punctuation
   .code_preprocessor = 0xF5F3D1, // Cream - muted directive color
   .code_keyword      = 0xCF8E6D, // Soft orange/coral - stands out
   .code_type         = 0x8CDCFE, // Light cyan - VS Code dark theme style
   .code_variable     = 0x9CDCFE, // Light blue - VS Code variable color
   .code_function     = 0x9CDCFE, // Light blue - same as variable
   .code_bracket      = 0x9CDCFE, // Light blue
   .accent1           = 0xF01231, // Red - alert or error
   .accent2           = 0x45F94E, // Green - success or go
};

static UITheme hero_theme{
   .panel1            = 0x202020, // Dark gray - gruvbox dark panel
   .panel2            = 0x202020, // Dark gray - unified panels
   .selected          = 0x3C3836, // Brown-gray - gruvbox selection
   .border            = 0x404040, // Medium gray - subtle border
   .text              = 0xDDDDDD, // Light gray - readable text
   .text_disabled     = 0x787D81, // Gray - visibly disabled
   .text_selected     = 0xFFFFFF, // White - high contrast selection
   .button_normal     = 0x202020, // Dark gray - flat button
   .button_hovered    = 0x4B5874, // Blue-gray - visible hover
   .button_pressed    = 0x0D0D0F, // Very dark - clear pressed
   .button_disabled   = 0x1B1F23, // Dark gray - disabled appearance
   .textbox_normal    = 0x202020, // Dark gray - input field
   .textbox_focused   = 0x3C3836, // Brown-gray - focused input
   .code_focused      = 0x3C3836, // Brown-gray - focused code area
   .code_background   = 0x202020, // Dark gray - gruvbox code background
   .code_default      = 0xBDA175, // Tan - gruvbox fg, warm
   .code_comment      = 0xA8A5A2, // Gray - subdued but visible
   .code_string       = 0xB3B54A, // Yellow-green - gruvbox string
   .code_number       = 0xD3869B, // Pink - gruvbox number/literal
   .code_operator     = 0xBDA175, // Tan - blends with default
   .code_preprocessor = 0xA0B8A0, // Sage green - gruvbox directive
   .code_keyword      = 0xFB6542, // Warm orange/red - gruvbox inspired
   .code_type         = 0x83A598, // Muted aqua/teal - gruvbox aqua
   .code_variable     = 0xD8C09A, // Light tan - gruvbox fg
   .code_function     = 0xD8C09A, // Light tan - same as variable
   .code_bracket      = 0xD8C09A, // Light tan
   .accent1           = 0xFF0000, // Red - alert or error
   .accent2           = 0x00FF00, // Green - success or go
};

static UITheme ice_theme{
   .panel1            = 0xF1F4FF, // Pale blue - cool panel background
   .panel2            = 0xFFFFFF, // White - clean secondary panel
   .selected          = 0xB5D0FE, // Sky blue - icy selection color
   .border            = 0x000000, // Black - crisp border
   .text              = 0x000000, // Black - high contrast text
   .text_disabled     = 0x787D81, // Gray - visibly disabled
   .text_selected     = 0x000000, // Black - readable on selection
   .button_normal     = 0xEAEDFF, // Light blue - subtle button
   .button_hovered    = 0xF0F8FF, // Azure - visible hover
   .button_pressed    = 0xB6C5FB, // Medium blue - clear pressed
   .button_disabled   = 0x1B1F23, // Dark gray - disabled appearance
   .textbox_normal    = 0xFFFFFF, // White - clean input field
   .textbox_focused   = 0xFFFFFF, // White - focused input
   .code_focused      = 0x9CD6FF, // Light blue - focused code area
   .code_background   = 0xE8F2FF, // Ice blue - cool code background
   .code_default      = 0x000000, // Black - crisp on ice blue
   .code_comment      = 0xAB6F81, // Mauve - soft, non-distracting
   .code_string       = 0x0F7D32, // Forest green - natural contrast
   .code_number       = 0x0058F5, // Bright blue - pops as literal
   .code_operator     = 0x720EE7, // Violet - distinctive punctuation
   .code_preprocessor = 0x900092, // Magenta - strong directive marker
   .code_keyword      = 0x0040FF, // Bright blue - pops on ice background
   .code_type         = 0x267F99, // Deep teal - cool tone fits ice theme
   .code_variable     = 0x001080, // Dark blue - readable on light
   .code_function     = 0x001080, // Dark blue - same as variable
   .code_bracket      = 0x001080, // Dark blue
   .accent1           = 0xFF0000, // Red - alert or error
   .accent2           = 0x00FF00, // Green - success or go
};

static UITheme lotus_theme{
   .panel1            = 0xFFF1F4, // Pale pink - soft panel background
   .panel2            = 0xFFFFFF, // White - clean secondary panel
   .selected          = 0xFEB5D0, // Pink - lotus blossom selection
   .border            = 0x000000, // Black - crisp border
   .text              = 0x000000, // Black - high contrast text
   .text_disabled     = 0x81787D, // Gray - visibly disabled
   .text_selected     = 0x000000, // Black - readable on selection
   .button_normal     = 0xFFEAED, // Light pink - subtle button
   .button_hovered    = 0xFFF0F8, // Lighter pink - visible hover
   .button_pressed    = 0xFBB6C5, // Rose pink - clear pressed
   .button_disabled   = 0x231B1F, // Dark gray - disabled appearance
   .textbox_normal    = 0xFFFFFF, // White - clean input field
   .textbox_focused   = 0xFFFFFF, // White - focused input
   .code_focused      = 0xFCBAFF, // Light purple - focused code area
   .code_background   = 0xFFE8F2, // Pale pink - warm code background
   .code_default      = 0x000000, // Black - clean on pink
   .code_comment      = 0x817B6F, // Brown-gray - earthy, subdued
   .code_string       = 0x704697, // Purple - harmonizes with lotus
   .code_number       = 0xC21140, // Rose red - fits pink theme
   .code_operator     = 0xC76716, // Burnt orange - warm punctuation
   .code_preprocessor = 0x7A7092, // Dusty purple - muted directive
   .code_keyword      = 0xD13B9E, // Pink/magenta - fits lotus pink theme
   .code_type         = 0x8B4789, // Purple - harmonizes with pink
   .code_variable     = 0x624C62, // Muted purple - fits lotus theme
   .code_function     = 0x624C62, // Muted purple - same as variable
   .code_bracket      = 0x624C62, // Muted purple
   .accent1           = 0xFF0000, // Red - alert or error
   .accent2           = 0x00FF00, // Green - success or go
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
