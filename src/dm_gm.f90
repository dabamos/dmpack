! Author:  Philipp Engel
! Licence: ISC
module dm_gm
    !! Abstraction layer over GraphicsMagick. On Linux, install the package
    !! `graphicsmagick`:
    !!
    !! ```
    !! $ sudo apt-get install graphicsmagick
    !! ```
    !!
    !! Example to read meta data of image `/tmp/image.jpg`:
    !!
    !! ```fortran
    !! character(len=*), parameter :: IMAGE_PATH = '/tmp/image.jpg'
    !!
    !! character(len=:), allocatable :: directory, format, mime
    !! integer                       :: width, height
    !! integer                       :: rc
    !!
    !! rc = dm_gm_get_dimensions(IMAGE_PATH, width, height)
    !! print '("image dimensions: ", i0, "x", i0)', width, height
    !!
    !! rc = dm_gm_get_directory(IMAGE_PATH, directory)
    !! print '("directory: ", a)', directory
    !!
    !! rc = dm_gm_get_file_format(IMAGE_PATH, format)
    !! print '("format: ", a)', format
    !!
    !! rc = dm_gm_get_mime(IMAGE_PATH, mime)
    !! print '("MIME: ", a)', mime
    !! ```
    use :: dm_error
    use :: dm_file
    use :: dm_mime
    implicit none (type, external)
    private

    integer, parameter, public :: GM_COLOR_LEN   = 16            !! Max. length of GM colour name.
    integer, parameter, public :: GM_COMMAND_LEN = FILE_PATH_LEN !! Max. length of command string.
    integer, parameter, public :: GM_FONT_LEN    = 64            !! Max. length of GM font name.
    integer, parameter, public :: GM_GRAVITY_LEN = 9             !! Max. length of GM gravity.

    ! GraphicsMagick gravity values.
    character(len=*), parameter, public :: GM_GRAVITY_E  = 'East'
    character(len=*), parameter, public :: GM_GRAVITY_N  = 'North'
    character(len=*), parameter, public :: GM_GRAVITY_NE = 'NorthEast'
    character(len=*), parameter, public :: GM_GRAVITY_NW = 'NorthWest'
    character(len=*), parameter, public :: GM_GRAVITY_S  = 'South'
    character(len=*), parameter, public :: GM_GRAVITY_SE = 'SouthEast'
    character(len=*), parameter, public :: GM_GRAVITY_SW = 'SouthWest'
    character(len=*), parameter, public :: GM_GRAVITY_W  = 'West'

    ! GraphicsMagick colour names.
    character(len=*), parameter, public :: GM_COLOR_ALICE_BLUE              = 'aliceblue'
    character(len=*), parameter, public :: GM_COLOR_ANTIQUE_WHITE           = 'antiquewhite'
    character(len=*), parameter, public :: GM_COLOR_AQUA                    = 'aqua'
    character(len=*), parameter, public :: GM_COLOR_AQUA_MARINE             = 'aquamarine'
    character(len=*), parameter, public :: GM_COLOR_AZURE                   = 'azure'
    character(len=*), parameter, public :: GM_COLOR_BEIGE                   = 'beige'
    character(len=*), parameter, public :: GM_COLOR_BISQUE                  = 'bisque'
    character(len=*), parameter, public :: GM_COLOR_BLACK                   = 'black'
    character(len=*), parameter, public :: GM_COLOR_BLANCHE_DALMOND         = 'blanchedalmond'
    character(len=*), parameter, public :: GM_COLOR_BLUE                    = 'blue'
    character(len=*), parameter, public :: GM_COLOR_BLUE_VIOLET             = 'blueviolet'
    character(len=*), parameter, public :: GM_COLOR_BROWN                   = 'brown'
    character(len=*), parameter, public :: GM_COLOR_BURLY_WOOD              = 'burlywood'
    character(len=*), parameter, public :: GM_COLOR_CADET_BLUE              = 'cadetblue'
    character(len=*), parameter, public :: GM_COLOR_CHARTREUSE              = 'chartreuse'
    character(len=*), parameter, public :: GM_COLOR_CHOCOLATE               = 'chocolate'
    character(len=*), parameter, public :: GM_COLOR_CORAL                   = 'coral'
    character(len=*), parameter, public :: GM_COLOR_CORN_FLOWER_BLUE        = 'cornflowerblue'
    character(len=*), parameter, public :: GM_COLOR_CORN_SILK               = 'cornsilk'
    character(len=*), parameter, public :: GM_COLOR_CRIMSON                 = 'crimson'
    character(len=*), parameter, public :: GM_COLOR_CYAN                    = 'cyan'
    character(len=*), parameter, public :: GM_COLOR_DARK_BLUE               = 'darkblue'
    character(len=*), parameter, public :: GM_COLOR_DARK_CYAN               = 'darkcyan'
    character(len=*), parameter, public :: GM_COLOR_DARK_GOLDEN_ROD         = 'darkgoldenrod'
    character(len=*), parameter, public :: GM_COLOR_DARK_GRAY               = 'darkgray'
    character(len=*), parameter, public :: GM_COLOR_DARK_GREEN              = 'darkgreen'
    character(len=*), parameter, public :: GM_COLOR_DARK_GREY               = 'darkgrey'
    character(len=*), parameter, public :: GM_COLOR_DARK_KHAKI              = 'darkkhaki'
    character(len=*), parameter, public :: GM_COLOR_DARK_MAGENTA            = 'darkmagenta'
    character(len=*), parameter, public :: GM_COLOR_DARK_OLIVE_GREEN        = 'darkolivegreen'
    character(len=*), parameter, public :: GM_COLOR_DARK_ORANGE             = 'darkorange'
    character(len=*), parameter, public :: GM_COLOR_DARK_ORCHID             = 'darkorchid'
    character(len=*), parameter, public :: GM_COLOR_DARK_RED                = 'darkred'
    character(len=*), parameter, public :: GM_COLOR_DARK_SALMON             = 'darksalmon'
    character(len=*), parameter, public :: GM_COLOR_DARK_SEA_GREEN          = 'darkseagreen'
    character(len=*), parameter, public :: GM_COLOR_DARK_SLATE_BLUE         = 'darkslateblue'
    character(len=*), parameter, public :: GM_COLOR_DARK_SLATE_GRAY         = 'darkslategray'
    character(len=*), parameter, public :: GM_COLOR_DARK_SLATE_GREY         = 'darkslategrey'
    character(len=*), parameter, public :: GM_COLOR_DARK_TURQUOISE          = 'darkturquoise'
    character(len=*), parameter, public :: GM_COLOR_DARK_VIOLET             = 'darkviolet'
    character(len=*), parameter, public :: GM_COLOR_DEEP_PINK               = 'deeppink'
    character(len=*), parameter, public :: GM_COLOR_DEEP_SKY_BLUE           = 'deepskyblue'
    character(len=*), parameter, public :: GM_COLOR_DIM_GRAY                = 'dimgray'
    character(len=*), parameter, public :: GM_COLOR_DIM_GREY                = 'dimgrey'
    character(len=*), parameter, public :: GM_COLOR_DODGER_BLUE             = 'dodgerblue'
    character(len=*), parameter, public :: GM_COLOR_FIRE_BRICK              = 'firebrick'
    character(len=*), parameter, public :: GM_COLOR_FLORAL_WHITE            = 'floralwhite'
    character(len=*), parameter, public :: GM_COLOR_FOREST_GREEN            = 'forestgreen'
    character(len=*), parameter, public :: GM_COLOR_FRACTAL                 = 'fractal'
    character(len=*), parameter, public :: GM_COLOR_FUCHSIA                 = 'fuchsia'
    character(len=*), parameter, public :: GM_COLOR_GAINSBORO               = 'gainsboro'
    character(len=*), parameter, public :: GM_COLOR_GHOST_WHITE             = 'ghostwhite'
    character(len=*), parameter, public :: GM_COLOR_GOLD                    = 'gold'
    character(len=*), parameter, public :: GM_COLOR_GOLDE_NROD              = 'goldenrod'
    character(len=*), parameter, public :: GM_COLOR_GRAY0                   = 'gray0'
    character(len=*), parameter, public :: GM_COLOR_GRAY1                   = 'gray1'
    character(len=*), parameter, public :: GM_COLOR_GRAY2                   = 'gray2'
    character(len=*), parameter, public :: GM_COLOR_GRAY3                   = 'gray3'
    character(len=*), parameter, public :: GM_COLOR_GRAY4                   = 'gray4'
    character(len=*), parameter, public :: GM_COLOR_GRAY5                   = 'gray5'
    character(len=*), parameter, public :: GM_COLOR_GRAY6                   = 'gray6'
    character(len=*), parameter, public :: GM_COLOR_GRAY7                   = 'gray7'
    character(len=*), parameter, public :: GM_COLOR_GRAY8                   = 'gray8'
    character(len=*), parameter, public :: GM_COLOR_GRAY9                   = 'gray9'
    character(len=*), parameter, public :: GM_COLOR_GRAY10                  = 'gray10'
    character(len=*), parameter, public :: GM_COLOR_GRAY11                  = 'gray11'
    character(len=*), parameter, public :: GM_COLOR_GRAY12                  = 'gray12'
    character(len=*), parameter, public :: GM_COLOR_GRAY13                  = 'gray13'
    character(len=*), parameter, public :: GM_COLOR_GRAY14                  = 'gray14'
    character(len=*), parameter, public :: GM_COLOR_GRAY15                  = 'gray15'
    character(len=*), parameter, public :: GM_COLOR_GRAY16                  = 'gray16'
    character(len=*), parameter, public :: GM_COLOR_GRAY17                  = 'gray17'
    character(len=*), parameter, public :: GM_COLOR_GRAY18                  = 'gray18'
    character(len=*), parameter, public :: GM_COLOR_GRAY19                  = 'gray19'
    character(len=*), parameter, public :: GM_COLOR_GRAY20                  = 'gray20'
    character(len=*), parameter, public :: GM_COLOR_GRAY21                  = 'gray21'
    character(len=*), parameter, public :: GM_COLOR_GRAY22                  = 'gray22'
    character(len=*), parameter, public :: GM_COLOR_GRAY23                  = 'gray23'
    character(len=*), parameter, public :: GM_COLOR_GRAY24                  = 'gray24'
    character(len=*), parameter, public :: GM_COLOR_GRAY25                  = 'gray25'
    character(len=*), parameter, public :: GM_COLOR_GRAY26                  = 'gray26'
    character(len=*), parameter, public :: GM_COLOR_GRAY27                  = 'gray27'
    character(len=*), parameter, public :: GM_COLOR_GRAY28                  = 'gray28'
    character(len=*), parameter, public :: GM_COLOR_GRAY29                  = 'gray29'
    character(len=*), parameter, public :: GM_COLOR_GRAY30                  = 'gray30'
    character(len=*), parameter, public :: GM_COLOR_GRAY31                  = 'gray31'
    character(len=*), parameter, public :: GM_COLOR_GRAY32                  = 'gray32'
    character(len=*), parameter, public :: GM_COLOR_GRAY33                  = 'gray33'
    character(len=*), parameter, public :: GM_COLOR_GRAY34                  = 'gray34'
    character(len=*), parameter, public :: GM_COLOR_GRAY35                  = 'gray35'
    character(len=*), parameter, public :: GM_COLOR_GRAY36                  = 'gray36'
    character(len=*), parameter, public :: GM_COLOR_GRAY37                  = 'gray37'
    character(len=*), parameter, public :: GM_COLOR_GRAY38                  = 'gray38'
    character(len=*), parameter, public :: GM_COLOR_GRAY39                  = 'gray39'
    character(len=*), parameter, public :: GM_COLOR_GRAY40                  = 'gray40'
    character(len=*), parameter, public :: GM_COLOR_GRAY41                  = 'gray41'
    character(len=*), parameter, public :: GM_COLOR_GRAY42                  = 'gray42'
    character(len=*), parameter, public :: GM_COLOR_GRAY43                  = 'gray43'
    character(len=*), parameter, public :: GM_COLOR_GRAY44                  = 'gray44'
    character(len=*), parameter, public :: GM_COLOR_GRAY45                  = 'gray45'
    character(len=*), parameter, public :: GM_COLOR_GRAY46                  = 'gray46'
    character(len=*), parameter, public :: GM_COLOR_GRAY47                  = 'gray47'
    character(len=*), parameter, public :: GM_COLOR_GRAY48                  = 'gray48'
    character(len=*), parameter, public :: GM_COLOR_GRAY49                  = 'gray49'
    character(len=*), parameter, public :: GM_COLOR_GRAY50                  = 'gray50'
    character(len=*), parameter, public :: GM_COLOR_GRAY51                  = 'gray51'
    character(len=*), parameter, public :: GM_COLOR_GRAY52                  = 'gray52'
    character(len=*), parameter, public :: GM_COLOR_GRAY53                  = 'gray53'
    character(len=*), parameter, public :: GM_COLOR_GRAY54                  = 'gray54'
    character(len=*), parameter, public :: GM_COLOR_GRAY55                  = 'gray55'
    character(len=*), parameter, public :: GM_COLOR_GRAY56                  = 'gray56'
    character(len=*), parameter, public :: GM_COLOR_GRAY57                  = 'gray57'
    character(len=*), parameter, public :: GM_COLOR_GRAY58                  = 'gray58'
    character(len=*), parameter, public :: GM_COLOR_GRAY59                  = 'gray59'
    character(len=*), parameter, public :: GM_COLOR_GRAY60                  = 'gray60'
    character(len=*), parameter, public :: GM_COLOR_GRAY61                  = 'gray61'
    character(len=*), parameter, public :: GM_COLOR_GRAY62                  = 'gray62'
    character(len=*), parameter, public :: GM_COLOR_GRAY63                  = 'gray63'
    character(len=*), parameter, public :: GM_COLOR_GRAY64                  = 'gray64'
    character(len=*), parameter, public :: GM_COLOR_GRAY65                  = 'gray65'
    character(len=*), parameter, public :: GM_COLOR_GRAY66                  = 'gray66'
    character(len=*), parameter, public :: GM_COLOR_GRAY67                  = 'gray67'
    character(len=*), parameter, public :: GM_COLOR_GRAY68                  = 'gray68'
    character(len=*), parameter, public :: GM_COLOR_GRAY69                  = 'gray69'
    character(len=*), parameter, public :: GM_COLOR_GRAY70                  = 'gray70'
    character(len=*), parameter, public :: GM_COLOR_GRAY71                  = 'gray71'
    character(len=*), parameter, public :: GM_COLOR_GRAY72                  = 'gray72'
    character(len=*), parameter, public :: GM_COLOR_GRAY73                  = 'gray73'
    character(len=*), parameter, public :: GM_COLOR_GRAY74                  = 'gray74'
    character(len=*), parameter, public :: GM_COLOR_GRAY75                  = 'gray75'
    character(len=*), parameter, public :: GM_COLOR_GRAY76                  = 'gray76'
    character(len=*), parameter, public :: GM_COLOR_GRAY77                  = 'gray77'
    character(len=*), parameter, public :: GM_COLOR_GRAY78                  = 'gray78'
    character(len=*), parameter, public :: GM_COLOR_GRAY79                  = 'gray79'
    character(len=*), parameter, public :: GM_COLOR_GRAY80                  = 'gray80'
    character(len=*), parameter, public :: GM_COLOR_GRAY81                  = 'gray81'
    character(len=*), parameter, public :: GM_COLOR_GRAY82                  = 'gray82'
    character(len=*), parameter, public :: GM_COLOR_GRAY83                  = 'gray83'
    character(len=*), parameter, public :: GM_COLOR_GRAY84                  = 'gray84'
    character(len=*), parameter, public :: GM_COLOR_GRAY85                  = 'gray85'
    character(len=*), parameter, public :: GM_COLOR_GRAY86                  = 'gray86'
    character(len=*), parameter, public :: GM_COLOR_GRAY87                  = 'gray87'
    character(len=*), parameter, public :: GM_COLOR_GRAY88                  = 'gray88'
    character(len=*), parameter, public :: GM_COLOR_GRAY89                  = 'gray89'
    character(len=*), parameter, public :: GM_COLOR_GRAY90                  = 'gray90'
    character(len=*), parameter, public :: GM_COLOR_GRAY91                  = 'gray91'
    character(len=*), parameter, public :: GM_COLOR_GRAY92                  = 'gray92'
    character(len=*), parameter, public :: GM_COLOR_GRAY93                  = 'gray93'
    character(len=*), parameter, public :: GM_COLOR_GRAY94                  = 'gray94'
    character(len=*), parameter, public :: GM_COLOR_GRAY95                  = 'gray95'
    character(len=*), parameter, public :: GM_COLOR_GRAY96                  = 'gray96'
    character(len=*), parameter, public :: GM_COLOR_GRAY97                  = 'gray97'
    character(len=*), parameter, public :: GM_COLOR_GRAY98                  = 'gray98'
    character(len=*), parameter, public :: GM_COLOR_GRAY99                  = 'gray99'
    character(len=*), parameter, public :: GM_COLOR_GRAY100                 = 'gray100'
    character(len=*), parameter, public :: GM_COLOR_GRAY                    = 'gray'
    character(len=*), parameter, public :: GM_COLOR_GREEN                   = 'green'
    character(len=*), parameter, public :: GM_COLOR_GREEN_YELLOW            = 'greenyellow'
    character(len=*), parameter, public :: GM_COLOR_GREY                    = 'grey'
    character(len=*), parameter, public :: GM_COLOR_HONEY_DEW               = 'honeydew'
    character(len=*), parameter, public :: GM_COLOR_HOT_PINK                = 'hotpink'
    character(len=*), parameter, public :: GM_COLOR_INDIAN_RED              = 'indianred'
    character(len=*), parameter, public :: GM_COLOR_INDIGO                  = 'indigo'
    character(len=*), parameter, public :: GM_COLOR_IVORY                   = 'ivory'
    character(len=*), parameter, public :: GM_COLOR_KHAKI                   = 'khaki'
    character(len=*), parameter, public :: GM_COLOR_LAVENDER                = 'lavender'
    character(len=*), parameter, public :: GM_COLOR_LAVENDER_BLUSH          = 'lavenderblush'
    character(len=*), parameter, public :: GM_COLOR_LAWN_GREEN              = 'lawngreen'
    character(len=*), parameter, public :: GM_COLOR_LEMON_CHIFFON           = 'lemonchiffon'
    character(len=*), parameter, public :: GM_COLOR_LIGHT_BLUE              = 'lightblue'
    character(len=*), parameter, public :: GM_COLOR_LIGHT_CORAL             = 'lightcoral'
    character(len=*), parameter, public :: GM_COLOR_LIGHT_CYAN              = 'lightcyan'
    character(len=*), parameter, public :: GM_COLOR_LIGHT_GOLDEN_ROD_YELLOW = 'lightgoldenrodyellow'
    character(len=*), parameter, public :: GM_COLOR_LIGHT_GRAY              = 'lightgray'
    character(len=*), parameter, public :: GM_COLOR_LIGHT_GREEN             = 'lightgreen'
    character(len=*), parameter, public :: GM_COLOR_LIGHT_GREY              = 'lightgrey'
    character(len=*), parameter, public :: GM_COLOR_LIGHT_PINK              = 'lightpink'
    character(len=*), parameter, public :: GM_COLOR_LIGHT_SALMON            = 'lightsalmon'
    character(len=*), parameter, public :: GM_COLOR_LIGHT_SEA_GREEN         = 'lightseagreen'
    character(len=*), parameter, public :: GM_COLOR_LIGHT_SKY_BLUE          = 'lightskyblue'
    character(len=*), parameter, public :: GM_COLOR_LIGHT_SLATE_GRAY        = 'lightslategray'
    character(len=*), parameter, public :: GM_COLOR_LIGHT_SLATE_GREY        = 'lightslategrey'
    character(len=*), parameter, public :: GM_COLOR_LIGHT_STEEL_BLUE        = 'lightsteelblue'
    character(len=*), parameter, public :: GM_COLOR_LIGHT_YELLOW            = 'lightyellow'
    character(len=*), parameter, public :: GM_COLOR_LIME                    = 'lime'
    character(len=*), parameter, public :: GM_COLOR_LIME_GREEN              = 'limegreen'
    character(len=*), parameter, public :: GM_COLOR_LINEN                   = 'linen'
    character(len=*), parameter, public :: GM_COLOR_MAGENTA                 = 'magenta'
    character(len=*), parameter, public :: GM_COLOR_MAROON                  = 'maroon'
    character(len=*), parameter, public :: GM_COLOR_MEDIUM_AQUA_MARINE      = 'mediumaquamarine'
    character(len=*), parameter, public :: GM_COLOR_MEDIUM_BLUE             = 'mediumblue'
    character(len=*), parameter, public :: GM_COLOR_MEDIUM_ORCHID           = 'mediumorchid'
    character(len=*), parameter, public :: GM_COLOR_MEDIUM_PURPLE           = 'mediumpurple'
    character(len=*), parameter, public :: GM_COLOR_MEDIUM_SEA_GREEN        = 'mediumseagreen'
    character(len=*), parameter, public :: GM_COLOR_MEDIUM_SLATE_BLUE       = 'mediumslateblue'
    character(len=*), parameter, public :: GM_COLOR_MEDIUM_SPRING_GREEN     = 'mediumspringgreen'
    character(len=*), parameter, public :: GM_COLOR_MEDIUM_TURQUOISE        = 'mediumturquoise'
    character(len=*), parameter, public :: GM_COLOR_MEDIUM_VIOLET_RED       = 'mediumvioletred'
    character(len=*), parameter, public :: GM_COLOR_MIDNIGHT_BLUE           = 'midnightblue'
    character(len=*), parameter, public :: GM_COLOR_MINT_CREAM              = 'mintcream'
    character(len=*), parameter, public :: GM_COLOR_MISTY_ROSE              = 'mistyrose'
    character(len=*), parameter, public :: GM_COLOR_MOCCASIN                = 'moccasin'
    character(len=*), parameter, public :: GM_COLOR_NAVAJO_WHITE            = 'navajowhite'
    character(len=*), parameter, public :: GM_COLOR_NAVY                    = 'navy'
    character(len=*), parameter, public :: GM_COLOR_NONE                    = 'none'
    character(len=*), parameter, public :: GM_COLOR_OLD_LACE                = 'oldlace'
    character(len=*), parameter, public :: GM_COLOR_OLIVE                   = 'olive'
    character(len=*), parameter, public :: GM_COLOR_OLIVE_DRAB              = 'olivedrab'
    character(len=*), parameter, public :: GM_COLOR_ORANGE                  = 'orange'
    character(len=*), parameter, public :: GM_COLOR_ORANGE_RED              = 'orangered'
    character(len=*), parameter, public :: GM_COLOR_ORCHID                  = 'orchid'
    character(len=*), parameter, public :: GM_COLOR_PALE_GOLDEN_ROD         = 'palegoldenrod'
    character(len=*), parameter, public :: GM_COLOR_PALE_GREEN              = 'palegreen'
    character(len=*), parameter, public :: GM_COLOR_PALE_TURQUOISE          = 'paleturquoise'
    character(len=*), parameter, public :: GM_COLOR_PALE_VIOLET_RED         = 'palevioletred'
    character(len=*), parameter, public :: GM_COLOR_PAPAYA_WHIP             = 'papayawhip'
    character(len=*), parameter, public :: GM_COLOR_PEACH_PUFF              = 'peachpuff'
    character(len=*), parameter, public :: GM_COLOR_PERU                    = 'peru'
    character(len=*), parameter, public :: GM_COLOR_PINK                    = 'pink'
    character(len=*), parameter, public :: GM_COLOR_PLUM                    = 'plum'
    character(len=*), parameter, public :: GM_COLOR_POWDER_BLUE             = 'powderblue'
    character(len=*), parameter, public :: GM_COLOR_PURPLE                  = 'purple'
    character(len=*), parameter, public :: GM_COLOR_RED                     = 'red'
    character(len=*), parameter, public :: GM_COLOR_ROSY_BROWN              = 'rosybrown'
    character(len=*), parameter, public :: GM_COLOR_ROYAL_BLUE              = 'royalblue'
    character(len=*), parameter, public :: GM_COLOR_SADDLE_BROWN            = 'saddlebrown'
    character(len=*), parameter, public :: GM_COLOR_SALMON                  = 'salmon'
    character(len=*), parameter, public :: GM_COLOR_SANDY_BROWN             = 'sandybrown'
    character(len=*), parameter, public :: GM_COLOR_SEA_GREEN               = 'seagreen'
    character(len=*), parameter, public :: GM_COLOR_SEA_SHELL               = 'seashell'
    character(len=*), parameter, public :: GM_COLOR_SIENNA                  = 'sienna'
    character(len=*), parameter, public :: GM_COLOR_SILVER                  = 'silver'
    character(len=*), parameter, public :: GM_COLOR_SKY_BLUE                = 'skyblue'
    character(len=*), parameter, public :: GM_COLOR_SLATE_BLUE              = 'slateblue'
    character(len=*), parameter, public :: GM_COLOR_SLATE_GRAY              = 'slategray'
    character(len=*), parameter, public :: GM_COLOR_SLATE_GREY              = 'slategrey'
    character(len=*), parameter, public :: GM_COLOR_SNOW                    = 'snow'
    character(len=*), parameter, public :: GM_COLOR_SPRING_GREEN            = 'springgreen'
    character(len=*), parameter, public :: GM_COLOR_STEEL_BLUE              = 'steelblue'
    character(len=*), parameter, public :: GM_COLOR_TAN                     = 'tan'
    character(len=*), parameter, public :: GM_COLOR_TEAL                    = 'teal'
    character(len=*), parameter, public :: GM_COLOR_THISTLE                 = 'thistle'
    character(len=*), parameter, public :: GM_COLOR_TOMATO                  = 'tomato'
    character(len=*), parameter, public :: GM_COLOR_TURQUOISE               = 'turquoise'
    character(len=*), parameter, public :: GM_COLOR_VIOLET                  = 'violet'
    character(len=*), parameter, public :: GM_COLOR_WHEAT                   = 'wheat'
    character(len=*), parameter, public :: GM_COLOR_WHITE                   = 'white'
    character(len=*), parameter, public :: GM_COLOR_WHITE_SMOKE             = 'whitesmoke'
    character(len=*), parameter, public :: GM_COLOR_YELLOW                  = 'yellow'
    character(len=*), parameter, public :: GM_COLOR_YELLOW_GREEN            = 'yellowgreen'

    character(len=*), parameter :: GM_BINARY = 'gm' !! GraphicsMagick binary name.

    type, public :: gm_text_box_type
        !! Text box settings for drawing text onto camera frame image.
        !!
        !! You can tag a font to specify whether it is a PostScript, TrueType,
        !! or X11 font. For example, `Arial.ttf` is a TrueType font,
        !! `ps:helvetica` is PostScript, and `x:fixed` is X11.
        character(len=GM_GRAVITY_LEN) :: gravity    = GM_GRAVITY_SW    !! Text position (GM).
        character(len=GM_COLOR_LEN)   :: background = GM_COLOR_BLACK   !! Box colour (GM).
        character(len=GM_COLOR_LEN)   :: foreground = GM_COLOR_WHITE   !! Text colour (GM).
        character(len=GM_FONT_LEN)    :: font       = 'Arial'          !! Font name (GM).
        integer                       :: font_size  = 12               !! Font size in points.
    end type gm_text_box_type

    public :: dm_gm_add_text_box
    public :: dm_gm_get_dimensions
    public :: dm_gm_get_directory
    public :: dm_gm_get_file_extension
    public :: dm_gm_get_file_format
    public :: dm_gm_get_mime

    private :: gm_identify
    private :: gm_prepare_add_text_box
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_gm_add_text_box(path, text, text_box, command) result(rc)
        !! Draws text camera image file, using GraphicsMagick. By default, the
        !! text box is drawn to the bottom-left corner of the image.
        !!
        !! For a list of all supported font names, run:
        !!
        !! ```
        !! $ gm convert -list font
        !! Path: /usr/local/lib/GraphicsMagick/config/type-windows.mgk
        !!
        !! Name                             Family                  Style   Stretch  Weight
        !! --------------------------------------------------------------------------------
        !! Arial                            Arial                   normal  normal    400
        !! Arial-Black                      Arial                   normal  normal    900
        !! Arial-Bold                       Arial                   normal  normal    700
        !! Arial-Bold-Italic                Arial                   italic  normal    700
        !! Arial-Italic                     Arial                   italic  normal    400
        !! ...
        !! ```
        !!
        !! If no text box is passed, the default values of the derived type are
        !! used.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if text or image path are empty.
        !! * `E_IO` if GraphicsMagick command execution failed.
        !! * `E_NOT_FOUND` if image at given path does no exist.
        !!
        character(len=*),              intent(in)            :: path     !! Image file path.
        character(len=*),              intent(in)            :: text     !! Text to add.
        type(gm_text_box_type),        intent(in),  optional :: text_box !! Image text box type.
        character(len=:), allocatable, intent(out), optional :: command  !! Executed command.

        character(len=GM_COMMAND_LEN) :: command_
        integer                       :: stat

        command_ = ' '

        io_block: block
            rc = E_EMPTY
            if (len_trim(path) == 0 .or. len_trim(text) == 0) return

            rc = E_NOT_FOUND
            if (.not. dm_file_exists(path)) return

            rc = E_IO
            call gm_prepare_add_text_box(command_, path, text, text_box)
            call execute_command_line(trim(command_), exitstat=stat)
            if (stat /= 0) return

            rc = E_NONE
        end block io_block

        if (present(command)) command = trim(command_)
    end function dm_gm_add_text_box

    integer function dm_gm_get_dimensions(path, width, height) result(rc)
        !! Uses GraphicsMagick to determine the dimensions of the image at
        !! given path. On error, width and height are 0.
        !!
        !! The function returns the followin error codes:
        !!
        !! * `E_NOT_FOUND` if image does not exist.
        !! * `E_READ` if reading dimensions failed.
        !! * `E_SYSTEM` if execution of GraphicsMagick failed.
        !!
        character(len=*), intent(in)  :: path   !! Image file path.
        integer,          intent(out) :: width  !! Image width.
        integer,          intent(out) :: height !! Image height.

        character(len=32) :: buffer
        integer           :: stat

        width  = 0
        height = 0

        rc = gm_identify(path, '%w %h', buffer)
        if (dm_is_error(rc)) return

        read (buffer, *, iostat=stat) width, height
        if (stat /= 0) return

        rc = E_NONE
    end function dm_gm_get_dimensions

    integer function dm_gm_get_directory(path, directory) result(rc)
        !! Uses GraphicsMagick to return the directory part of the image path.
        !! On error, the string `directory` is allocated but empty.
        !!
        !! The function returns the followin error codes:
        !!
        !! * `E_NOT_FOUND` if image does not exist.
        !! * `E_READ` if reading dimensions failed.
        !! * `E_SYSTEM` if execution of GraphicsMagick failed.
        !!
        character(len=*),              intent(in)  :: path      !! Image file path.
        character(len=:), allocatable, intent(out) :: directory !! Image file directory.

        character(len=FILE_PATH_LEN) :: buffer

        rc = gm_identify(path, '%d', buffer)
        directory = trim(buffer)
    end function dm_gm_get_directory

    integer function dm_gm_get_file_extension(path, extension) result(rc)
        !! Uses GraphicsMagick to read the image file extension (`jpg`,
        !! `png`, ...). On error, the string `file_format` is allocated but
        !! empty.
        !!
        !! The function returns the followin error codes:
        !!
        !! * `E_NOT_FOUND` if image does not exist.
        !! * `E_READ` if reading dimensions failed.
        !! * `E_SYSTEM` if execution of GraphicsMagick failed.
        !!
        character(len=*),              intent(in)  :: path      !! Image file path.
        character(len=:), allocatable, intent(out) :: extension !! Image file extension.

        character(len=8) :: buffer

        rc = gm_identify(path, '%e', buffer)
        extension = trim(buffer)
    end function dm_gm_get_file_extension

    integer function dm_gm_get_file_format(path, file_format) result(rc)
        !! Uses GraphicsMagick to determine the image file format (`JPEG`,
        !! `PNG`, ...). On error, the string `file_format` is allocated but
        !! empty.
        !!
        !! The function returns the followin error codes:
        !!
        !! * `E_NOT_FOUND` if image does not exist.
        !! * `E_READ` if reading dimensions failed.
        !! * `E_SYSTEM` if execution of GraphicsMagick failed.
        !!
        character(len=*),              intent(in)  :: path        !! Image file path.
        character(len=:), allocatable, intent(out) :: file_format !! Image file format.

        character(len=32) :: buffer

        rc = gm_identify(path, '%m', buffer)
        file_format = trim(buffer)
    end function dm_gm_get_file_format

    integer function dm_gm_get_mime(path, mime) result(rc)
        !! Determines the MIME type of the image through file format. The
        !! following file formats are recognised: GIF, JPEG, PNG, SVG. On
        !! error, the string `mime` is allocated but empty.
        !!
        !! The function returns the followin error codes:
        !!
        !! * `E_NOT_FOUND` if image does not exist.
        !! * `E_READ` if reading dimensions failed.
        !! * `E_SYSTEM` if execution of GraphicsMagick failed.
        !!
        character(len=*),              intent(in)  :: path !! Image file path.
        character(len=:), allocatable, intent(out) :: mime !! MIME type.

        character(len=:), allocatable :: file_format

        rc = dm_gm_get_file_format(path, file_format)

        select case (file_format)
            case ('GIF');  mime = MIME_GIF
            case ('JPEG'); mime = MIME_JPEG
            case ('PNG');  mime = MIME_PNG
            case ('SVG');  mime = MIME_SVG
            case default;  mime = ''
        end select
    end function dm_gm_get_mime

    ! **************************************************************************
    ! PRIVATE PROCEDURES.
    ! **************************************************************************
    integer function gm_identify(path, format, output) result(rc)
        !! Identifies image with GraphicsMagick and returns result in `output`.
        use :: dm_kind
        use :: dm_pipe

        character(len=*), intent(in)    :: path   !! Image file path.
        character(len=*), intent(in)    :: format !! GraphicsMagick identify format attributes.
        character(len=*), intent(inout) :: output !! Output string.

        integer(kind=i8) :: n
        type(pipe_type)  :: pipe

        output = ' '

        rc = E_INVALID
        if (len(output) == 0) return

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(path)) return

        rc = dm_pipe_open(pipe, 'gm identify -format "' // trim(format) // '" ' // trim(path), PIPE_RDONLY)
        if (dm_is_error(rc)) return

        rc = E_READ
        n  = dm_pipe_read(pipe, output)
        call dm_pipe_close(pipe)

        ! Remove null character.
        if (n == 0) then
            output(1:1) = ' '
            return
        else
            output(n:n) = ' '
            rc = E_NONE
        end if
    end function gm_identify

    subroutine gm_prepare_add_text_box(command, path, text, text_box)
        !! Prepares GraphicsMagick command to add text to image. The string
        !! `text` must not contain the quote characters `'` and `"`.
        character(len=GM_COMMAND_LEN), intent(out)          :: command  !! Prepared command string.
        character(len=*),              intent(in)           :: path     !! Image file path.
        character(len=*),              intent(in)           :: text     !! Text to add.
        type(gm_text_box_type),        intent(in), optional :: text_box !! Image text box type.

        character(len=32)         :: point_size
        type(gm_text_box_type) :: box

        if (present(text_box)) box = text_box

        write (command, '(" -gravity ", a, " -box ", a, " -fill ", a, " -draw ''text 0,0 """, a, """''", 2(1x, a))') &
            trim(box%gravity), trim(box%background), trim(box%foreground), trim(text), trim(path), path

        if (box%font_size > 0) then
            write (point_size, '(" -pointsize ", i0)') box%font_size
            command = trim(point_size) // command
        end if

        if (len_trim(box%font) > 0) then
            command = ' -font ' // trim(box%font) // command
        end if

        command = GM_BINARY // ' convert' // trim(command)
    end subroutine gm_prepare_add_text_box
end module dm_gm
