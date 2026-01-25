! Author:  Philipp Engel
! Licence: ISC
module dm_gm
    !! Abstraction layer over GraphicsMagick. On Linux, install the packages
    !! `graphicsmagick` and `gsfonts`:
    !!
    !! ```
    !! $ sudo apt-get install graphicsmagick gsfonts
    !! ```
    !!
    !! For a list of all fonts supported by GraphicsMagick, run:
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
    !! Edit `/usr/local/lib/GraphicsMagick/config/type.mgk` to set a custom
    !! font configuration. Use the Perl script `imagick_type_gen.pl` to
    !! generate a type file `type-custom.mgk`, for example:
    !!
    !! ```
    !! $ find /usr/local/share/fonts/ -type f -name "*.*" | perl ./imagick_type_gen.pl -f - > type-custom.mgk
    !! ```
    !!
    !! Copy the type file to the `config/` directory of GraphicsMagick and
    !! modify the path to the include file in `type.mgk` accordingly or replace
    !! `type.mgk` with `type-custom.mgk` altogether.
    !!
    !! Example to read meta data of image `/tmp/image.jpg`:
    !!
    !! ``` fortran
    !! character(*), parameter :: IMAGE_PATH = '/tmp/image.jpg'
    !!
    !! character(:), allocatable :: directory, format, mime
    !! integer                   :: width, height
    !! integer                   :: rc
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
    use :: dm_kind
    implicit none (type, external)
    private

    integer, parameter, public :: GM_COLOR_LEN   = 16            !! Max. length of GM colour name.
    integer, parameter, public :: GM_COMMAND_LEN = FILE_PATH_LEN !! Max. length of command string.
    integer, parameter, public :: GM_FONT_LEN    = 64            !! Max. length of GM font name.
    integer, parameter, public :: GM_GRAVITY_LEN = 9             !! Max. length of GM gravity.

    ! GraphicsMagick gravity values.
    character(*), parameter, public :: GM_GRAVITY_E  = 'East'
    character(*), parameter, public :: GM_GRAVITY_N  = 'North'
    character(*), parameter, public :: GM_GRAVITY_NE = 'NorthEast'
    character(*), parameter, public :: GM_GRAVITY_NW = 'NorthWest'
    character(*), parameter, public :: GM_GRAVITY_S  = 'South'
    character(*), parameter, public :: GM_GRAVITY_SE = 'SouthEast'
    character(*), parameter, public :: GM_GRAVITY_SW = 'SouthWest'
    character(*), parameter, public :: GM_GRAVITY_W  = 'West'

    ! GraphicsMagick colour names.
    character(*), parameter, public :: GM_COLOR_ALICE_BLUE              = 'aliceblue'
    character(*), parameter, public :: GM_COLOR_ANTIQUE_WHITE           = 'antiquewhite'
    character(*), parameter, public :: GM_COLOR_AQUA                    = 'aqua'
    character(*), parameter, public :: GM_COLOR_AQUA_MARINE             = 'aquamarine'
    character(*), parameter, public :: GM_COLOR_AZURE                   = 'azure'
    character(*), parameter, public :: GM_COLOR_BEIGE                   = 'beige'
    character(*), parameter, public :: GM_COLOR_BISQUE                  = 'bisque'
    character(*), parameter, public :: GM_COLOR_BLACK                   = 'black'
    character(*), parameter, public :: GM_COLOR_BLANCHE_DALMOND         = 'blanchedalmond'
    character(*), parameter, public :: GM_COLOR_BLUE                    = 'blue'
    character(*), parameter, public :: GM_COLOR_BLUE_VIOLET             = 'blueviolet'
    character(*), parameter, public :: GM_COLOR_BROWN                   = 'brown'
    character(*), parameter, public :: GM_COLOR_BURLY_WOOD              = 'burlywood'
    character(*), parameter, public :: GM_COLOR_CADET_BLUE              = 'cadetblue'
    character(*), parameter, public :: GM_COLOR_CHARTREUSE              = 'chartreuse'
    character(*), parameter, public :: GM_COLOR_CHOCOLATE               = 'chocolate'
    character(*), parameter, public :: GM_COLOR_CORAL                   = 'coral'
    character(*), parameter, public :: GM_COLOR_CORN_FLOWER_BLUE        = 'cornflowerblue'
    character(*), parameter, public :: GM_COLOR_CORN_SILK               = 'cornsilk'
    character(*), parameter, public :: GM_COLOR_CRIMSON                 = 'crimson'
    character(*), parameter, public :: GM_COLOR_CYAN                    = 'cyan'
    character(*), parameter, public :: GM_COLOR_DARK_BLUE               = 'darkblue'
    character(*), parameter, public :: GM_COLOR_DARK_CYAN               = 'darkcyan'
    character(*), parameter, public :: GM_COLOR_DARK_GOLDEN_ROD         = 'darkgoldenrod'
    character(*), parameter, public :: GM_COLOR_DARK_GRAY               = 'darkgray'
    character(*), parameter, public :: GM_COLOR_DARK_GREEN              = 'darkgreen'
    character(*), parameter, public :: GM_COLOR_DARK_GREY               = 'darkgrey'
    character(*), parameter, public :: GM_COLOR_DARK_KHAKI              = 'darkkhaki'
    character(*), parameter, public :: GM_COLOR_DARK_MAGENTA            = 'darkmagenta'
    character(*), parameter, public :: GM_COLOR_DARK_OLIVE_GREEN        = 'darkolivegreen'
    character(*), parameter, public :: GM_COLOR_DARK_ORANGE             = 'darkorange'
    character(*), parameter, public :: GM_COLOR_DARK_ORCHID             = 'darkorchid'
    character(*), parameter, public :: GM_COLOR_DARK_RED                = 'darkred'
    character(*), parameter, public :: GM_COLOR_DARK_SALMON             = 'darksalmon'
    character(*), parameter, public :: GM_COLOR_DARK_SEA_GREEN          = 'darkseagreen'
    character(*), parameter, public :: GM_COLOR_DARK_SLATE_BLUE         = 'darkslateblue'
    character(*), parameter, public :: GM_COLOR_DARK_SLATE_GRAY         = 'darkslategray'
    character(*), parameter, public :: GM_COLOR_DARK_SLATE_GREY         = 'darkslategrey'
    character(*), parameter, public :: GM_COLOR_DARK_TURQUOISE          = 'darkturquoise'
    character(*), parameter, public :: GM_COLOR_DARK_VIOLET             = 'darkviolet'
    character(*), parameter, public :: GM_COLOR_DEEP_PINK               = 'deeppink'
    character(*), parameter, public :: GM_COLOR_DEEP_SKY_BLUE           = 'deepskyblue'
    character(*), parameter, public :: GM_COLOR_DIM_GRAY                = 'dimgray'
    character(*), parameter, public :: GM_COLOR_DIM_GREY                = 'dimgrey'
    character(*), parameter, public :: GM_COLOR_DODGER_BLUE             = 'dodgerblue'
    character(*), parameter, public :: GM_COLOR_FIRE_BRICK              = 'firebrick'
    character(*), parameter, public :: GM_COLOR_FLORAL_WHITE            = 'floralwhite'
    character(*), parameter, public :: GM_COLOR_FOREST_GREEN            = 'forestgreen'
    character(*), parameter, public :: GM_COLOR_FRACTAL                 = 'fractal'
    character(*), parameter, public :: GM_COLOR_FUCHSIA                 = 'fuchsia'
    character(*), parameter, public :: GM_COLOR_GAINSBORO               = 'gainsboro'
    character(*), parameter, public :: GM_COLOR_GHOST_WHITE             = 'ghostwhite'
    character(*), parameter, public :: GM_COLOR_GOLD                    = 'gold'
    character(*), parameter, public :: GM_COLOR_GOLDEN_ROD              = 'goldenrod'
    character(*), parameter, public :: GM_COLOR_GRAY                    = 'gray'
    character(*), parameter, public :: GM_COLOR_GRAY0                   = 'gray0'
    character(*), parameter, public :: GM_COLOR_GRAY1                   = 'gray1'
    character(*), parameter, public :: GM_COLOR_GRAY2                   = 'gray2'
    character(*), parameter, public :: GM_COLOR_GRAY3                   = 'gray3'
    character(*), parameter, public :: GM_COLOR_GRAY4                   = 'gray4'
    character(*), parameter, public :: GM_COLOR_GRAY5                   = 'gray5'
    character(*), parameter, public :: GM_COLOR_GRAY6                   = 'gray6'
    character(*), parameter, public :: GM_COLOR_GRAY7                   = 'gray7'
    character(*), parameter, public :: GM_COLOR_GRAY8                   = 'gray8'
    character(*), parameter, public :: GM_COLOR_GRAY9                   = 'gray9'
    character(*), parameter, public :: GM_COLOR_GRAY10                  = 'gray10'
    character(*), parameter, public :: GM_COLOR_GRAY11                  = 'gray11'
    character(*), parameter, public :: GM_COLOR_GRAY12                  = 'gray12'
    character(*), parameter, public :: GM_COLOR_GRAY13                  = 'gray13'
    character(*), parameter, public :: GM_COLOR_GRAY14                  = 'gray14'
    character(*), parameter, public :: GM_COLOR_GRAY15                  = 'gray15'
    character(*), parameter, public :: GM_COLOR_GRAY16                  = 'gray16'
    character(*), parameter, public :: GM_COLOR_GRAY17                  = 'gray17'
    character(*), parameter, public :: GM_COLOR_GRAY18                  = 'gray18'
    character(*), parameter, public :: GM_COLOR_GRAY19                  = 'gray19'
    character(*), parameter, public :: GM_COLOR_GRAY20                  = 'gray20'
    character(*), parameter, public :: GM_COLOR_GRAY21                  = 'gray21'
    character(*), parameter, public :: GM_COLOR_GRAY22                  = 'gray22'
    character(*), parameter, public :: GM_COLOR_GRAY23                  = 'gray23'
    character(*), parameter, public :: GM_COLOR_GRAY24                  = 'gray24'
    character(*), parameter, public :: GM_COLOR_GRAY25                  = 'gray25'
    character(*), parameter, public :: GM_COLOR_GRAY26                  = 'gray26'
    character(*), parameter, public :: GM_COLOR_GRAY27                  = 'gray27'
    character(*), parameter, public :: GM_COLOR_GRAY28                  = 'gray28'
    character(*), parameter, public :: GM_COLOR_GRAY29                  = 'gray29'
    character(*), parameter, public :: GM_COLOR_GRAY30                  = 'gray30'
    character(*), parameter, public :: GM_COLOR_GRAY31                  = 'gray31'
    character(*), parameter, public :: GM_COLOR_GRAY32                  = 'gray32'
    character(*), parameter, public :: GM_COLOR_GRAY33                  = 'gray33'
    character(*), parameter, public :: GM_COLOR_GRAY34                  = 'gray34'
    character(*), parameter, public :: GM_COLOR_GRAY35                  = 'gray35'
    character(*), parameter, public :: GM_COLOR_GRAY36                  = 'gray36'
    character(*), parameter, public :: GM_COLOR_GRAY37                  = 'gray37'
    character(*), parameter, public :: GM_COLOR_GRAY38                  = 'gray38'
    character(*), parameter, public :: GM_COLOR_GRAY39                  = 'gray39'
    character(*), parameter, public :: GM_COLOR_GRAY40                  = 'gray40'
    character(*), parameter, public :: GM_COLOR_GRAY41                  = 'gray41'
    character(*), parameter, public :: GM_COLOR_GRAY42                  = 'gray42'
    character(*), parameter, public :: GM_COLOR_GRAY43                  = 'gray43'
    character(*), parameter, public :: GM_COLOR_GRAY44                  = 'gray44'
    character(*), parameter, public :: GM_COLOR_GRAY45                  = 'gray45'
    character(*), parameter, public :: GM_COLOR_GRAY46                  = 'gray46'
    character(*), parameter, public :: GM_COLOR_GRAY47                  = 'gray47'
    character(*), parameter, public :: GM_COLOR_GRAY48                  = 'gray48'
    character(*), parameter, public :: GM_COLOR_GRAY49                  = 'gray49'
    character(*), parameter, public :: GM_COLOR_GRAY50                  = 'gray50'
    character(*), parameter, public :: GM_COLOR_GRAY51                  = 'gray51'
    character(*), parameter, public :: GM_COLOR_GRAY52                  = 'gray52'
    character(*), parameter, public :: GM_COLOR_GRAY53                  = 'gray53'
    character(*), parameter, public :: GM_COLOR_GRAY54                  = 'gray54'
    character(*), parameter, public :: GM_COLOR_GRAY55                  = 'gray55'
    character(*), parameter, public :: GM_COLOR_GRAY56                  = 'gray56'
    character(*), parameter, public :: GM_COLOR_GRAY57                  = 'gray57'
    character(*), parameter, public :: GM_COLOR_GRAY58                  = 'gray58'
    character(*), parameter, public :: GM_COLOR_GRAY59                  = 'gray59'
    character(*), parameter, public :: GM_COLOR_GRAY60                  = 'gray60'
    character(*), parameter, public :: GM_COLOR_GRAY61                  = 'gray61'
    character(*), parameter, public :: GM_COLOR_GRAY62                  = 'gray62'
    character(*), parameter, public :: GM_COLOR_GRAY63                  = 'gray63'
    character(*), parameter, public :: GM_COLOR_GRAY64                  = 'gray64'
    character(*), parameter, public :: GM_COLOR_GRAY65                  = 'gray65'
    character(*), parameter, public :: GM_COLOR_GRAY66                  = 'gray66'
    character(*), parameter, public :: GM_COLOR_GRAY67                  = 'gray67'
    character(*), parameter, public :: GM_COLOR_GRAY68                  = 'gray68'
    character(*), parameter, public :: GM_COLOR_GRAY69                  = 'gray69'
    character(*), parameter, public :: GM_COLOR_GRAY70                  = 'gray70'
    character(*), parameter, public :: GM_COLOR_GRAY71                  = 'gray71'
    character(*), parameter, public :: GM_COLOR_GRAY72                  = 'gray72'
    character(*), parameter, public :: GM_COLOR_GRAY73                  = 'gray73'
    character(*), parameter, public :: GM_COLOR_GRAY74                  = 'gray74'
    character(*), parameter, public :: GM_COLOR_GRAY75                  = 'gray75'
    character(*), parameter, public :: GM_COLOR_GRAY76                  = 'gray76'
    character(*), parameter, public :: GM_COLOR_GRAY77                  = 'gray77'
    character(*), parameter, public :: GM_COLOR_GRAY78                  = 'gray78'
    character(*), parameter, public :: GM_COLOR_GRAY79                  = 'gray79'
    character(*), parameter, public :: GM_COLOR_GRAY80                  = 'gray80'
    character(*), parameter, public :: GM_COLOR_GRAY81                  = 'gray81'
    character(*), parameter, public :: GM_COLOR_GRAY82                  = 'gray82'
    character(*), parameter, public :: GM_COLOR_GRAY83                  = 'gray83'
    character(*), parameter, public :: GM_COLOR_GRAY84                  = 'gray84'
    character(*), parameter, public :: GM_COLOR_GRAY85                  = 'gray85'
    character(*), parameter, public :: GM_COLOR_GRAY86                  = 'gray86'
    character(*), parameter, public :: GM_COLOR_GRAY87                  = 'gray87'
    character(*), parameter, public :: GM_COLOR_GRAY88                  = 'gray88'
    character(*), parameter, public :: GM_COLOR_GRAY89                  = 'gray89'
    character(*), parameter, public :: GM_COLOR_GRAY90                  = 'gray90'
    character(*), parameter, public :: GM_COLOR_GRAY91                  = 'gray91'
    character(*), parameter, public :: GM_COLOR_GRAY92                  = 'gray92'
    character(*), parameter, public :: GM_COLOR_GRAY93                  = 'gray93'
    character(*), parameter, public :: GM_COLOR_GRAY94                  = 'gray94'
    character(*), parameter, public :: GM_COLOR_GRAY95                  = 'gray95'
    character(*), parameter, public :: GM_COLOR_GRAY96                  = 'gray96'
    character(*), parameter, public :: GM_COLOR_GRAY97                  = 'gray97'
    character(*), parameter, public :: GM_COLOR_GRAY98                  = 'gray98'
    character(*), parameter, public :: GM_COLOR_GRAY99                  = 'gray99'
    character(*), parameter, public :: GM_COLOR_GRAY100                 = 'gray100'
    character(*), parameter, public :: GM_COLOR_GREEN                   = 'green'
    character(*), parameter, public :: GM_COLOR_GREEN_YELLOW            = 'greenyellow'
    character(*), parameter, public :: GM_COLOR_GREY                    = 'grey'
    character(*), parameter, public :: GM_COLOR_HONEY_DEW               = 'honeydew'
    character(*), parameter, public :: GM_COLOR_HOT_PINK                = 'hotpink'
    character(*), parameter, public :: GM_COLOR_INDIAN_RED              = 'indianred'
    character(*), parameter, public :: GM_COLOR_INDIGO                  = 'indigo'
    character(*), parameter, public :: GM_COLOR_IVORY                   = 'ivory'
    character(*), parameter, public :: GM_COLOR_KHAKI                   = 'khaki'
    character(*), parameter, public :: GM_COLOR_LAVENDER                = 'lavender'
    character(*), parameter, public :: GM_COLOR_LAVENDER_BLUSH          = 'lavenderblush'
    character(*), parameter, public :: GM_COLOR_LAWN_GREEN              = 'lawngreen'
    character(*), parameter, public :: GM_COLOR_LEMON_CHIFFON           = 'lemonchiffon'
    character(*), parameter, public :: GM_COLOR_LIGHT_BLUE              = 'lightblue'
    character(*), parameter, public :: GM_COLOR_LIGHT_CORAL             = 'lightcoral'
    character(*), parameter, public :: GM_COLOR_LIGHT_CYAN              = 'lightcyan'
    character(*), parameter, public :: GM_COLOR_LIGHT_GOLDEN_ROD_YELLOW = 'lightgoldenrodyellow'
    character(*), parameter, public :: GM_COLOR_LIGHT_GRAY              = 'lightgray'
    character(*), parameter, public :: GM_COLOR_LIGHT_GREEN             = 'lightgreen'
    character(*), parameter, public :: GM_COLOR_LIGHT_GREY              = 'lightgrey'
    character(*), parameter, public :: GM_COLOR_LIGHT_PINK              = 'lightpink'
    character(*), parameter, public :: GM_COLOR_LIGHT_SALMON            = 'lightsalmon'
    character(*), parameter, public :: GM_COLOR_LIGHT_SEA_GREEN         = 'lightseagreen'
    character(*), parameter, public :: GM_COLOR_LIGHT_SKY_BLUE          = 'lightskyblue'
    character(*), parameter, public :: GM_COLOR_LIGHT_SLATE_GRAY        = 'lightslategray'
    character(*), parameter, public :: GM_COLOR_LIGHT_SLATE_GREY        = 'lightslategrey'
    character(*), parameter, public :: GM_COLOR_LIGHT_STEEL_BLUE        = 'lightsteelblue'
    character(*), parameter, public :: GM_COLOR_LIGHT_YELLOW            = 'lightyellow'
    character(*), parameter, public :: GM_COLOR_LIME                    = 'lime'
    character(*), parameter, public :: GM_COLOR_LIME_GREEN              = 'limegreen'
    character(*), parameter, public :: GM_COLOR_LINEN                   = 'linen'
    character(*), parameter, public :: GM_COLOR_MAGENTA                 = 'magenta'
    character(*), parameter, public :: GM_COLOR_MAROON                  = 'maroon'
    character(*), parameter, public :: GM_COLOR_MEDIUM_AQUA_MARINE      = 'mediumaquamarine'
    character(*), parameter, public :: GM_COLOR_MEDIUM_BLUE             = 'mediumblue'
    character(*), parameter, public :: GM_COLOR_MEDIUM_ORCHID           = 'mediumorchid'
    character(*), parameter, public :: GM_COLOR_MEDIUM_PURPLE           = 'mediumpurple'
    character(*), parameter, public :: GM_COLOR_MEDIUM_SEA_GREEN        = 'mediumseagreen'
    character(*), parameter, public :: GM_COLOR_MEDIUM_SLATE_BLUE       = 'mediumslateblue'
    character(*), parameter, public :: GM_COLOR_MEDIUM_SPRING_GREEN     = 'mediumspringgreen'
    character(*), parameter, public :: GM_COLOR_MEDIUM_TURQUOISE        = 'mediumturquoise'
    character(*), parameter, public :: GM_COLOR_MEDIUM_VIOLET_RED       = 'mediumvioletred'
    character(*), parameter, public :: GM_COLOR_MIDNIGHT_BLUE           = 'midnightblue'
    character(*), parameter, public :: GM_COLOR_MINT_CREAM              = 'mintcream'
    character(*), parameter, public :: GM_COLOR_MISTY_ROSE              = 'mistyrose'
    character(*), parameter, public :: GM_COLOR_MOCCASIN                = 'moccasin'
    character(*), parameter, public :: GM_COLOR_NAVAJO_WHITE            = 'navajowhite'
    character(*), parameter, public :: GM_COLOR_NAVY                    = 'navy'
    character(*), parameter, public :: GM_COLOR_NONE                    = 'none'
    character(*), parameter, public :: GM_COLOR_OLD_LACE                = 'oldlace'
    character(*), parameter, public :: GM_COLOR_OLIVE                   = 'olive'
    character(*), parameter, public :: GM_COLOR_OLIVE_DRAB              = 'olivedrab'
    character(*), parameter, public :: GM_COLOR_ORANGE                  = 'orange'
    character(*), parameter, public :: GM_COLOR_ORANGE_RED              = 'orangered'
    character(*), parameter, public :: GM_COLOR_ORCHID                  = 'orchid'
    character(*), parameter, public :: GM_COLOR_PALE_GOLDEN_ROD         = 'palegoldenrod'
    character(*), parameter, public :: GM_COLOR_PALE_GREEN              = 'palegreen'
    character(*), parameter, public :: GM_COLOR_PALE_TURQUOISE          = 'paleturquoise'
    character(*), parameter, public :: GM_COLOR_PALE_VIOLET_RED         = 'palevioletred'
    character(*), parameter, public :: GM_COLOR_PAPAYA_WHIP             = 'papayawhip'
    character(*), parameter, public :: GM_COLOR_PEACH_PUFF              = 'peachpuff'
    character(*), parameter, public :: GM_COLOR_PERU                    = 'peru'
    character(*), parameter, public :: GM_COLOR_PINK                    = 'pink'
    character(*), parameter, public :: GM_COLOR_PLUM                    = 'plum'
    character(*), parameter, public :: GM_COLOR_POWDER_BLUE             = 'powderblue'
    character(*), parameter, public :: GM_COLOR_PURPLE                  = 'purple'
    character(*), parameter, public :: GM_COLOR_RED                     = 'red'
    character(*), parameter, public :: GM_COLOR_ROSY_BROWN              = 'rosybrown'
    character(*), parameter, public :: GM_COLOR_ROYAL_BLUE              = 'royalblue'
    character(*), parameter, public :: GM_COLOR_SADDLE_BROWN            = 'saddlebrown'
    character(*), parameter, public :: GM_COLOR_SALMON                  = 'salmon'
    character(*), parameter, public :: GM_COLOR_SANDY_BROWN             = 'sandybrown'
    character(*), parameter, public :: GM_COLOR_SEA_GREEN               = 'seagreen'
    character(*), parameter, public :: GM_COLOR_SEA_SHELL               = 'seashell'
    character(*), parameter, public :: GM_COLOR_SIENNA                  = 'sienna'
    character(*), parameter, public :: GM_COLOR_SILVER                  = 'silver'
    character(*), parameter, public :: GM_COLOR_SKY_BLUE                = 'skyblue'
    character(*), parameter, public :: GM_COLOR_SLATE_BLUE              = 'slateblue'
    character(*), parameter, public :: GM_COLOR_SLATE_GRAY              = 'slategray'
    character(*), parameter, public :: GM_COLOR_SLATE_GREY              = 'slategrey'
    character(*), parameter, public :: GM_COLOR_SNOW                    = 'snow'
    character(*), parameter, public :: GM_COLOR_SPRING_GREEN            = 'springgreen'
    character(*), parameter, public :: GM_COLOR_STEEL_BLUE              = 'steelblue'
    character(*), parameter, public :: GM_COLOR_TAN                     = 'tan'
    character(*), parameter, public :: GM_COLOR_TEAL                    = 'teal'
    character(*), parameter, public :: GM_COLOR_THISTLE                 = 'thistle'
    character(*), parameter, public :: GM_COLOR_TOMATO                  = 'tomato'
    character(*), parameter, public :: GM_COLOR_TURQUOISE               = 'turquoise'
    character(*), parameter, public :: GM_COLOR_VIOLET                  = 'violet'
    character(*), parameter, public :: GM_COLOR_WHEAT                   = 'wheat'
    character(*), parameter, public :: GM_COLOR_WHITE                   = 'white'
    character(*), parameter, public :: GM_COLOR_WHITE_SMOKE             = 'whitesmoke'
    character(*), parameter, public :: GM_COLOR_YELLOW                  = 'yellow'
    character(*), parameter, public :: GM_COLOR_YELLOW_GREEN            = 'yellowgreen'

    character(*), parameter :: GM_BINARY = 'gm' !! GraphicsMagick binary name.

    type, public :: gm_text_box_type
        !! Text box settings for drawing text on image.
        !!
        !! You can tag a font to specify whether it is a PostScript, TrueType,
        !! or X11 font. For example, `Arial.ttf` is a TrueType font,
        !! `ps:helvetica` is PostScript, and `x:fixed` is X11.
        character(GM_GRAVITY_LEN) :: gravity    = GM_GRAVITY_SW    !! Text position (GM).
        character(GM_COLOR_LEN)   :: background = GM_COLOR_BLACK   !! Box colour (GM).
        character(GM_COLOR_LEN)   :: foreground = GM_COLOR_WHITE   !! Text colour (GM).
        character(GM_FONT_LEN)    :: font       = 'DejaVuSansMono' !! Font name (GM).
        integer                   :: font_size  = 12               !! Font size in points.
    end type gm_text_box_type

    public :: dm_gm_add_text_box
    public :: dm_gm_convert
    public :: dm_gm_create
    public :: dm_gm_font_is_valid
    public :: dm_gm_get_dimensions
    public :: dm_gm_get_directory
    public :: dm_gm_get_file_extension
    public :: dm_gm_get_file_format
    public :: dm_gm_get_file_name
    public :: dm_gm_get_mime

    private :: gm_identify
contains
    ! **************************************************************************
    ! PUBLIC PROCEDURES.
    ! **************************************************************************
    integer function dm_gm_add_text_box(path, text, text_box, command) result(rc)
        !! Draws text camera image file, using GraphicsMagick. By default, the
        !! text box is drawn to the bottom-left corner of the image. If no text
        !! box is passed, the default values of the derived type are used. Any
        !! quote characters `'` and `"` in string `text` will be removed.
        !!
        !! Note: Make sure to pass only sanitised or parameterised arguments to
        !! this function, or risk shell injections if one of them is
        !! user-supplied.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EMPTY` if text or image path are empty.
        !! * `E_EXEC` if GraphicsMagick command execution failed.
        !! * `E_NOT_FOUND` if image at given path does no exist.
        !!
        !! ## References
        !!
        !! * [GraphicsMagick draw command](http://www.graphicsmagick.org/GraphicsMagick.html#details-draw)
        !!
        use :: dm_string, only: dm_string_remove

        character(*),              intent(in)            :: path     !! Image file path.
        character(*),              intent(in)            :: text     !! Text to add.
        type(gm_text_box_type),    intent(in),  optional :: text_box !! Image text box.
        character(:), allocatable, intent(out), optional :: command  !! Executed command.

        character(len(text))      :: text_clean
        character(GM_COMMAND_LEN) :: command_
        character(32)             :: point_size
        integer                   :: cmdstat, stat
        type(gm_text_box_type)    :: box

        command_ = ' '
        if (present(text_box)) box = text_box

        io_block: block
            rc = E_EMPTY
            if (len_trim(path) == 0 .or. len_trim(text) == 0) exit io_block

            rc = E_NOT_FOUND
            if (.not. dm_file_exists(path)) exit io_block

            text_clean = text
            call dm_string_remove(text_clean, '"')
            call dm_string_remove(text_clean, "'")

            rc = E_FORMAT
            write (command_, '(" -gravity ", a, " -box ", a, " -fill ", a, " -draw ''text 0,0 """, a, """''", 2(1x, a))', iostat=stat) &
                trim(box%gravity), trim(box%background), trim(box%foreground), trim(text_clean), trim(path), path
            if (stat /= 0) exit io_block

            if (box%font_size > 0) then
                write (point_size, '(" -pointsize ", i0)') box%font_size
                command_ = trim(point_size) // command_
            end if

            if (len_trim(box%font) > 0) command_ = ' -font ' // trim(box%font) // command_
            command_ = GM_BINARY // ' convert' // trim(command_)

            rc = E_EXEC
            call execute_command_line(trim(command_), exitstat=stat, cmdstat=cmdstat)
            if (stat /= 0 .or. cmdstat /= 0) exit io_block

            rc = E_NONE
        end block io_block

        if (present(command)) command = trim(command_)
    end function dm_gm_add_text_box

    integer function dm_gm_convert(input_file, input_args, output_file, output_args) result(rc)
        !! Converts image file with GraphicsMagick.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXEC` if calling GraphicsMagick failed.
        !! * `E_FORMAT` if command preparation failed.
        !!
        character(*), intent(in)           :: input_file  !! Input image path.
        character(*), intent(in)           :: input_args  !! Input convert arguments.
        character(*), intent(in), optional :: output_file !! Output image path.
        character(*), intent(in), optional :: output_args !! Output convert arguments.

        character(GM_COMMAND_LEN) :: command
        integer                   :: stat

        rc = E_FORMAT
        if (present(output_file) .and. present(output_args)) then
            write (command, '(a, " convert ", a, 3(1x, a))', iostat=stat) GM_BINARY, trim(input_args), trim(input_file), trim(output_args), trim(output_file)
        else if (present(output_file)) then
            write (command, '(a, " convert ", a, 2(1x, a))', iostat=stat) GM_BINARY, trim(input_args), trim(input_file), trim(output_file)
        else
            write (command, '(a, " convert ", a, 1x, a)', iostat=stat)    GM_BINARY, trim(input_args), trim(input_file)
        end if
        if (stat /= 0) return

        rc = E_EXEC
        call execute_command_line(trim(command), exitstat=stat)
        if (stat /= 0) return

        rc = E_NONE
    end function dm_gm_convert

    integer function dm_gm_create(path, width, height, color) result(rc)
        !! Creates image file of given dimensions and color with
        !! GraphicsMagick.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_EXIST` if the image file already exists.
        !! * `E_IO` if calling GraphicsMagick failed.
        !!
        use :: dm_string, only: dm_string_is_present

        character(*), intent(in)           :: path   !! Image file path.
        integer,      intent(in)           :: width  !! Image width.
        integer,      intent(in)           :: height !! Image height.
        character(*), intent(in), optional :: color  !! Background color.

        character(128) :: arguments

        rc = E_EXIST
        if (dm_file_exists(path)) return

        write (arguments, '("-size ", i0, "x", i0)') width, height
        if (dm_string_is_present(color)) arguments = trim(arguments) // ' xc:"' // trim(color) // '"'

        rc = dm_gm_convert(path, arguments)
    end function dm_gm_create

    pure elemental logical function dm_gm_font_is_valid(font) result(valid)
        !! Returns `.true.` if font name contains only valid characters
        !! (`-0-9A-Za-z`).
        character(*), parameter :: FONT_SET = &
            '-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'

        character(*), intent(in) :: font !! GM font name.

        integer :: n

        valid = .false.
        n = len_trim(font)
        if (n == 0) return
        if (verify(font(1:n), FONT_SET) > 0) return
        valid = .true.
    end function dm_gm_font_is_valid

    integer function dm_gm_get_dimensions(path, width, height) result(rc)
        !! Uses GraphicsMagick to determine the dimensions of the image at
        !! given path. On error, width and height are 0.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if execution of GraphicsMagick failed.
        !! * `E_NOT_FOUND` if image does not exist.
        !! * `E_READ` if reading dimensions failed.
        !!
        character(*), intent(in)  :: path   !! Image file path.
        integer,      intent(out) :: width  !! Image width.
        integer,      intent(out) :: height !! Image height.

        character(32) :: buffer
        integer       :: stat

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
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if execution of GraphicsMagick failed.
        !! * `E_NOT_FOUND` if image does not exist.
        !! * `E_READ` if reading dimensions failed.
        !!
        character(*),              intent(in)  :: path      !! Image file path.
        character(:), allocatable, intent(out) :: directory !! Image file directory.

        character(FILE_PATH_LEN) :: buffer

        rc = gm_identify(path, '%d', buffer)
        directory = trim(buffer)
    end function dm_gm_get_directory

    integer function dm_gm_get_file_extension(path, extension) result(rc)
        !! Uses GraphicsMagick to read the image file extension (`jpg`,
        !! `png`, ...). On error, the string `file_format` is allocated but
        !! empty.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if execution of GraphicsMagick failed.
        !! * `E_NOT_FOUND` if image does not exist.
        !! * `E_READ` if reading dimensions failed.
        !!
        character(*),              intent(in)  :: path      !! Image file path.
        character(:), allocatable, intent(out) :: extension !! Image file extension.

        character(8) :: buffer

        rc = gm_identify(path, '%e', buffer)
        extension = trim(buffer)
    end function dm_gm_get_file_extension

    integer function dm_gm_get_file_format(path, file_format) result(rc)
        !! Uses GraphicsMagick to determine the image file format (`JPEG`,
        !! `PNG`, ...). On error, the string `file_format` is allocated but
        !! empty.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if execution of GraphicsMagick failed.
        !! * `E_NOT_FOUND` if image does not exist.
        !! * `E_READ` if reading dimensions failed.
        !!
        character(*),              intent(in)  :: path        !! Image file path.
        character(:), allocatable, intent(out) :: file_format !! Image file format.

        character(16) :: buffer

        rc = gm_identify(path, '%m', buffer)
        file_format = trim(buffer)
    end function dm_gm_get_file_format

    integer function dm_gm_get_file_name(path, file_name) result(rc)
        !! Uses GraphicsMagick to return the file name part of the image path.
        !! On error, the string `file_name` is allocated but empty.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if execution of GraphicsMagick failed.
        !! * `E_NOT_FOUND` if image does not exist.
        !! * `E_READ` if reading dimensions failed.
        !!
        character(*),              intent(in)  :: path      !! Image file path.
        character(:), allocatable, intent(out) :: file_name !! Image file name.

        character(512) :: buffer

        rc = gm_identify(path, '%f', buffer)
        file_name = trim(buffer)
    end function dm_gm_get_file_name

    integer function dm_gm_get_mime(path, mime) result(rc)
        !! Determines the MIME type of the image through file format. The
        !! following file formats are recognised: GIF, JPEG, PNG, SVG. On
        !! error, the string `mime` is allocated but empty.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_IO` if execution of GraphicsMagick failed.
        !! * `E_NOT_FOUND` if image does not exist.
        !! * `E_READ` if reading dimensions failed.
        !!
        use :: dm_mime

        character(*),              intent(in)  :: path !! Image file path.
        character(:), allocatable, intent(out) :: mime !! MIME type.

        character(:), allocatable :: file_format

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
    integer function gm_identify(path, format, output, nbyte) result(rc)
        !! Identifies image with GraphicsMagick and returns result in `output`.
        !! The string `output` must be large enough to hold the result.
        !!
        !! The function returns the following error codes:
        !!
        !! * `E_FORMAT` if command preparation failed.
        !! * `E_INVALID` if the output string is of length 0.
        !! * `E_IO` if calling GraphicsMagick failed.
        !! * `E_NOT_FOUND` if the image file could not be found.
        !! * `E_READ` if no result was read from GraphicsMagick.
        !!
        !! ## References
        !!
        !! * [GraphicsMagick format characters](http://www.graphicsmagick.org/GraphicsMagick.html#details-format)
        !!
        use, intrinsic :: iso_c_binding, only: c_new_line
        use :: dm_posix_pipe, only: dm_posix_pipe_execute

        character(*), intent(in)            :: path   !! Image file path.
        character(*), intent(in)            :: format !! GraphicsMagick format attributes.
        character(*), intent(inout)         :: output !! Output string.
        integer(i8),  intent(out), optional :: nbyte  !! Number of bytes read from pipe.

        character(GM_COMMAND_LEN) :: command
        integer                   :: i, stat

        output = ' '
        if (present(nbyte)) nbyte = 0_i8

        rc = E_INVALID
        if (len(output) == 0) return

        rc = E_NOT_FOUND
        if (.not. dm_file_exists(path)) return

        rc = E_FORMAT
        write (command, '(a, " identify -format """, a, """ ", a)', iostat=stat) GM_BINARY, trim(format), trim(path)
        if (stat /= 0) return

        rc = dm_posix_pipe_execute(command, output, nbyte)

        i = index(output, c_new_line)
        if (i > 0) output(i:) = ' '
    end function gm_identify
end module dm_gm
