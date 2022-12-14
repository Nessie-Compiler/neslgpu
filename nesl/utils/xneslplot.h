#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#define DEFAULT_TITLE "NESL Graphics Display"
#define ICONNAME "NESLG"
#define name_width 8
#define name_height 8
#define name_x_hot 8
#define name_y_hot 8

Display *display;
Window win;
GC graphcontext;
Pixmap pxmap;
int window_height;
char shade_bits[8] ={
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };
char *default_font_name = "6x13";

char *ps_header1 =
"%!PS-Adobe-1.0\n%%Title:\n%%Creator: Nesl\n%%CreationDate:\n%%Pages: 1\n%%DocumentFonts:\n";

char *ps_header2 =
"%%EndComments\n/spoofdict 25 dict def\nspoofdict begin\n/S {moveto lineto stroke} bind def\n/LS {moveto} bind def\n/L {2 copy lineto stroke moveto} bind def\n/P {moveto 0 0 rlineto stroke} bind def\n/F {findfont exch scalefont setfont} bind def\n/T {gsave moveto rotate show grestore} bind def\n%%EndProlog\n2 setlinecap\n0 setgray\n8 /Times-Roman F\n";

char *ps_tailer =
"showpage\nend\n";

extern void begin_paren_vector();
extern void end_paren_vector();
extern void end_paren_vector_with_space();
extern int get_number();
extern char *get_string();
extern int sum();
extern int read_int();
extern int *read_int_array();
extern char *read_char_array();
extern XPoint *read_points();
extern XSegment *read_segments();
extern XArc *read_arcs();
extern void put_number();
extern int all_equal();
