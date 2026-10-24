#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <termios.h>

/* notty clears ICANON/ECHO/ISIG/IXON but leaves IEXTEN, so on Linux the line
   discipline swallows ^O (VDISCARD) and ^V (VLNEXT) before the program sees
   them. OCaml's Unix.terminal_io has no c_iexten field (so notty's restore
   cannot put it back either), hence this stub. Returns the previous value. */
CAMLprim value
prigh_tty_set_iexten(value fd, value enabled)
{
  CAMLparam2(fd, enabled);
  struct termios t;
  int f = Int_val(fd);
  int was = 1;
  if (tcgetattr(f, &t) == 0) {
    was = (t.c_lflag & IEXTEN) != 0;
    if (Bool_val(enabled)) t.c_lflag |= IEXTEN; else t.c_lflag &= ~IEXTEN;
    tcsetattr(f, TCSANOW, &t);
  }
  CAMLreturn(Val_bool(was));
}
