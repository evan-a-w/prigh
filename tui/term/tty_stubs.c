#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <termios.h>

/* notty clears ICANON/ECHO/ISIG/IXON but leaves IEXTEN, so on Linux the line
   discipline swallows ^O (VDISCARD) and ^V (VLNEXT) before the program sees
   them. OCaml's Unix.terminal_io has no c_iexten field, hence this stub. */
CAMLprim value
prigh_tty_clear_iexten(value fd)
{
  CAMLparam1(fd);
  struct termios t;
  int f = Int_val(fd);
  if (tcgetattr(f, &t) == 0) {
    t.c_lflag &= ~IEXTEN;
    tcsetattr(f, TCSANOW, &t);
  }
  CAMLreturn(Val_unit);
}
