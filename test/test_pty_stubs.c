#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>
#ifdef __APPLE__
#include <util.h>
#else
#include <pty.h>
#endif

CAMLprim value monty_test_openpty(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(pair);
  int master, slave;
  if (openpty(&master, &slave, NULL, NULL, NULL) < 0) uerror("openpty", Nothing);
  pair = caml_alloc_tuple(2);
  Store_field(pair, 0, Val_int(master));
  Store_field(pair, 1, Val_int(slave));
  CAMLreturn(pair);
}

CAMLprim value monty_test_resize(value fd, value rows, value cols) {
  struct winsize size = {0};
  size.ws_row = Int_val(rows);
  size.ws_col = Int_val(cols);
  if (ioctl(Int_val(fd), TIOCSWINSZ, &size) < 0) uerror("TIOCSWINSZ", Nothing);
  return Val_unit;
}
