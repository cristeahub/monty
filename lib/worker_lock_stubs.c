#include <sys/file.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

/* flock follows the open file description across fork/exec. POSIX record
   locks (Unix.lockf) disappear with the supervisor even if its child lives. */
CAMLprim value monty_worker_flock(value fd)
{
  if (flock(Int_val(fd), LOCK_EX | LOCK_NB) == -1)
    caml_uerror("flock", Nothing);
  return Val_unit;
}

#include <sys/ioctl.h>
#include <caml/memory.h>
#include <caml/alloc.h>
CAMLprim value monty_terminal_size(value fd) {
  CAMLparam1(fd);
  CAMLlocal1(size);
  struct winsize ws;
  int rows = 24, cols = 80;
  if (ioctl(Int_val(fd), TIOCGWINSZ, &ws) == 0) {
    if (ws.ws_row > 0) rows = ws.ws_row;
    if (ws.ws_col > 0) cols = ws.ws_col;
  }
  size = caml_alloc_tuple(2);
  Store_field(size, 0, Val_int(rows));
  Store_field(size, 1, Val_int(cols));
  CAMLreturn(size);
}

#include <signal.h>
CAMLprim value monty_sigwinch(value unit) { return Val_int(SIGWINCH); }
