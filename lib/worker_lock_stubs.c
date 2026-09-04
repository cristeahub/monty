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
