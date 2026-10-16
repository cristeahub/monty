external open_pair_raw : unit -> Unix.file_descr * Unix.file_descr = "monty_test_openpty"
let open_pair () =
  let master,slave=open_pair_raw () in
  Unix.set_close_on_exec master;Unix.set_close_on_exec slave;master,slave
external resize_raw : Unix.file_descr -> int -> int -> unit = "monty_test_resize"
let resize fd ~rows ~cols = resize_raw fd rows cols
let set_raw fd =
  let attrs = Unix.tcgetattr fd in
  Unix.tcsetattr fd Unix.TCSANOW
    { attrs with c_ignbrk=false; c_brkint=false; c_parmrk=false; c_istrip=false;
      c_inlcr=false; c_igncr=false; c_icrnl=false; c_ixon=false; c_opost=false;
      c_echo=false; c_echonl=false; c_icanon=false; c_isig=false; c_csize=8;
      c_parenb=false; c_vmin=1; c_vtime=0 }
