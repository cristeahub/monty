(* Bounded, cancellable children. Never include child diagnostics in ordinary
   errors: provider stderr can contain credentials or message bodies. *)
let ( let* ) = Result.bind

let rec remove_temporary path =
  match Unix.lstat path with
  | {Unix.st_kind=Unix.S_DIR;_} ->
      Sys.readdir path |> Array.iter (fun name -> remove_temporary (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT,_,_) -> ()

let run ?(env = Unix.environment ()) ?(cancel = fun () -> false)
    ?(diagnostic_failure = false)
    ~seconds ~cwd ~command ~input () =
  State_store.protect_result ~action:"run context child" ~path:cwd (fun () ->
      let scratch = Filename.temp_file ~temp_dir:cwd ".context-" ".run" in
      Unix.unlink scratch;
      Unix.mkdir scratch 0o700;
      let temporary suffix = Filename.temp_file ~temp_dir:scratch "io" suffix in
      let stdin_path = temporary ".in" in
      let stdout_path = temporary ".out" in
      let stderr_path = temporary ".err" in
      Fun.protect
        ~finally:(fun () -> remove_temporary scratch)
        (fun () ->
          let* () = State_store.write_file_atomic ~path:stdin_path ~perm:0o600 input in
          let pid = Unix.fork () in
          if pid = 0 then (
            try
              ignore (Unix.setsid ());
              Unix.chdir scratch;
              List.iter (fun (path, flags, target) ->
                  let fd = Unix.openfile path flags 0o600 in
                  Unix.dup2 fd target; Unix.close fd)
                [stdin_path, [Unix.O_RDONLY], Unix.stdin;
                 stdout_path, [Unix.O_WRONLY; Unix.O_TRUNC], Unix.stdout;
                 stderr_path, [Unix.O_WRONLY; Unix.O_TRUNC], Unix.stderr];
              Unix.execve "/bin/sh" [|"/bin/sh"; "-c"; "exec " ^ command|] env
            with _ -> Unix._exit 127);
          let stop () =
            (try Unix.kill (-pid) Sys.sigkill with _ -> ());
            (try Unix.kill pid Sys.sigkill with _ -> ());
            (try ignore (Process.waitpid [] pid) with _ -> ())
          in
          let deadline = Unix.gettimeofday () +. float_of_int seconds in
          let rec wait () =
            let too_large = List.exists (fun p -> (Unix.stat p).Unix.st_size > 8_000_000)
                [stdout_path; stderr_path] in
            if cancel () then (stop (); Error "context operation cancelled")
            else if Unix.gettimeofday () >= deadline || too_large then (
              stop (); Error "context child exceeded time/output budget")
            else match Process.waitpid [Unix.WNOHANG] pid with
              | 0, _ -> (try ignore (Unix.select [] [] [] 0.1) with Unix.Unix_error(Unix.EINTR,_,_)->()); wait ()
              | _, Unix.WEXITED 0 -> Ok (Shell.read_file stdout_path)
              | _, _ when diagnostic_failure -> Ok (Shell.read_file stdout_path ^ Shell.read_file stderr_path)
              | _, status ->
                  let code = match status with Unix.WEXITED n -> n | _ -> -1 in
                  Error (Printf.sprintf "context child failed (exit %d); check credentials, CLI capability and configuration" code)
          in
          try wait () with exn -> stop (); raise exn))
