let () = match Array.to_list Sys.argv with
  | [_;_binary;native] -> Test_native_headless.run_suite "codex" native ""
  | _ -> failwith "usage: test_codex_native BIN CODEX"
