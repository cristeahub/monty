let () = match Array.to_list Sys.argv with
  | [_;_binary;native;python] -> Test_native_headless.run_suite "claude" native python
  | _ -> failwith "usage: test_claude_native BIN CLAUDE SDK_PYTHON"
