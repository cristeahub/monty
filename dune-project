(lang dune 3.24)

(name monty)

(package
 (name monty)
 (synopsis "Monty, the head butler for launching AI worker sessions")
 (description "Monty plans work in a head agent session and runs Pi, Codex or Claude workers in managed worktrees through Ghostty, Rex or headless chains.")
 (depends
  (ocaml (and (>= 5.2.0) (< 5.3.0)))
  cmdliner
  yojson
  fpath
  fmt
  base64
  eqaf
  uri
  lwt
  (cohttp-lwt-unix (and (>= 6.2) (< 7.0)))
  (websocket (= 2.17))
  (digestif (>= 1.3.1))))
