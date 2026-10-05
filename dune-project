(lang dune 3.20)

(name monty)

(package
 (name monty)
 (synopsis "Monty, the head butler for launching AI worker sessions")
 (description "Monty plans work in a head agent session and runs Pi or Codex workers in managed worktrees through Ghostty or headless chains.")
 (depends
  (ocaml (and (>= 5.2.0) (< 5.3.0)))
  (dune (>= 3.24))
  cmdliner
  yojson
  fpath
  fmt
  (cohttp-lwt-unix (and (>= 6.2) (< 7.0)))
  (websocket (= 2.17))
  (digestif (>= 1.3.1))))
