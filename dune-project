(lang dune 3.20)

(name monty)

(package
 (name monty)
 (synopsis "Monty, the head butler for launching AI worker sessions")
 (description "Monty plans work in a head agent session and launches Pi or Codex workers in Ghostty-backed worktrees.")
 (depends
  (ocaml (and (>= 5.2.0) (< 5.3.0)))
  (dune (>= 3.20))
  cmdliner
  yojson
  bos
  fpath
  rresult
  fmt))
