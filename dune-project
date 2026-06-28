(lang dune 3.20)

(name monty)

(package
 (name monty)
 (synopsis "Monty, the head butler for launching pi worker sessions")
 (description "Monty plans work in a head pi session and launches worker pi sessions in Ghostty-backed worktrees.")
 (depends
  (ocaml (and (>= 5.2.0) (< 5.3.0)))
  (dune (>= 3.20))
  cmdliner
  yojson
  bos
  fpath
  rresult
  fmt))
