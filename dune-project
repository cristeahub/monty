(lang dune 3.20)

(name monty)

(package
 (name monty)
 (synopsis "Monty, the head butler for native Pi task sessions")
 (description "Monty plans work in one Pi process and runs task agents in durable repo-scoped worktrees.")
 (depends
  (ocaml (and (>= 5.2.0) (< 5.3.0)))
  (dune (>= 3.20))
  cmdliner
  yojson
  bos
  fpath
  rresult
  fmt))
