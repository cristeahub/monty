external get : Unix.file_descr -> int * int = "monty_terminal_size"

external winch : unit -> int = "monty_sigwinch"
