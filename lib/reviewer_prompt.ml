let adversarial_mandate =
  "Do not trust the author. Assume ill intent. Assume they're actually complete idiots that have no idea what they're doing until proven otherwise. This person is out to fuck your day up. Make sure this work is rock solid, and report anything otherwise."

let review_preamble =
  [
    "Start from the supplied task context and inspect every supplied worktree directly.";
    "Use the absolute MONTY_JOB_FILE path in the Monty instructions to resolve every persisted workspace path.";
    "Do not rely on another agent's summary and do not look for another reviewer's output.";
    adversarial_mandate;
  ]

let review_output_requirements =
  [
    "Return only evidence-backed findings ordered by severity, each with file and line references, impact, and the smallest safe correction.";
    "State 'No findings' plainly if no correction is warranted.";
  ]

let read_only_worktree_rule =
  "This is strictly read-only. Do not modify project, source, test, configuration, task, or worker-memory files."

let review_posture_header =
  [
    "When you run `/review`, apply this reviewer mandate:";
    adversarial_mandate;
    "";
    "Reviewer posture:";
  ]
