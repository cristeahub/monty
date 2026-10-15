let allowed name =
  List.mem name ["MONTY_HOME";"MONTY_RUN_DIR";"MONTY_WORKER_DIR";"MONTY_JOB_ID";"MONTY_JOB_TITLE";
    "MONTY_JOB_REPO";"MONTY_JOB_BRANCH";"MONTY_JOB_CONTEXT";"MONTY_JOB_FILE";"MONTY_TASK_KEY";
    "MONTY_JOB_WORKTREE";"MONTY_WORKTREE_MODE";"MONTY_BRANCH_PREFIX"]
  || (String.starts_with ~prefix:"MONTY_WORKSPACE_" name &&
      String.length name>16 && String.for_all(function '0'..'9'->true|_->false)(String.sub name 16 (String.length name-16)))
  || List.exists(fun prefix->String.starts_with ~prefix name)["MONTY_FIXTURE_";"MONTY_TEST_";"MONTY_FAKE_CONTAINER_"]
