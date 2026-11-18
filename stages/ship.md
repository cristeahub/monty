# Ship: commit and open a draft pull request

Take the finished work on this task's branches to a draft pull request that is quick to review.

1. Check first. Run the project's checks, and stop and report if they fail or if there is nothing to ship.
2. Commit. Read the last 20 commits by the repository's configured git user and match their tone, tense and length. Write a succinct subject, and a body only when the reason is not obvious from it. Never mention an agent and never add a co-author.
3. Push the branch and open the pull request as a draft (`gh pr create --draft`). Never mark it ready, request reviewers or merge.
4. Keep the description succinct: what changed and why in a few lines, then how it was verified. No file-by-file lists, no restating the diff, no filler headings.
5. Make it visual. For anything a reviewer can see (UI, CLI output, rendered documents), capture before and after images, upload them and embed them side by side in the description. Take "before" from the base branch. Skip images only when the change has no visible effect, and say so in one line.
6. In a task that spans repositories, do this for every repository with changes and link the pull requests to each other.
7. Report the pull request URLs to the user, then publish a handoff that lists them.

Limits: this stage approves committing on this task's branches, pushing those branches and opening draft pull requests for them. It does not approve force-pushing, pushing any other branch, merging, posting review comments or running `monty done`.
