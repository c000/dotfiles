---
allowed-tools: Bash(git diff:*), Bash(git status:*), Bash(git log:*), Bash(git branch:*)
argument-hint: [base-branch]
description: Create a pull request
---

## Context

- Base branch: $ARGUMENTS (if not provided, use origin/main)
- Differ from base: !`git log $ARGUMENTS..` (if $ARGUMENTS is empty, use origin/main)
- IMPORTANT: Use the provided base branch argument, or origin/main if not specified

## Your task

1. Determine the base branch: use $ARGUMENTS if provided, otherwise use origin/main
2. Check the diff from the base branch: `git diff <base-branch>...HEAD`
3. Review commits since branching from the base branch: `git log <base-branch>..HEAD`
4. Then change branch name if needed
5. Then create a pull request (PR) with the appropriate title and description against the base branch
