Create a new git branch based on the task description.

Task/Purpose: $ARGUMENTS

Steps:
1. Check current branch with `git branch --show-current`
2. Generate an appropriate branch name following these conventions:
   - Use kebab-case (lowercase with hyphens)
   - Keep it short but descriptive (3-5 words max)
   - Common prefixes: `feat/`, `fix/`, `refactor/`, `chore/`, `docs/`
   - Examples: `feat/add-user-auth`, `fix/login-validation`, `refactor/api-client`
3. Run `git switch -c <branch-name>` to create and switch to the new branch
4. Confirm the branch was created successfully
