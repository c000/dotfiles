# Code Implementation

## Agent Usage
- **MUST**: Use `code-implementation-expert` agent when implementing new features or modifying existing code
- **Purpose**: Ensures new code follows existing patterns and conventions in the codebase
- **When to use**:
  - Adding new entities, components, or services
  - Implementing new API endpoints or methods
  - Creating new functions based on existing patterns
  - Refactoring code to match project conventions
- **Instructions**: 実装は必要最小限に留め、余計なファイル作成や変更は一切行わない。要求されたこと以外は絶対に実装しない

# Git Conventions

## Commit Messages
- **Language**: Write commit messages in Japanese
- **Format**: Use conventional commit format when applicable (e.g., `feat:`, `fix:`, `refactor:`)
- **Example**: `feat(duo): スタッフIDによるアプリケーション検索機能を追加`

## Pre-commit Requirements
- **MUST**: Always run lint and format checks before committing
- **Verification**: Ensure all lint and format issues are resolved before creating commits

## Staged Commits (@commands/commit-staged.md)
- **NEVER**: Use `git add` when executing @commands/commit-staged.md
- **MUST**: Only commit already staged files - do not stage new files
- **Purpose**: This command is specifically for committing files that are already staged
- **Workflow**: Check `git diff --cached` and `git log`, then commit only the staged files

## History Modification
- **CRITICAL**: NEVER perform git history modification operations without explicit user approval
- **NEVER**: Automatically run commands like:
  - `git commit --amend`
  - `git rebase`
  - `git rebase --continue` (even during an ongoing rebase, do not continue without asking)
  - `git reset --hard`
  - `git push --force` or `git push -f`
  - `git reflog`
  - Any command that rewrites git history
- **MUST**: Always ask the user first before performing any history-changing operations
- **Reason**: These operations can cause data loss or create conflicts with remote repositories
- **NO EXCEPTIONS**: Do not make autonomous decisions like "it's during a rebase" or "the fix is needed". Always confirm with the user first
- **Exception**: Only proceed if the user explicitly instructs (e.g., "continue", "amend it", "proceed")

## Fixup Commits
- **PREFER**: Use `git commit --fixup` instead of `git commit --amend` for fixing previous commits
- **Usage**: `git commit --fixup <commit-hash>` creates a fixup commit for later squashing
- **Workflow**:
  1. Make your fix changes
  2. Stage the changes: `git add <files>`
  3. Create fixup commit: `git commit --fixup <target-commit-hash>`
- **NEVER**: Automatically run `git rebase --autosquash` - only create the fixup commit
- **When to use `--amend`**: Only when explicitly requested by the user

# Rust Development

## Module Structure
- **CRITICAL**: ABSOLUTELY NEVER create `mod.rs` files anywhere
- **MUST**: Always use named module files (e.g., `domain.rs` NOT `domain/mod.rs`)

### ❌ Wrong
```
src/
├── domain/
│   └── mod.rs          ❌ NEVER do this
├── handlers/
│   └── mod.rs          ❌ NEVER do this
└── services/
    └── user/
        └── mod.rs      ❌ NEVER do this
```

### ✅ Correct
```
src/
├── domain.rs           ✅ Use this
├── handlers.rs         ✅ Use this
└── services/
    └── user.rs         ✅ Use this
```

## Testing
- When creating tests, start small and keep them working, then gradually add more functionality

### Test Patterns to Avoid

1. **Self-referential mapping validation tests**
   - ❌ Bad example:
     ```rust
     // Testing your own JSON mapping definition against itself
     fn test_index_mapping_contains_all_fields() {
         let mapping = MyDocument::index_mapping();
         assert!(mapping.get("properties").unwrap().contains_key("fieldName"));
     }
     ```
   - Reason: This just duplicates the implementation without verifying actual behavior
   - ✅ Better alternatives:
     - Integration tests that apply mappings to real Elasticsearch
     - Tests that verify document conversion works correctly

2. **Internal structure verification based on implementation details**
   - Don't test detailed JSON structure; instead verify actual behavior (Elasticsearch operations, search, indexing)

### Recommended Test Patterns

1. **Integration tests**: Test against real external systems (Elasticsearch, etc.)
2. **Conversion tests**: Verify Entity → DTO → Entity roundtrips work correctly
3. **Boundary tests**: Test edge cases and special conditions
4. **Behavior tests**: Verify system behavior, not implementation details

## Feature Flags
- When adding `testutil` feature, ensure dependency consistency
- Example: When adding `testutil` to `duo_personal_information`, also add `testutil` to its dependency `shared`
- Ensure feature flags are consistent across the entire dependency chain

# Command Line Tools

## File Reading
- **MUST**: Always use the Read tool instead of `cat`, `head`, `tail`, or other bash commands for reading files
- **Reason**: Provides better user experience and proper handling of file contents
- **Example**:
  - ❌ `cat file.txt`
  - ❌ `head -n 10 file.txt`
  - ✅ Use Read tool directly

## Text Processing
- **MUST**: Always use `perl` instead of `sed` or `awk` for text processing
- **Example**:
  - ❌ `sed -i 's/old/new/g' file.txt`
  - ✅ `perl -pi -e 's/old/new/g' file.txt`

# Database Development

## SQLx Prepare
- **MUST**: Run `sqlx prepare` after any SQL-related changes to update `.sqlx` files
- **When to run**: 
  - After adding new SQL queries
  - After modifying existing SQL queries
  - After changing database schema (migrations)
  - Before committing any database-related changes
- **Command**: `cargo sqlx prepare` (from the Rust project directory)
- **Purpose**: Generates compile-time verified query metadata for offline compilation

## Elasticsearch Access
- **Protocol**: HTTP (not HTTPS)
- **Authentication**: No authentication required
- **Example**: `curl -X GET "http://localhost:9200/_cluster/health?pretty"`
- **Indices**:
  - `matching` - Matching and candidate facility data
  - `facility` - Facility master data
  - `staff` - Staff data
  - `application` - Application data
