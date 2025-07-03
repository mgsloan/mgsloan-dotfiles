#!/usr/bin/env python3
"""
Commit unstaged changes to a specified branch without switching branches.
Uses git worktrees to avoid affecting the current working directory.
Preserves staged changes in the staging area.
"""

import sys
import subprocess
import argparse
from pathlib import Path


def run_git(cmd, cwd=None, check=False):
    """Run a git command and return the output."""
    result = subprocess.run(
        ['git'] + cmd,
        cwd=cwd,
        capture_output=True,
        text=True,
        check=check
    )
    return result.stdout.strip(), result.stderr.strip(), result.returncode


def run_git_or_fail(cmd, cwd=None):
    """Run a git command and return stdout. Raise exception if it fails."""
    stdout, _, _= run_git(cmd, cwd=cwd, check=True)
    return stdout


def check_git_state():
    """Check if the repository is in a clean state for this operation."""
    # Check if we're in a git repository
    try:
        run_git_or_fail(['rev-parse', '--git-dir'])
    except RuntimeError:
        print("Error: Not in a git repository", file=sys.stderr)
        sys.exit(1)

    # Check for rebase, merge, cherry-pick, etc.
    git_dir = Path('.git')
    if git_dir.is_file():
        # Handle git worktrees where .git is a file
        with open(git_dir) as f:
            git_dir = Path(f.read().strip().split(': ')[1])

    conflict_markers = [
        'rebase-merge',
        'rebase-apply',
        'MERGE_HEAD',
        'CHERRY_PICK_HEAD',
        'REVERT_HEAD',
        'BISECT_LOG'
    ]

    for marker in conflict_markers:
        if (git_dir / marker).exists():
            print(f"Error: Repository is in the middle of a {marker.replace('_', '-').lower()}", file=sys.stderr)
            sys.exit(1)


def check_unstaged_changes():
    """Check if there are unstaged changes."""
    stdout = run_git_or_fail(['diff', '--name-only'])
    if not stdout:
        print("No unstaged changes to commit", file=sys.stderr)
        sys.exit(0)


def ensure_worktree(branch_name, worktree_path):
    """Ensure worktree exists and has the correct branch checked out."""
    # Check if worktree already exists
    stdout = run_git_or_fail(['worktree', 'list', '--porcelain'])

    worktree_exists = False
    worktree_branch = None

    for line in stdout.split('\n'):
        if line.startswith('worktree ') and line.endswith(str(worktree_path)):
            worktree_exists = True
        elif worktree_exists and line.startswith('branch refs/heads/'):
            worktree_branch = line.split('/')[-1]
            break

    if worktree_exists:
        if worktree_branch != branch_name:
            print(f"Error: Worktree at {worktree_path} has branch '{worktree_branch}' checked out, expected '{branch_name}'", file=sys.stderr)
            sys.exit(1)
        print(f"Using existing worktree at {worktree_path}")
    else:
        # Check if branch exists
        _, _, returncode = run_git(['rev-parse', '--verify', f'refs/heads/{branch_name}'])

        if returncode != 0:
            # Create new branch
            print(f"Creating new branch '{branch_name}'")
            run_git_or_fail(['branch', branch_name])

        # Create worktree
        print(f"Creating worktree at {worktree_path}")
        worktree_path.parent.mkdir(parents=True, exist_ok=True)
        run_git_or_fail(['worktree', 'add', str(worktree_path), branch_name])


def rebase_branch_on_origin_main(worktree_path, branch_name):
    """Rebase the target branch atop origin/main in the worktree."""
    print(f"Rebasing branch '{branch_name}' atop origin/main...")

    # First, fetch origin to make sure we have the latest
    try:
        run_git_or_fail(['fetch', 'origin', 'main'], cwd=worktree_path)
    except subprocess.CalledProcessError:
        print("Warning: Could not fetch origin/main, proceeding without fetch", file=sys.stderr)

    # Check if origin/main exists
    _, _, returncode = run_git(['rev-parse', '--verify', 'origin/main'], cwd=worktree_path)
    if returncode != 0:
        print("Warning: origin/main not found, skipping rebase", file=sys.stderr)
        return

    # Check if the branch has any commits
    try:
        run_git_or_fail(['rev-parse', '--verify', f'{branch_name}^{{commit}}'], cwd=worktree_path)
        has_commits = True
    except subprocess.CalledProcessError:
        has_commits = False

    if not has_commits:
        print(f"Branch '{branch_name}' has no commits, skipping rebase")
        return

    # Perform the rebase
    _, stderr, returncode = run_git(['rebase', 'origin/main'], cwd=worktree_path)
    if returncode != 0:
        print(f"Error during rebase: {stderr}", file=sys.stderr)
        # Try to abort the rebase
        run_git(['rebase', '--abort'], cwd=worktree_path)
        raise RuntimeError(f"Failed to rebase {branch_name} onto origin/main")

    print(f"Successfully rebased '{branch_name}' atop origin/main")


def apply_unstaged_changes_to_worktree(worktree_path, branch_name):
    """Apply unstaged changes to the worktree and squash them onto the last commit."""
    # Get the original HEAD before making any changes
    original_head = run_git_or_fail(['rev-parse', 'HEAD'])

    # First, commit any staged changes temporarily
    has_staged_changes = False
    stdout = run_git_or_fail(['diff', '--cached', '--name-only'])
    if stdout:
        has_staged_changes = True
        print("Temporarily committing staged changes...")
        _, stderr, returncode = run_git(['commit', '-m', 'TEMP: staged changes'])
        if returncode != 0:
            print(f"Error creating temporary commit for staged changes: {stderr}", file=sys.stderr)
            return False
        staged_commit_sha = run_git_or_fail(['rev-parse', 'HEAD'])
    else:
        staged_commit_sha = original_head

    # Now commit the unstaged changes
    print("Creating temporary commit with unstaged changes...")
    _, stderr, returncode = run_git(['commit', '-a', '-m', 'Add'])

    if returncode != 0:
        print(f"Error creating temporary commit: {stderr}", file=sys.stderr)
        if has_staged_changes:
            # Restore to staged commit state
            run_git_or_fail(['reset', '--soft', original_head])
        return False

    try:
        # Get the commit SHA
        unstaged_commit_sha = run_git_or_fail(['rev-parse', 'HEAD'])

        # Check if the target branch has any commits
        try:
            run_git_or_fail(['rev-parse', '--verify', f'{branch_name}^{{commit}}'], cwd=worktree_path)
            has_commits = True
        except subprocess.CalledProcessError:
            has_commits = False

        if has_commits:
            # Cherry-pick the unstaged changes commit in the worktree
            print(f"Applying unstaged changes to branch '{branch_name}'...")
            _, stderr, returncode = run_git(
                ['cherry-pick', '--no-commit', unstaged_commit_sha],
                cwd=worktree_path
            )

            if returncode != 0:
                raise RuntimeError(f"Error cherry-picking changes: {stderr}")

            # Squash the changes onto the last commit
            print(f"Squashing changes onto the last commit in '{branch_name}'...")
            _, stderr, returncode = run_git(
                ['commit', '--amend', '--no-edit'],
                cwd=worktree_path
            )

            if returncode != 0:
                raise RuntimeError(f"Error amending commit: {stderr}")
        else:
            # No commits on the branch, just make a regular commit
            print(f"Branch '{branch_name}' has no commits, creating initial commit...")
            _, stderr, returncode = run_git(
                ['cherry-pick', unstaged_commit_sha],
                cwd=worktree_path
            )

            if returncode != 0:
                raise RuntimeError(f"Error cherry-picking changes: {stderr}")

    except Exception as e:
        # On any unexpected error, restore to original state
        print(f"{e}", file=sys.stderr)
        run_git_or_fail(['reset', '--hard', staged_commit_sha])
        if has_staged_changes:
            run_git_or_fail(['reset', '--soft', original_head])
        raise

    # Success - reset to staged commit to remove unstaged changes
    print("Cleaning up temporary commits...")
    run_git_or_fail(['reset', '--hard', staged_commit_sha])

    # Now reset soft to original HEAD to preserve staged changes
    if has_staged_changes:
        run_git_or_fail(['reset', '--soft', original_head])

    return True


def main():
    parser = argparse.ArgumentParser(description='Commit unstaged changes to another branch using worktrees')
    parser.add_argument('branch', help='Target branch name')
    args = parser.parse_args()

    # Perform checks
    check_git_state()
    check_unstaged_changes()

    # Get repository root
    repo_root = run_git_or_fail(['rev-parse', '--show-toplevel'])
    repo_root = Path(repo_root)

    # Set up worktree path
    worktree_path = repo_root.parent / 'worktrees' / args.branch

    # Ensure worktree exists with correct branch
    ensure_worktree(args.branch, worktree_path)

    # Rebase the target branch atop origin/main
    rebase_branch_on_origin_main(worktree_path, args.branch)

    # Apply and commit unstaged changes
    print(f"Applying unstaged changes to branch '{args.branch}'...")
    if apply_unstaged_changes_to_worktree(worktree_path, args.branch):
        print(f"Successfully committed unstaged changes to branch '{args.branch}'")
        print("Unstaged changes have been squashed onto the last commit")
        print("Staged changes have been preserved")
    else:
        print("Failed to apply unstaged changes", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
