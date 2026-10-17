#!/usr/bin/env bash
# Keep opted-in PR branches (in the fork) up to date by cherry-picking upstream
# commits that touch a monitored set of paths.
#
# Directives (one per line), read from the PR body and from comments by $AUTHOR
# (comments by anyone else are ignored):
#   /steakhal-ci auto-cherry-pick <path> [<path>...]   opt in + paths to monitor;
#                                                      the last one wins
#   /steakhal-ci ignore <sha> [<sha>...] [# comment]   never pick these commits;
#                                                      all occurrences accumulate
#
# Env (defaults in parentheses):
#   UP_REPO     (swiftlang/llvm-project)  repo holding the PRs and the upstream ref
#   UP_REMOTE   (upstream)                git remote name for UP_REPO
#   UP_BRANCH   (llvm.org/main)           branch on UP_REMOTE to pick from
#   FORK_REMOTE (origin)                  remote holding the PR head branches
#   FORK_OWNER  (owner of $GITHUB_REPOSITORY) only PRs whose head is in this owner's repo
#   AUTHOR      (steakhal)                only PRs by this login
#   DRY_RUN=1   report only: no checkout, no cherry-pick, no push
#   SKIP_FETCH=1  don't fetch (local testing)
#   LOCAL_BRANCH / LOCAL_BODY  process one branch with the given PR body, no gh needed

UP_REPO=${UP_REPO:-swiftlang/llvm-project}
UP_REMOTE=${UP_REMOTE:-upstream}
UP_BRANCH=${UP_BRANCH:-llvm.org/main}
FORK_REMOTE=${FORK_REMOTE:-origin}
FORK_OWNER=${FORK_OWNER:-${GITHUB_REPOSITORY%%/*}}
AUTHOR=${AUTHOR:-steakhal}
UP_REF="$UP_REMOTE/$UP_BRANCH"
FAILED=0

say() {
  echo "$*"
  [ -n "${GITHUB_STEP_SUMMARY:-}" ] && echo "$*" >> "$GITHUB_STEP_SUMMARY"
}

# PR JSON on stdin -> text to scan for directives: the body plus comments by $AUTHOR.
pr_text() {
  jq -r --arg a "$AUTHOR" \
    '[.body] + [.comments[]? | select(.author.login == $a) | .body] | join("\n")'
}

# Number of files changed by $1 that lie outside every path in the remaining args.
files_outside_paths() {
  local sha=$1; shift
  git diff-tree --no-commit-id --name-only -r "$sha" |
    awk -v p="$*" 'BEGIN { n = split(p, a, " ") }
      { ok = 0; for (i = 1; i <= n; i++) if ($0 == a[i] || index($0, a[i] "/") == 1) ok = 1
        if (!ok) c++ }
      END { print c + 0 }'
}

sync_branch() {
  local branch=$1 body=$2 label=${3:-$1}
  body=$(printf '%s\n' "$body" | tr -d '\r')

  local directive
  directive=$(printf '%s\n' "$body" |
    sed -n 's|^[[:space:]]*/steakhal-ci auto-cherry-pick[[:space:]]\{1,\}||p' | tail -1)
  [ -z "$directive" ] && return 0
  local paths
  read -ra paths <<<"$directive"
  [ ${#paths[@]} -eq 0 ] && return 0

  say "### $label (\`$branch\`)"
  say "Monitoring: ${paths[*]}"

  # Resolve ignored SHAs (short or full) to full hashes.
  local ignored="" tok full
  for tok in $(printf '%s\n' "$body" |
      sed -n 's|^[[:space:]]*/steakhal-ci ignore[[:space:]]\{1,\}||p' |
      sed 's/#.*//' | tr -s ' ,\t' '\n\n\n'); do
    if full=$(git rev-parse --verify -q "$tok^{commit}"); then
      ignored="$ignored $full"
    else
      say "- warning: cannot resolve ignored sha \`$tok\`"
    fi
  done

  local head
  if [ -n "${DRY_RUN:-}" ]; then
    head="$FORK_REMOTE/$branch"
  else
    git checkout -q -B "$branch" "$FORK_REMOTE/$branch" || { FAILED=1; return; }
    head=HEAD
  fi

  # Everything already on the branch (including its base) that was cherry-picked
  # from upstream.
  local done_file base
  done_file=$(mktemp)
  git log "$UP_REF..$head" --format=%B |
    sed -n 's/^(cherry picked from commit \([0-9a-f]\{40\}\))$/\1/p' > "$done_file"
  base=$(git merge-base "$head" "$UP_REF") || { say "- no merge-base with $UP_REF"; FAILED=1; return; }

  local picks="" review="" sha subj outside
  for sha in $(git log --reverse --no-merges --format=%H "$base..$UP_REF" -- "${paths[@]}"); do
    grep -qx "$sha" "$done_file" && continue
    case " $ignored " in *" $sha "*) continue ;; esac
    subj=$(git log -1 --format='`%h` %s' "$sha")
    outside=$(files_outside_paths "$sha" "${paths[@]}")
    if [ "$outside" -gt 0 ]; then
      review="$review$subj  (+$outside files outside monitored paths)"$'\n'
    else
      picks="$picks$sha "
    fi
  done
  rm -f "$done_file"

  if [ -n "$review" ]; then
    say "- **Needs your decision** (not picked; add \`/steakhal-ci ignore <sha>\` or pick by hand):"
    printf '%s' "$review" | sed 's/^/  - /' | while IFS= read -r l; do say "$l"; done
  fi
  if [ -z "$picks" ]; then
    say "- up to date"
    return
  fi

  for sha in $picks; do
    say "- ${DRY_RUN:+would pick }$(git log -1 --format='`%h` %s' "$sha")"
  done
  [ -n "${DRY_RUN:-}" ] && return

  for sha in $picks; do
    if ! git cherry-pick -x "$sha" >/dev/null; then
      git cherry-pick --abort
      say "- **conflict** picking \`${sha:0:12}\`; stopped, nothing pushed"
      git reset -q --hard "$FORK_REMOTE/$branch"
      FAILED=1
      return
    fi
  done
  git push "$FORK_REMOTE" "HEAD:refs/heads/$branch" || { FAILED=1; return; }
  say "- pushed"
}

if [ -z "${SKIP_FETCH:-}" ]; then
  git remote get-url "$UP_REMOTE" >/dev/null 2>&1 ||
    git remote add "$UP_REMOTE" "https://github.com/$UP_REPO.git"
  git fetch -q --filter=blob:none "$UP_REMOTE" "$UP_BRANCH"
fi

if [ -n "${LOCAL_BRANCH:-}" ]; then
  sync_branch "$LOCAL_BRANCH" "$LOCAL_BODY"
  exit $FAILED
fi

while IFS= read -r pr; do
  n=$(jq -r .number <<<"$pr")
  b=$(jq -r .headRefName <<<"$pr")
  body=$(pr_text <<<"$pr")
  [ -z "${SKIP_FETCH:-}" ] && git fetch -q --filter=blob:none "$FORK_REMOTE" "$b"
  sync_branch "$b" "$body" "$UP_REPO#$n"
done < <(gh pr list -R "$UP_REPO" --author "$AUTHOR" --state open -L 100 \
           --json number,body,comments,headRefName,headRepositoryOwner |
         jq -c --arg o "$FORK_OWNER" '.[] | select(.headRepositoryOwner.login == $o)')

exit $FAILED
