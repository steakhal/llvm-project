#!/usr/bin/env bash
# Keep opted-in PR branches (in the fork) up to date by cherry-picking upstream
# commits that touch a monitored set of paths.
#
# Directives (one per line), read from the PR body and from comments by $AUTHOR
# (comments by anyone else are ignored):
#   /steakhal-ci auto-cherry-pick <path> [<path>...]   opt in. Any upstream commit
#                                                      touching these paths is
#                                                      cherry-picked whole (it may
#                                                      touch other files too). The
#                                                      last such line wins.
#   /steakhal-ci ignore <sha> [<sha>...] [# comment]   never pick these commits; if
#                                                      one was already picked onto
#                                                      the PR, drop it from the
#                                                      branch (force-push with
#                                                      lease). Short SHAs work and
#                                                      may name the upstream commit
#                                                      or its backport. Accumulates.
#
# Only upstream commits newer than the earliest author date among the PR's own
# commits are considered, and only that much history is fetched.
#
# Env (defaults in parentheses):
#   UP_REPO     (swiftlang/llvm-project)  repo holding the PRs and the upstream ref
#   UP_REMOTE   (upstream)                git remote name for UP_REPO
#   UP_BRANCH   (llvm.org/main)           branch on UP_REMOTE to pick from
#   FORK_REMOTE (origin)                  remote holding the PR head branches
#   FORK_OWNER  (owner of $GITHUB_REPOSITORY) only PRs whose head is in this owner's repo
#   AUTHOR      (steakhal)                only PRs by this login
#   SLACK_DAYS  (7)                       history fetched before the cutoff
#   DRY_RUN=1   report only: no checkout, no cherry-pick, no push
#   SKIP_FETCH=1  don't fetch (local testing)
#   LOCAL_BRANCH / LOCAL_BODY / LOCAL_PR_COMMITS / LOCAL_SINCE
#               process one branch without gh (PR commits oldest first, space separated)

UP_REPO=${UP_REPO:-swiftlang/llvm-project}
UP_REMOTE=${UP_REMOTE:-upstream}
UP_BRANCH=${UP_BRANCH:-llvm.org/main}
FORK_REMOTE=${FORK_REMOTE:-origin}
FORK_OWNER=${FORK_OWNER:-${GITHUB_REPOSITORY%%/*}}
AUTHOR=${AUTHOR:-steakhal}
SLACK_DAYS=${SLACK_DAYS:-7}
UP_REF="$UP_REMOTE/$UP_BRANCH"
FAILED=0
IGNORED=""
MATCHED=""

say() {
  echo "$*"
  [ -n "${GITHUB_STEP_SUMMARY:-}" ] && echo "$*" >> "$GITHUB_STEP_SUMMARY"
}

# $1: ISO-8601 UTC timestamp, $2: days -> the timestamp $2 days earlier.
shift_days() {
  if date -u -d @0 >/dev/null 2>&1; then
    date -u -d "$1 - $2 days" +%Y-%m-%dT%H:%M:%SZ
  else
    date -u -j -v-"$2"d -f %Y-%m-%dT%H:%M:%SZ "$1" +%Y-%m-%dT%H:%M:%SZ
  fi
}

# PR JSON on stdin -> text to scan for directives: the body plus comments by $AUTHOR.
pr_text() {
  jq -r --arg a "$AUTHOR" \
    '[.body] + [.comments[]? | select(.author.login == $a) | .body] | join("\n")'
}

# True if the full sha $1 starts with any of the (possibly short) ignored shas.
is_ignored() {
  local t
  for t in $IGNORED; do
    case $1 in "$t"*) MATCHED="$MATCHED $t"; return 0 ;; esac
  done
  return 1
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

# Fetch only the history from (since - SLACK_DAYS) on, for upstream and the branch.
fetch_history() {
  local branch=$1 since=$2 first=$3 cut n=0
  cut=$(shift_days "$since" "$SLACK_DAYS") || return 1
  git fetch -q --filter=blob:none --shallow-since="$cut" "$UP_REMOTE" "$UP_BRANCH" || return 1
  git fetch -q --filter=blob:none --shallow-since="$cut" "$FORK_REMOTE" "$branch" || return 1
  # Dropping the PR's first commit needs its parent, which may predate the cutoff.
  until git rev-parse -q --verify "$first^" >/dev/null 2>&1; do
    n=$((n + 1)); [ $n -gt 20 ] && return 1
    git fetch -q --filter=blob:none --deepen=25 "$FORK_REMOTE" "$branch" || return 1
  done
}

sync_branch() {
  local branch=$1 body=$2 label=$3 pr_commits=$4 since=$5
  body=$(printf '%s\n' "$body" | tr -d '\r')

  local directive paths
  directive=$(printf '%s\n' "$body" |
    sed -n 's|^[[:space:]]*/steakhal-ci auto-cherry-pick[[:space:]]\{1,\}||p' | tail -1)
  [ -z "$directive" ] && return 0
  read -ra paths <<<"$directive"
  [ ${#paths[@]} -eq 0 ] && return 0

  say "### $label (\`$branch\`)"
  if [ -z "$pr_commits" ] || [ -z "$since" ]; then
    say "- the PR has no commits yet; nothing to anchor the search on"
    return
  fi
  say "Monitoring: ${paths[*]} (upstream commits since $since)"

  MATCHED=""
  IGNORED=$(printf '%s\n' "$body" |
    sed -n 's|^[[:space:]]*/steakhal-ci ignore[[:space:]]\{1,\}||p' |
    sed 's/#.*//' | tr -s ' ,\t' '\n\n\n' | tr 'A-F' 'a-f' |
    grep -E '^[0-9a-f]{7,40}$')

  if [ -z "${SKIP_FETCH:-}" ]; then
    fetch_history "$branch" "$since" "${pr_commits%% *}" ||
      { say "- fetching history failed"; FAILED=1; return; }
  fi

  local old
  old=$(git rev-parse "$FORK_REMOTE/$branch") ||
    { say "- cannot resolve $FORK_REMOTE/$branch"; FAILED=1; return; }

  # Upstream commits already cherry-picked onto the branch (incl. its base).
  local done_file
  done_file=$(mktemp)
  git log "$old" --format=%B |
    sed -n 's/^(cherry picked from commit \([0-9a-f]\{40\}\))$/\1/p' > "$done_file"

  # Backports on the PR whose upstream commit (or the backport itself) is ignored.
  local drops="" c up
  for c in $pr_commits; do
    up=$(git log -1 --format=%B "$c" |
      sed -n 's/^(cherry picked from commit \([0-9a-f]\{40\}\))$/\1/p' | tail -1)
    if { [ -n "$up" ] && is_ignored "$up"; } || is_ignored "$c"; then
      drops="$c $drops"   # newest first
    fi
  done

  # New upstream commits to pick, oldest first.
  local picks="" sha
  for sha in $(git log --reverse --no-merges --since="$since" --format=%H "$UP_REF" -- "${paths[@]}"); do
    is_ignored "$sha" && continue
    grep -qx "$sha" "$done_file" && continue
    picks="$picks$sha "
  done
  rm -f "$done_file"

  local t
  for t in $IGNORED; do
    case " $MATCHED " in *" $t "*) ;; *)
      say "- note: ignored \`$t\` matches no commit in range (older than the PR, or a typo)" ;;
    esac
  done

  if [ -z "$drops" ] && [ -z "$picks" ]; then
    say "- up to date"
    return
  fi

  local outside
  for c in $drops; do
    up=$(git log -1 --format=%B "$c" |
      sed -n 's/^(cherry picked from commit \([0-9a-f]\{40\}\))$/\1/p' | tail -1)
    say "- ${DRY_RUN:+would }drop $(git log -1 --format='`%h` %s' "$c") (ignored${up:+; backport of \`${up:0:12}\`})"
  done
  for sha in $picks; do
    outside=$(files_outside_paths "$sha" "${paths[@]}")
    say "- ${DRY_RUN:+would }pick $(git log -1 --format='`%h` %s' "$sha")$([ "$outside" -gt 0 ] && echo " (+$outside other files)")"
  done
  [ -n "${DRY_RUN:-}" ] && return

  # Check out only the files that picking and replaying will touch.
  { for sha in $picks; do git diff-tree --no-commit-id --name-only -r "$sha"; done
    if [ -n "$drops" ]; then
      for c in $pr_commits; do git diff-tree --no-commit-id --name-only -r "$c"; done
    fi; } | sort -u | sed 's/[][*?\\]/\\&/g; s|^|/|' |
    git sparse-checkout set --no-cone --stdin || { FAILED=1; return; }
  git checkout -q -B "$branch" "$FORK_REMOTE/$branch" || { FAILED=1; return; }

  for c in $drops; do
    if ! git rebase --onto "$c^" "$c" >/dev/null 2>&1; then
      git rebase --abort
      say "- **conflict** dropping \`${c:0:12}\` (later commits depend on it); nothing pushed"
      FAILED=1
      return
    fi
  done
  for sha in $picks; do
    if ! git cherry-pick -x "$sha" >/dev/null 2>&1; then
      git cherry-pick --abort
      say "- **conflict** picking \`${sha:0:12}\`; stopped, nothing pushed"
      FAILED=1
      return
    fi
  done
  if [ "$(git rev-parse HEAD)" != "$old" ]; then
    git push --force-with-lease="refs/heads/$branch:$old" "$FORK_REMOTE" \
      "HEAD:refs/heads/$branch" || { FAILED=1; return; }
    say "- pushed"
  fi
}

if [ -z "${SKIP_FETCH:-}" ]; then
  git remote get-url "$UP_REMOTE" >/dev/null 2>&1 ||
    git remote add "$UP_REMOTE" "https://github.com/$UP_REPO.git"
fi

if [ -n "${LOCAL_BRANCH:-}" ]; then
  sync_branch "$LOCAL_BRANCH" "$LOCAL_BODY" "$LOCAL_BRANCH" "${LOCAL_PR_COMMITS:-}" "${LOCAL_SINCE:-}"
  exit $FAILED
fi

# List PRs with light fields only: asking for commits/comments of many PRs at once
# exceeds GitHub's GraphQL node limit. The heavy fields are fetched per PR below.
if ! prs=$(gh pr list -R "$UP_REPO" --author "$AUTHOR" --state open -L 100 \
             --json number,headRefName,headRepositoryOwner); then
  echo "::error::gh pr list failed"
  exit 1
fi

for n in $(jq -r --arg o "$FORK_OWNER" \
             '.[] | select(.headRepositoryOwner.login == $o) | .number' <<<"$prs"); do
  if ! pr=$(gh pr view "$n" -R "$UP_REPO" --json number,body,comments,commits,headRefName); then
    echo "::error::gh pr view $n failed"
    FAILED=1
    continue
  fi
  b=$(jq -r .headRefName <<<"$pr")
  text=$(pr_text <<<"$pr")
  commits=$(jq -r '[.commits[].oid] | join(" ")' <<<"$pr")
  since=$(jq -r '[.commits[].authoredDate] | min // empty' <<<"$pr")
  sync_branch "$b" "$text" "$UP_REPO#$n" "$commits" "$since"
done

exit $FAILED
