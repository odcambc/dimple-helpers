#!/usr/bin/env bash
# Lint for the per-session isolation invariant documented in CLAUDE.md.
# Greps app.R and R/*.R for patterns that would let one user's session
# observe or affect another. Exits 0 on clean, 1 on any match.
#
# Run from the repo root:
#   ./check_isolation.sh

set -euo pipefail

cd "$(dirname "$0")"

# Files in scope: app.R + everything under R/. Exclude the lint itself
# (it's a shell script anyway, but be defensive).
FILES=(app.R)
while IFS= read -r f; do FILES+=("$f"); done < <(find R -type f -name "*.R" 2>/dev/null)

# ── POLICY ────────────────────────────────────────────────────────────────────
# Each entry: "regex|||human-readable explanation".
# Pipe-as-delimiter so regexes can contain spaces and parens.
#
# TODO(you): fill in the deny list. See trade-off notes in the chat or in
# CLAUDE.md § "Per-session isolation invariant". Suggested starting set
# (uncomment / adapt):
#
#   "<<-|||global / enclosing-scope write — breaks per-session isolation"
#   "^[[:space:]]*reactiveValues\\(|||top-level reactiveValues — shared across sessions (move inside server())"
#   "memoise\\(|||memoise() without explicit session scope mixes inputs across users"
#   "bindCache\\(|||bindCache() at module scope can mix per-session inputs"
#   "(write\\.csv|writeLines|saveRDS|writeRDS)\\(|||filesystem write from app code — log to stdout instead"
#   "DBI::dbWriteTable|||DB write from app code — not allowed in this stateless app"
#   "assign\\(.*envir[[:space:]]*=[[:space:]]*globalenv|||writes to global environment"
#
# Choose how strict you want to be:
#  * Minimal: just `<<-` and top-level `reactiveValues(`. Catches the most
#    common accidental leaks; tolerates future feature additions.
#  * Strict: include filesystem and DB write patterns even though we don't
#    use them today. Forces a CLAUDE.md update before anyone adds them.
PATTERNS=(
  "<<-|||global / enclosing-scope write — breaks per-session isolation"
  "^[[:space:]]*reactiveValues\\(|||top-level reactiveValues — shared across sessions (move inside server())"
)

# ── ENGINE (do not edit unless you're changing how the lint works) ───────────
if [ ${#PATTERNS[@]} -eq 0 ]; then
  echo "check_isolation.sh: PATTERNS array is empty — fill in the policy block above." >&2
  exit 2
fi

fail=0
for entry in "${PATTERNS[@]}"; do
  regex="${entry%%|||*}"
  reason="${entry##*|||}"
  # -n: line numbers; -E: extended regex; -H: always print filename.
  # We strip R comments (# …) before grepping so a forbidden pattern
  # mentioned in a comment doesn't trip the lint.
  for f in "${FILES[@]}"; do
    [ -f "$f" ] || continue
    # Remove text from the first unquoted '#' to end of line. Crude but
    # sufficient — no R string in this app contains '#' followed by code.
    stripped=$(sed -E 's/([^"'\''#]*)#.*/\1/' "$f")
    if printf '%s' "$stripped" | grep -nE "$regex" >/tmp/check_isolation.$$ 2>/dev/null; then
      while IFS= read -r hit; do
        echo "$f: $hit"
        echo "    ↳ $reason"
        fail=1
      done < /tmp/check_isolation.$$
    fi
    rm -f /tmp/check_isolation.$$
  done
done

if [ $fail -ne 0 ]; then
  echo
  echo "Isolation invariant violated. See CLAUDE.md § 'Per-session isolation invariant'." >&2
  exit 1
fi

echo "check_isolation.sh: clean ($(echo "${FILES[@]}" | wc -w | tr -d ' ') files scanned)"
