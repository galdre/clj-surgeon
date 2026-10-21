# Move Forms Into an Existing Namespace

**Status:** Planned 2026-10-21. Decisions answered by galdre on the decision page
"Move Into Existing Namespace" (key `move-into-ns`).
**Motivating evidence:** the invoice refactoring trial (2026-10-21). An agent
building domain vocabulary namespaces needed to move helpers such as
`has-email?` from `billing.invoice` into the existing `billing.customer`.
`:extract!` refuses an existing target, clojure-lsp's move-form refused
("Nothing to change"), and the agent's rules forbade a hand move, so it
stopped.

## Outcome

`:extract!` with an explicit `--into-existing` flag moves one or more top-level
forms from a source namespace into a namespace file that already exists. It
computes the complete future state of every file it would touch, including
callers in other files, before writing any of them. Every refusal is decided
at plan time, so application only replays a plan that is already known to
succeed. clj-kondo runs after the write as a safety net, and any new error
rolls the whole transaction back.

## Boundary

The caller chooses the forms, the target, and the anchor. The tool owns the
mechanical consequences: requires, qualifier rewrites, publicizing, caller
rewrites, ordering checks, refusals, atomic writes, rollback and receipts. It
never chooses which forms move, and never moves a form the caller did not
name.

## Decisions

| # | Question | Answer |
|---|---|---|
| 1 | Make a moved private form public when the source still calls it? | Yes, automatically, and report it in `:public-forms`. An explicit `:public-forms` list that omits one refuses. Done on this branch (439e590). |
| 2 | How does a caller ask for an existing target? | An explicit flag on `extract!`, `--into-existing`. Without it an existing target still refuses. |
| 3 | Where do the forms go? | An anchor is always required: `--above FORM` or `--below FORM`. Several forms keep their source order as one block at the anchor. If the result uses a form before its definition with no `declare`, the move still happens and the result carries a warning. Anchor details are under Anchors below. |
| 4 | How are requires merged? | Add each missing libspec, inserted at its alphabetical position; existing entries are never reordered. For a library the target already requires under a different alias, rewrite the moved forms' qualifiers to the target's alias. Refuse only when the target uses the same alias for a different library. |
| 5 | Calls into the target through an alias? | Rewrite to plain names (`customer/email` to `email`, `::customer/k` to `::k`) and add no require of the target to itself. |
| 6 | A form that would mean something different in the target? | Refuse a defined-name clash and a plain symbol that resolves differently in the target, naming each. An unqualified auto-resolved keyword (`::id`) moves unchanged and the result carries a warning listing each one; the caller renames them as it sees fit. An aliased keyword (`::inv/id`) is treated like an aliased symbol (decisions 4 and 5). |
| 7 | A moved form calls a form that stays behind? | Refuse, naming each form left behind, and suggest the `:ls-extract` set. |
| 8 | Callers in other files? | Rewrite qualified references, aliased references and var quotes to the target, adding the target's require where needed. Refuse a reference that cannot be rewritten exactly, and refuse a rewrite that would create a circular require. The codebase must never be left in an inconsistent state: detect refusals before editing any file wherever possible, and otherwise roll every file back. |
| 9 | CLI verification? | clj-kondo before and after, rollback on any new error; when clj-kondo is not installed, skip and report `:lint {:checked false}`. This is the fallback behind decision 8's plan-first rule, not a substitute for it. |

### Clarifications (answered 2026-10-21)

- **Requires (4):** insert at the sorted position; never reorder existing
  entries.
- **Auto-resolved keywords (6):** warn about unqualified `::` keywords instead
  of rewriting them; treat aliased `::alias/k` keywords like aliased symbols.
- **Plan first (8, 9):** the requirement is that a move never leaves the
  codebase inconsistent. Detecting a refusal before writing is the goal;
  rolling every file back is the fallback when that is not possible.

### Anchors (answered 2026-10-21, decision page "Move Anchor Decisions")

- **Named anchors** name only forms in clj-surgeon's definition list: core
  defining forms, the `>defn` macros, and macros declared in the project's
  `.clj-surgeon.edn`. An unrecognized macro, such as `(defwhatever foo ...)`,
  refuses; the refusal shows the `.clj-surgeon.edn` entry that would make it
  recognized, and the `--at-start` / `--at-end` alternatives.
- **Ambiguous names** refuse and list every match. The caller names one
  exactly with a typed anchor, such as `--below 'defmethod render :card'`.
- **No nameable form:** `--at-start` (directly after the `ns` form) and
  `--at-end` (after the last form). There are no line-number anchors; forms
  with no usable name, such as `(comment ...)`, cannot be anchors.
- Exactly one of `--above`, `--below`, `--at-start`, `--at-end` is required.

## Public Contract

```bash
# plan only; prints every file's future state summary and any refusal
clj-surgeon extract --file src/billing/invoice.clj \
  --forms '[has-email?]' --to src/billing/customer.clj \
  --into-existing --below email

# apply exactly that plan
clj-surgeon extract! --file src/billing/invoice.clj \
  --forms '[has-email?]' --to src/billing/customer.clj \
  --into-existing --below email --receipt-out /tmp/move.edn
```

Success reports `:applied`, `:public-forms`, `:requires-added`,
`:qualifiers-rewritten`, `:callers-rewritten`, `:warnings` (use before
definition; moved unqualified `::` keywords), `:lint`, and the receipt path. Every refusal reports a stable
`:error-type`, the offending items, `:files-unchanged true`, and a next step.

## Refusals

| `:error-type` | When |
|---|---|
| `:extraction-target-exists` | target exists and `--into-existing` is absent (unchanged behaviour) |
| `:into-existing-target-missing` | `--into-existing` and the target does not exist |
| `:anchor-required` | none, or more than one, of `--above`, `--below`, `--at-start`, `--at-end` |
| `:anchor-not-found` | the anchor names no form in the definition list; shows the `.clj-surgeon.edn` entry for an unrecognized macro |
| `:ambiguous-anchor` | the anchor's name matches several forms; lists each with a typed anchor that names it exactly |
| `:required-public-forms-missing` | an explicit `:public-forms` omits a required form |
| `:alias-conflict` | target uses a moved form's alias for a different library |
| `:name-clash` | target already defines a moved name |
| `:meaning-change` | a plain symbol in a moved form resolves differently in the target |
| `:left-behind-dependencies` | a moved form calls a form that stays in the source |
| `:caller-not-rewritable` | a reference in another file cannot be rewritten exactly (`:refer :all`, `resolve`, string-built symbols) |
| `:circular-require` | a rewrite would make two namespaces require each other, directly or through others |
| `:stale-source` | any planned file's hash changed before application |
| `:lint-regression` | new clj-kondo errors after the write; everything rolled back |

## Safety Invariants

- One snapshot of every file the plan reads; every candidate is computed
  from that snapshot.
- No file is written unless the plan holds no refusal.
- Every candidate parses before any write.
- Unrelated bytes in every file stay unchanged, including existing require
  order, comments and metadata.
- The receipt keeps the original bytes of every file touched, including the
  pre-existing target, and undo refuses if any result file has drifted.
- A handled failure during writing restores every file.

## Behaviour Matrix (tests)

| Dimension | Cases |
|---|---|
| Flag and target | no flag + existing target refuses; flag + missing target refuses; flag + existing target moves |
| Anchor | above; below; at-start; at-end; none; two given; unknown name; unrecognized custom macro (refusal shows the `.clj-surgeon.edn` entry); macro declared in `.clj-surgeon.edn`; ambiguous `defmulti`/`defmethod` name; typed `defmethod` anchor; target with only an `ns` form |
| Order | one form; several in source order; result uses a form before its definition (warning, still moves) |
| Requires | already present same alias; missing (sorted insert, first, middle, last); same library other alias (qualifiers rewritten); same alias other library (refuse); target has no `:require` clause |
| Self references | `alias/f` to the target becomes `f`; `::alias/k` becomes `::k`; no self-require added |
| Meaning | name defined in target; plain symbol shadowed by a target `:refer`; symbol excluded from `clojure.core` and redefined in target (the `billing.customer` / `name` case); unqualified `::k` moved unchanged with a warning; `::alias/k` rewritten like a symbol |
| Left behind | moved form calls a remaining form (refuse with the `:ls-extract` set) |
| Publicizing | required private form made public; explicit list missing one refuses |
| Callers | qualified, aliased, var quote, `:refer` list entry, test file; `:refer :all` refuses; rewrite that creates a cycle refuses |
| Atomicity | stale hash refuses; mid-commit failure restores all files; lint regression restores all files; receipt undo restores all files |

Pure tests use literal sources. The field regression is the trial's
`has-email?` move into `billing.customer`, which needs decisions 1, 4, 5 and 6
at once.

## Stages

Each stage is a separate, green commit.

1. Bug fixes in today's new-file extraction (see below).
2. Core move: flag, anchor, requires, self references, meaning refusals,
   left-behind refusal, publicizing, receipt with the target's original
   bytes, CLI lint fallback.
3. Caller rewrites in other files, with the circular-require refusal, all
   decided at plan time.
4. MCP route parity.

## Bugs Fixed First (stage 1)

- **Mutually recursive forms were deleted.** Moving two forms that call each
  other wrote neither to the new file but removed both from the source, and
  reported success. The ordering step dropped every form in a dependency
  cycle. Fixed: every selected form is written, and a `declare` is emitted
  for any moved form used before its definition.
- **A `declare` for a moved form stayed in the source.** Fixed: moved names
  are removed from the source's `declare` forms, and an emptied `declare` is
  removed.

## Documentation

- `extract` / `extract!` help: `--into-existing`, `--above`, `--below`, the
  refusals table.
- `skill.md` and the advanced skill reference: the move-into-existing
  workflow.
- CHANGELOG entries per stage; tech-tree row moves from OPEN to BUILDING to a
  measured status.

## Verification Gates

1. The field regression fails before stage 2 and passes after it.
2. The behaviour matrix passes.
3. clj-kondo is clean on every fixture result.
4. The fast suite shows no new failures against the recorded baseline.
5. One real-program move (the trial repository) succeeds, and its receipt's
   undo restores every file's original hash.
