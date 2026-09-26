# AGENTS.md — scholid

Instructions for AI coding agents working in this repository. Read this
file first. Before your first change in a session, also read
`CONTEXT.md` (what the package is, who depends on it, and why it is
shaped this way).

**Rule zero: point, don’t copy.** Every fact in this repo has exactly
one home. When you need a fact, follow the pointer. When you document
something, write it in its home and link to it from elsewhere. Never
restate the type list, regexes, function signatures, or per-type rules
in this file, in `CONTEXT.md`, or in new docs. If a pointer here is
stale, fix the pointer.

**Rule one: never stage, commit, or push.** No `git add`, `git commit`
or `git push`, even if a task seems to call for it; the maintainer does
all of that. Instead, finish every task that changed files with a
proposed commit message (format under “Git and releases”).

## Where knowledge lives

| Question | Home |
|----|----|
| Which types exist, their precedence, their patterns | `.scholid_registry()` in `R/scholid_registry.R`; [`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md) at runtime |
| Per-type formats, checksums, collision rules | `vignettes/scholid_definitions.Rmd` |
| Exported API, arguments, NA and length contracts | roxygen blocks in `R/*.R` (rendered to `man/`) |
| User workflows and design principles | `vignettes/get_started.Rmd` (“Design notes”) |
| Package pitch, dependencies, R floor, version | `DESCRIPTION` |
| Change history | `NEWS.md`, `git log` |
| Expected edge-case behaviour | `tests/testthat/` |
| CI (R CMD check matrix, coverage, pkgdown) | `.github/workflows/` |
| Scope, rationale, rejected identifier types | `CONTEXT.md` |

## How the code fits together

The six exported functions validate their input and then dispatch by
**name**: `.scholid_dispatch()` (`R/input_validation.R`) looks up
`is_<type>`, `normalize_<type>` or `extract_<type>` with
[`get0()`](https://rdrr.io/r/base/exists.html). The per-type
implementations live in `R/is_idtype_functions.R`,
`R/normalize_scholid.R` and `R/extract_scholid.R`.
[`classify_scholid()`](https://thomas-rauter.github.io/scholid/reference/classify_scholid.md)
and
[`detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.md)
walk the registry order. Consequences:

- A per-type function’s name is its registration. Renaming one breaks
  dispatch; only `tests/testthat/test-scholid_registry.R` catches that.
- Registry `order` values are user-visible behaviour: changing them
  changes what
  [`classify_scholid()`](https://thomas-rauter.github.io/scholid/reference/classify_scholid.md)
  and
  [`detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.md)
  return.

## Commands

Run from the package root. Locally, `.Rprofile` activates renv
(untracked); don’t modify renv state unless asked.

``` r

devtools::load_all()
devtools::document()               # after any roxygen change; rewrites man/, NAMESPACE
devtools::test()                   # or devtools::test(filter = "detect_scholid_type")
devtools::check()                  # target: 0 errors, 0 warnings, 0 notes
devtools::build_readme()           # after editing README.Rmd
spelling::spell_check_package()    # accepted new words go in inst/WORDLIST
urlchecker::url_check()            # after adding or changing URLs
```

## Invariants

- Follow the design notes in `vignettes/get_started.Rmd`. In particular:
  base R only, so never add a package to `Imports`; propose it to the
  maintainer instead.
- Stay compatible with the R floor in `DESCRIPTION`: no native pipe
  `|>`, no `\(x)` lambdas, no base functions newer than that version.
- Keep the NA and length contracts documented in each exported
  function’s roxygen `@return` / `@description`.
- `is_<type>()` accepts canonical form only. `normalize_<type>()`
  accepts wrapped forms (URLs, labels) and must only return values that
  pass `is_<type>()`. `extract_<type>()` must only return tokens that
  pass validation (use `.scholid_extract_validated()`).
- Registry and extraction regexes use PCRE (`perl = TRUE`).
- Precedence and collision rules are pinned by tests in
  `test-classify_scholid.R` and `test-detect_scholid_type.R`. Never
  loosen or delete an existing test to make a change pass; stop and ask.

## Code style

Match the surrounding code; `R/input_validation.R` is a good reference.

- 4-space indent; multi-line calls with one argument per line and
  aligned `=`.
- Keep lines at 80 characters or fewer (long regex literals are the
  exception).
- Files are sectioned with `# Level 1 function ... ----` (called by
  exported functions) and `# Level 2 ...` (called by level 1) headers.
- Per-type dispatch targets (`is_<type>`, `normalize_<type>`,
  `extract_<type>`) are unexported and not dot-prefixed. Other internals
  are dot-prefixed (`.scholid_*`, `.is_<type>_strict`,
  `.clean_extracted_<type>`).
- Every function has a roxygen block; internals use `@noRd`.
- Tests call `testthat::` explicitly and live in the test file named
  after the exported function they exercise.

## Recipes

### Add an identifier type

First check `CONTEXT.md` for types that were already rejected. Commit
`2d6feaa` (genome assembly) is the reference implementation, and
`2d62624` shows the documentation pass (`git show --stat <sha>`). Touch,
in order:

1.  Registry entry in `.scholid_registry()` with a unique `order` placed
    by specificity relative to overlapping types.
2.  `is_<type>()` in `R/is_idtype_functions.R`, `normalize_<type>()` in
    `R/normalize_scholid.R`, and `extract_<type>()` plus
    `.clean_extracted_<type>()` in `R/extract_scholid.R`.
3.  Tests in the is, normalize, extract, classify and detect test files,
    including collision tests against every type with an overlapping
    grammar. Update the hard-coded type lists in `test-scholid_types.R`
    and `test-scholid_registry.R`. Add entries for the new type in
    `tests/testthat/helper-scholid_fixtures.R`.
4.  A section in `vignettes/scholid_definitions.Rmd`, following its
    stated layout, plus a row in its overview table.
5.  The other places that name types: the “including …” list in the
    `DESCRIPTION` Description field and the Scope list in `README.Rmd`.
    Then run `devtools::build_readme()`. Don’t write the number of types
    anywhere; it goes stale.
6.  `inst/WORDLIST`, then a `NEWS.md` entry.

Tell the maintainer if scholidonline might want the type too; don’t edit
that repo from here.

### Fix a bug

Write a failing regression test from the real-world input first (see
`edc2a45` for the pattern), then fix. If the fix changes classification
or detection results for existing inputs, it is user-visible: add a
`NEWS.md` entry.

## Versioning and NEWS

These are the maintainer’s cross-package rules, which live outside this
repo.

- Semantic versioning, also before 1.0: PATCH = bug fixes only; MINOR =
  new features, including new types or user-visible behaviour changes;
  MAJOR = breaking changes.
- Don’t change `Version:` in `DESCRIPTION` unless asked.
- `NEWS.md`: user-visible changes only, past tense, each bullet starts
  with a verb, grouped by kind of change, formatted like the existing
  entries. Never edit the entries of a released version (one with a
  `v<version>` git tag). If the top section is already released, start a
  `# scholid (development version)` section above it.

## Git and releases

- Never stage, commit, or push (rule one). Read-only commands such as
  `git status`, `git diff`, `git log` and `git show` are fine.
- Proposed commit message, given in a fenced code block at the end of
  your final reply:
  - Header: imperative, at most 72 characters, no trailing period, in
    the style of `git log` (“Add …”, “Tighten …”, “Prefer … over …”).
  - A blank line, then a body wrapped at 72 characters that explains
    what changed and why (see `git show edc2a45`).
- Never rewrite history (amend, rebase, reset) or delete branches or
  tags. GitHub rulesets block force-pushes and deletions on every branch
  and tag anyway.
- Version bumps, tags, GitHub releases and CRAN submissions are
  maintainer-only.

## Hands off

- Generated: `man/` and `NAMESPACE` (edit roxygen instead), `README.md`
  (edit `README.Rmd`), `docs/` (pkgdown output, untracked, built by CI).
- Maintainer-only: `tarball_storage/`, `releases/`, `dev/` (local
  scratch, untracked, may not exist).
- The maintainer sometimes keeps the working tree read-only. If a write
  fails with a permission error, stop and ask; don’t `chmod`.

## Definition of done

- `devtools::document()` produces no unintended diff.
- `devtools::test()` passes and `devtools::check()` is clean.
- New words pass the spell check.
- Every changed fact is updated in its home (see the first table), not
  duplicated here or in `CONTEXT.md`.
- Nothing is staged or committed, and your reply ends with the proposed
  commit message (rule one).
