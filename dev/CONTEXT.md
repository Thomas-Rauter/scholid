# CONTEXT.md — scholid

What scholid is, who depends on it, and why it is shaped the way it is.
How to work on it is in `AGENTS.md`. Like `AGENTS.md`, this file points
to the single home of each fact instead of repeating it.

## What it is

A CRAN package for offline, syntax-level handling of scholarly
identifiers: validate, normalize, extract and locate in free text,
classify, and detect. The pitch is in `DESCRIPTION`, the supported types
are whatever
[`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md)
returns, and the user-facing tour is `vignettes/get_started.Rmd`.

## Who depends on it

- R users cleaning bibliographic or metadata columns and mining
  identifiers from text.
- Other R packages, as a low-level building block. The main one is
  **scholidonline** (same maintainer;
  <https://github.com/Thomas-Rauter/scholidonline>, locally at
  `../scholidonline`). It adds everything that needs the network:
  existence checks, metadata, linked identifiers and conversion. It
  imports scholid (see its `DESCRIPTION` for the minimum version) and
  calls scholid’s exported functions (`grep -rn "scholid::" R/` there).
  A behaviour change in an exported function can therefore break
  scholidonline.

## Why it is shaped this way

`vignettes/get_started.Rmd` (“Design notes”) states the principles. The
reasons behind them:

- **Safe to depend on.** No runtime dependencies and a low R floor keep
  scholid cheap to import for other packages. Adding a dependency or
  raising the R floor is a scope decision for the maintainer, not an
  implementation detail.
- **Conservative over clever.** A false positive, where a random string
  is reported as an identifier, is worse than a miss, because downstream
  code treats a match as a fact. Hence checksums wherever a scheme
  defines one, allowlisted prefixes, strict boundaries in extraction,
  and PMID as a last-resort fallback. Most of the history in `git log`
  is tightening in this direction.
- **Well-formed, not existing.** scholid answers “is this well-formed?”,
  never “does this exist?”. Anything that needs a registry lookup
  belongs in scholidonline.
- **One registry.** `.scholid_registry()` is the single source of truth
  for types and precedence. Exported functions dispatch by naming
  convention, so a new type extends the package without changing the
  exported API (see the 0.2.0 entry in `NEWS.md`).
- **Order encodes precedence.** Several identifier grammars overlap
  (bare digit strings, 8-character and 16-character forms, short
  accessions). The order and the individual collision rules are
  explained in `vignettes/scholid_definitions.Rmd` (“Classification
  order” and each type’s “Validation in scholid” section) and pinned by
  tests.

## Scope decisions

Identifier types that were considered and deliberately **not** added.
Don’t propose them again without a new argument.

| Candidate | Why not |
|----|----|
| Handle (generic) | No structural rule narrower than “anything with a slash”; DOI already covers the important case |
| URN | A zoo of namespaces; would need a parser per namespace |
| EAN-8, EAN-13 | Trade barcodes; ISBN-13 already covers the scholarly subset; high false-positive risk |
| LSID | Deprecated; only of historical interest |

Possible later, each needing its own case: ISTC, GND, RAiD, PURL, Handle
restricted to allowlisted namespaces, and further life-science
accessions (BioSample, ArrayExpress, Ensembl; Ensembl first needs a
policy for version suffixes).

## Status

Current version and release history: `DESCRIPTION` and `NEWS.md`.
