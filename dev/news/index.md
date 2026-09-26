# Changelog

## scholid (development version)

### Bug fixes

- Rejected identifiers containing invisible characters, such as soft
  hyphens and byte order marks, in
  [`is_scholid()`](https://thomas-rauter.github.io/scholid/reference/is_scholid.md)
  and
  [`classify_scholid()`](https://thomas-rauter.github.io/scholid/reference/classify_scholid.md).
  [`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md),
  [`detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.md),
  and
  [`extract_scholid()`](https://thomas-rauter.github.io/scholid/reference/extract_scholid.md)
  now remove those characters before normalizing or matching.

- Fixed
  [`detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.md)
  reporting bare 8-digit PMIDs such as `29456894` as `issn` when their
  digits happened to pass the ISSN checksum. Bare compact strings are
  now detected as ISSN only with a hyphen (`2434-561X`) or an `ISSN`
  label, so a bare `2434561X` is no longer detected.
  `normalize_scholid(x, "issn")` is unchanged.

### Internal improvements

- Sped up
  [`classify_scholid()`](https://thomas-rauter.github.io/scholid/reference/classify_scholid.md),
  [`detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.md),
  and
  [`extract_scholid()`](https://thomas-rauter.github.io/scholid/reference/extract_scholid.md)
  by checking whole vectors instead of one string at a time.

## scholid 0.2.0

CRAN release: 2026-06-04

### New identifier types

The package now supports 20 identifier types (up from 7 in 0.1.1). Each
type provides structural validation, normalization from URLs and labels,
and extraction from free text via the existing
[`is_scholid()`](https://thomas-rauter.github.io/scholid/reference/is_scholid.md),
[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md),
[`extract_scholid()`](https://thomas-rauter.github.io/scholid/reference/extract_scholid.md),
[`classify_scholid()`](https://thomas-rauter.github.io/scholid/reference/classify_scholid.md),
and
[`detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.md)
APIs.

New types in this release:

- **ROR** — Research Organization Registry iDs (checksum-validated)
- **RRID** — Research Resource Identifiers
- **SWHID** — Software Heritage persistent identifiers
- **OpenAlex** — OpenAlex entity keys (`W`, `A`, `S`, …)
- **bibcode** — SAO/NASA ADS bibliographic codes
- **ISNI** — International Standard Name Identifier (compact form;
  hyphenated ORCID-shaped strings remain `orcid`)
- **ARK** — Archival Resource Keys (`ark:/NAAN/Name`)
- **UniProt** — UniProtKB accessions
- **refseq** — NCBI RefSeq accessions (versioned)
- **sra** — INSDC Sequence Read Archive accessions (`SRR`, `SRX`, `SRP`,
  …)
- **geo** — NCBI GEO accessions (`GSE`, `GSM`, `GPL`, `GDS`)
- **bioproject** — INSDC BioProject accessions (`PRJNA`, `PRJEB`, …)
- **assembly** — INSDC genome assembly accessions (`GCA_`, `GCF_`,
  versioned)

Identifier definitions and validation rules are documented in the
`scholid_definitions` vignette.

### Internal improvements

- Introduced a central identifier registry as the single source of truth
  for type names, classification order, extraction patterns, and
  per-type metadata.
- Refactored per-type implementations to reduce duplication; exported
  APIs dispatch by naming convention (`is_<type>`, `normalize_<type>`,
  `extract_<type>`).
- Optimized
  [`classify_scholid()`](https://thomas-rauter.github.io/scholid/reference/classify_scholid.md)
  and
  [`detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.md)
  to avoid redundant work when resolving types.

## scholid 0.1.1

CRAN release: 2026-04-24

### Bug fixes

- Tightened normalization and validation behavior for checksum-based
  identifiers.
- Improved consistency between detection, normalization, and validation
  for ISBN, ORCID, DOI, PMCID, and arXiv identifiers.
- Fixed several edge cases in identifier parsing and canonicalization.

## scholid 0.1.0

CRAN release: 2026-02-13

Initial release.
