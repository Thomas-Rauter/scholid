# scholid (development version)

## Bug fixes

- Rejected identifiers containing invisible characters, such as soft
  hyphens and byte order marks, in `is_scholid()` and
  `classify_scholid()`. `normalize_scholid()`, `detect_scholid_type()`,
  and `extract_scholid()` now remove those characters before
  normalizing or matching.

- Fixed `detect_scholid_type()` reporting bare 8-digit PMIDs such as
  `29456894` as `issn` when their digits happened to pass the ISSN checksum.
  Bare compact strings are now detected as ISSN only with a hyphen
  (`2434-561X`) or an `ISSN` label, so a bare `2434561X` is no longer
  detected. `normalize_scholid(x, "issn")` is unchanged.

## Internal improvements

- Sped up `classify_scholid()`, `detect_scholid_type()`, and
  `extract_scholid()` by checking whole vectors instead of one string
  at a time.

## Documentation

- Corrected the DOI case guidance in the definitions vignette: DOI
  names are case-insensitive for ASCII letters, and scholid preserves
  case when validating and normalizing them.

# scholid 0.2.0

## New identifier types

The package now supports 20 identifier types (up from 7 in 0.1.1). Each type
provides structural validation, normalization from URLs and labels, and
extraction from free text via the existing `is_scholid()`, `normalize_scholid()`,
`extract_scholid()`, `classify_scholid()`, and `detect_scholid_type()` APIs.

New types in this release:

- **ROR** — Research Organization Registry iDs (checksum-validated)
- **RRID** — Research Resource Identifiers
- **SWHID** — Software Heritage persistent identifiers
- **OpenAlex** — OpenAlex entity keys (`W`, `A`, `S`, …)
- **bibcode** — SAO/NASA ADS bibliographic codes
- **ISNI** — International Standard Name Identifier (compact form; hyphenated
  ORCID-shaped strings remain `orcid`)
- **ARK** — Archival Resource Keys (`ark:/NAAN/Name`)
- **UniProt** — UniProtKB accessions
- **refseq** — NCBI RefSeq accessions (versioned)
- **sra** — INSDC Sequence Read Archive accessions (`SRR`, `SRX`, `SRP`, …)
- **geo** — NCBI GEO accessions (`GSE`, `GSM`, `GPL`, `GDS`)
- **bioproject** — INSDC BioProject accessions (`PRJNA`, `PRJEB`, …)
- **assembly** — INSDC genome assembly accessions (`GCA_`, `GCF_`, versioned)

Identifier definitions and validation rules are documented in the
`scholid_definitions` vignette.

## Internal improvements

- Introduced a central identifier registry as the single source of truth for
  type names, classification order, extraction patterns, and per-type metadata.
- Refactored per-type implementations to reduce duplication; exported APIs
  dispatch by naming convention (`is_<type>`, `normalize_<type>`, `extract_<type>`).
- Optimized `classify_scholid()` and `detect_scholid_type()` to avoid redundant
  work when resolving types.

# scholid 0.1.1

## Bug fixes

-   Tightened normalization and validation behavior for checksum-based identifiers.
-   Improved consistency between detection, normalization, and validation for ISBN, ORCID, DOI, PMCID, and arXiv identifiers.
-   Fixed several edge cases in identifier parsing and canonicalization.

# scholid 0.1.0

Initial release.
