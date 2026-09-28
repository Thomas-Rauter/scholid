# scholid (development version)

## Bug fixes

- Treated Unicode spaces, such as the no-break space and the ideographic
  space, as whitespace. `is_scholid()` and `classify_scholid()` no longer
  accept a DOI or SWHID with such a space inside, and `extract_scholid()`
  no longer runs a DOI into the next word. `normalize_scholid()`,
  `detect_scholid_type()`, and `extract_scholid()` now trim these spaces
  and accept them after labels and between digit groups.

- Fixed `detect_scholid_type()` reporting a bare 8-digit PMID as `issn`
  when an invisible character was inside it.

## New features

- Added `locate_scholid()`, which finds identifiers of all types, or of
  the types given, in free text. It returns a data frame with one row per
  identifier: the text element, the type, the token `extract_scholid()`
  returns, the identifier as written, and its start and end positions.
  Where the spans of hits of different types overlap, such as an ISBN and
  the PMID-like digits inside it, it keeps the longer one.

- Accepted Unicode dashes, such as U+2010 and the en dash, in ORCID, ISBN,
  ISNI, and ISSN values, and full-width digits in every type except DOI,
  ARK, SWHID, and RRID, in `normalize_scholid()`, `extract_scholid()`, and
  `detect_scholid_type()`. They are read as their ASCII forms.
  `is_scholid()` still accepts only ASCII. The per-type rules are under
  "Input characters" in the definitions vignette.

- Percent-decoded `doi.org` and `dx.doi.org` URLs in
  `normalize_scholid(x, "doi")`, so `https://doi.org/10.1000%2F182` gives
  `10.1000/182`.

# scholid 0.2.1

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
