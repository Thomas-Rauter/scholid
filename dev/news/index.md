# Changelog

## scholid (development version)

### Bug fixes

- Treated Unicode spaces, such as the no-break space and the ideographic
  space, as whitespace.
  [`is_scholid()`](https://thomas-rauter.github.io/scholid/reference/is_scholid.md)
  and
  [`classify_scholid()`](https://thomas-rauter.github.io/scholid/reference/classify_scholid.md)
  no longer accept a DOI or SWHID with such a space inside, and
  [`extract_scholid()`](https://thomas-rauter.github.io/scholid/reference/extract_scholid.md)
  no longer runs a DOI into the next word.
  [`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md),
  [`detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.md),
  and
  [`extract_scholid()`](https://thomas-rauter.github.io/scholid/reference/extract_scholid.md)
  now trim these spaces and accept them after labels and between digit
  groups.

- Fixed
  [`detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.md)
  reporting a bare 8-digit PMID as `issn` when an invisible character
  was inside it.

### New features

- Added
  [`locate_scholid()`](https://thomas-rauter.github.io/scholid/reference/locate_scholid.md),
  which finds identifiers of all types, or of the types given, in free
  text. It returns a data frame with one row per identifier: the text
  element, the type, the token
  [`extract_scholid()`](https://thomas-rauter.github.io/scholid/reference/extract_scholid.md)
  returns, the identifier as written, and its start and end positions.
  Where the spans of hits of different types overlap, such as an ISBN
  and the PMID-like digits inside it, it keeps the longer one.

- Added
  [`scholid_key()`](https://thomas-rauter.github.io/scholid/reference/scholid_key.md),
  which gives keys for comparing identifiers, so that values written
  differently match in
  [`duplicated()`](https://rdrr.io/r/base/duplicated.html),
  [`match()`](https://rdrr.io/r/base/match.html), and
  [`merge()`](https://rdrr.io/r/base/merge.html). It normalizes values
  like
  [`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md)
  and then applies the type’s key rule: an ISBN-10 gets the key of its
  ISBN-13, arXiv, RefSeq, and genome assembly keys leave out the
  version, DOI keys uppercase ASCII letters, and SWHID keys leave out
  the qualifiers. Keys are not identifiers, and some don’t pass
  [`is_scholid()`](https://thomas-rauter.github.io/scholid/reference/is_scholid.md).
  The rules are in the definitions vignette.

- Added
  [`format_scholid()`](https://thomas-rauter.github.io/scholid/reference/format_scholid.md),
  which writes identifiers as resolver URLs, as CURIEs, or, for arXiv,
  as the DOIs that arXiv registers, such as `10.48550/arXiv.2101.00001`,
  without the version. It normalizes values like
  [`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md)
  first, and
  [`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md)
  reads back what it writes. DOI URLs percent-encode `%`, `#`, and `?`;
  everything else is written as it is. Asking for a form that a type
  doesn’t have, such as a URL for an ISBN, is an error that names the
  forms it has. The forms of each type are in the definitions vignette.

- Accepted each type’s resolver URL and Bioregistry CURIE, older
  resolver URLs that still resolve, and arXiv DOIs in
  [`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md).
  Inputs such as `https://pubmed.ncbi.nlm.nih.gov/12345678/`,
  `pubmed:12345678`,
  `https://pmc.ncbi.nlm.nih.gov/articles/PMC1234567/`,
  `https://portal.issn.org/resource/ISSN/0317-8471`,
  `openalex:W2741809807`, and, for `"arxiv"`,
  `10.48550/arXiv.2101.00001` now normalize instead of giving `NA`. URLs
  may use `http` or `https`, any case, and a slash at the end.
  [`detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.md)
  detects these forms too, and still reports arXiv DOIs as `doi`. The
  forms and their sources are in the definitions vignette.

- Accepted Unicode dashes, such as U+2010 and the en dash, in ORCID,
  ISBN, ISNI, and ISSN values, and full-width digits in every type
  except DOI, ARK, SWHID, and RRID, in
  [`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md),
  [`extract_scholid()`](https://thomas-rauter.github.io/scholid/reference/extract_scholid.md),
  and
  [`detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.md).
  They are read as their ASCII forms.
  [`extract_scholid()`](https://thomas-rauter.github.io/scholid/reference/extract_scholid.md)
  doesn’t read dashes in ISSNs, because an en dash in free text usually
  marks a range, and
  [`is_scholid()`](https://thomas-rauter.github.io/scholid/reference/is_scholid.md)
  still accepts only ASCII. The per-type rules are under “Input
  characters” in the definitions vignette.

- Percent-decoded `doi.org` and `dx.doi.org` URLs in
  `normalize_scholid(x, "doi")`, so `https://doi.org/10.1000%2F182`
  gives `10.1000/182`.

### Documentation

- Added “Input characters”, “Comparison keys”, and “Resolver URLs and
  CURIEs” to the definitions vignette, and a table of resolver URL and
  CURIE forms, with their sources, to each type’s section.

- Added sections on
  [`scholid_key()`](https://thomas-rauter.github.io/scholid/reference/scholid_key.md),
  [`format_scholid()`](https://thomas-rauter.github.io/scholid/reference/format_scholid.md),
  and
  [`locate_scholid()`](https://thomas-rauter.github.io/scholid/reference/locate_scholid.md)
  to the Get started vignette.

## scholid 0.2.1

CRAN release: 2026-09-28

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

### Documentation

- Corrected the DOI case guidance in the definitions vignette: DOI names
  are case-insensitive for ASCII letters, and scholid preserves case
  when validating and normalizing them.

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
