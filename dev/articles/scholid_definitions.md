# How Scholarly Identifiers Are Defined

## Introduction

This vignette explains how common scholarly identifiers are formally
defined, what their structural components are, and what it means for
them to be *valid* in a programmatic context.

When working with identifiers in R, it is essential to distinguish
between:

- **Structural validity** (does it match the formal grammar?)
- **Checksum validity** (does the control digit verify?)
- **Registry validity** (does the identifier actually exist?)

The functions in `scholid` validate identifiers at the **structural**
level and verify checksums where defined (ORCID, ROR, ISBN, ISSN). They
do not check registry or online existence. The regexes shown below
describe the structural form that an identifier must match; checksum
rules are documented separately for relevant types.

------------------------------------------------------------------------

## DOI (Digital Object Identifier)

**Governing body:** International DOI Foundation  
**Standard:** ISO 26324

### Structure

A DOI has two parts:

    prefix/suffix

#### Prefix

- Always begins with `10.`
- Followed by a registrant code (4–9 digits)

Example:

    10.1000
    10.1038

#### Suffix

- Assigned by the registrant
- May contain almost any printable character
- Has no globally fixed grammar
- Case-sensitive in theory

Example:

    10.1000/182
    10.1038/s41586-020-2649-2

### Important Properties

- No checksum.
- The suffix is opaque.
- Structural validation cannot confirm existence.
- DOI resolution requires registry lookup (e.g., via doi.org).

### Structural Regex

A commonly accepted structural regex:

    ^10\.\d{4,9}/\S+$

This checks: - Prefix starts with `10.` - 4–9 digits - A slash -
Non-whitespace suffix

------------------------------------------------------------------------

## ISNI (International Standard Name Identifier)

**Governing body:** ISNI International Agency  
**Standard:** ISO 27729  
**Documentation:** [ISNI](https://isni.org/)

### Structure

An ISNI uniquely identifies public identities of contributors to media
content. The identifier is 16 characters: 15 decimal digits plus a check
character.

Compact canonical form:

    000000012146438X

Human-readable presentation uses an `ISNI` prefix and spaces in blocks
of four:

    ISNI 0000 0001 2146 438X

Preferred resolver URLs include:

    https://isni.org/isni/000000012146438X

ORCID iDs use the same ISO/IEC 7064 MOD 11-2 checksum on 16 characters
but are canonicalized in `scholid` with hyphens. Compact checksum-valid
16-character strings are treated as ISNI; hyphenated strings are treated
as ORCID.

### Checksum

Uses ISO/IEC 7064 MOD 11-2, identical to ORCID. The check character may
be `0`–`9` or `X`.

### Structural Regex

Compact canonical form:

    ^\d{15}[\dX]$

------------------------------------------------------------------------

## ORCID

**Governing body:** ORCID, Inc.  
**Standard basis:** ISO 7064 (checksum algorithm)

### Structure

An ORCID iD consists of 16 characters:

    0000-0002-1825-0097

#### Components

- 16 digits total
- Grouped as 4-4-4-4
- Final character is a checksum digit
- Check digit may be `X`

Internally (without hyphens):

    0000000218250097

### Checksum

Uses ISO 7064 Mod 11-2 algorithm.  
A structurally correct ORCID may still be invalid if the checksum does
not match.

### Structural Regex

Hyphenated form:

    ^\d{4}-\d{4}-\d{4}-\d{3}[\dX]$

Unhyphenated internal form:

    ^\d{15}[\dX]$

------------------------------------------------------------------------

## ROR (Research Organization Registry)

**Governing body:** ROR Community  
**Documentation:** [ROR identifier
pattern](https://ror.readme.io/docs/identifier)

### Structure

A ROR iD is a 9-character lowercase string:

    0abcdef94

Preferred external form is the full URL:

    https://ror.org/01an7q238

### Checksum

The last two characters are a checksum derived from the preceding seven
characters using Crockford base32 encoding and ISO/IEC 7064 MOD 97-10
rules, matching ROR’s identifier generation implementation.

### Structural Regex

Compact form:

    ^0[a-hjkmnp-tv-z0-9]{6}[0-9]{2}$

------------------------------------------------------------------------

## RRID (Research Resource Identifier)

**Governing body:** Resource Identification Initiative (SciCrunch)  
**Documentation:** [RRID guidelines](https://rrid.site/about/guidelines)

### Structure

A RRID cites a research resource such as an antibody, cell line, model
organism, software tool, or plasmid. The canonical form includes the
literal `RRID:` prefix followed by an authority-specific accession:

    RRID:AB_262044
    RRID:CVCL_2260
    RRID:SCR_007358
    RRID:IMSR_JAX:000664
    RRID:MGI:3840442
    RRID:Addgene_80088

Preferred resolver URLs include:

    https://scicrunch.org/resolver/RRID:AB_262044

### Validation in scholid

RRID validation is **structural only**. There is no checksum algorithm,
and registry existence is not checked.

To limit false positives, `scholid` accepts only canonical
`RRID:`-prefixed forms and validates the accession body against a
conservative allowlist of known RRID authority prefixes (for example
`AB`, `CVCL`, `SCR`, `IMSR`, `MGI`, `Addgene`). Bare local IDs such as
`AB_262044` without the `RRID:` prefix are rejected.

### Structural Regex

Canonical form (authority body validated separately):

    ^RRID:.+$

------------------------------------------------------------------------

## ISBN (International Standard Book Number)

**Governing body:** International ISBN Agency  
**Standard:** ISO 2108

### Two Forms

#### ISBN-10

- 9 digits + checksum digit
- Check digit may be `X`

Example:

    0306406152
    030640615X

#### ISBN-13

- 13 digits
- Usually begins with 978 or 979
- EAN-13 checksum algorithm

Example:

    9780306406157

### Structural Regex

ISBN-10:

    ^\d{9}[\dX]$

ISBN-13:

    ^\d{13}$

------------------------------------------------------------------------

## ISSN (International Standard Serial Number)

**Governing body:** ISSN International Centre  
**Standard:** ISO 3297

### Structure

An ISSN has 8 characters:

    1234-567X

#### Components

- 7 digits
- 1 checksum digit (0–9 or X)
- Canonical display includes a hyphen after 4 digits

Internal numeric form:

    1234567X

### Structural Regex

Hyphenated:

    ^\d{4}-\d{3}[\dX]$

Compact form:

    ^\d{7}[\dX]$

------------------------------------------------------------------------

## arXiv Identifier

**Authority:** arXiv (Cornell University)

### Two Formats

#### Modern (post-2007)

    YYMM.NNNN
    YYMM.NNNNN

Optional version suffix:

    YYMM.NNNN(v2)

Components: - 4-digit year/month - Dot - 4–5 digit submission number -
Optional version `vN`

Structural regex:

    ^\d{4}\.\d{4,5}(v\d+)?$

------------------------------------------------------------------------

#### Legacy (pre-2007)

    archive/YYMMNNN

Example:

    hep-th/9901001

Structural regex:

    ^[a-z\-]+/\d{7}(v\d+)?$

------------------------------------------------------------------------

## ADS Bibcode

**Authority:** SAO/NASA Astrophysics Data System (ADS)  
**Documentation:** [ADS bibliographic
codes](https://adsabs.harvard.edu/abs_doc/help_pages/data.html)

### Structure

An ADS bibcode is a fixed **19-character** identifier for bibliographic
records in astronomy and related fields. The format follows SIMBAD/NED
conventions:

    YYYYJJJJJVVVVM PPPPA

Where:

- `YYYY` — publication year (four digits)
- `JJJJJ` — journal abbreviation, left-justified, padded with `.`
- `VVVV` — volume, right-justified, padded with `.`
- `M` — qualifier (e.g. `L` for letters)
- `PPPP` — page, right-justified, padded with `.`
- `A` — first letter of the first author’s surname

Example:

    1992ApJ...400L...1W

Preferred resolver URLs include:

    https://ui.adsabs.harvard.edu/abs/1992ApJ...400L...1W

### Validation in scholid

Bibcode validation is **structural only**. There is no checksum
algorithm, and ADS existence is not checked.

To limit false positives, `scholid` requires exactly 19 characters, a
letter in the journal field, and a letter as the final author-initial
character. Case is preserved in canonical form.

### Structural Regex

    ^\d{4}[A-Za-z0-9.]{14}[A-Za-z]$

------------------------------------------------------------------------

## OpenAlex ID

**Governing body:** OurResearch (OpenAlex)  
**Documentation:** [OpenAlex key
concepts](https://developers.openalex.org/guides/key-concepts)

### Structure

Every OpenAlex entity has a persistent ID. The official form is a URL:

    https://openalex.org/W2741809807

The short key (`W2741809807`) is commonly used in API calls and tabular
data. Keys are case-insensitive; `scholid` canonicalizes them to
uppercase.

A key consists of:

- a single letter prefix indicating entity type (`W`, `A`, `S`, `I`,
  `T`, `K`, `P`, `F`, or `G`)
- a numeric tail (at least five digits)

Examples:

    W2741809807
    A5023888391
    I97018004

### Validation in scholid

OpenAlex validation is **structural only**. There is no checksum
algorithm, and registry existence is not checked.

Deprecated concept IDs (`C` prefix) are not accepted. Bare keys are
accepted only when they match the structural pattern; wrapped URLs
should be normalized with
[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md)
before classification.

Works, authors, and institutions in OpenAlex often also have DOI, ORCID,
or ROR identifiers respectively; those types are checked earlier during
classification.

### Structural Regex

Canonical uppercase key:

    ^[WASTIKPFG][0-9]{5,}$

------------------------------------------------------------------------

## SWHID (SoftWare Hash IDentifier)

**Governing body:** Software Heritage  
**Standard:** ISO/IEC 18670  
**Documentation:** [SWHID
specification](https://docs.softwareheritage.org/devel/swh-model/persistent-identifiers.html)

### Structure

A SWHID identifies a software artifact archived by Software Heritage.
The core identifier has four colon-separated fields:

    swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2

Where:

- `swh` is the scheme prefix
- `1` is the scheme version
- `cnt` is the object type (`cnt`, `dir`, `rev`, `rel`, or `snp`)
- the final field is a 40-character lowercase hex SHA-1 intrinsic
  identifier

Optional qualifiers may follow, separated by semicolons:

    swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;origin=https://example.org/repo.git;path=/src/main.c;lines=9-15

Resolver URLs include:

    https://archive.softwareheritage.org/swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2

### Validation in scholid

SWHID validation is **structural only**. The embedded hash is an
intrinsic content identifier, but verifying that it matches the
referenced artifact requires access to the artifact itself and is not
performed by `scholid`.

To limit false positives, `scholid` requires the explicit `swh:` prefix
and rejects bare 40-character hex strings (for example Git commit
hashes). Known qualifier keys (`origin`, `visit`, `anchor`, `path`,
`lines`) are validated conservatively when present.

### Structural Regex

Core form:

    ^swh:1:(cnt|dir|rev|rel|snp):[0-9a-f]{40}$

------------------------------------------------------------------------

## PMID (PubMed Identifier)

**Authority:** U.S. National Library of Medicine

### Structure

- Pure integer
- Variable length
- No checksum

Example:

    12345678

Structural regex:

    ^\d+$

------------------------------------------------------------------------

## PMCID (PubMed Central Identifier)

**Authority:** PubMed Central

### Structure

    PMC1234567

Components: - Literal prefix `PMC` - One or more digits

Structural regex:

    ^PMC\d+$
