# Getting started with scholid

`scholid` is a lightweight, dependency-free (base R only) toolkit for
working with scholarly and academic identifiers. It provides small,
well-tested helpers to detect, normalize, compare, format, classify,
extract, and locate common identifier strings.

This vignette introduces the interface and typical workflows for mixed,
messy identifier data.

## Installation

``` r

install.packages("scholid")
```

## Interface

`scholid` exposes a small set of user-facing functions that operate
consistently across identifier types:

- [`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md)
  lists supported identifier types.
- `is_scholid(x, type)` checks whether values match the identifier type.
- `normalize_scholid(x, type)` returns canonical identifier strings.
- `scholid_key(x, type)` returns keys for comparing and deduplicating
  identifiers.
- `format_scholid(x, type, as)` writes identifiers as resolver URLs,
  CURIEs, or arXiv DOIs.
- `extract_scholid(text, type)` extracts identifiers from free text.
- `locate_scholid(text, types)` finds identifiers of several types in
  free text, with their positions.
- `classify_scholid(x)` guesses the identifier type per element.
- `detect_scholid_type(x)` detects identifier types from canonical or
  wrapped input values (e.g., URLs or labels).

These generic helpers dispatch internally to type-specific
implementations such as `is_doi()`, `normalize_orcid()`, and
`extract_isbn()`.

## Supported identifier types

``` r

scholid::scholid_types()
```

    ##  [1] "doi"        "arxiv"      "bibcode"    "openalex"   "swhid"     
    ##  [6] "ark"        "isni"       "orcid"      "ror"        "rrid"      
    ## [11] "uniprot"    "refseq"     "sra"        "geo"        "bioproject"
    ## [16] "assembly"   "isbn"       "issn"       "pmcid"      "pmid"

For per-type formats, validation rules, and classification order, see
the **How Scholarly Identifiers Are Defined** vignette
([`vignette("scholid_definitions", package = "scholid")`](https://thomas-rauter.github.io/scholid/articles/scholid_definitions.md)),
also linked from the package site as *About identifiers*.

## Detect: `is_scholid()`

[`is_scholid()`](https://thomas-rauter.github.io/scholid/reference/is_scholid.md)
checks whether each value is a valid identifier of a specific type. It
expects canonical (or near-canonical) input; wrapped forms such as URLs
should be normalized first. For types that define a checksum, both
[`is_scholid()`](https://thomas-rauter.github.io/scholid/reference/is_scholid.md)
and
[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md)
verify it; the definitions vignette says which types do. It is
vectorized and preserves missing values.

``` r

x <- c(
    "10.1000/182",
    "not a doi",
    NA
)
scholid::is_scholid(
    x    = x,
    type = "doi"
)
```

    ## [1]  TRUE FALSE    NA

## Normalize: `normalize_scholid()`

Normalization removes common wrappers and enforces a canonical
representation. This is particularly useful when identifiers are stored
as URLs or prefixed labels.

``` r

x <- c(
  "https://doi.org/10.1000/182.",
  "doi:10.1000/182",
  " 10.1000/182 "
)
scholid::normalize_scholid(
    x    = x, 
    type = "doi"
)
```

    ## [1] "10.1000/182" "10.1000/182" "10.1000/182"

For ORCID iDs, normalization removes URL prefixes and enforces
hyphenated grouping.

``` r

x <- c(
  "https://orcid.org/0000-0002-1825-0097",
  "0000000218250097"
)
scholid::normalize_scholid(
    x    = x,
    type = "orcid"
)
```

    ## [1] "0000-0002-1825-0097" "0000-0002-1825-0097"

Normalization is designed to be predictable: - `NA` input stays `NA`. -
Invalid inputs typically become `NA_character_`.

## Compare: `scholid_key()`

Normalization keeps apart values that name the same thing in different
ways: an ISBN-10 and its ISBN-13, versions of one arXiv preprint, or
DOIs that differ only in the case of ASCII letters.
[`scholid_key()`](https://thomas-rauter.github.io/scholid/reference/scholid_key.md)
normalizes values and gives them keys that match, for use with
[`duplicated()`](https://rdrr.io/r/base/duplicated.html),
[`unique()`](https://rdrr.io/r/base/unique.html),
[`match()`](https://rdrr.io/r/base/match.html), or
[`merge()`](https://rdrr.io/r/base/merge.html).

``` r

x <- c(
  "0306406152",
  "ISBN 978-0-306-40615-7",
  "0-8044-2957-X",
  "9780804429573"
)
keys <- scholid::scholid_key(
    x    = x,
    type = "isbn"
)
keys
```

    ## [1] "9780306406157" "9780306406157" "9780804429573" "9780804429573"

``` r

x[!duplicated(keys)]
```

    ## [1] "0306406152"    "0-8044-2957-X"

``` r

scholid::scholid_key(
    x    = c("2101.00001v1", "arXiv:2101.00001v2"),
    type = "arxiv"
)
```

    ## [1] "2101.00001" "2101.00001"

``` r

scholid::scholid_key(
    x    = c("10.1000/abc", "https://doi.org/10.1000/ABC"),
    type = "doi"
)
```

    ## [1] "10.1000/ABC" "10.1000/ABC"

Keys are for comparing only. Some don’t pass
[`is_scholid()`](https://thomas-rauter.github.io/scholid/reference/is_scholid.md),
such as the RefSeq key `NM_000546` without its version, so keep the
normalized values for storing and showing identifiers. The key rule of
each type is in the definitions vignette.

## Write: `format_scholid()`

[`format_scholid()`](https://thomas-rauter.github.io/scholid/reference/format_scholid.md)
writes identifiers as resolver URLs (the default), as CURIEs, or, for
arXiv preprints, as the DOIs that arXiv registers. It normalizes its
input first, so wrapped values work, and values that don’t normalize
give `NA`. For links in a report:

``` r

x <- c(
  "doi:10.7717/peerj.4375",
  "https://doi.org/10.1000/182",
  "not a doi"
)
urls <- scholid::format_scholid(
    x    = x,
    type = "doi"
)
urls
```

    ## [1] "https://doi.org/10.7717/peerj.4375" "https://doi.org/10.1000/182"       
    ## [3] NA

[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md)
reads the output back:

``` r

scholid::normalize_scholid(
    x    = urls,
    type = "doi"
)
```

    ## [1] "10.7717/peerj.4375" "10.1000/182"        NA

CURIEs use the type’s [Bioregistry](https://bioregistry.io) prefix:

``` r

scholid::format_scholid(
    x    = c("PMID: 12345678", "https://pubmed.ncbi.nlm.nih.gov/7654321/"),
    type = "pmid",
    as   = "curie"
)
```

    ## [1] "pubmed:12345678" "pubmed:7654321"

The arXiv DOI names the preprint, so it leaves out the version:

``` r

scholid::format_scholid(
    x    = c("arXiv:1706.03762v7", "hep-th/9901001"),
    type = "arxiv",
    as   = "doi"
)
```

    ## [1] "10.48550/arXiv.1706.03762"     "10.48550/arXiv.hep-th/9901001"

Not every type has every form. ISBNs, for example, have no resolver URL,
and asking for one is an error that names the forms the type has:

``` r

scholid::format_scholid(
    x    = "978-0-306-40615-7",
    type = "isbn",
    as   = "url"
)
```

    ## Error:
    ## ! Type "isbn" has no "url" form. Available forms for "isbn": "curie".

The forms of each type are listed under “Resolver URLs and CURIEs” in
the definitions vignette.

## Extract: `extract_scholid()`

Extraction is for harvesting identifiers from unstructured text. The
result is a list with one element per input element. Each element is a
character vector of matches (possibly empty).

``` r

txt <- c(
  "See https://doi.org/10.1000/182 and doi:10.5555/12345678.",
  "No identifier here.",
  NA
)
scholid::extract_scholid(
    text = txt,
    type = "doi"
)
```

    ## [[1]]
    ## [1] "10.1000/182"      "10.5555/12345678"
    ## 
    ## [[2]]
    ## character(0)
    ## 
    ## [[3]]
    ## character(0)

The list return type is intentional: a single text string can contain
multiple identifiers.

## Locate: `locate_scholid()`

[`locate_scholid()`](https://thomas-rauter.github.io/scholid/reference/locate_scholid.md)
looks for identifiers of all supported types at once, or of the types
given in `types`, and reports where each one is. The result is a data
frame with one row per identifier: the element of `text` it was found
in, its type, the token
[`extract_scholid()`](https://thomas-rauter.github.io/scholid/reference/extract_scholid.md)
returns for it, the identifier as written, and its start and end
positions.

``` r

txt <- c(
  "See doi:10.1000/182 and ORCID 0000-0002-1825-0097.",
  NA,
  "ISBN 978 0 306 40615 7 (PMID 12345678)"
)
hits <- scholid::locate_scholid(text = txt)
hits
```

    ##   element  type                  id               match start end
    ## 1       1   doi         10.1000/182         10.1000/182     9  19
    ## 2       1 orcid 0000-0002-1825-0097 0000-0002-1825-0097    31  49
    ## 3       3  isbn   978 0 306 40615 7   978 0 306 40615 7     6  22
    ## 4       3  pmid            12345678            12345678    30  37

``` r

substr(txt[hits$element], hits$start, hits$end)
```

    ## [1] "10.1000/182"         "0000-0002-1825-0097" "978 0 306 40615 7"  
    ## [4] "12345678"

The digits `40615` inside the ISBN also look like a PMID. Where the
spans of two hits overlap,
[`locate_scholid()`](https://thomas-rauter.github.io/scholid/reference/locate_scholid.md)
keeps the longer one, and for equal lengths the type that comes first in
[`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md).
Only the types asked for take part:

``` r

scholid::locate_scholid(
    text  = txt[3],
    types = "pmid"
)
```

    ##   element type       id    match start end
    ## 1       1 pmid    40615    40615    16  20
    ## 2       1 pmid 12345678 12345678    30  37

## Classify: `classify_scholid()`

[`classify_scholid()`](https://thomas-rauter.github.io/scholid/reference/classify_scholid.md)
returns the best-guess identifier type per element for mixed identifier
columns. Classification is based on the set of available `is_<type>()`
checks and the precedence order defined by
[`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md).

``` r

x <- c(
  "10.1000/182",
  "0000-0002-1825-0097",
  "PMC12345",
  "2101.00001v2",
  "not an id",
  NA
)
scholid::classify_scholid(x = x)
```

    ## [1] "doi"   "orcid" "pmcid" "arxiv" NA      NA

### Normalization + classification in messy data

Many identifiers appear wrapped (URLs, prefixes, trailing punctuation).
Classification is strict and expects canonical strings. A common pattern
is:

1.  Extract identifiers from text.
2.  Normalize extracted values.
3.  Classify and/or validate.

``` r

txt <- "Read https://doi.org/10.1000/182 (and ORCID 0000-0002-1825-0097)."
dois <- scholid::extract_scholid(txt, "doi")[[1]]
orcids <- scholid::extract_scholid(txt, "orcid")[[1]]

dois_n <- scholid::normalize_scholid(dois, "doi")
orcids_n <- scholid::normalize_scholid(orcids, "orcid")

scholid::classify_scholid(c(dois_n, orcids_n))
```

    ## [1] "doi"   "orcid"

``` r

scholid::is_scholid(dois_n, "doi")
```

    ## [1] TRUE

``` r

scholid::is_scholid(orcids_n, "orcid")
```

    ## [1] TRUE

## Detect: `detect_scholid_type()`

[`detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.md)
performs best-effort type detection for mixed, messy identifier input.
In contrast to
[`classify_scholid()`](https://thomas-rauter.github.io/scholid/reference/classify_scholid.md),
detection also recognizes common wrapped forms such as URLs and prefixed
labels (e.g., `doi:`, `https://orcid.org/`, `arXiv:`, `PMID:`).

Detection is useful when working with raw data where identifiers may not
yet be normalized.

For example, wrapped identifiers are not classified strictly:

``` r

x <- c(
  "https://doi.org/10.1000/182",
  "ORCID: 0000-0002-1825-0097",
  "arXiv:2101.00001",
  "PMID: 12345",
  "not an id"
)
scholid::classify_scholid(x)
```

    ## [1] NA NA NA NA NA

However, they can be detected directly:

``` r

scholid::detect_scholid_type(x)
```

    ## [1] "doi"   "orcid" "arxiv" "pmid"  NA

Whitespace and minor formatting irregularities are handled
conservatively:

``` r

scholid::detect_scholid_type(
  c(
    " 0000-0002-1825-0097 ",
    " 10.1000/182 ",
    "ISSN 0317-8471"
  )
)
```

    ## [1] "orcid" "doi"   "issn"

[`detect_scholid_type()`](https://thomas-rauter.github.io/scholid/reference/detect_scholid_type.md)
does not modify values. Once the identifier type is known, use
[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md)
to convert wrapped input to canonical form and
[`is_scholid()`](https://thomas-rauter.github.io/scholid/reference/is_scholid.md)
to validate already-canonical values. Both apply checksum verification
where applicable.

A typical workflow for messy data is:

1.  Detect identifier types.
2.  Normalize by detected type.
3.  Validate canonical identifiers.

This separation keeps detection permissive, normalization focused on
canonicalization of wrapped input, and validation available for
already-canonical strings.

## Design notes

`scholid` is intentionally small and conservative:

- It uses base R only at runtime.
- Functions are vectorized and return stable types.
- Type-specific logic is kept in small `is_*()`, `normalize_*()`,
  `key_*()`, and `extract_*()` helpers.
- The package is designed to be a low-level building block for other
  packages and for workflows.
