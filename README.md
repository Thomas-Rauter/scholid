
# scholid

[![R-CMD-check](https://github.com/Thomas-Rauter/scholid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Thomas-Rauter/scholid/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://img.shields.io/codecov/c/github/Thomas-Rauter/scholid?branch=main&logo=codecov)](https://app.codecov.io/gh/Thomas-Rauter/scholid)
[![CRAN
since](https://img.shields.io/badge/CRAN%20since-Feb%202026-blue)](https://CRAN.R-project.org/package=scholid)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/scholid)](https://CRAN.R-project.org/package=scholid)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/last-month/scholid)](https://CRAN.R-project.org/package=scholid)

`scholid` provides lightweight, dependency-free utilities for working
with scholarly identifiers in R. The package is designed as a small,
well-tested foundation that can be safely reused by other packages and
data workflows. For the supported identifier types, see Scope and
`scholid_types()`.

See the full documentation at the [scholid
website](https://thomas-rauter.github.io/scholid/).

For online lookup, conversion, metadata retrieval, and linked identifier
discovery, see
[`scholidonline`](https://thomas-rauter.github.io/scholidonline/).

## Installation

Install the released version from CRAN:

``` r
install.packages("scholid")
```

## Scope

The package focuses on common identifier systems used in scholarly
communication:

- DOI
- arXiv
- ADS bibcode
- OpenAlex
- Software Heritage (SWHID)
- ARK
- ISNI
- ORCID iD
- ROR
- RRID
- UniProt
- RefSeq
- SRA
- GEO
- BioProject
- Genome assembly (GCA/GCF)
- ISBN
- ISSN
- PubMed Central (PMCID)
- PubMed (PMID)

## Interface

User-available functions:

| Function                      | Purpose                                                              |
|-------------------------------|----------------------------------------------------------------------|
| `scholid_types()`             | List supported scholarly identifier types                            |
| `is_scholid(x, type)`         | Test whether values conform to a given identifier type               |
| `normalize_scholid(x, type)`  | Normalize identifiers to canonical form                              |
| `scholid_key(x, type)`        | Give keys for comparing and deduplicating identifiers                |
| `format_scholid(x, type, as)` | Write identifiers as resolver URLs, CURIEs, or arXiv DOIs            |
| `extract_scholid(text, type)` | Extract identifiers of a given type from free text                   |
| `locate_scholid(text, types)` | Find identifiers of several types in free text, with their positions |
| `classify_scholid(x)`         | Guess the identifier type of each input value                        |
| `detect_scholid_type(x)`      | Detect identifier types from canonical or wrapped input values       |

## Examples

``` r
# list supported scholarly identifier types
scholid::scholid_types()
```

    ##  [1] "doi"        "arxiv"      "bibcode"    "openalex"   "swhid"     
    ##  [6] "ark"        "isni"       "orcid"      "ror"        "rrid"      
    ## [11] "uniprot"    "refseq"     "sra"        "geo"        "bioproject"
    ## [16] "assembly"   "isbn"       "issn"       "pmcid"      "pmid"

``` r
# test whether values match a given identifier type
scholid::is_scholid(
  x    = "10.1000/182",
  type = "doi"
)
```

    ## [1] TRUE

``` r
# normalize identifiers to canonical form
scholid::normalize_scholid(
  x    = "https://doi.org/10.1000/182",
  type = "doi"
)
```

    ## [1] "10.1000/182"

``` r
# give keys that match an ISBN-10 and its ISBN-13
scholid::scholid_key(
  x    = c("0306406152", "978-0-306-40615-7"),
  type = "isbn"
)
```

    ## [1] "9780306406157" "9780306406157"

``` r
# write identifiers as resolver URLs or CURIEs
scholid::format_scholid(
  x    = "PMID: 12345678",
  type = "pmid"
)
```

    ## [1] "https://pubmed.ncbi.nlm.nih.gov/12345678/"

``` r
scholid::format_scholid(
  x    = "0000-0002-1825-0097",
  type = "orcid",
  as   = "curie"
)
```

    ## [1] "orcid:0000-0002-1825-0097"

``` r
# extract identifiers of a given type from free text
scholid::extract_scholid(
  text = "See https://doi.org/10.1000/182 for details.",
  type = "doi"
)
```

    ## [[1]]
    ## [1] "10.1000/182"

``` r
# locate identifiers of all types in free text, with their positions
scholid::locate_scholid(
  text = "See doi:10.1000/182 and ISBN 978-0-306-40615-7."
)
```

    ##   element type                id             match start end
    ## 1       1  doi       10.1000/182       10.1000/182     9  19
    ## 2       1 isbn 978-0-306-40615-7 978-0-306-40615-7    30  46

``` r
# classify the identifier type of each input value
scholid::classify_scholid(
  x = c(
    "10.1000/182",
    "0000-0002-1825-0097",
    "not an id"
  )
)
```

    ## [1] "doi"   "orcid" NA

``` r
# detect identifier types from canonical or wrapped input values
scholid::detect_scholid_type(
  x = c(
    "https://doi.org/10.1000/182",
    "ORCID: 0000-0002-1825-0097",
    "arXiv:2101.00001",
    "not an id"
  )
)
```

    ## [1] "doi"   "orcid" "arxiv" NA

For more detailed usage patterns check out the **Get started** vignette.

## License

MIT
