# scholid

[![R-CMD-check](https://github.com/Thomas-Rauter/scholid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Thomas-Rauter/scholid/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Thomas-Rauter/scholid/branch/main/graph/badge.svg)](https://codecov.io/gh/Thomas-Rauter/scholid)

`scholid` provides lightweight, dependency-free utilities for working
with scholarly identifiers in R. The package is designed as a small,
well-tested foundation that can be safely reused by other packages and
data workflows.

## Installation

Install the released version from CRAN:

``` r
install.packages("scholid")
```

## Scope

The package focuses on common identifier systems used in scholarly
communication:

- DOI
- ORCID iD
- ISBN
- ISSN
- arXiv
- PubMed (PMID)
- PubMed Central (PMCID)

## Design principles

- Base R only (no runtime dependencies)
- Vectorized functions with predictable output
- Strict input validation
- Clear separation between detection, normalization, and extraction
- Comprehensive unit tests

## Core interface

The main entry points are generic helpers that dispatch to
identifier-specific implementations:

``` r
scholid::scholid_types()
scholid::is_scholid(x, type)
scholid::normalize_scholid(x, type)
scholid::extract_scholid(text, type)
scholid::classify_scholid(x)
```

Each identifier type also has dedicated helpers such as `is_doi()`,
`normalize_orcid()`, and `extract_isbn()`.

## Example

A minimal example using fully qualified calls:

``` r
x <- c(
  "https://doi.org/10.1000/182",
  "0000-0002-1825-0097"
)

scholid::normalize_scholid(x[1], "doi")
```

``` R
## [1] "10.1000/182"
```

``` r
scholid::is_scholid(x[2], "orcid")
```

``` R
## [1] TRUE
```

For more detailed usage patterns, including extraction from text and
classification of mixed identifier columns, see the **Get started**
vignette.

## License

MIT
