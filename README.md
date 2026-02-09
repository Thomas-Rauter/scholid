# scholid

[![R-CMD-check](https://github.com/Thomas-Rauter/scholid/actions/workflows/
R-CMD-check.yaml/badge.svg)](
https://github.com/Thomas-Rauter/scholid/actions/workflows/
R-CMD-check.yaml)

[![Codecov test coverage](https://codecov.io/gh/Thomas-Rauter/scholid/
branch/main/graph/badge.svg)](https://codecov.io/gh/Thomas-Rauter/scholid)

`scholid` provides lightweight, dependency-free utilities for working with
scholarly and academic identifiers in R. The package is designed as a small,
well-tested foundation that can be safely reused by other packages and data
workflows.

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

```r
scholid_types()
is_scholid(x, type)
normalize_scholid(x, type)
extract_scholid(text, type)
classify_scholid(x)
```

Each identifier type also has dedicated helpers such as `is_doi()`,
`normalize_orcid()`, and `extract_isbn()`.

## Example

```r
x <- c(
  "https://doi.org/10.1000/182",
  "0000-0002-1825-0097"
)

normalize_scholid(x[1], "doi")
is_scholid(x[2], "orcid")
```

## Status

The API is intentionally small and stable. The package is under active
development, with a strong emphasis on correctness and long-term
maintainability.

## License

MIT
