# Detect scholarly identifier types

Performs best-effort detection of scholarly identifier types from
possibly wrapped identifier strings (e.g., URLs or labels).

For each element of the input, the function returns the first matching
identifier type, or `NA_character_` if no supported type matches.

Detection first attempts classification based on canonical identifier
syntax (see
[`classify_scholid()`](https://thomas-rauter.github.io/scholid/dev/reference/classify_scholid.md)).
If no match is found, the function attempts per-type normalization (see
[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/dev/reference/normalize_scholid.md))
and returns the first type for which normalization yields a non-missing
result.

Use
[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/dev/reference/normalize_scholid.md)
to convert detected values to canonical form once the identifier type is
known.

## Usage

``` r
detect_scholid_type(x)
```

## Arguments

- x:

  A vector of candidate identifier values.

## Value

A character vector of the same length as `x`, giving the detected
identifier type for each element, or `NA_character_` if no match is
found.

## See also

[`classify_scholid()`](https://thomas-rauter.github.io/scholid/dev/reference/classify_scholid.md),
[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/dev/reference/normalize_scholid.md),
[`scholid_types()`](https://thomas-rauter.github.io/scholid/dev/reference/scholid_types.md)

## Examples

``` r
detect_scholid_type(c(
  "https://doi.org/10.1000/182",
  "doi:10.1000/182",
  "https://orcid.org/0000-0002-1825-0097",
  "arXiv:2101.12345v2",
  "PMID: 12345678",
  "PMCID: PMC1234567",
  "not an id"
))
#> [1] "doi"   "doi"   "orcid" "arxiv" "issn"  "pmcid" NA     
```
