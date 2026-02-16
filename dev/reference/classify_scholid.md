# Classify scholarly identifiers

Performs best-guess classification of scholarly identifier strings. For
each element of the input, the function returns the first matching
identifier type, or `NA_character_` if no supported type matches.

Classification is based on canonical identifier syntax. Wrapped forms
(e.g., URLs or labels) should be normalized first with
[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md).

## Usage

``` r
classify_scholid(x)
```

## Arguments

- x:

  A vector of candidate identifier values.

## Value

A character vector of the same length as `x`, giving the detected
identifier type for each element, or `NA_character_` if no match is
found.

## Examples

``` r
classify_scholid(c("10.1000/182", "0000-0002-1825-0097", "not an id"))
#> [1] "doi"   "orcid" NA     
classify_scholid(normalize_scholid("https://doi.org/10.1000/182", "doi"))
#> [1] "doi"
```
