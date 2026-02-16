# Extract scholarly identifiers from text

Extract identifiers of a single supported type from free text.

The result is a list with one element per input element. Each element is
a character vector of matches (possibly length 0). `NA` inputs yield an
empty character vector.

Matches are returned as found in the text; use
[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/dev/reference/normalize_scholid.md)
to convert identifiers to canonical form.

## Usage

``` r
extract_scholid(text, type)
```

## Arguments

- text:

  A character vector of text.

- type:

  A single string giving the identifier type. See
  [`scholid_types()`](https://thomas-rauter.github.io/scholid/dev/reference/scholid_types.md)
  for supported values.

## Value

A list of character vectors of extracted identifiers.

## Examples

``` r
extract_scholid("See https://doi.org/10.1000/182.", "doi")
#> [[1]]
#> [1] "10.1000/182."
#> 
extract_scholid("ORCID 0000-0002-1825-0097", "orcid")
#> [[1]]
#> [1] "0000-0002-1825-0097"
#> 
```
