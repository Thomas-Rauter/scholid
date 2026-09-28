# Extract scholarly identifiers from text

Extract identifiers of a single supported type from free text.

The result is a list with one element per input element. Each element is
a character vector of matches (possibly length 0). `NA` inputs yield an
empty character vector.

Matches are returned as extracted identifier tokens from the text.
Surrounding prose punctuation or markup fragments may be removed where
necessary to isolate the identifier. Use
[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md)
to convert identifiers to canonical form.

Before matching, invisible characters are removed from the text and
Unicode spaces count as whitespace. For some types, Unicode dashes and
full-width digits are read as their ASCII forms, and the returned tokens
use the ASCII forms. See "Input characters" in
[`vignette("scholid_definitions", package = "scholid")`](https://thomas-rauter.github.io/scholid/articles/scholid_definitions.md).

## Usage

``` r
extract_scholid(text, type)
```

## Arguments

- text:

  A character vector of text.

- type:

  A single string giving the identifier type. See
  [`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md)
  for supported values.

## Value

A list of character vectors of extracted identifiers.

## See also

[`locate_scholid()`](https://thomas-rauter.github.io/scholid/reference/locate_scholid.md)
to find identifiers of several types at once, with their positions in
the text.

## Examples

``` r
extract_scholid("See https://doi.org/10.1000/182.", "doi")
#> [[1]]
#> [1] "10.1000/182"
#> 
extract_scholid("ORCID 0000-0002-1825-0097", "orcid")
#> [[1]]
#> [1] "0000-0002-1825-0097"
#> 
```
