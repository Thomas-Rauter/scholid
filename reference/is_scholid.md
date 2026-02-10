# Check scholarly identifiers

Vectorized predicate for checking whether values match a supported
scholarly identifier type (e.g., DOI, ORCID).

## Usage

``` r
is_scholid(x, type)
```

## Arguments

- x:

  A vector of values to check.

- type:

  A single string giving the identifier type. See
  [`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md)
  for supported values.

## Value

A logical vector with the same length as `x`. `NA` inputs yield `NA`
outputs.

## Examples

``` r
is_scholid("10.1000/182", "doi")
#> [1] TRUE
is_scholid("0000-0002-1825-0097", "orcid")
#> [1] TRUE
is_scholid(c("10.1000/182", NA), "doi")
#> [1] TRUE   NA
```
