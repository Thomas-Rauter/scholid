# Normalize scholarly identifiers

Vectorized normalizer that converts supported scholarly identifier
values to a canonical form (e.g., removing URL prefixes, labels, or
separators).

Normalization requires that inputs match the expected identifier
structure. For identifier types with checksum algorithms, normalization
also requires checksum-valid values. Inputs that do not meet these
requirements yield `NA_character_`.

Normalized outputs are canonical, type-specific representations of valid
identifiers.

Use
[`is_scholid()`](https://thomas-rauter.github.io/scholid/reference/is_scholid.md)
to test whether values are fully valid identifiers, including checksum
verification where applicable.

## Usage

``` r
normalize_scholid(x, type)
```

## Arguments

- x:

  A vector of values to normalize.

- type:

  A single string giving the identifier type. See
  [`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md)
  for supported values.

## Value

A character vector with the same length as `x`. Invalid, checksum-
failing, or structurally non-matching inputs yield `NA_character_`.

## See also

[`is_scholid()`](https://thomas-rauter.github.io/scholid/reference/is_scholid.md),
[`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md)

## Examples

``` r
normalize_scholid("https://doi.org/10.1000/182", "doi")
#> [1] "10.1000/182"
normalize_scholid("https://orcid.org/0000-0002-1825-0097", "orcid")
#> [1] "0000-0002-1825-0097"
```
