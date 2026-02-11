# Normalize scholarly identifiers

Vectorized normalizer that converts supported scholarly identifier
values to a canonical form (e.g., removing URL prefixes, labels, or
separators).

Normalization is structural: inputs that conform to the expected
identifier syntax are converted to a canonical representation. Inputs
that do not match the required structure yield `NA_character_`.

For identifier types with checksum algorithms (e.g., ORCID, ISBN, ISSN),
normalization does not verify checksum correctness. It only enforces
structural plausibility and canonical formatting.

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

A character vector with the same length as `x`. Invalid or structurally
non-matching inputs yield `NA_character_`.

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
