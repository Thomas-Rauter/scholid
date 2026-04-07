# Test scholarly identifier validity

Vectorized predicate that tests whether values are valid scholarly
identifiers of a given supported type.

Validation is stricter than normalization. Values must conform to the
canonical identifier syntax, and for identifier types with checksum
algorithms (e.g., ORCID, ISBN, ISSN), checksum correctness is verified.

Inputs that are `NA` yield `NA`. Non-matching values return `FALSE`.

Use
[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/dev/reference/normalize_scholid.md)
to convert structurally plausible identifiers to canonical form without
performing checksum validation.

## Usage

``` r
is_scholid(x, type)
```

## Arguments

- x:

  A vector of values to test.

- type:

  A single string giving the identifier type. See
  [`scholid_types()`](https://thomas-rauter.github.io/scholid/dev/reference/scholid_types.md)
  for supported values.

## Value

A logical vector of the same length as `x`, indicating whether each
element is a valid identifier of the specified type.

## See also

[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/dev/reference/normalize_scholid.md),
[`scholid_types()`](https://thomas-rauter.github.io/scholid/dev/reference/scholid_types.md)

## Examples

``` r
is_scholid("10.1000/182", "doi")
#> [1] TRUE
is_scholid("0000-0002-1825-0097", "orcid")
#> [1] TRUE
```
