# Comparison keys for scholarly identifiers

Vectorized helper that gives each value a key for comparing identifiers.
Values that name the same thing get the same key, even when they are
written differently: an ISBN-10 and its ISBN-13, versions of one arXiv
preprint or RefSeq sequence, or DOIs that differ only in the case of
ASCII letters. Use keys with
[`duplicated()`](https://rdrr.io/r/base/duplicated.html),
[`unique()`](https://rdrr.io/r/base/unique.html),
[`match()`](https://rdrr.io/r/base/match.html), and
[`merge()`](https://rdrr.io/r/base/merge.html).

`x` is first normalized with the rules of
[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md),
so wrapped input such as URLs and labels works. Then the type's key rule
is applied. For a type without a key rule, the key is the normalized
value. The rules are in each type's "Validation in scholid" section of
the *How Scholarly Identifiers Are Defined* vignette
([`vignette("scholid_definitions", package = "scholid")`](https://thomas-rauter.github.io/scholid/articles/scholid_definitions.md)).

A key is for comparing, not an identifier. Some keys don't pass
[`is_scholid()`](https://thomas-rauter.github.io/scholid/reference/is_scholid.md),
such as `NM_000546` for the RefSeq accession `NM_000546.5`, and a key
can leave out what tells two records apart, such as a version. Don't
store or show keys as identifiers; keep the normalized values for that.

## Usage

``` r
scholid_key(x, type)
```

## Arguments

- x:

  A vector of values to compare.

- type:

  A single string giving the identifier type. See
  [`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md)
  for supported values.

## Value

A character vector with the same length as `x`. Values that are `NA` or
don't normalize yield `NA_character_`.

## See also

[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md),
[`format_scholid()`](https://thomas-rauter.github.io/scholid/reference/format_scholid.md),
[`is_scholid()`](https://thomas-rauter.github.io/scholid/reference/is_scholid.md),
[`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md)

## Examples

``` r
isbns <- c("0306406152", "ISBN 978-0-306-40615-7", "0-8044-2957-X")
scholid_key(isbns, "isbn")
#> [1] "9780306406157" "9780306406157" "9780804429573"
duplicated(scholid_key(isbns, "isbn"))
#> [1] FALSE  TRUE FALSE

dois <- c("10.1000/abc", "https://doi.org/10.1000/ABC")
duplicated(scholid_key(dois, "doi"))
#> [1] FALSE  TRUE

# Versions of one preprint share a key
scholid_key(c("2101.00001v1", "arXiv:2101.00001v2"), "arxiv")
#> [1] "2101.00001" "2101.00001"
```
