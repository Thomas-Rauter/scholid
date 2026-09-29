# Normalize scholarly identifiers

Vectorized normalizer that converts supported scholarly identifier
values to a canonical form (e.g., removing URL prefixes, labels, or
separators).

Normalization requires that inputs match the expected identifier
structure. For identifier types that define a checksum, normalization
also requires checksum-valid values. Inputs that do not meet these
requirements yield `NA_character_`.

Normalized outputs are canonical, type-specific representations of valid
identifiers.

Use
[`is_scholid()`](https://thomas-rauter.github.io/scholid/reference/is_scholid.md)
to test whether already-canonical values are valid identifiers of a
given type. Both functions apply checksum verification where applicable;
normalization additionally accepts wrapped input forms and returns
canonical strings.

Before normalizing, invisible characters are removed and Unicode spaces
count as whitespace. For some types, Unicode dashes and full-width
digits are read as their ASCII forms. See "Input characters" in the *How
Scholarly Identifiers Are Defined* vignette
([`vignette("scholid_definitions", package = "scholid")`](https://thomas-rauter.github.io/scholid/articles/scholid_definitions.md)).
Its DOI section covers DOI case and the percent-decoding of `doi.org`
URLs.

Wrapped forms include each type's resolver URL and CURIE, and arXiv DOIs
for arXiv identifiers. "Resolver URLs and CURIEs" in the same vignette
says how they are read, and each type's section names its forms.
[`format_scholid()`](https://thomas-rauter.github.io/scholid/reference/format_scholid.md)
writes these forms.

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

A character vector with the same length as `x`. Values that are `NA`,
invalid, checksum-failing, or structurally non-matching yield
`NA_character_`.

## See also

[`is_scholid()`](https://thomas-rauter.github.io/scholid/reference/is_scholid.md),
[`format_scholid()`](https://thomas-rauter.github.io/scholid/reference/format_scholid.md),
[`scholid_key()`](https://thomas-rauter.github.io/scholid/reference/scholid_key.md),
[`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md)

## Examples

``` r
normalize_scholid("https://doi.org/10.1000/182", "doi")
#> [1] "10.1000/182"
normalize_scholid("https://orcid.org/0000-0002-1825-0097", "orcid")
#> [1] "0000-0002-1825-0097"
normalize_scholid("pubmed:12345678", "pmid")
#> [1] "12345678"
normalize_scholid("https://doi.org/10.48550/arXiv.2101.00001", "arxiv")
#> [1] "2101.00001"
```
