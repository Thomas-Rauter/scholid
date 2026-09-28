# Locate scholarly identifiers in text

Find identifiers of several types in free text at once, and report where
each one is. For each type, the identifiers found are those that
[`extract_scholid()`](https://thomas-rauter.github.io/scholid/reference/extract_scholid.md)
returns.

The result is a data frame with one row per identifier found and these
columns:

- `element`: the index of the element of `text` it was found in.

- `type`: its identifier type.

- `id`: the token
  [`extract_scholid()`](https://thomas-rauter.github.io/scholid/reference/extract_scholid.md)
  returns for it.

- `match`: the identifier as written in the text. It leaves out a URL or
  label in front and the punctuation after it, but keeps a prefix of the
  canonical form, such as `RRID:` or `ark:`. It can differ from `id` in
  case, in spaces between digit groups, and in look-alike and invisible
  characters.

- `start`, `end`: the positions of `match` in `text[element]`, counted
  as [`substr()`](https://rdrr.io/r/base/substr.html) counts them, so
  that `substr(text[element], start, end) == match`. They include the
  invisible characters that matching ignores.

Rows are sorted by `element`, then `start`. `NA` elements give no rows.
When nothing is found, the result has zero rows and the same columns.

The same stretch of text can be found by several types, such as an ISBN
and the PMID-like digits inside it. Within one element, when the spans
of two hits overlap, only one is kept: the longer span wins, and for
equal lengths the type that comes first in
[`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md),
the order
[`classify_scholid()`](https://thomas-rauter.github.io/scholid/reference/classify_scholid.md)
uses. Hits are taken in that order, and each is kept unless it overlaps
one already kept. Only the types in `types` take part, so a type left
out can't hide another.

## Usage

``` r
locate_scholid(text, types = scholid_types())
```

## Arguments

- text:

  A character vector of text.

- types:

  A character vector of identifier types to look for. See
  [`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md)
  for supported values. Duplicates and order don't matter.

## Value

A data frame with one row per identifier found and the columns `element`
(integer), `type`, `id`, `match` (character), `start`, and `end`
(integer).

## See also

[`extract_scholid()`](https://thomas-rauter.github.io/scholid/reference/extract_scholid.md)
to extract identifiers of one type,
[`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md)

## Examples

``` r
locate_scholid(c(
  "See doi:10.1000/182 and ORCID 0000-0002-1825-0097.",
  NA,
  "PMID: 12345678"
))
#>   element  type                  id               match start end
#> 1       1   doi         10.1000/182         10.1000/182     9  19
#> 2       1 orcid 0000-0002-1825-0097 0000-0002-1825-0097    31  49
#> 3       3  pmid            12345678            12345678     7  14

# The ISBN hides the PMID-like digits inside it ...
locate_scholid("ISBN 978 0 306 40615 7")
#>   element type                id             match start end
#> 1       1 isbn 978 0 306 40615 7 978 0 306 40615 7     6  22
# ... unless ISBN is not looked for.
locate_scholid("ISBN 978 0 306 40615 7", types = "pmid")
#>   element type    id match start end
#> 1       1 pmid 40615 40615    16  20
```
