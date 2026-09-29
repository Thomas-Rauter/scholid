# Write scholarly identifiers as resolver URLs, CURIEs, or DOIs

Vectorized writer that gives identifiers in a form for links, reports,
and linked data:

- `"url"`: the resolver URL, such as `https://doi.org/10.1000/182`.

- `"curie"`: the CURIE, the type's Bioregistry prefix, a colon, and the
  identifier, such as `pubmed:12345678`. For types whose canonical form
  already is a CURIE, it is that form.

- `"doi"`: for arXiv identifiers, the DOI that arXiv registers, such as
  `10.48550/arXiv.2101.00001`. The DOI names the preprint, not a
  version, so the version is left out, and so is the subject class of
  old-style identifiers.

`x` is first normalized with the rules of
[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md),
so wrapped input such as URLs and labels works. The output is plain
text: identifiers are written into URLs as they are, except for
characters that the resolver requires to be percent-encoded, such as `#`
in a DOI.

Which types have which forms, and the sources of the forms, are in
"Resolver URLs and CURIEs" and each type's section of the *How Scholarly
Identifiers Are Defined* vignette
([`vignette("scholid_definitions", package = "scholid")`](https://thomas-rauter.github.io/scholid/articles/scholid_definitions.md)).

What `format_scholid()` writes,
[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md)
reads back: `normalize_scholid(format_scholid(x, type, as), type)`
equals `normalize_scholid(x, type)` for `"url"` and `"curie"`. For
`"doi"`, `normalize_scholid(format_scholid(x, "arxiv", "doi"), "arxiv")`
is the arXiv identifier without its version.

## Usage

``` r
format_scholid(x, type, as = c("url", "curie", "doi"))
```

## Arguments

- x:

  A vector of values to write.

- type:

  A single string giving the identifier type. See
  [`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md)
  for supported values.

- as:

  A single string giving the form: `"url"` (the default), `"curie"`, or
  `"doi"`. Matched with
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html), so it can be
  abbreviated.

## Value

A character vector with the same length as `x`. Values that are `NA` or
don't normalize yield `NA_character_`. If `type` has no form `as`, such
as a URL for an ISBN, `format_scholid()` stops with an error that names
the forms `type` has.

## See also

[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/reference/normalize_scholid.md),
[`scholid_key()`](https://thomas-rauter.github.io/scholid/reference/scholid_key.md),
[`scholid_types()`](https://thomas-rauter.github.io/scholid/reference/scholid_types.md)

## Examples

``` r
format_scholid("https://doi.org/10.1000/182", "doi")
#> [1] "https://doi.org/10.1000/182"
format_scholid(c("PMID: 12345678", "not a PMID", NA), "pmid")
#> [1] "https://pubmed.ncbi.nlm.nih.gov/12345678/"
#> [2] NA                                         
#> [3] NA                                         
format_scholid("0000-0002-1825-0097", "orcid", as = "curie")
#> [1] "orcid:0000-0002-1825-0097"
format_scholid("arXiv:2101.00001v1", "arxiv", as = "doi")
#> [1] "10.48550/arXiv.2101.00001"

# normalize_scholid() reads the output back
url <- format_scholid("P12345", "uniprot")
normalize_scholid(url, "uniprot")
#> [1] "P12345"
```
