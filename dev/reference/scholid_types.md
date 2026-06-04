# Supported scholid identifier types

Returns the set of identifier types supported by the scholid package in
classification priority order (most specific first). The package
currently supports twenty types (from DOI and ORCID through life-science
and archive identifiers). For per-type formats, validation rules, and
classification precedence, see the *How Scholarly Identifiers Are
Defined* vignette
([`vignette("scholid_definitions", package = "scholid")`](https://thomas-rauter.github.io/scholid/articles/scholid_definitions.md)).

## Usage

``` r
scholid_types()
```

## Value

A character vector of supported identifier type strings.

## Examples

``` r
scholid_types()
#>  [1] "doi"        "arxiv"      "bibcode"    "openalex"   "swhid"     
#>  [6] "ark"        "isni"       "orcid"      "ror"        "rrid"      
#> [11] "uniprot"    "refseq"     "sra"        "geo"        "bioproject"
#> [16] "assembly"   "isbn"       "issn"       "pmcid"      "pmid"      
"orcid" %in% scholid_types()
#> [1] TRUE
```
