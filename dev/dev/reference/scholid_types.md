# Supported scholid identifier types

Returns the set of identifier types supported by the scholid package.

## Usage

``` r
scholid_types()
```

## Value

A character vector of supported identifier type strings.

## Examples

``` r
scholid_types()
#> [1] "arxiv" "doi"   "isbn"  "issn"  "orcid" "pmcid" "pmid" 
"orcid" %in% scholid_types()
#> [1] TRUE
```
