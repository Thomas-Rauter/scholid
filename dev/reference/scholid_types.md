# Supported scholid identifier types

Returns the set of identifier types supported by the scholid package in
classification priority order (most specific first).

## Usage

``` r
scholid_types()
```

## Value

A character vector of supported identifier type strings.

## Examples

``` r
scholid_types()
#>  [1] "doi"      "arxiv"    "bibcode"  "openalex" "swhid"    "ark"     
#>  [7] "isni"     "orcid"    "ror"      "rrid"     "uniprot"  "refseq"  
#> [13] "sra"      "isbn"     "issn"     "pmcid"    "pmid"    
"orcid" %in% scholid_types()
#> [1] TRUE
```
