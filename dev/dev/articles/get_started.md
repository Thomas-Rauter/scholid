# Getting started with scholid

`scholid` is a lightweight, dependency-free (base R only) toolkit for
working with scholarly and academic identifiers. It provides small,
well-tested helpers to detect, normalize, classify, and extract common
identifier strings.

This vignette introduces the interface and typical workflows for mixed,
messy identifier data.

## Installation

``` r
install.packages("scholid")
```

## Interface

`scholid` exposes a small set of user-facing functions that operate
consistently across identifier types:

- [`scholid_types()`](https://thomas-rauter.github.io/scholid/dev/reference/scholid_types.md)
  lists supported identifier types.
- `is_scholid(x, type)` checks whether values match the identifier type.
- `normalize_scholid(x, type)` returns canonical identifier strings.
- `extract_scholid(text, type)` extracts identifiers from free text.
- `classify_scholid(x)` guesses the identifier type per element.
- `detect_scholid_type(x)` detects identifier types from canonical or
  wrapped input values (e.g., URLs or labels).

These generic helpers dispatch internally to type-specific
implementations such as `is_doi()`, `normalize_orcid()`, and
`extract_isbn()`.

## Supported identifier types

``` r
scholid::scholid_types()
```

    ## [1] "arxiv" "doi"   "isbn"  "issn"  "orcid" "pmcid" "pmid"

## Detect: `is_scholid()`

[`is_scholid()`](https://thomas-rauter.github.io/scholid/dev/reference/is_scholid.md)
checks whether each value matches a specific identifier type. It is
vectorized and preserves missing values.

``` r
x <- c(
    "10.1000/182",
    "not a doi",
    NA
)
scholid::is_scholid(
    x    = x,
    type = "doi"
)
```

    ## [1]  TRUE FALSE    NA

## Normalize: `normalize_scholid()`

Normalization removes common wrappers and enforces a canonical
representation. This is particularly useful when identifiers are stored
as URLs or prefixed labels.

``` r
x <- c(
  "https://doi.org/10.1000/182.",
  "doi:10.1000/182",
  " 10.1000/182 "
)
scholid::normalize_scholid(
    x    = x, 
    type = "doi"
)
```

    ## [1] "10.1000/182" "10.1000/182" "10.1000/182"

For ORCID iDs, normalization removes URL prefixes and enforces
hyphenated grouping.

``` r
x <- c(
  "https://orcid.org/0000-0002-1825-0097",
  "0000000218250097"
)
scholid::normalize_scholid(
    x    = x,
    type = "orcid"
)
```

    ## [1] "0000-0002-1825-0097" "0000-0002-1825-0097"

Normalization is designed to be predictable: - `NA` input stays `NA`. -
Invalid inputs typically become `NA_character_`.

## Extract: `extract_scholid()`

Extraction is for harvesting identifiers from unstructured text. The
result is a list with one element per input element. Each element is a
character vector of matches (possibly empty).

``` r
txt <- c(
  "See https://doi.org/10.1000/182 and doi:10.5555/12345678.",
  "No identifier here.",
  NA
)
scholid::extract_scholid(
    text = txt,
    type = "doi"
)
```

    ## [[1]]
    ## [1] "10.1000/182"       "10.5555/12345678."
    ## 
    ## [[2]]
    ## character(0)
    ## 
    ## [[3]]
    ## character(0)

The list return type is intentional: a single text string can contain
multiple identifiers.

## Classify: `classify_scholid()`

[`classify_scholid()`](https://thomas-rauter.github.io/scholid/dev/reference/classify_scholid.md)
returns the best-guess identifier type per element for mixed identifier
columns. Classification is based on the set of available `is_<type>()`
checks and the precedence order defined by
[`scholid_types()`](https://thomas-rauter.github.io/scholid/dev/reference/scholid_types.md).

``` r
x <- c(
  "10.1000/182",
  "0000-0002-1825-0097",
  "PMC12345",
  "2101.00001v2",
  "not an id",
  NA
)
scholid::classify_scholid(x = x)
```

    ## [1] "doi"   "orcid" "pmcid" "arxiv" NA      NA

### Normalization + classification in messy data

Many identifiers appear wrapped (URLs, prefixes, trailing punctuation).
Classification is strict and expects canonical strings. A common pattern
is:

1.  Extract identifiers from text.
2.  Normalize extracted values.
3.  Classify and/or validate.

``` r
txt <- "Read https://doi.org/10.1000/182 (and ORCID 0000-0002-1825-0097)."
dois <- scholid::extract_scholid(txt, "doi")[[1]]
orcids <- scholid::extract_scholid(txt, "orcid")[[1]]

dois_n <- scholid::normalize_scholid(dois, "doi")
orcids_n <- scholid::normalize_scholid(orcids, "orcid")

scholid::classify_scholid(c(dois_n, orcids_n))
```

    ## [1] "doi"   "orcid"

``` r
scholid::is_scholid(dois_n, "doi")
```

    ## [1] TRUE

``` r
scholid::is_scholid(orcids_n, "orcid")
```

    ## [1] TRUE

## Detect: `detect_scholid_type()`

[`detect_scholid_type()`](https://thomas-rauter.github.io/scholid/dev/reference/detect_scholid_type.md)
performs best-effort type detection for mixed, messy identifier input.
In contrast to
[`classify_scholid()`](https://thomas-rauter.github.io/scholid/dev/reference/classify_scholid.md),
detection also recognizes common wrapped forms such as URLs and prefixed
labels (e.g., `doi:`, `https://orcid.org/`, `arXiv:`, `PMID:`).

Detection is useful when working with raw data where identifiers may not
yet be normalized.

For example, wrapped identifiers are not classified strictly:

``` r
x <- c(
  "https://doi.org/10.1000/182",
  "ORCID: 0000-0002-1825-0097",
  "arXiv:2101.00001",
  "PMID: 12345",
  "not an id"
)
scholid::classify_scholid(x)
```

    ## [1] NA NA NA NA NA

However, they can be detected directly:

``` r
scholid::detect_scholid_type(x)
```

    ## [1] "doi"   "orcid" "arxiv" "pmid"  NA

Whitespace and minor formatting irregularities are handled
conservatively:

``` r
scholid::detect_scholid_type(
  c(
    " 0000-0002-1825-0097 ",
    " 10.1000/182 ",
    "ISSN 0317-8471"
  )
)
```

    ## [1] "orcid" "doi"   "issn"

[`detect_scholid_type()`](https://thomas-rauter.github.io/scholid/dev/reference/detect_scholid_type.md)
does not modify values. Once the identifier type is known, use
[`normalize_scholid()`](https://thomas-rauter.github.io/scholid/dev/reference/normalize_scholid.md)
to convert to canonical form and
[`is_scholid()`](https://thomas-rauter.github.io/scholid/dev/reference/is_scholid.md)
for strict validation.

A typical workflow for messy data is:

1.  Detect identifier types.
2.  Normalize by detected type.
3.  Validate canonical identifiers.

This separation keeps detection permissive and normalization
predictable, while preserving strict validation where needed.

## Design notes

`scholid` is intentionally small and conservative:

- It uses base R only at runtime.
- Functions are vectorized and return stable types.
- Type-specific logic is kept in small `is_*()`, `normalize_*()`, and
  `extract_*()` helpers.
- The package is designed to be a low-level building block for other
  packages and for workflows.

## Session information

``` r
sessionInfo()
```

    ## R version 4.5.2 (2025-10-31)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.3 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    ##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    ##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    ## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] digest_0.6.39      desc_1.4.3         R6_2.6.1           fastmap_1.2.0     
    ##  [5] xfun_0.56          cachem_1.1.0       knitr_1.51         htmltools_0.5.9   
    ##  [9] rmarkdown_2.30     lifecycle_1.0.5    cli_3.6.5          scholid_0.1.0.9000
    ## [13] sass_0.4.10        pkgdown_2.2.0      textshaping_1.0.4  jquerylib_0.1.4   
    ## [17] systemfonts_1.3.1  compiler_4.5.2     tools_4.5.2        ragg_1.5.0        
    ## [21] bslib_0.10.0       evaluate_1.0.5     yaml_2.3.12        otel_0.2.0        
    ## [25] jsonlite_2.0.0     rlang_1.1.7        fs_1.6.6           htmlwidgets_1.6.4
