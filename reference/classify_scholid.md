# Classify scholarly identifiers

Performs best-guess classification of scholarly identifier strings. For
each element of the input, the function returns the first matching
identifier type, or `NA_character_` if no supported type matches.

## Usage

``` r
classify_scholid(x)
```

## Arguments

- x:

  A vector of candidate identifier values.

## Value

A character vector of the same length as `x`, giving the detected
identifier type for each element, or `NA_character_` if no match is
found.
