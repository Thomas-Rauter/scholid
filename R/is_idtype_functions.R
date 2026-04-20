# Level 1 function (functions called by exported functions) definitions --------
## is_<id>() function definitions ----------------------------------------------


#' Check Digital Object Identifiers
#'
#' Tests whether values conform to the DOI syntax.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_doi <- function(x) {
    x <- as.character(x)
    out <- rep(NA, length(x))
    ok <- !is.na(x)
    out[ok] <- vapply(x[ok], .is_doi_strict, logical(1))
    out
}


#' Check ORCID identifiers
#'
#' Tests whether values are valid ORCID iDs, including checksum.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_orcid <- function(x) {
    x <- as.character(x)
    out <- rep(NA, length(x))

    ok <- !is.na(x)
    pat <- "^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9Xx]$"
    y <- toupper(x[ok])

    valid <- grepl(pat, y)
    res <- rep(FALSE, length(y))

    chk <- function(id) {
        d <- strsplit(gsub("-", "", id), "")[[1]]
        s <- 0
        for (i in seq_len(15)) {
            s <- (s + as.integer(d[i])) * 2
        }
        r <- (12 - (s %% 11)) %% 11
        cd <- if (r == 10) "X" else as.character(r)
        cd == d[16]
    }

    res[valid] <- vapply(y[valid], chk, logical(1))
    out[ok] <- res
    out
}


#' Check ISBN identifiers
#'
#' Tests whether values are valid ISBN-10 or ISBN-13 identifiers.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_isbn <- function(x) {
    raw <- as.character(x)
    out <- rep(NA, length(raw))

    ok <- !is.na(raw)

    is10 <- function(s) {
        if (!grepl("^\\d{9}[0-9X]$", s)) return(FALSE)
        d <- strsplit(s, "")[[1]]
        v <- as.integer(d[1:9])

        s9 <- sum(v * 10:2)
        cdn <- (11 - (s9 %% 11)) %% 11
        cd <- if (cdn == 10) "X" else as.character(cdn)

        cd == d[10]
    }

    is13 <- function(s) {
        if (!grepl("^\\d{13}$", s)) return(FALSE)
        d <- as.integer(strsplit(s, "")[[1]])
        r <- sum(d[1:12] * rep(c(1, 3), 6))
        cd <- (10 - (r %% 10)) %% 10
        cd == d[13]
    }

    out[ok] <- vapply(raw[ok], function(s) {
        if (!.isbn_format_ok(s)) {
            return(FALSE)
        }

        compact <- toupper(gsub("[- ]", "", s))
        is10(compact) || is13(compact)
    }, logical(1))

    out
}


#' Check ISSN identifiers
#'
#' Tests whether values are valid ISSNs, including checksum.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_issn <- function(x) {
    x <- as.character(x)
    out <- rep(NA, length(x))

    ok <- !is.na(x)
    pat <- "^\\d{4}-\\d{3}[0-9X]$"
    y <- x[ok]

    chk <- function(s) {
        d <- strsplit(gsub("-", "", s), "")[[1]]
        v <- as.integer(d[1:7])
        r <- sum(v * 8:2) %% 11
        cd <- if (r == 0) "0" else if (r == 1) "X" else as.character(11 - r)
        cd == d[8]
    }

    res <- grepl(pat, y)
    res[res] <- vapply(y[res], chk, logical(1))
    out[ok] <- res
    out
}


#' Check arXiv identifiers
#'
#' Tests whether values match valid arXiv identifier formats.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_arxiv <- function(x) {
    x <- as.character(x)
    out <- rep(NA, length(x))
    ok <- !is.na(x)
    reg <- .scholid_registry()[["arxiv"]]
    pat1 <- reg$pat1
    pat2 <- reg$pat2
    out[ok] <- grepl(pat1, x[ok], perl = TRUE) | grepl(pat2, x[ok], perl = TRUE)
    out
}


#' Check PubMed identifiers
#'
#' Tests whether values are structurally plausible PubMed identifiers
#' (PMIDs). PMID checks are based on digit-only syntax, with exclusion of
#' values that are valid ISBNs to reduce cross-type false positives.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_pmid <- function(x) {
    x <- as.character(x)
    out <- rep(NA, length(x))

    ok <- !is.na(x)
    y <- x[ok]

    pat <- .scholid_registry()[["pmid"]]$pat
    res <- grepl(pat, y, perl = TRUE)

    res[res] <- !is_isbn(y[res])

    out[ok] <- res
    out
}


#' Check PubMed Central identifiers
#'
#' Tests whether values are valid PMCID identifiers.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_pmcid <- function(x) {
    x <- as.character(x)
    out <- rep(NA, length(x))
    ok <- !is.na(x)
    pat <- .scholid_registry()[["pmcid"]]$pat
    out[ok] <- grepl(pat, x[ok], perl = TRUE)
    out
}


# Level 2 functions (functions called by level 1 functions) definitions --------


#' Strict DOI validator
#'
#' @param x A single character string.
#'
#' @return A single logical value.
#'
#' @noRd
.is_doi_strict <- function(x) {
    if (!nzchar(x)) {
        return(FALSE)
    }

    # Broad DOI structure
    pat <- .scholid_registry()[["doi"]]$pat
    if (!grepl(pat, x, perl = TRUE)) {
        return(FALSE)
    }

    # Reject obvious markup contamination
    if (grepl("[\"']", x, perl = TRUE)) {
        return(FALSE)
    }
    if (grepl("</", x, perl = TRUE)) {
        return(FALSE)
    }
    if (grepl(">[^[:space:]]*<", x, perl = TRUE)) {
        return(FALSE)
    }

    # Reject obvious trailing wrapper characters
    if (grepl("[<>()\\[\\]{}]$", x, perl = TRUE)) {
        return(FALSE)
    }

    # Reject a DOI immediately followed by letters after an unmatched closer,
    # e.g. 10.1000/182)yy
    if (grepl("[)\\]}>][[:alpha:]]+$", x, perl = TRUE)) {
        return(FALSE)
    }

    TRUE
}


#' Check whether an ISBN string has an acceptable input format
#'
#' @description
#' Returns `TRUE` for compact ISBN-10 and ISBN-13 strings, and for grouped
#' forms that use single spaces or hyphens in acceptable positions.
#'
#' This check validates input formatting only. It does not verify the ISBN
#' checksum.
#'
#' @param x A single candidate ISBN string.
#'
#' @return A single logical value.
#'
#' @noRd
.isbn_format_ok <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return(FALSE)
    }

    # compact forms
    if (grepl("^\\d{9}[0-9Xx]$", x) || grepl("^\\d{13}$", x)) {
        return(TRUE)
    }

    # formatted forms: digits/X separated by single spaces or hyphens,
    # with 10 or 13 ISBN characters total after stripping separators
    if (!grepl("^[0-9Xx -]+$", x)) {
        return(FALSE)
    }
    if (grepl("(^[- ]|[- ]$|[- ]{2,}|[- ]{2,})", x)) {
        return(FALSE)
    }

    compact <- gsub("[- ]", "", x)
    n <- nchar(compact)

    if (n == 10) {
        # ISBN-10: must consist of 4 groups if separators are present
        return(grepl("^[0-9]+([ -][0-9]+){2}[ -][0-9Xx]$", x))
    }

    if (n == 13) {
        # ISBN-13: grouped form must start with 978 or 979
        return(grepl("^97[89]([ -][0-9]+){4}$", x))
    }

    FALSE
}
