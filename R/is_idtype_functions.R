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
    pat <- .scholid_registry()[["doi"]]$pat
    out[ok] <- grepl(pat, x[ok], perl = TRUE)
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
    pat <- "^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$"
    y <- x[ok]

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
    x <- toupper(gsub("[^0-9Xx]", "", as.character(x)))
    out <- rep(NA, length(x))

    ok <- !is.na(x)

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

    out[ok] <- vapply(
        x[ok],
        function(s) is10(s) || is13(s),
        logical(1)
    )

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
#' Tests whether values are valid PubMed identifiers (PMIDs).
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
    pat <- .scholid_registry()[["pmid"]]$pat
    out[ok] <- grepl(pat, x[ok], perl = TRUE)
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
