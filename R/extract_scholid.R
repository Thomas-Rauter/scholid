#' Extract scholarly identifiers from text
#'
#' @description
#' Extract identifiers of a single supported type from free text.
#'
#' The result is a list with one element per input element. Each element is a
#' character vector of matches (possibly length 0). `NA` inputs yield an empty
#' character vector.
#'
#' Matches are returned as found in the text; use `normalize_scholid()` to
#' convert identifiers to canonical form.
#'
#' @param text A character vector of text.
#' @param type A single string giving the identifier type. See
#'   `scholid_types()` for supported values.
#'
#' @return A list of character vectors of extracted identifiers.
#'
#' @examples
#' extract_scholid("See https://doi.org/10.1000/182.", "doi")
#' extract_scholid("ORCID 0000-0002-1825-0097", "orcid")
#'
#' @export
extract_scholid <- function(
        text,
        type
) {
    .scholid_check_x(
        text,
        arg = "text"
        )
    type <- .scholid_match_type(type)

    fun_name <- paste0(
        "extract_",
        type
        )
    fun <- get0(
        fun_name,
        mode = "function",
        inherits = TRUE
        )

    # nocov start
    if (is.null(fun)) {
        stop("Missing implementation: ", fun_name, "().", call. = FALSE)
    }
    # nocov end

    fun(text)
}


# Level 1 functions (functions called by exported functions) definitions -------
## extract_<id>() function definitions -----------------------------------------


#' Extract DOI identifiers from text
#'
#' @description
#' Extracts Digital Object Identifiers (DOIs) from free text or URLs.
#'
#' @param text A character vector of text.
#'
#' @return A list of character vectors of extracted DOIs.
#'
#' @noRd
extract_doi <- function(text) {
    pat <- "(10\\.[0-9]{4,9}/[^\\s\"<>]+)"
    .extract_with_pattern(
        text = text,
        pat  = pat
    )
}


#' Extract ORCID identifiers from text
#'
#' @description
#' Extracts ORCID iDs from free text or URLs.
#'
#' @param text A character vector of text.
#'
#' @return A list of character vectors of extracted ORCID iDs.
#'
#' @noRd
extract_orcid <- function(text) {
    pat <- "(\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X])"
    .extract_with_pattern(
        text = text,
        pat  = pat
    )
}


#' Extract ISBN identifiers from text
#'
#' @description
#' Extracts ISBN-10 and ISBN-13 identifiers from free text.
#'
#' @param text A character vector of text.
#'
#' @return A list of character vectors of extracted ISBNs.
#'
#' @noRd
extract_isbn <- function(text) {
    pat <- "([0-9Xx][0-9Xx\\- ]{8,16}[0-9Xx])"
    .extract_with_pattern(
        text = text,
        pat  = pat
    )
}


#' Extract ISSN identifiers from text
#'
#' @description
#' Extracts ISSN identifiers from free text.
#'
#' @param text A character vector of text.
#'
#' @return A list of character vectors of extracted ISSNs.
#'
#' @noRd
extract_issn <- function(text) {
    pat <- "(\\d{4}-\\d{3}[0-9X])"
    .extract_with_pattern(
        text = text,
        pat  = pat
    )
}


#' Extract arXiv identifiers from text
#'
#' @description
#' Extracts arXiv identifiers in both modern and legacy formats.
#'
#' @param text A character vector of text.
#'
#' @return A list of character vectors of extracted arXiv identifiers.
#'
#' @noRd
extract_arxiv <- function(text) {
    pat <- "(\\d{4}\\.\\d{4,5}(v\\d+)?|[a-z\\-]+/\\d{7}(v\\d+)?)"
    .extract_with_pattern(
        text = text,
        pat  = pat
    )
}


#' Extract PubMed identifiers from text
#'
#' @description
#' Extracts PubMed identifiers (PMIDs) from free text.
#'
#' @param text A character vector of text.
#'
#' @return A list of character vectors of extracted PMIDs.
#'
#' @noRd
extract_pmid <- function(text) {
    pat <- "(?<!PMC)\\b\\d{4,9}\\b"
    .extract_with_pattern(
        text = text,
        pat  = pat
    )
}


#' Extract PubMed Central identifiers from text
#'
#' @description
#' Extracts PubMed Central identifiers (PMCIDs) from free text.
#'
#' @param text A character vector of text.
#'
#' @return A list of character vectors of extracted PMCIDs.
#'
#' @noRd
extract_pmcid <- function(text) {
    pat <- "(PMC\\d+)"
    .extract_with_pattern(
        text = text,
        pat  = pat
        )
}


# Level 2 functions (functions called by level 1 functions) definitions --------


#' Extract matches from text using a regular expression
#'
#' @description
#' Internal helper that applies a single regular expression pattern to each
#' element of a character vector and returns all matches.
#'
#' The result is a list with one element per input element. Each element is a
#' character vector of matches (possibly length 0). `NA` inputs yield an empty
#' character vector. Matching is performed using `gregexpr()` with
#' `perl = TRUE`.
#'
#' @param text A character vector of text.
#' @param pat A single regular expression pattern.
#'
#' @return A list of character vectors of extracted matches.
#'
#' @noRd
.extract_with_pattern <- function(
        text,
        pat
) {
    text <- as.character(text)
    out <- vector("list", length(text))

    for (i in seq_along(text)) {
        if (is.na(text[i])) {
            out[[i]] <- character(0)
            next
        }
        m <- gregexpr(pat, text[i], perl = TRUE)
        hits <- regmatches(text[i], m)[[1]]
        out[[i]] <- if (length(hits)) hits else character(0)
    }

    out
}
