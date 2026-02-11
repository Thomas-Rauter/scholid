#' Normalize scholarly identifiers
#'
#' @description
#' Vectorized normalizer that converts supported scholarly identifier values
#' to a canonical form (e.g., removing URL prefixes, labels, or separators).
#'
#' Normalization is structural: inputs that conform to the expected identifier
#' syntax are converted to a canonical representation. Inputs that do not match
#' the required structure yield `NA_character_`.
#'
#' For identifier types with checksum algorithms (e.g., ORCID, ISBN, ISSN),
#' normalization does not verify checksum correctness. It only enforces
#' structural plausibility and canonical formatting.
#'
#' Use [is_scholid()] to test whether values are fully valid identifiers,
#' including checksum verification where applicable.
#'
#' @param x A vector of values to normalize.
#' @param type A single string giving the identifier type. See
#'   [scholid_types()] for supported values.
#'
#' @return A character vector with the same length as `x`. Invalid or
#'   structurally non-matching inputs yield `NA_character_`.
#'
#' @examples
#' normalize_scholid("https://doi.org/10.1000/182", "doi")
#' normalize_scholid("https://orcid.org/0000-0002-1825-0097", "orcid")
#'
#' @seealso [is_scholid()], [scholid_types()]
#' @export
normalize_scholid <- function(
        x,
        type
        ) {
    .scholid_check_x(
        x,
        arg = "x"
        )
    type <- .scholid_match_type(type)

    fun_name <- paste0(
        "normalize_",
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

    fun(x)
}


# Level 1 function (functions called by exported functions) definitions --------
## normalize_<id>() function definitions ---------------------------------------


#' Normalize Digital Object Identifiers
#'
#' @description
#' Normalizes DOI strings by removing URL prefixes, `doi:` labels, and
#' trailing punctuation.
#'
#' @param x A vector of DOI values.
#'
#' @return A character vector of normalized DOIs.
#'
#' @noRd
normalize_doi <- function(x) {
    x <- as.character(x)
    out <- rep(NA_character_, length(x))

    ok <- !is.na(x)
    y <- trimws(x[ok])

    y <- sub("^doi:\\s*", "", y, ignore.case = TRUE)
    y <- sub("^https?://(dx\\.)?doi\\.org/", "", y, ignore.case = TRUE)
    y <- sub("[[:punct:]]+$", "", y)

    pat <- .scholid_registry()[["doi"]]$pat
    y[!grepl(pat, y, perl = TRUE)] <- NA_character_

    out[ok] <- y
    out
}


#' Normalize ORCID identifiers
#'
#' @description
#' Normalizes ORCID iDs by removing URL prefixes and enforcing canonical
#' hyphenated grouping.
#'
#' @param x A vector of ORCID values.
#'
#' @return A character vector of normalized ORCID iDs.
#'
#' @noRd
normalize_orcid <- function(x) {
    x <- as.character(x)
    out <- rep(NA_character_, length(x))

    ok <- !is.na(x)
    y <- trimws(x[ok])

    y <- sub("^https?://orcid\\.org/", "", y, ignore.case = TRUE)
    y <- gsub("[^0-9X]", "", y)

    pat <- "^\\d{15}[0-9X]$"
    y[!grepl(pat, y)] <- NA_character_

    y <- ifelse(
        is.na(y),
        NA_character_,
        paste(
            substr(y, 1, 4),
            substr(y, 5, 8),
            substr(y, 9, 12),
            substr(y, 13, 16),
            sep = "-"
        )
    )

    out[ok] <- y
    out
}


#' Normalize ISBN identifiers
#'
#' @description
#' Normalizes ISBN-10 and ISBN-13 values by removing separators and
#' validating length.
#'
#' @param x A vector of ISBN values.
#'
#' @return A character vector of normalized ISBNs.
#'
#' @noRd
normalize_isbn <- function(x) {
    x <- as.character(x)
    out <- rep(NA_character_, length(x))

    ok <- !is.na(x)
    y <- toupper(gsub("[^0-9Xx]", "", x[ok]))

    is10 <- grepl("^\\d{9}[0-9X]$", y)
    is13 <- grepl("^\\d{13}$", y)

    y[!(is10 | is13)] <- NA_character_

    out[ok] <- y
    out
}


#' Normalize ISSN identifiers
#'
#' @description
#' Normalizes ISSN values by removing prefixes and enforcing `NNNN-NNNN`
#' format.
#'
#' @param x A vector of ISSN values.
#'
#' @return A character vector of normalized ISSNs.
#'
#' @noRd
normalize_issn <- function(x) {
    x <- as.character(x)
    out <- rep(NA_character_, length(x))

    ok <- !is.na(x)
    y <- trimws(x[ok])

    y <- sub("^ISSN\\s*", "", y, ignore.case = TRUE)
    y <- toupper(gsub("[^0-9X]", "", y))

    pat <- "^\\d{7}[0-9X]$"
    y[!grepl(pat, y)] <- NA_character_

    y <- ifelse(
        is.na(y),
        NA_character_,
        paste0(substr(y, 1, 4), "-", substr(y, 5, 8))
    )

    out[ok] <- y
    out
}


#' Normalize arXiv identifiers
#'
#' @description
#' Normalizes arXiv identifiers by removing URL prefixes and `arXiv:` labels.
#'
#' @param x A vector of arXiv identifier values.
#'
#' @return A character vector of normalized arXiv identifiers.
#'
#' @noRd
normalize_arxiv <- function(x) {
    x <- as.character(x)
    out <- rep(NA_character_, length(x))

    ok <- !is.na(x)
    y <- trimws(x[ok])

    y <- sub("^arXiv:\\s*", "", y, ignore.case = TRUE)
    y <- sub("^https?://arxiv\\.org/abs/", "", y, ignore.case = TRUE)

    reg <- .scholid_registry()[["arxiv"]]
    pat1 <- reg$pat1
    pat2 <- reg$pat2

    y[!(grepl(pat1, y) | grepl(pat2, y))] <- NA_character_

    out[ok] <- y
    out
}


#' Normalize PubMed identifiers
#'
#' @description
#' Normalizes PubMed identifiers by removing labels and whitespace.
#'
#' @param x A vector of PubMed identifier values.
#'
#' @return A character vector of normalized PMIDs.
#'
#' @noRd
normalize_pmid <- function(x) {
    x <- as.character(x)
    out <- rep(NA_character_, length(x))

    ok <- !is.na(x)
    y <- trimws(x[ok])

    y <- sub("^PMID:\\s*", "", y, ignore.case = TRUE)

    pat <- .scholid_registry()[["pmid"]]$pat
    y[!grepl(pat, y, perl = TRUE)] <- NA_character_

    out[ok] <- y
    out
}


#' Normalize PubMed Central identifiers
#'
#' @description
#' Normalizes PMCID values by removing labels and enforcing `PMC` prefix.
#'
#' @param x A vector of PubMed Central identifier values.
#'
#' @return A character vector of normalized PMCIDs.
#'
#' @noRd
normalize_pmcid <- function(x) {
    x <- as.character(x)
    out <- rep(NA_character_, length(x))

    ok <- !is.na(x)
    y <- trimws(x[ok])

    y <- sub("^PMCID:\\s*", "", y, ignore.case = TRUE)
    y <- toupper(y)
    pat <- .scholid_registry()[["pmcid"]]$pat
    y[!grepl(pat, y, perl = TRUE)] <- NA_character_

    out[ok] <- y
    out
}
