#' Normalize scholarly identifiers
#'
#' @description
#' Vectorized normalizer that converts supported scholarly identifier values
#' to a canonical form (e.g., removing URL prefixes, labels, or separators).
#'
#' Normalization requires that inputs match the expected identifier structure.
#' For identifier types with checksum algorithms, normalization also requires
#' checksum-valid values. Inputs that do not meet these requirements yield
#' `NA_character_`.
#'
#' Normalized outputs are canonical, type-specific representations of valid
#' identifiers.
#'
#' Use [is_scholid()] to test whether values are fully valid identifiers,
#' including checksum verification where applicable.
#'
#' @param x A vector of values to normalize.
#' @param type A single string giving the identifier type. See
#'   [scholid_types()] for supported values.
#'
#' @return A character vector with the same length as `x`. Invalid, checksum-
#'   failing, or structurally non-matching inputs yield `NA_character_`.
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

    keep <- vapply(y, .is_doi_strict, logical(1))
    y[!keep] <- NA_character_

    out[ok] <- y
    out
}


#' Normalize ORCID identifiers
#'
#' @description
#' Normalizes ORCID iDs from canonical hyphenated, compact, space-separated,
#' URL-prefixed, or `orcid:`-prefixed forms to canonical hyphenated form.
#'
#' Only plausible ORCID input formats are accepted. Inputs with arbitrary
#' surrounding text, malformed separators, or other unsupported wrapping
#' yield `NA_character_`.
#'
#' Normalization requires checksum-valid identifiers.
#'
#' @param x A vector of ORCID values.
#'
#' @return A character vector of normalized ORCID iDs. Invalid,
#'   unsupported, or checksum-failing inputs yield `NA_character_`.
#'
#' @noRd
normalize_orcid <- function(x) {
    x <- as.character(x)
    out <- rep(NA_character_, length(x))

    ok <- !is.na(x)
    y <- trimws(x[ok])

    y <- sub("^https?://orcid\\.org/", "", y, ignore.case = TRUE)
    y <- sub("^orcid\\s*:\\s*", "", y, ignore.case = TRUE)

    is_hyph <- grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9Xx]$", y)
    is_comp <- grepl("^\\d{15}[0-9Xx]$", y)
    is_spac <- grepl("^\\d{4} \\d{4} \\d{4} \\d{3}[0-9Xx]$", y)

    y[!(is_hyph | is_comp | is_spac)] <- NA_character_

    y <- ifelse(
        is.na(y),
        NA_character_,
        toupper(gsub("[- ]", "", y))
    )

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

    y[!is.na(y) & !is_orcid(y)] <- NA_character_

    out[ok] <- y
    out
}


#' Normalize ISBN identifiers
#'
#' @description
#' Normalizes ISBN-10 and ISBN-13 values by removing optional `ISBN`,
#' `ISBN-10`, or `ISBN-13` labels, stripping separators, enforcing compact
#' canonical form, and requiring checksum-valid identifiers.
#'
#' @param x A vector of ISBN values.
#'
#' @return A character vector of normalized ISBNs. Invalid or
#'   checksum-failing inputs yield `NA_character_`.
#'
#' @noRd
normalize_isbn <- function(x) {
    x <- as.character(x)
    out <- rep(NA_character_, length(x))

    ok <- !is.na(x)

    out[ok] <- vapply(x[ok], function(s) {
        s <- trimws(s)
        s <- sub(
            "^(?i:isbn(?:-1[03])?)\\s*:?\\s*",
            "",
            s,
            perl = TRUE
        )

        if (!.isbn_format_ok(s)) {
            return(NA_character_)
        }

        y <- toupper(gsub("[- ]", "", s))

        is10 <- grepl("^\\d{9}[0-9X]$", y)
        is13 <- grepl("^\\d{13}$", y)

        if (!(is10 || is13)) {
            return(NA_character_)
        }

        if (!is_isbn(y)) {
            return(NA_character_)
        }

        y
    }, character(1))

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

    # Remove only an optional ISSN label at the beginning
    y <- sub("^ISSN\\s*:?[[:space:]]*", "", y, ignore.case = TRUE)

    # Accept only full-string ISSN forms:
    # - hyphenated: NNNN-NNNN
    # - compact:    NNNNNNNN
    is_hyph <- grepl("^\\d{4}-\\d{3}[0-9Xx]$", y)
    is_comp <- grepl("^\\d{7}[0-9Xx]$", y)

    y[!(is_hyph | is_comp)] <- NA_character_

    # Canonicalize to compact uppercase form first
    y <- ifelse(
        is.na(y),
        NA_character_,
        toupper(gsub("-", "", y))
    )

    # Reinsert canonical hyphen
    y <- ifelse(
        is.na(y),
        NA_character_,
        paste0(substr(y, 1, 4), "-", substr(y, 5, 8))
    )

    # Keep only checksum-valid ISSNs
    y[!is.na(y) & !is_issn(y)] <- NA_character_

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

    y <- sub(
        "^PMID(?:[[:space:]]*:[[:space:]]*|[[:space:]]+)",
        "",
        y,
        ignore.case = TRUE
    )

    pat <- .scholid_registry()[["pmid"]]$pat
    y[!grepl(pat, y, perl = TRUE)] <- NA_character_

    out[ok] <- y
    out
}


#' Normalize PubMed Central identifiers
#'
#' @description
#' Normalizes PMCID values by removing optional `PMCID` labels and enforcing
#' canonical `PMC`-prefixed form.
#'
#' When a `PMCID` label is present, digit-only values are interpreted as the
#' numeric part of a PMCID and normalized by restoring the missing `PMC`
#' prefix.
#'
#' @param x A vector of PubMed Central identifier values.
#'
#' @return A character vector of normalized PMCIDs. Invalid or unsupported
#'   inputs yield `NA_character_`.
#'
#' @noRd
normalize_pmcid <- function(x) {
    x <- as.character(x)
    out <- rep(NA_character_, length(x))

    ok <- !is.na(x)
    y <- trimws(x[ok])

    had_label <- grepl(
        "^PMCID\\s*:?[[:space:]]*",
        y,
        ignore.case = TRUE
    )

    y <- sub(
        "^PMCID\\s*:?[[:space:]]*",
        "",
        y,
        ignore.case = TRUE
    )

    y <- toupper(y)

    needs_prefix <- had_label & grepl("^\\d+$", y)
    y[needs_prefix] <- paste0("PMC", y[needs_prefix])

    pat <- .scholid_registry()[["pmcid"]]$pat
    y[!grepl(pat, y, perl = TRUE)] <- NA_character_

    out[ok] <- y
    out
}
