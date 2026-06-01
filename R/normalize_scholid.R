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
#' Use [is_scholid()] to test whether already-canonical values are valid
#' identifiers of a given type. Both functions apply checksum verification
#' where applicable; normalization additionally accepts wrapped input forms
#' and returns canonical strings.
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

    .scholid_dispatch(
        type   = type,
        prefix = "normalize_",
        x      = x
    )
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
    init <- .scholid_init_na_character(x)
    y <- trimws(init$x[init$ok])

    y <- sub("^doi:\\s*", "", y, ignore.case = TRUE)
    y <- sub("^https?://(dx\\.)?doi\\.org/", "", y, ignore.case = TRUE)
    y <- sub("[[:punct:]]+$", "", y)

    keep <- vapply(y, .is_doi_strict, logical(1))
    y[!keep] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize SWHID identifiers
#'
#' @description
#' Normalizes Software Heritage identifiers from resolver URLs or labeled forms
#' to canonical compact `swh:` form. Inputs must include an explicit `swh:`
#' prefix or a supported resolver URL; bare 40-character hex strings are
#' rejected.
#'
#' Normalization requires structurally valid identifiers. Content-hash
#' correctness is not checked.
#'
#' @param x A vector of SWHID values.
#'
#' @return A character vector of normalized SWHIDs. Invalid or unsupported
#'   inputs yield `NA_character_`.
#'
#' @noRd
normalize_swhid <- function(x) {
    init <- .scholid_init_na_character(x)
    y <- trimws(init$x[init$ok])

    has_marker <- grepl("swh\\s*:", y, ignore.case = TRUE) |
        grepl("archive\\.softwareheritage\\.org/", y, ignore.case = TRUE) |
        grepl("browse\\.softwareheritage\\.org/", y, ignore.case = TRUE) |
        grepl("identifiers\\.org/swh/", y, ignore.case = TRUE)

    y[!has_marker] <- NA_character_

    y <- sub(
        "^https?://archive\\.softwareheritage\\.org/",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub(
        "^https?://browse\\.softwareheritage\\.org/",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub(
        "^https?://identifiers\\.org/swh/",
        "",
        y,
        ignore.case = TRUE
    )
    y <- gsub("[[:space:]]+", "", y)
    y <- sub("[.,;:!?]+$", "", y)

    y <- vapply(y, function(val) {
        if (is.na(val) || !nzchar(val)) {
            return(NA_character_)
        }

        val <- .canonicalize_swhid(val)
        if (is_swhid(val)) {
            val
        } else {
            NA_character_
        }
    }, character(1))

    init$out[init$ok] <- y
    init$out
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
    init <- .scholid_init_na_character(x)
    y <- trimws(init$x[init$ok])

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

    init$out[init$ok] <- y
    init$out
}


#' Normalize ROR identifiers
#'
#' @description
#' Normalizes ROR iDs from URL-prefixed, label-prefixed, or compact forms to
#' canonical lowercase compact form.
#'
#' Normalization requires checksum-valid identifiers.
#'
#' @param x A vector of ROR values.
#'
#' @return A character vector of normalized ROR iDs. Invalid or
#'   checksum-failing inputs yield `NA_character_`.
#'
#' @noRd
normalize_ror <- function(x) {
    init <- .scholid_init_na_character(x)
    y <- trimws(init$x[init$ok])

    y <- sub("^https?://ror\\.org/", "", y, ignore.case = TRUE)
    y <- sub("^ror\\.org/", "", y, ignore.case = TRUE)
    y <- sub("^ror\\s*:?\\s*", "", y, ignore.case = TRUE)
    y <- sub("/+$", "", y)
    y <- tolower(y)

    y[!is.na(y) & !is_ror(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize RRID identifiers
#'
#' @description
#' Normalizes Research Resource Identifiers from resolver URLs or labeled forms
#' to canonical `RRID:` form. Inputs must include an explicit `RRID:` label
#' or a supported resolver URL; bare local IDs are rejected.
#'
#' Normalization requires structurally valid identifiers for known RRID
#' authorities.
#'
#' @param x A vector of RRID values.
#'
#' @return A character vector of normalized RRIDs. Invalid or unsupported
#'   inputs yield `NA_character_`.
#'
#' @noRd
normalize_rrid <- function(x) {
    init <- .scholid_init_na_character(x)
    y <- trimws(init$x[init$ok])

    has_marker <- grepl("RRID\\s*:", y, ignore.case = TRUE) |
        grepl("scicrunch\\.org/resolver/", y, ignore.case = TRUE) |
        grepl("identifiers\\.org/", y, ignore.case = TRUE) |
        grepl("n2t\\.net/", y, ignore.case = TRUE) |
        grepl("bioregistry\\.io/rrid:", y, ignore.case = TRUE)

    y[!has_marker] <- NA_character_

    y <- sub(
        "^https?://scicrunch\\.org/resolver/",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub(
        "^https?://identifiers\\.org/",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub(
        "^https?://n2t\\.net/rrid:",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub(
        "^https?://bioregistry\\.io/rrid:",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub("^RRID[[:space:]]*:[[:space:]]*", "RRID:", y, ignore.case = TRUE)
    y <- sub("[[:punct:]]+$", "", y)

    y[!is.na(y) & !is_rrid(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
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
    init <- .scholid_init_na_character(x)

    init$out[init$ok] <- vapply(init$x[init$ok], function(s) {
        s <- trimws(s)
        s <- .strip_isbn_label(s)

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

    init$out
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
    init <- .scholid_init_na_character(x)
    y <- trimws(init$x[init$ok])

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

    init$out[init$ok] <- y
    init$out
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
    init <- .scholid_init_na_character(x)
    y <- trimws(init$x[init$ok])

    y <- sub("^arXiv:\\s*", "", y, ignore.case = TRUE)
    y <- sub("^https?://arxiv\\.org/abs/", "", y, ignore.case = TRUE)

    y[!is.na(y) & !is_arxiv(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize ADS bibcodes
#'
#' @description
#' Normalizes SAO/NASA ADS bibliographic codes from ADS URLs, `bibcode:`
#' labels, or bare 19-character strings to canonical bibcode form. Case is
#' preserved.
#'
#' Normalization requires structurally valid bibcodes. ADS existence is not
#' checked.
#'
#' @param x A vector of bibcode values.
#'
#' @return A character vector of normalized bibcodes. Invalid or unsupported
#'   inputs yield `NA_character_`.
#'
#' @noRd
normalize_bibcode <- function(x) {
    init <- .scholid_init_na_character(x)
    y <- trimws(init$x[init$ok])
    y <- sub("[.,;:!?]+$", "", y)

    bare_pat <- .bibcode_pat()
    has_marker <- grepl("adsabs\\.harvard\\.edu", y, ignore.case = TRUE) |
        grepl("(?i)^bibcode\\s*:", y, perl = TRUE) |
        grepl(bare_pat, y, perl = TRUE)

    y[!has_marker] <- NA_character_

    y <- sub(
        "^https?://(?:ui\\.)?adsabs\\.harvard\\.edu/abs/",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub("(?i)^bibcode\\s*:?\\s*", "", y, perl = TRUE)

    y[!is.na(y) & !is_bibcode(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize OpenAlex identifiers
#'
#' @description
#' Normalizes OpenAlex IDs from `openalex.org` or `api.openalex.org` URLs to
#' canonical uppercase key form. Inputs must include an explicit OpenAlex URL
#' or a bare key matching the structural pattern; other strings are rejected.
#'
#' Normalization requires structurally valid identifiers. Registry existence
#' is not checked.
#'
#' @param x A vector of OpenAlex values.
#'
#' @return A character vector of normalized OpenAlex IDs. Invalid or
#'   unsupported inputs yield `NA_character_`.
#'
#' @noRd
normalize_openalex <- function(x) {
    init <- .scholid_init_na_character(x)
    y <- trimws(init$x[init$ok])

    bare_pat <- .openalex_key_pat()
    has_marker <- grepl("openalex\\.org/", y, ignore.case = TRUE) |
        grepl(paste0("(?i)", bare_pat), y, perl = TRUE)

    y[!has_marker] <- NA_character_

    y <- sub("^https?://openalex\\.org/", "", y, ignore.case = TRUE)
    y <- sub(
        paste0(
            "^https?://api\\.openalex\\.org/",
            "(?:works|authors|sources|institutions|topics|keywords|",
            "publishers|funders|grants|concepts)/"
        ),
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub("[[:punct:]]+$", "", y)
    y <- toupper(y)

    y[!is.na(y) & !is_openalex(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
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
    init <- .scholid_init_na_character(x)
    y <- trimws(init$x[init$ok])

    y <- sub(
        "^PMID(?:[[:space:]]*:[[:space:]]*|[[:space:]]+)",
        "",
        y,
        ignore.case = TRUE
    )

    y[!is.na(y) & !is_pmid(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
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
    init <- .scholid_init_na_character(x)
    y <- trimws(init$x[init$ok])

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

    y[!is.na(y) & !is_pmcid(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}
