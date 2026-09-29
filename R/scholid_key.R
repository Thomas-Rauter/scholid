#' Comparison keys for scholarly identifiers
#'
#' @description
#' Vectorized helper that gives each value a key for comparing identifiers.
#' Values that name the same thing get the same key, even when they are
#' written differently: an ISBN-10 and its ISBN-13, versions of one arXiv
#' preprint or RefSeq sequence, or DOIs that differ only in the case of ASCII
#' letters. Use keys with `duplicated()`, `unique()`, `match()`, and
#' `merge()`.
#'
#' `x` is first normalized with the rules of [normalize_scholid()], so
#' wrapped input such as URLs and labels works. Then the type's key rule is
#' applied. For a type without a key rule, the key is the normalized value.
#' The rules are in each type's "Validation in scholid" section of the *How
#' Scholarly Identifiers Are Defined* vignette
#' (`vignette("scholid_definitions", package = "scholid")`).
#'
#' A key is for comparing, not an identifier. Some keys don't pass
#' [is_scholid()], such as `NM_000546` for the RefSeq accession
#' `NM_000546.5`, and a key can leave out what tells two records apart,
#' such as a version. Don't store or show keys as identifiers; keep the
#' normalized values for that.
#'
#' @param x A vector of values to compare.
#' @param type A single string giving the identifier type. See
#'   [scholid_types()] for supported values.
#'
#' @return A character vector with the same length as `x`. Values that are
#'   `NA` or don't normalize yield `NA_character_`.
#'
#' @examples
#' isbns <- c("0306406152", "ISBN 978-0-306-40615-7", "0-8044-2957-X")
#' scholid_key(isbns, "isbn")
#' duplicated(scholid_key(isbns, "isbn"))
#'
#' dois <- c("10.1000/abc", "https://doi.org/10.1000/ABC")
#' duplicated(scholid_key(dois, "doi"))
#'
#' # Versions of one preprint share a key
#' scholid_key(c("2101.00001v1", "arXiv:2101.00001v2"), "arxiv")
#'
#' @seealso [normalize_scholid()], [format_scholid()], [is_scholid()],
#'   [scholid_types()]
#' @export
scholid_key <- function(
        x,
        type
) {
    .scholid_check_x(
        x,
        arg = "x"
    )
    type <- .scholid_match_type(type)

    normalized <- .scholid_dispatch(
        type   = type,
        prefix = "normalize_",
        x      = x
    )
    .scholid_dispatch(
        type   = type,
        prefix = "key_",
        x      = normalized
    )
}


# Level 1 function (functions called by exported functions) definitions --------
## key_<id>() function definitions: types with a key rule ----------------------


#' Comparison keys for Digital Object Identifiers
#'
#' @description
#' Uppercases ASCII letters. DOI names are case-insensitive for ASCII
#' letters only, so other characters are kept as they are.
#'
#' @param x A character vector of normalized DOIs.
#'
#' @return A character vector of keys.
#'
#' @noRd
key_doi <- function(x) {
    .scholid_ascii_upper(x)
}


#' Comparison keys for arXiv identifiers
#'
#' @description
#' Drops the version suffix, so all versions of a preprint share a key.
#'
#' @param x A character vector of normalized arXiv identifiers.
#'
#' @return A character vector of keys.
#'
#' @noRd
key_arxiv <- function(x) {
    .scholid_drop_version(
        x,
        type = "arxiv"
    )
}


#' Comparison keys for SWHID identifiers
#'
#' @description
#' Drops the qualifiers and keeps the core `swh:1:<type>:<hash>`.
#' Qualifiers describe the context of an object, not the object.
#'
#' @param x A character vector of normalized SWHIDs.
#'
#' @return A character vector of keys.
#'
#' @noRd
key_swhid <- function(x) {
    .swhid_split(x)$core
}


#' Comparison keys for RefSeq accession numbers
#'
#' @description
#' Drops the version suffix, so all versions of a sequence share a key.
#'
#' @param x A character vector of normalized RefSeq accessions.
#'
#' @return A character vector of keys.
#'
#' @noRd
key_refseq <- function(x) {
    .scholid_drop_version(
        x,
        type = "refseq"
    )
}


#' Comparison keys for genome assembly accession numbers
#'
#' @description
#' Drops the version suffix. `GCA_` and `GCF_` accessions keep different
#' keys, because they are different records.
#'
#' @param x A character vector of normalized assembly accessions.
#'
#' @return A character vector of keys.
#'
#' @noRd
key_assembly <- function(x) {
    .scholid_drop_version(
        x,
        type = "assembly"
    )
}


#' Comparison keys for ISBN identifiers
#'
#' @description
#' Converts an ISBN-10 to its ISBN-13: `978`, the first nine digits, and a
#' recomputed check digit. An ISBN-13 is kept.
#'
#' @param x A character vector of normalized ISBNs.
#'
#' @return A character vector of keys.
#'
#' @noRd
key_isbn <- function(x) {
    isbn10 <- !is.na(x) & nchar(x) == 10L
    if (any(isbn10)) {
        body <- paste0("978", substr(x[isbn10], 1L, 9L))
        x[isbn10] <- paste0(body, .isbn13_check_digit(body))
    }
    x
}


## key_<id>() function definitions: key is the normalized value ---------------


#' Comparison keys for ADS bibcodes
#'
#' @param x A character vector of normalized bibcodes.
#'
#' @return `x`, the normalized bibcodes.
#'
#' @noRd
key_bibcode <- function(x) {
    x
}


#' Comparison keys for OpenAlex identifiers
#'
#' @param x A character vector of normalized OpenAlex IDs.
#'
#' @return `x`, the normalized OpenAlex IDs.
#'
#' @noRd
key_openalex <- function(x) {
    x
}


#' Comparison keys for ARK identifiers
#'
#' @param x A character vector of normalized ARKs.
#'
#' @return `x`, the normalized ARKs.
#'
#' @noRd
key_ark <- function(x) {
    x
}


#' Comparison keys for ISNI identifiers
#'
#' @param x A character vector of normalized ISNIs.
#'
#' @return `x`, the normalized ISNIs.
#'
#' @noRd
key_isni <- function(x) {
    x
}


#' Comparison keys for ORCID identifiers
#'
#' @param x A character vector of normalized ORCID iDs.
#'
#' @return `x`, the normalized ORCID iDs.
#'
#' @noRd
key_orcid <- function(x) {
    x
}


#' Comparison keys for ROR identifiers
#'
#' @param x A character vector of normalized ROR iDs.
#'
#' @return `x`, the normalized ROR iDs.
#'
#' @noRd
key_ror <- function(x) {
    x
}


#' Comparison keys for RRID identifiers
#'
#' @param x A character vector of normalized RRIDs.
#'
#' @return `x`, the normalized RRIDs.
#'
#' @noRd
key_rrid <- function(x) {
    x
}


#' Comparison keys for UniProt accession numbers
#'
#' @param x A character vector of normalized UniProt accessions.
#'
#' @return `x`, the normalized UniProt accessions.
#'
#' @noRd
key_uniprot <- function(x) {
    x
}


#' Comparison keys for SRA accession numbers
#'
#' @param x A character vector of normalized SRA accessions.
#'
#' @return `x`, the normalized SRA accessions.
#'
#' @noRd
key_sra <- function(x) {
    x
}


#' Comparison keys for GEO accession numbers
#'
#' @param x A character vector of normalized GEO accessions.
#'
#' @return `x`, the normalized GEO accessions.
#'
#' @noRd
key_geo <- function(x) {
    x
}


#' Comparison keys for BioProject accession numbers
#'
#' @param x A character vector of normalized BioProject accessions.
#'
#' @return `x`, the normalized BioProject accessions.
#'
#' @noRd
key_bioproject <- function(x) {
    x
}


#' Comparison keys for ISSN identifiers
#'
#' @param x A character vector of normalized ISSNs.
#'
#' @return `x`, the normalized ISSNs.
#'
#' @noRd
key_issn <- function(x) {
    x
}


#' Comparison keys for PubMed Central identifiers
#'
#' @param x A character vector of normalized PMCIDs.
#'
#' @return `x`, the normalized PMCIDs.
#'
#' @noRd
key_pmcid <- function(x) {
    x
}


#' Comparison keys for PubMed identifiers
#'
#' @param x A character vector of normalized PMIDs.
#'
#' @return `x`, the normalized PMIDs.
#'
#' @noRd
key_pmid <- function(x) {
    x
}


# Level 2 functions (functions called by level 1 functions) definitions --------


#' Drop the version suffix of canonical identifiers
#'
#' @description
#' Removes the match of the type's registry `version_pat`. Values without a
#' version and missing values are returned unchanged.
#'
#' @param x A character vector of normalized identifiers.
#' @param type A validated identifier type with a `version_pat`.
#'
#' @return A character vector the same length as `x`.
#'
#' @noRd
.scholid_drop_version <- function(
        x,
        type
) {
    sub(
        .scholid_registry()[[type]]$version_pat,
        "",
        x,
        perl = TRUE
    )
}


#' Uppercase ASCII letters and keep other characters
#'
#' @description
#' Maps `a`-`z` to `A`-`Z` in every locale. `toupper()` would also change
#' non-ASCII letters, and it follows the locale's rules (in a Turkish
#' locale, `i` can become U+0130). ASCII-only values go through `chartr()`.
#' Values with a non-ASCII character are converted to UTF-8 and go through
#' `gsub(fixed = TRUE)`, because `chartr()` can fail on UTF-8 text in a
#' non-UTF-8 locale. Missing values, and values that are not valid UTF-8,
#' are returned unchanged.
#'
#' @param x A character vector.
#'
#' @return A character vector the same length as `x`.
#'
#' @noRd
.scholid_ascii_upper <- function(x) {
    ascii <- !is.na(x) & !grepl(
        "[^\\x01-\\x7F]",
        x,
        perl     = TRUE,
        useBytes = TRUE
    )
    x[ascii] <- chartr(
        paste(letters, collapse = ""),
        paste(LETTERS, collapse = ""),
        x[ascii]
    )

    cand <- .scholid_unicode_candidates(x)
    if (length(cand$idx)) {
        y <- cand$y
        for (i in seq_along(letters)) {
            y <- gsub(
                letters[[i]],
                LETTERS[[i]],
                y,
                fixed = TRUE
            )
        }
        x[cand$idx] <- y
    }
    x
}
