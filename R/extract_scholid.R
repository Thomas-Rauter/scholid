#' Extract scholarly identifiers from text
#'
#' @description
#' Extract identifiers of a single supported type from free text.
#'
#' The result is a list with one element per input element. Each element is a
#' character vector of matches (possibly length 0). `NA` inputs yield an empty
#' character vector.
#'
#' Matches are returned as extracted identifier tokens from the text.
#' Surrounding prose punctuation or markup fragments may be removed where
#' necessary to isolate the identifier. Use [normalize_scholid()] to
#' convert identifiers to canonical form.
#'
#' Before matching, invisible characters are removed from the text and
#' Unicode spaces count as whitespace. For some types, Unicode dashes and
#' full-width digits are read as their ASCII forms, and the returned tokens
#' use the ASCII forms. See "Input characters" in
#' `vignette("scholid_definitions", package = "scholid")`.
#'
#' @param text A character vector of text.
#' @param type A single string giving the identifier type. See
#'   [scholid_types()] for supported values.
#'
#' @return A list with the same length as `text`, whose elements are
#'   character vectors of extracted identifiers.
#'
#' @examples
#' extract_scholid("See https://doi.org/10.1000/182.", "doi")
#' extract_scholid("ORCID 0000-0002-1825-0097", "orcid")
#'
#' @seealso [locate_scholid()] to find identifiers of several types at once,
#'   with their positions in the text, [normalize_scholid()],
#'   [scholid_types()]
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

    .scholid_dispatch(
        type   = type,
        prefix = "extract_",
        x      = text
    )
}


# Level 1 functions (functions called by exported functions) definitions -------


#' Locate identifiers of one type in text
#'
#' @description
#' Internal helper that returns every identifier `extract_scholid()` finds
#' for `type`, one row per hit, with its position in `text`. It calls
#' `extract_<type>(text, positions = TRUE)`, the code path of
#' `extract_scholid()`, so `id` split by `element` gives the same tokens.
#'
#' The span is the identifier as written. It starts at the `id` group of
#' the registry `extract_pat`, so it leaves out a URL, host, or label in
#' front of the identifier but keeps a prefix of the canonical form, such
#' as `RRID:`, `ark:`, or `swh:1:`. It leaves out the trailing characters
#' that the type's cleaner trims. It can contain characters that `id` does
#' not: spaces between digit groups, look-alike dashes and digits,
#' invisible characters, and letters in another case.
#'
#' Positions refer to `as.character(text)`, including the invisible
#' characters that extraction removes, and count characters, as `substr()`
#' does. In text marked `"bytes"`, and in text that is not valid in its
#' encoding, they count bytes. R's regular expression engine skips text
#' that is not valid UTF-8 with a warning unless another element is marked
#' `"bytes"`, so such text usually gives no rows, as it gives no tokens in
#' `extract_scholid()`. Where it gives rows, `match` is marked `"bytes"`.
#'
#' @param text A vector of text, or a cache from `.scholid_text_cache()`.
#' @param type A validated identifier type string.
#'
#' @return A data frame with one row per hit, in the order
#'   `extract_scholid()` returns the tokens, and the columns `element`
#'   (integer index into `text`), `id` (the extracted token), `match` (the
#'   identifier as written), and `start` and `end` (integer positions of
#'   `match` in `text[element]`, 1-based, `end` inclusive), so that
#'   `substr(text[element], start, end) == match`. `NA` and empty elements
#'   give no rows.
#'
#' @noRd
.scholid_locate_type <- function(
        text,
        type
) {
    fun <- .scholid_resolve_impl(
        type   = type,
        prefix = "extract_"
    )
    fun(
        text,
        positions = TRUE
    )
}


#' Cache the cleaned forms of a text vector
#'
#' @description
#' Internal helper that lets several extraction calls on the same text
#' share the work of `.scholid_clean_chars()`. `.scholid_extract_validated()`
#' accepts the cache in place of `text` and cleans the text once per
#' combination of `dashes` and `digits`, not once per type.
#'
#' @param text A vector of text.
#'
#' @return An environment with `text`, the text as a character vector, and
#'   `work`, a list of cleaned text by folding flags, filled on first use.
#'
#' @noRd
.scholid_text_cache <- function(text) {
    cache <- new.env(parent = emptyenv())
    cache$text <- as.character(text)
    cache$work <- list()
    cache
}


## extract_<id>() function definitions -----------------------------------------


#' Extract DOI identifiers from text
#'
#' @description
#' Extracts Digital Object Identifiers (DOIs) from free text or URLs.
#'
#' Extracted DOI candidates are cleaned to remove surrounding prose punctuation
#' or markup tails where necessary to isolate the DOI token, and only cleaned
#' candidates that satisfy DOI structure rules are returned.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted DOIs.
#'
#' @noRd
extract_doi <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "doi",
        clean_fn    = .clean_extracted_doi,
        validate_fn = is_doi,
        positions   = positions
    )
}


#' Extract ARK identifiers from text
#'
#' @description
#' Extracts Archival Resource Keys from free text or resolver URLs.
#'
#' Extracted ARK candidates are cleaned to remove URL prefixes and trailing
#' prose punctuation where necessary, and only structurally valid ARKs are
#' returned.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted ARKs.
#'
#' @noRd
extract_ark <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "ark",
        clean_fn    = .clean_extracted_ark,
        validate_fn = is_ark,
        positions   = positions
    )
}


#' Extract ISNI identifiers from text
#'
#' @description
#' Extracts International Standard Name Identifiers from free text, labels,
#' or resolver URLs.
#'
#' Extracted ISNI candidates are cleaned to remove URL prefixes and trailing
#' prose punctuation where necessary, and only checksum-valid compact ISNIs
#' are returned.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted ISNIs.
#'
#' @noRd
extract_isni <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "isni",
        clean_fn    = .clean_extracted_isni,
        validate_fn = is_isni,
        dashes      = TRUE,
        digits      = TRUE,
        positions   = positions
    )
}


#' Extract ORCID identifiers from text
#'
#' @description
#' Extracts ORCID iDs from free text or URLs.
#'
#' Extracted ORCID candidates are cleaned to remove trailing prose punctuation
#' where necessary, and only checksum-valid ORCID iDs are returned.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted ORCID iDs.
#'
#' @noRd
extract_orcid <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "orcid",
        clean_fn    = .clean_extracted_trailing_punct,
        validate_fn = is_orcid,
        dashes      = TRUE,
        digits      = TRUE,
        positions   = positions
    )
}


#' Extract UniProt accession numbers from text
#'
#' @description
#' Extracts UniProtKB accession numbers from free text or resolver URLs.
#'
#' Extracted UniProt candidates are cleaned to remove URL prefixes and
#' trailing prose punctuation where necessary, and only structurally valid
#' accessions are returned.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted UniProt accessions.
#'
#' @noRd
extract_uniprot <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "uniprot",
        clean_fn    = .clean_extracted_uniprot,
        validate_fn = is_uniprot,
        digits      = TRUE,
        positions   = positions
    )
}


#' Extract RefSeq accession numbers from text
#'
#' @description
#' Extracts RefSeq accessions from free text or URLs.
#'
#' Extracted RefSeq candidates are cleaned to remove URL prefixes and
#' trailing prose punctuation where necessary, and only structurally valid
#' accessions are returned.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted RefSeq accessions.
#'
#' @noRd
extract_refseq <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "refseq",
        clean_fn    = .clean_extracted_refseq,
        validate_fn = is_refseq,
        digits      = TRUE,
        positions   = positions
    )
}


#' Extract SRA accession numbers from text
#'
#' @description
#' Extracts SRA accessions from free text or URLs.
#'
#' Extracted SRA candidates are cleaned to remove URL prefixes and trailing
#' prose punctuation where necessary, and only structurally valid accessions
#' are returned.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted SRA accessions.
#'
#' @noRd
extract_sra <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "sra",
        clean_fn    = .clean_extracted_sra,
        validate_fn = is_sra,
        digits      = TRUE,
        positions   = positions
    )
}


#' Extract GEO accession numbers from text
#'
#' @description
#' Extracts GEO accessions from free text or URLs.
#'
#' Extracted GEO candidates are cleaned to remove URL prefixes and trailing
#' prose punctuation where necessary, and only structurally valid accessions
#' are returned.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted GEO accessions.
#'
#' @noRd
extract_geo <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "geo",
        clean_fn    = .clean_extracted_geo,
        validate_fn = is_geo,
        digits      = TRUE,
        positions   = positions
    )
}


#' Extract BioProject accession numbers from text
#'
#' @description
#' Extracts BioProject accessions from free text or URLs.
#'
#' Extracted BioProject candidates are cleaned to remove URL prefixes and
#' trailing prose punctuation where necessary, and only structurally valid
#' accessions are returned.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted BioProject accessions.
#'
#' @noRd
extract_bioproject <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "bioproject",
        clean_fn    = .clean_extracted_bioproject,
        validate_fn = is_bioproject,
        digits      = TRUE,
        positions   = positions
    )
}


#' Extract genome assembly accession numbers from text
#'
#' @description
#' Extracts INSDC assembly accessions from free text or URLs.
#'
#' Extracted assembly candidates are cleaned to remove URL prefixes and
#' trailing prose punctuation where necessary, and only structurally valid
#' accessions are returned.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted assembly accessions.
#'
#' @noRd
extract_assembly <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "assembly",
        clean_fn    = .clean_extracted_assembly,
        validate_fn = is_assembly,
        digits      = TRUE,
        positions   = positions
    )
}


#' Extract ROR identifiers from text
#'
#' @description
#' Extracts ROR iDs from free text or URLs.
#'
#' Extracted ROR candidates are cleaned to remove URL prefixes and trailing
#' prose punctuation where necessary, and only checksum-valid ROR iDs are
#' returned.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted ROR iDs.
#'
#' @noRd
extract_ror <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "ror",
        clean_fn    = .clean_extracted_ror,
        validate_fn = is_ror,
        digits      = TRUE,
        positions   = positions
    )
}


#' Extract RRID identifiers from text
#'
#' @description
#' Extracts Research Resource Identifiers from free text or resolver URLs.
#'
#' Extracted RRID candidates are cleaned to remove URL prefixes and trailing
#' prose punctuation where necessary, and only structurally valid RRIDs for
#' known authorities are returned.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted RRIDs.
#'
#' @noRd
extract_rrid <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "rrid",
        clean_fn    = .clean_extracted_rrid,
        validate_fn = is_rrid,
        positions   = positions
    )
}


#' Extract ISBN identifiers from text
#'
#' @description
#' Extracts ISBN-10 and ISBN-13 identifiers from free text.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted ISBNs.
#'
#' @noRd
extract_isbn <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "isbn",
        clean_fn    = .clean_extracted_trailing_punct,
        validate_fn = is_isbn,
        dashes      = TRUE,
        digits      = TRUE,
        positions   = positions
    )
}


#' Extract ISSN identifiers from text
#'
#' @description
#' Extracts ISSN identifiers from free text.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted ISSNs.
#'
#' @noRd
extract_issn <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "issn",
        clean_fn    = .clean_extracted_trailing_punct,
        validate_fn = is_issn,
        digits      = TRUE,
        positions   = positions
    )
}


#' Extract arXiv identifiers from text
#'
#' @description
#' Extracts arXiv identifiers in both modern and legacy formats.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted arXiv identifiers.
#'
#' @noRd
extract_arxiv <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "arxiv",
        clean_fn    = .clean_extracted_trailing_punct,
        validate_fn = is_arxiv,
        digits      = TRUE,
        positions   = positions
    )
}


#' Extract ADS bibcodes from text
#'
#' @description
#' Extracts SAO/NASA ADS bibliographic codes from free text or ADS URLs.
#'
#' Extracted bibcode candidates are cleaned to remove URL prefixes and
#' trailing prose punctuation where necessary, and only structurally valid
#' bibcodes are returned.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted bibcodes.
#'
#' @noRd
extract_bibcode <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "bibcode",
        clean_fn    = .clean_extracted_bibcode,
        validate_fn = is_bibcode,
        digits      = TRUE,
        positions   = positions
    )
}


#' Extract OpenAlex identifiers from text
#'
#' @description
#' Extracts OpenAlex IDs from free text or OpenAlex URLs.
#'
#' Extracted OpenAlex candidates are cleaned to remove URL prefixes and
#' trailing prose punctuation where necessary, and only structurally valid
#' identifiers are returned.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted OpenAlex IDs.
#'
#' @noRd
extract_openalex <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "openalex",
        clean_fn    = .clean_extracted_openalex,
        validate_fn = is_openalex,
        digits      = TRUE,
        positions   = positions
    )
}


#' Extract SWHID identifiers from text
#'
#' @description
#' Extracts Software Heritage identifiers from free text or resolver URLs.
#'
#' Extracted SWHID candidates are cleaned to remove URL prefixes and trailing
#' prose punctuation where necessary, and only structurally valid SWHIDs are
#' returned.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted SWHIDs.
#'
#' @noRd
extract_swhid <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "swhid",
        clean_fn    = .clean_extracted_swhid,
        validate_fn = is_swhid,
        positions   = positions
    )
}


#' Extract PubMed identifiers from text
#'
#' @description
#' Extracts PubMed identifiers (PMIDs) from free text.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted PMIDs.
#'
#' @noRd
extract_pmid <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "pmid",
        clean_fn    = .clean_extracted_trailing_punct,
        validate_fn = is_pmid,
        digits      = TRUE,
        positions   = positions
    )
}


#' Extract PubMed Central identifiers from text
#'
#' @description
#' Extracts PubMed Central identifiers (PMCIDs) from free text.
#'
#' @param text A character vector of text.
#' @param positions If `TRUE`, return a data frame of hits with their
#'   positions instead; see `.scholid_locate_type()`.
#'
#' @return A list of character vectors of extracted PMCIDs.
#'
#' @noRd
extract_pmcid <- function(
        text,
        positions = FALSE
) {
    .scholid_extract_validated(
        text        = text,
        type        = "pmcid",
        clean_fn    = .clean_extracted_trailing_punct,
        validate_fn = is_pmcid,
        digits      = TRUE,
        positions   = positions
    )
}


# Level 2 functions (functions called by level 1 functions) definitions --------


#' Match a regular expression in text
#'
#' @description
#' Internal helper that cleans the text with `.scholid_clean_chars()`, or
#' takes the cleaned text from `cache` if an earlier call cleaned it with
#' the same flags, and applies a single regular expression pattern with
#' `gregexpr()` and `perl = TRUE`. `NA` inputs are matched as `""`, so they
#' have no matches.
#'
#' @param cache A cache from `.scholid_text_cache()`.
#' @param pat A single regular expression pattern.
#' @param dashes Whether to fold Unicode dashes.
#' @param digits Whether to fold full-width digits.
#'
#' @return A list with `work`, the cleaned text; `m`, the `gregexpr()`
#'   result on it; and `hits`, a list with one character vector of matches
#'   per input element.
#'
#' @noRd
.extract_with_pattern <- function(
        cache,
        pat,
        dashes = FALSE,
        digits = FALSE
) {
    key <- paste0("dashes_", dashes, "_digits_", digits)
    work <- cache$work[[key]]
    if (is.null(work)) {
        work <- .scholid_clean_chars(
            cache$text,
            dashes = dashes,
            digits = digits
        )
        work[is.na(work)] <- ""
        cache$work[[key]] <- work
    }
    m <- gregexpr(pat, work, perl = TRUE)
    list(
        work = work,
        m    = m,
        hits = regmatches(work, m)
    )
}


#' Clean, filter, and validate extracted identifier candidates
#'
#' @description
#' Internal helper that post-processes regex extraction results. All matches
#' are cleaned with `clean_fn`, then filtered to non-empty values and
#' validated with `validate_fn`.
#'
#' @param out A list of character vectors of raw regex matches.
#' @param clean_fn Vectorized cleaner. Must return a character vector the
#'   same length as its input. Missing or empty inputs become `""`.
#' @param validate_fn Vectorized validator returning logical values.
#'
#' @return A list with one value per validated identifier, in input order,
#'   in each of `element`, the index into `out`; `index`, the position
#'   among all matches, `unlist(out)`; and `id`, the cleaned identifier.
#'
#' @noRd
.extract_filter_validate <- function(
        out,
        clean_fn,
        validate_fn
) {
    lens <- lengths(out)
    if (!any(lens)) {
        return(list(
            element = integer(),
            index   = integer(),
            id      = character()
        ))
    }

    hits <- unlist(out, use.names = FALSE)
    cleaned <- clean_fn(hits)
    keep <- !is.na(cleaned) & nzchar(cleaned)
    if (any(keep)) {
        ok <- validate_fn(cleaned[keep])
        ok[is.na(ok)] <- FALSE
        keep[keep] <- ok
    }

    list(
        element = rep.int(seq_along(out), lens)[keep],
        index   = which(keep),
        id      = cleaned[keep]
    )
}


#' Split validated identifiers by input element
#'
#' @param hits A list as returned by `.extract_filter_validate()`.
#' @param n The number of input elements.
#'
#' @return A list of `n` character vectors of validated identifiers.
#'
#' @noRd
.extract_split_hits <- function(
        hits,
        n
) {
    if (!length(hits$id)) {
        return(rep(list(character(0)), n))
    }

    res <- split(
        hits$id,
        factor(hits$element, levels = seq_len(n))
    )
    names(res) <- NULL
    res
}


#' Replace missing or empty strings with an empty string
#'
#' @param x A character vector.
#'
#' @return A character vector the same length as `x`.
#'
#' @noRd
.empty_if_blank <- function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x)] <- ""
    x
}


#' Extract and validate identifiers using registry patterns
#'
#' @description
#' Internal helper that extracts identifier candidates from free text using
#' the registry `extract_pat` for a type, then cleans and validates matches.
#' With `positions = TRUE`, it also locates each identifier in `text`.
#'
#' @param text A character vector of text, or a cache from
#'   `.scholid_text_cache()`, which the `extract_<type>()` functions pass
#'   on unchanged.
#' @param type A validated identifier type string.
#' @param clean_fn Function applied to each raw match.
#' @param validate_fn Vectorized validator returning logical values.
#' @param dashes Whether to fold Unicode dashes in `text`.
#' @param digits Whether to fold full-width digits in `text`.
#' @param positions Whether to return a data frame of hits with their
#'   positions, as described in `.scholid_locate_type()`.
#'
#' @return A list of character vectors of validated identifiers, or with
#'   `positions = TRUE`, a data frame.
#'
#' @noRd
.scholid_extract_validated <- function(
        text,
        type,
        clean_fn,
        validate_fn,
        dashes    = FALSE,
        digits    = FALSE,
        positions = FALSE
) {
    cache <- text
    if (!is.environment(cache)) {
        cache <- .scholid_text_cache(text)
    }
    pat <- .scholid_registry_extract_pat(type)
    # Same matches; gregexpr() is faster without the capture group.
    matched <- .extract_with_pattern(
        cache  = cache,
        pat    = sub("(?<id>", "(?:", pat, fixed = TRUE),
        dashes = dashes,
        digits = digits
    )
    hits <- .extract_filter_validate(
        out         = matched$hits,
        clean_fn    = clean_fn,
        validate_fn = validate_fn
    )

    if (!positions) {
        return(.extract_split_hits(
            hits = hits,
            n    = length(cache$text)
        ))
    }

    .extract_located(
        text     = cache$text,
        matched  = matched,
        hits     = hits,
        pat      = pat,
        clean_fn = clean_fn
    )
}


#' Clean an extracted bibcode candidate
#'
#' @description
#' Removes URL prefixes, trailing punctuation, and surrounding whitespace
#' from an extracted bibcode candidate.
#'
#' @param x A character vector of extracted bibcode candidates.
#'
#' @return A character vector of cleaned candidates, with `""` for blank
#'   inputs.
#'
#' @noRd
.clean_extracted_bibcode <- function(x) {
    x <- .empty_if_blank(x)
    x <- sub("[.,;:!?]+$", "", x, perl = TRUE)
    x <- trimws(x)
    x <- sub(
        "^https?://(?:ui\\.)?adsabs\\.harvard\\.edu/abs/",
        "",
        x,
        ignore.case = TRUE
    )
    sub("(?i)^bibcode\\s*:?\\s*", "", x, perl = TRUE)
}


.clean_extracted_openalex <- function(x) {
    x <- .empty_if_blank(x)
    x <- sub("[[:space:][:punct:]]+$", "", x, perl = TRUE)
    x <- trimws(x)
    x <- sub("^https?://openalex\\.org/", "", x, ignore.case = TRUE)
    x <- sub(
        paste0(
            "^https?://api\\.openalex\\.org/",
            "(?:works|authors|sources|institutions|topics|keywords|",
            "publishers|funders|grants|concepts)/"
        ),
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub("/+$", "", x)
    toupper(x)
}


.clean_extracted_uniprot <- function(x) {
    x <- .empty_if_blank(x)
    x <- sub("[.,;:!?]+$", "", x, perl = TRUE)
    x <- trimws(x)
    x <- sub(
        "^https?://(?:www\\.)?uniprot\\.org/(?:uniprot|uniprotkb)/",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub(
        "^https?://identifiers\\.org/uniprot/",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub("(?i)^uniprot:", "", x, perl = TRUE)
    toupper(x)
}


.clean_extracted_refseq <- function(x) {
    x <- .empty_if_blank(x)
    x <- sub("[.,;:!?]+$", "", x, perl = TRUE)
    x <- trimws(x)
    x <- sub(
        "^https?://www\\.ncbi\\.nlm\\.nih\\.gov/(?:nuccore|protein)/",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub(
        "^https?://identifiers\\.org/refseq/",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub("(?i)^refseq:", "", x, perl = TRUE)
    toupper(x)
}


.clean_extracted_sra <- function(x) {
    x <- .empty_if_blank(x)
    x <- sub("[.,;:!?]+$", "", x, perl = TRUE)
    x <- trimws(x)
    x <- sub(
        "^https?://www\\.ncbi\\.nlm\\.nih\\.gov/sra/",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub(
        "^https?://identifiers\\.org/sra/",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub("(?i)^sra:", "", x, perl = TRUE)
    toupper(x)
}


.clean_extracted_geo <- function(x) {
    x <- .empty_if_blank(x)
    x <- sub("[.,;:!?]+$", "", x, perl = TRUE)
    x <- trimws(x)
    x <- sub(
        "^https?://www\\.ncbi\\.nlm\\.nih\\.gov/geo/query/acc\\.cgi\\?acc=",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub(
        "^https?://identifiers\\.org/geo/",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub("(?i)^geo:", "", x, perl = TRUE)
    x <- sub("[?#&].*$", "", x)
    toupper(x)
}


.clean_extracted_bioproject <- function(x) {
    x <- .empty_if_blank(x)
    x <- sub("[.,;:!?]+$", "", x, perl = TRUE)
    x <- trimws(x)
    x <- sub(
        "^https?://www\\.ncbi\\.nlm\\.nih\\.gov/bioproject/",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub(
        "^https?://identifiers\\.org/bioproject[:/]",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub("(?i)^bioproject:", "", x, perl = TRUE)
    x <- sub("(?i)^\\?term=", "", x, perl = TRUE)
    x <- sub("[?#&].*$", "", x)
    toupper(x)
}


.clean_extracted_assembly <- function(x) {
    x <- .empty_if_blank(x)
    x <- sub("[.,;:!?]+$", "", x, perl = TRUE)
    x <- trimws(x)
    x <- sub(
        "^https?://www\\.ncbi\\.nlm\\.nih\\.gov/(?:assembly|datasets/genome)/",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub(
        "^https?://identifiers\\.org/insdc\\.(?:gca|gcf):",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub("(?i)^assembly:", "", x, perl = TRUE)
    x <- sub("/+$", "", x)
    toupper(x)
}


.clean_extracted_ark <- function(x) {
    x <- .empty_if_blank(x)
    x <- sub("[.,;:!?]+$", "", x, perl = TRUE)
    val <- .canonicalize_ark(trimws(x))
    val[is.na(val)] <- ""
    val
}


.clean_extracted_isni <- function(x) {
    x <- .empty_if_blank(x)
    x <- sub("[.,;:!?]+$", "", x, perl = TRUE)
    x <- trimws(x)
    x <- sub("^https?://isni\\.org/isni/", "", x, ignore.case = TRUE)
    x <- sub("(?i)^urn:isni:", "", x, perl = TRUE)
    x <- sub("(?i)^isni[[:space:]]*:?[[:space:]]*", "", x, perl = TRUE)
    x <- sub(
        "(?i)^https?://viaf\\.org/viaf/sourceID/ISNI(?:%7C|\\|)",
        "",
        x,
        perl = TRUE
    )
    toupper(gsub("[-[:space:]]", "", x))
}


.clean_extracted_ror <- function(x) {
    x <- .empty_if_blank(x)
    x <- sub("[[:space:][:punct:]]+$", "", x, perl = TRUE)
    x <- trimws(x)
    x <- sub("^https?://ror\\.org/", "", x, ignore.case = TRUE)
    x <- sub("^ror\\.org/", "", x, ignore.case = TRUE)
    x <- sub("/+$", "", x)
    tolower(x)
}


#' Clean an extracted RRID candidate
#'
#' @description
#' Removes resolver URL prefixes, trailing punctuation, and surrounding
#' whitespace from an extracted RRID candidate, and normalizes the `RRID:`
#' label.
#'
#' @param x A character vector of extracted RRID candidates.
#'
#' @return A character vector of cleaned candidates, with `""` for blank
#'   inputs.
#'
#' @noRd
.clean_extracted_rrid <- function(x) {
    x <- .empty_if_blank(x)
    x <- sub("[[:space:][:punct:]]+$", "", x, perl = TRUE)
    x <- trimws(x)
    x <- sub(
        "^https?://scicrunch\\.org/resolver/",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub(
        "^https?://identifiers\\.org/",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub(
        "^https?://n2t\\.net/rrid:",
        "",
        x,
        ignore.case = TRUE
    )
    sub("^RRID[[:space:]]*:[[:space:]]*", "RRID:", x, ignore.case = TRUE)
}


#' Clean an extracted SWHID candidate
#'
#' @description
#' Removes resolver URL prefixes, trailing prose punctuation, and surrounding
#' whitespace from an extracted SWHID candidate, and canonicalizes the core
#' identifier.
#'
#' @param x A character vector of extracted SWHID candidates.
#'
#' @return A character vector of cleaned candidates, with `""` for blank
#'   inputs.
#'
#' @noRd
.clean_extracted_swhid <- function(x) {
    x <- .empty_if_blank(x)
    x <- sub("[.,;:!?\"']+$", "", x, perl = TRUE)
    x <- trimws(x)
    x <- sub(
        "^https?://archive\\.softwareheritage\\.org/",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub(
        "^https?://browse\\.softwareheritage\\.org/",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub(
        "^https?://identifiers\\.org/swh/",
        "",
        x,
        ignore.case = TRUE
    )
    x <- gsub("[[:space:]]+", "", x)
    .canonicalize_swhid(x)
}


#' Clean an extracted DOI candidate
#'
#' @description
#' Removes obvious surrounding markup tails and trailing prose delimiters from
#' a DOI candidate extracted from free text, while preserving valid DOI-internal
#' punctuation where possible.
#'
#' @param x A character vector of extracted DOI candidates.
#'
#' @return A character vector of cleaned candidates, with `""` for blank
#'   inputs.
#'
#' @noRd
.clean_extracted_doi <- function(x) {
    x <- .empty_if_blank(x)
    if (!length(x)) {
        return(character())
    }

    x <- .strip_doi_markup_tail(x)

    repeat {
        old <- x
        x <- sub("[.,;:!?\"']+$", "", x, perl = TRUE)
        x <- .strip_unmatched_trailing_closer(x, "\\)", "\\(")
        x <- .strip_unmatched_trailing_closer(x, "\\]", "\\[")
        x <- .strip_unmatched_trailing_closer(x, "\\}", "\\{")
        x <- .strip_unmatched_trailing_closer(x, ">", "<")
        if (identical(x, old)) {
            break
        }
    }

    bad <- nzchar(x) & !is_doi(x)
    bad[is.na(bad)] <- FALSE
    if (any(bad)) {
        x[bad] <- vapply(
            x[bad],
            .truncate_to_valid_doi_prefix,
            character(1),
            USE.NAMES = FALSE
        )
    }
    x
}


#' Clean trailing punctuation and whitespace from an extracted candidate
#'
#' @description
#' Removes trailing punctuation and surrounding whitespace from an extracted
#' identifier candidate.
#'
#' @param x A character vector of extracted identifier candidates.
#'
#' @return A character vector of cleaned candidates, with `""` for blank
#'   inputs.
#'
#' @noRd
.clean_extracted_trailing_punct <- function(x) {
    x <- .empty_if_blank(x)
    x <- sub("[[:space:][:punct:]]+$", "", x, perl = TRUE)
    trimws(x)
}


# Level 3 functions (functions called by level 2 functions) definitions --------


#' Build the data frame of located identifiers
#'
#' @description
#' Internal helper for `.scholid_extract_validated(positions = TRUE)`. It
#' finds each span in the cleaned text with `.extract_spans()`, maps it back
#' to `text` with `.extract_map_positions()`, and reads `match` from `text`.
#'
#' @param text A character vector of original text.
#' @param matched A list as returned by `.extract_with_pattern()`.
#' @param hits A list as returned by `.extract_filter_validate()`.
#' @param pat The registry `extract_pat`, with its `id` group.
#' @param clean_fn The cleaner that produced `hits`.
#'
#' @return A data frame as described in `.scholid_locate_type()`.
#'
#' @noRd
.extract_located <- function(
        text,
        matched,
        hits,
        pat,
        clean_fn
) {
    span <- .extract_spans(
        matched  = matched,
        hits     = hits,
        pat      = pat,
        clean_fn = clean_fn
    )
    src <- text[hits$element]
    pos <- .extract_map_positions(
        orig      = src,
        work      = matched$work[hits$element],
        start     = span$start,
        end       = span$end,
        use_bytes = span$use_bytes
    )
    Encoding(src[pos$bytes]) <- "bytes"

    data.frame(
        element          = hits$element,
        id               = hits$id,
        match            = substr(src, pos$start, pos$end),
        start            = pos$start,
        end              = pos$end,
        stringsAsFactors = FALSE
    )
}


#' Find the span of each validated identifier in the cleaned text
#'
#' @description
#' A span starts at the `id` group of the match. It ends where the shortest
#' prefix of the match that still cleans to the same identifier ends, so it
#' leaves out the trailing characters that `clean_fn` trims. Trimming one
#' character at a time from the end can stop too early: `clean_fn` turns
#' `10.1000/182.</a>` and `10.1000/182.</a` into `10.1000/182`, but
#' `10.1000/182.</` into `10.1000/182.`.
#'
#' `matched` comes from `pat` with the `id` group made non-capturing.
#' `pat` itself is matched again only on the elements that have a match,
#' in the same unit, so it finds the same matches.
#'
#' @param matched A list as returned by `.extract_with_pattern()`.
#' @param hits A list as returned by `.extract_filter_validate()`.
#' @param pat The registry `extract_pat`, with its `id` group.
#' @param clean_fn The cleaner that produced `hits`.
#'
#' @return A list with integer vectors `start` and `end`, one value per
#'   hit, and `use_bytes`, whether `gregexpr()` counted them in bytes.
#'
#' @noRd
.extract_spans <- function(
        matched,
        hits,
        pat,
        clean_fn
) {
    idx <- hits$index
    if (!length(idx)) {
        return(list(
            start     = integer(),
            end       = integer(),
            use_bytes = FALSE
        ))
    }

    has <- lengths(matched$hits) > 0L
    use_bytes <- any(unlist(lapply(
        matched$m[has],
        attr,
        which = "useBytes"
    )))
    m <- gregexpr(
        pat,
        matched$work[has],
        perl     = TRUE,
        useBytes = use_bytes
    )
    raw <- unlist(matched$hits, use.names = FALSE)[idx]
    match_start <- unlist(m, use.names = FALSE)[idx]
    cap_start <- do.call(rbind, lapply(m, attr, which = "capture.start"))
    cap_length <- do.call(rbind, lapply(m, attr, which = "capture.length"))
    id_start <- unname(cap_start[idx, "id"])

    # Lengths of the prefixes of raw that end at the start and at the end
    # of the id group. The shortest prefix that cleans to id lies between.
    # Cleaners remove characters or change their case, and add at most one
    # (ark: becomes ark:/), so it is at least first + nchar(id) - 2 long.
    first <- id_start - match_start + 1L
    full <- first + unname(cap_length[idx, "id"]) - 1L
    n_id <- nchar(hits$id, type = if (use_bytes) "bytes" else "chars")
    len <- pmin(pmax(first, first + n_id - 2L), full)
    todo <- which(len < full)
    while (length(todo)) {
        same <- clean_fn(substr(raw[todo], 1L, len[todo])) == hits$id[todo]
        todo <- todo[is.na(same) | !same]
        len[todo] <- len[todo] + 1L
        todo <- todo[len[todo] < full[todo]]
    }

    list(
        start     = id_start,
        end       = match_start + len - 1L,
        use_bytes = use_bytes
    )
}


#' Map positions in cleaned text back to the original text
#'
#' @description
#' `.scholid_clean_chars()` deletes invisible characters and folds
#' look-alikes one character for one, so only a deleted character shifts
#' the positions that follow it. When `gregexpr()` counted bytes, each
#' multibyte character shifts them too. The result counts characters of
#' `orig`, as `substr()` does, or bytes where `orig` is marked `"bytes"` or
#' is not valid in its encoding. Only non-ASCII values that had a character
#' deleted, or were counted in bytes, are mapped, one at a time.
#'
#' @param orig A character vector of original text, one value per hit.
#' @param work The cleaned text, one value per hit.
#' @param start,end Integer positions of each hit in `work`.
#' @param use_bytes Whether `start` and `end` count bytes.
#'
#' @return A list with integer vectors `start` and `end`, and a logical
#'   vector `bytes`, whether the positions of each hit count bytes.
#'
#' @noRd
.extract_map_positions <- function(
        orig,
        work,
        start,
        end,
        use_bytes
) {
    bytes <- rep(FALSE, length(orig))
    wide <- grepl(
        "[^\\x01-\\x7F]",
        orig,
        perl     = TRUE,
        useBytes = TRUE
    )
    if (!any(wide)) {
        return(list(
            start = start,
            end   = end,
            bytes = bytes
        ))
    }

    bytes[wide] <- is.na(nchar(orig[wide], allowNA = TRUE))
    remap <- wide
    if (!use_bytes) {
        remap[wide] <- .scholid_has_chars(
            orig[wide],
            .scholid_invisible_chars()
        )
    }

    invisible <- utf8ToInt(paste(.scholid_invisible_chars(), collapse = ""))
    for (i in which(remap)) {
        o <- utf8ToInt(enc2utf8(orig[[i]]))
        w <- utf8ToInt(enc2utf8(work[[i]]))
        if (anyNA(o) || anyNA(w)) {
            next
        }

        at <- c(start[[i]], end[[i]])
        if (use_bytes) {
            at <- rep.int(seq_along(w), .utf8_nbytes(w))[at]
        }
        # Character j of work is character kept[j] of orig.
        kept <- which(!o %in% invisible)
        at <- kept[at]
        if (bytes[[i]]) {
            last <- cumsum(.utf8_nbytes(o))
            at <- c(c(0L, last)[at[[1L]]] + 1L, last[at[[2L]]])
        }
        start[[i]] <- at[[1L]]
        end[[i]] <- at[[2L]]
    }

    list(
        start = start,
        end   = end,
        bytes = bytes
    )
}


#' Number of UTF-8 bytes of each code point
#'
#' @param x An integer vector of Unicode code points.
#'
#' @return An integer vector the same length as `x`.
#'
#' @noRd
.utf8_nbytes <- function(x) {
    1L + (x >= 128L) + (x >= 2048L) + (x >= 65536L)
}


#' Strip obvious markup tails from an extracted DOI candidate
#'
#' @description
#' Removes trailing HTML or attribute fragments that may be captured when a DOI
#' appears inside markup such as an anchor tag or quoted URL attribute.
#'
#' @param x A character vector of extracted DOI candidates.
#'
#' @return A character vector the same length as `x`.
#'
#' @noRd
.strip_doi_markup_tail <- function(x) {
    # Example:
    #   10.1000/182">paper</a>
    # becomes:
    #   10.1000/182
    x <- sub("([\"']>.*)$", "", x, perl = TRUE)

    # Example:
    #   10.1000/182</a>
    # becomes:
    #   10.1000/182
    x <- sub("(</[[:alnum:]][^[:space:]]*)$", "", x, perl = TRUE)

    x
}

#' Count regex matches in each string
#'
#' @param x A character vector.
#' @param pat A single regular expression.
#'
#' @return An integer vector the same length as `x`.
#'
#' @noRd
.count_matches <- function(x, pat) {
    if (!length(x)) {
        return(integer())
    }
    m <- gregexpr(pat, x, perl = TRUE)
    vapply(m, function(one) {
        if (identical(one[1], -1L)) {
            0L
        } else {
            length(one)
        }
    }, integer(1), USE.NAMES = FALSE)
}


#' Strip one unmatched trailing closer if present
#'
#' @param x A character vector.
#' @param closer Closing delimiter regex, e.g. "\\)".
#' @param opener Opening delimiter regex, e.g. "\\(".
#'
#' @return A character vector the same length as `x`.
#'
#' @noRd
.strip_unmatched_trailing_closer <- function(
        x,
        closer,
        opener
) {
    ends <- grepl(paste0(closer, "$"), x, perl = TRUE)
    if (!any(ends)) {
        return(x)
    }

    n_close <- .count_matches(x[ends], closer)
    n_open <- .count_matches(x[ends], opener)
    drop <- n_close > n_open
    if (!any(drop)) {
        return(x)
    }

    idx <- which(ends)[drop]
    x[idx] <- sub(paste0(closer, "$"), "", x[idx], perl = TRUE)
    x
}


#' Truncate an extracted DOI candidate to its longest valid DOI prefix
#'
#' @param x A single extracted DOI candidate.
#'
#' @return A cleaned DOI candidate string, or "" if no valid DOI prefix exists.
#'
#' @noRd
.truncate_to_valid_doi_prefix <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return("")
    }

    while (nzchar(x) && !is_doi(x)) {
        x <- substr(x, 1L, nchar(x) - 1L)
    }

    x
}
