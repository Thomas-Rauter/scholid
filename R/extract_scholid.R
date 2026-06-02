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
#' necessary to isolate the identifier. Use `normalize_scholid()` to convert
#' identifiers to canonical form.
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

    .scholid_dispatch(
        type   = type,
        prefix = "extract_",
        x      = text
    )
}


# Level 1 functions (functions called by exported functions) definitions -------
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
#'
#' @return A list of character vectors of extracted DOIs.
#'
#' @noRd
extract_doi <- function(text) {
    .scholid_extract_validated(
        text        = text,
        type        = "doi",
        clean_fn    = .clean_extracted_doi,
        validate_fn = is_doi
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
#'
#' @return A list of character vectors of extracted ARKs.
#'
#' @noRd
extract_ark <- function(text) {
    .scholid_extract_validated(
        text        = text,
        type        = "ark",
        clean_fn    = .clean_extracted_ark,
        validate_fn = is_ark
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
#'
#' @return A list of character vectors of extracted ISNIs.
#'
#' @noRd
extract_isni <- function(text) {
    .scholid_extract_validated(
        text        = text,
        type        = "isni",
        clean_fn    = .clean_extracted_isni,
        validate_fn = is_isni
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
#'
#' @return A list of character vectors of extracted ORCID iDs.
#'
#' @noRd
extract_orcid <- function(text) {
    .scholid_extract_validated(
        text        = text,
        type        = "orcid",
        clean_fn    = .clean_extracted_trailing_punct,
        validate_fn = is_orcid
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
#'
#' @return A list of character vectors of extracted UniProt accessions.
#'
#' @noRd
extract_uniprot <- function(text) {
    .scholid_extract_validated(
        text        = text,
        type        = "uniprot",
        clean_fn    = .clean_extracted_uniprot,
        validate_fn = is_uniprot
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
#'
#' @return A list of character vectors of extracted RefSeq accessions.
#'
#' @noRd
extract_refseq <- function(text) {
    .scholid_extract_validated(
        text        = text,
        type        = "refseq",
        clean_fn    = .clean_extracted_refseq,
        validate_fn = is_refseq
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
#'
#' @return A list of character vectors of extracted SRA accessions.
#'
#' @noRd
extract_sra <- function(text) {
    .scholid_extract_validated(
        text        = text,
        type        = "sra",
        clean_fn    = .clean_extracted_sra,
        validate_fn = is_sra
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
#'
#' @return A list of character vectors of extracted ROR iDs.
#'
#' @noRd
extract_ror <- function(text) {
    .scholid_extract_validated(
        text        = text,
        type        = "ror",
        clean_fn    = .clean_extracted_ror,
        validate_fn = is_ror
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
#'
#' @return A list of character vectors of extracted RRIDs.
#'
#' @noRd
extract_rrid <- function(text) {
    .scholid_extract_validated(
        text        = text,
        type        = "rrid",
        clean_fn    = .clean_extracted_rrid,
        validate_fn = is_rrid
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
    .scholid_extract_validated(
        text        = text,
        type        = "isbn",
        clean_fn    = .clean_extracted_trailing_punct,
        validate_fn = is_isbn
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
    .scholid_extract_validated(
        text        = text,
        type        = "issn",
        clean_fn    = .clean_extracted_trailing_punct,
        validate_fn = is_issn
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
    .scholid_extract_validated(
        text        = text,
        type        = "arxiv",
        clean_fn    = .clean_extracted_trailing_punct,
        validate_fn = is_arxiv
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
#'
#' @return A list of character vectors of extracted bibcodes.
#'
#' @noRd
extract_bibcode <- function(text) {
    .scholid_extract_validated(
        text        = text,
        type        = "bibcode",
        clean_fn    = .clean_extracted_bibcode,
        validate_fn = is_bibcode
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
#'
#' @return A list of character vectors of extracted OpenAlex IDs.
#'
#' @noRd
extract_openalex <- function(text) {
    .scholid_extract_validated(
        text        = text,
        type        = "openalex",
        clean_fn    = .clean_extracted_openalex,
        validate_fn = is_openalex
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
#'
#' @return A list of character vectors of extracted SWHIDs.
#'
#' @noRd
extract_swhid <- function(text) {
    .scholid_extract_validated(
        text        = text,
        type        = "swhid",
        clean_fn    = .clean_extracted_swhid,
        validate_fn = is_swhid
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
    .scholid_extract_validated(
        text        = text,
        type        = "pmid",
        clean_fn    = .clean_extracted_trailing_punct,
        validate_fn = is_pmid
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
    .scholid_extract_validated(
        text        = text,
        type        = "pmcid",
        clean_fn    = .clean_extracted_trailing_punct,
        validate_fn = is_pmcid
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


#' Clean, filter, and validate extracted identifier candidates
#'
#' @description
#' Internal helper that post-processes regex extraction results. Each list
#' element is cleaned with `clean_fn`, then filtered to non-empty values and
#' validated with `validate_fn`.
#'
#' @param out A list of character vectors of raw regex matches.
#' @param clean_fn Function applied to each raw match.
#' @param validate_fn Vectorized validator returning logical values.
#'
#' @return A list of character vectors of validated identifiers.
#'
#' @noRd
.extract_filter_validate <- function(
        out,
        clean_fn,
        validate_fn
) {
    lapply(out, function(hits) {
        if (!length(hits)) {
            return(character(0))
        }

        cleaned <- vapply(
            hits,
            clean_fn,
            character(1),
            USE.NAMES = FALSE
        )

        cleaned <- cleaned[nzchar(cleaned)]
        cleaned <- cleaned[!is.na(cleaned)]
        cleaned <- cleaned[validate_fn(cleaned)]
        cleaned
    })
}


#' Extract and validate identifiers using registry patterns
#'
#' @description
#' Internal helper that extracts identifier candidates from free text using
#' the registry `extract_pat` for a type, then cleans and validates matches.
#'
#' @param text A character vector of text.
#' @param type A validated identifier type string.
#' @param clean_fn Function applied to each raw match.
#' @param validate_fn Vectorized validator returning logical values.
#'
#' @return A list of character vectors of validated identifiers.
#'
#' @noRd
.scholid_extract_validated <- function(
        text,
        type,
        clean_fn,
        validate_fn
) {
    out <- .extract_with_pattern(
        text = text,
        pat  = .scholid_registry_extract_pat(type)
    )

    .extract_filter_validate(
        out         = out,
        clean_fn    = clean_fn,
        validate_fn = validate_fn
    )
}


#' Clean an extracted ROR candidate
#'
#' @description
#' Removes URL prefixes, trailing punctuation, and surrounding whitespace
#' from an extracted ROR candidate.
#'
#' @param x A single extracted ROR candidate.
#'
#' @return A cleaned ROR candidate string, or `""` if empty.
#'
#' @noRd
.clean_extracted_bibcode <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return("")
    }

    x <- sub("[.,;:!?]+$", "", x, perl = TRUE)
    x <- trimws(x)
    x <- sub(
        "^https?://(?:ui\\.)?adsabs\\.harvard\\.edu/abs/",
        "",
        x,
        ignore.case = TRUE
    )
    x <- sub("(?i)^bibcode\\s*:?\\s*", "", x, perl = TRUE)
    x
}


.clean_extracted_openalex <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return("")
    }

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
    if (is.na(x) || !nzchar(x)) {
        return("")
    }

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
    if (is.na(x) || !nzchar(x)) {
        return("")
    }

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
    if (is.na(x) || !nzchar(x)) {
        return("")
    }

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


.clean_extracted_ark <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return("")
    }

    x <- sub("[.,;:!?]+$", "", x, perl = TRUE)
    val <- .canonicalize_ark(trimws(x))
    if (is.na(val)) {
        ""
    } else {
        val
    }
}


.clean_extracted_isni <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return("")
    }

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
    if (is.na(x) || !nzchar(x)) {
        return("")
    }

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
#' @param x A single extracted RRID candidate.
#'
#' @return A cleaned RRID candidate string, or `""` if empty.
#'
#' @noRd
.clean_extracted_rrid <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return("")
    }

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
    x <- sub("^RRID[[:space:]]*:[[:space:]]*", "RRID:", x, ignore.case = TRUE)
    x
}


#' Clean an extracted SWHID candidate
#'
#' @description
#' Removes resolver URL prefixes, trailing prose punctuation, and surrounding
#' whitespace from an extracted SWHID candidate, and canonicalizes the core
#' identifier.
#'
#' @param x A single extracted SWHID candidate.
#'
#' @return A cleaned SWHID candidate string, or `""` if empty.
#'
#' @noRd
.clean_extracted_swhid <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return("")
    }

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
    x <- .canonicalize_swhid(x)
    x
}


#' Clean an extracted DOI candidate
#'
#' @description
#' Removes obvious surrounding markup tails and trailing prose delimiters from
#' a DOI candidate extracted from free text, while preserving valid DOI-internal
#' punctuation where possible.
#'
#' @param x A single extracted DOI candidate.
#'
#' @return A cleaned DOI candidate string, or "" if empty.
#'
#' @noRd
.clean_extracted_doi <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return("")
    }

    x <- .strip_doi_markup_tail(x)

    repeat {
        old <- x

        # Strip terminal prose punctuation and quotes
        x <- sub("[.,;:!?\"']+$", "", x, perl = TRUE)

        # Strip unmatched closing delimiters at the end
        x <- .strip_unmatched_trailing_closer(x, "\\)", "\\(")
        x <- .strip_unmatched_trailing_closer(x, "\\]", "\\[")
        x <- .strip_unmatched_trailing_closer(x, "\\}", "\\{")
        x <- .strip_unmatched_trailing_closer(x, ">", "<")

        if (identical(x, old)) {
            break
        }
    }

    # Final safeguard: trim back to the longest valid DOI prefix
    x <- .truncate_to_valid_doi_prefix(x)

    x
}


#' Clean trailing punctuation and whitespace from an extracted candidate
#'
#' @description
#' Removes trailing punctuation and surrounding whitespace from an extracted
#' identifier candidate.
#'
#' @param x A single extracted identifier candidate.
#'
#' @return A cleaned candidate string, or `""` if empty.
#'
#' @noRd
.clean_extracted_trailing_punct <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return("")
    }

    x <- sub("[[:space:][:punct:]]+$", "", x, perl = TRUE)
    x <- trimws(x)
    x
}


# Level 3 functions (functions called by level 2 functions) definitions --------


#' Strip obvious markup tails from an extracted DOI candidate
#'
#' @description
#' Removes trailing HTML or attribute fragments that may be captured when a DOI
#' appears inside markup such as an anchor tag or quoted URL attribute.
#'
#' @param x A single extracted DOI candidate.
#'
#' @return A character string.
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

#' Count regex matches in a single string
#'
#' @param x A single character string.
#' @param pat A single regular expression.
#'
#' @return An integer count.
#'
#' @noRd
.count_matches <- function(x, pat) {
    m <- gregexpr(pat, x, perl = TRUE)[[1]]
    if (identical(m[1], -1L)) {
        0L
    } else {
        length(m)
    }
}


#' Strip one unmatched trailing closer if present
#'
#' @param x A single character string.
#' @param closer Closing delimiter regex, e.g. "\\)".
#' @param opener Opening delimiter regex, e.g. "\\(".
#'
#' @return A character string.
#'
#' @noRd
.strip_unmatched_trailing_closer <- function(
        x,
        closer,
        opener
) {
    if (grepl(paste0(closer, "$"), x, perl = TRUE) &&
        .count_matches(x, closer) > .count_matches(x, opener)) {
        x <- sub(paste0(closer, "$"), "", x, perl = TRUE)
    }
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
