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
    pat <- "(?<![[:alnum:]_])(10\\.[0-9]{4,9}/\\S+)"
    out <- .extract_with_pattern(
        text = text,
        pat  = pat
    )

    .extract_filter_validate(
        out          = out,
        clean_fn     = .clean_extracted_doi,
        validate_fn  = is_doi
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
    pat <- "(\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9Xx])"
    out <- .extract_with_pattern(
        text = text,
        pat  = pat
    )

    .extract_filter_validate(
        out          = out,
        clean_fn     = .clean_extracted_trailing_punct,
        validate_fn  = is_orcid
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
    pat <- "(?<![[:alnum:]_])([0-9Xx][0-9Xx\\- ]{8,16}[0-9Xx])(?![[:alnum:]_\\-/])"
    out <- .extract_with_pattern(
        text = text,
        pat  = pat
    )

    .extract_filter_validate(
        out          = out,
        clean_fn     = .clean_extracted_trailing_punct,
        validate_fn  = is_isbn
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
    pat <- "(?<![[:alnum:]_\\-])(\\d{4}-\\d{3}[0-9Xx])(?![[:alnum:]_\\-])"
    out <- .extract_with_pattern(
        text = text,
        pat  = pat
    )

    .extract_filter_validate(
        out          = out,
        clean_fn     = .clean_extracted_trailing_punct,
        validate_fn  = is_issn
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
    pat <- paste0(
        "(?<![[:alnum:]_\\./-])",
        "(",
        "\\d{4}\\.\\d{4,5}(v\\d+)?",
        "|",
        "[a-z\\-]+/\\d{7}(v\\d+)?",
        ")",
        "(?![[:alnum:]_\\-/])"
    )

    out <- .extract_with_pattern(
        text = text,
        pat  = pat
    )

    .extract_filter_validate(
        out          = out,
        clean_fn     = .clean_extracted_trailing_punct,
        validate_fn  = is_arxiv
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
    pat <- paste0(
        "(?<![[:alnum:]_./-]|PMC)",
        "\\d{4,9}",
        "(?![[:alnum:]_]|[-/.][[:alnum:]_])"
    )

    out <- .extract_with_pattern(
        text = text,
        pat  = pat
    )

    .extract_filter_validate(
        out          = out,
        clean_fn     = .clean_extracted_trailing_punct,
        validate_fn  = is_pmid
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
    pat <- "(?<![[:alnum:]_./-])PMC\\d+(?![[:alnum:]_]|[-/.][[:alnum:]_])"
    out <- .extract_with_pattern(
        text = text,
        pat  = pat
    )

    .extract_filter_validate(
        out          = out,
        clean_fn     = .clean_extracted_trailing_punct,
        validate_fn  = is_pmcid
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
