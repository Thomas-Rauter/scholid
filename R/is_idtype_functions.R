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
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- vapply(init$x[init$ok], .is_doi_strict, logical(1))
    init$out
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
    init <- .scholid_init_na_logical(x)

    pat <- "^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9Xx]$"
    y <- toupper(init$x[init$ok])

    valid <- grepl(pat, y)
    res <- rep(FALSE, length(y))

    res[valid] <- vapply(
        y[valid],
        function(id) {
            .iso7064_mod11_2_valid(gsub("-", "", id))
        },
        logical(1)
    )
    init$out[init$ok] <- res
    init$out
}


#' Check ISNI identifiers
#'
#' Tests whether values are valid International Standard Name Identifiers in
#' canonical compact 16-character form, including ISO/IEC 7064 MOD 11-2
#' checksum. Hyphenated ORCID-style strings are rejected.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_isni <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- vapply(
        init$x[init$ok],
        .is_isni_strict,
        logical(1)
    )
    init$out
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
    init <- .scholid_init_na_logical(x)

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

    init$out[init$ok] <- vapply(init$x[init$ok], function(s) {
        if (!.isbn_format_ok(s)) {
            return(FALSE)
        }

        compact <- toupper(gsub("[- ]", "", s))
        is10(compact) || is13(compact)
    }, logical(1))

    init$out
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
    init <- .scholid_init_na_logical(x)

    pat <- "^\\d{4}-\\d{3}[0-9X]$"
    y <- init$x[init$ok]

    chk <- function(s) {
        d <- strsplit(gsub("-", "", s), "")[[1]]
        v <- as.integer(d[1:7])
        r <- sum(v * 8:2) %% 11
        cd <- if (r == 0) "0" else if (r == 1) "X" else as.character(11 - r)
        cd == d[8]
    }

    res <- grepl(pat, y)
    res[res] <- vapply(y[res], chk, logical(1))
    init$out[init$ok] <- res
    init$out
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
    init <- .scholid_init_na_logical(x)
    reg <- .scholid_registry()[["arxiv"]]
    pat1 <- reg$pat1
    pat2 <- reg$pat2
    init$out[init$ok] <- grepl(pat1, init$x[init$ok], perl = TRUE) |
        grepl(pat2, init$x[init$ok], perl = TRUE)
    init$out
}


#' Check ARK identifiers
#'
#' Tests whether values are valid Archival Resource Keys in canonical `ark:/`
#' form. Validation is structural only; resolver existence is not checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_ark <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- vapply(
        init$x[init$ok],
        .is_ark_strict,
        logical(1)
    )
    init$out
}


#' Check ADS bibcodes
#'
#' Tests whether values are valid SAO/NASA ADS bibliographic codes in canonical
#' 19-character form. Validation is structural only; ADS existence is not
#' checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_bibcode <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- vapply(
        init$x[init$ok],
        .is_bibcode_strict,
        logical(1)
    )
    init$out
}


#' Check OpenAlex identifiers
#'
#' Tests whether values are valid OpenAlex IDs in canonical uppercase key
#' form. Validation is structural only; registry existence is not checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_openalex <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- vapply(
        init$x[init$ok],
        .is_openalex_strict,
        logical(1)
    )
    init$out
}


#' Check SWHID identifiers
#'
#' Tests whether values are valid Software Heritage identifiers in canonical
#' `swh:` form. Validation is structural only; content-hash correctness is
#' not checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_swhid <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- vapply(
        init$x[init$ok],
        .is_swhid_strict,
        logical(1)
    )
    init$out
}


#' Check ROR identifiers
#'
#' Tests whether values are valid ROR iDs, including checksum.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_ror <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- vapply(
        init$x[init$ok],
        .is_ror_strict,
        logical(1)
    )
    init$out
}


#' Check RRID identifiers
#'
#' Tests whether values are valid Research Resource Identifiers in canonical
#' `RRID:` form. Validation is structural and limited to known RRID authority
#' prefixes; registry existence is not checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_rrid <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- vapply(
        init$x[init$ok],
        .is_rrid_strict,
        logical(1)
    )
    init$out
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
    init <- .scholid_init_na_logical(x)
    y <- init$x[init$ok]

    pat <- .scholid_registry()[["pmid"]]$pat
    res <- grepl(pat, y, perl = TRUE)

    res[res] <- !is_isbn(y[res])

    init$out[init$ok] <- res
    init$out
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
    init <- .scholid_init_na_logical(x)
    pat <- .scholid_registry()[["pmcid"]]$pat
    init$out[init$ok] <- grepl(pat, init$x[init$ok], perl = TRUE)
    init$out
}


# Level 2 functions (functions called by level 1 functions) definitions --------


#' Validate a compact 16-character ISO/IEC 7064 MOD 11-2 identifier
#'
#' @description
#' Internal helper shared by ORCID and ISNI validators. The input must be a
#' 16-character string of digits with an optional `X` check character.
#'
#' @param compact A single compact 16-character identifier string.
#'
#' @return A single logical value.
#'
#' @noRd
.iso7064_mod11_2_valid <- function(compact) {
    if (is.na(compact) || nchar(compact) != 16L) {
        return(FALSE)
    }

    d <- strsplit(compact, "")[[1]]
    if (!all(grepl("[0-9]", d[1:15]))) {
        return(FALSE)
    }

    if (!grepl("^[0-9X]$", d[16])) {
        return(FALSE)
    }

    s <- 0L
    for (i in seq_len(15)) {
        s <- (s + as.integer(d[i])) * 2L
    }
    r <- (12L - (s %% 11L)) %% 11L
    cd <- if (r == 10L) {
        "X"
    } else {
        as.character(r)
    }

    identical(cd, d[16])
}


#' Return the ISNI validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.isni_pat <- function() {
    .scholid_registry()[["isni"]]$pat
}


#' Strict ISNI validator
#'
#' @description
#' Validates canonical compact ISNIs (`000000012146438X`). Hyphenated
#' ORCID-style strings and wrapped forms are rejected; use
#' `normalize_isni()` first.
#'
#' @param x A single character string in canonical form.
#'
#' @return A single logical value.
#'
#' @noRd
.is_isni_strict <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return(FALSE)
    }

    x <- trimws(x)
    if (grepl("[[:space:]-]", x, perl = TRUE)) {
        return(FALSE)
    }

    x <- toupper(x)
    pat <- .isni_pat()
    if (!grepl(pat, x, perl = TRUE)) {
        return(FALSE)
    }

    .iso7064_mod11_2_valid(x)
}


#' Strip optional ISBN labels from identifier strings
#'
#' @description
#' Removes optional `ISBN`, `ISBN-10`, or `ISBN-13` labels from the
#' beginning of identifier strings.
#'
#' @param x A character vector of candidate ISBN strings.
#'
#' @return A character vector with labels removed.
#'
#' @noRd
.strip_isbn_label <- function(x) {
    sub(
        "^(?i:isbn(?:-1[03])?)\\s*:?\\s*",
        "",
        x,
        perl = TRUE
    )
}


#' Return the OpenAlex key validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.openalex_key_pat <- function() {
    .scholid_registry()[["openalex"]]$pat
}


#' Strict OpenAlex validator
#'
#' @description
#' Validates canonical uppercase OpenAlex keys (`W2741809807`). Wrapped URLs
#' and lowercase keys are rejected; use `normalize_openalex()` first.
#'
#' @param x A single character string in canonical form.
#'
#' @return A single logical value.
#'
#' @noRd
.is_openalex_strict <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return(FALSE)
    }

    x <- trimws(x)
    if (grepl("[[:space:]]", x, perl = TRUE)) {
        return(FALSE)
    }

    pat <- .openalex_key_pat()
    if (!grepl(pat, x, perl = TRUE)) {
        return(FALSE)
    }

    identical(x, toupper(x))
}


#' Return the ARK validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.ark_pat <- function() {
    .scholid_registry()[["ark"]]$pat
}


#' Canonicalize an ARK string to ark:/NAAN/Name form
#'
#' @param x A single ARK candidate string.
#'
#' @return A canonical ARK string, or `NA_character_` if no ARK label is present.
#'
#' @noRd
.canonicalize_ark <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return(NA_character_)
    }

    x <- trimws(x)
    pos <- regexpr("(?i)ark:", x, perl = TRUE)[1]
    if (pos < 1L) {
        return(NA_character_)
    }

    x <- substr(x, pos, nchar(x))
    x <- sub("(?i)^ark:/*", "ark:/", x, perl = TRUE)
    x <- sub("[.,;:!?]+$", "", x)
    x <- sub("[?#].*$", "", x)
    x
}


#' Return the bibcode validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.bibcode_pat <- function() {
    .scholid_registry()[["bibcode"]]$pat
}


#' Strict ARK validator
#'
#' @description
#' Validates canonical `ark:/NAAN/Name` identifiers. Wrapped URLs and bare
#' paths without the `ark:` label are rejected; use `normalize_ark()` first.
#'
#' @param x A single character string in canonical form.
#'
#' @return A single logical value.
#'
#' @noRd
.is_ark_strict <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return(FALSE)
    }

    x <- trimws(x)
    if (grepl("^https?://", x, ignore.case = TRUE)) {
        return(FALSE)
    }

    if (!grepl("(?i)^ark:", x, perl = TRUE)) {
        return(FALSE)
    }

    x <- .canonicalize_ark(x)
    if (is.na(x) || grepl("[[:space:]]", x, perl = TRUE)) {
        return(FALSE)
    }

    pat <- .ark_pat()
    grepl(pat, x, perl = TRUE)
}


#' Strict bibcode validator
#'
#' @description
#' Validates canonical 19-character ADS bibcodes (`YYYYJJJJJVVVVM PPPPA`).
#' Wrapped URLs are rejected; use `normalize_bibcode()` first.
#'
#' @param x A single character string in canonical form.
#'
#' @return A single logical value.
#'
#' @noRd
.is_bibcode_strict <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return(FALSE)
    }

    x <- trimws(x)
    if (nchar(x) != 19L) {
        return(FALSE)
    }

    if (grepl("[[:space:]]", x, perl = TRUE)) {
        return(FALSE)
    }

    pat <- .bibcode_pat()
    if (!grepl(pat, x, perl = TRUE)) {
        return(FALSE)
    }

    journal <- substr(x, 5L, 9L)
    grepl("[A-Za-z]", journal, perl = TRUE)
}


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


#' Decode a Crockford base32 string to an integer
#'
#' @description
#' Internal helper for ROR checksum validation. Accepts lowercase Crockford
#' base32 strings and maps `i`/`l` to `1` and `o` to `0`, following ROR's
#' identifier generation rules.
#'
#' @param x A single Crockford base32 string.
#'
#' @return An integer value, or `NA_integer_` if decoding fails.
#'
#' @noRd
.crockford_base32_decode <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return(NA_integer_)
    }

    chars <- strsplit(gsub("-", "", tolower(x), fixed = TRUE), "")[[1]]
    alphabet <- strsplit("0123456789abcdefghjkmnpqrstvwxyz", "")[[1]]
    n <- 0L

    for (ch in chars) {
        if (ch %in% c("i", "l")) {
            ch <- "1"
        } else if (ch == "o") {
            ch <- "0"
        }

        idx <- match(ch, alphabet)
        if (is.na(idx)) {
            return(NA_integer_)
        }

        n <- n * 32L + (idx - 1L)
    }

    n
}


#' Strict ROR validator
#'
#' @param x A single character string in canonical compact form.
#'
#' @return A single logical value.
#'
#' @noRd
.is_ror_strict <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return(FALSE)
    }

    x <- tolower(trimws(x))
    pat <- .scholid_registry()[["ror"]]$pat
    if (!grepl(pat, x, perl = TRUE)) {
        return(FALSE)
    }

    body_num <- .crockford_base32_decode(substr(x, 2L, 7L))
    if (is.na(body_num)) {
        return(FALSE)
    }

    expected <- sprintf(
        "%02d",
        98L - (as.numeric(body_num) * 100) %% 97
    )
    identical(substr(x, 8L, 9L), expected)
}


#' Return RRID body patterns from the registry
#'
#' @return A character vector of regular expression fragments for RRID bodies.
#'
#' @noRd
.rrid_body_patterns <- function() {
    .scholid_registry()[["rrid"]]$body_patterns
}


#' Strict RRID validator
#'
#' @description
#' Validates canonical `RRID:` identifiers against a conservative allowlist
#' of known authority body patterns. Bare local IDs without the `RRID:` prefix
#' are rejected.
#'
#' @param x A single character string in canonical form.
#'
#' @return A single logical value.
#'
#' @noRd
.is_rrid_strict <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return(FALSE)
    }

    x <- trimws(x)
    if (!grepl("^RRID:", x)) {
        return(FALSE)
    }

    body <- substr(x, 6L, nchar(x))
    if (!nzchar(body)) {
        return(FALSE)
    }

    patterns <- .rrid_body_patterns()
    any(vapply(
        patterns,
        function(p) grepl(paste0("^", p, "$"), body, perl = TRUE),
        logical(1)
    ))
}


#' Return the SWHID core validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.swhid_core_pat <- function() {
    .scholid_registry()[["swhid"]]$core_pat
}


#' Split a SWHID into core and qualifier segments
#'
#' @param x A single compact SWHID string without surrounding whitespace.
#'
#' @return A list with `core` and `qualifiers` character strings.
#'
#' @noRd
.swhid_split <- function(x) {
    pos <- regexpr(";", x, fixed = TRUE)[1]

    if (pos == -1L) {
        return(list(
            core        = x,
            qualifiers  = ""
        ))
    }

    list(
        core        = substr(x, 1L, pos - 1L),
        qualifiers  = substr(x, pos + 1L, nchar(x))
    )
}


#' Validate SWHID qualifier segments
#'
#' @param qualifiers A semicolon-separated qualifier string without a leading
#'   semicolon.
#'
#' @return A single logical value.
#'
#' @noRd
.is_swhid_qualifiers_valid <- function(qualifiers) {
    if (!nzchar(qualifiers)) {
        return(TRUE)
    }

    parts <- strsplit(qualifiers, ";", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]

    if (!length(parts)) {
        return(TRUE)
    }

    keys <- character(0)
    core_pat <- .swhid_core_pat()

    for (part in parts) {
        if (!grepl("^(origin|visit|anchor|path|lines)=", part, perl = TRUE)) {
            return(FALSE)
        }

        key <- sub("=.*$", "", part)
        if (key %in% keys) {
            return(FALSE)
        }
        keys <- c(keys, key)

        val <- sub("^[^=]+=", "", part)
        if (!nzchar(val)) {
            return(FALSE)
        }

        if (key %in% c("visit", "anchor")) {
            if (!grepl(core_pat, val, perl = TRUE)) {
                return(FALSE)
            }
        } else if (key == "path") {
            if (!grepl("^/", val, perl = TRUE)) {
                return(FALSE)
            }
        } else if (key == "lines") {
            if (!grepl("^[0-9]+(-[0-9]+)?$", val, perl = TRUE)) {
                return(FALSE)
            }
        } else if (key == "origin") {
            if (!grepl("^[a-zA-Z][a-zA-Z0-9+.-]*:.+", val, perl = TRUE)) {
                return(FALSE)
            }
        }
    }

    TRUE
}


#' Canonicalize a compact SWHID string
#'
#' @description
#' Lowercases the core identifier and embedded visit/anchor qualifier cores.
#' The input must already be whitespace-free.
#'
#' @param x A single compact SWHID string.
#'
#' @return A canonical SWHID string.
#'
#' @noRd
.canonicalize_swhid <- function(x) {
    parts <- .swhid_split(x)
    core <- tolower(parts$core)

    if (!nzchar(parts$qualifiers)) {
        return(core)
    }

    qual_parts <- strsplit(parts$qualifiers, ";", fixed = TRUE)[[1]]
    qual_parts <- vapply(qual_parts, function(part) {
        if (grepl("^(visit|anchor)=", part, perl = TRUE)) {
            prefix <- sub("=.*$", "=", part)
            paste0(prefix, tolower(sub("^[^=]+=", "", part)))
        } else {
            part
        }
    }, character(1))

    paste0(core, ";", paste(qual_parts, collapse = ";"))
}


#' Strict SWHID validator
#'
#' @description
#' Validates canonical `swh:` identifiers. The core must use lowercase hex,
#' scheme version `1`, and a known object type. Optional qualifiers must use
#' known keys and pass conservative value checks. Bare 40-character hex strings
#' without the `swh:` prefix are rejected.
#'
#' @param x A single character string in canonical form.
#'
#' @return A single logical value.
#'
#' @noRd
.is_swhid_strict <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return(FALSE)
    }

    x <- gsub("[[:space:]]+", "", trimws(x))

    if (!grepl("^swh:", x)) {
        return(FALSE)
    }

    parts <- .swhid_split(x)

    if (!grepl(.swhid_core_pat(), parts$core, perl = TRUE)) {
        return(FALSE)
    }

    .is_swhid_qualifiers_valid(parts$qualifiers)
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
