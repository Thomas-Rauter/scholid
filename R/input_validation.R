# Level 1 function (functions called by exported functions) definitions --------


#' Validate vector-like inputs
#'
#' @description
#' Internal helper for validating inputs expected to be vector-like. The
#' function checks that the argument is present, not `NULL`, and is either
#' an atomic vector or a list. Errors are thrown for invalid inputs.
#'
#' @param x An input expected to be an atomic vector or list.
#' @param arg Name of the argument, used in error messages.
#'
#' @return Invisibly returns `TRUE` if validation succeeds.
#'
#' @noRd
.scholid_check_x <- function(
        x,
        arg
) {
    if (missing(x)) {
        stop("`", arg, "` is required.", call. = FALSE)
    }

    if (is.null(x)) {
        stop("`", arg, "` must not be NULL.", call. = FALSE)
    }

    if (is.data.frame(x)) {
        stop("`", arg, "` must not be a data frame.", call. = FALSE)
    }

    if (!is.atomic(x) && !is.list(x)) {
        cls <- paste(class(x), collapse = "/")
        stop("`", arg, "` must be an atomic vector or list, not ", cls, ".",
             call. = FALSE)
    }

    invisible(TRUE)
}


#' Match and validate scholarly identifier type
#'
#' @description
#' Internal helper for validating identifier type arguments. The function
#' enforces that the input is a non-empty scalar character string and matches
#' one of the supported scholid identifier types exactly. Partial matching
#' and abbreviations are not allowed.
#'
#' @param type A scalar character string specifying an identifier type.
#'
#' @return A length-one character vector giving the validated identifier
#'   type.
#'
#' @noRd
.scholid_match_type <- function(type) {
    type_chr <- .scholid_as_scalar_character(
        x   = type,
        arg = "type"
        )
    if (is.na(type_chr)) {
        stop("`type` must be a non-empty string.", call. = FALSE)
    }

    choices <- scholid_types()
    out <- match.arg(
        type_chr,
        choices = choices,
        several.ok = FALSE
        )

    if (!identical(type_chr, out)) {
        stop(
            "`type` must match exactly; abbreviations are not allowed.",
             call. = FALSE
            )
    }

    out
}


#' Match and validate a vector of scholarly identifier types
#'
#' @description
#' Internal helper for validating `types` arguments. As for a single
#' `type`, factors are converted to character, whitespace is trimmed, and
#' each value must match a supported type exactly. Duplicates are dropped.
#'
#' @param types A character vector of identifier types.
#'
#' @return A character vector of distinct validated types, in the order of
#'   `scholid_types()`.
#'
#' @noRd
.scholid_match_types <- function(types) {
    if (is.null(types)) {
        stop("`types` must not be NULL.", call. = FALSE)
    }

    if (is.factor(types)) {
        types <- as.character(types)
    }

    if (!is.character(types)) {
        stop("`types` must be a character vector.", call. = FALSE)
    }

    if (!length(types)) {
        stop("`types` must contain at least one type.", call. = FALSE)
    }

    types <- trimws(types)
    if (anyNA(types) || !all(nzchar(types))) {
        stop(
            "`types` must not contain NA or empty strings.",
            call. = FALSE
        )
    }

    choices <- scholid_types()
    unknown <- unique(types[!types %in% choices])
    if (length(unknown)) {
        stop(
            "`types` contains unsupported types: ",
            paste0("\"", unknown, "\"", collapse = ", "),
            ". See `scholid_types()`.",
            call. = FALSE
        )
    }

    choices[choices %in% types]
}


#' Resolve a per-type scholid implementation function
#'
#' @description
#' Internal helper that looks up a type-specific implementation function by
#' name prefix and validated type.
#'
#' @param type A validated identifier type string.
#' @param prefix Function name prefix, e.g. `"is_"` or `"normalize_"`.
#' @param required If `TRUE`, stop when the implementation is missing.
#'
#' @return The resolved function, or `NULL` if missing and `required = FALSE`.
#'
#' @noRd
.scholid_resolve_impl <- function(
        type,
        prefix,
        required = TRUE
) {
    fun_name <- paste0(
        prefix,
        type
    )
    fun <- get0(
        fun_name,
        mode = "function",
        inherits = TRUE
    )

    if (is.null(fun)) {
        if (required) {
            # nocov start
            stop("Missing implementation: ", fun_name, "().", call. = FALSE)
            # nocov end
        }
        return(NULL)
    }

    fun
}


#' Dispatch to a per-type scholid implementation function
#'
#' @description
#' Internal helper that resolves and calls a type-specific implementation
#' function by name prefix and validated type.
#'
#' @param type A validated identifier type string.
#' @param prefix Function name prefix, e.g. `"is_"` or `"normalize_"`.
#' @param x Input passed to the resolved implementation function.
#'
#' @return The result of the resolved implementation function.
#'
#' @noRd
.scholid_dispatch <- function(
        type,
        prefix,
        x
) {
    fun <- .scholid_resolve_impl(
        type     = type,
        prefix   = prefix,
        required = TRUE
    )
    fun(x)
}


# Level 2 function (functions called by lvl 1 functions) definitions -----------


#' Coerce input to a single trimmed character value
#'
#' @description
#' Internal helper for validating scalar character arguments. Factors are
#' converted to character, whitespace is trimmed, and empty strings are
#' converted to `NA_character_`. Errors are thrown for missing, `NULL`,
#' non-scalar, or non-character inputs.
#'
#' @param x An input value expected to be a scalar character.
#' @param arg Name of the argument, used in error messages.
#'
#' @return A length-one character vector, or `NA_character_` if the input
#'   is an empty string.
#'
#' @noRd
.scholid_as_scalar_character <- function(
        x,
        arg
) {
    if (missing(x)) {
        stop("`", arg, "` is required.", call. = FALSE)
    }

    if (is.null(x)) {
        stop("`", arg, "` must not be NULL.", call. = FALSE)
    }

    if (length(x) != 1L) {
        stop("`", arg, "` must be length 1.", call. = FALSE)
    }

    if (is.factor(x)) {
        x <- as.character(x)
    }

    if (!is.character(x)) {
        stop("`", arg, "` must be a character string.", call. = FALSE)
    }

    x <- trimws(x)
    if (!nzchar(x)) {
        return(NA_character_)
    }

    x
}


#' Characters that are never part of a canonical identifier
#'
#' @description
#' U+00AD, U+200B-U+200F, U+202A-U+202E, U+2060-U+2064,
#' U+2066-U+2069, and U+FEFF. Validators reject values that contain
#' one of them. Normalization and extraction remove them before matching.
#'
#' @return A character vector of those characters.
#'
#' @noRd
.scholid_invisible_chars <- function() {
    c(
        "\u00AD",
        "\u200B",
        "\u200C",
        "\u200D",
        "\u200E",
        "\u200F",
        "\u202A",
        "\u202B",
        "\u202C",
        "\u202D",
        "\u202E",
        "\u2060",
        "\u2061",
        "\u2062",
        "\u2063",
        "\u2064",
        "\u2066",
        "\u2067",
        "\u2068",
        "\u2069",
        "\uFEFF"
    )
}


#' Unicode look-alikes of ASCII spaces, hyphens and digits
#'
#' @description
#' `space`: U+00A0, U+1680, U+2000-U+200A, U+202F, U+205F, and U+3000.
#' They count as whitespace everywhere. `dash`: U+2010-U+2013, U+2212,
#' U+FE63, and U+FF0D, but not the em dash U+2014. `digit`: the full-width
#' digits U+FF10-U+FF19, in the order 0 to 9. Dashes and digits are folded
#' to ASCII only for the types that ask for it.
#'
#' @return A list of character vectors `space`, `dash`, and `digit`.
#'
#' @noRd
.scholid_lookalike_chars <- function() {
    list(
        space = c(
            "\u00A0",
            "\u1680",
            "\u2000",
            "\u2001",
            "\u2002",
            "\u2003",
            "\u2004",
            "\u2005",
            "\u2006",
            "\u2007",
            "\u2008",
            "\u2009",
            "\u200A",
            "\u202F",
            "\u205F",
            "\u3000"
        ),
        dash = c(
            "\u2010",
            "\u2011",
            "\u2012",
            "\u2013",
            "\u2212",
            "\uFE63",
            "\uFF0D"
        ),
        digit = c(
            "\uFF10",
            "\uFF11",
            "\uFF12",
            "\uFF13",
            "\uFF14",
            "\uFF15",
            "\uFF16",
            "\uFF17",
            "\uFF18",
            "\uFF19"
        )
    )
}


#' Values that can hold a non-ASCII character
#'
#' @description
#' Selects non-missing values that contain a non-ASCII byte and are valid
#' UTF-8 after `enc2utf8()`. The byte scan uses `useBytes = TRUE`, so
#' ASCII-only input is handled with one pass. Values that are not valid
#' UTF-8 are skipped, because `gsub()` fails on them when other values are
#' UTF-8, and `grepl()` warns once per call.
#'
#' @param x A character vector.
#'
#' @return A list with `idx`, the selected positions in `x`, and `y`, the
#'   UTF-8 values at those positions.
#'
#' @noRd
.scholid_unicode_candidates <- function(x) {
    idx <- which(!is.na(x))
    if (length(idx)) {
        non_ascii <- grepl(
            "[^\\x01-\\x7F]",
            x[idx],
            perl     = TRUE,
            useBytes = TRUE
        )
        idx <- idx[non_ascii]
    }

    y <- enc2utf8(x[idx])
    valid <- validUTF8(y)

    list(
        idx = idx[valid],
        y   = y[valid]
    )
}


#' Whether values contain one of the given characters
#'
#' @description
#' Missing values, and values that are not valid UTF-8, are reported as not
#' containing one.
#'
#' @param x A character vector.
#' @param chars A character vector of single non-ASCII characters.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.scholid_has_chars <- function(
        x,
        chars
) {
    hit <- rep(FALSE, length(x))
    cand <- .scholid_unicode_candidates(x)
    if (!length(cand$idx)) {
        return(hit)
    }

    found <- rep(FALSE, length(cand$y))
    for (ch in chars) {
        found <- found | grepl(
            ch,
            cand$y,
            fixed = TRUE
        )
    }
    hit[cand$idx] <- found
    hit
}


#' Remove invisible characters and fold look-alikes to ASCII
#'
#' @description
#' Drops the characters from `.scholid_invisible_chars()` and replaces the
#' Unicode spaces from `.scholid_lookalike_chars()` with an ASCII space.
#' With `dashes = TRUE`, Unicode dashes become `-`. With `digits = TRUE`,
#' full-width digits become `0`-`9`. Missing values, ASCII-only values, and
#' values that are not valid UTF-8 are returned unchanged.
#'
#' @param x A character vector.
#' @param dashes Whether to fold Unicode dashes.
#' @param digits Whether to fold full-width digits.
#'
#' @return A character vector the same length as `x`.
#'
#' @noRd
.scholid_clean_chars <- function(
        x,
        dashes = FALSE,
        digits = FALSE
) {
    cand <- .scholid_unicode_candidates(x)
    if (!length(cand$idx)) {
        return(x)
    }

    y <- cand$y
    for (ch in .scholid_invisible_chars()) {
        y <- gsub(
            ch,
            "",
            y,
            fixed = TRUE
        )
    }

    # gsub(fixed = TRUE) rather than chartr(), which fails on UTF-8 text
    # in a non-UTF-8 locale.
    lookalike <- .scholid_lookalike_chars()
    old <- lookalike$space
    new <- rep(" ", length(old))
    if (dashes) {
        old <- c(old, lookalike$dash)
        new <- c(new, rep("-", length(lookalike$dash)))
    }
    if (digits) {
        old <- c(old, lookalike$digit)
        new <- c(new, as.character(0:9))
    }
    for (i in seq_along(old)) {
        y <- gsub(
            old[[i]],
            new[[i]],
            y,
            fixed = TRUE
        )
    }
    x[cand$idx] <- y
    x
}


#' Initialize a logical output vector with NA for non-missing inputs
#'
#' @description
#' Internal helper for vectorized validators. Coerces input to character and
#' prepares a logical output vector with `NA` for missing values. Values that
#' contain an invisible character or a Unicode space are not ok, and their
#' output is `FALSE`.
#'
#' @param x A vector of values to validate.
#'
#' @return A list with elements `x`, `out`, and `ok`.
#'
#' @noRd
.scholid_init_na_logical <- function(x) {
    x <- as.character(x)
    ok <- !is.na(x)
    out <- rep(NA, length(x))
    if (any(ok)) {
        bad <- .scholid_has_chars(
            x[ok],
            c(
                .scholid_invisible_chars(),
                .scholid_lookalike_chars()$space
            )
        )
        if (any(bad)) {
            bad_idx <- which(ok)[bad]
            out[bad_idx] <- FALSE
            ok[bad_idx] <- FALSE
        }
    }
    list(
        x   = x,
        out = out,
        ok  = ok
    )
}


#' Initialize a character output vector with NA for non-missing inputs
#'
#' @description
#' Internal helper for vectorized normalizers. Coerces input to character and
#' prepares a character output vector with `NA_character_` for missing
#' values. Non-missing values are cleaned with `.scholid_clean_chars()`
#' first.
#'
#' @param x A vector of values to normalize.
#' @param dashes Whether to fold Unicode dashes.
#' @param digits Whether to fold full-width digits.
#'
#' @return A list with elements `x`, `out`, and `ok`.
#'
#' @noRd
.scholid_init_na_character <- function(
        x,
        dashes = FALSE,
        digits = FALSE
) {
    x <- as.character(x)
    ok <- !is.na(x)
    if (any(ok)) {
        x[ok] <- .scholid_clean_chars(
            x[ok],
            dashes = dashes,
            digits = digits
        )
    }
    list(
        x   = x,
        out = rep(NA_character_, length(x)),
        ok  = ok
    )
}
