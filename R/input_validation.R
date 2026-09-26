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


#' Whether any value contains a non-ASCII character
#'
#' @description
#' Skips invisible-character scans when every value is ASCII. The pattern
#' itself is ASCII, so PCRE does not enter UTF-8 mode.
#'
#' @param x A character vector.
#'
#' @return A single logical value.
#'
#' @noRd
.scholid_any_non_ascii <- function(x) {
    any(
        grepl(
            "[^\\x01-\\x7F]",
            x,
            perl = TRUE
        ),
        na.rm = TRUE
    )
}


#' Whether values contain an invisible character
#'
#' @description
#' Missing values are reported as not containing one. ASCII-only values
#' cannot contain these characters and are handled with one scan.
#'
#' @param x A character vector.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.scholid_has_invisible <- function(x) {
    hit <- rep(FALSE, length(x))
    if (!length(x)) {
        return(hit)
    }

    known <- !is.na(x)
    if (!any(known) || !.scholid_any_non_ascii(x[known])) {
        return(hit)
    }

    y <- x[known]
    found <- rep(FALSE, length(y))
    for (ch in .scholid_invisible_chars()) {
        found <- found | grepl(
            ch,
            y,
            fixed = TRUE
        )
    }
    found[is.na(found)] <- FALSE
    hit[known] <- found
    hit
}


#' Remove invisible characters
#'
#' @description
#' Drops the characters from `.scholid_invisible_chars()` and leaves
#' missing values unchanged. ASCII-only input is returned unchanged.
#'
#' @param x A character vector.
#'
#' @return A character vector the same length as `x`.
#'
#' @noRd
.scholid_strip_invisible <- function(x) {
    if (!length(x)) {
        return(x)
    }

    known <- !is.na(x)
    if (!any(known) || !.scholid_any_non_ascii(x[known])) {
        return(x)
    }

    y <- x[known]
    for (ch in .scholid_invisible_chars()) {
        y <- gsub(
            ch,
            "",
            y,
            fixed = TRUE
        )
    }
    x[known] <- y
    x
}


#' Initialize a logical output vector with NA for non-missing inputs
#'
#' @description
#' Internal helper for vectorized validators. Coerces input to character and
#' prepares a logical output vector with `NA` for missing values. Values that
#' contain an invisible character are not ok, and their output is `FALSE`.
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
        bad <- .scholid_has_invisible(x[ok])
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
#' values. Invisible characters are removed from non-missing values first.
#'
#' @param x A vector of values to normalize.
#'
#' @return A list with elements `x`, `out`, and `ok`.
#'
#' @noRd
.scholid_init_na_character <- function(x) {
    x <- as.character(x)
    ok <- !is.na(x)
    if (any(ok)) {
        x[ok] <- .scholid_strip_invisible(x[ok])
    }
    list(
        x   = x,
        out = rep(NA_character_, length(x)),
        ok  = ok
    )
}
