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
