#' Check scholarly identifiers
#'
#' Vectorized predicate for checking whether values match a supported
#' scholarly identifier type (e.g., DOI, ORCID). Dispatches to the
#' corresponding `is_*()` implementation (such as `is_doi()`).
#'
#' @param x A vector of values to check.
#' @param type A single string giving the identifier type. See
#'   `scholid_types()` for supported values.
#'
#' @return A logical vector with the same length as `x`. `NA` inputs yield
#'   `NA` outputs.
#'
#' @examples
#' is_scholid("10.1000/182", "doi")
#' is_scholid("0000-0002-1825-0097", "orcid")
#' is_scholid(c("10.1000/182", NA), "doi")
#'
#' @export
is_scholid <- function(
        x,
        type
        ) {
    .scholid_check_x(
        x,
        arg = "x"
        )
    type <- .scholid_match_type(type)

    fun_name <- paste0(
        "is_",
        type
        )
    fun <- get0(
        fun_name,
        mode = "function",
        inherits = TRUE
        )

    # nocov start
    if (is.null(fun)) {
        stop(
            "Missing implementation: ",
            fun_name,
            "().",
            call. = FALSE
            )
    }
    # nocov end

    fun(x)
}
