#' Classify scholarly identifiers
#'
#' Performs best-guess classification of scholarly identifier strings.
#' For each element of the input, the function returns the first matching
#' identifier type, or `NA_character_` if no supported type matches.
#'
#' @param x A vector of candidate identifier values.
#'
#' @return A character vector of the same length as `x`, giving the detected
#'   identifier type for each element, or `NA_character_` if no match is
#'   found.
#'
#' @export
classify_scholid <- function(x) {
    .scholid_check_x(x, arg = "x")

    x <- as.character(x)
    out <- rep(NA_character_, length(x))

    idx <- which(!is.na(x))
    if (!length(idx)) {
        return(out)
    }

    types <- scholid_types()
    ns <- environment(classify_scholid)

    for (type in types) {
        fun <- get0(
            paste0("is_", type),
            envir = ns,
            mode = "function",
            inherits = FALSE
        )

        # nocov start
        if (is.null(fun)) {
            next
        }
        # nocov end

        res <- fun(x[idx])
        hit <- !is.na(res) & res

        fill <- is.na(out[idx]) & hit
        out[idx[fill]] <- type
    }

    out
}

