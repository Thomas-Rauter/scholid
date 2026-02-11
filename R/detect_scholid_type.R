#' Detect scholarly identifier types
#'
#' @description
#' Performs best-effort detection of scholarly identifier types from possibly
#' wrapped identifier strings (e.g., URLs or labels).
#'
#' For each element of the input, the function returns the first matching
#' identifier type, or `NA_character_` if no supported type matches.
#'
#' Detection first attempts classification based on canonical identifier
#' syntax (see [classify_scholid()]). If no match is found, the function
#' attempts per-type normalization (see [normalize_scholid()]) and returns
#' the first type for which normalization yields a non-missing result.
#'
#' Use [normalize_scholid()] to convert detected values to canonical form
#' once the identifier type is known.
#'
#' @param x A vector of candidate identifier values.
#'
#' @return A character vector of the same length as `x`, giving the detected
#'   identifier type for each element, or `NA_character_` if no match is
#'   found.
#'
#' @examples
#' detect_scholid_type(c(
#'   "https://doi.org/10.1000/182",
#'   "doi:10.1000/182",
#'   "https://orcid.org/0000-0002-1825-0097",
#'   "arXiv:2101.12345v2",
#'   "PMID: 12345678",
#'   "PMCID: PMC1234567",
#'   "not an id"
#' ))
#'
#' @seealso [classify_scholid()], [normalize_scholid()], [scholid_types()]
#' @export
detect_scholid_type <- function(x) {
    .scholid_check_x(
        x,
        arg = "x"
    )

    x <- as.character(x)
    out <- rep(
        NA_character_,
        length(x)
    )

    idx <- which(!is.na(x))
    if (!length(idx)) {
        return(out)
    }

    # 1) strict canonical classification (after trimming)
    x_trim <- trimws(x)
    out[idx] <- classify_scholid(x_trim[idx])

    # 2) best-effort detection via per-type normalization
    rem <- idx[is.na(out[idx])]
    if (!length(rem)) {
        return(out)
    }

    types <- scholid_types()

    for (type in types) {
        norm <- normalize_scholid(
            x    = x_trim[rem],
            type = type
        )

        hit <- !is.na(norm)
        fill <- is.na(out[rem]) & hit
        out[rem[fill]] <- type
    }

    out
}
