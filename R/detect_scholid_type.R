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

    x_trim <- trimws(x)

    # 1) strict canonical classification
    out[idx] <- classify_scholid(x_trim[idx])

    types <- scholid_types()

    # 2) best-effort detection via per-type normalization
    #    for values still unresolved, and for provisional PMID hits
    rem <- idx[is.na(out[idx]) | out[idx] == "pmid"]
    if (!length(rem)) {
        return(out)
    }

    # Prefer more-specific types before PMID fallback
    detect_types <- setdiff(types, "pmid")

    for (type in detect_types) {
        vals <- x_trim[rem]

        if (identical(type, "isbn")) {
            vals <- sub(
                "^(?i:isbn(?:-1[03])?)\\s*:?\\s*",
                "",
                vals,
                perl = TRUE
            )
        }

        norm <- normalize_scholid(
            x = vals,
            type = type
        )

        hit <- !is.na(norm)
        fill <- hit & (is.na(out[rem]) | out[rem] == "pmid")
        out[rem[fill]] <- type
    }

    # 3) final PMID fallback for anything still unresolved
    rem2 <- idx[is.na(out[idx])]
    if (length(rem2)) {
        norm <- normalize_scholid(
            x = x_trim[rem2],
            type = "pmid"
        )
        out[rem2[!is.na(norm)]] <- "pmid"
    }

    out
}
