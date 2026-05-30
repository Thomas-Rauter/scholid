#' Test scholarly identifier validity
#'
#' @description
#' Vectorized predicate that tests whether values are valid scholarly
#' identifiers of a given supported type.
#'
#' Validation is stricter than normalization. Values must conform to the
#' canonical identifier syntax, and for identifier types with checksum
#' algorithms (e.g., ORCID, ISBN, ISSN), checksum correctness is verified.
#'
#' Inputs that are `NA` yield `NA`. Non-matching values return `FALSE`.
#'
#' Use [normalize_scholid()] to convert structurally plausible identifiers
#' to canonical form without performing checksum validation.
#'
#' @param x A vector of values to test.
#' @param type A single string giving the identifier type. See
#'   [scholid_types()] for supported values.
#'
#' @return A logical vector of the same length as `x`, indicating whether
#'   each element is a valid identifier of the specified type.
#'
#' @examples
#' is_scholid("10.1000/182", "doi")
#' is_scholid("0000-0002-1825-0097", "orcid")
#'
#' @seealso [normalize_scholid()], [scholid_types()]
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

    .scholid_dispatch(
        type   = type,
        prefix = "is_",
        x      = x
    )
}
