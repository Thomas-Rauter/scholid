#' Supported scholid identifier types
#'
#' @description
#' Returns the set of identifier types supported by the scholid package in
#' classification priority order (most specific first). For per-type formats,
#' validation rules, and classification precedence, see the *How Scholarly
#' Identifiers Are Defined* vignette
#' (`vignette("scholid_definitions", package = "scholid")`).
#'
#' @return A character vector of supported identifier type strings.
#' @examples
#' scholid_types()
#' "orcid" %in% scholid_types()
#' @export
scholid_types <- function() {
    .scholid_types_ordered()
}
