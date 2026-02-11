#' Supported scholid identifier types
#'
#' @description
#' Returns the set of identifier types supported by the scholid package.
#'
#' @return A character vector of supported identifier type strings.
#' @examples
#' scholid_types()
#' "orcid" %in% scholid_types()
#' @export
scholid_types <- function() {
    names(.scholid_registry())
}
