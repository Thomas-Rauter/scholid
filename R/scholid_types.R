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


#' Internal scholid identifier registry
#'
#' @description
#' Internal helper that defines the supported identifier types for scholid.
#' This is the single source of truth for type names used by exported helpers.
#'
#' @return A named list. Names are identifier types; values are reserved for
#'   per-type metadata.
#' @noRd
.scholid_registry <- function() {
    reg <- list(
        arxiv = list(),
        doi   = list(),
        isbn  = list(),
        issn  = list(),
        orcid = list(),
        pmcid = list(),
        pmid  = list()
    )
    reg[order(names(reg))]
}
