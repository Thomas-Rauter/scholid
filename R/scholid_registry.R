# Level 1 function (functions called by exported functions) definitions --------


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
        arxiv = list(
            pat1 = "^\\d{4}\\.\\d{4,5}(v\\d+)?$",       # post 2007
            # pre 2007
            pat2 = "^[a-z]+(?:-[a-z]+)*(?:\\.[A-Z]{2})?/\\d{7}(v\\d+)?$"
        ),
        doi = list(
            pat = "^10\\.[0-9]{4,9}/\\S+$"
        ),
        isbn = list(),  # checksum-based validation; no single shared pattern
        issn = list(),  # checksum-based validation; normaliz. uses compact form
        orcid = list(), # checksum-based validation; normaliz. uses compact form
        pmcid = list(
            pat = "^PMC\\d+$"
        ),
        pmid = list(
            pat = "^\\d+$"
        )
    )

    reg[order(names(reg))]
}
