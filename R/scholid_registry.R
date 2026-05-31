# Level 1 function (functions called by exported functions) definitions --------


#' Internal scholid identifier registry
#'
#' @description
#' Internal helper that defines the supported identifier types for scholid.
#' This is the single source of truth for type names, classification order,
#' and per-type metadata.
#'
#' Each entry must include an `order` field. Lower values are checked first
#' during classification and detection. Optional `detect_last = TRUE` marks
#' fallback types that are deferred during best-effort detection.
#'
#' @return A named list. Names are identifier types; values are per-type
#'   metadata lists.
#' @noRd
.scholid_registry <- function() {
    rrid_body_patterns <- c(
        "AB_\\d+",
        "CVCL_[0-9A-Z]+",
        "SCR_\\d+",
        "Addgene_\\d+",
        "IMSR_[A-Z]+:\\d+",
        "MGI:\\d+",
        "WB:[A-Za-z0-9._-]+",
        "FlyBase:[A-Za-z0-9._-]+",
        "RGD:\\d+",
        "ZFIN:[A-Za-z0-9._-]+",
        "ZIRC:[A-Za-z0-9._-]+",
        "MMRRC_\\d+",
        "BDSC_\\d+",
        "DGGR_\\d+",
        "VDRC_\\d+",
        "BCBC_\\d+",
        "XGSC_[A-Za-z0-9._-]+",
        "NXR_[A-Za-z0-9._-]+",
        "RRRC_\\d+",
        "TSC_\\d+",
        "FlyORF_\\d+"
    )

    list(
        doi = list(
            order       = 10L,
            pat         = "^10\\.[0-9]{4,9}/\\S+$",
            extract_pat = "(?<![[:alnum:]_])(10\\.[0-9]{4,9}/\\S+)"
        ),
        arxiv = list(
            order = 20L,
            pat1  = "^\\d{4}\\.\\d{4,5}(v\\d+)?$",       # post 2007
            # pre 2007
            pat2  = "^[a-z]+(?:-[a-z]+)*(?:\\.[A-Z]{2})?/\\d{7}(v\\d+)?$",
            extract_pat = paste0(
                "(?<![[:alnum:]_\\./-])",
                "(",
                "\\d{4}\\.\\d{4,5}(v\\d+)?",
                "|",
                "[a-z\\-]+/\\d{7}(v\\d+)?",
                ")",
                "(?![[:alnum:]_\\-/])"
            )
        ),
        orcid = list(
            order       = 30L,
            extract_pat = "(\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9Xx])"
        ),
        ror = list(
            order       = 35L,
            pat         = "^0[a-hjkmnp-tv-z0-9]{6}[0-9]{2}$",
            extract_pat = paste0(
                "(?<![[:alnum:]_./-])",
                "(?:https?://ror\\.org/)?",
                "0[a-hjkmnp-tv-z0-9]{6}[0-9]{2}",
                "(?![[:alnum:]_])"
            )
        ),
        rrid = list(
            order = 37L,
            pat   = "^RRID:.+$",
            body_patterns = rrid_body_patterns,
            extract_pat = paste0(
                "(?<![[:alnum:]_./-])",
                "(?:https?://(?:scicrunch\\.org/resolver/|identifiers\\.org/|n2t\\.net/)?)?",
                "RRID:[[:space:]]*",
                "(?:",
                paste(rrid_body_patterns, collapse = "|"),
                ")",
                "(?![[:alnum:]_])"
            )
        ),
        isbn = list(
            order       = 40L,
            extract_pat = "(?<![[:alnum:]_])([0-9Xx][0-9Xx\\- ]{8,16}[0-9Xx])(?![[:alnum:]_\\-/])"
        ),
        issn = list(
            order       = 50L,
            extract_pat = "(?<![[:alnum:]_\\-])(\\d{4}-\\d{3}[0-9Xx])(?![[:alnum:]_\\-])"
        ),
        pmcid = list(
            order       = 60L,
            pat         = "^PMC\\d+$",
            extract_pat = "(?<![[:alnum:]_./-])PMC\\d+(?![[:alnum:]_]|[-/.][[:alnum:]_])"
        ),
        pmid = list(
            order       = 90L,
            detect_last = TRUE,
            pat         = "^\\d+$",
            extract_pat = paste0(
                "(?<![[:alnum:]_./-]|PMC)",
                "\\d{4,9}",
                "(?![[:alnum:]_]|[-/.][[:alnum:]_])"
            )
        )
    )
}


#' Return scholid identifier types in classification priority order
#'
#' @description
#' Internal helper that returns supported identifier types sorted by registry
#' `order`, with type names as a tie-breaker.
#'
#' @return A character vector of identifier type names.
#'
#' @noRd
.scholid_types_ordered <- function() {
    reg <- .scholid_registry()
    ord <- vapply(
        reg,
        function(entry) entry$order,
        integer(1)
    )
    names(reg)[order(ord, names(reg))]
}


#' Return identifier types marked for deferred detection
#'
#' @description
#' Internal helper that returns registry types with `detect_last = TRUE`, in
#' classification priority order.
#'
#' @return A character vector of identifier type names.
#'
#' @noRd
.scholid_detect_last_types <- function() {
    reg <- .scholid_registry()
    types <- .scholid_types_ordered()
    types[vapply(
        reg[types],
        function(entry) isTRUE(entry$detect_last),
        logical(1)
    )]
}


#' Return identifier types used for primary detection
#'
#' @description
#' Internal helper that returns registry types without `detect_last`, in
#' classification priority order.
#'
#' @return A character vector of identifier type names.
#'
#' @noRd
.scholid_detect_primary_types <- function() {
    reg <- .scholid_registry()
    types <- .scholid_types_ordered()
    types[!vapply(
        reg[types],
        function(entry) isTRUE(entry$detect_last),
        logical(1)
    )]
}


#' Return the free-text extraction pattern for an identifier type
#'
#' @description
#' Internal helper that returns the registry `extract_pat` for a supported
#' identifier type.
#'
#' @param type A validated identifier type string.
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.scholid_registry_extract_pat <- function(type) {
    entry <- .scholid_registry()[[type]]

    if (is.null(entry) || is.null(entry$extract_pat)) {
        stop(
            "Missing extract_pat registry entry for type: ",
            type,
            call. = FALSE
        )
    }

    entry$extract_pat
}
