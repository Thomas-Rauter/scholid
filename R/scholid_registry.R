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

    refseq_prefixes <- c(
        "AC", "AP", "NC", "NG", "NM", "NP", "NR", "NT", "NW", "NZ",
        "XM", "XP", "XR", "YP", "WP"
    )
    refseq_prefix_pat <- paste(refseq_prefixes, collapse = "|")
    refseq_core_pat <- paste0(
        "(?:", refseq_prefix_pat, ")_[A-Z0-9]+\\.[0-9]+"
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
        bibcode = list(
            order = 21L,
            pat = "^\\d{4}[A-Za-z0-9.]{14}[A-Za-z]$",
            extract_pat = paste0(
                "(?<![[:alnum:]_/])",
                "(?:https?://(?:ui\\.)?adsabs\\.harvard\\.edu/abs/)?",
                "\\d{4}[A-Za-z0-9.]{14}[A-Za-z]",
                "(?![[:alnum:].])"
            )
        ),
        openalex = list(
            order = 22L,
            pat   = "^[WASTIKPFG][0-9]{5,}$",
            extract_pat = paste0(
                "(?<![[:alnum:]_./-])",
                "(?:https?://openalex\\.org/|",
                "https?://api\\.openalex\\.org/",
                "(?:works|authors|sources|institutions|topics|keywords|",
                "publishers|funders|grants|concepts)/)?",
                "[WASTIKPFG][0-9]{5,}",
                "(?![[:alnum:]_])"
            )
        ),
        swhid = list(
            order = 25L,
            core_pat = "^swh:1:(cnt|dir|rev|rel|snp):[0-9a-f]{40}$",
            qualifier_keys = c(
                "origin",
                "visit",
                "anchor",
                "path",
                "lines"
            ),
            extract_pat = paste0(
                "(?<![[:alnum:]_])",
                "(?:https?://(?:archive|browse)\\.softwareheritage\\.org/|",
                "https?://identifiers\\.org/swh/)?",
                "swh:1:(?:cnt|dir|rev|rel|snp):[0-9a-fA-F]{40}",
                "(?:;(?:origin|visit|anchor|path|lines)=[^[:space:]<>\")']+)*",
                "(?![[:alnum:]_:])"
            )
        ),
        ark = list(
            order = 27L,
            pat   = "^ark:/[0-9]{5}/[0-9A-Za-z][0-9A-Za-z._/=-]*$",
            extract_pat = paste0(
                "(?i)(?<![[:alnum:]_])",
                "(?:https?://[^[:space:]<>\")']+/)?",
                "ark:/*",
                "[0-9]{5}/",
                "[0-9A-Za-z][0-9A-Za-z._/=-]*",
                "(?![[:alnum:]_:=/])"
            )
        ),
        isni = list(
            order = 29L,
            pat   = "^\\d{15}[0-9X]$",
            extract_pat = paste0(
                "(?i)(?<![[:alnum:]_])",
                "(?:ISNI[[:space:]]*|",
                "https?://isni\\.org/isni/|",
                "urn:isni:|",
                "https?://viaf\\.org/viaf/sourceID/ISNI%7C)?",
                "(?:",
                "(?:\\d{4}[[:space:]]?){3}\\d{3}[0-9X]",
                "|",
                "\\d{15}[0-9X]",
                ")",
                "(?![[:alnum:]_\\-])"
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
        uniprot = list(
            order = 38L,
            pat   = paste0(
                "^(?:[OPQ][0-9][A-Z0-9]{3}[0-9]|",
                "[A-NR-Z][0-9](?:[A-Z][A-Z0-9]{2}[0-9]){1,2})$"
            ),
            extract_pat = paste0(
                "(?i)(?<![[:alnum:]_])",
                "(?:https?://(?:www\\.)?uniprot\\.org/(?:uniprot|uniprotkb)/|",
                "https?://identifiers\\.org/uniprot/|",
                "uniprot:)?",
                "(?:[OPQ][0-9][A-Z0-9]{3}[0-9]|",
                "[A-NR-Z][0-9](?:[A-Z][A-Z0-9]{2}[0-9]){1,2})",
                "(?![[:alnum:]_\\-])"
            )
        ),
        refseq = list(
            order = 39L,
            pat   = paste0("^", refseq_core_pat, "$"),
            extract_pat = paste0(
                "(?i)(?<![[:alnum:]_])",
                "(?:https?://www\\.ncbi\\.nlm\\.nih\\.gov/(?:nuccore|protein)/|",
                "https?://identifiers\\.org/refseq/|",
                "refseq:)?",
                refseq_core_pat,
                "(?![[:alnum:]_\\-])"
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
