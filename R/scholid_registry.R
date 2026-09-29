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
#' Each `extract_pat` marks the identifier with a named group `(?<id>...)`.
#' The group leaves out a URL, host, or label in front of the identifier and
#' keeps a prefix that the canonical form starts with, such as `RRID:`.
#' `.scholid_locate_type()` reports its position.
#'
#' Optional `version_pat` matches the version suffix at the end of the
#' canonical form, for types whose identifiers carry one. `scholid_key()`
#' drops it.
#'
#' Each entry states how the identifier is written as a link and as a
#' CURIE. `normalize_scholid()` reads these forms, and `format_scholid()`
#' writes them.
#'
#' - `url`: resolver URL templates, in which `{id}` stands for the
#'   canonical identifier. A template's name, if any, is a regular
#'   expression; a link uses the first template whose name matches the
#'   identifier or that has no name. `character(0)` means the type has no
#'   resolver.
#' - `url_alt` (optional): other URL templates that are read but never
#'   written, such as older forms that still resolve.
#' - `url_escape` (optional): characters of the identifier that
#'   `format_scholid()` percent-encodes in URLs, because the resolver would
#'   read them as URL syntax. The type's normalizer must decode them in
#'   URLs.
#' - `curie`: the Bioregistry prefix, or `NA_character_` if Bioregistry
#'   has no entry for the type. With `curie_is_id = TRUE`, the canonical
#'   form already starts with the prefix and a colon, in any case, so it is
#'   the CURIE form.
#' - `doi` (optional): the template of the DOI that the type's authority
#'   registers for each identifier.
#'
#' The sources are named in `vignettes/scholid_definitions.Rmd`.
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
    # RefSeq and assembly accessions end in a period and a version number.
    accession_version_pat <- "\\.[0-9]+"
    refseq_core_pat <- paste0(
        "(?:", refseq_prefix_pat, ")_[A-Z0-9]+", accession_version_pat
    )
    sra_core_pat <- "[SED]R[RXSP][0-9]{5,}"
    geo_core_pat <- "(?:GSE|GSM|GPL|GDS)[0-9]{2,}"
    bioproject_prefixes <- c("PRJNA", "PRJEB", "PRJDB", "PRJDA", "PRJEA")
    bioproject_prefix_pat <- paste(bioproject_prefixes, collapse = "|")
    bioproject_core_pat <- paste0(
        "(?:", bioproject_prefix_pat, ")[0-9]{2,}"
    )
    assembly_core_pat <- paste0("GC[AF]_[0-9]{9}", accession_version_pat)
    arxiv_version_pat <- "v\\d+"

    list(
        doi = list(
            order       = 10L,
            url         = "https://doi.org/{id}",
            url_alt     = "https://dx.doi.org/{id}",
            # DOI Handbook 4.7; doi.org reads these as an escape, a
            # fragment, and a query.
            url_escape  = c("%", "#", "?"),
            curie       = "doi",
            pat         = "^10\\.[0-9]{4,9}/\\S+$",
            extract_pat = "(?<![[:alnum:]_])(?<id>10\\.[0-9]{4,9}/\\S+)"
        ),
        arxiv = list(
            order = 20L,
            url   = "https://arxiv.org/abs/{id}",
            curie = "arxiv",
            doi   = "10.48550/arXiv.{id}",
            # post 2007
            pat1  = paste0("^\\d{4}\\.\\d{4,5}(", arxiv_version_pat, ")?$"),
            # pre 2007
            pat2  = paste0(
                "^[a-z]+(?:-[a-z]+)*(?:\\.[A-Z]{2})?/\\d{7}(",
                arxiv_version_pat,
                ")?$"
            ),
            version_pat = paste0(arxiv_version_pat, "$"),
            extract_pat = paste0(
                "(?<![[:alnum:]_\\./-])",
                "(?<id>",
                "\\d{4}\\.\\d{4,5}(", arxiv_version_pat, ")?",
                "|",
                "[a-z\\-]+/\\d{7}(", arxiv_version_pat, ")?",
                ")",
                "(?![[:alnum:]_\\-/])"
            )
        ),
        bibcode = list(
            order = 21L,
            url   = "https://ui.adsabs.harvard.edu/abs/{id}",
            url_alt = "https://adsabs.harvard.edu/abs/{id}",
            curie = NA_character_,
            pat = "^\\d{4}[A-Za-z0-9.]{14}[A-Za-z]$",
            extract_pat = paste0(
                "(?<![[:alnum:]_/])",
                "(?:https?://(?:ui\\.)?adsabs\\.harvard\\.edu/abs/)?",
                "(?<id>\\d{4}[A-Za-z0-9.]{14}[A-Za-z])",
                "(?![[:alnum:].])"
            )
        ),
        openalex = list(
            order = 22L,
            url   = "https://openalex.org/{id}",
            curie = "openalex",
            pat   = "^[WASTIKPFG][0-9]{5,}$",
            extract_pat = paste0(
                "(?<![[:alnum:]_./-])",
                "(?:https?://openalex\\.org/|",
                "https?://api\\.openalex\\.org/",
                "(?:works|authors|sources|institutions|topics|keywords|",
                "publishers|funders|grants|concepts)/)?",
                "(?<id>[WASTIKPFG][0-9]{5,})",
                "(?![[:alnum:]_])"
            )
        ),
        swhid = list(
            order = 25L,
            url   = "https://archive.softwareheritage.org/{id}",
            curie = "swh",
            curie_is_id = TRUE,
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
                "(?<id>swh:1:(?:cnt|dir|rev|rel|snp):[0-9a-fA-F]{40}",
                "(?:;(?:origin|visit|anchor|path|lines)=[^[:space:]<>\")']+)*)",
                "(?![[:alnum:]_:])"
            )
        ),
        ark = list(
            order = 27L,
            url   = "https://n2t.net/{id}",
            curie = "ark",
            curie_is_id = TRUE,
            pat   = "^ark:/[0-9]{5}/[0-9A-Za-z][0-9A-Za-z._/=-]*$",
            extract_pat = paste0(
                "(?i)(?<![[:alnum:]_])",
                "(?:https?://[^[:space:]<>\")']+/)?",
                "(?<id>ark:/*",
                "[0-9]{5}/",
                "[0-9A-Za-z][0-9A-Za-z._/=-]*)",
                "(?![[:alnum:]_:=/])"
            )
        ),
        isni = list(
            order = 29L,
            url   = "https://isni.org/isni/{id}",
            curie = "isni",
            pat   = "^\\d{15}[0-9X]$",
            extract_pat = paste0(
                "(?i)(?<![[:alnum:]_])",
                "(?:ISNI[[:space:]]*|",
                "https?://isni\\.org/isni/|",
                "urn:isni:|",
                "https?://viaf\\.org/viaf/sourceID/ISNI%7C)?",
                "(?<id>",
                "(?:\\d{4}[[:space:]]?){3}\\d{3}[0-9X]",
                "|",
                "\\d{15}[0-9X]",
                ")",
                "(?![[:alnum:]_\\-])"
            )
        ),
        orcid = list(
            order       = 30L,
            url         = "https://orcid.org/{id}",
            curie       = "orcid",
            extract_pat = "(?<id>\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9Xx])"
        ),
        ror = list(
            order       = 35L,
            url         = "https://ror.org/{id}",
            curie       = "ror",
            pat         = "^0[a-hjkmnp-tv-z0-9]{6}[0-9]{2}$",
            extract_pat = paste0(
                "(?<![[:alnum:]_./-])",
                "(?:https?://ror\\.org/)?",
                "(?<id>0[a-hjkmnp-tv-z0-9]{6}[0-9]{2})",
                "(?![[:alnum:]_])"
            )
        ),
        rrid = list(
            order = 37L,
            url   = "https://scicrunch.org/resolver/{id}",
            curie = "rrid",
            curie_is_id = TRUE,
            pat   = "^RRID:.+$",
            body_patterns = rrid_body_patterns,
            extract_pat = paste0(
                "(?<![[:alnum:]_./-])",
                "(?:https?://(?:scicrunch\\.org/resolver/|identifiers\\.org/|n2t\\.net/)?)?",
                "(?<id>RRID:[[:space:]]*",
                "(?:",
                paste(rrid_body_patterns, collapse = "|"),
                "))",
                "(?![[:alnum:]_])"
            )
        ),
        uniprot = list(
            order = 38L,
            url   = "https://www.uniprot.org/uniprotkb/{id}",
            url_alt = "https://www.uniprot.org/uniprot/{id}",
            curie = "uniprot",
            pat   = paste0(
                "^(?:[OPQ][0-9][A-Z0-9]{3}[0-9]|",
                "[A-NR-Z][0-9](?:[A-Z][A-Z0-9]{2}[0-9]){1,2})$"
            ),
            extract_pat = paste0(
                "(?i)(?<![[:alnum:]_])",
                "(?:https?://(?:www\\.)?uniprot\\.org/(?:uniprot|uniprotkb)/|",
                "https?://identifiers\\.org/uniprot/|",
                "uniprot:)?",
                "(?<id>[OPQ][0-9][A-Z0-9]{3}[0-9]|",
                "[A-NR-Z][0-9](?:[A-Z][A-Z0-9]{2}[0-9]){1,2})",
                "(?![[:alnum:]_\\-])"
            )
        ),
        refseq = list(
            order       = 39L,
            # Protein records resolve under /protein/, the others under
            # /nuccore/.
            url         = c(
                "^(?:AP|NP|WP|XP|YP)_" =
                    "https://www.ncbi.nlm.nih.gov/protein/{id}",
                "https://www.ncbi.nlm.nih.gov/nuccore/{id}"
            ),
            curie       = "refseq",
            pat         = paste0("^", refseq_core_pat, "$"),
            version_pat = paste0(accession_version_pat, "$"),
            extract_pat = paste0(
                "(?i)(?<![[:alnum:]_])",
                "(?:https?://www\\.ncbi\\.nlm\\.nih\\.gov/(?:nuccore|protein)/|",
                "https?://identifiers\\.org/refseq/|",
                "refseq:)?",
                "(?<id>",
                refseq_core_pat,
                ")",
                "(?![[:alnum:]_\\-])"
            )
        ),
        sra = list(
            order = 40L,
            url   = "https://www.ncbi.nlm.nih.gov/sra/{id}",
            curie = "insdc.sra",
            pat   = paste0("^", sra_core_pat, "$"),
            extract_pat = paste0(
                "(?i)(?<![[:alnum:]_])",
                "(?:https?://www\\.ncbi\\.nlm\\.nih\\.gov/sra/|",
                "https?://identifiers\\.org/sra/|",
                "sra:)?",
                "(?<id>",
                sra_core_pat,
                ")",
                "(?![[:alnum:]_\\-])"
            )
        ),
        geo = list(
            order = 41L,
            url   = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc={id}",
            curie = "geo",
            pat   = paste0("^", geo_core_pat, "$"),
            extract_pat = paste0(
                "(?i)(?<![[:alnum:]_])",
                "(?:https?://www\\.ncbi\\.nlm\\.nih\\.gov/geo/query/acc\\.cgi\\?acc=|",
                "https?://identifiers\\.org/geo/|",
                "geo:)?",
                "(?<id>",
                geo_core_pat,
                ")",
                "(?![[:alnum:]_\\-])"
            )
        ),
        bioproject = list(
            order = 42L,
            url   = "https://www.ncbi.nlm.nih.gov/bioproject/{id}",
            url_alt = "https://www.ncbi.nlm.nih.gov/bioproject/?term={id}",
            curie = "bioproject",
            pat   = paste0("^", bioproject_core_pat, "$"),
            extract_pat = paste0(
                "(?i)(?<![[:alnum:]_])",
                "(?:https?://www\\.ncbi\\.nlm\\.nih\\.gov/bioproject/(?:\\?term=)?|",
                "https?://identifiers\\.org/bioproject[:/]|",
                "bioproject:)?",
                "(?<id>",
                bioproject_core_pat,
                ")",
                "(?![[:alnum:]_\\-])"
            )
        ),
        assembly = list(
            order       = 43L,
            url         = "https://www.ncbi.nlm.nih.gov/datasets/genome/{id}",
            url_alt     = "https://www.ncbi.nlm.nih.gov/assembly/{id}",
            curie       = "ncbi.assembly",
            pat         = paste0("^", assembly_core_pat, "$"),
            version_pat = paste0(accession_version_pat, "$"),
            extract_pat = paste0(
                "(?i)(?<![[:alnum:]_])",
                "(?:https?://www\\.ncbi\\.nlm\\.nih\\.gov/(?:assembly|datasets/genome)/|",
                "https?://identifiers\\.org/insdc\\.(?:gca|gcf):|",
                "assembly:)?",
                "(?<id>",
                assembly_core_pat,
                ")",
                "(?![[:alnum:]_\\-])"
            )
        ),
        isbn = list(
            order       = 44L,
            url         = character(0),
            curie       = "isbn",
            extract_pat = "(?<![[:alnum:]_])(?<id>[0-9Xx][0-9Xx\\- ]{8,16}[0-9Xx])(?![[:alnum:]_\\-/])"
        ),
        issn = list(
            order       = 50L,
            url         = "https://portal.issn.org/resource/ISSN/{id}",
            url_alt     = "https://issn.org/resource/ISSN/{id}",
            curie       = "issn",
            extract_pat = "(?<![[:alnum:]_\\-])(?<id>\\d{4}-\\d{3}[0-9Xx])(?![[:alnum:]_\\-])"
        ),
        pmcid = list(
            order       = 60L,
            url         = "https://pmc.ncbi.nlm.nih.gov/articles/{id}/",
            url_alt     = "https://www.ncbi.nlm.nih.gov/pmc/articles/{id}/",
            curie       = "pmc",
            pat         = "^PMC\\d+$",
            extract_pat = "(?<![[:alnum:]_./-])(?<id>PMC\\d+)(?![[:alnum:]_]|[-/.][[:alnum:]_])"
        ),
        pmid = list(
            order       = 90L,
            url         = "https://pubmed.ncbi.nlm.nih.gov/{id}/",
            url_alt     = "https://www.ncbi.nlm.nih.gov/pubmed/{id}",
            curie       = "pubmed",
            detect_last = TRUE,
            pat         = "^\\d+$",
            extract_pat = paste0(
                "(?<![[:alnum:]_./-]|PMC)",
                "(?<id>\\d{4,9})",
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
