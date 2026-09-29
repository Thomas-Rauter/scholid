#' Normalize scholarly identifiers
#'
#' @description
#' Vectorized normalizer that converts supported scholarly identifier values
#' to a canonical form (e.g., removing URL prefixes, labels, or separators).
#'
#' Normalization requires that inputs match the expected identifier structure.
#' For identifier types with checksum algorithms (ORCID, ROR, ISNI, ISBN, ISSN),
#' normalization also requires checksum-valid values. Inputs that do not meet
#' these requirements yield `NA_character_`.
#'
#' Normalized outputs are canonical, type-specific representations of valid
#' identifiers.
#'
#' Use [is_scholid()] to test whether already-canonical values are valid
#' identifiers of a given type. Both functions apply checksum verification
#' where applicable; normalization additionally accepts wrapped input forms
#' and returns canonical strings.
#'
#' Before normalizing, invisible characters are removed and Unicode spaces
#' count as whitespace. For some types, Unicode dashes and full-width digits
#' are read as their ASCII forms. See "Input characters" in the *How
#' Scholarly Identifiers Are Defined* vignette
#' (`vignette("scholid_definitions", package = "scholid")`). Its DOI
#' section covers DOI case and the percent-decoding of `doi.org` URLs.
#'
#' Wrapped forms include each type's resolver URL and CURIE, and arXiv
#' DOIs for arXiv identifiers. "Resolver URLs and CURIEs" in the same
#' vignette says how they are read, and each type's section names its
#' forms.
#'
#' @param x A vector of values to normalize.
#' @param type A single string giving the identifier type. See
#'   [scholid_types()] for supported values.
#'
#' @return A character vector with the same length as `x`. Invalid, checksum-
#'   failing, or structurally non-matching inputs yield `NA_character_`.
#'
#' @examples
#' normalize_scholid("https://doi.org/10.1000/182", "doi")
#' normalize_scholid("https://orcid.org/0000-0002-1825-0097", "orcid")
#' normalize_scholid("pubmed:12345678", "pmid")
#' normalize_scholid("https://doi.org/10.48550/arXiv.2101.00001", "arxiv")
#'
#' @seealso [is_scholid()], [scholid_types()]
#' @export
normalize_scholid <- function(
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
        prefix = "normalize_",
        x      = x
    )
}


# Level 1 function (functions called by exported functions) definitions --------
## normalize_<id>() function definitions ---------------------------------------


#' Normalize Digital Object Identifiers
#'
#' @description
#' Normalizes DOI strings by removing resolver URLs, `doi:` labels, and
#' trailing punctuation. The part after a resolver URL's host is
#' percent-decoded. Bare DOIs and `doi:` labels are not, because `%` can be
#' part of a DOI name.
#'
#' @param x A vector of DOI values.
#'
#' @return A character vector of normalized DOIs.
#'
#' @noRd
normalize_doi <- function(x) {
    init <- .scholid_init_na_character(x)
    y <- trimws(init$x[init$ok])

    y <- sub("^doi:\\s*", "", y, ignore.case = TRUE)
    forms <- .scholid_strip_forms(y, "doi")
    y <- forms$x
    if (any(forms$url)) {
        y[forms$url] <- .scholid_clean_chars(
            .scholid_percent_decode(y[forms$url])
        )
    }
    y <- sub("[[:punct:]]+$", "", y)

    keep <- .is_doi_strict(y)
    y[!keep] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize SWHID identifiers
#'
#' @description
#' Normalizes Software Heritage identifiers from resolver URLs or labeled forms
#' to canonical compact `swh:` form. Inputs must include an explicit `swh:`
#' prefix or a supported resolver URL; bare 40-character hex strings are
#' rejected.
#'
#' Normalization requires structurally valid identifiers. Content-hash
#' correctness is not checked.
#'
#' @param x A vector of SWHID values.
#'
#' @return A character vector of normalized SWHIDs. Invalid or unsupported
#'   inputs yield `NA_character_`.
#'
#' @noRd
normalize_swhid <- function(x) {
    init <- .scholid_init_na_character(x)
    y <- trimws(init$x[init$ok])

    has_marker <- grepl("swh\\s*:", y, ignore.case = TRUE) |
        grepl("archive\\.softwareheritage\\.org/", y, ignore.case = TRUE) |
        grepl("browse\\.softwareheritage\\.org/", y, ignore.case = TRUE) |
        grepl("identifiers\\.org/swh/", y, ignore.case = TRUE)

    y[!has_marker] <- NA_character_
    is_url <- grepl("^https?://", y, ignore.case = TRUE)

    y <- sub(
        "^https?://archive\\.softwareheritage\\.org/",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub(
        "^https?://browse\\.softwareheritage\\.org/",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub(
        "^https?://identifiers\\.org/swh/",
        "",
        y,
        ignore.case = TRUE
    )
    y <- gsub("[[:space:]]+", "", y)
    y <- sub("[.,;:!?]+$", "", y)
    # A slash after a URL's SWHID. With qualifiers, it can end a path.
    y[is_url] <- sub("^([^;]*)/$", "\\1", y[is_url])

    todo <- !is.na(y) & nzchar(y)
    if (any(todo)) {
        canon <- .canonicalize_swhid(y[todo])
        good <- is_swhid(canon)
        good[is.na(good)] <- FALSE
        canon[!good] <- NA_character_
        y[todo] <- canon
    }
    y[!todo] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize ARK identifiers
#'
#' @description
#' Normalizes Archival Resource Keys from resolver URLs or `ark:`-prefixed
#' strings to canonical `ark:/NAAN/Name` form. Inputs must include an explicit
#' `ark:` label; bare paths without the label are rejected.
#'
#' Normalization requires structurally valid identifiers. Resolver existence
#' is not checked.
#'
#' @param x A vector of ARK values.
#'
#' @return A character vector of normalized ARKs. Invalid or unsupported
#'   inputs yield `NA_character_`.
#'
#' @noRd
normalize_ark <- function(x) {
    init <- .scholid_init_na_character(x)
    y <- trimws(init$x[init$ok])

    has_marker <- grepl("(?i)ark:", y, perl = TRUE)
    y[!has_marker] <- NA_character_

    todo <- !is.na(y) & nzchar(y)
    if (any(todo)) {
        canon <- .canonicalize_ark(y[todo])
        good <- is_ark(canon)
        good[is.na(good)] <- FALSE
        canon[!good] <- NA_character_
        y[todo] <- canon
    }
    y[!todo] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize ISNI identifiers
#'
#' @description
#' Normalizes International Standard Name Identifiers from resolver URLs,
#' CURIEs, `ISNI`-prefixed spaced forms, or compact 16-character strings to
#' canonical compact uppercase form. Bare hyphenated ORCID-style strings are
#' rejected.
#'
#' Normalization requires checksum-valid identifiers.
#'
#' @param x A vector of ISNI values.
#'
#' @return A character vector of normalized ISNIs. Invalid, unsupported, or
#'   checksum-failing inputs yield `NA_character_`.
#'
#' @noRd
normalize_isni <- function(x) {
    init <- .scholid_init_na_character(
        x,
        dashes = TRUE,
        digits = TRUE
    )
    y <- trimws(init$x[init$ok])
    y <- sub("[.,;:!?]+$", "", y)

    is_orcid_hyph <- grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9Xx]$", y)
    y[is_orcid_hyph] <- NA_character_

    forms <- .scholid_strip_forms(y, "isni")
    y <- forms$x

    bare_pat <- .isni_pat()
    has_marker <- forms$url | forms$curie |
        grepl("(?i)^urn:isni:", y, perl = TRUE) |
        grepl("(?i)^isni[[:space:]]", y, perl = TRUE) |
        grepl("viaf\\.org/viaf/sourceID/ISNI", y, ignore.case = TRUE) |
        grepl(paste0("(?i)", bare_pat), y, perl = TRUE) |
        grepl("(?i)^(?:\\d{4}[[:space:]]?){3}\\d{3}[0-9X]$", y, perl = TRUE)

    y[!has_marker] <- NA_character_

    y <- sub("(?i)^urn:isni:", "", y, perl = TRUE)
    y <- sub("(?i)^isni[[:space:]]*:?[[:space:]]*", "", y, perl = TRUE)
    y <- sub(
        "(?i)^https?://viaf\\.org/viaf/sourceID/ISNI(?:%7C|\\|)",
        "",
        y,
        perl = TRUE
    )
    y <- toupper(gsub("[-[:space:]]", "", y))

    y[!is.na(y) & !is_isni(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize ORCID identifiers
#'
#' @description
#' Normalizes ORCID iDs from canonical hyphenated, compact, space-separated,
#' URL-prefixed, or `orcid:`-prefixed forms to canonical hyphenated form.
#'
#' Only plausible ORCID input formats are accepted. Inputs with arbitrary
#' surrounding text, malformed separators, or other unsupported wrapping
#' yield `NA_character_`.
#'
#' Normalization requires checksum-valid identifiers.
#'
#' @param x A vector of ORCID values.
#'
#' @return A character vector of normalized ORCID iDs. Invalid,
#'   unsupported, or checksum-failing inputs yield `NA_character_`.
#'
#' @noRd
normalize_orcid <- function(x) {
    init <- .scholid_init_na_character(
        x,
        dashes = TRUE,
        digits = TRUE
    )
    y <- trimws(init$x[init$ok])

    y <- .scholid_strip_forms(y, "orcid")$x
    y <- sub("^orcid\\s*:\\s*", "", y, ignore.case = TRUE)

    is_hyph <- grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9Xx]$", y)
    is_comp <- grepl("^\\d{15}[0-9Xx]$", y)
    is_spac <- grepl("^\\d{4} \\d{4} \\d{4} \\d{3}[0-9Xx]$", y)

    y[!(is_hyph | is_comp | is_spac)] <- NA_character_

    y <- ifelse(
        is.na(y),
        NA_character_,
        toupper(gsub("[- ]", "", y))
    )

    y <- ifelse(
        is.na(y),
        NA_character_,
        paste(
            substr(y, 1, 4),
            substr(y, 5, 8),
            substr(y, 9, 12),
            substr(y, 13, 16),
            sep = "-"
        )
    )

    y[!is.na(y) & !is_orcid(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize UniProt accession numbers
#'
#' @description
#' Normalizes UniProtKB accession numbers from UniProt or identifiers.org
#' URLs, `uniprot:`-prefixed strings, or bare accessions to canonical uppercase
#' form.
#'
#' Normalization requires structurally valid accession numbers. Registry
#' existence is not checked.
#'
#' @param x A vector of UniProt values.
#'
#' @return A character vector of normalized UniProt accessions. Invalid or
#'   unsupported inputs yield `NA_character_`.
#'
#' @noRd
normalize_uniprot <- function(x) {
    init <- .scholid_init_na_character(
        x,
        digits = TRUE
    )
    y <- trimws(init$x[init$ok])
    y <- sub("[.,;:!?]+$", "", y)
    forms <- .scholid_strip_forms(y, "uniprot")
    y <- forms$x

    bare_pat <- .uniprot_pat()
    has_marker <- forms$url | forms$curie |
        grepl("uniprot\\.org/(?:uniprot|uniprotkb)/", y, ignore.case = TRUE) |
        grepl("identifiers\\.org/uniprot/", y, ignore.case = TRUE) |
        grepl("(?i)^uniprot:", y, perl = TRUE) |
        grepl(paste0("(?i)", bare_pat), y, perl = TRUE)

    y[!has_marker] <- NA_character_

    y <- sub(
        "^https?://(?:www\\.)?uniprot\\.org/(?:uniprot|uniprotkb)/",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub(
        "^https?://identifiers\\.org/uniprot/",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub("(?i)^uniprot:", "", y, perl = TRUE)
    y <- toupper(y)

    y[!is.na(y) & !is_uniprot(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize RefSeq accession numbers
#'
#' @description
#' Normalizes RefSeq accessions from NCBI or identifiers.org URLs,
#' `refseq:`-prefixed strings, or bare accessions to canonical uppercase form
#' with a version suffix.
#'
#' Normalization requires structurally valid accession numbers. Registry
#' existence is not checked.
#'
#' @param x A vector of RefSeq values.
#'
#' @return A character vector of normalized RefSeq accessions. Invalid or
#'   unsupported inputs yield `NA_character_`.
#'
#' @noRd
normalize_refseq <- function(x) {
    init <- .scholid_init_na_character(
        x,
        digits = TRUE
    )
    y <- trimws(init$x[init$ok])
    y <- sub("[.,;:!?]+$", "", y)
    forms <- .scholid_strip_forms(y, "refseq")
    y <- forms$x

    bare_pat <- .refseq_pat()
    has_marker <- forms$url | forms$curie |
        grepl("identifiers\\.org/refseq/", y, ignore.case = TRUE) |
        grepl("(?i)^refseq:", y, perl = TRUE) |
        grepl(paste0("(?i)", bare_pat), y, perl = TRUE)

    y[!has_marker] <- NA_character_

    y <- sub(
        "^https?://identifiers\\.org/refseq/",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub("(?i)^refseq:", "", y, perl = TRUE)
    y <- toupper(y)

    y[!is.na(y) & !is_refseq(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize SRA accession numbers
#'
#' @description
#' Normalizes SRA accessions from NCBI or identifiers.org URLs,
#' `sra:`-prefixed strings, or bare accessions to canonical uppercase form.
#'
#' Normalization requires structurally valid accession numbers. Registry
#' existence is not checked.
#'
#' @param x A vector of SRA values.
#'
#' @return A character vector of normalized SRA accessions. Invalid or
#'   unsupported inputs yield `NA_character_`.
#'
#' @noRd
normalize_sra <- function(x) {
    init <- .scholid_init_na_character(
        x,
        digits = TRUE
    )
    y <- trimws(init$x[init$ok])
    y <- sub("[.,;:!?]+$", "", y)
    forms <- .scholid_strip_forms(y, "sra")
    y <- forms$x

    bare_pat <- .sra_pat()
    has_marker <- forms$url | forms$curie |
        grepl("identifiers\\.org/sra/", y, ignore.case = TRUE) |
        grepl("(?i)^sra:", y, perl = TRUE) |
        grepl(paste0("(?i)", bare_pat), y, perl = TRUE)

    y[!has_marker] <- NA_character_

    y <- sub(
        "^https?://identifiers\\.org/sra/",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub("(?i)^sra:", "", y, perl = TRUE)
    y <- toupper(y)

    y[!is.na(y) & !is_sra(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize GEO accession numbers
#'
#' @description
#' Normalizes GEO accessions from NCBI or identifiers.org URLs,
#' `geo:`-prefixed strings, or bare accessions to canonical uppercase form.
#'
#' Normalization requires structurally valid accession numbers. Registry
#' existence is not checked.
#'
#' @param x A vector of GEO values.
#'
#' @return A character vector of normalized GEO accessions. Invalid or
#'   unsupported inputs yield `NA_character_`.
#'
#' @noRd
normalize_geo <- function(x) {
    init <- .scholid_init_na_character(
        x,
        digits = TRUE
    )
    y <- trimws(init$x[init$ok])
    y <- sub("[.,;:!?]+$", "", y)
    forms <- .scholid_strip_forms(y, "geo")
    y <- forms$x

    bare_pat <- .geo_pat()
    has_marker <- forms$url | forms$curie |
        grepl("identifiers\\.org/geo/", y, ignore.case = TRUE) |
        grepl("(?i)^geo:", y, perl = TRUE) |
        grepl(paste0("(?i)", bare_pat), y, perl = TRUE)

    y[!has_marker] <- NA_character_

    y <- sub(
        "^https?://identifiers\\.org/geo/",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub("(?i)^geo:", "", y, perl = TRUE)
    y <- sub("[?#].*$", "", y)
    y <- toupper(y)

    y[!is.na(y) & !is_geo(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize BioProject accession numbers
#'
#' @description
#' Normalizes BioProject accessions from NCBI or identifiers.org URLs,
#' `bioproject:`-prefixed strings, or bare accessions to canonical uppercase
#' form.
#'
#' Normalization requires structurally valid accession numbers. Registry
#' existence is not checked.
#'
#' @param x A vector of BioProject values.
#'
#' @return A character vector of normalized BioProject accessions. Invalid or
#'   unsupported inputs yield `NA_character_`.
#'
#' @noRd
normalize_bioproject <- function(x) {
    init <- .scholid_init_na_character(
        x,
        digits = TRUE
    )
    y <- trimws(init$x[init$ok])
    y <- sub("[.,;:!?]+$", "", y)
    forms <- .scholid_strip_forms(y, "bioproject")
    y <- forms$x

    bare_pat <- .bioproject_pat()
    has_marker <- forms$url | forms$curie |
        grepl("identifiers\\.org/bioproject", y, ignore.case = TRUE) |
        grepl("(?i)^bioproject:", y, perl = TRUE) |
        grepl(paste0("(?i)", bare_pat), y, perl = TRUE)

    y[!has_marker] <- NA_character_

    y <- sub(
        "^https?://identifiers\\.org/bioproject[:/]",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub("(?i)^bioproject:", "", y, perl = TRUE)
    y <- sub("[?#].*$", "", y)
    y <- toupper(y)

    y[!is.na(y) & !is_bioproject(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize genome assembly accession numbers
#'
#' @description
#' Normalizes INSDC assembly accessions from NCBI or identifiers.org URLs,
#' `assembly:`-prefixed strings, or bare accessions to canonical uppercase form
#' with a version suffix.
#'
#' Normalization requires structurally valid accession numbers. Registry
#' existence is not checked.
#'
#' @param x A vector of assembly values.
#'
#' @return A character vector of normalized assembly accessions. Invalid or
#'   unsupported inputs yield `NA_character_`.
#'
#' @noRd
normalize_assembly <- function(x) {
    init <- .scholid_init_na_character(
        x,
        digits = TRUE
    )
    y <- trimws(init$x[init$ok])
    y <- sub("[.,;:!?]+$", "", y)
    forms <- .scholid_strip_forms(y, "assembly")
    y <- forms$x

    bare_pat <- .assembly_pat()
    has_marker <- forms$url | forms$curie |
        grepl("identifiers\\.org/insdc\\.(?:gca|gcf):", y, ignore.case = TRUE) |
        grepl("(?i)^assembly:", y, perl = TRUE) |
        grepl(paste0("(?i)", bare_pat), y, perl = TRUE)

    y[!has_marker] <- NA_character_

    y <- sub(
        "^https?://identifiers\\.org/insdc\\.(?:gca|gcf):",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub("(?i)^assembly:", "", y, perl = TRUE)
    y <- sub("/+$", "", y)
    y <- toupper(y)

    y[!is.na(y) & !is_assembly(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize ROR identifiers
#'
#' @description
#' Normalizes ROR iDs from URL-prefixed, label-prefixed, or compact forms to
#' canonical lowercase compact form.
#'
#' Normalization requires checksum-valid identifiers.
#'
#' @param x A vector of ROR values.
#'
#' @return A character vector of normalized ROR iDs. Invalid or
#'   checksum-failing inputs yield `NA_character_`.
#'
#' @noRd
normalize_ror <- function(x) {
    init <- .scholid_init_na_character(
        x,
        digits = TRUE
    )
    y <- trimws(init$x[init$ok])

    y <- .scholid_strip_forms(y, "ror")$x
    y <- sub("^ror\\.org/", "", y, ignore.case = TRUE)
    y <- sub("^ror\\s*:?\\s*", "", y, ignore.case = TRUE)
    y <- sub("/+$", "", y)
    y <- tolower(y)

    y[!is.na(y) & !is_ror(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize RRID identifiers
#'
#' @description
#' Normalizes Research Resource Identifiers from resolver URLs or labeled forms
#' to canonical `RRID:` form. Inputs must include an explicit `RRID:` label
#' or a supported resolver URL; bare local IDs are rejected.
#'
#' Normalization requires structurally valid identifiers for known RRID
#' authorities.
#'
#' @param x A vector of RRID values.
#'
#' @return A character vector of normalized RRIDs. Invalid or unsupported
#'   inputs yield `NA_character_`.
#'
#' @noRd
normalize_rrid <- function(x) {
    init <- .scholid_init_na_character(x)
    y <- trimws(init$x[init$ok])
    forms <- .scholid_strip_forms(y, "rrid")
    y <- forms$x

    has_marker <- forms$url |
        grepl("RRID\\s*:", y, ignore.case = TRUE) |
        grepl("identifiers\\.org/", y, ignore.case = TRUE) |
        grepl("n2t\\.net/", y, ignore.case = TRUE) |
        grepl("bioregistry\\.io/rrid:", y, ignore.case = TRUE)

    y[!has_marker] <- NA_character_

    y <- sub(
        "^https?://identifiers\\.org/",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub(
        "^https?://n2t\\.net/rrid:",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub(
        "^https?://bioregistry\\.io/rrid:",
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub("^RRID[[:space:]]*:[[:space:]]*", "RRID:", y, ignore.case = TRUE)
    y <- sub("[[:punct:]]+$", "", y)

    y[!is.na(y) & !is_rrid(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize ISBN identifiers
#'
#' @description
#' Normalizes ISBN-10 and ISBN-13 values by removing optional `ISBN`,
#' `ISBN-10`, or `ISBN-13` labels, stripping separators, enforcing compact
#' canonical form, and requiring checksum-valid identifiers.
#'
#' @param x A vector of ISBN values.
#'
#' @return A character vector of normalized ISBNs. Invalid or
#'   checksum-failing inputs yield `NA_character_`.
#'
#' @noRd
normalize_isbn <- function(x) {
    init <- .scholid_init_na_character(
        x,
        dashes = TRUE,
        digits = TRUE
    )

    s <- trimws(init$x[init$ok])
    s <- .scholid_strip_forms(s, "isbn")$x
    s <- .strip_isbn_label(s)
    out <- rep(NA_character_, length(s))
    fmt <- .isbn_format_ok(s)

    if (any(fmt)) {
        compact <- toupper(gsub("[- ]", "", s[fmt]))
        shape <- grepl("^\\d{9}[0-9X]$", compact) |
            grepl("^\\d{13}$", compact)
        valid <- rep(FALSE, length(compact))
        if (any(shape)) {
            valid[shape] <- is_isbn(compact[shape])
        }
        out[which(fmt)[valid]] <- compact[valid]
    }

    init$out[init$ok] <- out
    init$out
}


#' Normalize ISSN identifiers
#'
#' @description
#' Normalizes ISSN values by removing resolver URLs and prefixes and
#' enforcing `NNNN-NNNN` format.
#'
#' @param x A vector of ISSN values.
#'
#' @return A character vector of normalized ISSNs.
#'
#' @noRd
normalize_issn <- function(x) {
    init <- .scholid_init_na_character(
        x,
        dashes = TRUE,
        digits = TRUE
    )
    y <- trimws(init$x[init$ok])
    y <- .scholid_strip_forms(y, "issn")$x

    # Remove only an optional ISSN label at the beginning
    y <- sub("^ISSN\\s*:?[[:space:]]*", "", y, ignore.case = TRUE)

    # Accept only full-string ISSN forms:
    # - hyphenated: NNNN-NNNN
    # - compact:    NNNNNNNN
    is_hyph <- grepl("^\\d{4}-\\d{3}[0-9Xx]$", y)
    is_comp <- grepl("^\\d{7}[0-9Xx]$", y)

    y[!(is_hyph | is_comp)] <- NA_character_

    # Canonicalize to compact uppercase form first
    y <- ifelse(
        is.na(y),
        NA_character_,
        toupper(gsub("-", "", y))
    )

    # Reinsert canonical hyphen
    y <- ifelse(
        is.na(y),
        NA_character_,
        paste0(substr(y, 1, 4), "-", substr(y, 5, 8))
    )

    # Keep only checksum-valid ISSNs
    y[!is.na(y) & !is_issn(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize arXiv identifiers
#'
#' @description
#' Normalizes arXiv identifiers by removing URL prefixes and `arXiv:`
#' labels, and reads arXiv DOIs.
#'
#' @param x A vector of arXiv identifier values.
#'
#' @return A character vector of normalized arXiv identifiers.
#'
#' @noRd
normalize_arxiv <- function(x) {
    init <- .scholid_init_na_character(
        x,
        digits = TRUE
    )
    y <- trimws(init$x[init$ok])

    from_doi <- .arxiv_from_doi(y)
    y <- sub("^arXiv:\\s*", "", y, ignore.case = TRUE)
    y <- .scholid_strip_forms(y, "arxiv")$x
    y[!is.na(from_doi)] <- from_doi[!is.na(from_doi)]

    y[!is.na(y) & !is_arxiv(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize ADS bibcodes
#'
#' @description
#' Normalizes SAO/NASA ADS bibliographic codes from ADS URLs, `bibcode:`
#' labels, or bare 19-character strings to canonical bibcode form. Case is
#' preserved.
#'
#' Normalization requires structurally valid bibcodes. ADS existence is not
#' checked.
#'
#' @param x A vector of bibcode values.
#'
#' @return A character vector of normalized bibcodes. Invalid or unsupported
#'   inputs yield `NA_character_`.
#'
#' @noRd
normalize_bibcode <- function(x) {
    init <- .scholid_init_na_character(
        x,
        digits = TRUE
    )
    y <- trimws(init$x[init$ok])
    y <- sub("[.,;:!?]+$", "", y)
    forms <- .scholid_strip_forms(y, "bibcode")
    y <- forms$x

    bare_pat <- .bibcode_pat()
    has_marker <- forms$url |
        grepl("(?i)^bibcode\\s*:", y, perl = TRUE) |
        grepl(bare_pat, y, perl = TRUE)

    y[!has_marker] <- NA_character_

    y <- sub("(?i)^bibcode\\s*:?\\s*", "", y, perl = TRUE)

    y[!is.na(y) & !is_bibcode(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize OpenAlex identifiers
#'
#' @description
#' Normalizes OpenAlex IDs from `openalex.org` or `api.openalex.org` URLs or
#' CURIEs to canonical uppercase key form. Inputs must include an explicit
#' OpenAlex URL or CURIE or a bare key matching the structural pattern;
#' other strings are rejected.
#'
#' Normalization requires structurally valid identifiers. Registry existence
#' is not checked.
#'
#' @param x A vector of OpenAlex values.
#'
#' @return A character vector of normalized OpenAlex IDs. Invalid or
#'   unsupported inputs yield `NA_character_`.
#'
#' @noRd
normalize_openalex <- function(x) {
    init <- .scholid_init_na_character(
        x,
        digits = TRUE
    )
    y <- trimws(init$x[init$ok])
    forms <- .scholid_strip_forms(y, "openalex")
    y <- forms$x

    bare_pat <- .openalex_key_pat()
    has_marker <- forms$url | forms$curie |
        grepl("openalex\\.org/", y, ignore.case = TRUE) |
        grepl(paste0("(?i)", bare_pat), y, perl = TRUE)

    y[!has_marker] <- NA_character_

    y <- sub(
        paste0(
            "^https?://api\\.openalex\\.org/",
            "(?:works|authors|sources|institutions|topics|keywords|",
            "publishers|funders|grants|concepts)/"
        ),
        "",
        y,
        ignore.case = TRUE
    )
    y <- sub("[[:punct:]]+$", "", y)
    y <- toupper(y)

    y[!is.na(y) & !is_openalex(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize PubMed identifiers
#'
#' @description
#' Normalizes PubMed identifiers by removing resolver URLs, CURIEs,
#' labels, and whitespace.
#'
#' @param x A vector of PubMed identifier values.
#'
#' @return A character vector of normalized PMIDs.
#'
#' @noRd
normalize_pmid <- function(x) {
    init <- .scholid_init_na_character(
        x,
        digits = TRUE
    )
    y <- trimws(init$x[init$ok])

    y <- .scholid_strip_forms(y, "pmid")$x
    y <- sub(
        "^PMID(?:[[:space:]]*:[[:space:]]*|[[:space:]]+)",
        "",
        y,
        ignore.case = TRUE
    )

    y[!is.na(y) & !is_pmid(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


#' Normalize PubMed Central identifiers
#'
#' @description
#' Normalizes PMCID values by removing resolver URLs, CURIEs, and optional
#' `PMCID` labels, and enforcing canonical `PMC`-prefixed form.
#'
#' When a `PMCID` label is present, digit-only values are interpreted as the
#' numeric part of a PMCID and normalized by restoring the missing `PMC`
#' prefix.
#'
#' @param x A vector of PubMed Central identifier values.
#'
#' @return A character vector of normalized PMCIDs. Invalid or unsupported
#'   inputs yield `NA_character_`.
#'
#' @noRd
normalize_pmcid <- function(x) {
    init <- .scholid_init_na_character(
        x,
        digits = TRUE
    )
    y <- trimws(init$x[init$ok])
    y <- .scholid_strip_forms(y, "pmcid")$x

    had_label <- grepl(
        "^PMCID\\s*:?[[:space:]]*",
        y,
        ignore.case = TRUE
    )

    y <- sub(
        "^PMCID\\s*:?[[:space:]]*",
        "",
        y,
        ignore.case = TRUE
    )

    y <- toupper(y)

    needs_prefix <- had_label & grepl("^\\d+$", y)
    y[needs_prefix] <- paste0("PMC", y[needs_prefix])

    y[!is.na(y) & !is_pmcid(y)] <- NA_character_

    init$out[init$ok] <- y
    init$out
}


# Level 2 functions (functions called by level 1 functions) definitions --------


#' Remove a resolver URL or CURIE prefix recorded in the registry
#'
#' @description
#' Removes from the start of each value a URL built from one of the
#' registry's `url` and `url_alt` templates for `type`, or else the
#' registry's `curie` prefix and its colon. A URL matches with `http` or
#' `https` and in any case. The rest of its template and a slash after the
#' identifier are removed too. If several templates match, the one with
#' the longest text before `{id}` wins. The CURIE prefix matches in any
#' case and must be followed by the identifier, not by a space. It is kept
#' for types with `curie_is_id`, whose canonical form starts with it.
#'
#' ARK and SWHID identifiers can end in a slash, so their normalizers don't
#' use this helper.
#'
#' @param x A character vector.
#' @param type A validated identifier type string.
#'
#' @return A list with `x`, the values without the URL or CURIE prefix, and
#'   the logical vectors `url` and `curie`, which mark the values that had
#'   one.
#'
#' @noRd
.scholid_strip_forms <- function(
        x,
        type
) {
    entry <- .scholid_registry()[[type]]
    url <- rep(FALSE, length(x))
    curie <- rep(FALSE, length(x))

    parts <- lapply(
        c(entry$url, entry$url_alt),
        .scholid_split_template
    )
    heads <- vapply(
        parts,
        function(p) p[["head"]],
        character(1)
    )
    for (i in order(-nchar(heads))) {
        # \Q...\E quotes the template text for PCRE.
        head_pat <- paste0(
            "^https?://\\Q",
            sub("^https?://", "", heads[[i]]),
            "\\E"
        )
        tail_pat <- paste0(
            "\\Q",
            sub("/$", "", parts[[i]][["tail"]]),
            "\\E/?$"
        )
        hit <- !url & !is.na(x) & grepl(
            head_pat,
            x,
            ignore.case = TRUE,
            perl        = TRUE
        )
        if (any(hit)) {
            y <- sub(head_pat, "", x[hit], ignore.case = TRUE, perl = TRUE)
            x[hit] <- sub(tail_pat, "", y, ignore.case = TRUE, perl = TRUE)
            url <- url | hit
        }
    }

    if (!is.na(entry$curie) && !isTRUE(entry$curie_is_id)) {
        curie_pat <- paste0("^\\Q", entry$curie, "\\E:(?![[:space:]])")
        curie <- !url & !is.na(x) & grepl(
            curie_pat,
            x,
            ignore.case = TRUE,
            perl        = TRUE
        )
        x[curie] <- sub(
            curie_pat,
            "",
            x[curie],
            ignore.case = TRUE,
            perl        = TRUE
        )
    }

    list(
        x     = x,
        url   = url,
        curie = curie
    )
}


#' Read arXiv identifiers from arXiv DOIs
#'
#' @description
#' Reads arXiv DataCite DOIs (`10.48550/arXiv.<id>`), bare, with a `doi:`
#' label, or as a `doi.org` URL, like `normalize_doi()`. The prefix matches
#' in any case, and the identifier is returned in lowercase. arXiv
#' registers one DOI per article, whose identifier has no version and, if
#' old-style, no subject class. DOIs with either, and all other values,
#' give `NA_character_`.
#'
#' @param x A character vector.
#'
#' @return A character vector the same length as `x`.
#'
#' @noRd
.arxiv_from_doi <- function(x) {
    reg <- .scholid_registry()[["arxiv"]]
    head <- .scholid_split_template(reg$doi)[["head"]]
    out <- rep(NA_character_, length(x))

    doi <- normalize_doi(x)
    hit <- !is.na(doi) & grepl(
        paste0("^\\Q", head, "\\E"),
        doi,
        ignore.case = TRUE,
        perl        = TRUE
    )
    if (any(hit)) {
        id <- tolower(substring(doi[hit], nchar(head) + 1L))
        unregistered <- grepl(reg$version_pat, id, perl = TRUE) |
            grepl("^[^/]*\\.[a-z]{2}/", id, perl = TRUE)
        id[unregistered] <- NA_character_
        out[hit] <- id
    }
    out
}


#' Decode percent-escapes
#'
#' @description
#' Decodes runs of `%XX` escapes as UTF-8. A value is returned unchanged
#' when it contains a `%` that does not start a two-digit hex escape, or when
#' an escape run decodes to a NUL byte or to invalid UTF-8.
#'
#' @param x A character vector.
#'
#' @return A character vector the same length as `x`.
#'
#' @noRd
.scholid_percent_decode <- function(x) {
    idx <- which(!is.na(x) & grepl("%", x, fixed = TRUE))
    if (length(idx)) {
        malformed <- grepl("%(?![0-9A-Fa-f]{2})", x[idx], perl = TRUE)
        idx <- idx[!malformed]
    }
    if (!length(idx)) {
        return(x)
    }

    y <- x[idx]
    m <- gregexpr("(?:%[0-9A-Fa-f]{2})+", y, perl = TRUE)
    runs <- regmatches(y, m)
    decoded <- vapply(
        unlist(runs, use.names = FALSE),
        .scholid_decode_escape_run,
        character(1),
        USE.NAMES = FALSE
    )
    group <- rep.int(seq_along(y), lengths(runs))
    good <- !(seq_along(y) %in% group[is.na(decoded)])
    if (!any(good)) {
        return(x)
    }

    keep <- group %in% which(good)
    y_good <- y[good]
    regmatches(y_good, m[good]) <- split(
        decoded[keep],
        factor(group[keep], levels = which(good))
    )
    x[idx[good]] <- y_good
    x
}


# Level 3 functions (functions called by level 2 functions) definitions --------


#' Split a registry template at its `{id}` placeholder
#'
#' @param template A single template string from the registry.
#'
#' @return A character vector with `head`, the text before `{id}`, and
#'   `tail`, the text after it.
#'
#' @noRd
.scholid_split_template <- function(template) {
    pos <- regexpr("{id}", template, fixed = TRUE)
    c(
        head = substr(template, 1L, pos - 1L),
        tail = substr(template, pos + 4L, nchar(template))
    )
}


#' Decode one run of percent-escapes
#'
#' @param run A single string of consecutive `%XX` escapes.
#'
#' @return The decoded UTF-8 string, or `NA_character_` if the bytes contain
#'   a NUL or are not valid UTF-8.
#'
#' @noRd
.scholid_decode_escape_run <- function(run) {
    n <- nchar(run)
    hex <- substring(
        run,
        seq.int(2L, n, 3L),
        seq.int(3L, n, 3L)
    )
    bytes <- as.raw(strtoi(hex, 16L))
    if (any(bytes == as.raw(0L))) {
        return(NA_character_)
    }

    out <- rawToChar(bytes)
    if (!validUTF8(out)) {
        return(NA_character_)
    }
    Encoding(out) <- "UTF-8"
    out
}
