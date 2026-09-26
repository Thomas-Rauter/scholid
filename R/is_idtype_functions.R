# Level 1 function (functions called by exported functions) definitions --------
## is_<id>() function definitions ----------------------------------------------


#' Check Digital Object Identifiers
#'
#' Tests whether values conform to the DOI syntax.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_doi <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- .is_doi_strict(init$x[init$ok])
    init$out
}


#' Check ORCID identifiers
#'
#' Tests whether values are valid ORCID iDs, including checksum.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_orcid <- function(x) {
    init <- .scholid_init_na_logical(x)

    pat <- "^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9Xx]$"
    y <- toupper(init$x[init$ok])

    valid <- grepl(pat, y)
    res <- rep(FALSE, length(y))

    if (any(valid)) {
        res[valid] <- .iso7064_mod11_2_valid(gsub("-", "", y[valid]))
    }
    init$out[init$ok] <- res
    init$out
}


#' Check ISNI identifiers
#'
#' Tests whether values are valid International Standard Name Identifiers in
#' canonical compact 16-character form, including ISO/IEC 7064 MOD 11-2
#' checksum. Hyphenated ORCID-style strings are rejected.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_isni <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- .is_isni_strict(init$x[init$ok])
    init$out
}


#' Check ISBN identifiers
#'
#' Tests whether values are valid ISBN-10 or ISBN-13 identifiers.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_isbn <- function(x) {
    init <- .scholid_init_na_logical(x)
    y <- init$x[init$ok]
    res <- rep(FALSE, length(y))
    fmt <- .isbn_format_ok(y)

    if (any(fmt)) {
        compact <- toupper(gsub("[- ]", "", y[fmt]))
        res[fmt] <- .isbn10_valid(compact) | .isbn13_valid(compact)
    }

    init$out[init$ok] <- res
    init$out
}


#' Check ISSN identifiers
#'
#' Tests whether values are valid ISSNs, including checksum.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_issn <- function(x) {
    init <- .scholid_init_na_logical(x)

    pat <- "^\\d{4}-\\d{3}[0-9X]$"
    y <- init$x[init$ok]
    res <- grepl(pat, y)

    if (any(res)) {
        compact <- gsub("-", "", y[res])
        acc <- rep(0, sum(res))
        w <- 8:2
        for (i in seq_len(7L)) {
            acc <- acc + as.integer(substr(compact, i, i)) * w[[i]]
        }
        r <- acc %% 11
        cd <- ifelse(
            r == 0,
            "0",
            ifelse(r == 1, "X", as.character(11 - r))
        )
        res[res] <- cd == substr(compact, 8L, 8L)
    }

    init$out[init$ok] <- res
    init$out
}


#' Check arXiv identifiers
#'
#' Tests whether values match valid arXiv identifier formats.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_arxiv <- function(x) {
    init <- .scholid_init_na_logical(x)
    reg <- .scholid_registry()[["arxiv"]]
    pat1 <- reg$pat1
    pat2 <- reg$pat2
    init$out[init$ok] <- grepl(pat1, init$x[init$ok], perl = TRUE) |
        grepl(pat2, init$x[init$ok], perl = TRUE)
    init$out
}


#' Check ARK identifiers
#'
#' Tests whether values are valid Archival Resource Keys in canonical `ark:/`
#' form. Validation is structural only; resolver existence is not checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_ark <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- .is_ark_strict(init$x[init$ok])
    init$out
}


#' Check ADS bibcodes
#'
#' Tests whether values are valid SAO/NASA ADS bibliographic codes in canonical
#' 19-character form. Validation is structural only; ADS existence is not
#' checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_bibcode <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- .is_bibcode_strict(init$x[init$ok])
    init$out
}


#' Check OpenAlex identifiers
#'
#' Tests whether values are valid OpenAlex IDs in canonical uppercase key
#' form. Validation is structural only; registry existence is not checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_openalex <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- .is_openalex_strict(init$x[init$ok])
    init$out
}


#' Check SWHID identifiers
#'
#' Tests whether values are valid Software Heritage identifiers in canonical
#' `swh:` form. Validation is structural only; content-hash correctness is
#' not checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_swhid <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- .is_swhid_strict(init$x[init$ok])
    init$out
}


#' Check UniProt accession numbers
#'
#' Tests whether values are valid UniProtKB accession numbers in canonical
#' uppercase form. Validation is structural only; registry existence is not
#' checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_uniprot <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- .is_uniprot_strict(init$x[init$ok])
    init$out
}


#' Check RefSeq accession numbers
#'
#' Tests whether values are valid NCBI RefSeq accessions in canonical
#' uppercase form with a version suffix. Validation is structural only;
#' registry existence is not checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_refseq <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- .is_refseq_strict(init$x[init$ok])
    init$out
}


#' Check SRA accession numbers
#'
#' Tests whether values are valid INSDC SRA accessions in canonical
#' uppercase form. Validation is structural only; registry existence is not
#' checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_sra <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- .is_sra_strict(init$x[init$ok])
    init$out
}


#' Check GEO accession numbers
#'
#' Tests whether values are valid NCBI GEO accessions in canonical uppercase
#' form. Validation is structural only; registry existence is not checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_geo <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- .is_geo_strict(init$x[init$ok])
    init$out
}


#' Check BioProject accession numbers
#'
#' Tests whether values are valid INSDC BioProject accessions in canonical
#' uppercase form. Validation is structural only; registry existence is not
#' checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_bioproject <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- .is_bioproject_strict(init$x[init$ok])
    init$out
}


#' Check genome assembly accession numbers
#'
#' Tests whether values are valid INSDC genome assembly accessions (`GCA_`,
#' `GCF_`) in canonical uppercase form with a version suffix. Validation is
#' structural only; registry existence is not checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_assembly <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- .is_assembly_strict(init$x[init$ok])
    init$out
}


#' Check ROR identifiers
#'
#' Tests whether values are valid ROR iDs, including checksum.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_ror <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- .is_ror_strict(init$x[init$ok])
    init$out
}


#' Check RRID identifiers
#'
#' Tests whether values are valid Research Resource Identifiers in canonical
#' `RRID:` form. Validation is structural and limited to known RRID authority
#' prefixes; registry existence is not checked.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_rrid <- function(x) {
    init <- .scholid_init_na_logical(x)
    init$out[init$ok] <- .is_rrid_strict(init$x[init$ok])
    init$out
}


#' Check PubMed identifiers
#'
#' Tests whether values are structurally plausible PubMed identifiers
#' (PMIDs). PMID checks are based on digit-only syntax, with exclusion of
#' values that are valid ISBNs to reduce cross-type false positives.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_pmid <- function(x) {
    init <- .scholid_init_na_logical(x)
    y <- init$x[init$ok]

    pat <- .scholid_registry()[["pmid"]]$pat
    res <- grepl(pat, y, perl = TRUE)

    res[res] <- !is_isbn(y[res])

    init$out[init$ok] <- res
    init$out
}


#' Check PubMed Central identifiers
#'
#' Tests whether values are valid PMCID identifiers.
#'
#' @param x A vector of values to check.
#'
#' @return A logical vector. `NA` inputs yield `NA`.
#'
#' @noRd
is_pmcid <- function(x) {
    init <- .scholid_init_na_logical(x)
    pat <- .scholid_registry()[["pmcid"]]$pat
    init$out[init$ok] <- grepl(pat, init$x[init$ok], perl = TRUE)
    init$out
}


# Level 2 functions (functions called by level 1 functions) definitions --------


#' Validate compact 16-character ISO/IEC 7064 MOD 11-2 identifiers
#'
#' @description
#' Internal helper shared by ORCID and ISNI validators. Each value must be a
#' 16-character string of digits with an optional `X` check character.
#'
#' @param compact A character vector of compact 16-character identifiers.
#'
#' @return A logical vector the same length as `compact`.
#'
#' @noRd
.iso7064_mod11_2_valid <- function(compact) {
    res <- rep(FALSE, length(compact))
    ok <- !is.na(compact) & nchar(compact) == 16L
    if (!any(ok)) {
        return(res)
    }

    y <- compact[ok]
    shape <- grepl("^[0-9]{15}[0-9X]$", y)
    if (!any(shape)) {
        return(res)
    }

    z <- y[shape]
    acc <- rep(0L, length(z))
    for (i in seq_len(15L)) {
        acc <- (acc + as.integer(substr(z, i, i))) * 2L
    }
    r <- (12L - (acc %% 11L)) %% 11L
    cd <- ifelse(r == 10L, "X", as.character(r))
    res[which(ok)[shape]] <- cd == substr(z, 16L, 16L)
    res
}


#' Return the ISNI validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.isni_pat <- function() {
    .scholid_registry()[["isni"]]$pat
}


#' Strict ISNI validator
#'
#' @description
#' Validates canonical compact ISNIs (`000000012146438X`). Hyphenated
#' ORCID-style strings and wrapped forms are rejected; use
#' `normalize_isni()` first.
#'
#' @param x A character vector in canonical form.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.is_isni_strict <- function(x) {
    res <- rep(FALSE, length(x))
    ok <- !is.na(x) & nzchar(x)
    if (!any(ok)) {
        return(res)
    }

    y <- trimws(x[ok])
    sep <- grepl("[[:space:]-]", y, perl = TRUE)
    y <- toupper(y)
    hit <- !sep & grepl(.isni_pat(), y, perl = TRUE)
    if (any(hit)) {
        hit[hit] <- .iso7064_mod11_2_valid(y[hit])
    }
    res[which(ok)] <- hit
    res
}


#' Strip optional ISBN labels from identifier strings
#'
#' @description
#' Removes optional `ISBN`, `ISBN-10`, or `ISBN-13` labels from the
#' beginning of identifier strings.
#'
#' @param x A character vector of candidate ISBN strings.
#'
#' @return A character vector with labels removed.
#'
#' @noRd
.strip_isbn_label <- function(x) {
    sub(
        "^(?i:isbn(?:-1[03])?)\\s*:?\\s*",
        "",
        x,
        perl = TRUE
    )
}


#' Return the OpenAlex key validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.openalex_key_pat <- function() {
    .scholid_registry()[["openalex"]]$pat
}


#' Strict OpenAlex validator
#'
#' @description
#' Validates canonical uppercase OpenAlex keys (`W2741809807`). Wrapped URLs
#' and lowercase keys are rejected; use `normalize_openalex()` first.
#'
#' @param x A character vector in canonical form.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.is_openalex_strict <- function(x) {
    res <- rep(FALSE, length(x))
    ok <- !is.na(x) & nzchar(x)
    if (!any(ok)) {
        return(res)
    }

    y <- trimws(x[ok])
    # UniProtKB 6-character accessions share P/O/Q/G + digit prefixes
    # with OpenAlex keys.
    res[which(ok)] <- !grepl("[[:space:]]", y, perl = TRUE) &
        grepl(.openalex_key_pat(), y, perl = TRUE) &
        !grepl(.uniprot_pat(), y, perl = TRUE) &
        (y == toupper(y))
    res[is.na(res)] <- FALSE
    res
}


#' Return the ARK validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.ark_pat <- function() {
    .scholid_registry()[["ark"]]$pat
}


#' Return the UniProt validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.uniprot_pat <- function() {
    .scholid_registry()[["uniprot"]]$pat
}


#' Return the RefSeq validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.refseq_pat <- function() {
    .scholid_registry()[["refseq"]]$pat
}


#' Return the SRA validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.sra_pat <- function() {
    .scholid_registry()[["sra"]]$pat
}


#' Return the GEO validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.geo_pat <- function() {
    .scholid_registry()[["geo"]]$pat
}


#' Return the BioProject validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.bioproject_pat <- function() {
    .scholid_registry()[["bioproject"]]$pat
}


#' Return the genome assembly validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.assembly_pat <- function() {
    .scholid_registry()[["assembly"]]$pat
}


#' Canonicalize ARK strings to ark:/NAAN/Name form
#'
#' @param x A character vector of ARK candidates.
#'
#' @return A character vector of canonical ARK strings, with `NA_character_`
#'   where no ARK label is present.
#'
#' @noRd
.canonicalize_ark <- function(x) {
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x) & nzchar(x)
    if (!any(ok)) {
        return(out)
    }

    y <- trimws(x[ok])
    pos <- regexpr("(?i)ark:", y, perl = TRUE)
    found <- !is.na(pos) & pos > 0L
    if (!any(found)) {
        return(out)
    }

    z <- substr(y[found], pos[found], nchar(y[found]))
    z <- sub("(?i)^ark:/*", "ark:/", z, perl = TRUE)
    z <- sub("[.,;:!?]+$", "", z)
    z <- sub("[?#].*$", "", z)
    out[which(ok)[found]] <- z
    out
}


#' Strict structural check for canonical uppercase tokens
#'
#' @description
#' Vectorized check used by accession validators. Missing and empty values
#' are rejected. A leading URL, a character matching `reject_pat`, or any
#' lowercase letter is rejected. Remaining values must match `pat`.
#'
#' @param x A character vector.
#' @param pat Validation pattern.
#' @param reject_pat Pattern of disallowed characters.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.is_upper_token <- function(x, pat, reject_pat) {
    res <- rep(FALSE, length(x))
    ok <- !is.na(x) & nzchar(x)
    if (!any(ok)) {
        return(res)
    }

    y <- trimws(x[ok])
    res[which(ok)] <- !grepl("^https?://", y, ignore.case = TRUE) &
        !grepl(reject_pat, y, perl = TRUE) &
        (y == toupper(y)) &
        grepl(pat, y, perl = TRUE)
    res[is.na(res)] <- FALSE
    res
}


#' Return the bibcode validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.bibcode_pat <- function() {
    .scholid_registry()[["bibcode"]]$pat
}


#' Strict UniProt validator
#'
#' @description
#' Validates canonical uppercase UniProtKB accession numbers (`P12345`,
#' `A0A022YWF9`). Wrapped URLs and lowercase accessions are rejected; use
#' `normalize_uniprot()` first.
#'
#' @param x A character vector in canonical form.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.is_uniprot_strict <- function(x) {
    .is_upper_token(
        x,
        pat = .uniprot_pat(),
        reject_pat = "[[:space:]/|:]"
    )
}


#' Strict RefSeq validator
#'
#' @description
#' Validates canonical uppercase RefSeq accessions (`NM_001744.6`,
#' `NP_001735.1`). Wrapped URLs and lowercase accessions are rejected; use
#' `normalize_refseq()` first.
#'
#' @param x A character vector in canonical form.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.is_refseq_strict <- function(x) {
    .is_upper_token(
        x,
        pat = .refseq_pat(),
        reject_pat = "[[:space:]/|:]"
    )
}


#' Strict SRA validator
#'
#' @description
#' Validates canonical uppercase SRA accessions (`SRR1553610`,
#' `SRX1234567`). Wrapped URLs and lowercase accessions are rejected; use
#' `normalize_sra()` first.
#'
#' @param x A character vector in canonical form.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.is_sra_strict <- function(x) {
    .is_upper_token(
        x,
        pat = .sra_pat(),
        reject_pat = "[[:space:]/|:]"
    )
}


#' Strict GEO validator
#'
#' @description
#' Validates canonical uppercase GEO accessions (`GSE2553`, `GSM313800`,
#' `GPL96`). Wrapped URLs and lowercase accessions are rejected; use
#' `normalize_geo()` first.
#'
#' @param x A character vector in canonical form.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.is_geo_strict <- function(x) {
    .is_upper_token(
        x,
        pat = .geo_pat(),
        reject_pat = "[[:space:]/|:?&=]"
    )
}


#' Strict BioProject validator
#'
#' @description
#' Validates canonical uppercase BioProject accessions (`PRJNA257197`,
#' `PRJEB12345`). Wrapped URLs and lowercase accessions are rejected; use
#' `normalize_bioproject()` first.
#'
#' @param x A character vector in canonical form.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.is_bioproject_strict <- function(x) {
    .is_upper_token(
        x,
        pat = .bioproject_pat(),
        reject_pat = "[[:space:]/|:?&=]"
    )
}


#' Strict genome assembly validator
#'
#' @description
#' Validates canonical uppercase assembly accessions (`GCF_000001405.40`,
#' `GCA_009914755.4`). Wrapped URLs and lowercase accessions are rejected; use
#' `normalize_assembly()` first.
#'
#' @param x A character vector in canonical form.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.is_assembly_strict <- function(x) {
    .is_upper_token(
        x,
        pat = .assembly_pat(),
        reject_pat = "[[:space:]/|:?&=]"
    )
}


#' Strict ARK validator
#'
#' @description
#' Validates canonical `ark:/NAAN/Name` identifiers. Wrapped URLs and bare
#' paths without the `ark:` label are rejected; use `normalize_ark()` first.
#'
#' @param x A character vector in canonical form.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.is_ark_strict <- function(x) {
    res <- rep(FALSE, length(x))
    ok <- !is.na(x) & nzchar(x)
    if (!any(ok)) {
        return(res)
    }

    y <- trimws(x[ok])
    cand <- !grepl("^https?://", y, ignore.case = TRUE) &
        grepl("(?i)^ark:", y, perl = TRUE)
    good <- rep(FALSE, length(y))
    if (any(cand)) {
        canon <- .canonicalize_ark(y[cand])
        good[cand] <- !is.na(canon) &
            !grepl("[[:space:]]", canon, perl = TRUE) &
            grepl(.ark_pat(), canon, perl = TRUE)
    }
    res[which(ok)] <- good
    res[is.na(res)] <- FALSE
    res
}


#' Strict bibcode validator
#'
#' @description
#' Validates canonical 19-character ADS bibcodes (`YYYYJJJJJVVVVM PPPPA`).
#' Wrapped URLs are rejected; use `normalize_bibcode()` first.
#'
#' @param x A character vector in canonical form.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.is_bibcode_strict <- function(x) {
    res <- rep(FALSE, length(x))
    ok <- !is.na(x) & nzchar(x)
    if (!any(ok)) {
        return(res)
    }

    y <- trimws(x[ok])
    journal <- substr(y, 5L, 9L)
    res[which(ok)] <- nchar(y) == 19L &
        !grepl("[[:space:]]", y, perl = TRUE) &
        grepl(.bibcode_pat(), y, perl = TRUE) &
        grepl("[A-Za-z]", journal, perl = TRUE)
    res[is.na(res)] <- FALSE
    res
}


#' Strict DOI validator
#'
#' @param x A character vector.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.is_doi_strict <- function(x) {
    res <- !is.na(x) & nzchar(x)
    if (!any(res)) {
        return(rep(FALSE, length(x)))
    }

    y <- x[res]
    pat <- .scholid_registry()[["doi"]]$pat
    res[res] <- grepl(pat, y, perl = TRUE) &
        !grepl("[\"']", y, perl = TRUE) &
        !grepl("</", y, perl = TRUE) &
        !grepl(">[^[:space:]]*<", y, perl = TRUE) &
        !grepl("[<>()\\[\\]{}]$", y, perl = TRUE) &
        !grepl("[)\\]}>][[:alpha:]]+$", y, perl = TRUE)
    res[is.na(res)] <- FALSE
    res
}


#' Decode a Crockford base32 string to an integer
#'
#' @description
#' Internal helper for ROR checksum validation. Accepts lowercase Crockford
#' base32 strings and maps `i`/`l` to `1` and `o` to `0`, following ROR's
#' identifier generation rules.
#'
#' @param x A single Crockford base32 string.
#'
#' @return An integer value, or `NA_integer_` if decoding fails.
#'
#' @noRd
.crockford_base32_decode <- function(x) {
    if (is.na(x) || !nzchar(x)) {
        return(NA_integer_)
    }

    chars <- strsplit(gsub("-", "", tolower(x), fixed = TRUE), "")[[1]]
    alphabet <- strsplit("0123456789abcdefghjkmnpqrstvwxyz", "")[[1]]
    n <- 0L

    for (ch in chars) {
        if (ch %in% c("i", "l")) {
            ch <- "1"
        } else if (ch == "o") {
            ch <- "0"
        }

        idx <- match(ch, alphabet)
        if (is.na(idx)) {
            return(NA_integer_)
        }

        n <- n * 32L + (idx - 1L)
    }

    n
}


#' Strict ROR validator
#'
#' @param x A character vector in canonical compact form.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.is_ror_strict <- function(x) {
    res <- rep(FALSE, length(x))
    ok <- !is.na(x) & nzchar(x)
    if (!any(ok)) {
        return(res)
    }

    y <- tolower(trimws(x[ok]))
    pat <- .scholid_registry()[["ror"]]$pat
    hit <- grepl(pat, y, perl = TRUE)
    if (any(hit)) {
        cand <- y[hit]
        body_num <- vapply(
            substr(cand, 2L, 7L),
            .crockford_base32_decode,
            integer(1),
            USE.NAMES = FALSE
        )
        known <- !is.na(body_num)
        good <- rep(FALSE, length(cand))
        if (any(known)) {
            expected <- sprintf(
                "%02d",
                98L - (as.numeric(body_num[known]) * 100) %% 97
            )
            good[known] <- substr(cand[known], 8L, 9L) == expected
        }
        hit[hit] <- good
    }
    res[which(ok)] <- hit
    res
}


#' Return RRID body patterns from the registry
#'
#' @return A character vector of regular expression fragments for RRID bodies.
#'
#' @noRd
.rrid_body_patterns <- function() {
    .scholid_registry()[["rrid"]]$body_patterns
}


#' Strict RRID validator
#'
#' @description
#' Validates canonical `RRID:` identifiers against a conservative allowlist
#' of known authority body patterns. Bare local IDs without the `RRID:` prefix
#' are rejected.
#'
#' @param x A character vector in canonical form.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.is_rrid_strict <- function(x) {
    res <- rep(FALSE, length(x))
    ok <- !is.na(x) & nzchar(x)
    if (!any(ok)) {
        return(res)
    }

    y <- trimws(x[ok])
    has <- grepl("^RRID:", y)
    body <- substr(y, 6L, nchar(y))
    hit <- has & nzchar(body)
    if (any(hit)) {
        b <- body[hit]
        matched <- rep(FALSE, length(b))
        for (p in .rrid_body_patterns()) {
            matched <- matched | grepl(
                paste0("^", p, "$"),
                b,
                perl = TRUE
            )
        }
        hit[hit] <- matched
    }
    res[which(ok)] <- hit
    res
}


#' Return the SWHID core validation pattern from the registry
#'
#' @return A single regular expression pattern string.
#'
#' @noRd
.swhid_core_pat <- function() {
    .scholid_registry()[["swhid"]]$core_pat
}


#' Split SWHIDs into core and qualifier segments
#'
#' @param x A character vector of compact SWHID strings.
#'
#' @return A list with `core` and `qualifiers` character vectors, each the
#'   same length as `x`. Qualifiers are `""` when a value has no semicolon.
#'
#' @noRd
.swhid_split <- function(x) {
    n <- length(x)
    core <- as.character(x)
    qual <- rep("", n)
    ok <- !is.na(x)
    if (!any(ok)) {
        return(list(
            core       = core,
            qualifiers = qual
        ))
    }

    pos <- rep(-1L, n)
    pos[ok] <- regexpr(";", x[ok], fixed = TRUE)
    has <- !is.na(pos) & pos > 0L
    if (any(has)) {
        core[has] <- substr(x[has], 1L, pos[has] - 1L)
        qual[has] <- substr(x[has], pos[has] + 1L, nchar(x[has]))
    }

    list(
        core       = core,
        qualifiers = qual
    )
}


#' Validate SWHID qualifier segments
#'
#' @param qualifiers A semicolon-separated qualifier string without a leading
#'   semicolon.
#'
#' @return A single logical value.
#'
#' @noRd
.is_swhid_qualifiers_valid <- function(qualifiers) {
    if (!nzchar(qualifiers)) {
        return(TRUE)
    }

    parts <- strsplit(qualifiers, ";", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]

    if (!length(parts)) {
        return(TRUE)
    }

    keys <- character(0)
    core_pat <- .swhid_core_pat()

    for (part in parts) {
        if (!grepl("^(origin|visit|anchor|path|lines)=", part, perl = TRUE)) {
            return(FALSE)
        }

        key <- sub("=.*$", "", part)
        if (key %in% keys) {
            return(FALSE)
        }
        keys <- c(keys, key)

        val <- sub("^[^=]+=", "", part)
        if (!nzchar(val)) {
            return(FALSE)
        }

        if (key %in% c("visit", "anchor")) {
            if (!grepl(core_pat, val, perl = TRUE)) {
                return(FALSE)
            }
        } else if (key == "path") {
            if (!grepl("^/", val, perl = TRUE)) {
                return(FALSE)
            }
        } else if (key == "lines") {
            if (!grepl("^[0-9]+(-[0-9]+)?$", val, perl = TRUE)) {
                return(FALSE)
            }
        } else if (key == "origin") {
            if (!grepl("^[a-zA-Z][a-zA-Z0-9+.-]*:.+", val, perl = TRUE)) {
                return(FALSE)
            }
        }
    }

    TRUE
}


#' Canonicalize compact SWHID strings
#'
#' @description
#' Lowercases the core identifier and embedded visit/anchor qualifier cores.
#' The input must already be whitespace-free.
#'
#' @param x A character vector of compact SWHID strings.
#'
#' @return A character vector of canonical SWHID strings.
#'
#' @noRd
.canonicalize_swhid <- function(x) {
    parts <- .swhid_split(x)
    core <- tolower(parts$core)
    out <- core
    has_q <- !is.na(parts$qualifiers) & nzchar(parts$qualifiers)
    if (!any(has_q)) {
        return(out)
    }

    idx <- which(has_q)
    out[idx] <- vapply(idx, function(i) {
        qual_parts <- strsplit(
            parts$qualifiers[[i]],
            ";",
            fixed = TRUE
        )[[1]]
        qual_parts <- vapply(qual_parts, function(part) {
            if (grepl("^(visit|anchor)=", part, perl = TRUE)) {
                prefix <- sub("=.*$", "=", part)
                paste0(
                    prefix,
                    tolower(sub("^[^=]+=", "", part))
                )
            } else {
                part
            }
        }, character(1))
        paste0(core[[i]], ";", paste(qual_parts, collapse = ";"))
    }, character(1), USE.NAMES = FALSE)
    out
}


#' Strict SWHID validator
#'
#' @description
#' Validates canonical `swh:` identifiers. The core must use lowercase hex,
#' scheme version `1`, and a known object type. Optional qualifiers must use
#' known keys and pass conservative value checks. Bare 40-character hex strings
#' without the `swh:` prefix are rejected.
#'
#' @param x A character vector in canonical form.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.is_swhid_strict <- function(x) {
    res <- rep(FALSE, length(x))
    ok <- !is.na(x) & nzchar(x)
    if (!any(ok)) {
        return(res)
    }

    y <- gsub("[[:space:]]+", "", trimws(x[ok]))
    has <- grepl("^swh:", y)
    good <- rep(FALSE, length(y))
    if (any(has)) {
        parts <- .swhid_split(y[has])
        core_ok <- grepl(.swhid_core_pat(), parts$core, perl = TRUE)
        qual_ok <- core_ok
        need <- core_ok & nzchar(parts$qualifiers)
        if (any(need)) {
            qual_ok[need] <- vapply(
                parts$qualifiers[need],
                .is_swhid_qualifiers_valid,
                logical(1)
            )
        }
        good[has] <- qual_ok
    }
    res[which(ok)] <- good
    res
}


#' Check whether ISBN strings have an acceptable input format
#'
#' @description
#' Returns `TRUE` for compact ISBN-10 and ISBN-13 strings, and for grouped
#' forms that use single spaces or hyphens in acceptable positions.
#'
#' This check validates input formatting only. It does not verify the ISBN
#' checksum.
#'
#' @param x A character vector of candidate ISBN strings.
#'
#' @return A logical vector the same length as `x`.
#'
#' @noRd
.isbn_format_ok <- function(x) {
    res <- rep(FALSE, length(x))
    ok <- !is.na(x) & nzchar(x)
    if (!any(ok)) {
        return(res)
    }

    y <- x[ok]
    compact_form <- grepl("^\\d{9}[0-9Xx]$", y) | grepl("^\\d{13}$", y)
    out <- compact_form
    rest <- !compact_form
    if (any(rest)) {
        z <- y[rest]
        keep <- grepl("^[0-9Xx -]+$", z) &
            !grepl("(^[- ]|[- ]$|[- ]{2,}|[- ]{2,})", z)
        grouped <- rep(FALSE, length(z))
        if (any(keep)) {
            w <- z[keep]
            body <- gsub("[- ]", "", w)
            n <- nchar(body)
            is10 <- n == 10L & grepl(
                "^[0-9]+([ -][0-9]+){2}[ -][0-9Xx]$",
                w
            )
            is13 <- n == 13L & grepl("^97[89]([ -][0-9]+){4}$", w)
            grouped[keep] <- is10 | is13
        }
        out[rest] <- grouped
    }

    res[which(ok)] <- out
    res
}


#' Validate ISBN-10 checksums
#'
#' @param compact A character vector of compact uppercase ISBN strings.
#'
#' @return A logical vector the same length as `compact`.
#'
#' @noRd
.isbn10_valid <- function(compact) {
    res <- rep(FALSE, length(compact))
    ok <- !is.na(compact) & grepl("^\\d{9}[0-9X]$", compact)
    if (!any(ok)) {
        return(res)
    }

    y <- compact[ok]
    acc <- rep(0, length(y))
    w <- 10:2
    for (i in seq_len(9L)) {
        acc <- acc + as.integer(substr(y, i, i)) * w[[i]]
    }
    cdn <- (11 - (acc %% 11)) %% 11
    cd <- ifelse(cdn == 10, "X", as.character(cdn))
    res[which(ok)] <- cd == substr(y, 10L, 10L)
    res
}


#' Validate ISBN-13 checksums
#'
#' @param compact A character vector of compact ISBN-13 strings.
#'
#' @return A logical vector the same length as `compact`.
#'
#' @noRd
.isbn13_valid <- function(compact) {
    res <- rep(FALSE, length(compact))
    ok <- !is.na(compact) & grepl("^\\d{13}$", compact)
    if (!any(ok)) {
        return(res)
    }

    y <- compact[ok]
    acc <- rep(0, length(y))
    w <- rep(c(1, 3), 6)
    for (i in seq_len(12L)) {
        acc <- acc + as.integer(substr(y, i, i)) * w[[i]]
    }
    cd <- (10 - (acc %% 10)) %% 10
    res[which(ok)] <- cd == as.integer(substr(y, 13L, 13L))
    res
}
