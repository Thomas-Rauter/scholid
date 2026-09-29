#' Write scholarly identifiers as resolver URLs, CURIEs, or DOIs
#'
#' @description
#' Vectorized writer that gives identifiers in a form for links, reports,
#' and linked data:
#'
#' - `"url"`: the resolver URL, such as `https://doi.org/10.1000/182`.
#' - `"curie"`: the CURIE, the type's Bioregistry prefix, a colon, and the
#'   identifier, such as `pubmed:12345678`. For types whose canonical form
#'   already is a CURIE, it is that form.
#' - `"doi"`: for arXiv identifiers, the DOI that arXiv registers, such as
#'   `10.48550/arXiv.2101.00001`. The DOI names the preprint, not a
#'   version, so the version is left out, and so is the subject class of
#'   old-style identifiers.
#'
#' `x` is first normalized with the rules of [normalize_scholid()], so
#' wrapped input such as URLs and labels works. The output is plain text:
#' identifiers are written into URLs as they are, except for characters
#' that the resolver requires to be percent-encoded, such as `#` in a DOI.
#'
#' Which types have which forms, and the sources of the forms, are in
#' "Resolver URLs and CURIEs" and each type's section of the *How
#' Scholarly Identifiers Are Defined* vignette
#' (`vignette("scholid_definitions", package = "scholid")`).
#'
#' What `format_scholid()` writes, [normalize_scholid()] reads back:
#' `normalize_scholid(format_scholid(x, type, as), type)` equals
#' `normalize_scholid(x, type)` for `"url"` and `"curie"`. For `"doi"`,
#' `normalize_scholid(format_scholid(x, "arxiv", "doi"), "arxiv")` is the
#' arXiv identifier without its version.
#'
#' @param x A vector of values to write.
#' @param type A single string giving the identifier type. See
#'   [scholid_types()] for supported values.
#' @param as A single string giving the form: `"url"` (the default),
#'   `"curie"`, or `"doi"`. Matched with [match.arg()], so it can be
#'   abbreviated.
#'
#' @return A character vector with the same length as `x`. Values that are
#'   `NA` or don't normalize yield `NA_character_`. If `type` has no form
#'   `as`, such as a URL for an ISBN, `format_scholid()` stops with an error
#'   that names the forms `type` has.
#'
#' @examples
#' format_scholid("https://doi.org/10.1000/182", "doi")
#' format_scholid(c("PMID: 12345678", "not a PMID", NA), "pmid")
#' format_scholid("0000-0002-1825-0097", "orcid", as = "curie")
#' format_scholid("arXiv:2101.00001v1", "arxiv", as = "doi")
#'
#' # normalize_scholid() reads the output back
#' url <- format_scholid("P12345", "uniprot")
#' normalize_scholid(url, "uniprot")
#'
#' @seealso [normalize_scholid()], [scholid_key()], [scholid_types()]
#' @export
format_scholid <- function(
        x,
        type,
        as = c("url", "curie", "doi")
) {
    .scholid_check_x(
        x,
        arg = "x"
    )
    type <- .scholid_match_type(type)
    as <- match.arg(as)
    .scholid_check_form(
        type = type,
        as   = as
    )

    normalized <- .scholid_dispatch(
        type   = type,
        prefix = "normalize_",
        x      = x
    )
    switch(
        as,
        url   = .scholid_format_url(
            normalized,
            type = type
        ),
        curie = .scholid_format_curie(
            normalized,
            type = type
        ),
        # .scholid_check_form() lets only arXiv through: the registry test
        # pins it as the only type with a `doi` template.
        doi   = .arxiv_format_doi(normalized)
    )
}


# Level 1 function (functions called by exported functions) definitions --------


#' Stop if a type has no form
#'
#' @param type A validated identifier type string.
#' @param as A form: `"url"`, `"curie"`, or `"doi"`.
#'
#' @return Invisibly returns `TRUE` if `type` has the form `as`.
#'
#' @noRd
.scholid_check_form <- function(
        type,
        as
) {
    forms <- .scholid_forms(type)
    if (!as %in% forms) {
        stop(
            "Type \"", type, "\" has no \"", as, "\" form. ",
            "Available forms for \"", type, "\": ",
            paste0("\"", forms, "\"", collapse = ", "),
            ".",
            call. = FALSE
        )
    }

    invisible(TRUE)
}


#' Write normalized identifiers as resolver URLs
#'
#' @description
#' Fills the registry's `url` template with each identifier. With several
#' templates, each identifier uses the first one whose name matches it or
#' that has no name. The characters in the registry's `url_escape` are
#' percent-encoded first.
#'
#' @param x A character vector of normalized identifiers.
#' @param type A validated identifier type with a `url` template.
#'
#' @return A character vector the same length as `x`.
#'
#' @noRd
.scholid_format_url <- function(
        x,
        type
) {
    entry <- .scholid_registry()[[type]]
    templates <- entry$url
    keys <- names(templates)
    if (is.null(keys)) {
        keys <- rep("", length(templates))
    }

    id <- .scholid_percent_encode(
        x,
        chars = entry$url_escape
    )
    out <- rep(NA_character_, length(x))
    todo <- !is.na(x)
    for (i in seq_along(templates)) {
        hit <- todo
        if (nzchar(keys[[i]])) {
            hit[todo] <- grepl(keys[[i]], x[todo], perl = TRUE)
        }
        out[hit] <- .scholid_fill_template(
            templates[[i]],
            id = id[hit]
        )
        todo <- todo & !hit
    }
    out
}


#' Write normalized identifiers as CURIEs
#'
#' @description
#' Puts the registry's `curie` prefix and a colon in front of each
#' identifier. For types with `curie_is_id`, the canonical form is the
#' CURIE and is returned as it is.
#'
#' @param x A character vector of normalized identifiers.
#' @param type A validated identifier type with a `curie` prefix.
#'
#' @return A character vector the same length as `x`.
#'
#' @noRd
.scholid_format_curie <- function(
        x,
        type
) {
    entry <- .scholid_registry()[[type]]
    if (isTRUE(entry$curie_is_id)) {
        return(x)
    }

    out <- paste0(entry$curie, ":", x)
    out[is.na(x)] <- NA_character_
    out
}


#' Write normalized arXiv identifiers as the DOIs that arXiv registers
#'
#' @description
#' arXiv registers one DOI per article, whose identifier has no version
#' and, if old-style, no subject class. `.arxiv_from_doi()` reads these
#' DOIs.
#'
#' @param x A character vector of normalized arXiv identifiers.
#'
#' @return A character vector the same length as `x`.
#'
#' @noRd
.arxiv_format_doi <- function(x) {
    id <- .scholid_drop_version(
        x,
        type = "arxiv"
    )
    id <- sub("^([^/.]+)\\.[A-Z]{2}/", "\\1/", id, perl = TRUE)
    .scholid_fill_template(
        .scholid_registry()[["arxiv"]]$doi,
        id = id
    )
}


# Level 2 functions (functions called by level 1 functions) definitions --------


#' Forms a type can be written in
#'
#' @description
#' `"url"` if the registry has a `url` template, `"curie"` if it has a
#' `curie` prefix, and `"doi"` if it has a `doi` template.
#'
#' @param type A validated identifier type string.
#'
#' @return A character vector, a subset of `c("url", "curie", "doi")` in
#'   that order.
#'
#' @noRd
.scholid_forms <- function(type) {
    entry <- .scholid_registry()[[type]]
    c("url", "curie", "doi")[c(
        length(entry$url) > 0L,
        !is.na(entry$curie),
        !is.null(entry$doi)
    )]
}


#' Fill a registry template with identifiers
#'
#' @param template A single template string from the registry.
#' @param id A character vector of identifiers.
#'
#' @return A character vector the same length as `id`, `NA` where `id` is
#'   `NA`.
#'
#' @noRd
.scholid_fill_template <- function(
        template,
        id
) {
    parts <- .scholid_split_template(template)
    out <- paste0(parts[["head"]], id, parts[["tail"]])
    out[is.na(id)] <- NA_character_
    out
}


#' Percent-encode the given characters
#'
#' @description
#' `%` is encoded first, so that the escapes for the other characters are
#' not encoded again.
#'
#' @param x A character vector.
#' @param chars A character vector of single ASCII characters, or `NULL`.
#'
#' @return A character vector the same length as `x`.
#'
#' @noRd
.scholid_percent_encode <- function(
        x,
        chars
) {
    chars <- c(chars[chars == "%"], chars[chars != "%"])
    for (ch in chars) {
        x <- gsub(
            ch,
            sprintf("%%%02X", utf8ToInt(ch)),
            x,
            fixed = TRUE
        )
    }
    x
}
