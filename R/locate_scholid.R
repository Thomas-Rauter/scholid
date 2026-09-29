#' Locate scholarly identifiers in text
#'
#' @description
#' Find identifiers of several types in free text at once, and report where
#' each one is. For each type, the identifiers found are those that
#' [extract_scholid()] returns.
#'
#' The result is a data frame with one row per identifier found and these
#' columns:
#'
#' - `element`: the index of the element of `text` it was found in.
#' - `type`: its identifier type.
#' - `id`: the token `extract_scholid()` returns for it.
#' - `match`: the identifier as written in the text. It leaves out a URL or
#'   label in front and the punctuation after it, but keeps a prefix of the
#'   canonical form, such as `RRID:` or `ark:`. It can differ from `id` in
#'   case, in spaces between digit groups, and in look-alike and invisible
#'   characters.
#' - `start`, `end`: the positions of `match` in `text[element]`, counted
#'   as `substr()` counts them, so that
#'   `substr(text[element], start, end) == match`. They include the
#'   invisible characters that matching ignores.
#'
#' Rows are sorted by `element`, then `start`. `NA` elements give no rows.
#' When nothing is found, the result has zero rows and the same columns.
#'
#' The same stretch of text can be found by several types, such as an ISBN
#' and the PMID-like digits inside it. Within one element, when the spans
#' of two hits overlap, only one is kept: the longer span wins, and for
#' equal lengths the type that comes first in [scholid_types()], the order
#' [classify_scholid()] uses. Hits are taken in that order, and each is kept
#' unless it overlaps one already kept. Only the types in `types` take part,
#' so a type left out can't hide another.
#'
#' @param text A character vector of text.
#' @param types A character vector of identifier types to look for. See
#'   [scholid_types()] for supported values. Duplicates and order don't
#'   matter.
#'
#' @return A data frame with one row per identifier found and the columns
#'   `element` (integer), `type`, `id`, `match` (character), `start`, and
#'   `end` (integer).
#'
#' @examples
#' locate_scholid(c(
#'   "See doi:10.1000/182 and ORCID 0000-0002-1825-0097.",
#'   NA,
#'   "PMID: 12345678"
#' ))
#'
#' # The ISBN hides the PMID-like digits inside it ...
#' locate_scholid("ISBN 978 0 306 40615 7")
#' # ... unless ISBN is not looked for.
#' locate_scholid("ISBN 978 0 306 40615 7", types = "pmid")
#'
#' @seealso [extract_scholid()] to extract identifiers of one type,
#'   [scholid_types()]
#' @export
locate_scholid <- function(
        text,
        types = scholid_types()
) {
    .scholid_check_x(
        text,
        arg = "text"
    )
    types <- .scholid_match_types(types)

    hits <- .locate_all_types(
        cache = .scholid_text_cache(text),
        types = types
    )
    keep <- .locate_resolve_overlaps(
        element = hits$element,
        start   = hits$start,
        end     = hits$end,
        rank    = match(hits$type, types)
    )
    hits <- hits[keep, , drop = FALSE]
    rownames(hits) <- NULL
    hits
}


# Level 1 functions (functions called by exported functions) definitions -------


#' Locate identifiers of several types in text
#'
#' @description
#' Internal helper that calls `.scholid_locate_type()` for each type on
#' the same text cache, so the text is cleaned once per folding setting,
#' and binds the hits into one data frame.
#'
#' @param cache A cache from `.scholid_text_cache()`.
#' @param types A character vector of validated identifier types.
#'
#' @return A data frame with the columns of `locate_scholid()`, one row per
#'   hit of every type, sorted by `element`, then `start`.
#'
#' @noRd
.locate_all_types <- function(
        cache,
        types
) {
    found <- lapply(
        types,
        function(type) .scholid_locate_type(
            text = cache,
            type = type
        )
    )
    column <- function(name) {
        unlist(lapply(found, `[[`, name), use.names = FALSE)
    }

    hits <- data.frame(
        element          = column("element"),
        type             = rep(types, vapply(found, nrow, integer(1))),
        id               = column("id"),
        match            = column("match"),
        start            = column("start"),
        end              = column("end"),
        stringsAsFactors = FALSE
    )
    hits[order(hits$element, hits$start), , drop = FALSE]
}


#' Choose which of overlapping hits to keep
#'
#' @description
#' Internal helper for the overlap rule of `locate_scholid()`. Within one
#' element, hits are taken longest span first, then by `rank`, and each is
#' kept unless it overlaps one already kept.
#'
#' Hits that overlap no other are kept outright. The rest fall into groups
#' of hits linked by overlaps. Each round keeps the first remaining hit of
#' every group and drops the hits of the group that overlap it, which gives
#' the same result as taking the hits one at a time.
#'
#' @param element,start,end Integer vectors of hits, sorted by `element`,
#'   then `start`.
#' @param rank Integer vector, the precedence of each hit's type; lower
#'   wins.
#'
#' @return A logical vector, whether to keep each hit.
#'
#' @noRd
.locate_resolve_overlaps <- function(
        element,
        start,
        end,
        rank
) {
    n <- length(element)
    keep <- rep(TRUE, n)
    if (n < 2L) {
        return(keep)
    }

    # reach[i] is the largest end among hits 1..i of the same element. The
    # ranks of end within ascending elements rise from element to element,
    # so their running maximum never looks back into an earlier element.
    by_end <- order(element, end)
    end_rank <- integer(n)
    end_rank[by_end] <- seq_len(n)
    reach <- end[by_end][cummax(end_rank)]

    first <- c(
        TRUE,
        element[-1L] != element[-n] | start[-1L] > reach[-n]
    )
    group <- cumsum(first)
    todo <- which(group %in% group[duplicated(group)])
    todo <- todo[order(start[todo] - end[todo], rank[todo], start[todo])]

    while (length(todo)) {
        again <- duplicated(group[todo])
        best <- todo[!again]
        rest <- todo[again]
        winner <- best[match(group[rest], group[best])]
        clash <- start[rest] <= end[winner] & start[winner] <= end[rest]
        keep[rest[clash]] <- FALSE
        todo <- rest[!clash]
    }

    keep
}
