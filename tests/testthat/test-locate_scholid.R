locate_cols <- c("element", "type", "id", "match", "start", "end")
locate_col_types <- c(
    "integer",
    "character",
    "character",
    "character",
    "integer",
    "integer"
)

locate_texts <- c(
    scholid_extract_texts,
    unlist(scholid_type_inputs, use.names = FALSE)
)

# Checks the rows locate_scholid() returns for one text element.
expect_kept <- function(
        text,
        type,
        match,
        start,
        end,
        id    = match,
        types = scholid_types()
) {
    got <- locate_scholid(
        text,
        types = types
    )
    info <- encodeString(text)
    testthat::expect_identical(got$type, type, info = info)
    testthat::expect_identical(got$id, id, info = info)
    testthat::expect_true(all(got$match == match), info = info)
    testthat::expect_identical(got$start, as.integer(start), info = info)
    testthat::expect_identical(got$end, as.integer(end), info = info)
    testthat::expect_identical(
        substr(text[got$element], got$start, got$end),
        got$match,
        info = info
    )
}

# Checks that in one text, each hit of `loser` overlaps the one hit of
# `winner`, and that only the `winner` hit is kept when both types take
# part.
expect_overlap_winner <- function(
        text,
        winner,
        loser
) {
    info <- paste(winner, loser, encodeString(text))
    won <- locate_scholid(
        text,
        types = winner
    )
    lost <- locate_scholid(
        text,
        types = loser
    )
    testthat::expect_identical(nrow(won), 1L, info = info)
    testthat::expect_true(nrow(lost) >= 1L, info = info)
    testthat::expect_true(
        all(lost$start <= won$end & won$start <= lost$end),
        info = info
    )

    both <- locate_scholid(
        text,
        types = c(winner, loser)
    )
    testthat::expect_identical(both, won, info = info)
}

testthat::test_that(
    "locate_scholid returns the documented columns",
    {
        got <- locate_scholid(c(
            NA_character_,
            "",
            "See doi:10.1000/182 and PMID: 12345678."
        ))

        testthat::expect_s3_class(got, "data.frame")
        testthat::expect_identical(names(got), locate_cols)
        testthat::expect_identical(
            unname(vapply(got, typeof, character(1))),
            locate_col_types
        )
        testthat::expect_identical(got$element, c(3L, 3L))
        testthat::expect_identical(got$type, c("doi", "pmid"))
        testthat::expect_identical(got$id, c("10.1000/182", "12345678"))
        testthat::expect_identical(got$match, c("10.1000/182", "12345678"))
        testthat::expect_identical(got$start, c(9L, 31L))
        testthat::expect_identical(got$end, c(19L, 38L))
    }
)

testthat::test_that(
    "locate_scholid returns zero rows with the same columns",
    {
        for (text in list(
            character(0),
            list(),
            NA_character_,
            c(NA_character_, ""),
            "No identifier in this sentence."
        )) {
            got <- locate_scholid(text)
            testthat::expect_s3_class(got, "data.frame")
            testthat::expect_identical(nrow(got), 0L)
            testthat::expect_identical(names(got), locate_cols)
            testthat::expect_identical(
                unname(vapply(got, typeof, character(1))),
                locate_col_types
            )
        }

        got <- locate_scholid(
            "PMID: 12345678",
            types = "doi"
        )
        testthat::expect_identical(nrow(got), 0L)
        testthat::expect_identical(names(got), locate_cols)
    }
)

testthat::test_that(
    "locate_scholid gives no rows for NA elements",
    {
        got <- locate_scholid(c(
            NA_character_,
            "PMID: 12345678",
            NA_character_,
            "PMID: 7654321"
        ))

        testthat::expect_identical(got$element, c(2L, 4L))
        testthat::expect_identical(got$id, c("12345678", "7654321"))
    }
)

testthat::test_that(
    "locate_scholid sorts rows by element, then start, and numbers them",
    {
        got <- locate_scholid(c(
            "PMID 12345678, then doi:10.1000/182 and RRID:AB_262044",
            "ORCID 0000-0002-1825-0097 before arXiv:2101.00001"
        ))

        testthat::expect_identical(got$element, c(1L, 1L, 1L, 2L, 2L))
        testthat::expect_identical(
            got$type,
            c("pmid", "doi", "rrid", "orcid", "arxiv")
        )
        testthat::expect_identical(got$start, c(6L, 25L, 41L, 7L, 40L))
        testthat::expect_identical(
            rownames(got),
            as.character(seq_len(nrow(got)))
        )

        all <- locate_scholid(locate_texts)
        testthat::expect_false(is.unsorted(all$element))
        for (e in unique(all$element)) {
            testthat::expect_false(
                is.unsorted(all$start[all$element == e], strictly = TRUE)
            )
        }
        testthat::expect_identical(
            rownames(all),
            as.character(seq_len(nrow(all)))
        )
    }
)

testthat::test_that(
    "locate_scholid spans recover the match from the text",
    {
        got <- locate_scholid(locate_texts)

        testthat::expect_true(nrow(got) > 0L)
        testthat::expect_identical(
            substr(locate_texts[got$element], got$start, got$end),
            got$match
        )
        testthat::expect_true(all(got$start >= 1L & got$start <= got$end))
    }
)

testthat::test_that(
    "locate_scholid with one type returns what extract_scholid returns",
    {
        for (t in scholid_types()) {
            got <- locate_scholid(
                locate_texts,
                types = t
            )
            testthat::expect_true(all(got$type == t), info = t)

            ids <- split(
                got$id,
                factor(got$element, levels = seq_along(locate_texts))
            )
            names(ids) <- NULL
            testthat::expect_identical(
                ids,
                extract_scholid(locate_texts, t),
                info = t
            )
        }
    }
)

testthat::test_that(
    "locate_scholid returns extract_scholid tokens where no spans overlap",
    {
        one <- do.call(rbind, lapply(
            scholid_types(),
            function(t) locate_scholid(locate_texts, types = t)
        ))

        overlapping <- integer()
        for (d in split(one, one$element)) {
            if (nrow(d) < 2L) {
                next
            }
            ov <- outer(d$start, d$end, "<=") & t(outer(d$start, d$end, "<="))
            diag(ov) <- FALSE
            if (any(ov)) {
                overlapping <- c(overlapping, d$element[[1L]])
            }
        }
        testthat::expect_true(length(overlapping) > 0L)

        text <- locate_texts[-overlapping]
        got <- locate_scholid(text)
        for (t in scholid_types()) {
            of_type <- got[got$type == t, ]
            ids <- split(
                of_type$id,
                factor(of_type$element, levels = seq_along(text))
            )
            names(ids) <- NULL
            testthat::expect_identical(
                ids,
                extract_scholid(text, t),
                info = t
            )
        }
    }
)

testthat::test_that(
    "locate_scholid keeps hits as a one-at-a-time greedy pass would",
    {
        # Inputs that overlap each other when joined, plus plain ones.
        parts <- c(
            "10.1000/182",
            "10.5555/12345678",
            "10.1000/182;:",
            "978 0 306 40615 7",
            "0306406152",
            "0000 0001 2146 438X",
            "12345678",
            "03178471",
            "0000‐0002‐1825‐0097",
            "0000-0002-1825-0097",
            "RRID:MGI:3840442",
            "RRID: AB_262044",
            "RRID: Addgene_80088",
            "0317-8471",
            "P12345",
            "GSE2553",
            "PMC1234567",
            "2101.00001"
        )
        ij <- expand.grid(
            i = seq_along(parts),
            j = seq_along(parts)
        )
        text <- c(
            paste(parts[ij$i], parts[ij$j]),
            paste0(parts[ij$i], ",", parts[ij$j]),
            paste0("(", parts[ij$i], ") [", parts[ij$j], "]"),
            paste(parts[ij$i], parts[ij$j], parts[rev(ij$i)])
        )

        # Reference: per element, hits longest first, then by type order,
        # each kept unless it overlaps one already kept.
        types <- scholid_types()
        hits <- do.call(rbind, lapply(
            types,
            function(t) locate_scholid(text, types = t)
        ))
        len <- hits$end - hits$start + 1L
        rank <- match(hits$type, types)
        keep <- rep(FALSE, nrow(hits))
        for (e in unique(hits$element)) {
            ix <- which(hits$element == e)
            ix <- ix[order(-len[ix], rank[ix], hits$start[ix])]
            kept <- integer()
            for (i in ix) {
                clash <- hits$start[kept] <= hits$end[i] &
                    hits$start[i] <= hits$end[kept]
                if (!any(clash)) {
                    kept <- c(kept, i)
                }
            }
            keep[kept] <- TRUE
        }
        want <- hits[keep, ]
        want <- want[order(want$element, want$start), ]
        rownames(want) <- NULL

        testthat::expect_true(sum(!keep) > 0L)
        testthat::expect_identical(locate_scholid(text), want)
    }
)

testthat::test_that(
    "locate_scholid keeps a DOI over identifiers inside its suffix",
    {
        expect_overlap_winner(
            "10.1000/182,ark:/13030/654xz321",
            "doi",
            "ark"
        )
        expect_overlap_winner("10.1000/182,2101.00001", "doi", "arxiv")
        expect_overlap_winner(
            "10.1000/182,GCA_009914755.4",
            "doi",
            "assembly"
        )
        expect_overlap_winner(
            "10.1000/182,1992ApJ...400L...1W",
            "doi",
            "bibcode"
        )
        expect_overlap_winner("10.1000/182,PRJDB303", "doi", "bioproject")
        expect_overlap_winner("10.1000/GSE2553", "doi", "geo")
        expect_overlap_winner("10.1007/978-0-306-40615-7", "doi", "isbn")
        expect_overlap_winner(
            "10.1000/182,000000012146438X",
            "doi",
            "isni"
        )
        expect_overlap_winner("10.1051/0004-6361/201936345", "doi", "issn")
        expect_overlap_winner("10.1000/182,W2741809807", "doi", "openalex")
        expect_overlap_winner(
            "10.1000/182,0000-0002-1825-0097",
            "doi",
            "orcid"
        )
        expect_overlap_winner("10.1000/182,PMC1234567", "doi", "pmcid")
        expect_overlap_winner("10.1000/182,12345678", "doi", "pmid")
        expect_overlap_winner("10.1000/182,NM_001744.6", "doi", "refseq")
        expect_overlap_winner("10.1000/182,01an7q238", "doi", "ror")
        expect_overlap_winner("10.1000/182,RRID:AB_262044", "doi", "rrid")
        expect_overlap_winner("10.1000/182,SRR1553610", "doi", "sra")
        expect_overlap_winner(
            paste0(
                "10.1000/182,",
                "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2"
            ),
            "doi",
            "swhid"
        )
        expect_overlap_winner("10.1000/P12345", "doi", "uniprot")
    }
)

testthat::test_that(
    "locate_scholid keeps an identifier over the PMID digits inside it",
    {
        expect_overlap_winner("ISBN 978 0 306 40615 7", "isbn", "pmid")
        expect_kept(
            "ISBN 978 0 306 40615 7",
            "isbn",
            "978 0 306 40615 7",
            6,
            22
        )
        expect_kept(
            "ISBN 978 0 306 40615 7",
            "pmid",
            "40615",
            16,
            20,
            types = "pmid"
        )

        expect_overlap_winner("ISNI 0000 0001 2146 438X", "isni", "pmid")
        expect_overlap_winner(
            "ORCID 0000‐0002‐1825‐0097",
            "orcid",
            "pmid"
        )
        expect_overlap_winner("RRID:MGI:3840442", "rrid", "pmid")
    }
)

testthat::test_that(
    "locate_scholid keeps the longer span where spans overlap partly",
    {
        # These partial overlaps come from hits that run into the next
        # identifier: a DOI takes ",978" or ",RRID", and a checksum-valid
        # ISNI starts inside a DOI or an ISSN.
        expect_overlap_winner("10.1000/182,978 0 306 40615 7", "isbn", "doi")
        expect_overlap_winner("10.5555/12345678 03178471", "isni", "doi")
        expect_overlap_winner("10.1000/182,RRID: AB_262044", "doi", "rrid")
        expect_overlap_winner(
            "10.1000/182,RRID: Addgene_80088",
            "rrid",
            "doi"
        )
        expect_overlap_winner(
            "0317-8471 0000 0001 2146 438X",
            "isni",
            "issn"
        )
    }
)

testthat::test_that(
    "locate_scholid keeps the earlier type where spans are equally long",
    {
        # Both spans are 17 characters long.
        expect_overlap_winner(
            "10.1000/182;:,978 0 306 40615 7",
            "doi",
            "isbn"
        )
    }
)

testthat::test_that(
    "locate_scholid keeps a hit that overlaps only dropped hits",
    {
        # The DOI beats the ISBN, which no longer hides the PMID.
        expect_kept(
            "10.5555/12345678,978 0 306 40615 7",
            c("doi", "pmid"),
            c("10.5555/12345678,978", "40615"),
            c(1, 28),
            c(20, 32)
        )
    }
)

testthat::test_that(
    ".locate_resolve_overlaps applies the rule to spans",
    {
        testthat::expect_identical(
            .locate_resolve_overlaps(
                element = integer(),
                start   = integer(),
                end     = integer(),
                rank    = integer()
            ),
            logical()
        )

        # Element 1: a chain; element 2: equal spans; element 3: spans
        # that share one position; element 4: adjacent spans; element 5:
        # one span inside another, and a later span on its own.
        got <- .locate_resolve_overlaps(
            element = c(1L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 5L, 5L),
            start   = c(1L, 8L, 14L, 1L, 1L, 1L, 5L, 1L, 6L, 1L, 2L, 28L, 40L),
            end     = c(10L, 15L, 20L, 4L, 4L, 5L, 9L, 5L, 9L, 30L, 3L, 35L,
                        45L),
            rank    = c(2L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 3L, 1L, 1L, 2L)
        )
        testthat::expect_identical(
            got,
            c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE,
              TRUE, FALSE, FALSE, TRUE)
        )
    }
)

testthat::test_that(
    "locate_scholid restricts the search to `types`",
    {
        x <- "ISBN 978 0 306 40615 7"
        pmid <- locate_scholid(
            x,
            types = "pmid"
        )
        testthat::expect_identical(pmid$type, "pmid")

        both <- locate_scholid(
            x,
            types = c("pmid", "isbn")
        )
        testthat::expect_identical(both$type, "isbn")

        # Order and duplicates don't matter.
        testthat::expect_identical(
            locate_scholid(x, types = c("isbn", "pmid")),
            both
        )
        testthat::expect_identical(
            locate_scholid(x, types = c("pmid", "isbn", "pmid")),
            both
        )
        testthat::expect_identical(
            locate_scholid(x, types = c("pmid", "pmid")),
            pmid
        )

        # As for `type` elsewhere, factors and padded names are accepted.
        testthat::expect_identical(
            locate_scholid(x, types = factor("pmid")),
            pmid
        )
        testthat::expect_identical(
            locate_scholid(x, types = " pmid "),
            pmid
        )
    }
)

testthat::test_that(
    "locate_scholid validates `types`",
    {
        testthat::expect_error(
            locate_scholid("x", types = NULL),
            "`types` must not be NULL"
        )
        testthat::expect_error(
            locate_scholid("x", types = 1),
            "`types` must be a character vector"
        )
        testthat::expect_error(
            locate_scholid("x", types = list("doi")),
            "`types` must be a character vector"
        )
        testthat::expect_error(
            locate_scholid("x", types = character(0)),
            "`types` must contain at least one type"
        )
        testthat::expect_error(
            locate_scholid("x", types = NA_character_),
            "`types` must not contain NA or empty strings"
        )
        testthat::expect_error(
            locate_scholid("x", types = c("doi", NA)),
            "`types` must not contain NA or empty strings"
        )
        testthat::expect_error(
            locate_scholid("x", types = c("doi", " ")),
            "`types` must not contain NA or empty strings"
        )
        testthat::expect_error(
            locate_scholid("x", types = c("doi", "not_a_type", "nope")),
            "`types` contains unsupported types: \"not_a_type\", \"nope\"",
            fixed = TRUE
        )
        testthat::expect_error(
            locate_scholid("x", types = "or"),
            "unsupported types: \"or\"",
            fixed = TRUE
        )
        testthat::expect_error(
            locate_scholid("x", types = "DOI"),
            "unsupported types: \"DOI\"",
            fixed = TRUE
        )
    }
)

testthat::test_that(
    "locate_scholid validates `text`",
    {
        testthat::expect_error(
            locate_scholid(),
            "`text` is required"
        )
        testthat::expect_error(
            locate_scholid(NULL),
            "`text` must not be NULL"
        )
        testthat::expect_error(
            locate_scholid(data.frame(x = 1)),
            "data frame"
        )
        testthat::expect_error(
            locate_scholid(as.environment(list(a = 1))),
            "atomic vector or list"
        )
    }
)

testthat::test_that(
    "locate_scholid positions count invisible and look-alike characters",
    {
        expect_kept(
            "see PMC123​4567 here",
            "pmcid",
            "PMC123​4567",
            5,
            15,
            id = "PMC1234567"
        )
        expect_kept(
            "ISBN 978–0–306–40615–7.",
            "isbn",
            "978–0–306–40615–7",
            6,
            22,
            id = "978-0-306-40615-7"
        )
        expect_kept(
            paste0("​PMID: ", scholid_fullwidth("12345678"), "."),
            "pmid",
            scholid_fullwidth("12345678"),
            8,
            15,
            id = "12345678"
        )
        expect_kept(
            "ORCID 0000‐0002‐­1825‐0097",
            "orcid",
            "0000‐0002‐­1825‐0097",
            7,
            26,
            id = "0000-0002-1825-0097"
        )
        expect_kept(
            "ISNI 0000 0001 2146 438X",
            "isni",
            "0000 0001 2146 438X",
            6,
            24,
            id = "000000012146438X"
        )
    }
)
