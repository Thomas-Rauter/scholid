testthat::test_that(
    "extract_scholid dispatches to extract_<type>()",
    {
        txt <- "See https://doi.org/10.1000/182 for details."
        got <- extract_scholid(
            txt,
            "doi"
        )

        testthat::expect_true(is.list(got))
        testthat::expect_length(
            got,
            1L
        )
        testthat::expect_true(is.character(got[[1]]))
        testthat::expect_true(length(got[[1]]) >= 1L)
    }
)

testthat::test_that(
    "extract_scholid is vectorized over text",
    {
        txt <- c(
            "doi:10.1000/182",
            "no identifier here",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "doi"
        )

        testthat::expect_type(
            got,
            "list"
        )
        testthat::expect_length(
            got,
            length(txt)
        )
        testthat::expect_true(is.character(got[[1]]))
        testthat::expect_length(
            got[[2]],
            0L
        )
        testthat::expect_length(
            got[[3]],
            0L
        )
    }
)

testthat::test_that(
    "extract_scholid returns empty vectors for no matches",
    {
        txt <- c(
            "nothing here",
            "still nothing"
        )

        got <- extract_scholid(
            txt,
            "doi"
        )

        testthat::expect_true(
            all(vapply(got, length, integer(1)) == 0L)
        )
    }
)

testthat::test_that(
    "extract_scholid validates `type` strictly",
    {
        testthat::expect_error(
            extract_scholid(
                "x",
                NA_character_
            ),
            "`type` must be a non-empty string"
        )

        testthat::expect_error(
            extract_scholid(
                "x",
                ""
            ),
            "`type` must be a non-empty string"
        )

        testthat::expect_error(
            extract_scholid(
                "x",
                "not_a_type"
            ),
            "should be one of"
        )
    }
)

testthat::test_that(
    "extract_scholid validates `text`",
    {
        testthat::expect_error(
            extract_scholid(
                type = "doi"
            ),
            "`text` is required"
        )

        testthat::expect_error(
            extract_scholid(
                NULL,
                "doi"
            ),
            "`text` must not be NULL"
        )

        testthat::expect_error(
            extract_scholid(
                data.frame(x = 1),
                "doi"
            ),
            "data frame"
        )
    }
)

testthat::test_that(
    "extract_scholid returns exact DOI matches for simple cases",
    {
        txt <- c(
            "doi:10.1000/182",
            "nope",
            NA_character_
        )
        got <- extract_scholid(
            txt,
            "doi"
        )

        testthat::expect_identical(
            got[[1]],
            "10.1000/182"
        )
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
        testthat::expect_identical(
            got[[3]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_doi finds multiple DOIs in one string",
    {
        txt <- "two DOIs: 10.1000/182 and 10.5555/12345678"
        got <- extract_scholid(
            txt,
            "doi"
        )[[1]]

        testthat::expect_true(any(got == "10.1000/182"))
        testthat::expect_true(any(got == "10.5555/12345678"))
    }
)

testthat::test_that(
    "extract_doi stops at quotes and angle brackets",
    {
        txt1 <- 'doi "10.1000/182"'
        txt2 <- "doi <10.1000/182>"
        txt3 <- "doi >10.1000/182<"

        got1 <- extract_scholid(
            txt1,
            "doi"
        )[[1]]
        got2 <- extract_scholid(
            txt2,
            "doi"
        )[[1]]
        got3 <- extract_scholid(
            txt3,
            "doi"
        )[[1]]

        testthat::expect_true(any(got1 == "10.1000/182"))
        testthat::expect_true(any(got2 == "10.1000/182"))
        testthat::expect_true(any(got3 == "10.1000/182"))
    }
)

testthat::test_that(
    "extract_orcid finds ORCIDs and allows X as the last character",
    {
        txt <- c(
            "ORCID 0000-0002-1825-0097",
            "ORCID 0000-0002-1694-233X",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "orcid"
        )

        testthat::expect_true(any(got[[1]] == "0000-0002-1825-0097"))
        testthat::expect_true(any(got[[2]] == "0000-0002-1694-233X"))
        testthat::expect_identical(
            got[[3]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_orcid returns multiple matches from one string",
    {
        txt <- paste(
            "two ORCIDs:",
            "0000-0002-1825-0097",
            "and 0000-0002-1694-233X"
        )

        got <- extract_scholid(
            txt,
            "orcid"
        )[[1]]

        testthat::expect_true(any(got == "0000-0002-1825-0097"))
        testthat::expect_true(any(got == "0000-0002-1694-233X"))
    }
)

testthat::test_that(
    "extract_isbn finds ISBN-10 and ISBN-13 with hyphens/spaces",
    {
        txt <- c(
            "ISBN 0-306-40615-2",
            "ISBN 978 0 306 40615 7",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "isbn"
        )

        testthat::expect_true(length(got[[1]]) >= 1L)
        testthat::expect_true(length(got[[2]]) >= 1L)
        testthat::expect_identical(
            got[[3]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_issn finds ISSN with X and ignores non-matches",
    {
        txt <- c(
            "ISSN 1234-567X",
            "no issn here"
        )

        got <- extract_scholid(
            txt,
            "issn"
        )

        testthat::expect_true(any(got[[1]] == "1234-567X"))
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_arxiv finds modern and legacy arXiv identifiers",
    {
        txt <- c(
            "arXiv:2101.00001v2",
            "legacy hep-th/9901001v2"
        )

        got <- extract_scholid(
            txt,
            "arxiv"
        )

        testthat::expect_true(any(got[[1]] == "2101.00001v2"))
        testthat::expect_true(any(got[[2]] == "hep-th/9901001v2"))
    }
)

testthat::test_that(
    "extract_pmcid extracts PMCID tokens",
    {
        txt <- c(
            "PMCID: PMC12345",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "pmcid"
        )

        testthat::expect_true(any(got[[1]] == "PMC12345"))
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_pmid does not treat PMCID digits as PMID",
    {
        txt <- c(
            "PMC12345 and PMID 1234567",
            "just PMC99999"
        )

        got <- extract_scholid(
            txt,
            "pmid"
        )

        testthat::expect_true(any(got[[1]] == "1234567"))
        testthat::expect_false(any(got[[1]] == "12345"))
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_scholid works across multiple ID types",
    {
        txt <- c(
            "doi:10.1000/182",
            "ORCID 0000-0002-1825-0097",
            "arXiv:2101.00001v2",
            "PMCID: PMC12345",
            "PMID 1234567",
            "ISSN 1234-567X",
            "ISBN 0-306-40615-2"
        )

        got_doi <- extract_scholid(
            txt,
            "doi"
        )
        got_orc <- extract_scholid(
            txt,
            "orcid"
        )
        got_arx <- extract_scholid(
            txt,
            "arxiv"
        )
        got_pmc <- extract_scholid(
            txt,
            "pmcid"
        )
        got_pmi <- extract_scholid(
            txt,
            "pmid"
        )
        got_isn <- extract_scholid(
            txt,
            "issn"
        )
        got_isb <- extract_scholid(
            txt,
            "isbn"
        )

        testthat::expect_true(length(got_doi[[1]]) >= 1L)
        testthat::expect_true(length(got_orc[[2]]) >= 1L)
        testthat::expect_true(length(got_arx[[3]]) >= 1L)
        testthat::expect_true(length(got_pmc[[4]]) >= 1L)
        testthat::expect_true(length(got_pmi[[5]]) >= 1L)
        testthat::expect_true(length(got_isn[[6]]) >= 1L)
        testthat::expect_true(length(got_isb[[7]]) >= 1L)
    }
)

testthat::test_that(
    "extract_pmid does not extract digits from PMC tokens",
    {
        txt <- c(
            "PMC12345 1234567 PMC99999 7654321",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "pmid"
        )

        testthat::expect_identical(
            got[[1]],
            c("1234567", "7654321")
        )
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_issn returns empty when there is no ISSN pattern",
    {
        txt <- c(
            "no issn here",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "issn"
        )

        testthat::expect_identical(
            got[[1]],
            character(0)
        )
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_arxiv returns empty for NA inputs",
    {
        txt <- c(
            NA_character_,
            "arXiv:2101.00001v2"
        )

        got <- extract_scholid(
            txt,
            "arxiv"
        )

        testthat::expect_identical(
            got[[1]],
            character(0)
        )
        testthat::expect_true(length(got[[2]]) >= 1L)
    }
)
