testthat::test_that(
    "is_scholid dispatches to is_<type>()",
    {
        x <- c(
            "10.1000/182",
            "not a doi",
            NA_character_
        )

        testthat::expect_identical(
            is_scholid(
                x,
                "doi"
            ),
            is_doi(x)
        )
    }
)

testthat::test_that(
    "is_scholid is vectorized and preserves NA",
    {
        x <- c(
            NA_character_,
            "10.1000/182"
        )

        out <- is_scholid(
            x,
            "doi"
        )

        testthat::expect_type(
            out,
            "logical"
        )
        testthat::expect_length(
            out,
            length(x)
        )
        testthat::expect_true(is.na(out[1]))
    }
)

testthat::test_that(
    "is_scholid validates `type` strictly",
    {
        testthat::expect_error(
            is_scholid(
                "x",
                NA_character_
            ),
            "`type` must be a non-empty string"
        )

        testthat::expect_error(
            is_scholid(
                "x",
                ""
            ),
            "`type` must be a non-empty string"
        )

        testthat::expect_error(
            is_scholid(
                "x",
                "not_a_type"
            ),
            "should be one of"
        )

        if (all(c("pmid", "pmcid") %in% scholid_types())) {
            testthat::expect_error(
                is_scholid(
                    "x",
                    "pmi"
                ),
                "abbreviations are not allowed"
            )
        }
    }
)

testthat::test_that(
    "is_scholid validates `x`",
    {
        testthat::expect_error(
            is_scholid(
                type = "doi"
            ),
            "`x` is required"
        )

        testthat::expect_error(
            is_scholid(
                NULL,
                "doi"
            ),
            "`x` must not be NULL"
        )

        testthat::expect_error(
            is_scholid(
                data.frame(x = 1),
                "doi"
            ),
            "data frame"
        )
    }
)

testthat::test_that(
    "is_doi accepts doi syntax and rejects spaces",
    {
        x <- c(
            "10.1000/182",
            "10.1000/with space",
            "10.1000",
            NA_character_
        )

        got <- is_scholid(
            x,
            "doi"
        )

        testthat::expect_identical(
            got,
            c(TRUE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_orcid validates checksum and allows X check digit",
    {
        x <- c(
            "0000-0002-1825-0097",
            "0000-0002-1694-233X",
            "0000-0002-1825-0098",
            "0000-0002-1825-009",
            NA_character_
        )

        got <- is_scholid(
            x,
            "orcid"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_isbn validates ISBN-10 and ISBN-13 checksums",
    {
        x <- c(
            "0-306-40615-2",
            "978-0-306-40615-7",
            "0-306-40615-3",
            "978-0-306-40615-8",
            "not an isbn",
            NA_character_
        )

        got <- is_scholid(
            x,
            "isbn"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, FALSE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_issn validates checksum and rejects malformed inputs",
    {
        x <- c(
            "0317-8471",
            "2434-561X",
            "0317-8472",
            "0317-847",
            NA_character_
        )

        got <- is_scholid(
            x,
            "issn"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_arxiv accepts modern and legacy formats with optional version",
    {
        x <- c(
            "2101.00001v2",
            "2101.00001",
            "hep-th/9901001v2",
            "hep-th/9901001",
            "21.00001",
            "hep-th/990100",
            NA_character_
        )

        got <- is_scholid(
            x,
            "arxiv"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_pmid accepts digits only",
    {
        x <- c(
            "1234567",
            "012345",
            "12a3",
            "PMC12345",
            NA_character_
        )

        got <- is_scholid(
            x,
            "pmid"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_pmcid accepts PMC prefix and digits only",
    {
        x <- c(
            "PMC12345",
            "PMC012345",
            "pmc12345",
            "12345",
            NA_character_
        )

        got <- is_scholid(
            x,
            "pmcid"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_scholid works across multiple ID types",
    {
        x <- c(
            "10.1000/182",
            "0000-0002-1825-0097",
            "0-306-40615-2",
            "0317-8471",
            "2101.00001v2",
            "1234567",
            "PMC12345",
            NA_character_
        )

        got_doi <- is_scholid(
            x,
            "doi"
        )
        got_orc <- is_scholid(
            x,
            "orcid"
        )
        got_isb <- is_scholid(
            x,
            "isbn"
        )
        got_isn <- is_scholid(
            x,
            "issn"
        )
        got_arx <- is_scholid(
            x,
            "arxiv"
        )
        got_pmi <- is_scholid(
            x,
            "pmid"
        )
        got_pmc <- is_scholid(
            x,
            "pmcid"
        )

        testthat::expect_true(got_doi[1])
        testthat::expect_true(got_orc[2])
        testthat::expect_true(got_isb[3])
        testthat::expect_true(got_isn[4])
        testthat::expect_true(got_arx[5])
        testthat::expect_true(got_pmi[6])
        testthat::expect_true(got_pmc[7])

        testthat::expect_true(is.na(got_doi[8]))
        testthat::expect_true(is.na(got_orc[8]))
        testthat::expect_true(is.na(got_isb[8]))
        testthat::expect_true(is.na(got_isn[8]))
        testthat::expect_true(is.na(got_arx[8]))
        testthat::expect_true(is.na(got_pmi[8]))
        testthat::expect_true(is.na(got_pmc[8]))
    }
)

testthat::test_that(
    "is_orcid returns FALSE for pattern mismatches and checksum failures",
    {
        x <- c(
            "0000-0002-1825-0097",
            "0000-0002-1825-0098",
            "0000-0002-1825-009",
            "abcd-0002-1825-0097",
            NA_character_
        )

        got <- is_scholid(
            x,
            "orcid"
        )

        testthat::expect_identical(
            got,
            c(TRUE, FALSE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_isbn hits is10/is13 regex-fail branches and checksum branches",
    {
        x <- c(
            "0-306-40615-2",
            "978-0-306-40615-7",
            "030640615",
            "978030640615",
            "97803064061570",
            "0-306-40615-3",
            "978-0-306-40615-8",
            NA_character_
        )

        got <- is_scholid(
            x,
            "isbn"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_issn rejects pattern mismatches and checksum failures",
    {
        x <- c(
            "0317-8471",
            "0317-8472",
            "03178471",
            "0317-84A1",
            NA_character_
        )

        got <- is_scholid(
            x,
            "issn"
        )

        testthat::expect_identical(
            got,
            c(TRUE, FALSE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_arxiv rejects malformed ids for both modern and legacy formats",
    {
        x <- c(
            "2101.00001v2",
            "hep-th/9901001v2",
            "2101.000",
            "hep-th/990100",
            "HEP-TH/9901001",
            NA_character_
        )

        got <- is_scholid(
            x,
            "arxiv"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, FALSE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_scholid coerces numeric input and preserves NA",
    {
        x <- c(
            1234567,
            NA_real_
        )

        got <- is_scholid(
            x,
            "pmid"
        )

        testthat::expect_identical(
            got,
            c(TRUE, NA)
        )
    }
)

testthat::test_that(
    "is_pmid and is_pmcid reject near-misses",
    {
        x_pmid <- c(
            "1234",
            "12a3",
            " 1234",
            NA_character_
        )

        got_pmid <- is_scholid(
            x_pmid,
            "pmid"
        )

        testthat::expect_identical(
            got_pmid,
            c(TRUE, FALSE, FALSE, NA)
        )

        x_pmc <- c(
            "PMC123",
            "PMC",
            "pmc123",
            NA_character_
        )

        got_pmc <- is_scholid(
            x_pmc,
            "pmcid"
        )

        testthat::expect_identical(
            got_pmc,
            c(TRUE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_issn accepts check digit 0 branch",
    {
        x <- c(
            "0000-0000",
            NA_character_
        )

        got <- is_scholid(
            x,
            "issn"
        )

        testthat::expect_identical(
            got,
            c(TRUE, NA)
        )
    }
)

testthat::test_that(
    "is_isbn uppercases x and validates ISBN-10 with X check digit",
    {
        x <- c(
            "0-8044-2957-x",
            "0-8044-2957-X",
            NA_character_
        )

        got <- is_scholid(
            x,
            "isbn"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, NA)
        )
    }
)

