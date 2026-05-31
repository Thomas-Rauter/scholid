testthat::test_that("classify_scholid returns character with input length", {
    x <- c("10.1000/182", NA_character_, "not an id")
    got <- classify_scholid(x)

    testthat::expect_type(got, "character")
    testthat::expect_length(got, length(x))
})

testthat::test_that("classify_scholid returns NA for NA and unknown values", {
    x <- c(NA_character_, " ", "", "not an id")
    got <- classify_scholid(x)

    testthat::expect_true(all(is.na(got)))
})

testthat::test_that("classify_scholid classifies canonical identifiers", {
    x <- c(
        "10.1000/182",
        "0000-0002-1825-0097",
        "PMC12345",
        "12345",
        "2101.00001v2",
        "hep-th/9901001",
        "0317-8471",
        "9780306406157",
        "0306406152"
    )

    got <- classify_scholid(x)

    testthat::expect_equal(got[1], "doi")
    testthat::expect_equal(got[2], "orcid")
    testthat::expect_equal(got[3], "pmcid")
    testthat::expect_equal(got[4], "pmid")
    testthat::expect_equal(got[5], "arxiv")
    testthat::expect_equal(got[6], "arxiv")
    testthat::expect_equal(got[7], "issn")
    testthat::expect_equal(got[8], "isbn")
    testthat::expect_equal(got[9], "isbn")
})

testthat::test_that(
    "classify_scholid classifies canonical ROR iDs",
    {
        x <- c(
            "01an7q238",
            "02mhbdp94",
            "02s376052"
        )

        got <- classify_scholid(x)

        testthat::expect_identical(
            got,
            c("ror", "ror", "ror")
        )
    }
)

testthat::test_that(
    "classify_scholid classifies canonical SWHIDs",
    {
        x <- c(
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d",
            "swh:1:dir:d198bc9d7a6bcf6db04f476d29314f157507d505"
        )

        got <- classify_scholid(x)

        testthat::expect_identical(
            got,
            c("swhid", "swhid", "swhid")
        )
    }
)

testthat::test_that(
    "classify_scholid classifies canonical RRIDs",
    {
        x <- c(
            "RRID:AB_262044",
            "RRID:SCR_007358",
            "RRID:IMSR_JAX:000664"
        )

        got <- classify_scholid(x)

        testthat::expect_identical(
            got,
            c("rrid", "rrid", "rrid")
        )
    }
)

testthat::test_that("classify_scholid does not classify wrapped identifiers", {
    x <- c(
        "https://doi.org/10.1000/182",
        "https://orcid.org/0000-0002-1825-0097",
        "https://ror.org/01an7q238",
        "https://scicrunch.org/resolver/RRID:AB_262044",
        "arXiv:2101.00001",
        "ISSN 0317-8471",
        "PMID: 12345",
        "PMCID: PMC12345",
        "https://archive.softwareheritage.org/swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2"
    )

    got <- classify_scholid(x)

    testthat::expect_true(all(is.na(got)))
})

testthat::test_that("classify_scholid works after per-type normalization", {
    x <- c(
        "https://doi.org/10.1000/182",
        "https://orcid.org/0000-0002-1825-0097",
        "https://ror.org/01an7q238",
        "arXiv:2101.00001v2",
        "ISSN 0317-8471",
        "PMID: 12345",
        "PMCID: PMC12345",
        "0-306-40615-2"
    )

    x[1] <- normalize_doi(x[1])
    x[2] <- normalize_orcid(x[2])
    x[3] <- normalize_ror(x[3])
    x[4] <- normalize_arxiv(x[4])
    x[5] <- normalize_issn(x[5])
    x[6] <- normalize_pmid(x[6])
    x[7] <- normalize_pmcid(x[7])
    x[8] <- normalize_isbn(x[8])

    got <- classify_scholid(x)

    testthat::expect_equal(got[1], "doi")
    testthat::expect_equal(got[2], "orcid")
    testthat::expect_equal(got[3], "ror")
    testthat::expect_equal(got[4], "arxiv")
    testthat::expect_equal(got[5], "issn")
    testthat::expect_equal(got[6], "pmid")
    testthat::expect_equal(got[7], "pmcid")
    testthat::expect_equal(got[8], "isbn")
})

testthat::test_that(
    "classify_scholid works for RRIDs after normalization",
    {
        x <- c(
            "https://scicrunch.org/resolver/RRID:AB_262044",
            "RRID: SCR_007358"
        )

        x <- normalize_rrid(x)
        got <- classify_scholid(x)

        testthat::expect_identical(
            got,
            c("rrid", "rrid")
        )
    }
)

testthat::test_that(
    "classify_scholid works for SWHIDs after normalization",
    {
        x <- c(
            "https://archive.softwareheritage.org/swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "SWH:1:REV:309cf2674ee7a0749978cf8265ab91a60aea0f7d"
        )

        x <- normalize_swhid(x)
        got <- classify_scholid(x)

        testthat::expect_identical(
            got,
            c("swhid", "swhid")
        )
    }
)

testthat::test_that(
    "classify_scholid rejects checksum-invalid ROR iDs",
    {
        x <- c(
            "02mhbdp99",
            "not-a-ror"
        )

        got <- classify_scholid(x)

        testthat::expect_true(all(is.na(got)))
    }
)

testthat::test_that(
    "classify_scholid rejects bare local IDs and unknown RRID authorities",
    {
        x <- c(
            "AB_262044",
            "RRID:UNKNOWN_123",
            "not-a-rrid"
        )

        got <- classify_scholid(x)

        testthat::expect_true(all(is.na(got)))
    }
)

testthat::test_that(
    "classify_scholid rejects bare hex strings and invalid SWHIDs",
    {
        x <- c(
            "94a9ed024d3859793618152ea559a168bbcbb5e2",
            "swh:2:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;unknown=foo"
        )

        got <- classify_scholid(x)

        testthat::expect_true(all(is.na(got)))
    }
)

testthat::test_that("classify_scholid respects scholid_types order", {
    x <- c("PMC12345", "10.1000/182")
    got <- classify_scholid(x)

    testthat::expect_equal(got[1], "pmcid")
    testthat::expect_equal(got[2], "doi")
})

testthat::test_that("classify_scholid errors on invalid x inputs", {
    testthat::expect_error(
        classify_scholid(),
        "`x` is required"
    )

    testthat::expect_error(
        classify_scholid(NULL),
        "`x` must not be NULL"
    )

    testthat::expect_error(
        classify_scholid(data.frame(x = 1)),
        "data frame"
    )
})


testthat::test_that(
    "classify_scholid returns all NA when all inputs are NA",
    {
        x <- rep(
            NA_character_,
            3L
        )

        got <- classify_scholid(x)

        testthat::expect_true(all(is.na(got)))
    }
)
