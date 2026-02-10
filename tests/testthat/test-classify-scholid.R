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

testthat::test_that("classify_scholid does not classify wrapped identifiers", {
    x <- c(
        "https://doi.org/10.1000/182",
        "https://orcid.org/0000-0002-1825-0097",
        "arXiv:2101.00001",
        "ISSN 0317-8471",
        "PMID: 12345",
        "PMCID: PMC12345"
    )

    got <- classify_scholid(x)

    testthat::expect_true(all(is.na(got)))
})

testthat::test_that("classify_scholid works after per-type normalization", {
    x <- c(
        "https://doi.org/10.1000/182",
        "https://orcid.org/0000-0002-1825-0097",
        "arXiv:2101.00001v2",
        "ISSN 0317-8471",
        "PMID: 12345",
        "PMCID: PMC12345",
        "0-306-40615-2"
    )

    x[1] <- normalize_doi(x[1])
    x[2] <- normalize_orcid(x[2])
    x[3] <- normalize_arxiv(x[3])
    x[4] <- normalize_issn(x[4])
    x[5] <- normalize_pmid(x[5])
    x[6] <- normalize_pmcid(x[6])
    x[7] <- normalize_isbn(x[7])

    got <- classify_scholid(x)

    testthat::expect_equal(got[1], "doi")
    testthat::expect_equal(got[2], "orcid")
    testthat::expect_equal(got[3], "arxiv")
    testthat::expect_equal(got[4], "issn")
    testthat::expect_equal(got[5], "pmid")
    testthat::expect_equal(got[6], "pmcid")
    testthat::expect_equal(got[7], "isbn")
})

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
