testthat::test_that("detect_scholid_type returns char with input length", {
    x <- c("10.1000/182", NA_character_, "not an id")
    got <- detect_scholid_type(x)

    testthat::expect_type(got, "character")
    testthat::expect_length(got, length(x))
})

testthat::test_that(
    "detect_scholid_type returns NA for NA and unknown values", {
    x <- c(NA_character_, " ", "", "not an id")
    got <- detect_scholid_type(x)

    testthat::expect_true(all(is.na(got)))
})

testthat::test_that("detect_scholid_type classifies canonical identifiers", {
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

    got <- detect_scholid_type(x)

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

testthat::test_that("detect_scholid_type detects wrapped identifiers", {
    x <- c(
        "https://doi.org/10.1000/182",
        "doi:10.1000/182",
        "https://orcid.org/0000-0002-1825-0097",
        "arXiv:2101.00001",
        "https://arxiv.org/abs/hep-th/9901001",
        "ISSN 0317-8471",
        "ISBN 978-3-16-148410-0",
        "PMID: 12345",
        "PMCID: PMC12345"
    )

    got <- detect_scholid_type(x)

    testthat::expect_equal(got[1], "doi")
    testthat::expect_equal(got[2], "doi")
    testthat::expect_equal(got[3], "orcid")
    testthat::expect_equal(got[4], "arxiv")
    testthat::expect_equal(got[5], "arxiv")
    testthat::expect_equal(got[6], "issn")
    testthat::expect_equal(got[7], "isbn")
    testthat::expect_equal(got[8], "pmid")
    testthat::expect_equal(got[9], "pmcid")
})

testthat::test_that("detect_scholid_type trims whitespace before detection", {
    x <- c(
        " 0000-0002-1825-0097 ",
        "  10.1000/182  ",
        "  PMC12345 "
    )

    got <- detect_scholid_type(x)

    testthat::expect_equal(got[1], "orcid")
    testthat::expect_equal(got[2], "doi")
    testthat::expect_equal(got[3], "pmcid")
})

testthat::test_that("detect_scholid_type respects scholid_types order", {
    x <- c("PMC12345", "10.1000/182")
    got <- detect_scholid_type(x)

    testthat::expect_equal(got[1], "pmcid")
    testthat::expect_equal(got[2], "doi")
})

testthat::test_that("detect_scholid_type errors on invalid x inputs", {
    testthat::expect_error(
        detect_scholid_type(),
        "`x` is required"
    )

    testthat::expect_error(
        detect_scholid_type(NULL),
        "`x` must not be NULL"
    )

    testthat::expect_error(
        detect_scholid_type(data.frame(x = 1)),
        "data frame"
    )
})

testthat::test_that(
    "detect_scholid_type returns all NA when all inputs are NA",
    {
        x <- rep(
            NA_character_,
            3L
        )

        got <- detect_scholid_type(x)

        testthat::expect_true(all(is.na(got)))
    }
)
