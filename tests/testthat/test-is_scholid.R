testthat::test_that("is_scholid dispatches to is_<type>()", {
    if (!exists("is_doi", mode = "function")) {
        testthat::skip("is_doi() not available.")
    }

    x <- c("10.1000/182", "not a doi", NA)
    testthat::expect_identical(is_scholid(x, "doi"), is_doi(x))
})

testthat::test_that("is_scholid is vectorized and preserves NA", {
    if (!exists("is_doi", mode = "function")) {
        testthat::skip("is_doi() not available.")
    }

    x <- c(NA, "10.1000/182")
    out <- is_scholid(x, "doi")

    testthat::expect_type(out, "logical")
    testthat::expect_length(out, length(x))
    testthat::expect_true(is.na(out[1]))
})

testthat::test_that("is_scholid validates `type` strictly", {
    testthat::expect_error(
        is_scholid("x", NA_character_),
        "`type` must be a non-empty string"
    )

    testthat::expect_error(
        is_scholid("x", ""),
        "`type` must be a non-empty string"
    )

    testthat::expect_error(
        is_scholid("x", "not_a_type"),
        "should be one of"
    )

    if (all(c("pmid", "pmcid") %in% scholid_types())) {
        testthat::expect_error(
            is_scholid("x", "pmi"),
            "abbreviations are not allowed"
        )
    }
})

testthat::test_that("is_scholid validates `x`", {
    testthat::expect_error(
        is_scholid(type = "doi"),
        "`x` is required"
    )

    testthat::expect_error(
        is_scholid(NULL, "doi"),
        "`x` must not be NULL"
    )

    testthat::expect_error(
        is_scholid(data.frame(x = 1), "doi"),
        "data frame"
    )
})
