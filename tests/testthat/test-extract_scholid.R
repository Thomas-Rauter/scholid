testthat::test_that("extract_scholid dispatches to extract_<type>()", {
    if (!exists("extract_doi", mode = "function")) {
        testthat::skip("extract_doi() not available.")
    }

    txt <- "See https://doi.org/10.1000/182 for details."
    got <- extract_scholid(txt, "doi")

    testthat::expect_true(is.list(got))
    testthat::expect_length(got, 1L)
    testthat::expect_true(length(got[[1]]) >= 1L)
})

testthat::test_that("extract_scholid is vectorized over text", {
    txt <- c(
        "doi:10.1000/182",
        "no identifier here",
        NA_character_
    )

    got <- extract_scholid(txt, "doi")

    testthat::expect_type(got, "list")
    testthat::expect_length(got, length(txt))
    testthat::expect_true(is.character(got[[1]]))
    testthat::expect_length(got[[2]], 0L)
    testthat::expect_length(got[[3]], 0L)
})

testthat::test_that("extract_scholid returns empty vectors for no matches", {
    txt <- c(
        "nothing here",
        "still nothing"
    )

    got <- extract_scholid(txt, "doi")

    testthat::expect_true(all(vapply(got, length, integer(1)) == 0L))
})

testthat::test_that("extract_scholid validates `type` strictly", {
    testthat::expect_error(
        extract_scholid("x", NA_character_),
        "`type` must be a non-empty string"
    )

    testthat::expect_error(
        extract_scholid("x", ""),
        "`type` must be a non-empty string"
    )

    testthat::expect_error(
        extract_scholid("x", "not_a_type"),
        "should be one of"
    )
})

testthat::test_that("extract_scholid validates `text`", {
    testthat::expect_error(
        extract_scholid(type = "doi"),
        "`text` is required"
    )

    testthat::expect_error(
        extract_scholid(NULL, "doi"),
        "`text` must not be NULL"
    )

    testthat::expect_error(
        extract_scholid(data.frame(x = 1), "doi"),
        "data frame"
    )
})

testthat::test_that("extract_scholid works across multiple ID types", {
    txt <- c(
        "doi:10.1000/182",
        "ORCID 0000-0002-1825-0097",
        "arXiv:2101.00001v2",
        "PMCID: PMC12345"
    )

    got_doi <- extract_scholid(txt, "doi")
    got_orc <- extract_scholid(txt, "orcid")
    got_arx <- extract_scholid(txt, "arxiv")
    got_pmc <- extract_scholid(txt, "pmcid")

    testthat::expect_true(length(got_doi[[1]]) >= 1L)
    testthat::expect_true(length(got_orc[[2]]) >= 1L)
    testthat::expect_true(length(got_arx[[3]]) >= 1L)
    testthat::expect_true(length(got_pmc[[4]]) >= 1L)
})
