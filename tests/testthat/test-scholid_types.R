testthat::test_that(
    "scholid_types returns stable, clean, non-empty types",
    {
        x <- scholid_types()

        testthat::expect_type(x, "character")
        testthat::expect_true(length(x) > 0L)
        testthat::expect_false(anyNA(x))

        testthat::expect_identical(x, unique(x))
        testthat::expect_identical(x, sort(x))

        testthat::expect_true(all(nzchar(x)))
        testthat::expect_true(all(grepl("^[a-z0-9]+$", x)))

        testthat::expect_true(all(c("doi", "orcid") %in% x))
    }
)
