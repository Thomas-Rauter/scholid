testthat::test_that(
    ".scholid_check_x errors when `x` is missing",
    {
        testthat::expect_error(
            .scholid_check_x(
                arg = "x"
            ),
            "`x` is required"
        )
    }
)

testthat::test_that(
    ".scholid_check_x errors when `x` is NULL",
    {
        testthat::expect_error(
            .scholid_check_x(
                x = NULL,
                arg = "x"
            ),
            "`x` must not be NULL"
        )
    }
)

testthat::test_that(
    ".scholid_check_x errors on data frames",
    {
        testthat::expect_error(
            .scholid_check_x(
                x = data.frame(a = 1),
                arg = "x"
            ),
            "data frame"
        )
    }
)

testthat::test_that(
    ".scholid_check_x errors on non-atomic, non-list inputs",
    {
        x <- as.environment(list(a = 1))

        testthat::expect_error(
            .scholid_check_x(
                x = x,
                arg = "x"
            ),
            "atomic vector or list"
        )
    }
)

testthat::test_that(
    ".scholid_check_x returns TRUE invisibly for atomic vectors and lists",
    {
        ok1 <- .scholid_check_x(
            x = 1:3,
            arg = "x"
        )
        ok2 <- .scholid_check_x(
            x = list(1, 2),
            arg = "x"
        )

        testthat::expect_true(isTRUE(ok1))
        testthat::expect_true(isTRUE(ok2))
    }
)

testthat::test_that(
    ".scholid_as_scalar_character errors when `x` is missing",
    {
        testthat::expect_error(
            .scholid_as_scalar_character(
                arg = "type"
            ),
            "`type` is required"
        )
    }
)

testthat::test_that(
    ".scholid_as_scalar_character errors when `x` is NULL",
    {
        testthat::expect_error(
            .scholid_as_scalar_character(
                x = NULL,
                arg = "type"
            ),
            "`type` must not be NULL"
        )
    }
)

testthat::test_that(
    ".scholid_as_scalar_character errors when `x` is not length 1",
    {
        testthat::expect_error(
            .scholid_as_scalar_character(
                x = c("a", "b"),
                arg = "type"
            ),
            "must be length 1"
        )
    }
)

testthat::test_that(
    ".scholid_as_scalar_character errors when `x` is not character",
    {
        testthat::expect_error(
            .scholid_as_scalar_character(
                x = 1,
                arg = "type"
            ),
            "must be a character string"
        )
    }
)

testthat::test_that(
    ".scholid_as_scalar_character converts factors to character",
    {
        x <- factor("doi")

        got <- .scholid_as_scalar_character(
            x = x,
            arg = "type"
        )

        testthat::expect_identical(
            got,
            "doi"
        )
    }
)

testthat::test_that(
    ".scholid_as_scalar_character trims whitespace",
    {
        got <- .scholid_as_scalar_character(
            x = "  doi  ",
            arg = "type"
        )

        testthat::expect_identical(
            got,
            "doi"
        )
    }
)

testthat::test_that(
    ".scholid_as_scalar_character returns NA for empty or whitespace strings",
    {
        got1 <- .scholid_as_scalar_character(
            x = "",
            arg = "type"
        )
        got2 <- .scholid_as_scalar_character(
            x = "   ",
            arg = "type"
        )

        testthat::expect_true(is.na(got1))
        testthat::expect_true(is.na(got2))
    }
)

testthat::test_that(
    ".scholid_match_type errors on NA or empty strings",
    {
        testthat::expect_error(
            .scholid_match_type(
                NA_character_
            ),
            "non-empty string"
        )

        testthat::expect_error(
            .scholid_match_type(
                ""
            ),
            "non-empty string"
        )
    }
)

testthat::test_that(
    ".scholid_match_type rejects unknown types",
    {
        testthat::expect_error(
            .scholid_match_type(
                "not_a_type"
            ),
            "should be one of"
        )
    }
)

testthat::test_that(
    ".scholid_match_type rejects abbreviations and partial matching",
    {
        choices <- scholid_types()
        testthat::skip_if(length(choices) == 0L)

        t1 <- substr(choices[1], 1, 1)

        if (t1 != choices[1]) {
            testthat::expect_error(
                .scholid_match_type(
                    t1
                ),
                "abbreviations are not allowed"
            )
        } else {
            testthat::skip("No shorter abbreviation available for this type.")
        }
    }
)

testthat::test_that(
    ".scholid_match_type accepts exact supported types",
    {
        choices <- scholid_types()
        testthat::skip_if(length(choices) == 0L)

        got <- .scholid_match_type(
            choices[1]
        )

        testthat::expect_identical(
            got,
            choices[1]
        )
    }
)
