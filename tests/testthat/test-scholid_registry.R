testthat::test_that(
    "scholid_types matches registry classification order",
    {
        testthat::expect_identical(
            scholid_types(),
            .scholid_types_ordered()
        )
    }
)

testthat::test_that(
    "registry order values are unique",
    {
        reg <- .scholid_registry()
        ord <- vapply(reg, function(entry) entry$order, integer(1))

        testthat::test_that(
            "registry order values are unique",
            {
                reg <- .scholid_registry()
                ord <- vapply(reg, function(entry) entry$order, integer(1))

                testthat::expect_identical(length(ord), length(unique(ord)))
            }
        )
    }
)

testthat::test_that(
    "registry marks pmid as detect_last fallback",
    {
        testthat::expect_identical(
            .scholid_detect_last_types(),
            "pmid"
        )

        testthat::expect_identical(
            .scholid_detect_primary_types(),
            c("doi", "arxiv", "orcid", "isbn", "issn", "pmcid")
        )
    }
)

testthat::test_that(
    "registry places pmid last in classification order",
    {
        types <- scholid_types()

        testthat::expect_identical(types[[length(types)]], "pmid")
    }
)
