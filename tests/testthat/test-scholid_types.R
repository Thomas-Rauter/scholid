testthat::test_that(
    "scholid_types returns stable, clean, non-empty types",
    {
        x <- scholid_types()

        testthat::expect_type(x, "character")
        testthat::expect_true(length(x) > 0L)
        testthat::expect_false(anyNA(x))

        testthat::expect_identical(x, unique(x))
        testthat::expect_identical(
            x,
            c("doi",
              "arxiv",
              "bibcode",
              "openalex",
              "swhid",
              "ark",
              "isni",
              "orcid",
              "ror",
              "rrid",
              "uniprot",
              "refseq",
              "sra",
              "geo",
              "bioproject",
              "assembly",
              "isbn",
              "issn",
              "pmcid",
              "pmid"
              )
        )

        testthat::expect_true(all(nzchar(x)))
        testthat::expect_true(all(grepl("^[a-z0-9]+$", x)))

        testthat::expect_true(all(c("doi", "orcid") %in% x))
    }
)

testthat::test_that(
    "cross-type: fixtures name every scholid_types() value",
    {
        testthat::expect_setequal(
            names(scholid_type_inputs),
            scholid_types()
        )
    }
)
