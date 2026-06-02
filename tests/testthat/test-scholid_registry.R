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

        testthat::expect_identical(length(ord), length(unique(ord)))
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
              "pmcid"
              )
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

testthat::test_that(
    "registry entries have required metadata and implementations",
    {
        reg <- .scholid_registry()
        types <- scholid_types()

        testthat::expect_identical(types, names(reg))

        for (type in types) {
            entry <- reg[[type]]

            testthat::expect_true(
                !is.null(entry$order),
                info = paste("missing order for type:", type)
            )
            testthat::expect_true(
                nzchar(entry$extract_pat),
                info = paste("missing extract_pat for type:", type)
            )

            testthat::expect_false(
                is.null(.scholid_resolve_impl(
                    type,
                    "is_",
                    required = FALSE
                    )),
                info = paste("missing is_", type, "()", sep = "")
            )
            testthat::expect_false(
                is.null(.scholid_resolve_impl(
                    type,
                    "normalize_",
                    required = FALSE
                    )),
                info = paste("missing normalize_", type, "()", sep = "")
            )
            testthat::expect_false(
                is.null(.scholid_resolve_impl(
                    type,
                    "extract_",
                    required = FALSE
                    )),
                info = paste("missing extract_", type, "()", sep = "")
            )

            testthat::expect_false(
                is.null(.scholid_registry_extract_pat(type)),
                info = paste("extract_pat lookup failed for type:", type)
            )
        }
    }
)
