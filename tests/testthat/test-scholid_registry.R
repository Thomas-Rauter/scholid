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
                is.null(.scholid_resolve_impl(
                    type,
                    "key_",
                    required = FALSE
                    )),
                info = paste("missing key_", type, "()", sep = "")
            )

            testthat::expect_false(
                is.null(.scholid_registry_extract_pat(type)),
                info = paste("extract_pat lookup failed for type:", type)
            )
        }
    }
)

testthat::test_that(
    "registry extract patterns mark the identifier with an id group",
    {
        for (type in scholid_types()) {
            m <- regexpr(
                .scholid_registry_extract_pat(type),
                "",
                perl = TRUE
            )
            testthat::expect_true(
                "id" %in% attr(m, "capture.names"),
                info = type
            )
        }
    }
)

testthat::test_that(
    "registry states each type's resolver URL and CURIE prefix, or none",
    {
        reg <- .scholid_registry()

        for (type in scholid_types()) {
            entry <- reg[[type]]
            urls <- c(entry$url, entry$url_alt)

            testthat::expect_true(
                is.character(entry$url),
                info = paste("missing url for type:", type)
            )
            testthat::expect_true(
                is.character(entry$curie) && length(entry$curie) == 1L,
                info = paste("missing curie for type:", type)
            )
            testthat::expect_true(
                all(grepl("^https://[^{}]+[{]id[}][^{}]*$", urls)),
                info = paste("malformed url template for type:", type)
            )
            testthat::expect_true(
                is.na(entry$curie) || grepl("^[a-z0-9.]+$", entry$curie),
                info = paste("malformed curie for type:", type)
            )
            testthat::expect_true(
                !isTRUE(entry$curie_is_id) || !is.na(entry$curie),
                info = paste("curie_is_id without curie for type:", type)
            )
        }

        no_url <- names(reg)[vapply(
            reg,
            function(entry) !length(entry$url),
            logical(1)
        )]
        no_curie <- names(reg)[vapply(
            reg,
            function(entry) is.na(entry$curie),
            logical(1)
        )]
        curie_is_id <- names(reg)[vapply(
            reg,
            function(entry) isTRUE(entry$curie_is_id),
            logical(1)
        )]

        testthat::expect_identical(no_url, "isbn")
        testthat::expect_identical(no_curie, "bibcode")
        testthat::expect_setequal(curie_is_id, c("swhid", "ark", "rrid"))
    }
)

testthat::test_that(
    "registry records a DOI template for arXiv and URL escapes for DOIs only",
    {
        reg <- .scholid_registry()

        has_doi <- names(reg)[vapply(
            reg,
            function(entry) !is.null(entry$doi),
            logical(1)
        )]
        has_url_escape <- names(reg)[vapply(
            reg,
            function(entry) !is.null(entry$url_escape),
            logical(1)
        )]

        testthat::expect_identical(has_doi, "arxiv")
        testthat::expect_true(
            grepl("^10\\.[0-9]{4,9}/[^{}]*[{]id[}]$", reg$arxiv$doi)
        )
        testthat::expect_identical(has_url_escape, "doi")
        testthat::expect_setequal(reg$doi$url_escape, c("%", "#", "?"))
    }
)
