testthat::test_that("normalize_doi strips wrappers and trailing punctuation", {
    x <- c(
        "10.1000/182",
        " doi:10.1000/182 ",
        "https://doi.org/10.1000/182",
        "http://dx.doi.org/10.1000/182.",
        "not a doi",
        NA
    )

    got <- normalize_doi(x)
    exp <- c(
        "10.1000/182",
        "10.1000/182",
        "10.1000/182",
        "10.1000/182",
        NA_character_,
        NA_character_
    )

    testthat::expect_identical(got, exp)
})

testthat::test_that(
    "DOI normalization keeps valid inputs canonical",
    {
        testthat::expect_equal(
            normalize_scholid(
                c(
                    "10.1000/182",
                    "doi:10.1000/182",
                    "https://doi.org/10.1000/182",
                    "10.1000/182."
                ),
                "doi"
            ),
            c(
                "10.1000/182",
                "10.1000/182",
                "10.1000/182",
                "10.1000/182"
            )
        )
    }
)

testthat::test_that(
    "DOI normalization rejects contaminated DOI-like inputs",
    {
        testthat::expect_equal(
            normalize_scholid(
                c(
                    "10.1000/182</a>",
                    "10.1000/182'foo",
                    "10.1000/182>abc<",
                    "10.1000/182)yy"
                ),
                "doi"
            ),
            c(
                NA_character_,
                NA_character_,
                NA_character_,
                NA_character_
            )
        )
    }
)

testthat::test_that(
    "normalized DOI outputs are valid DOIs and classify as doi",
    {
        x <- normalize_scholid(
            c(
                "10.1000/182",
                "doi:10.1000/182",
                "https://doi.org/10.1000/182",
                "10.1000/182."
            ),
            "doi"
        )

        testthat::expect_true(all(is_scholid(x, "doi")))
        testthat::expect_true(all(classify_scholid(x) == "doi"))
    }
)

testthat::test_that(
    "DOI normalization drops wrapped forms it cannot cleanly recover",
    {
        testthat::expect_equal(
            normalize_scholid(
                c(
                    "(10.1000/182)",
                    "[10.1000/182]",
                    "{10.1000/182}",
                    "<10.1000/182>"
                ),
                "doi"
            ),
            c(
                NA_character_,
                NA_character_,
                NA_character_,
                NA_character_
            )
        )
    }
)

testthat::test_that("normalize_orcid removes wrappers and enforces grouping", {
    x <- c(
        "0000-0002-1825-0097",
        "https://orcid.org/0000-0002-1825-0097",
        "0000000218250097",
        " 0000 0002 1825 0097 ",
        "bad",
        NA
    )

    got <- normalize_orcid(x)
    exp <- c(
        "0000-0002-1825-0097",
        "0000-0002-1825-0097",
        "0000-0002-1825-0097",
        "0000-0002-1825-0097",
        NA_character_,
        NA_character_
    )

    testthat::expect_identical(got, exp)
})

testthat::test_that(
    "normalize_ror strips wrappers and enforces compact lowercase form", {
    x <- c(
        "01an7q238",
        "https://ror.org/01an7q238",
        "https://ror.org/01an7q238/",
        "ror.org/01an7q238",
        "ROR: 02mhbdp94",
        " 02s376052 ",
        "02mhbdp99",
        "bad",
        NA
    )

    got <- normalize_ror(x)
    exp <- c(
        "01an7q238",
        "01an7q238",
        "01an7q238",
        "01an7q238",
        "02mhbdp94",
        "02s376052",
        NA_character_,
        NA_character_,
        NA_character_
    )

    testthat::expect_identical(got, exp)
})

testthat::test_that(
    "normalize_swhid strips wrappers and requires swh prefix or resolver URL",
    {
        x <- c(
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "https://archive.softwareheritage.org/swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "https://identifiers.org/swh/swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d",
            "SWH:1:CNT:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "94a9ed024d3859793618152ea559a168bbcbb5e2",
            "swh:2:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "bad",
            NA
        )

        got <- normalize_swhid(x)
        exp <- c(
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d",
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            NA_character_,
            NA_character_,
            NA_character_,
            NA_character_
        )

        testthat::expect_identical(got, exp)
    }
)

testthat::test_that("normalize_swhid strips wrappers and lwcases compactly", {
    x <- c(
        "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
        "https://archive.softwareheritage.org/swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
        "https://identifiers.org/swh/swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d",
        "SWH:1:CNT:94a9ed024d3859793618152ea559a168bbcbb5e2",
        "94a9ed024d3859793618152ea559a168bbcbb5e2",
        "swh:2:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
        "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;unknown=foo",
        "bad",
        NA
    )

    got <- normalize_swhid(x)

    exp <- c(
        "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
        "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
        "swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d",
        "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_
    )

    testthat::expect_identical(got, exp)
})

testthat::test_that(
    "normalize_rrid strips wrappers and requires RRID label or resolver URL",
    {
        x <- c(
            "RRID:AB_262044",
            "https://scicrunch.org/resolver/RRID:AB_262044",
            "https://identifiers.org/RRID:SCR_007358",
            "RRID: AB_262044",
            "rrid:CVCL_2260",
            "AB_262044",
            "RRID:UNKNOWN_123",
            "bad",
            NA
        )

        got <- normalize_rrid(x)
        exp <- c(
            "RRID:AB_262044",
            "RRID:AB_262044",
            "RRID:SCR_007358",
            "RRID:AB_262044",
            "RRID:CVCL_2260",
            NA_character_,
            NA_character_,
            NA_character_,
            NA_character_
        )

        testthat::expect_identical(got, exp)
    }
)

testthat::test_that(
    "normalized RRID outputs are valid RRIDs and classify as rrid",
    {
        x <- normalize_scholid(
            c(
                "RRID:AB_262044",
                "https://scicrunch.org/resolver/RRID:SCR_007358",
                "RRID: Addgene_80088"
            ),
            "rrid"
        )

        testthat::expect_true(all(is_rrid(x)))
        testthat::expect_true(all(is_scholid(x, "rrid")))
        testthat::expect_true(all(classify_scholid(x) == "rrid"))
    }
)

testthat::test_that(
    "normalized SWHID outputs are valid SWHIDs and classify as swhid",
    {
        x <- normalize_scholid(
            c(
                "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
                "https://archive.softwareheritage.org/swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d",
                paste0(
                    "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;",
                    "origin=https://example.org/repo.git;",
                    "lines=9-15"
                )
            ),
            "swhid"
        )

        testthat::expect_true(all(is_swhid(x)))
        testthat::expect_true(all(is_scholid(x, "swhid")))
        testthat::expect_true(all(classify_scholid(x) == "swhid"))
    }
)

testthat::test_that(
    "RRID normalization accepts plausible labeled and URL input forms",
    {
        x <- c(
            "RRID:AB_262044",
            "https://scicrunch.org/resolver/RRID:AB_262044",
            "RRID: IMSR_JAX:000664",
            "rrid:MGI:3840442"
        )

        testthat::expect_identical(
            normalize_scholid(x, "rrid"),
            c(
                "RRID:AB_262044",
                "RRID:AB_262044",
                "RRID:IMSR_JAX:000664",
                "RRID:MGI:3840442"
            )
        )
    }
)

testthat::test_that(
    "RRID normalization rejects bare local IDs and unknown authorities",
    {
        x <- c(
            "AB_262044",
            "RRID:UNKNOWN_123",
            "catalog AB_262044",
            "(RRID:AB_262044)",
            "RRID:AB_262044xyz"
        )

        testthat::expect_identical(
            normalize_scholid(x, "rrid"),
            rep(NA_character_, length(x))
        )
    }
)

testthat::test_that("ISBN normalization keeps valid inputs canonical", {
    testthat::expect_equal(
        normalize_scholid(
            c("978-0-306-40615-7", "0306406152"),
            "isbn"
        ),
        c("9780306406157", "0306406152")
    )
})

testthat::test_that(
    "ISBN normalization rejects checksum-invalid ISBN-like inputs",
    {
        testthat::expect_equal(
            normalize_scholid(
                c("1234567890123", "030640615X"),
                "isbn"
            ),
            c(NA_character_, NA_character_)
        )
    }
)

testthat::test_that(
    "normalized ISBN outputs are valid ISBNs and classify as isbn",
    {
        x <- normalize_scholid(
            c("9780306406157", "0306406152"),
            "isbn"
        )

        testthat::expect_true(all(is_scholid(x, "isbn")))
        testthat::expect_true(all(classify_scholid(x) == "isbn"))
    }
)

testthat::test_that("normalize_isbn removes separators and uppercases X", {
    x <- c(
        "0-306-40615-2",
        "0306406152",
        "978-0-306-40615-7",
        "9780306406157",
        "0-306-40615-x",
        "not an isbn",
        NA
    )

    got <- normalize_isbn(x)
    exp <- c(
        "0306406152",
        "0306406152",
        "9780306406157",
        "9780306406157",
        NA_character_,
        NA_character_,
        NA_character_
    )

    testthat::expect_identical(got, exp)
})

testthat::test_that(
    "normalize_issn strips ISSN prefix + accepts only compact/hyph. forms", {
    x <- c(
        "0317-8471",
        "ISSN 0317-8471",
        "03178471",
        "0317 8471",
        "bad",
        NA
    )

    got <- normalize_issn(x)
    exp <- c(
        "0317-8471",
        "0317-8471",
        "0317-8471",
        NA_character_,
        NA_character_,
        NA_character_
    )

    testthat::expect_identical(got, exp)
})

testthat::test_that("normalize_arxiv strips wrappers and preserves versions", {
    x <- c(
        "arXiv:2101.00001",
        "https://arxiv.org/abs/2101.00001v2",
        "2101.00001v2",
        "hep-th/9901001",
        "hep-th/9901001v3",
        "bad",
        NA
    )

    got <- normalize_arxiv(x)
    exp <- c(
        "2101.00001",
        "2101.00001v2",
        "2101.00001v2",
        "hep-th/9901001",
        "hep-th/9901001v3",
        NA_character_,
        NA_character_
    )

    testthat::expect_identical(got, exp)
})

testthat::test_that(
    "normalize_pmid accepts labeled forms with or without a colon",
    {
        x <- c(
            "PMID: 12345678",
            "PMID 12345678",
            "pmid 7654321",
            "pmid: 7654321",
            "  PMID 12345678  ",
            "PMID12345678",
            "not a pmid"
        )

        testthat::expect_identical(
            normalize_scholid(x, "pmid"),
            c(
                "12345678",
                "12345678",
                "7654321",
                "7654321",
                "12345678",
                NA_character_,
                NA_character_
            )
        )

        testthat::expect_identical(
            detect_scholid_type(x),
            c(
                "pmid",
                "pmid",
                "pmid",
                "pmid",
                "pmid",
                NA_character_,
                NA_character_
            )
        )
    }
)

testthat::test_that("normalize_pmid strips label and requires digits", {
    x <- c(
        "12345",
        "PMID: 12345",
        "  12345  ",
        "12A",
        NA
    )

    got <- normalize_pmid(x)
    exp <- c(
        "12345",
        "12345",
        "12345",
        NA_character_,
        NA_character_
    )

    testthat::expect_identical(got, exp)
})

testthat::test_that(
    "normalize_pmcid accepts labeled forms with or without a colon",
    {
        testthat::expect_identical(
            normalize_scholid("PMCID: PMC1234567", "pmcid"),
            "PMC1234567"
        )
        testthat::expect_identical(
            normalize_scholid("PMCID PMC1234567", "pmcid"),
            "PMC1234567"
        )
        testthat::expect_identical(
            normalize_scholid("pmcid PMC7654321", "pmcid"),
            "PMC7654321"
        )
    }
)

testthat::test_that(
    "detect_scholid_type detects labeled pmcid values",
    {
        x <- c(
            "PMCID: PMC1234567",
            "PMCID PMC1234567",
            "pmcid PMC7654321",
            "12345678"
        )

        got <- detect_scholid_type(x)

        testthat::expect_identical(
            got,
            c("pmcid", "pmcid", "pmcid", "pmid")
        )
    }
)

testthat::test_that("normalize_pmcid strips label and enforces PMC prefix", {
    x <- c(
        "PMC12345",
        "PMCID: PMC12345",
        "pmc12345",
        "12345",
        NA
    )

    got <- normalize_pmcid(x)
    exp <- c(
        "PMC12345",
        "PMC12345",
        "PMC12345",
        NA_character_,
        NA_character_
    )

    testthat::expect_identical(got, exp)
})

testthat::test_that(
    "ORCID normalization keeps valid inputs canonical",
    {
        testthat::expect_equal(
            normalize_scholid(
                c(
                    "0000-0002-1825-0097",
                    "0000000218250097",
                    "https://orcid.org/0000-0002-1825-0097"
                ),
                "orcid"
            ),
            c(
                "0000-0002-1825-0097",
                "0000-0002-1825-0097",
                "0000-0002-1825-0097"
            )
        )
    }
)

testthat::test_that(
    "ORCID normalization rejects checksum-invalid ORCID-like inputs",
    {
        testthat::expect_equal(
            normalize_scholid(
                c(
                    "0000-0002-1825-009X",
                    "000000021825009X",
                    "https://orcid.org/0000-0002-1825-009X"
                ),
                "orcid"
            ),
            c(NA_character_, NA_character_, NA_character_)
        )
    }
)

testthat::test_that(
    "normalized ORCID outputs are valid ORCIDs and classify as orcid",
    {
        x <- normalize_scholid(
            c(
                "0000-0002-1825-0097",
                "0000000218250097",
                "orcid:0000-0002-1825-0097"
            ),
            "orcid"
        )

        testthat::expect_true(all(is_scholid(x, "orcid")))
        testthat::expect_true(all(classify_scholid(x) == "orcid"))
    }
)

testthat::test_that(
    "normalized ROR outputs are valid ROR iDs and classify as ror",
    {
        x <- normalize_scholid(
            c(
                "01an7q238",
                "https://ror.org/02mhbdp94",
                "ROR: 02s376052"
            ),
            "ror"
        )

        testthat::expect_true(all(is_ror(x)))
        testthat::expect_true(all(is_scholid(x, "ror")))
        testthat::expect_true(all(classify_scholid(x) == "ror"))
    }
)

testthat::test_that(
    "normalize_orcid canonicalizes valid lowercase x to uppercase X",
    {
        x <- c(
            "0000-0002-1694-233x",
            NA_character_
        )

        got <- normalize_orcid(x)

        testthat::expect_identical(
            got,
            c(
                "0000-0002-1694-233X",
                NA_character_
            )
        )
    }
)

testthat::test_that(
    "normalize_doi strips multiple trailing punctuation",
    {
        x <- c(
            "10.1000/182,,,",
            "10.1000/182;:",
            NA_character_
        )

        got <- normalize_doi(x)

        testthat::expect_identical(
            got,
            c("10.1000/182", "10.1000/182", NA_character_)
        )
    }
)

testthat::test_that(
    "normalize_scholid dispatches to normalize_<type>()",
    {
        x <- c(
            " https://doi.org/10.1000/182. ",
            NA_character_
        )

        got <- normalize_scholid(
            x,
            "doi"
        )

        testthat::expect_identical(
            got,
            c("10.1000/182", NA_character_)
        )
    }
)

testthat::test_that(
    "normalize_scholid works for another type",
    {
        x <- c(
            "https://orcid.org/0000-0002-1825-0097",
            "bad",
            NA_character_
        )

        got <- normalize_scholid(
            x,
            "orcid"
        )

        testthat::expect_identical(
            got,
            c("0000-0002-1825-0097", NA_character_, NA_character_)
        )
    }
)


testthat::test_that(
    "issn normalization rejects invalid values and canonicalizes valid ones",
    {
        testthat::expect_false(is_issn("9999-9999"))
        testthat::expect_false(is_issn("2434-561X-90"))

        testthat::expect_identical(
            normalize_scholid("9999-9999", "issn"),
            NA_character_
        )
        testthat::expect_identical(
            normalize_scholid("2434-561X-90", "issn"),
            NA_character_
        )
        testthat::expect_identical(
            normalize_scholid("2434561X", "issn"),
            "2434-561X"
        )
        testthat::expect_identical(
            normalize_scholid("2434-561x", "issn"),
            "2434-561X"
        )
    }
)

testthat::test_that(
    "arXiv normalization accepts dotted old-style identifiers",
    {
        x <- c(
            "math.GT/0309136",
            "math.GT/0309136v1",
            "cs.CL/0501001"
        )

        testthat::expect_identical(
            normalize_scholid(x, "arxiv"),
            c(
                "math.GT/0309136",
                "math.GT/0309136v1",
                "cs.CL/0501001"
            )
        )
    }
)

testthat::test_that(
    "arXiv normalization handles wrapped dotted old-style identifiers",
    {
        x <- c(
            "arXiv:math.GT/0309136",
            "https://arxiv.org/abs/math.GT/0309136"
        )

        testthat::expect_identical(
            normalize_scholid(x, "arxiv"),
            c(
                "math.GT/0309136",
                "math.GT/0309136"
            )
        )
    }
)

testthat::test_that(
    "dotted old-style arXiv identifiers validate and classify as arxiv",
    {
        x <- normalize_scholid(
            c(
                "math.GT/0309136",
                "math.GT/0309136v1",
                "cs.CL/0501001",
                "arXiv:math.GT/0309136"
            ),
            "arxiv"
        )

        testthat::expect_true(all(is_scholid(x, "arxiv")))
        testthat::expect_true(all(classify_scholid(x) == "arxiv"))
    }
)

testthat::test_that(
    "ORCID normalization accepts plausible input forms",
    {
        x <- c(
            "0000-0002-1825-0097",
            "0000 0002 1825 0097",
            "https://orcid.org/0000-0002-1825-0097",
            "orcid:0000-0002-1825-0097"
        )

        testthat::expect_identical(
            normalize_scholid(x, "orcid"),
            c(
                "0000-0002-1825-0097",
                "0000-0002-1825-0097",
                "0000-0002-1825-0097",
                "0000-0002-1825-0097"
            )
        )
    }
)

testthat::test_that(
    "ORCID normalization rejects noisy or malformed inputs",
    {
        x <- c(
            "0000_0002_1825_0097",
            "(0000-0002-1825-0097)",
            "0000-0002-1825-0097.",
            "abc0000-0002-1825-0097",
            "0000-0002-1825-0097xyz",
            "0000/0002/1825/0097",
            "0000--0002--1825--0097"
        )

        testthat::expect_identical(
            normalize_scholid(x, "orcid"),
            c(
                NA_character_,
                NA_character_,
                NA_character_,
                NA_character_,
                NA_character_,
                NA_character_,
                NA_character_
            )
        )
    }
)

testthat::test_that(
    "PMCID normalization restores missing PMC prefix after PMCID label",
    {
        x <- c(
            "PMCID:123456",
            "PMCID 123456",
            "pmcid: 123456"
        )

        testthat::expect_identical(
            normalize_scholid(x, "pmcid"),
            c(
                "PMC123456",
                "PMC123456",
                "PMC123456"
            )
        )
    }
)

testthat::test_that(
    "ROR normalization accepts plausible input forms",
    {
        x <- c(
            "01an7q238",
            "https://ror.org/01an7q238",
            "ror.org/02mhbdp94",
            "ROR: 02s376052"
        )

        testthat::expect_identical(
            normalize_scholid(x, "ror"),
            c(
                "01an7q238",
                "01an7q238",
                "02mhbdp94",
                "02s376052"
            )
        )
    }
)

testthat::test_that(
    "ROR normalization rejects checksum-invalid and malformed inputs",
    {
        x <- c(
            "02mhbdp99",
            "not-a-ror",
            "02mhbdp9",
            "(01an7q238)",
            "abc01an7q238",
            "01an7q238xyz"
        )

        testthat::expect_identical(
            normalize_scholid(x, "ror"),
            rep(NA_character_, length(x))
        )
    }
)

testthat::test_that(
    "SWHID normalization accepts plausible labeled and URL input forms",
    {
        x <- c(
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "https://archive.softwareheritage.org/swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "SWH:1:REV:309cf2674ee7a0749978cf8265ab91a60aea0f7d",
            paste0(
                "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;",
                "origin=https://example.org/repo.git;",
                "visit=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9"
            )
        )

        testthat::expect_identical(
            normalize_scholid(x, "swhid"),
            c(
                "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
                "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
                "swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d",
                paste0(
                    "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;",
                    "origin=https://example.org/repo.git;",
                    "visit=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9"
                )
            )
        )
    }
)

testthat::test_that(
    "SWHID normalization rejects bare hex strings and invalid qualifiers",
    {
        x <- c(
            "94a9ed024d3859793618152ea559a168bbcbb5e2",
            "swh:2:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;unknown=foo",
            "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;path=relative",
            "(swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2)"
        )

        testthat::expect_identical(
            normalize_scholid(x, "swhid"),
            rep(NA_character_, length(x))
        )
    }
)

testthat::test_that(
    "PMCID normalization keeps unlabeled numeric forms invalid",
    {
        x <- c(
            "PMC 123456",
            "123456"
        )

        testthat::expect_identical(
            normalize_scholid(x, "pmcid"),
            c(
                NA_character_,
                NA_character_
            )
        )
    }
)

testthat::test_that(
    "normalized PMCID outputs validate and classify as pmcid",
    {
        x <- normalize_scholid(
            c(
                "PMCID: PMC123456",
                "PMCID:123456",
                "pmcid: 123456"
            ),
            "pmcid"
        )

        testthat::expect_true(all(is_scholid(x, "pmcid")))
        testthat::expect_true(all(classify_scholid(x) == "pmcid"))
    }
)
