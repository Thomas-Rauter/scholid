testthat::test_that(
    "extract_scholid dispatches to extract_<type>()",
    {
        txt <- "See https://doi.org/10.1000/182 for details."
        got <- extract_scholid(
            txt,
            "doi"
        )

        testthat::expect_true(is.list(got))
        testthat::expect_length(
            got,
            1L
        )
        testthat::expect_true(is.character(got[[1]]))
        testthat::expect_true(length(got[[1]]) >= 1L)
    }
)

testthat::test_that(
    "extract_scholid is vectorized over text",
    {
        txt <- c(
            "doi:10.1000/182",
            "no identifier here",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "doi"
        )

        testthat::expect_type(
            got,
            "list"
        )
        testthat::expect_length(
            got,
            length(txt)
        )
        testthat::expect_true(is.character(got[[1]]))
        testthat::expect_length(
            got[[2]],
            0L
        )
        testthat::expect_length(
            got[[3]],
            0L
        )
    }
)

testthat::test_that(
    "extract_scholid returns empty vectors for no matches",
    {
        txt <- c(
            "nothing here",
            "still nothing"
        )

        got <- extract_scholid(
            txt,
            "doi"
        )

        testthat::expect_true(
            all(vapply(got, length, integer(1)) == 0L)
        )
    }
)

testthat::test_that(
    "extract_scholid validates `type` strictly",
    {
        testthat::expect_error(
            extract_scholid(
                "x",
                NA_character_
            ),
            "`type` must be a non-empty string"
        )

        testthat::expect_error(
            extract_scholid(
                "x",
                ""
            ),
            "`type` must be a non-empty string"
        )

        testthat::expect_error(
            extract_scholid(
                "x",
                "not_a_type"
            ),
            "should be one of"
        )
    }
)

testthat::test_that(
    "extract_scholid validates `text`",
    {
        testthat::expect_error(
            extract_scholid(
                type = "doi"
            ),
            "`text` is required"
        )

        testthat::expect_error(
            extract_scholid(
                NULL,
                "doi"
            ),
            "`text` must not be NULL"
        )

        testthat::expect_error(
            extract_scholid(
                data.frame(x = 1),
                "doi"
            ),
            "data frame"
        )
    }
)

testthat::test_that(
    "extract_scholid returns exact DOI matches for simple cases",
    {
        txt <- c(
            "doi:10.1000/182",
            "nope",
            NA_character_
        )
        got <- extract_scholid(
            txt,
            "doi"
        )

        testthat::expect_identical(
            got[[1]],
            "10.1000/182"
        )
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
        testthat::expect_identical(
            got[[3]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_doi finds multiple DOIs in one string",
    {
        txt <- "two DOIs: 10.1000/182 and 10.5555/12345678"
        got <- extract_scholid(
            txt,
            "doi"
        )[[1]]

        testthat::expect_true(any(got == "10.1000/182"))
        testthat::expect_true(any(got == "10.5555/12345678"))
    }
)

testthat::test_that(
    "extract_doi stops at quotes and angle brackets",
    {
        txt1 <- 'doi "10.1000/182"'
        txt2 <- "doi <10.1000/182>"
        txt3 <- "doi >10.1000/182<"

        got1 <- extract_scholid(
            txt1,
            "doi"
        )[[1]]
        got2 <- extract_scholid(
            txt2,
            "doi"
        )[[1]]
        got3 <- extract_scholid(
            txt3,
            "doi"
        )[[1]]

        testthat::expect_true(any(got1 == "10.1000/182"))
        testthat::expect_true(any(got2 == "10.1000/182"))
        testthat::expect_true(any(got3 == "10.1000/182"))
    }
)

testthat::test_that(
    "extract_orcid finds ORCIDs and allows X as the last character",
    {
        txt <- c(
            "ORCID 0000-0002-1825-0097",
            "ORCID 0000-0002-1694-233X",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "orcid"
        )

        testthat::expect_true(any(got[[1]] == "0000-0002-1825-0097"))
        testthat::expect_true(any(got[[2]] == "0000-0002-1694-233X"))
        testthat::expect_identical(
            got[[3]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_orcid returns multiple matches from one string",
    {
        txt <- paste(
            "two ORCIDs:",
            "0000-0002-1825-0097",
            "and 0000-0002-1694-233X"
        )

        got <- extract_scholid(
            txt,
            "orcid"
        )[[1]]

        testthat::expect_true(any(got == "0000-0002-1825-0097"))
        testthat::expect_true(any(got == "0000-0002-1694-233X"))
    }
)

testthat::test_that(
    "extract_ror finds ROR iDs in text and URLs",
    {
        txt <- c(
            "Affiliation: https://ror.org/01an7q238.",
            "ROR 02mhbdp94",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "ror"
        )

        testthat::expect_true(any(got[[1]] == "01an7q238"))
        testthat::expect_true(any(got[[2]] == "02mhbdp94"))
        testthat::expect_identical(
            got[[3]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_ror returns multiple matches from one string",
    {
        txt <- paste(
            "affiliations:",
            "https://ror.org/01an7q238",
            "and https://ror.org/02mhbdp94"
        )

        got <- extract_scholid(
            txt,
            "ror"
        )[[1]]

        testthat::expect_true(any(got == "01an7q238"))
        testthat::expect_true(any(got == "02mhbdp94"))
    }
)

testthat::test_that(
    "extract_ror rejects checksum-invalid ROR candidates",
    {
        txt <- "see https://ror.org/02mhbdp99 for details"

        got <- extract_scholid(
            txt,
            "ror"
        )[[1]]

        testthat::expect_identical(
            got,
            character(0)
        )
    }
)

testthat::test_that(
    "extract_swhid finds SWHIDs in text and URLs",
    {
        txt <- c(
            "Archive swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2.",
            "See https://archive.softwareheritage.org/swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "swhid"
        )

        testthat::expect_true(any(got[[1]] == "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2"))
        testthat::expect_true(any(got[[2]] == "swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d"))
        testthat::expect_identical(
            got[[3]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_swhid returns multiple matches from one string",
    {
        txt <- paste(
            "artifacts:",
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "and swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d"
        )

        got <- extract_scholid(
            txt,
            "swhid"
        )[[1]]

        testthat::expect_true(any(got == "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2"))
        testthat::expect_true(any(got == "swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d"))
    }
)

testthat::test_that(
    "extract_swhid rejects invalid SWHID candidates",
    {
        txt <- "see swh:2:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2 for details"

        got <- extract_scholid(
            txt,
            "swhid"
        )[[1]]

        testthat::expect_identical(
            got,
            character(0)
        )
    }
)

testthat::test_that(
    "extract_swhid does not extract bare hex strings without swh prefix",
    {
        txt <- "commit 94a9ed024d3859793618152ea559a168bbcbb5e2 was archived"

        got <- extract_scholid(
            txt,
            "swhid"
        )[[1]]

        testthat::expect_identical(
            got,
            character(0)
        )
    }
)

testthat::test_that(
    "extract_rrid finds RRIDs in text and URLs",
    {
        txt <- c(
            "Antibody RRID:AB_262044.",
            "Tool https://scicrunch.org/resolver/RRID:SCR_007358",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "rrid"
        )

        testthat::expect_true(any(got[[1]] == "RRID:AB_262044"))
        testthat::expect_true(any(got[[2]] == "RRID:SCR_007358"))
        testthat::expect_identical(
            got[[3]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_rrid returns multiple matches from one string",
    {
        txt <- paste(
            "resources:",
            "RRID:AB_262044",
            "and RRID:SCR_007358"
        )

        got <- extract_scholid(
            txt,
            "rrid"
        )[[1]]

        testthat::expect_true(any(got == "RRID:AB_262044"))
        testthat::expect_true(any(got == "RRID:SCR_007358"))
    }
)

testthat::test_that(
    "extract_rrid rejects unknown authority candidates",
    {
        txt <- "see RRID:UNKNOWN_123 for details"

        got <- extract_scholid(
            txt,
            "rrid"
        )[[1]]

        testthat::expect_identical(
            got,
            character(0)
        )
    }
)

testthat::test_that(
    "extract_rrid does not extract bare local IDs without RRID label",
    {
        txt <- "antibody AB_262044 was used"

        got <- extract_scholid(
            txt,
            "rrid"
        )[[1]]

        testthat::expect_identical(
            got,
            character(0)
        )
    }
)

testthat::test_that(
    "extract_openalex finds OpenAlex IDs in text and URLs",
    {
        txt <- c(
            "Work https://openalex.org/W2741809807.",
            "Author https://api.openalex.org/authors/A5023888391",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "openalex"
        )

        testthat::expect_true(any(got[[1]] == "W2741809807"))
        testthat::expect_true(any(got[[2]] == "A5023888391"))
        testthat::expect_identical(
            got[[3]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_openalex returns multiple matches from one string",
    {
        txt <- paste(
            "entities:",
            "W2741809807",
            "and A5023888391"
        )

        got <- extract_scholid(
            txt,
            "openalex"
        )[[1]]

        testthat::expect_true(any(got == "W2741809807"))
        testthat::expect_true(any(got == "A5023888391"))
    }
)

testthat::test_that(
    "extract_openalex rejects deprecated concept and invalid candidates",
    {
        txt <- "see https://openalex.org/C12345678 and W123 for details"

        got <- extract_scholid(
            txt,
            "openalex"
        )[[1]]

        testthat::expect_identical(
            got,
            character(0)
        )
    }
)

testthat::test_that(
    "extract_openalex does not extract unrelated letter-digit tokens",
    {
        txt <- "version v2741809807 was noted"

        got <- extract_scholid(
            txt,
            "openalex"
        )[[1]]

        testthat::expect_identical(
            got,
            character(0)
        )
    }
)

testthat::test_that(
    "extract_isbn finds ISBN-10 and ISBN-13 with hyphens/spaces",
    {
        txt <- c(
            "ISBN 0-306-40615-2",
            "ISBN 978 0 306 40615 7",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "isbn"
        )

        testthat::expect_true(length(got[[1]]) >= 1L)
        testthat::expect_true(length(got[[2]]) >= 1L)
        testthat::expect_identical(
            got[[3]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_issn finds ISSN with X and ignores non-matches",
    {
        txt <- c(
            "ISSN 2434-561X",
            "no issn here"
        )

        got <- extract_scholid(
            txt,
            "issn"
        )

        testthat::expect_true(any(got[[1]] == "2434-561X"))
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_arxiv rejects partial matches inside larger tokens",
    {
        txt <- c(
            "bad modern 21011.12345",
            "bad version 2101.12345v",
            "bad version hep-th/9901001v",
            "phone-like 2101.12345-90",
            "slash tail 2101.12345/extra",
            "arXiv:2101.12345v2",
            "legacy hep-th/9901001v3"
        )

        got <- extract_scholid(
            txt,
            "arxiv"
        )

        testthat::expect_identical(
            got[[1]],
            character(0)
        )
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
        testthat::expect_identical(
            got[[3]],
            character(0)
        )
        testthat::expect_identical(
            got[[4]],
            character(0)
        )
        testthat::expect_identical(
            got[[5]],
            character(0)
        )
        testthat::expect_identical(
            got[[6]],
            "2101.12345v2"
        )
        testthat::expect_identical(
            got[[7]],
            "hep-th/9901001v3"
        )
    }
)

testthat::test_that(
    "extract_arxiv handles wrappers and multiple valid matches",
    {
        txt <- c(
            "Quoted '2101.12345'.",
            "Wrapped (2101.12345v2).",
            "Two IDs: 2101.12345 and hep-th/9901001v2.",
            "[hep-th/9901001]",
            "{math/0303001}."
        )

        got <- extract_scholid(
            txt,
            "arxiv"
        )

        testthat::expect_identical(
            got[[1]],
            "2101.12345"
        )
        testthat::expect_identical(
            got[[2]],
            "2101.12345v2"
        )
        testthat::expect_identical(
            got[[3]],
            c("2101.12345", "hep-th/9901001v2")
        )
        testthat::expect_identical(
            got[[4]],
            "hep-th/9901001"
        )
        testthat::expect_identical(
            got[[5]],
            "math/0303001"
        )
    }
)

testthat::test_that(
    "extract_arxiv finds modern and legacy arXiv identifiers",
    {
        txt <- c(
            "arXiv:2101.00001v2",
            "legacy hep-th/9901001v2"
        )

        got <- extract_scholid(
            txt,
            "arxiv"
        )

        testthat::expect_true(any(got[[1]] == "2101.00001v2"))
        testthat::expect_true(any(got[[2]] == "hep-th/9901001v2"))
    }
)

testthat::test_that(
    "extract_pmcid extracts PMCID tokens",
    {
        txt <- c(
            "PMCID: PMC12345",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "pmcid"
        )

        testthat::expect_true(any(got[[1]] == "PMC12345"))
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_pmid rejects hyphen slash and decimal false positives",
    {
        txt <- c(
            "phone-like 12345678-90",
            "slash tail 12345678/extra",
            "double token 12345678-87654321",
            "path /12345678/",
            "decimal 1234.5678",
            "PMID: 12345678.",
            "Wrapped (7654321).",
            "Noise: abc 3456789 xyz",
            "PMCID PMC1234567"
        )

        got <- extract_scholid(
            txt,
            "pmid"
        )

        testthat::expect_identical(
            got[[1]],
            character(0)
        )
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
        testthat::expect_identical(
            got[[3]],
            character(0)
        )
        testthat::expect_identical(
            got[[4]],
            character(0)
        )
        testthat::expect_identical(
            got[[5]],
            character(0)
        )
        testthat::expect_identical(
            got[[6]],
            "12345678"
        )
        testthat::expect_identical(
            got[[7]],
            "7654321"
        )
        testthat::expect_identical(
            got[[8]],
            "3456789"
        )
        testthat::expect_identical(
            got[[9]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_pmid respects token boundaries in mixed text",
    {
        txt <- c(
            "x12345678y",
            "prefix_12345678",
            "suffix 12345678_extra",
            "mixed A12345678",
            "surrounded by spaces 12345678 okay"
        )

        got <- extract_scholid(
            txt,
            "pmid"
        )

        testthat::expect_identical(
            got[[1]],
            character(0)
        )
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
        testthat::expect_identical(
            got[[3]],
            character(0)
        )
        testthat::expect_identical(
            got[[4]],
            character(0)
        )
        testthat::expect_identical(
            got[[5]],
            "12345678"
        )
    }
)

testthat::test_that(
    "extract_pmcid rejects embedded and adjacent false positives",
    {
        txt <- c(
            "xPMC1234567y",
            "prefix_PMC1234567",
            "suffix PMC1234567_extra",
            "mixed APMC1234567",
            "phone-like PMC1234567-90",
            "slash tail PMC1234567/extra",
            "double token PMC1234567-PMC7654321",
            "path /PMC1234567/",
            "PMC12x345",
            "surrounded by spaces PMC1234567 okay"
        )

        got <- extract_scholid(
            txt,
            "pmcid"
        )

        testthat::expect_identical(
            got[[1]],
            character(0)
        )
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
        testthat::expect_identical(
            got[[3]],
            character(0)
        )
        testthat::expect_identical(
            got[[4]],
            character(0)
        )
        testthat::expect_identical(
            got[[5]],
            character(0)
        )
        testthat::expect_identical(
            got[[6]],
            character(0)
        )
        testthat::expect_identical(
            got[[7]],
            character(0)
        )
        testthat::expect_identical(
            got[[8]],
            character(0)
        )
        testthat::expect_identical(
            got[[9]],
            character(0)
        )
        testthat::expect_identical(
            got[[10]],
            "PMC1234567"
        )
    }
)

testthat::test_that(
    "extract_pmcid keeps wrapped and sentence-final valid values",
    {
        txt <- c(
            "PMCID: PMC1234567.",
            "Wrapped (PMC7654321).",
            "Noise: abc PMC9999999 xyz",
            "PMID 12345678"
        )

        got <- extract_scholid(
            txt,
            "pmcid"
        )

        testthat::expect_identical(
            got[[1]],
            "PMC1234567"
        )
        testthat::expect_identical(
            got[[2]],
            "PMC7654321"
        )
        testthat::expect_identical(
            got[[3]],
            "PMC9999999"
        )
        testthat::expect_identical(
            got[[4]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_pmid does not treat PMCID digits as PMID",
    {
        txt <- c(
            "PMC12345 and PMID 1234567",
            "just PMC99999"
        )

        got <- extract_scholid(
            txt,
            "pmid"
        )

        testthat::expect_true(any(got[[1]] == "1234567"))
        testthat::expect_false(any(got[[1]] == "12345"))
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_scholid works across multiple ID types",
    {
    txt <- c(
        "doi:10.1000/182",
        "ORCID 0000-0002-1825-0097",
        "arXiv:2101.00001v2",
        "OpenAlex https://openalex.org/W2741809807",
        "PMCID: PMC12345",
        "PMID 1234567",
        "ISSN 2434-561X",
        "ISBN 0-306-40615-2",
        "ROR https://ror.org/01an7q238",
        "RRID:AB_262044",
        "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2"
    )

    got_doi <- extract_scholid(txt, "doi")
    got_orc <- extract_scholid(txt, "orcid")
    got_arx <- extract_scholid(txt, "arxiv")
    got_oa <- extract_scholid(txt, "openalex")
    got_pmc <- extract_scholid(txt, "pmcid")
    got_pmi <- extract_scholid(txt, "pmid")
    got_isn <- extract_scholid(txt, "issn")
    got_isb <- extract_scholid(txt, "isbn")
    got_ror <- extract_scholid(txt, "ror")
    got_rrid <- extract_scholid(txt, "rrid")
    got_swhid <- extract_scholid(txt, "swhid")

    testthat::expect_true(length(got_doi[[1]]) >= 1L)
    testthat::expect_true(length(got_orc[[2]]) >= 1L)
    testthat::expect_true(length(got_arx[[3]]) >= 1L)
    testthat::expect_true(length(got_oa[[4]]) >= 1L)
    testthat::expect_true(length(got_pmc[[5]]) >= 1L)
    testthat::expect_true(length(got_pmi[[6]]) >= 1L)
    testthat::expect_true(length(got_isn[[7]]) >= 1L)
    testthat::expect_true(length(got_isb[[8]]) >= 1L)
    testthat::expect_true(length(got_ror[[9]]) >= 1L)
    testthat::expect_true(length(got_rrid[[10]]) >= 1L)
    testthat::expect_true(length(got_swhid[[11]]) >= 1L)
    }
)

testthat::test_that(
    "extract_pmid does not extract digits from PMC tokens",
    {
        txt <- c(
            "PMC12345 1234567 PMC99999 7654321",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "pmid"
        )

        testthat::expect_identical(
            got[[1]],
            c("1234567", "7654321")
        )
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_issn returns empty when there is no ISSN pattern",
    {
        txt <- c(
            "no issn here",
            NA_character_
        )

        got <- extract_scholid(
            txt,
            "issn"
        )

        testthat::expect_identical(
            got[[1]],
            character(0)
        )
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_arxiv returns empty for NA inputs",
    {
        txt <- c(
            NA_character_,
            "arXiv:2101.00001v2"
        )

        got <- extract_scholid(
            txt,
            "arxiv"
        )

        testthat::expect_identical(
            got[[1]],
            character(0)
        )
        testthat::expect_true(length(got[[2]]) >= 1L)
    }
)


testthat::test_that("extract_doi removes trailing prose punctuation", {
    txt <- c(
        "See doi:10.1000/182.",
        "See (10.1000/182).",
        "See 10.1000/182, and then more text.",
        "Quoted DOI: '10.1000/182'; end.",
        "Markdown link: [paper](https://doi.org/10.1000/182).",
        "Nested punctuation ((10.1000/182)).",
        "Square bracket [10.1000/182],",
        "Curly brace {10.1000/182}.",
        "Double quote \"10.1000/182\".",
        "Single quote '10.1000/182'."
    )

    expected <- list(
        "10.1000/182",
        "10.1000/182",
        "10.1000/182",
        "10.1000/182",
        "10.1000/182",
        "10.1000/182",
        "10.1000/182",
        "10.1000/182",
        "10.1000/182",
        "10.1000/182"
    )

    testthat::expect_identical(extract_doi(txt), expected)
})


testthat::test_that("extract_doi handles multiple matches and missing values", {
    txt <- c(
        "Two DOIs: 10.1000/182; 10.5555/12345678.",
        "No DOI here.",
        NA_character_
    )

    expected <- list(
        c("10.1000/182", "10.5555/12345678"),
        character(0),
        character(0)
    )

    testthat::expect_identical(extract_doi(txt), expected)
})


testthat::test_that("extract_doi preserves valid DOI-internal punctuation", {
    txt <- c(
        paste0(
            "Potentially tricky DOI: ",
            "10.1002/(SICI)1097-4571(199205)43:4<284::AID-ASI5>3.0.CO;2-0."
        ),
        "Another punctuation-heavy DOI: 10.1207/s15327965pli1503_02.",
        "DOI inside URL: https://doi.org/10.1207/s15327965pli1503_02.",
        "DOI with query-like tail in prose: 10.1000/abc_def-ghi.jkl;",
        "Parenthetical context: (10.1207/s15327965pli1503_02)."
    )

    expected <- list(
        "10.1002/(SICI)1097-4571(199205)43:4<284::AID-ASI5>3.0.CO;2-0",
        "10.1207/s15327965pli1503_02",
        "10.1207/s15327965pli1503_02",
        "10.1000/abc_def-ghi.jkl",
        "10.1207/s15327965pli1503_02"
    )

    testthat::expect_identical(extract_doi(txt), expected)
})


testthat::test_that("extract_doi removes simple markup wrappers", {
    txt <- c(
        "<https://doi.org/10.1000/182>",
        "Angle wrapped DOI: <10.1000/182>.",
        "Mixed wrapper: (<10.1000/182>).",
        "[doi](https://doi.org/10.1000/182)",
        "href='https://doi.org/10.1000/182'",
        "href=\"https://doi.org/10.1000/182\"",
        "<a href=\"https://doi.org/10.1000/182\">paper</a>"
    )

    expected <- list(
        "10.1000/182",
        "10.1000/182",
        "10.1000/182",
        "10.1000/182",
        "10.1000/182",
        "10.1000/182",
        "10.1000/182"
    )

    testthat::expect_identical(extract_doi(txt), expected)
})


testthat::test_that(
    "extract_doi does not start matching inside larger tokens", {
    txt <- c(
        "Messy text: xx10.1000/182yy",
        "Messy text: xx_10.1000/182yy",
        "Messy text: xx 10.1000/182 yy",
        "Messy text: (10.1000/182)yy"
    )

    out <- extract_doi(txt)

    testthat::expect_identical(out[[1]], character(0))
    testthat::expect_identical(out[[2]], character(0))
    testthat::expect_identical(out[[3]], "10.1000/182")
    testthat::expect_identical(out[[4]], "10.1000/182")
})


testthat::test_that(
    "extract_isbn filters false positives via ISBN validation", {
    txt <- c(
        "ISBN 978-0-306-40615-7",
        "ISBN 0-306-40615-2",
        "Order number 12-3456-7890",
        "Phone: 0043 123 4567890",
        "Random digits 1234 5678 90X",
        "Code 978 0 306 40615 7",
        "Noise: abc 0-306-40615-2 xyz"
    )

    got <- extract_scholid(txt, "isbn")

    testthat::expect_identical(got[[1]], "978-0-306-40615-7")
    testthat::expect_identical(got[[2]], "0-306-40615-2")
    testthat::expect_identical(got[[3]], character(0))
    testthat::expect_identical(got[[4]], character(0))
    testthat::expect_identical(got[[5]], character(0))
    testthat::expect_identical(got[[6]], "978 0 306 40615 7")
    testthat::expect_identical(got[[7]], "0-306-40615-2")
})


testthat::test_that(
    "extract_isbn rejects false positives and token-internal matches",
    {
        txt <- c(
            "Order number 12-3456-7890",
            "Phone: 0043 123 4567890",
            "Random digits 1234 5678 90X",
            "Prefix noise xx978-0-306-40615-7",
            "Suffix noise 978-0-306-40615-7yy"
        )

        got <- extract_scholid(
            txt,
            "isbn"
        )

        testthat::expect_identical(
            got[[1]],
            character(0)
        )
        testthat::expect_identical(
            got[[2]],
            character(0)
        )
        testthat::expect_identical(
            got[[3]],
            character(0)
        )
        testthat::expect_identical(
            got[[4]],
            character(0)
        )
        testthat::expect_identical(
            got[[5]],
            character(0)
        )
    }
)

testthat::test_that(
    "extract_isbn keeps valid forms and strips surrounding punctuation",
    {
        txt <- c(
            "ISBN 978-0-306-40615-7.",
            "Wrapped (0-306-40615-2).",
            "Code 978 0 306 40615 7",
            "Noise: abc 0-306-40615-2 xyz"
        )

        got <- extract_scholid(
            txt,
            "isbn"
        )

        testthat::expect_identical(
            got[[1]],
            "978-0-306-40615-7"
        )
        testthat::expect_identical(
            got[[2]],
            "0-306-40615-2"
        )
        testthat::expect_identical(
            got[[3]],
            "978 0 306 40615 7"
        )
        testthat::expect_identical(
            got[[4]],
            "0-306-40615-2"
        )
    }
)


testthat::test_that(
    "extract_issn respects token boundaries and wrappers",
    {
        txt <- c(
            "ISSN 2434-561X",
            "Quoted '2434-561X'.",
            "Wrapped (2434-561X).",
            "Noise: abc 2434-561X xyz",
            "Phone-like 2434-561X-90",
            "Double token 2434-561X-0378-5955",
            "Prefix hyphen abc-2434-561X",
            "Surrounded by spaces 2434-561X okay"
        )

        got <- extract_scholid(
            txt,
            "issn"
        )

        testthat::expect_identical(
            got[[1]],
            "2434-561X"
        )
        testthat::expect_identical(
            got[[2]],
            "2434-561X"
        )
        testthat::expect_identical(
            got[[3]],
            "2434-561X"
        )
        testthat::expect_identical(
            got[[4]],
            "2434-561X"
        )
        testthat::expect_identical(
            got[[5]],
            character(0)
        )
        testthat::expect_identical(
            got[[6]],
            character(0)
        )
        testthat::expect_identical(
            got[[7]],
            character(0)
        )
        testthat::expect_identical(
            got[[8]],
            "2434-561X"
        )
    }
)

testthat::test_that(
    "detect_scholid_type detects wrapped issn values",
    {
        x <- c(
            "2434-561X",
            "0378-5955",
            "ISSN 2434-561X",
            "issn: 0378-5955",
            "2434-561x",
            "not an issn"
        )

        got <- detect_scholid_type(x)

        testthat::expect_identical(
            got,
            c("issn", "issn", "issn", "issn", "issn", NA_character_)
        )
    }
)
