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
        "030640615X",
        NA_character_,
        NA_character_
    )

    testthat::expect_identical(got, exp)
})

testthat::test_that("normalize_issn strips prefix and enforces NNNN-NNNN", {
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
        "0317-8471",
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
    "normalize_orcid drops lowercase x and returns NA",
    {
        x <- c(
            "0000-0002-1694-233x",
            NA_character_
        )

        got <- normalize_orcid(x)

        testthat::expect_identical(
            got,
            c(NA_character_, NA_character_)
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
