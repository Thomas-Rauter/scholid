# Types with a key rule. The key of every other type is its normalized
# value.
key_rule_types <- c("doi", "arxiv", "swhid", "refseq", "assembly", "isbn")

key_non_ascii_dois <- c(
    "10.1000/café-x",
    "10.1000/Été",
    "10.1000/été",
    "10.1000/ıi",
    "10.1000/straße"
)

# Keys of the fixture inputs of every type, plus non-ASCII DOIs.
key_all_types <- function() {
    lapply(
        scholid_types(),
        function(t) {
            x <- scholid_type_inputs[[t]]
            if (identical(t, "doi")) {
                x <- c(x, key_non_ascii_dois)
            }
            scholid_key(x, t)
        }
    )
}

# Evaluates `code` with LC_CTYPE set to "C", and skips the test when the
# locale can't be set.
key_in_c_ctype <- function(code) {
    old <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", old), add = TRUE)
    set <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
    if (!identical(set, "C")) {
        testthat::skip("cannot set LC_CTYPE to C")
    }
    code
}

testthat::test_that(
    "scholid_key gives an ISBN-10 the key of its ISBN-13",
    {
        testthat::expect_identical(
            scholid_key(
                c(
                    "0306406152",
                    "978-0-306-40615-7",
                    "9780306406157",
                    "ISBN 0-306-40615-2"
                ),
                "isbn"
            ),
            rep("9780306406157", 4L)
        )
        testthat::expect_identical(
            scholid_key(
                c(
                    "080442957X",
                    "0-8044-2957-x",
                    "9780804429573"
                ),
                "isbn"
            ),
            rep("9780804429573", 3L)
        )
    }
)

testthat::test_that(
    "scholid_key keeps ISBN-13 values, including 979 ones",
    {
        x <- c(
            "9780306406157",
            "979-10-90636-07-1"
        )

        testthat::expect_identical(
            scholid_key(x, "isbn"),
            c(
                "9780306406157",
                "9791090636071"
            )
        )
    }
)

testthat::test_that(
    "scholid_key ISBN keys are valid ISBN-13s",
    {
        keys <- scholid_key(
            scholid_type_inputs$isbn,
            "isbn"
        )
        keys <- keys[!is.na(keys)]

        testthat::expect_true(length(keys) > 0L)
        testthat::expect_true(all(grepl("^[0-9]{13}$", keys)))
        testthat::expect_true(all(is_scholid(keys, "isbn")))
    }
)

testthat::test_that(
    "scholid_key drops new-style arXiv versions",
    {
        x <- c(
            "2101.00001v1",
            "2101.00001v2",
            "2101.00001",
            "arXiv:2101.00001v12",
            "https://arxiv.org/abs/2101.00001v2"
        )

        testthat::expect_identical(
            scholid_key(x, "arxiv"),
            rep("2101.00001", 5L)
        )
    }
)

testthat::test_that(
    "scholid_key drops old-style arXiv versions",
    {
        x <- c(
            "hep-th/9901001v3",
            "hep-th/9901001",
            "math.GT/0309136v1",
            "arXiv:math.GT/0309136",
            "solv-int/9901001v2"
        )

        testthat::expect_identical(
            scholid_key(x, "arxiv"),
            c(
                "hep-th/9901001",
                "hep-th/9901001",
                "math.GT/0309136",
                "math.GT/0309136",
                "solv-int/9901001"
            )
        )
    }
)

testthat::test_that(
    "scholid_key drops RefSeq versions",
    {
        x <- c(
            "NM_000546.5",
            "NM_000546.6",
            "refseq:nm_000546.5",
            "https://www.ncbi.nlm.nih.gov/nuccore/NM_000546.6",
            "NZ_CASIGT010000001.1"
        )

        testthat::expect_identical(
            scholid_key(x, "refseq"),
            c(
                rep("NM_000546", 4L),
                "NZ_CASIGT010000001"
            )
        )
    }
)

testthat::test_that(
    "scholid_key drops genome assembly versions",
    {
        x <- c(
            "GCF_000001405.40",
            "GCF_000001405.39",
            "https://www.ncbi.nlm.nih.gov/assembly/GCF_000001405.40",
            "GCA_000001405.29",
            "assembly:GCA_000001405.28"
        )

        testthat::expect_identical(
            scholid_key(x, "assembly"),
            c(
                rep("GCF_000001405", 3L),
                rep("GCA_000001405", 2L)
            )
        )
    }
)

testthat::test_that(
    "scholid_key keeps GCA and GCF accessions with the same number apart",
    {
        keys <- scholid_key(
            c(
                "GCA_000001405.29",
                "GCF_000001405.40"
            ),
            "assembly"
        )

        testthat::expect_false(anyNA(keys))
        testthat::expect_false(identical(keys[[1]], keys[[2]]))
    }
)

testthat::test_that(
    "scholid_key uppercases ASCII letters in DOIs",
    {
        x <- c(
            "10.1000/abc",
            "10.1000/ABC",
            "10.1000/AbC",
            "doi:10.1000/abc",
            "https://doi.org/10.1000/aBc"
        )

        testthat::expect_identical(
            scholid_key(x, "doi"),
            rep("10.1000/ABC", 5L)
        )
    }
)

testthat::test_that(
    "scholid_key keeps non-ASCII characters in DOIs as they are",
    {
        testthat::expect_identical(
            scholid_key(key_non_ascii_dois, "doi"),
            c(
                "10.1000/CAFé-X",
                "10.1000/ÉTé",
                "10.1000/éTé",
                "10.1000/ıI",
                "10.1000/STRAßE"
            )
        )
    }
)

testthat::test_that(
    "scholid_key DOI keys don't depend on the input encoding",
    {
        latin1 <- "10.1000/caf\xe9"
        Encoding(latin1) <- "latin1"

        testthat::expect_identical(
            scholid_key(latin1, "doi"),
            scholid_key("10.1000/café", "doi")
        )
    }
)

testthat::test_that(
    "scholid_key drops SWHID qualifiers",
    {
        core <- "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b"
        x <- c(
            core,
            paste0(
                core,
                ";origin=https://example.org/repo.git;lines=9-15"
            ),
            paste0(core, ";lines=1-2"),
            paste0(
                "https://archive.softwareheritage.org/",
                core,
                ";origin=https://example.org/repo.git"
            ),
            "SWH:1:CNT:4D99D2D18326621CCDD70F5EA66C2E2AC236AD8B"
        )

        testthat::expect_identical(
            scholid_key(x, "swhid"),
            rep(core, 5L)
        )
    }
)

testthat::test_that(
    "scholid_key gives the normalized value for types without a key rule",
    {
        for (t in setdiff(scholid_types(), key_rule_types)) {
            x <- scholid_type_inputs[[t]]
            testthat::expect_identical(
                scholid_key(x, t),
                normalize_scholid(x, t),
                info = t
            )
        }
    }
)

testthat::test_that(
    "cross-type: wrapped input gives the key of its normalized value",
    {
        for (t in scholid_types()) {
            x <- scholid_type_inputs[[t]]
            testthat::expect_identical(
                scholid_key(x, t),
                scholid_key(normalize_scholid(x, t), t),
                info = t
            )
        }
    }
)

testthat::test_that(
    "cross-type: scholid_key keeps length and is NA where x doesn't normalize",
    {
        for (t in scholid_types()) {
            x <- scholid_type_inputs[[t]]
            got <- scholid_key(x, t)

            testthat::expect_type(got, "character")
            testthat::expect_identical(
                length(got),
                length(x),
                info = t
            )
            testthat::expect_identical(
                is.na(got),
                is.na(normalize_scholid(x, t)),
                info = t
            )
        }
    }
)

testthat::test_that(
    "scholid_key gives NA for NA, empty, and invalid input",
    {
        x <- c(
            NA,
            "",
            "   ",
            "not an identifier",
            NA
        )
        for (t in scholid_types()) {
            testthat::expect_identical(
                scholid_key(x, t),
                rep(NA_character_, length(x)),
                info = t
            )
        }

        testthat::expect_identical(
            scholid_key(NA, "doi"),
            NA_character_
        )
        testthat::expect_identical(
            scholid_key(character(0), "isbn"),
            character(0)
        )
    }
)

testthat::test_that(
    "scholid_key finds duplicates that normalization keeps apart",
    {
        x <- c(
            "0306406152",
            "ISBN 978-0-306-40615-7",
            "9780804429573",
            "0-8044-2957-X"
        )

        testthat::expect_identical(
            duplicated(normalize_scholid(x, "isbn")),
            rep(FALSE, 4L)
        )
        testthat::expect_identical(
            duplicated(scholid_key(x, "isbn")),
            c(FALSE, TRUE, FALSE, TRUE)
        )
    }
)

testthat::test_that(
    "scholid_key keys are not always valid identifiers",
    {
        key <- scholid_key("NM_000546.5", "refseq")

        testthat::expect_identical(key, "NM_000546")
        testthat::expect_false(is_scholid(key, "refseq"))
    }
)

testthat::test_that(
    "scholid_key gives the same keys in the C locale",
    {
        before <- key_all_types()
        in_c <- key_in_c_ctype(key_all_types())

        testthat::expect_identical(in_c, before)
    }
)

testthat::test_that(
    "scholid_key validates `type`",
    {
        testthat::expect_error(
            scholid_key("x"),
            "`type` is required"
        )
        testthat::expect_error(
            scholid_key(
                "x",
                NULL
            ),
            "`type` must not be NULL"
        )
        testthat::expect_error(
            scholid_key(
                "x",
                c("doi", "isbn")
            ),
            "`type` must be length 1"
        )
        testthat::expect_error(
            scholid_key(
                "x",
                ""
            ),
            "`type` must be a non-empty string"
        )
        testthat::expect_error(
            scholid_key(
                "x",
                "not_a_type"
            ),
            "should be one of"
        )
        testthat::expect_error(
            scholid_key(
                "x",
                "pmi"
            ),
            "abbreviations are not allowed"
        )
    }
)

testthat::test_that(
    "scholid_key validates `x`",
    {
        testthat::expect_error(
            scholid_key(type = "doi"),
            "`x` is required"
        )
        testthat::expect_error(
            scholid_key(
                NULL,
                "doi"
            ),
            "`x` must not be NULL"
        )
        testthat::expect_error(
            scholid_key(
                data.frame(x = 1),
                "doi"
            ),
            "data frame"
        )
    }
)
