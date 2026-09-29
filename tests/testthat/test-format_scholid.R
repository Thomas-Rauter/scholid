# Types without a form. Every other type has it.
format_no_form <- list(
    url   = "isbn",
    curie = "bibcode",
    doi   = setdiff(scholid_types(), "arxiv")
)

format_has_form <- function(type, as) {
    !type %in% format_no_form[[as]]
}

testthat::test_that(
    "format_scholid writes each type's resolver URL",
    {
        x <- list(
            doi = "10.1000/182",
            arxiv = "2101.00001v1",
            bibcode = "1998AJ....116.1009R",
            openalex = "W2741809807",
            swhid = "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            ark = "ark:/12148/btv1b8449691v",
            isni = "0000000121032683",
            orcid = "0000-0002-1825-0097",
            ror = "01an7q238",
            rrid = "RRID:AB_262044",
            uniprot = "P12345",
            refseq = c("NM_001744.6", "NP_001735.1"),
            sra = "SRR1553610",
            geo = "GSE2553",
            bioproject = "PRJNA257197",
            assembly = "GCF_000001405.40",
            issn = "0317-8471",
            pmcid = "PMC1234567",
            pmid = "12345678"
        )
        exp <- list(
            doi = "https://doi.org/10.1000/182",
            arxiv = "https://arxiv.org/abs/2101.00001v1",
            bibcode = "https://ui.adsabs.harvard.edu/abs/1998AJ....116.1009R",
            openalex = "https://openalex.org/W2741809807",
            swhid = paste0(
                "https://archive.softwareheritage.org/",
                "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2"
            ),
            ark = "https://n2t.net/ark:/12148/btv1b8449691v",
            isni = "https://isni.org/isni/0000000121032683",
            orcid = "https://orcid.org/0000-0002-1825-0097",
            ror = "https://ror.org/01an7q238",
            rrid = "https://scicrunch.org/resolver/RRID:AB_262044",
            uniprot = "https://www.uniprot.org/uniprotkb/P12345",
            refseq = c(
                "https://www.ncbi.nlm.nih.gov/nuccore/NM_001744.6",
                "https://www.ncbi.nlm.nih.gov/protein/NP_001735.1"
            ),
            sra = "https://www.ncbi.nlm.nih.gov/sra/SRR1553610",
            geo = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE2553",
            bioproject = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA257197",
            assembly = paste0(
                "https://www.ncbi.nlm.nih.gov/datasets/genome/",
                "GCF_000001405.40"
            ),
            issn = "https://portal.issn.org/resource/ISSN/0317-8471",
            pmcid = "https://pmc.ncbi.nlm.nih.gov/articles/PMC1234567/",
            pmid = "https://pubmed.ncbi.nlm.nih.gov/12345678/"
        )

        testthat::expect_setequal(
            names(x),
            setdiff(scholid_types(), format_no_form$url)
        )
        for (t in names(x)) {
            testthat::expect_identical(
                format_scholid(x[[t]], t, "url"),
                exp[[t]],
                info = t
            )
        }
    }
)

testthat::test_that(
    "format_scholid writes each type's CURIE",
    {
        x <- list(
            doi = "10.1000/182",
            arxiv = "2101.00001v1",
            openalex = "W2741809807",
            swhid = "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            ark = "ark:/12148/btv1b8449691v",
            isni = "0000000121032683",
            orcid = "0000-0002-1825-0097",
            ror = "01an7q238",
            rrid = "RRID:AB_262044",
            uniprot = "P12345",
            refseq = "NM_001744.6",
            sra = "SRR1553610",
            geo = "GSE2553",
            bioproject = "PRJNA257197",
            assembly = "GCF_000001405.40",
            isbn = "9780306406157",
            issn = "0317-8471",
            pmcid = "PMC1234567",
            pmid = "12345678"
        )
        exp <- list(
            doi = "doi:10.1000/182",
            arxiv = "arxiv:2101.00001v1",
            openalex = "openalex:W2741809807",
            swhid = "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            ark = "ark:/12148/btv1b8449691v",
            isni = "isni:0000000121032683",
            orcid = "orcid:0000-0002-1825-0097",
            ror = "ror:01an7q238",
            rrid = "RRID:AB_262044",
            uniprot = "uniprot:P12345",
            refseq = "refseq:NM_001744.6",
            sra = "insdc.sra:SRR1553610",
            geo = "geo:GSE2553",
            bioproject = "bioproject:PRJNA257197",
            assembly = "ncbi.assembly:GCF_000001405.40",
            isbn = "isbn:9780306406157",
            issn = "issn:0317-8471",
            pmcid = "pmc:PMC1234567",
            pmid = "pubmed:12345678"
        )

        testthat::expect_setequal(
            names(x),
            setdiff(scholid_types(), format_no_form$curie)
        )
        for (t in names(x)) {
            testthat::expect_identical(
                format_scholid(x[[t]], t, "curie"),
                exp[[t]],
                info = t
            )
        }
    }
)

testthat::test_that(
    "format_scholid writes arXiv DOIs without the version",
    {
        x <- c(
            "2101.00001",
            "2101.00001v1",
            "arXiv:2101.00001v2",
            "https://arxiv.org/abs/2101.00001v1",
            "10.48550/arXiv.2101.00001",
            "https://doi.org/10.48550/arXiv.2101.00001",
            "1706.03762v7",
            "hep-th/9901001",
            "hep-th/9901001v3",
            "arXiv:math/0303001"
        )
        exp <- c(
            rep("10.48550/arXiv.2101.00001", 6),
            "10.48550/arXiv.1706.03762",
            rep("10.48550/arXiv.hep-th/9901001", 2),
            "10.48550/arXiv.math/0303001"
        )

        got <- format_scholid(x, "arxiv", "doi")

        testthat::expect_identical(got, exp)
        testthat::expect_true(all(is_scholid(got, "doi")))
    }
)

testthat::test_that(
    "format_scholid leaves the subject class out of old-style arXiv DOIs",
    {
        x <- c(
            "math.GT/0309136",
            "math.GT/0309136v1",
            "https://arxiv.org/abs/math.GT/0309136"
        )

        testthat::expect_identical(
            format_scholid(x, "arxiv", "doi"),
            rep("10.48550/arXiv.math/0309136", 3)
        )
        testthat::expect_identical(
            format_scholid(x, "arxiv", "url"),
            c(
                "https://arxiv.org/abs/math.GT/0309136",
                "https://arxiv.org/abs/math.GT/0309136v1",
                "https://arxiv.org/abs/math.GT/0309136"
            )
        )
    }
)

testthat::test_that(
    "format_scholid keeps the arXiv version in URLs and CURIEs",
    {
        x <- c(
            "2101.00001v1",
            "hep-th/9901001v3"
        )

        testthat::expect_identical(
            format_scholid(x, "arxiv", "url"),
            c(
                "https://arxiv.org/abs/2101.00001v1",
                "https://arxiv.org/abs/hep-th/9901001v3"
            )
        )
        testthat::expect_identical(
            format_scholid(x, "arxiv", "curie"),
            c(
                "arxiv:2101.00001v1",
                "arxiv:hep-th/9901001v3"
            )
        )
    }
)

testthat::test_that(
    "format_scholid picks the RefSeq URL by prefix",
    {
        x <- c(
            "NM_001744.6",
            "NR_046018.2",
            "NC_045512.2",
            "NZ_CASIGT010000001.1",
            "XM_011520000.1",
            "NP_001735.1",
            "XP_011518300.1",
            "YP_009724390.1",
            "WP_000001234.1",
            "AP_000001.1"
        )
        exp <- c(
            paste0("https://www.ncbi.nlm.nih.gov/nuccore/", x[1:5]),
            paste0("https://www.ncbi.nlm.nih.gov/protein/", x[6:10])
        )

        testthat::expect_identical(
            format_scholid(x, "refseq", "url"),
            exp
        )
    }
)

testthat::test_that(
    "format_scholid percent-encodes %, #, and ? in DOI URLs only",
    {
        x <- c(
            "10.1000/456#789",
            "10.1000/a?b=c",
            "10.1000/a%2Fb",
            "10.1000/50%off",
            "10.1000/a#b?c%d"
        )

        testthat::expect_identical(
            format_scholid(x, "doi", "url"),
            c(
                "https://doi.org/10.1000/456%23789",
                "https://doi.org/10.1000/a%3Fb=c",
                "https://doi.org/10.1000/a%252Fb",
                "https://doi.org/10.1000/50%25off",
                "https://doi.org/10.1000/a%23b%3Fc%25d"
            )
        )
        testthat::expect_identical(
            format_scholid(x, "doi", "curie"),
            paste0("doi:", x)
        )
        testthat::expect_identical(
            normalize_scholid(format_scholid(x, "doi", "url"), "doi"),
            x
        )
    }
)

testthat::test_that(
    "format_scholid writes other DOI characters as they are",
    {
        x <- c(
            paste0(
                "10.1002/(SICI)1097-4571(199205)43:4",
                "<284::AID-ASI5>3.0.CO;2-0"
            ),
            "10.1000/a\u2013b",
            "10.1000/caf\u00e9",
            "10.48550/arXiv.hep-th/9901001"
        )

        testthat::expect_identical(
            format_scholid(x, "doi", "url"),
            paste0("https://doi.org/", x)
        )
    }
)

testthat::test_that(
    "cross-type: normalize_scholid reads back format_scholid URLs and CURIEs",
    {
        for (t in scholid_types()) {
            x <- scholid_type_inputs[[t]]
            for (as in c("url", "curie")) {
                if (!format_has_form(t, as)) {
                    next
                }
                testthat::expect_identical(
                    normalize_scholid(format_scholid(x, t, as), t),
                    normalize_scholid(x, t),
                    info = paste(t, as)
                )
            }
        }
    }
)

testthat::test_that(
    "normalize_scholid reads back arXiv DOIs as the unversioned identifier",
    {
        x <- scholid_type_inputs$arxiv
        back <- normalize_scholid(format_scholid(x, "arxiv", "doi"), "arxiv")
        # arXiv DOIs leave out the version and the subject class.
        exp <- sub("\\.[A-Z]{2}/", "/", scholid_key(x, "arxiv"))

        testthat::expect_identical(back, exp)
        testthat::expect_false(any(grepl("v[0-9]+$", back)))
    }
)

testthat::test_that(
    "cross-type: format_scholid is NA exactly where x doesn't normalize",
    {
        for (t in scholid_types()) {
            x <- scholid_type_inputs[[t]]
            for (as in c("url", "curie", "doi")) {
                if (!format_has_form(t, as)) {
                    next
                }
                got <- format_scholid(x, t, as)

                testthat::expect_type(got, "character")
                testthat::expect_identical(
                    is.na(got),
                    is.na(normalize_scholid(x, t)),
                    info = paste(t, as)
                )
            }
        }
    }
)

testthat::test_that(
    "format_scholid gives the same output for wrapped and canonical input",
    {
        x <- list(
            doi = c(
                "10.1000/182",
                "https://doi.org/10.1000/182",
                "doi:10.1000/182",
                " DOI: 10.1000/182. "
            ),
            arxiv = c(
                "2101.00001v1",
                "arXiv:2101.00001v1",
                "https://arxiv.org/abs/2101.00001v1",
                "arxiv:2101.00001v1"
            ),
            orcid = c(
                "0000-0002-1825-0097",
                "https://orcid.org/0000-0002-1825-0097",
                "orcid:0000-0002-1825-0097",
                "0000000218250097"
            ),
            rrid = c(
                "RRID:AB_262044",
                "rrid:AB_262044",
                "RRID: AB_262044",
                "https://scicrunch.org/resolver/RRID:AB_262044"
            ),
            refseq = c(
                "NM_001744.6",
                "nm_001744.6",
                "refseq:NM_001744.6",
                "https://www.ncbi.nlm.nih.gov/nuccore/NM_001744.6"
            ),
            isbn = c(
                "9780306406157",
                "ISBN 978-0-306-40615-7",
                "isbn:9780306406157"
            ),
            pmcid = c(
                "PMC1234567",
                "PMCID: PMC1234567",
                "pmc:PMC1234567",
                "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1234567/"
            ),
            pmid = c(
                "12345678",
                "PMID: 12345678",
                "pubmed:12345678",
                "https://www.ncbi.nlm.nih.gov/pubmed/12345678"
            )
        )

        for (t in names(x)) {
            for (as in c("url", "curie", "doi")) {
                if (!format_has_form(t, as)) {
                    next
                }
                got <- format_scholid(x[[t]], t, as)
                testthat::expect_false(anyNA(got), info = paste(t, as))
                testthat::expect_identical(
                    got,
                    rep(got[[1]], length(got)),
                    info = paste(t, as)
                )
            }
        }
    }
)

testthat::test_that(
    "format_scholid gives NA for NA, empty, and invalid input",
    {
        x <- c(
            NA,
            "",
            "   ",
            "not an identifier",
            NA
        )
        for (t in scholid_types()) {
            for (as in c("url", "curie", "doi")) {
                if (!format_has_form(t, as)) {
                    next
                }
                testthat::expect_identical(
                    format_scholid(x, t, as),
                    rep(NA_character_, length(x)),
                    info = paste(t, as)
                )
            }
        }

        testthat::expect_identical(
            format_scholid(NA, "doi"),
            NA_character_
        )
        testthat::expect_identical(
            format_scholid(character(0), "doi"),
            character(0)
        )
        testthat::expect_identical(
            format_scholid(c("10.1000/182", "not a doi", NA), "doi"),
            c("https://doi.org/10.1000/182", NA, NA)
        )
    }
)

testthat::test_that(
    "format_scholid writes URLs by default and matches `as` like match.arg",
    {
        testthat::expect_identical(
            format_scholid("12345678", "pmid"),
            "https://pubmed.ncbi.nlm.nih.gov/12345678/"
        )
        testthat::expect_identical(
            format_scholid("12345678", "pmid", "cur"),
            "pubmed:12345678"
        )
        testthat::expect_identical(
            format_scholid("2101.00001v1", "arxiv", "d"),
            "10.48550/arXiv.2101.00001"
        )
    }
)

testthat::test_that(
    "format_scholid errors for a form the type does not have",
    {
        testthat::expect_error(
            format_scholid("9780306406157", "isbn", "url"),
            paste0(
                "Type \"isbn\" has no \"url\" form. ",
                "Available forms for \"isbn\": \"curie\"."
            ),
            fixed = TRUE
        )
        testthat::expect_error(
            format_scholid("1998AJ....116.1009R", "bibcode", "curie"),
            paste0(
                "Type \"bibcode\" has no \"curie\" form. ",
                "Available forms for \"bibcode\": \"url\"."
            ),
            fixed = TRUE
        )
        testthat::expect_error(
            format_scholid("10.1000/182", "doi", "doi"),
            paste0(
                "Type \"doi\" has no \"doi\" form. ",
                "Available forms for \"doi\": \"url\", \"curie\"."
            ),
            fixed = TRUE
        )

        for (t in format_no_form$doi) {
            testthat::expect_error(
                format_scholid(character(0), t, "doi"),
                paste0("Type \"", t, "\" has no \"doi\" form."),
                fixed = TRUE
            )
        }
        testthat::expect_error(
            format_scholid(NA, "isbn", "url"),
            "has no \"url\" form",
            fixed = TRUE
        )
    }
)

testthat::test_that(
    "format_scholid validates `as`",
    {
        testthat::expect_error(
            format_scholid("10.1000/182", "doi", "link"),
            "should be one of"
        )
        testthat::expect_error(
            format_scholid("10.1000/182", "doi", NA_character_),
            "should be one of"
        )
        testthat::expect_error(
            format_scholid("10.1000/182", "doi", ""),
            "should be one of"
        )
        testthat::expect_error(
            format_scholid("10.1000/182", "doi", c("url", "curie")),
            "must be of length 1"
        )
        testthat::expect_error(
            format_scholid("10.1000/182", "doi", 1),
            "character vector"
        )
        testthat::expect_error(
            format_scholid("9780306406157", "isbn", "link"),
            "should be one of"
        )
    }
)

testthat::test_that(
    "format_scholid validates `type`",
    {
        testthat::expect_error(
            format_scholid("x"),
            "`type` is required"
        )
        testthat::expect_error(
            format_scholid(
                "x",
                NULL
            ),
            "`type` must not be NULL"
        )
        testthat::expect_error(
            format_scholid(
                "x",
                c("doi", "isbn")
            ),
            "`type` must be length 1"
        )
        testthat::expect_error(
            format_scholid(
                "x",
                ""
            ),
            "`type` must be a non-empty string"
        )
        testthat::expect_error(
            format_scholid(
                "x",
                "not_a_type"
            ),
            "should be one of"
        )
        testthat::expect_error(
            format_scholid(
                "x",
                "pmi"
            ),
            "abbreviations are not allowed"
        )
    }
)

testthat::test_that(
    "format_scholid validates `x`",
    {
        testthat::expect_error(
            format_scholid(type = "doi"),
            "`x` is required"
        )
        testthat::expect_error(
            format_scholid(
                NULL,
                "doi"
            ),
            "`x` must not be NULL"
        )
        testthat::expect_error(
            format_scholid(
                data.frame(x = 1),
                "doi"
            ),
            "data frame"
        )
    }
)
