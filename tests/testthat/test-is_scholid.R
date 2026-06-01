testthat::test_that(
    "is_scholid dispatches to is_<type>()",
    {
        x <- c(
            "10.1000/182",
            "not a doi",
            NA_character_
        )

        testthat::expect_identical(
            is_scholid(
                x,
                "doi"
            ),
            is_doi(x)
        )
    }
)

testthat::test_that(
    "is_scholid is vectorized and preserves NA",
    {
        x <- c(
            NA_character_,
            "10.1000/182"
        )

        out <- is_scholid(
            x,
            "doi"
        )

        testthat::expect_type(
            out,
            "logical"
        )
        testthat::expect_length(
            out,
            length(x)
        )
        testthat::expect_true(is.na(out[1]))
    }
)

testthat::test_that(
    "is_scholid validates `type` strictly",
    {
        testthat::expect_error(
            is_scholid(
                "x",
                NA_character_
            ),
            "`type` must be a non-empty string"
        )

        testthat::expect_error(
            is_scholid(
                "x",
                ""
            ),
            "`type` must be a non-empty string"
        )

        testthat::expect_error(
            is_scholid(
                "x",
                "not_a_type"
            ),
            "should be one of"
        )

        if (all(c("pmid", "pmcid") %in% scholid_types())) {
            testthat::expect_error(
                is_scholid(
                    "x",
                    "pmi"
                ),
                "abbreviations are not allowed"
            )
        }
    }
)

testthat::test_that(
    "is_scholid validates `x`",
    {
        testthat::expect_error(
            is_scholid(
                type = "doi"
            ),
            "`x` is required"
        )

        testthat::expect_error(
            is_scholid(
                NULL,
                "doi"
            ),
            "`x` must not be NULL"
        )

        testthat::expect_error(
            is_scholid(
                data.frame(x = 1),
                "doi"
            ),
            "data frame"
        )
    }
)

testthat::test_that(
    "is_doi accepts doi syntax and rejects spaces",
    {
        x <- c(
            "10.1000/182",
            "10.1000/with space",
            "10.1000",
            NA_character_
        )

        got <- is_scholid(
            x,
            "doi"
        )

        testthat::expect_identical(
            got,
            c(TRUE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_orcid accepts canonical valid ORCIDs including lowercase x",
    {
        x <- c(
            "0000-0002-1825-0097",
            "0000-0000-0000-001X",
            "0000-0000-0000-001x"
        )

        testthat::expect_identical(
            is_orcid(x),
            c(TRUE, TRUE, TRUE)
        )
    }
)

testthat::test_that(
    "is_orcid rejects canonical checksum-invalid ORCIDs",
    {
        x <- c(
            "0000-0002-1825-009X",
            "0000-0000-0000-0017",
            "0000-0000-0000-0010"
        )

        testthat::expect_identical(
            is_orcid(x),
            c(FALSE, FALSE, FALSE)
        )
    }
)

testthat::test_that(
    "ORCID normalization canonicalizes valid non-canonical inputs",
    {
        x <- c(
            "0000000218250097",
            "000000000000001x",
            "https://orcid.org/0000-0002-1825-0097",
            "orcid:0000-0000-0000-001x"
        )

        testthat::expect_identical(
            normalize_scholid(x, "orcid"),
            c(
                "0000-0002-1825-0097",
                "0000-0000-0000-001X",
                "0000-0002-1825-0097",
                "0000-0000-0000-001X"
            )
        )
    }
)

testthat::test_that(
    "normalized ORCID outputs validate and classify as orcid",
    {
        x <- normalize_scholid(
            c(
                "0000000218250097",
                "000000000000001x",
                "https://orcid.org/0000-0002-1825-0097",
                "orcid:0000-0000-0000-001x"
            ),
            "orcid"
        )

        testthat::expect_true(all(is_orcid(x)))
        testthat::expect_true(all(is_scholid(x, "orcid")))
        testthat::expect_true(all(classify_scholid(x) == "orcid"))
    }
)

testthat::test_that(
    "is_orcid validates checksum and allows X check digit",
    {
        x <- c(
            "0000-0002-1825-0097",
            "0000-0002-1694-233X",
            "0000-0002-1825-0098",
            "0000-0002-1825-009",
            NA_character_
        )

        got <- is_scholid(
            x,
            "orcid"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_ror accepts checksum-valid compact ROR iDs",
    {
        x <- c(
            "02mhbdp94",
            "01an7q238",
            "02s376052"
        )

        testthat::expect_identical(
            is_ror(x),
            c(TRUE, TRUE, TRUE)
        )
    }
)

testthat::test_that(
    "is_ror rejects checksum-invalid and malformed ROR iDs",
    {
        x <- c(
            "02mhbdp94",
            "02mhbdp99",
            "not-a-ror",
            "02mhbdp9",
            NA_character_
        )

        testthat::expect_identical(
            is_ror(x),
            c(TRUE, FALSE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_scholid dispatches to is_ror for type ror",
    {
        x <- c("01an7q238", "02mhbdp99", NA_character_)

        testthat::expect_identical(
            is_scholid(x, "ror"),
            is_ror(x)
        )
    }
)

testthat::test_that(
    "ROR normalization canonicalizes valid non-canonical inputs",
    {
        x <- c(
            "01an7q238",
            "https://ror.org/01an7q238",
            "ror.org/01an7q238",
            "ROR: 01an7q238"
        )

        testthat::expect_identical(
            normalize_scholid(x, "ror"),
            c(
                "01an7q238",
                "01an7q238",
                "01an7q238",
                "01an7q238"
            )
        )
    }
)

testthat::test_that(
    "normalized ROR outputs validate and classify as ror",
    {
        x <- normalize_scholid(
            c(
                "https://ror.org/01an7q238",
                "ROR: 02mhbdp94"
            ),
            "ror"
        )

        testthat::expect_true(all(is_ror(x)))
        testthat::expect_true(all(is_scholid(x, "ror")))
        testthat::expect_true(all(classify_scholid(x) == "ror"))
    }
)

testthat::test_that(
    "is_swhid accepts canonical core SWHIDs for known object types",
    {
        x <- c(
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "swh:1:dir:d198bc9d7a6bcf6db04f476d29314f157507d505",
            "swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d",
            "swh:1:rel:22ece559cc7cc2364edc5e5593d63ae8bd229f9f",
            "swh:1:snp:c7c108084bc0bf3d81436bf980b46e98bd338453"
        )

        testthat::expect_identical(
            is_swhid(x),
            rep(TRUE, length(x))
        )
    }
)

testthat::test_that(
    "is_swhid accepts canonical qualified SWHIDs with known qualifiers",
    {
        x <- paste0(
            "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;",
            "origin=https://example.org/repo.git;",
            "visit=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9;",
            "lines=9-15"
        )

        testthat::expect_true(is_swhid(x))
    }
)

testthat::test_that(
    "is_swhid rejects bare hex strings, non-canonical casing, and invalid forms",
    {
        x <- c(
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "94a9ed024d3859793618152ea559a168bbcbb5e2",
            "SWH:1:CNT:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "swh:2:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;unknown=foo",
            "not-a-swhid",
            NA_character_
        )

        testthat::expect_identical(
            is_swhid(x),
            c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_scholid dispatches to is_swhid for type swhid",
    {
        x <- c(
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "94a9ed024d3859793618152ea559a168bbcbb5e2",
            NA_character_
        )

        testthat::expect_identical(
            is_scholid(x, "swhid"),
            is_swhid(x)
        )
    }
)

testthat::test_that(
    "SWHID normalization canonicalizes valid labeled and URL inputs",
    {
        x <- c(
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "https://archive.softwareheritage.org/swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "SWH:1:CNT:94a9ed024d3859793618152ea559a168bbcbb5e2"
        )

        testthat::expect_identical(
            normalize_scholid(x, "swhid"),
            rep(
                "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
                3L
            )
        )
    }
)

testthat::test_that(
    "normalized SWHID outputs validate and classify as swhid",
    {
        x <- normalize_scholid(
            c(
                "https://archive.softwareheritage.org/swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d",
                "SWH:1:DIR:d198bc9d7a6bcf6db04f476d29314f157507d505"
            ),
            "swhid"
        )

        testthat::expect_true(all(is_swhid(x)))
        testthat::expect_true(all(is_scholid(x, "swhid")))
        testthat::expect_true(all(classify_scholid(x) == "swhid"))
    }
)

testthat::test_that(
    "is_swhid accepts canonical SWHIDs for known object types",
    {
        x <- c(
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "swh:1:dir:d198bc9d7a6bcf6db04f476d29314f157507d505",
            "swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d",
            "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;origin=https://gitorious.org/ocamlp3l/ocamlp3l_cvs.git;visit=swh:1:snp:d7f1b9eb7ccb596c2622c4780febaa02549830f9;lines=9-15"
        )

        testthat::expect_identical(
            is_swhid(x),
            rep(TRUE, length(x))
        )
    }
)

testthat::test_that(
    "is_swhid rejects bare hex strings and malformed SWHIDs",
    {
        x <- c(
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "94a9ed024d3859793618152ea559a168bbcbb5e2",
            "swh:2:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "SWH:1:CNT:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;unknown=foo",
            "not-a-swhid",
            NA_character_
        )

        testthat::expect_identical(
            is_swhid(x),
            c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_scholid dispatches to is_swhid for type swhid",
    {
        x <- c(
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "swh:2:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            NA_character_
        )

        testthat::expect_identical(
            is_scholid(x, "swhid"),
            is_swhid(x)
        )
    }
)

testthat::test_that(
    "SWHID normalization canonicalizes valid labeled and URL inputs",
    {
        x <- c(
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "https://archive.softwareheritage.org/swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
            "SWH:1:CNT:94a9ed024d3859793618152ea559a168bbcbb5e2"
        )

        testthat::expect_identical(
            normalize_scholid(x, "swhid"),
            rep(
                "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
                3L
            )
        )
    }
)

testthat::test_that(
    "normalized SWHID outputs validate and classify as swhid",
    {
        x <- normalize_scholid(
            c("https://archive.softwareheritage.org/swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d",
              "SWH:1:CNT:94a9ed024d3859793618152ea559a168bbcbb5e2"
            ),
            "swhid"
        )

        testthat::expect_true(all(is_swhid(x)))
        testthat::expect_true(all(is_scholid(x, "swhid")))
        testthat::expect_true(all(classify_scholid(x) == "swhid"))
    }
)

testthat::test_that(
    "is_rrid accepts canonical RRIDs for known authorities",
    {
        x <- c(
            "RRID:AB_262044",
            "RRID:CVCL_2260",
            "RRID:SCR_007358",
            "RRID:IMSR_JAX:000664",
            "RRID:MGI:3840442",
            "RRID:Addgene_80088"
        )

        testthat::expect_identical(
            is_rrid(x),
            rep(TRUE, length(x))
        )
    }
)

testthat::test_that(
    "is_rrid rejects bare local IDs and unknown authorities",
    {
        x <- c(
            "RRID:AB_262044",
            "AB_262044",
            "RRID:UNKNOWN_123",
            "not-a-rrid",
            NA_character_
        )

        testthat::expect_identical(
            is_rrid(x),
            c(TRUE, FALSE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_scholid dispatches to is_rrid for type rrid",
    {
        x <- c("RRID:AB_262044", "RRID:UNKNOWN_123", NA_character_)

        testthat::expect_identical(
            is_scholid(x, "rrid"),
            is_rrid(x)
        )
    }
)

testthat::test_that(
    "RRID normalization canonicalizes valid labeled and URL inputs",
    {
        x <- c(
            "RRID:AB_262044",
            "https://scicrunch.org/resolver/RRID:AB_262044",
            "RRID: AB_262044",
            "rrid:SCR_007358"
        )

        testthat::expect_identical(
            normalize_scholid(x, "rrid"),
            c(
                "RRID:AB_262044",
                "RRID:AB_262044",
                "RRID:AB_262044",
                "RRID:SCR_007358"
            )
        )
    }
)

testthat::test_that(
    "normalized RRID outputs validate and classify as rrid",
    {
        x <- normalize_scholid(
            c(
                "https://scicrunch.org/resolver/RRID:AB_262044",
                "RRID: SCR_007358"
            ),
            "rrid"
        )

        testthat::expect_true(all(is_rrid(x)))
        testthat::expect_true(all(is_scholid(x, "rrid")))
        testthat::expect_true(all(classify_scholid(x) == "rrid"))
    }
)

testthat::test_that(
    "is_openalex accepts canonical uppercase keys for known entity types",
    {
        x <- c(
            "W2741809807",
            "A5023888391",
            "I97018004",
            "S137773608",
            "T154945302",
            "F4320332160",
            "G12345678",
            "K12345678",
            "P12345678"
        )

        testthat::expect_identical(
            is_openalex(x),
            rep(TRUE, length(x))
        )
    }
)

testthat::test_that(
    "is_openalex rejects lowercase keys, short tails, and deprecated prefixes",
    {
        x <- c(
            "W2741809807",
            "w2741809807",
            "W123",
            "C12345678",
            "X12345678",
            "not-an-openalex-id",
            NA_character_
        )

        testthat::expect_identical(
            is_openalex(x),
            c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_scholid dispatches to is_openalex for type openalex",
    {
        x <- c("W2741809807", "w2741809807", NA_character_)

        testthat::expect_identical(
            is_scholid(x, "openalex"),
            is_openalex(x)
        )
    }
)

testthat::test_that(
    "OpenAlex normalization canonicalizes valid labeled and URL inputs",
    {
        x <- c(
            "W2741809807",
            "https://openalex.org/W2741809807",
            "https://api.openalex.org/works/W2741809807",
            "w2741809807"
        )

        testthat::expect_identical(
            normalize_scholid(x, "openalex"),
            rep("W2741809807", length(x))
        )
    }
)

testthat::test_that(
    "normalized OpenAlex outputs validate and classify as openalex",
    {
        x <- normalize_scholid(
            c(
                "https://openalex.org/W2741809807",
                "https://api.openalex.org/authors/A5023888391",
                "i97018004"
            ),
            "openalex"
        )

        testthat::expect_identical(
            x,
            c("W2741809807", "A5023888391", "I97018004")
        )
        testthat::expect_true(all(is_openalex(x)))
        testthat::expect_true(all(is_scholid(x, "openalex")))
        testthat::expect_true(all(classify_scholid(x) == "openalex"))
    }
)

testthat::test_that(
    "is_isbn validates ISBN-10 and ISBN-13 checksums",
    {
        x <- c(
            "0-306-40615-2",
            "978-0-306-40615-7",
            "0-306-40615-3",
            "978-0-306-40615-8",
            "not an isbn",
            NA_character_
        )

        got <- is_scholid(
            x,
            "isbn"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, FALSE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_issn validates checksum and rejects malformed inputs",
    {
        x <- c(
            "0317-8471",
            "2434-561X",
            "0317-8472",
            "0317-847",
            NA_character_
        )

        got <- is_scholid(
            x,
            "issn"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_arxiv accepts modern and legacy formats with optional version",
    {
        x <- c(
            "2101.00001v2",
            "2101.00001",
            "hep-th/9901001v2",
            "hep-th/9901001",
            "21.00001",
            "hep-th/990100",
            NA_character_
        )

        got <- is_scholid(
            x,
            "arxiv"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_scholid does not treat valid ISBNs as PMIDs",
    {
        x <- c(
            "9780306406157",
            "0306406152"
        )

        testthat::expect_identical(
            is_scholid(x, "pmid"),
            c(FALSE, FALSE)
        )
    }
)

testthat::test_that(
    "is_scholid still accepts ordinary digit-only PMIDs",
    {
        x <- c(
            "12345678",
            "20493630",
            "1234567890123"
        )

        testthat::expect_identical(
            is_scholid(x, "pmid"),
            c(TRUE, TRUE, TRUE)
        )
    }
)

testthat::test_that(
    "classify_scholid keeps digit-only valid ISBNs as isbn",
    {
        x <- c(
            "9780306406157",
            "0306406152"
        )

        testthat::expect_identical(
            classify_scholid(x),
            c("isbn", "isbn")
        )
    }
)

testthat::test_that(
    "is_pmid accepts digits only",
    {
        x <- c(
            "1234567",
            "012345",
            "12a3",
            "PMC12345",
            NA_character_
        )

        got <- is_scholid(
            x,
            "pmid"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_pmcid accepts PMC prefix and digits only",
    {
        x <- c(
            "PMC12345",
            "PMC012345",
            "pmc12345",
            "12345",
            NA_character_
        )

        got <- is_scholid(
            x,
            "pmcid"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_scholid works across multiple ID types",
    {
        x <- c(
            "10.1000/182",
            "0000-0002-1825-0097",
            "0-306-40615-2",
            "0317-8471",
            "2101.00001v2",
            "1234567",
            "PMC12345",
            NA_character_
        )

        got_doi <- is_scholid(
            x,
            "doi"
        )
        got_orc <- is_scholid(
            x,
            "orcid"
        )
        got_isb <- is_scholid(
            x,
            "isbn"
        )
        got_isn <- is_scholid(
            x,
            "issn"
        )
        got_arx <- is_scholid(
            x,
            "arxiv"
        )
        got_pmi <- is_scholid(
            x,
            "pmid"
        )
        got_pmc <- is_scholid(
            x,
            "pmcid"
        )

        testthat::expect_true(got_doi[1])
        testthat::expect_true(got_orc[2])
        testthat::expect_true(got_isb[3])
        testthat::expect_true(got_isn[4])
        testthat::expect_true(got_arx[5])
        testthat::expect_true(got_pmi[6])
        testthat::expect_true(got_pmc[7])

        testthat::expect_true(is.na(got_doi[8]))
        testthat::expect_true(is.na(got_orc[8]))
        testthat::expect_true(is.na(got_isb[8]))
        testthat::expect_true(is.na(got_isn[8]))
        testthat::expect_true(is.na(got_arx[8]))
        testthat::expect_true(is.na(got_pmi[8]))
        testthat::expect_true(is.na(got_pmc[8]))
    }
)

testthat::test_that(
    "is_orcid returns FALSE for pattern mismatches and checksum failures",
    {
        x <- c(
            "0000-0002-1825-0097",
            "0000-0002-1825-0098",
            "0000-0002-1825-009",
            "abcd-0002-1825-0097",
            NA_character_
        )

        got <- is_scholid(
            x,
            "orcid"
        )

        testthat::expect_identical(
            got,
            c(TRUE, FALSE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_isbn hits is10/is13 regex-fail branches and checksum branches",
    {
        x <- c(
            "0-306-40615-2",
            "978-0-306-40615-7",
            "030640615",
            "978030640615",
            "97803064061570",
            "0-306-40615-3",
            "978-0-306-40615-8",
            NA_character_
        )

        got <- is_scholid(
            x,
            "isbn"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_issn rejects pattern mismatches and checksum failures",
    {
        x <- c(
            "0317-8471",
            "0317-8472",
            "03178471",
            "0317-84A1",
            NA_character_
        )

        got <- is_scholid(
            x,
            "issn"
        )

        testthat::expect_identical(
            got,
            c(TRUE, FALSE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_arxiv rejects malformed ids for both modern and legacy formats",
    {
        x <- c(
            "2101.00001v2",
            "hep-th/9901001v2",
            "2101.000",
            "hep-th/990100",
            "HEP-TH/9901001",
            NA_character_
        )

        got <- is_scholid(
            x,
            "arxiv"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, FALSE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_scholid coerces numeric input and preserves NA",
    {
        x <- c(
            1234567,
            NA_real_
        )

        got <- is_scholid(
            x,
            "pmid"
        )

        testthat::expect_identical(
            got,
            c(TRUE, NA)
        )
    }
)

testthat::test_that(
    "is_pmid and is_pmcid reject near-misses",
    {
        x_pmid <- c(
            "1234",
            "12a3",
            " 1234",
            NA_character_
        )

        got_pmid <- is_scholid(
            x_pmid,
            "pmid"
        )

        testthat::expect_identical(
            got_pmid,
            c(TRUE, FALSE, FALSE, NA)
        )

        x_pmc <- c(
            "PMC123",
            "PMC",
            "pmc123",
            NA_character_
        )

        got_pmc <- is_scholid(
            x_pmc,
            "pmcid"
        )

        testthat::expect_identical(
            got_pmc,
            c(TRUE, FALSE, FALSE, NA)
        )
    }
)

testthat::test_that(
    "is_issn accepts check digit 0 branch",
    {
        x <- c(
            "0000-0000",
            NA_character_
        )

        got <- is_scholid(
            x,
            "issn"
        )

        testthat::expect_identical(
            got,
            c(TRUE, NA)
        )
    }
)

testthat::test_that(
    "is_isbn uppercases x and validates ISBN-10 with X check digit",
    {
        x <- c(
            "0-8044-2957-x",
            "0-8044-2957-X",
            NA_character_
        )

        got <- is_scholid(
            x,
            "isbn"
        )

        testthat::expect_identical(
            got,
            c(TRUE, TRUE, NA)
        )
    }
)


testthat::test_that(
    "isbn validation rejects malformed grouped forms",
    {
        testthat::expect_false(is_isbn("1234 5678 9X"))
        testthat::expect_false(is_isbn("97-80-306-40615-7"))

        testthat::expect_identical(
            normalize_scholid("1234 5678 9X", "isbn"),
            NA_character_
        )
        testthat::expect_identical(
            normalize_scholid("97-80-306-40615-7", "isbn"),
            NA_character_
        )
    }
)
