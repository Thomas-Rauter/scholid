# Shared inputs for the cross-type invariant tests.
# testthat sources helper-*.R before the test files. Names of
# scholid_type_inputs must match scholid_types(). Examples are taken
# from the existing tests and vignettes/scholid_definitions.Rmd.
# Some inputs contain invisible characters or Unicode look-alikes; see
# scholid_invisible_chars and scholid_lookalike_chars.

scholid_type_inputs <- list(
    doi = c(
        "10.1000/182",
        "10.1038/s41586-020-2649-2",
        "10.5555/12345678",
        "10.1207/s15327965pli1503_02",
        "10.1000/abc_def-ghi.jkl",
        paste0(
            "10.1002/(SICI)1097-4571(199205)43:4",
            "<284::AID-ASI5>3.0.CO;2-0"
        ),
        " doi:10.1000/182 ",
        "doi:10.1000/182",
        "https://doi.org/10.1000/182",
        "http://dx.doi.org/10.1000/182.",
        "10.1000/182.",
        "10.1000/182,,,",
        "10.1000/182;:",
        "10.1000/with space",
        "10.1000",
        "10.1000/182</a>",
        "10.1000/182'foo",
        "(10.1000/182)",
        "[10.1000/182]",
        "not a doi",
        "10.1000/\u200B182",
        "\uFEFF10.1000/182",
        "\u00A010.1000/182\u00A0",
        "10.1000/1\u00A082",
        "10.1000/a\u2013b",
        "https://doi.org/10.1000%2F182",
        "",
        NA_character_
    ),
    arxiv = c(
        "2101.00001",
        "2101.00001v2",
        "2101.12345",
        "hep-th/9901001",
        "hep-th/9901001v3",
        "math.GT/0309136",
        "math.GT/0309136v1",
        "cs.CL/0501001",
        "math/0303001",
        "arXiv:2101.00001",
        "https://arxiv.org/abs/2101.00001v2",
        "arXiv:math.GT/0309136",
        "https://arxiv.org/abs/hep-th/9901001",
        "https://arxiv.org/abs/math.GT/0309136",
        "2101.000",
        "21.00001",
        "hep-th/990100",
        "HEP-TH/9901001",
        "bad",
        "",
        NA_character_
    ),
    bibcode = c(
        "1992ApJ...400L...1W",
        "1995ApJ...438..387R",
        "1974MNRAS.168..249B",
        "https://ui.adsabs.harvard.edu/abs/1992ApJ...400L...1W",
        "https://adsabs.harvard.edu/abs/1995ApJ...438..387R",
        "bibcode: 1974MNRAS.168..249B",
        "bibcode:1995ApJ...438..387R",
        "1992ApJ...400L...1W.",
        "1992ApJ...400L...1",
        "1992.....400L...1W",
        "catalog 1992ApJ...400L...1W",
        "(1992ApJ...400L...1W)",
        "not-a-bibcode",
        "",
        NA_character_
    ),
    openalex = c(
        "W2741809807",
        "A5023888391",
        "I97018004",
        "S137773608",
        "T154945302",
        "F4320332160",
        "G12345678",
        "K12345678",
        "P12345678",
        "https://openalex.org/W2741809807",
        "https://openalex.org/W2741809807/",
        "https://api.openalex.org/works/W2741809807",
        "https://api.openalex.org/authors/A5023888391",
        "https://api.openalex.org/institutions/I97018004",
        "w2741809807",
        "W123",
        "C12345678",
        "X12345678",
        "P12345",
        "not-an-openalex-id",
        "",
        NA_character_
    ),
    swhid = c(
        "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
        "swh:1:dir:d198bc9d7a6bcf6db04f476d29314f157507d505",
        "swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d",
        "swh:1:rel:22ece559cc7cc2364edc5e5593d63ae8bd229f9f",
        "swh:1:snp:c7c108084bc0bf3d81436bf980b46e98bd338453",
        paste0(
            "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;",
            "origin=https://example.org/repo.git;",
            "lines=9-15"
        ),
        paste0(
            "https://archive.softwareheritage.org/",
            "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2"
        ),
        paste0(
            "https://identifiers.org/swh/",
            "swh:1:rev:309cf2674ee7a0749978cf8265ab91a60aea0f7d"
        ),
        "SWH:1:CNT:94a9ed024d3859793618152ea559a168bbcbb5e2",
        "94a9ed024d3859793618152ea559a168bbcbb5e2",
        "swh:2:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2",
        paste0(
            "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;",
            "unknown=foo"
        ),
        paste0(
            "swh:1:cnt:4d99d2d18326621ccdd70f5ea66c2e2ac236ad8b;",
            "path=relative"
        ),
        "(swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2)",
        "not-a-swhid",
        "",
        NA_character_
    ),
    ark = c(
        "ark:/12148/btv1b8449691v",
        "ark:/12148/btv1b8449691v/f29",
        "ark:/13030/654xz321",
        "https://n2t.net/ark:/12148/btv1b8449691v",
        "https://n2t.net/ark:/13030/654xz321",
        "ark:13030/654xz321",
        "ark:/12148/btv1b8449691v/f29.",
        "12148/btv1b8449691v",
        "ark:/1234/btv1b8449691v",
        "ark:/12148/",
        "not-an-ark",
        "",
        NA_character_
    ),
    isni = c(
        "000000012146438X",
        "000000012124423X",
        "https://isni.org/isni/000000012124423X",
        "https://isni.org/isni/000000012146438X",
        "ISNI 0000 0001 2146 438X",
        "urn:isni:000000012146438X",
        "0000-0002-1825-0097",
        "000000012146438A",
        "catalog 000000012146438X",
        "(000000012146438X)",
        "not-an-isni",
        "0000\u00A00001\u00A02146\u00A0438X",
        "ISNI 0000\u20130001\u20132146\u2013438X",
        "",
        NA_character_
    ),
    orcid = c(
        "0000-0002-1825-0097",
        "0000-0002-1694-233X",
        "0000-0000-0000-001X",
        "0000-0000-0000-001x",
        "0000-0002-1694-233x",
        "0000000218250097",
        "000000000000001x",
        " 0000 0002 1825 0097 ",
        "https://orcid.org/0000-0002-1825-0097",
        "orcid:0000-0002-1825-0097",
        "orcid:0000-0000-0000-001x",
        "0000-0002-1825-009X",
        "0000-0002-1825-0098",
        "0000-0002-1825-009",
        "0000_0002_1825_0097",
        "(0000-0002-1825-0097)",
        "0000-0002-1825-0097xyz",
        "bad",
        "\uFEFF0000-0002-1825-0097",
        "0000-0002-\u00AD1825-0097",
        "0000\u20100002\u20101825\u20100097",
        "0000\u20130002\u20131825\u20130097",
        "0000\u20140002\u20141825\u20140097",
        "\uFF10\uFF10\uFF10\uFF10-0002-1825-0097",
        "",
        NA_character_
    ),
    ror = c(
        "01an7q238",
        "02mhbdp94",
        "02s376052",
        "https://ror.org/01an7q238",
        "https://ror.org/01an7q238/",
        "https://ror.org/02mhbdp94",
        "ror.org/01an7q238",
        "ROR: 02mhbdp94",
        "ROR: 02s376052",
        " 02s376052 ",
        "02mhbdp99",
        "02mhbdp9",
        "(01an7q238)",
        "01an7q238xyz",
        "not-a-ror",
        "",
        NA_character_
    ),
    rrid = c(
        "RRID:AB_262044",
        "RRID:CVCL_2260",
        "RRID:SCR_007358",
        "RRID:IMSR_JAX:000664",
        "RRID:MGI:3840442",
        "RRID:Addgene_80088",
        "RRID: AB_262044",
        "RRID: Addgene_80088",
        "rrid:CVCL_2260",
        "rrid:MGI:3840442",
        "https://scicrunch.org/resolver/RRID:AB_262044",
        "https://identifiers.org/RRID:SCR_007358",
        "AB_262044",
        "RRID:UNKNOWN_123",
        "RRID:AB_262044xyz",
        "(RRID:AB_262044)",
        "bad",
        "",
        NA_character_
    ),
    uniprot = c(
        "P12345",
        "P04637",
        "Q9H0H5",
        "A0A022YWF9",
        "p12345",
        "https://www.uniprot.org/uniprot/P04637",
        "https://www.uniprot.org/uniprot/P12345",
        "https://identifiers.org/uniprot/Q9H0H5",
        "uniprot:A0A022YWF9",
        "uniprot:Q9H0H5",
        "P123",
        "RRID:AB_262044",
        "",
        NA_character_
    ),
    refseq = c(
        "NM_001744.6",
        "NP_001735.1",
        "NC_003619.1",
        "NZ_CASIGT010000001.1",
        "NM_021964.7",
        "nm_001744.6",
        "https://www.ncbi.nlm.nih.gov/nuccore/NM_001744.6",
        "https://www.ncbi.nlm.nih.gov/protein/NP_001735.1",
        "https://identifiers.org/refseq/NC_003619.1",
        "refseq:NM_021964.7",
        "refseq:NP_001735.1",
        "NM_001744",
        "RRID:AB_262044",
        "not-refseq",
        "",
        NA_character_
    ),
    sra = c(
        "SRR1553610",
        "SRX1234567",
        "SRP006081",
        "SRS123456",
        "ERR1234567",
        "DRR1234567",
        "srr1553610",
        "https://www.ncbi.nlm.nih.gov/sra/SRR1553610",
        "https://identifiers.org/sra/SRX1234567",
        "sra:SRP006081",
        "sra:SRX1234567",
        "SRR123",
        "NM_001744.6",
        "",
        NA_character_
    ),
    geo = c(
        "GSE2553",
        "GSM313800",
        "GPL96",
        "GDS505",
        "gse2553",
        paste0(
            "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi",
            "?acc=GSE2553"
        ),
        "https://identifiers.org/geo/GSM313800",
        "https://identifiers.org/geo/GPL96",
        "geo:GPL96",
        "geo:GSM313800",
        "geo:GDS505",
        "GSE1",
        "SRR1553610",
        "",
        NA_character_
    ),
    bioproject = c(
        "PRJNA257197",
        "PRJEB12345",
        "PRJDB303",
        "PRJDA1234",
        "prjna257197",
        "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA257197",
        "https://www.ncbi.nlm.nih.gov/bioproject/?term=PRJEB12345",
        "https://identifiers.org/bioproject/PRJDB303",
        "bioproject:PRJDA1234",
        "bioproject:PRJEB12345",
        "PRJNA1",
        "GSE2553",
        "",
        NA_character_
    ),
    assembly = c(
        "GCF_000001405.40",
        "GCA_000001405.29",
        "GCA_009914755.4",
        "gcf_000001405.40",
        "https://www.ncbi.nlm.nih.gov/assembly/GCF_000001405.40",
        paste0(
            "https://www.ncbi.nlm.nih.gov/datasets/genome/",
            "GCA_009914755.4/"
        ),
        "https://identifiers.org/insdc.gcf:GCF_000001405.40",
        "assembly:GCA_000001405.29",
        "GCF_000001405",
        "NM_001744.6",
        "",
        NA_character_
    ),
    isbn = c(
        "0306406152",
        "9780306406157",
        "0-306-40615-2",
        "978-0-306-40615-7",
        "0-8044-2957-X",
        "0-8044-2957-x",
        "ISBN 978-3-16-148410-0",
        "ISBN 9780306406157",
        "ISBN 978 0 306 40615 7",
        "isbn:9780306406157",
        "ISBN-13: 9780306406157",
        "978-0-306-40615-8",
        "0-306-40615-3",
        "0-306-40615-x",
        "030640615X",
        "1234567890123",
        "030640615",
        "not an isbn",
        "978\u20130\u2013306\u201340615\u20137",
        "978\u00A00\u00A0306\u00A040615\u00A07",
        "",
        NA_character_
    ),
    issn = c(
        "0317-8471",
        "2434-561X",
        "2434-561x",
        "0378-5955",
        "0000-0000",
        "2434561X",
        "03178471",
        "ISSN 0317-8471",
        "ISSN 2434-561X",
        "issn: 0378-5955",
        "0317-8472",
        "9999-9999",
        "0317 8471",
        "2434-561X-90",
        "0317-847",
        "0317-84A1",
        "0317\u20108471",
        "0317\u20148471",
        "bad",
        "",
        NA_character_
    ),
    pmcid = c(
        "PMC123",
        "PMC12345",
        "PMC012345",
        "PMC123456",
        "PMC1234567",
        "pmc12345",
        "PMCID: PMC1234567",
        "PMCID PMC1234567",
        "PMCID: PMC12345",
        "PMCID:123456",
        "PMCID 123456",
        "pmcid: 123456",
        "pmcid PMC7654321",
        "PMC",
        "12345",
        "PMC 123456",
        "PMC123\u200B4567",
        "PMC\uFF11\uFF12\uFF13\uFF14\uFF15\uFF16\uFF17",
        "",
        NA_character_
    ),
    pmid = c(
        "1234",
        "12345",
        "012345",
        "1234567",
        "7654321",
        "12345678",
        "20493630",
        "29456894",
        "PMID: 12345678",
        "PMID 12345678",
        "pmid 7654321",
        "pmid: 7654321",
        "  PMID 12345678  ",
        "PMID: 12345",
        "12a3",
        "12A",
        "PMC12345",
        "9780306406157",
        "0306406152",
        "not a pmid",
        "1234\u200B5678",
        "1234\u00AD5678",
        "PMID:\u00A012345678",
        "PMID: \uFF11\uFF12\uFF13\uFF14\uFF15",
        "\uFF12\uFF19\uFF14\uFF15\uFF16\uFF18\uFF19\uFF14",
        "",
        NA_character_
    )
)

scholid_extract_texts <- c(
    "See doi:10.1000/182.",
    "See (10.1000/182).",
    "Markdown link: [paper](https://doi.org/10.1000/182).",
    paste0(
        "Potentially tricky DOI: ",
        "10.1002/(SICI)1097-4571(199205)43:4",
        "<284::AID-ASI5>3.0.CO;2-0."
    ),
    "Quoted '2101.12345'.",
    "Two IDs: 2101.12345 and hep-th/9901001v2.",
    "[hep-th/9901001]",
    "references: 1992ApJ...400L...1W and 1995ApJ...438..387R",
    "Work https://openalex.org/W2741809807.",
    "Author [A5023888391] and institution I97018004.",
    paste0(
        "Archived at https://archive.softwareheritage.org/",
        "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2."
    ),
    "Link https://n2t.net/ark:/13030/654xz321",
    "ARK (ark:/12148/btv1b8449691v/f29).",
    "ISNI 0000 0001 2146 438X next to ORCID 0000-0002-1825-0097.",
    "ROR https://ror.org/01an7q238.",
    "Antibody RRID:AB_262044.",
    "Tool https://scicrunch.org/resolver/RRID:SCR_007358",
    "see RRID:UNKNOWN_123 for details",
    "Proteins P04637, Q9H0H5, and https://www.uniprot.org/uniprot/P12345.",
    "Sequences NM_001744.6 and NP_001735.1.",
    "Run SRR1553610 and experiment SRX1234567.",
    "Series GSE2553; sample GSM313800; platform GPL96.",
    paste0(
        "GEO https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi",
        "?acc=GSE2553"
    ),
    "Projects PRJNA257197 and PRJEB12345.",
    "Assembly https://www.ncbi.nlm.nih.gov/assembly/GCF_000001405.40.",
    "ISBN 978-0-306-40615-7.",
    "ISBN 0-306-40615-2",
    "ISSN 2434-561X and issn: 0378-5955.",
    "PMID: 12345678.",
    "Wrapped (7654321).",
    "PMCID: PMC1234567",
    "No identifier in this sentence.",
    "see PMC123\u200B4567 here",
    "see 1234\u200B5678 here",
    "see 1234\u00AD5678 here",
    "ORCID 0000\u20130002\u20131825\u20130097.",
    "doi:10.1000/182\u00A0and more",
    "see 10.1000/a\u2013b here",
    "ISBN 978\u20130\u2013306\u201340615\u20137.",
    "PMID: \uFF11\uFF12\uFF13\uFF14\uFF15\uFF16\uFF17\uFF18.",
    "pp. 1998\u20132003",
    NA_character_
)

# Must match .scholid_invisible_chars() in R/input_validation.R.
# Repeated here so a dropped character fails these tests.
scholid_invisible_chars <- c(
    "\u00AD",
    "\u200B",
    "\u200C",
    "\u200D",
    "\u200E",
    "\u200F",
    "\u202A",
    "\u202B",
    "\u202C",
    "\u202D",
    "\u202E",
    "\u2060",
    "\u2061",
    "\u2062",
    "\u2063",
    "\u2064",
    "\u2066",
    "\u2067",
    "\u2068",
    "\u2069",
    "\uFEFF"
)

# Must match .scholid_lookalike_chars() in R/input_validation.R.
# Repeated here so a dropped character fails these tests.
scholid_lookalike_chars <- list(
    space = c(
        "\u00A0",
        "\u1680",
        "\u2000",
        "\u2001",
        "\u2002",
        "\u2003",
        "\u2004",
        "\u2005",
        "\u2006",
        "\u2007",
        "\u2008",
        "\u2009",
        "\u200A",
        "\u202F",
        "\u205F",
        "\u3000"
    ),
    dash = c(
        "\u2010",
        "\u2011",
        "\u2012",
        "\u2013",
        "\u2212",
        "\uFE63",
        "\uFF0D"
    ),
    digit = c(
        "\uFF10",
        "\uFF11",
        "\uFF12",
        "\uFF13",
        "\uFF14",
        "\uFF15",
        "\uFF16",
        "\uFF17",
        "\uFF18",
        "\uFF19"
    )
)

scholid_fullwidth <- function(x) {
    for (i in 0:9) {
        x <- gsub(
            as.character(i),
            scholid_lookalike_chars$digit[[i + 1L]],
            x,
            fixed = TRUE
        )
    }
    x
}

scholid_invisible_ids <- c(
    doi   = "10.1000/182",
    orcid = "0000-0002-1825-0097",
    pmid  = "12345678",
    pmcid = "PMC1234567"
)

scholid_insert_invisible <- function(id, ch, where) {
    if (identical(where, "start")) {
        return(paste0(ch, id))
    }
    if (identical(where, "end")) {
        return(paste0(id, ch))
    }
    mid <- nchar(id) %/% 2L
    paste0(
        substr(id, 1L, mid),
        ch,
        substr(id, mid + 1L, nchar(id))
    )
}

scholid_invisible_variants <- function(id) {
    places <- c("start", "middle", "end")
    n <- length(scholid_invisible_chars) * length(places)
    out <- character(n)
    i <- 0L
    for (ch in scholid_invisible_chars) {
        for (where in places) {
            i <- i + 1L
            out[[i]] <- scholid_insert_invisible(
                id,
                ch,
                where
            )
        }
    }
    out
}
