context("crunchtabs tests")

# test_that("Create dataset from fixture", {
#     ds <- newDatasetFromFixture("ds1")
#     expect_true(is.dataset(ds))
#     expect_identical(dim(ds), c(20L, 15L))
#     expect_identical(names(ds),
#                      c("allpets", "q1", "petloc", "ndogs", "ndogs_a", "ndogs_b", "q3",
#                        "country", "wave", "Weight", "age", "age2", "age3", "age5", "gender"))
#     expect_identical(name(ds), "Example dataset")
#     expect_identical(names(categories(ds$q1)),
#                      c("Cat", "Dog", "Bird", "Skipped", "Not Asked"))
#     expect_is(ds, "CrunchDataset")
#     browser()
# })

test_that("Basic method dispatch error handling", {
    expect_error(getResults(NULL),
        "getResults doesn't support objects of class NULL")
    expect_error(getNames(NULL),
        "getNames doesn't support objects of class NULL")
})

with_mock_tabs("ds1_book1.json", "ds1_mt1.json", {
    test_that("We can get a tab book", {
        b <- tabBook()
        expect_is(b, "TabBookResult")
    })
    test_that("We can get tabs_data", {
        banner <- jsonlite::fromJSON(file.path(fixtures_dir, "ds1_banner1.json"), simplifyVector=FALSE)
        dataset <- jsonlite::fromJSON(file.path(fixtures_dir, "ds1.json"), simplifyVector=FALSE)
        b <- tabBooks(banner = banner, dataset = dataset)

        expect_is(b, "list")
    })
})

with_mock_tabs("ds2_book1.json", "ds2_mt1.json", {
    test_that("we can compute row and column pvalues for a simple table", {
        b <- tabBook()
        expect_is(b, "TabBookResult")
        pcol <- makePValues(b, direction="col")
        prow <- makePvalues(b, direction="row")
        col_values <- array(
            c(3.283750331428692e-06, 0.04516436946032343, -0.9577195252707627, -0.03898071836081218,
            -0.7306711675450073, -0.0005442777355741946, -0.23871322109053694,
            -0.7790821573541462, 0.4188339427315062, 0.08300853251899931, 0.03407687846602858,
            -0.565415310302352, -0.0016907762000637483, 0.8690523318123631,
            0.124604881728557, 0.17960851330057404, -0.8576297648771554, 0.42427765051355637,
            -0.18527519273483595, -0.3865636825430918, -0.001631412606915239,
            -1e-11, -0.002538170966906117, -0.18561502720280165, -0.25042536927889003,
            0.004933155309021053, 2.092535638098525e-07,-0.029096132519773832,
            -0.2717288155941673, -0.3038600017616253, -0.9389528575553883, 0.2837275213812045,
            -0.0022774458717707002, 0.7047777625885967, 2.6540191258916934e-05),
            dim=c(5L, 7L))
        row_values <- array(
            c(1.6695350735229653e-06, 0.037313576148173544, -0.9571259804728898, -0.029906815132613396,
            -0.6993254250587566, -0.00017777090684334418, -0.19871432070405515,
            -0.7789071743607883, 0.442674444893985, 0.11212512893481619, 0.04273428332765694,
            -0.5567907910877268, -0.0016911261350758267, 0.8707320757564629,
            0.19410499934361947, 0.2802585068194452, -0.8888112761415692, 0.5169205835730142,
            -0.2502867838369822, -0.4746624243294846, -0.010785964463142061,
            -1e-11, -0.0020260472105257943, -0.1874167806534155, -0.2282464106542077,
            0.0017317558034855018, 5.190005736288583e-08, -0.03208397964669696,
            -0.4176301235495803, -0.4722398956874967, -0.9585109213487233, 0.44933165598956304,
            -0.02044942806172978, 0.7863384646870586, 0.005096227304728584),
            dim=c(5L, 7L))
        expect_equivalent(pcol, col_values)
        expect_equivalent(pcol, col_values)
    })
})
