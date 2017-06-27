context('hypothesis testing')

with_mock_tabs("ds2_book1.json", "ds2_mt1.json", {
	sut <- tabBook()
	test_that("the expected shape of each raw CrunchCube excludes missing values", {
        expect_is(sut, "TabBookResult")
		expect_equal(sut[[1]][[1]]@useNA, "no")
		expect_equal(sut[[1]][[2]]@useNA, "no")
		expect_equal(dim(as.array(sut[[1]][[1]])), c(5, 1))
		expect_equal(dim(as.array(sut[[1]][[2]])), c(5, 7))
	})
    test_that("we can compute row and column pvalues for a simple table", {
    	sut <- sut[[1]][[2]]
        col_values <- matrix(
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
            nrow=5, ncol=7, byrow=TRUE)

        counts <- as.array(sut)
        counts_unweighted <- bases(sut, margin=0)
        str(counts_unweighted)
        result <- crunchtabs:::compute_pvals(counts, counts_unweighted)

        expect_equivalent(result, col_values)
    })
})