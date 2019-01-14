context('Hypothesis testing')

with_mock_tabs("ds2_book1.json", "ds2_mt1.json", expr = {
	sut <- tabBook()

	test_that("the expected shape of each raw CrunchCube excludes missing values", {
        expect_is(sut, "TabBookResult")
		expect_equal(sut[[1]][[1]]@useNA, "no")
		expect_equal(sut[[1]][[2]]@useNA, "no")
		expect_equal(dim(as.array(sut[[1]][[1]])), c(5, 1))
		expect_equal(dim(as.array(sut[[1]][[2]])), c(5, 7))
		expect_equal(dim(bases(sut[[1]][[2]], margin=0)), c(5, 7))
	})
	test_that("setting useNA behaves as expected", {
		subtable <- sut[[1]][[2]]
		subtable@useNA <- "always"
		expect_equal(dim(as.array(subtable)), c(8, 10))
		expect_equal(dim(bases(subtable, margin=0)), c(8, 10))
	})
	test_that("margins and bases of categorical-by-categorical subtable make sense", {
		subtable <- sut[[1]][[2]]

		colmargin <- margin.table(subtable, 2)
		rowmargin <- margin.table(subtable, 1)

		expected_row_margin <- structure(
			c(233.8294316898, 168.9009571766, 111.0965406625, 393.6781399731, 92.4949304981),
			.Dim = 5L, .Dimnames = structure(list(app_dtrmp =
				c("Strongly approve", "Somewhat approve", "Somewhat disapprove",
					"Strongly disapprove", "Not sure")),
			.Names = "app_dtrmp"))
		expected_col_margin <- structure(
			c(101.6600702838, 166.0998778008, 61.0604720754, 134.5968281414, 218.5057609743, 290.8366985161, 27.2402922083),
			.Dim = 7L, .Dimnames = structure(
				list(pew_churatd = c("More than once a week", "Once a week", "Once or twice a month",
					"A few times a year", "Seldom", "Never", "Don't know")),
			.Names = "pew_churatd"))

		expect_equivalent(as.array(rowmargin), expected_row_margin)
		expect_equivalent(as.array(colmargin), expected_col_margin)

		expect_equivalent(as.vector(bases(subtable, margin=1)), c(254, 164, 88, 432, 62))
		expect_equivalent(as.vector(bases(subtable, margin=2)), c(98, 172, 62, 137, 211, 297, 23))
	})
    test_that("we can compute row and column pvalues for a simple table", {
    	subtable <- sut[[1]][[2]]
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

        counts <- as.array(subtable)
        counts_unweighted <- bases(subtable, margin=0)
        result <- crunchtabs:::compute_pvals(counts, counts_unweighted)

        expect_equivalent(result, col_values)
    })
})
