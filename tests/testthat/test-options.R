withTestContext({
  # Confirms that we can use set_opts and the single-option setter and that the
  # single-option setter does not overwrite previously configured state (#655)
  test_that("an option can be set and retrieved", {
    projRoot <- cloneTestProject("empty")

    owd <- setwd(projRoot)
    on.exit(setwd(owd))

    packrat::set_opts(ignored.packages = c("emo"), persist = FALSE)
    packrat::opts$snapshot.recommended.packages(TRUE, persist = FALSE)

    expect_equal(get_opts("ignored.packages"), c("emo"))
    expect_equal(get_opts("snapshot.recommended.packages"), TRUE)
  })
})
