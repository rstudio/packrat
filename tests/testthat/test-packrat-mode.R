context("packrat mode")

test_that("packrat_mode successfully sets the library paths when turned on and off", {

  with_dir(tempdir(), {

    ## Make sure packrat mode is off
    if (packrat:::isPackratModeOn())
      packrat::off()

    orig_libs <- getLibPaths()

    # don't use packrat::on so we can avoid the initialization step
    packrat:::setPackratModeOn(auto.snapshot = FALSE, clean.search.path = FALSE)

    packrat::off()

    expect_identical(orig_libs, getLibPaths())

  })

})
