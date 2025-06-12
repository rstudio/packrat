## Release Instructions

Use `usethis::use_release_issue()` to create a release issue.

In the "Submit to CRAN" section, Use the following to adjust the version
before `devtools::submit_cran()`:

- [ ] `usethis::use_version('patch')`
- [ ] `source("R/update.R"); updateInit();`

In the "Wait for CRAN" section, use:

- [ ] `usethis::use_dev_version()`
- [ ] `source("R/update.R"); updateInit();`
- [ ] Commit and push the result.

### Prerequisites

You need a functional LaTeX installation in order to run some of these checks.

```r
install.packages("tinytex")
tinytex:::install_prebuilt('TinyTeX')
```

This installs a more full TeX install than occurs than
`tinytex::install_tinytex` does by default.
(<https://github.com/rstudio/tinytex/issues/323#issuecomment-898734967>)
