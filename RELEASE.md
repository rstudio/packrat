## Release Instructions

Create an issue with the following content, replacing `${RELEASE_VERSION}` and
`${NEXT_RELEASE_VERSION}` with the current and next release versions.

````markdown
```console
RELEASE_VERSION=0.9.1
NEXT_RELEASE_VERSION=0.9.2
```console

### Prepare for release

- [ ] Switch to `main` and pull.

   ```bash
   git checkout main
   git pull
   ```

- [ ] Create and switch to a release branch (e.g. `release/${RELEASE_VERSION}`).

    ```bash
    git checkout -b "release/${RELEASE_VERSION}"
    ```

- [ ] Update version in `DESCRIPTION` file.

- [ ] Update header and version in `NEWS.md`. confirm recent changes are
      included. ([style reference](https://style.tidyverse.org/news.html#news-release))

- [ ] Update auto-loaders (from R):

    ```r
    source("R/update.R"); updateInit();
    ```

- [ ] Commit your changes!

    ```bash
    git commit -m "preparing for ${RELEASE_VERSION} release" NEWS.md DESCRIPTION inst
    ```

- [ ] Check current CRAN check results:
    <https://cran.rstudio.org/web/checks/check_results_packrat.html>

- [ ] Check that all URLs are valid (from R):

    ```r
    install.packages("urlchecker")
    urlchecker::url_check()
    ```

- [ ] Generate a release `.tar.gz` by running the following command from the
    parent directory of your `packrat` repository:
    
    ```console
    R CMD build packrat
    ```

- [ ] Test the package (also from the parent directory):

    ```console
    R CMD check --as-cran "packrat_${RELEASE_VERSION}.tar.gz"
    ```
    
- [ ] Push the `release/${RELEASE_VERSION}` branch to GitHub, create a PR, and let CI run.

- [ ] Fix any issues identified by the previous steps. Rinse and repeat.

### Submit to CRAN

- [ ] Submit to CRAN. Cross fingers. https://cran.r-project.org/

- [ ] Check your email; confirm package submission.

- [ ] Wait for CRAN approval.

### Approved by CRAN

- [ ] Squash-and-merge the PR with the `release/${RELEASE_VERSION}` changes back to `main`.

- [ ] Create a git tag for your new release and push that tag.

    ```console
    git fetch
    git tag -a -m "CRAN release: v${RELEASE_VERSION}" "v${RELEASE_VERSION}" COMMIT_HASH
    git push origin "v${RELEASE_VERSION}"
    ```

- [ ] Create a GitHub release from the tag and include its NEWS.md items as
      release notes.

- [ ] Create a branch to bump the version for development (e.g. `development/${NEXT_RELEASE_VERSION}`).

    ```console
    git checkout main
    git pull
    git checkout -b "development/${NEXT_RELEASE_VERSION}"
    ```

- [ ] Update `NEWS.md` with an "unreleased" version header.

    `````markdown
    # Packrat ${NEXT_RELEASE_VERSION} (UNRELEASED)
    `````

- [ ] Run the following command to update `DESCRIPTION` and the auto-loaders:
    
    ```console
    R -f configure.R
    ```

- [ ] Commit your changes!

    ```bash
    git commit -m "preparing for ${NEXT_RELEASE_VERSION} development" NEWS.md DESCRIPTION inst
    ```

- [ ] Push the `development/${NEXT_RELEASE_VERSION}` branch to GitHub, create
      a PR, and let CI run.
      
- [ ] Squash-and-merge the PR with the `development/${NEXT_RELEASE_VERSION}`
      changes back to `main`.
````

### Prerequisites

You need a functional LaTeX installation in order to run some of these checks.

```r
install.packages("tinytex")
tinytex:::install_prebuilt('TinyTeX')
```

This installs a more full TeX install than occurs than
`tinytex::install_tinytex` does by default.
(<https://github.com/rstudio/tinytex/issues/323#issuecomment-898734967>)
