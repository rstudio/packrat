## Release Instructions

-   Create and switch to a release branch (e.g. `release/<version>`).

    ```bash
    git checkout -b release/0.9.0
    ```

-   Update version in `DESCRIPTION` file.

-   Update header and version in `NEWS.md`. confirm recent changes are included.

-   Update auto-loaders (from R):

    ```r
    source("R/update.R"); updateInit();
    ```

-   Commit your changes!

    ```bash
    git commit -m 'preparing for 0.9.0 release' NEWS.md DESCRIPTION inst
    ```

-   Check that all URLs are valid (from R):

    ```r
    install.packages("urlchecker")
    urlchecker::url_check()
    ```

-   Generate a release `.tar.gz` by running the following command from the
    parent directory of your `packrat` repository:
    
    ```console
    R CMD build packrat
    ```

-   Test the package (also from the parent directory):

    ```console
    R CMD check --as-cran packrat_*.tar.gz
    ```
    
-   Push the branch to GitHub and let our CI workflow run.

-   Fix any issues identified by the previous steps. Rinse and repeat.

-   Submit to CRAN. Cross fingers. https://cran.r-project.org/
    
-   After submission, squash-and-merge the `release/<version>` branch back to
    `main`.

-   Create a git tag for your new release and push that tag.

    ```console
    git tag -a -m 'CRAN release: vX.Y.Z' vX.Y.Z COMMIT_HASH
    git push origin vX.Y.Z
    ```

-   Create a GitHub release against that tag and include the NEWS.md items in
    its notes.

-   Create a branch to bump for development (e.g. `development/<version>`).

    Update `NEWS.md` with an "unreleased" version header and run the following
    command to update `DESCRIPTION` and the auto-loaders:
    
    ```console
    R -f configure.R
    ```

    Squash-and-merge this branch back to `main`.
