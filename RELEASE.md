
## Release Notes

- Use git to switch to a release branch (e.g. `release/<version>`).
- Update version in `DESCRIPTION` file.
- Update version in `NEWS.md`.
- Run `source("R/update.R"); updateInit()` to update auto-loaders.
- Run `urlchecker::url_check()` and fix or update URLs.
- Run `R CMD build packrat` from parent directory to generate tarball.
- Run `R CMD check --as-cran packrat_*.tar.gz` to test the package.
- Fix associated issues; rinse; repeat.
- Submit to CRAN.
- Cross fingers.
- After submission, squash and merge `release/<version>` branch back to master.
- Run `R -f configure.R` to bump version, and update `NEWS.md` with new header for next version's release notes.
