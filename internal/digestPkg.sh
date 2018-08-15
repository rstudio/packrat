#!/usr/bin/env sh

## Testing a package using a source package tarball
PACKAGE_NAME="digestPkg"
cd ~/git
R CMD INSTALL packrat
rm -rf ${PACKAGE_NAME}
mkdir ${PACKAGE_NAME}
cd ${PACKAGE_NAME}
touch foo.R bar.R baz.R
echo "library(digest)" >> foo.R
cp ~/git/packrat/internal/digestPkg.Rproj ~/git/${PACKAGE_NAME}/
R --vanilla -e "options(repos = c(CRAN = 'https://cran.rstudio.org')); library(packrat); dir.create('.git'); init(options = list(local.repos = '~/git'))"
R
