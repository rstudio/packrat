#!/usr/bin/env sh

## Testing a package using a source package tarball
PACKAGE_NAME="shinyPkg"
cd ~/git
R CMD build packrat && R CMD INSTALL packrat_0.2.0.100.tar.gz
rm -rf ${PACKAGE_NAME}
mkdir ${PACKAGE_NAME}
cd ${PACKAGE_NAME}
touch foo.R bar.R baz.R
echo "library(shiny)" >> foo.R
cp ~/git/packrat/internal/digestPkg.Rproj ~/git/${PACKAGE_NAME}/
R --vanilla -e "options(repos = c(CRAN = 'http://cran.rstudio.org')); library(packrat); dir.create('.git'); bootstrap(source=c('~/git/packrat'))"
R
