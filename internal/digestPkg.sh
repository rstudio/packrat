#!/usr/bin/env sh

## Testing a package using a source package tarball
PACKAGE_NAME="digestPkg"
cd ~/git
R CMD build packrat && R CMD INSTALL packrat_0.2.0.101.tar.gz
rm -rf ${PACKAGE_NAME}
mkdir ${PACKAGE_NAME}
cd ${PACKAGE_NAME}
touch foo.R bar.R baz.R
echo "library(digest)" >> foo.R
cp ~/git/packrat/internal/digestPkg.Rproj ~/git/${PACKAGE_NAME}/
R --vanilla -e "options(repos = c(CRAN = 'http://cran.rstudio.org')); library(packrat); dir.create('.git'); bootstrap(source=c('~/git/packrat', '~/git/digest_0.6.4.1.tar.gz'))"
R
