#!/usr/bin/env sh

## Testing a package using a source package tarball
cd ~/git
R CMD build packrat && R CMD INSTALL packrat_0.2.0.99.tar.gz
rm -rf digestPkg
mkdir digestPkg
cd digestPkg
touch foo.R bar.R baz.R
echo "library(digest)" >> foo.R
cp ~/git/digest/DESCRIPTION ~/git/testPkg/DESCRIPTION
R --vanilla -e "options(repos = c(CRAN = 'http://cran.rstudio.org')); library(packrat); dir.create('.git'); bootstrap(source=c('~/git/packrat', '~/git/digest_0.6.4.1.tar.gz'))"
R
