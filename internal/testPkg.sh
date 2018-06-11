## Launch into a test package using packrat
cd ~/git
R CMD build packrat && R CMD INSTALL packrat_0.2.0.99.tar.gz
rm -rf testPkg
mkdir testPkg
cd testPkg
touch foo.R bar.R baz.R
echo "library(digest)" >> foo.R
cp ~/git/digest/DESCRIPTION ~/git/testPkg/DESCRIPTION
R --vanilla -e "options(repos = c(CRAN = 'https://cran.rstudio.org')); library(packrat); dir.create('.git'); bootstrap(source='~/git/packrat')"
R
