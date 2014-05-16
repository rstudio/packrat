## Launch into a test package using packrat
cd ~/git
R CMD build packrat && R CMD INSTALL packrat_0.1.0.99.tar.gz
rm -rf testPkg
mkdir testPkg
cd testPkg
touch foo.R bar.R baz.R
echo "library(digest)" >> foo.R
R --vanilla -e "library(packrat); bootstrap(source='~/git/packrat')"
R
