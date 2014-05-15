## Base package
cd ${TMPDIR}
rm -rf testPkg
mkdir testPkg
cd testPkg
touch foo.R bar.R baz.R
echo "library(digest)" >> foo.R
R --vanilla -e "packrat::bootstrap(source='~/git/packrat')"
R
