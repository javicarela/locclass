R	:= R
RSCRIPT	:= Rscript
DELETE	:= rm -fR

.SILENT:
.PHONEY: clean test check build install package data usage help

usage:
	echo "Available targets:"
	echo ""
	echo " install  - install the package, writing the output into install.log"
	echo " check    - run R CMD check on the package"
	echo " package  - build source package"
	echo " help     - shows all available targets"
	echo " test	- run tests"
	echo " rforge 	- prepare package for R-forge"

help: usage
	echo " clean    - clean up package cruft"
	echo " pkg/locClassMlr      - roxygenize skel/ into ../pkg/locClassMlr/"

install: clean pkg
	echo "Installing package ..."
	${R} CMD INSTALL ../pkg/locClassMlr > install.log 2>&1

test: install
	echo "Testing package ..."
	${RSCRIPT} ./test_all.R

check: clean pkg
	echo "Running R CMD check ..."
	${R} CMD check ../pkg/locClassMlr && ${DELETE} pkg.Rcheck

clean:
	echo "Cleaning up ..."
	${DELETE} skel/src/*.o skel/src/*.so skel.Rcheck
	${DELETE} ../pkg/locClassMlr
	${DELETE} .RData .Rhistory build.log install.log roxygen.log package.log

package: pkg
	echo "Building package file ..."
	${R} CMD build ../pkg/locClassMlr/ > package.log 2>&1

rforge: package
	  echo "Deleting unused files ..."
	${DELETE} ../pkg/locClassMlr/.svn
	${DELETE} ../pkg/locClassMlr/inst/.svn
	${DELETE} ../pkg/locClassMlr/inst/tests/.Rapp.history
	${DELETE} ../pkg/locClassMlr/inst/tests/.svn
	${DELETE} ../pkg/locClassMlr/inst/doc/.svn
	${DELETE} ../pkg/locClassMlr/R/.Rapp.history
	${DELETE} ../pkg/locClassMlr/R/.svn
	${DELETE} ../pkg/locClassMlr/src/.svn

pkg: clean
	echo "Roxygenizing package ..."
	${RSCRIPT} ./tools/roxygenize > roxygen.log 2>&1
