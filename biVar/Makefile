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

help: usage
	echo " clean    - clean up package cruft"
	echo " pkg/biVar      - roxygenize skel/ into ../pkg/biVar/"

install: clean pkg
	echo "Installing package ..."
	${R} CMD INSTALL ../pkg/biVar > install.log 2>&1

test: install
	echo "Testing package ..."
	${RSCRIPT} ./test_all.R

check: clean pkg
	echo "Running R CMD check ..."
	${R} CMD check ../pkg/biVar && ${DELETE} pkg.Rcheck

clean:
	echo "Cleaning up ..."
	${DELETE} skel/src/*.o skel/src/*.so skel.Rcheck
	${DELETE} ../pkg/biVar
	${DELETE} .RData .Rhistory build.log install.log roxygen.log package.log

package: pkg
	echo "Building package file ..."
	${R} CMD build ../pkg/biVar/ > package.log 2>&1

pkg: clean
	echo "Roxygenizing package ..."
	${RSCRIPT} ./tools/roxygenize > roxygen.log 2>&1
	${DELETE} ../pkg/biVar/inst
