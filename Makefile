include Makefile.defs

VERSION := 1.0-2
TAR = optimParallel_$(VERSION).tar.gz


.PHONY: all update-src tar lib \
        test test-package test-examples \
        check check-cran \
	install \
        winbuild winbuild-devel \
        cp rm 

all: lib

update-src:
	cd optimParallel && sed -i -r -- 's/^Version:.*/Version: '$(VERSION)'/g' DESCRIPTION ;      
	cd optimParallel && sed -i -r -- 's/^Date:.*/Date: '`date +'%F'`'/g' DESCRIPTION ;
	$(RSCRIPT) -e "roxygen2::roxygenize(\"optimParallel\")"

lib: update-src
	mkdir -p lib
	$(R) CMD INSTALL -l lib optimParallel


tar: tar/$(TAR)
tar/$(TAR): update-src
	mkdir -p tar
	$(R) CMD build optimParallel
	mv $(TAR) tar/$(TAR)



test: test-package test-examples  

test-package: update-src
	$(RSCRIPT) -e "devtools::test(\"optimParallel/\")"

test-examples: update-src
	$(RSCRIPT) -e "devtools::run_examples(\"optimParallel/\", show=FALSE, test=FALSE, run=FALSE)"
	rm -f Rplots*.pdf

check: update-src
	$(RSCRIPT) -e "devtools::check(\"optimParallel\", cran = FALSE)"

check-cran: update-src tar/$(TAR)
	$(R) CMD check --as-cran tar/$(TAR) 

install: tar/$(TAR)	
	$(R) CMD INSTALL tar/$(TAR)

winbuild: update-src
	$(RSCRIPT) -e "devtools::check_win_release(pkg = \"optimParallel\")"

winbuild-devel: update-src
	$(RSCRIPT) -e "devtools::check_win_devel(pkg = \"optimParallel\")"



cp: tar
	mkdir -p ../optimParallel
	rm ../optimParallel/* -rf
	cp tar/$(TAR) ../optimParallel/
	rsync -av --exclude=".git/*" optimParallel/* ../optimParallel
	rsync -av optimParallel/.Rbuildignore ../optimParallel/.Rbuildignore

rm:
	rm -fr tar lib tests-local/*.Rout optimParallel/src/*.so optimParallel/src/*.o optimParallel.Rcheck
