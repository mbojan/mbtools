include Makevars

.PHONY: check
check: 
	$(RCALL) check -o $(CHECKDIR) $(CURDIR)

.PHONY: build
build: 
	$(RCALL) build $(SRCDIR)
	mv $(PKGNAME)*tar.gz $(shell dirname $(SRCDIR))/

.PHONY: install
install: 
	mkdir -p $(INSTALLDIR)
	$(RCALL) INSTALL -l $(INSTALLDIR) $(SRCDIR)

.PHONY: commit
commit:
	@svn diff > svndiff
	@svn commit --editor-cmd 'vim -c "sp svndiff"'
	@rm svndiff


.PHONY: checkcode
checkcode:
	$(RCALL) check  --no-codoc --no-vignettes --no-latex -o $(CHECKDIR) $(SRCDIR)

.PHONY: checkdoc
checkdoc:
	$(RCALL) check --no-tests --no-install -o $(CHECKDIR) $(SRCDIR)

.PHONY: cleanall
cleanall:
	- rm -f $(CHECKDIR) $(PKGPDF) $(PKGDVI)

.PHONY: uninstall
uninstall:
	- rm -R --force $(INSTALLDIR)

# package manuals

.PHONY: pdf
pdf: $(PKGPDF)

$(PKGPDF): $(SRCDIR:=/man/*.Rd)
	$(RCALL) Rd2pdf --no-preview -o $(shell dirname $(SRCDIR))/$(PKGPDF) $(SRCDIR)

.PHONY: dvi
dvi: $(PKGDVI)

$(PKGDVI): $(SRCDIR:=/man/*.Rd)
	$(RCALL) Rd2dvi --no-preview -o $(shell dirname $(SRCDIR))/$(PKGDVI) $(SRCDIR)
