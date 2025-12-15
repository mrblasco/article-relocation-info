# -------------------------------
# Config
# -------------------------------
OUTDIR 			:= docs
SECTIONS 		:= $(wildcard *.Rmd)
ASSETS 			:= refs.bib _output.yml
DATE        	:= $(shell date +%Y-%m-%d)
SLUG        	:= fairness-asylum

# Output files
HTML_OUT    := $(OUTDIR)/index.html
PDF_OUT     := $(OUTDIR)/report.pdf
DOCX_OUT    := $(OUTDIR)/report.docx

# Versioned exports
PDF_VERSION := $(DATE)-$(SLUG).pdf
DOCX_VERSION := $(DATE)-$(SLUG).docx

all:

# -------------------------------
# Build rules
# -------------------------------
$(OUTDIR):
	mkdir -p $(OUTDIR)

html: $(HTML_OUT)
pdf:  $(PDF_OUT)
docx: $(DOCX_OUT)

$(PDF_OUT): main.Rmd $(SECTIONS) $(ASSETS) | $(OUTDIR)
	Rscript -e 'rmarkdown::render("$<", output_file = "$@", output_format="bookdown::pdf_document2")'

$(HTML_OUT): main.Rmd $(SECTIONS) $(ASSETS) | $(OUTDIR)
	Rscript -e 'rmarkdown::render("$<", output_file = "$@", output_format="distill::distill_article")'

$(DOCX_OUT): main.Rmd $(SECTIONS) $(ASSETS) | $(OUTDIR)
	Rscript -e 'rmarkdown::render("$<", output_file = "$@", output_format="bookdown::word_document2")'


# -------------------------------
# Journal submission versions
# (e.g., anonymized, double-spaced, different template)
# -------------------------------
journal: journal-pdf

journal-pdf: $(MAIN) $(SECTIONS) $(ASSETS) | $(OUTDIR)
	Rscript -e 'rmarkdown::render("$<", output_file="$(JOURNAL_PDF)", params=list(journal=TRUE), output_format="bookdown::pdf_document2")'


# -------------------------------
# Utilities
# -------------------------------
view:
	open -a Skim $(PDF_OUT)

clean:
	rm *.ttt *.fff *.log

# -------------------------------
# Versioned copies for submission
# -------------------------------

archive:
	git tag submission-$(DATE)

