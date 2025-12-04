OUTDIR := docs
SECTIONS := $(wildcard *.Rmd)
ASSETS := refs.bib _output.yml

all: html view

pdf: $(OUTDIR)/report.pdf
docx: $(OUTDIR)/report.docx
html: $(OUTDIR)/index.html

$(OUTDIR)/report.pdf: main.Rmd $(SECTIONS) $(ASSETS)
	Rscript -e 'rmarkdown::render("$<", output_file = "$@", output_format="bookdown::pdf_document2")'

$(OUTDIR)/index.html: main.Rmd $(SECTIONS) $(ASSETS)
	Rscript -e 'rmarkdown::render("$<", output_file = "$@", output_format="distill::distill_article")'

$(OUTDIR)/report.docx: main.Rmd $(SECTIONS) $(ASSETS)
	Rscript -e 'rmarkdown::render("$<", output_file = "$@", output_format="bookdown::word_document2")'

view:
	open $(OUTDIR)/index.html
