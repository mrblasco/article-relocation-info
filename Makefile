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

# -------------------------------
# All targets 
# -------------------------------
all: analysis pdf

# -------------------------------
# Analysis
# -------------------------------

DATA_FILES := data/processed/fair_survey_clean.rds data/processed/fair_survey_long.rds
DESC_TABLES := results/tables/descriptives_all_tables.rds
ROLOGIT := results/tables/rologit_coeffs.rds

$(DATA_FILES): scripts/01_prepare_data.R
	@echo "Preparing data..."
	Rscript $<

$(DESC_TABLES): scripts/02_descriptives.R $(DATA_FILES)
	@echo "Running descriptive scripts..."
	Rscript $<

$(ROLOGIT): scripts/33_rologit_base.R $(DATA_FILES)
	@echo "Running descriptive scripts..."
	Rscript $<

analysis: $(DESC_TABLES) $(ROLOGIT)

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
# Archive
# -------------------------------

store: archive/$(DATE)-$(SLUG).tar.gz 

archive:
	@mkdir -p $@

archive/$(DATE)-$(SLUG).tar.gz: docs | archive
	@tar -czf $@ $<
	@git tag "version-$(DATE)" -m "output file: $@"

report_diff.tex: docs/report.tex
	@mkdir -p tmp/
	@tar -xzvf archive/2025-12-16-$(SLUG).tar.gz -C tmp
	@latexdiff --config="PICTUREENV=(?:picture|DIFnomarkup|tabu)[\w\d*@]*" tmp/docs/report.tex $< > report_diff.tex

report_diff.pdf: report_diff.tex
	@xelatex $<
	@xelatex $<
	@open -a Skim $@

diff_clean: 
	rm *_diff.*
	rm -r ./tmp/

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