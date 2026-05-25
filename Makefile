# -------------------------------
# Config
# -------------------------------
MANUSCRIPT  := manuscript/main.Rmd
SECTIONS    := $(wildcard manuscript/*.Rmd)
ASSETS      := manuscript/refs.bib manuscript/_output.yml
OUTDIR      := docs
DATE        := $(shell date +%Y-%m-%d)
SLUG        := fairness-asylum

JOURNAL     ?= jebo
REV         ?= rev1
FROM        ?= initial
TO          ?= rev1

# Output files
HTML_OUT    := $(OUTDIR)/index.html
PDF_OUT     := $(OUTDIR)/main.pdf
DOCX_OUT    := $(OUTDIR)/report.docx

# Versioned exports
PDF_VERSION  := $(DATE)-$(SLUG).pdf
DOCX_VERSION := $(DATE)-$(SLUG).docx

# -------------------------------
# All targets
# -------------------------------
all: analysis pdf

manuscript: html pdf

# -------------------------------
# Analysis
# -------------------------------

DATA_FILES  := data/processed/fair_survey_clean.rds data/processed/fair_survey_long.rds
DESC_TABLES := results/tables/descriptives_all_tables.rds
ROLOGIT     := results/tables/rologit_coeffs.rds

$(DATA_FILES): scripts/01_prepare_data.R
	@echo "Preparing data..."
	Rscript $<

$(DESC_TABLES): scripts/02_descriptives.R $(DATA_FILES)
	@echo "Running descriptive scripts..."
	Rscript $<

$(ROLOGIT): scripts/33_rologit_base.R $(DATA_FILES)
	@echo "Running rologit..."
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

$(PDF_OUT): $(MANUSCRIPT) $(SECTIONS) $(ASSETS) | $(OUTDIR)
	Rscript -e 'rmarkdown::render("$(MANUSCRIPT)", output_dir="$(OUTDIR)", output_format="bookdown::pdf_document2")'

$(HTML_OUT): $(MANUSCRIPT) $(SECTIONS) $(ASSETS) | $(OUTDIR)
	Rscript -e 'rmarkdown::render("$(MANUSCRIPT)", output_dir="$(OUTDIR)", output_format="distill::distill_article")'

$(DOCX_OUT): $(MANUSCRIPT) $(SECTIONS) $(ASSETS) | $(OUTDIR)
	Rscript -e 'rmarkdown::render("$(MANUSCRIPT)", output_dir="$(OUTDIR)", output_format="bookdown::word_document2")'

# -------------------------------
# Journal submissions
# -------------------------------

# Save a tracked snapshot of the current build.
# Usage: make submit JOURNAL=jebo REV=rev1
submit: pdf
	@mkdir -p submissions/$(JOURNAL)/$(REV)
	@cp $(OUTDIR)/main.tex submissions/$(JOURNAL)/$(REV)/manuscript.tex
	@echo "Saved: submissions/$(JOURNAL)/$(REV)/manuscript.tex"
	@echo "Stage and commit with: git add submissions/ && git commit -m 'snapshot $(JOURNAL) $(REV)'"

# Generate a latexdiff .tex between two submission snapshots.
# Usage: make diff JOURNAL=jebo FROM=initial TO=rev1
diff:
	@mkdir -p submissions/$(JOURNAL)/$(TO)
	latexdiff \
	    --config="PICTUREENV=(?:picture|DIFnomarkup|tabu)[\w\d*@]*" \
	    submissions/$(JOURNAL)/$(FROM)/manuscript.tex \
	    submissions/$(JOURNAL)/$(TO)/manuscript.tex \
	    > submissions/$(JOURNAL)/$(TO)/diff.tex
	@echo "Diff written to: submissions/$(JOURNAL)/$(TO)/diff.tex"

# Compile the diff .tex to PDF.
# Usage: make diff-pdf JOURNAL=jebo REV=rev1
diff-pdf:
	cd submissions/$(JOURNAL)/$(REV) && xelatex diff.tex && xelatex diff.tex
	@echo "PDF built: submissions/$(JOURNAL)/$(REV)/diff.pdf"

# -------------------------------
# Archive (legacy, kept for reference)
# -------------------------------

store: archive/$(DATE)-$(SLUG).tar.gz

archive:
	@mkdir -p $@

archive/$(DATE)-$(SLUG).tar.gz: docs | archive
	@tar -czf $@ $<
	@git tag "version-$(DATE)" -m "output file: $@"

# -------------------------------
# Utilities
# -------------------------------
view:
	open -a Skim $(PDF_OUT)

clean:
	rm -f *.ttt *.fff *.log
