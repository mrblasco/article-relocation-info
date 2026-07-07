SECTIONS := $(wildcard sections/*) 
APPENDICES := $(wildcard appendices/*)
CONFIG := config/_config.yml
OUTPUT := config/_output.yml

all: analysis draft

## -----------------------------
## Report
## -----------------------------
draft: output/main.pdf

output/%.pdf output/%.docx: %.Rmd $(SECTIONS) $(APPENDICES)
	Rscript -e 'rmarkdown::render("$<", output_format = "all", output_dir = "$(dir $@)", quiet = TRUE)'


## -----------------------------
## Analysis pipeline logs
## -----------------------------
SCRIPTS := \
	01_clean_data.R \
	02_process_data.R \
	03_analysis_top_ranks.R \
	04_analysis_avg_rank.R \
	05_conditional_ate.R

LOGDIR := output/logs
LOGS := $(SCRIPTS:%.R=$(LOGDIR)/%.log)

analysis: $(LOGS)

$(LOGDIR)/%.log : scripts/%.R
	@mkdir -p $(dir $@)
	Rscript $< > $@ 2>&1 | tee $@


## -----------------------------
## Diff. version 
## -----------------------------

TMP := tmp

$(TMP):
	mkdir -p $(TMP)/output

prepare: $(TMP)
	cp output/main.tex submissions/manuscript/rev1/main.tex
	cp submissions/jebo/manuscript/initial/manuscript.tex $(TMP)/old.tex
	cp output/main.tex $(TMP)/new.tex
	cp refs.bib $(TMP)/
	cp -r output/figures $(TMP)/output/

diff: prepare
	cd $(TMP) && \
	latexdiff --flatten --type=UNDERLINE old.tex new.tex > diff.tex && \
	latexmk -f -xelatex diff.tex


## -----------------------------
## Response to reviewers
## -----------------------------

RESPONSE := submissions/jebo/response_to_referees/response.Rmd

review: submissions/jebo/response_to_reviewers/response.pdf

submissions/jebo/response_to_reviewers/response.pdf : submissions/jebo/response_to_reviewers/response.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'all')"

## -----------------------------
## Cleanup
## -----------------------------
clean:
	rm -r $(LOGDIR)

view:
	open -a Skim output/main.pdf

help:
	Rscript render.R --help
