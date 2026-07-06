SECTIONS := $(wildcard sections/*) 
APPENDICES := $(wildcard appendices/*)
CONFIG := config/_config.yml
OUTPUT := config/_output.yml

all: analysis draft

## -----------------------------
## Report
## -----------------------------
draft: output/main.pdf


output/%.pdf: %.Rmd $(SECTIONS) $(APPENDICES)
	Rscript -e 'rmarkdown::render("$<", output_format = "bookdown::pdf_document2", output_dir = "$(dir $@)", quiet = TRUE)'

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
## Cleanup
## -----------------------------
clean:
	rm -r $(LOGDIR)

view:
	open -a Skim output/main.pdf

help:
	Rscript render.R --help
