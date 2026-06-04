CHAPTERS := $(wildcard chapters/*) 
APPENDICES := $(wildcard appendices/*)
CONFIG := config/_config.yml
OUTPUT := config/_output.yml

output/main.pdf: main.Rmd $(CONFIG) $(OUTPUT) $(CHAPTERS) $(APPENDICES)
	@Rscript scripts/render.R \
		--input $< \
		--params $(CONFIG) \
		--output_yaml $(OUTPUT) \
		--format bookdown::pdf_document2 \
		--output_dir $(dir $@)

view:
	open -a Skim output/main.pdf

help:
	Rscript render.R --help
