SECTIONS := $(wildcard sections/*) 
OTHER := config.yml refs.bib

DATE := $(shell date +%Y-%m-%d)
SUBMISSION_DIR := submissions/jebo/submissions/$(DATE)
DIFF_DIR := output/diff

MANUSCRIPT := output/main.pdf
TITLEPAGE := output/titlepage.docx
ANALYSIS_LOG := output/run_analysis.log
DIFF_FILE := output/diff/main_diff.pdf
RESPONSE := submissions/jebo/round2/response.pdf

LATEX_EXPORT := output/latex_export

all: manuscript

manuscript: $(MANUSCRIPT) $(TITLEPAGE)

review: $(RESPONSE)

diff: $(DIFF_FILE)

latex-export: $(LATEX_EXPORT)/README.txt


$(MANUSCRIPT): main.Rmd $(SECTIONS) $(ANALYSIS_LOG) $(OTHER)
	Rscript -e 'rmarkdown::render("$<", output_format = "all", output_dir = "$(dir $@)")'

$(TITLEPAGE) : main.Rmd
	Rscript -e 'rmarkdown::render("$<", output_file = "$@", params = list(titlepage_only = "yes"), output_format = "bookdown::word_document2")'

$(ANALYSIS_LOG): run_analysis.R
	Rscript $< > $@ 2>&1

$(RESPONSE): $(RESPONSE:.pdf=.Rmd) refs.bib
	cp refs.bib $(dir $@)
	Rscript -e "rmarkdown::render('$<', output_format = 'all')"


$(DIFF_FILE):
	mkdir -p $(dir $@)
	cp -R output/figures $(dir $@)
	cp output/main.tex $(dir $@)/new.tex
	cp submissions/jebo/round1/manuscript.tex $(dir $@)/old.tex
	cd $(dir $@) && \
	latexdiff \
		--flatten \
		--type=UNDERLINE \
		--graphics-markup=both \
		--exclude-textcmd="section,caption" \
		--exclude-safecmd="multicolumn" \
		old.tex new.tex > diff.tex && \
	latexmk -f -xelatex -interaction=nonstopmode diff.tex


$(LATEX_EXPORT)/README.txt: output/main.tex refs.bib
	rm -rf $(LATEX_EXPORT)
	mkdir -p $(LATEX_EXPORT)

	cp output/main.tex $(LATEX_EXPORT)/
	cp refs.bib $(LATEX_EXPORT)/

	mkdir -p $(LATEX_EXPORT)/output
	cp -R output/figures $(LATEX_EXPORT)/output

	echo "Compile with:\nlatexmk -xelatex main.tex\n" > $@
	echo "Folder structure:" >> $@
	tree $(LATEX_EXPORT) >> $@

latex-compile: 
	cd $(LATEX_EXPORT); latexmk -xelatex main.tex && open main.pdf


view:
	open -a Skim $(MANUSCRIPT)

