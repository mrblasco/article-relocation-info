
OUTDIR=docs
INPUT=main.Rmd
FORMAT=all

echo "Rendering $INPUT to $OUTDIR..."
Rscript -e "rmarkdown::render('$INPUT', output_format = '$FORMAT', output_dir='$OUTDIR')"


