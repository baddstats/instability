default: all

all: results zoomed

results:
	R --vanilla < script.R

zoomed: pixelsize-figs.html

pixelsize-figs.html: pixelsize-figs.Rmd
	Rscript -e "rmarkdown::render('pixelsize-figs.Rmd')"
