# instability

This repository contains the code scripts for our paper on instability.
It is required by the journal. The repository will become public when we
submit the article.

| Folder name | Contents |
| --------  | -------- |
| `R`       | Folder of R code |
| `R/data-auto` | Folder for saved results of computation |
| `R/pix-auto` | Folder for saved figure files |

| File name | Contents |
| --------  | -------- |
| `R`       | Folder of R code |
| `R/data-auto` | Folder for saved results of computation |
| `R/pix-auto` | Folder for saved figure files |
| `R/script.R` | R script to compute results and figures for the article |
| `R/wofe.R`   | R functions for Weights of Evidence calculation |
| `R/evidenceTable.R` | R functions required by `wofe.R` |
| `R/pixelsizeData.R` | R code to load data used in the article |
| `R/pixelsizeCode.R` | R functions used by `script.R` |
| `R/pixelsize-figs.Rmd` | R markdown document used to generate more figures |


The R source file `R/script.R` is a script that computes and saves the results
for the article, and generates figures. Figures are saved in `pix-auto`
and data files are saved in `data-auto`.

The R markdown document `R/pixelsize-figs.Rmd` generates some
additional figures using the saved data.

In a Linux system, simply type `make` to run everything. Since that would take
about 12 hours on a typical laptop, the previously-computed results
are saved in `data-auto`.
