#'
#'     Example data for studying effect of pixel size
#'
#'     Creates 'solist' object 'mur30'
#' 
#'     Copyright (c) Adrian Baddeley, 2022-2024
#' 

library(spatstat)

## Murchison data (supplied in spatstat.data) - rescale to kilometres
mur <- solapply(murchison, rescale, 1000, unitname="km")

## select subset of 30
##     (the 25 known deposits with substantial endowment, plus 5 occurrences)
the30 <- c(1,  23,  83,  84,  86,  90,  91,  92,  93,  94,
           118, 131, 132, 138, 139, 148, 161, 170, 171,
           177, 187, 189, 190, 191, 198, 205, 206, 207, 208, 247)
mur30 <- mur
mur30$gold <- mur$gold[the30]


