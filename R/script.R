#'
#'     Script to study effect of pixel size
#'
#'     Copyright (c) Adrian Baddeley, 2022-2024

library(spatstat)
source("evidenceTable.R")
source("wofe.R")
source("pixelsizeData.R")
source("pixelsizeCode.R")

## Plot full Murchison data
plotit("murchisonGreen", { showdata(mur) })
plotit("murchisonGreenFaults", { showdata(mur, show.faults=TRUE) })

## avoid accidents: henceforth use only 'mur30'
rm(mur)

## Plot 30-deposit subset of Murchison data
plotit("murchison30Green", { showdata(mur30) })
plotit("murchison30GreenFaults", { showdata(mur30, show.faults=TRUE) })

####################################################
## Perform main experiment, and produce graphics
## (approx 6 hours for each call if re-computing)

## rule S1
doit("pixelsizeS1")

## rule R1, ordered
doit("pixelsizeR1ordered", respectDeposits=TRUE)

## rule R1, maximal
## doit("pixelsizeR1maximal", respectDeposits=TRUE, vote="any")

