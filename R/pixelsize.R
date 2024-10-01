#'
#'     Script to study effect of pixel size
#'
#'     Copyright (c) Adrian Baddeley, 2022-2024

library(spatstat)

#' load new code 
source("evidenceTable.R")
source("wofe.R")

source("pixelsizeData.R")
source("pixelsizeCode.R")

## Plot Murchison data
plotit("murchisonGreen", { showdata(mur) })
plotit("murchisonGreenFaults", { showdata(mur, show.faults=TRUE) })

####################################################
## Perform main experiment, and produce graphics
## (approx 6 hours for each call if re-computing)

## ..... ADJUST PIXEL SIZE
## rule S1
doit("pixelsizeGreenAdjustEpsFull")

## rule R1, ordered
doit("pixelsizeGreenRespectAdjustEpsFull", 
     respectDeposits=TRUE)

## rule R1, maximal
doit("pixelsizeGreenRespectAnyAdjustEpsFull", 
     respectDeposits=TRUE, vote="any")

## ....  GROW FRAME
## rule S1, grow frame
doit("pixelsizeGreenGrowFrameFull", 
     rule.eps="grow.frame")

## rule R1
doit("pixelsizeGreenRespectGrowFrameFull", 
     respectDeposits=TRUE,
     rule.eps="grow.frame")

## rule R1, maximal
doit("pixelsizeGreenRespectAnyGrowFrameFull", 
     respectDeposits=TRUE, vote="any",
     rule.eps="grow.frame")


#####################################################
## Plot subset of Murchison data around the town of Reedy

plotit("murchisonReedyGreen", { showdata(murReedy) })
plotit("murchisonReedyGreenFaults", { showdata(murReedy, show.faults=TRUE) })

## approx 1.5 hours for each call if re-computing
doit("pixReedyGrnAdjustEpsFull", 0.02, 2, MUR=murReedy)
doit("pixReedyGrnRespectAdjustEpsFull", 0.02, 2, MUR=murReedy,
      respectDeposits=TRUE)
doit("pixReedyGrnGrowFrameFull", 0.02, 2, MUR=murReedy,
      rule.eps="grow.frame")
doit("pixReedyGrnRespectGrowFrameFull", 0.02, 2, MUR=murReedy,
      respectDeposits=TRUE, rule.eps="grow.frame")

#####################################################
## Another subset: North East quarter of data

plotit("murchisonNEgreen", { showdata(murNE) })
plotit("murchisonNEgreenFaults", { showdata(murNE, show.faults=TRUE) })

##  about 30 minutes for each call if re-computing
doit("pixNEgrnAdjustEpsFull", 0.1, 2, 80, MUR=murNE)
doit("pixNEgrnRespectAdjustEpsFull", 0.1, 2, 80, MUR=murNE,
     respectDeposits=TRUE)
doit("pixNEgrnGrowFrameFull", 0.1, 2, 80, MUR=murNE,
     rule.eps="grow.frame")
doit("pixNEgrnRespectGrowFrameFull", 0.1, 2, 80,  MUR=murNE,
     respectDeposits=TRUE, rule.eps="grow.frame")

#####################################################
## Another subset: North East quarter of North East quarter!

plotit("murchisonNENEgreen", { showdata(murNENE) })
plotit("murchisonNENEgreenFaults", { showdata(murNENE, show.faults=TRUE) })

##  about 5 minutes for each call if re-computing
doit("pixNENEgrnAdjustEpsFull", 0.1, 2, 40, MUR=murNENE)
doit("pixNENEgrnRespectAdjustEpsFull", 0.1, 2, 40, MUR=murNENE,
     respectDeposits=TRUE)
doit("pixNENEgrnGrowFrameFull", 0.1, 2, 40, MUR=murNENE,
     rule.eps="grow.frame")
doit("pixNENEgrnRespectGrowFrameFull", 0.1, 2, 40, MUR=murNENE,
     respectDeposits=TRUE, rule.eps="grow.frame")

#####################################################
## Bad subset found by experiment

plotit("murchisonBadGreen", { showdata(murBad) })
plotit("murchisonBadGreenFaults", { showdata(murBad, show.faults=TRUE) })

##  about 3 minutes for each call if re-computing
doit("pixBadGrnAdjustEpsFull", 0.1, 2, 32, MUR=murBad)
doit("pixBadGrnRespectAdjustEpsFull", 0.1, 2, 32, MUR=murBad,
     respectDeposits=TRUE)
doit("pixBadGrnGrowFrameFull", 0.1, 2, 32, MUR=murBad,
     rule.eps="grow.frame")
doit("pixBadGrnRespectGrowFrameFull", 0.1, 2, 32, MUR=murBad,
     respectDeposits=TRUE, rule.eps="grow.frame")

#####################################################
## 25 large deposits (extracted by Warick)

plotit("murchisonBig25Green", { showdata(murBig25) })
plotit("murchisonBig25GreenFaults", { showdata(murBig25, show.faults=TRUE) })

## (approx 6 hours for each call if re-computing)
## rule S1
doit("pixBig25GrnAdjustEpsFull", MUR=murBig25)

## rule R1, ordered
doit("pixBig25GrnRespectAdjustEpsFull", MUR=murBig25,
     respectDeposits=TRUE)

if(FALSE){
## rule R1, maximal
doit("pixBig25GrnRespectAnyAdjustEpsFull", MUR=murBig25,
     respectDeposits=TRUE, vote="any")
## ........... GROW FRAME  .....................
## rule S1, grow frame
doit("pixBig25GrnGrowFrameFull", MUR=murBig25,
     rule.eps="grow.frame")
## rule R1
doit("pixBig25GrnRespectGrowFrameFull", MUR=murBig25,
     respectDeposits=TRUE,
     rule.eps="grow.frame")
## rule R1, maximal
doit("pixBig25GrnRespectAnyGrowFrameFull", MUR=murBig25,
     respectDeposits=TRUE, vote="any",
     rule.eps="grow.frame")
}

#####################################################
## 25 large deposits plus 5 deposits closest to greenstone boundary

plotit("murchisonNaughty30Green", { showdata(murNaughty30) })
plotit("murchisonNaughty30GreenFaults",
{ showdata(murNaughty30, show.faults=TRUE) })

## (approx 6 hours for each call if re-computing)
## rule S1
doit("pixNaughty30GrnAdjustEpsFull", MUR=murNaughty30)

## rule R1, ordered
doit("pixNaughty30GrnRespectAdjustEpsFull", MUR=murNaughty30,
     respectDeposits=TRUE)

if(FALSE){

## rule R1, maximal
doit("pixNaughty30GrnRespectAnyAdjustEpsFull", MUR=murNaughty30,
     respectDeposits=TRUE, vote="any")
  
## ........... GROW FRAME  .....................
## rule S1, grow frame
doit("pixNaughty30GrnGrowFrameFull", MUR=murNaughty30,
     rule.eps="grow.frame")
## rule R1
doit("pixNaughty30GrnRespectGrowFrameFull", MUR=murNaughty30,
     respectDeposits=TRUE,
     rule.eps="grow.frame")
## rule R1, maximal
doit("pixNaughty30GrnRespectAnyGrowFrameFull", MUR=murNaughty30,
     respectDeposits=TRUE, vote="any",
     rule.eps="grow.frame")
}

#####################################################
## Demonstration example - 8 x 8 km subset with only one deposit
plotit("murchisonDemo", { showdata(murDemo) })

plotit("murchisonDemo3", { showdatawithpixels(murDemo, 3) })

plotit("murchisonDemo4", { showdatawithpixels(murDemo, 4) })

plotit("murchisonDemo5", { showdatawithpixels(murDemo, 5) })

plotit("murchisonDemo6", { showdatawithpixels(murDemo, 6) })

#######################################################
## Illustrations of steps in the algorithms
## 4 x 4 grid
## Rule 'S'
plotit("ruleSshowBres4", { showdiscretised(murDemo, 4, what="B", rule="S") })
plotit("ruleSshowDres4", { showdiscretised(murDemo, 4, what="D", rule="S") })
## Rule 'R'
plotit("ruleRshowBres4", { showdiscretised(murDemo, 4, what="B", rule="R") })
plotit("ruleRshowDres4", { showdiscretised(murDemo, 4, what="D", rule="R") })
## 5 x 5 grid
## Rule 'S'
plotit("ruleSshowBres5", { showdiscretised(murDemo, 5, what="B", rule="S") })
plotit("ruleSshowDres5", { showdiscretised(murDemo, 5, what="D", rule="S") })
## Rule 'R'
plotit("ruleRshowBres5", { showdiscretised(murDemo, 5, what="B", rule="R") })
plotit("ruleRshowDres5", { showdiscretised(murDemo, 5, what="D", rule="R") })

###########################################################

## Feature is 2km buffer around faults, instead of greenstone

fn <- "faultbuffer"
ft <- "fault buffer (2km)"

## (approx 6 hours for each call if re-computing)
doit("pixelsizeBuf2AdjustEpsFull", 
     feature=fn, featuretext=ft)

if(FALSE) {
doit("pixelsizeBuf2RespectAdjustEpsFull", 
     respectDeposits=TRUE, 
     feature=fn, featuretext=ft)

doit("pixelsizeBuf2GrowFrameFull", 
     rule.eps="grow.frame",
     feature=fn, featuretext=ft)

doit("pixelsizeBuf2RespectGrowFrameFull", 
     respectDeposits=TRUE, rule.eps="grow.frame",
     feature=fn, featuretext=ft)
}

