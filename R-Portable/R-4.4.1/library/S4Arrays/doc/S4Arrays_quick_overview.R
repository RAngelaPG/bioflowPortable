## ----eval=FALSE---------------------------------------------------------------
#  if (!require("BiocManager", quietly=TRUE))
#      install.packages("BiocManager")
#  BiocManager::install("S4Arrays")

## ----message=FALSE------------------------------------------------------------
library(S4Arrays)

showClass("Array")

## -----------------------------------------------------------------------------
sessionInfo()

