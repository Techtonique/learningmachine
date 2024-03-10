## -----------------------------------------------------------------------------
library(ggplot2)
library(learningmachine)
library(caret)
library(palmerpenguins)
library(mlbench)
library(skimr)
library(reshape2)
library(pROC)
library(reticulate)

## ----cache=TRUE---------------------------------------------------------------
reticulate::py_install("scikit-learn")
sklearn <- reticulate::import("sklearn")

## -----------------------------------------------------------------------------
reticulate::py_install("matplotlib")
plt <- reticulate::import("matplotlib")$pyplot

