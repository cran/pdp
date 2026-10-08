## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4.5,
  fig.align = "center",
  message = FALSE,
  warning = FALSE
)

## ----boston-rf----------------------------------------------------------------
library(pdp)
library(randomForest)

data(boston)
set.seed(101)
boston.rf <- randomForest(cmedv ~ ., data = boston, ntree = 250)

## ----batch--------------------------------------------------------------------
system.time(  # classic: one predict() call per grid point
  pd1 <- partial(boston.rf, pred.var = "lstat", train = boston)
)
system.time(  # batched: score up to one million rows per predict() call
  pd2 <- partial(boston.rf, pred.var = "lstat", train = boston,
                 batch.size = 1e6)
)
identical(pd1, pd2)

## ----batch-plot---------------------------------------------------------------
plot(pd1)  # classic (line)
tinyplot::tinyplot_add(yhat ~ lstat, data = pd2, type = "p", col = "red2")

## ----parallel, eval=FALSE-----------------------------------------------------
# library(doParallel)
# 
# cl <- makeCluster(4)  # use 4 workers
# registerDoParallel(cl)
# pd <- partial(boston.rf, pred.var = c("lstat", "rm"), chull = TRUE,
#               train = boston, parallel = TRUE)
# stopCluster(cl)

## ----gbm----------------------------------------------------------------------
library(gbm)

set.seed(103)
boston.gbm <- gbm(cmedv ~ ., data = boston, distribution = "gaussian",
                  n.trees = 500, interaction.depth = 3, shrinkage = 0.1)

system.time(
  pd.recursive <- partial(boston.gbm, pred.var = "lstat", n.trees = 500,
                          train = boston)  # recursive = TRUE is the default
)
system.time(
  pd.brute <- partial(boston.gbm, pred.var = "lstat", n.trees = 500,
                      recursive = FALSE, train = boston, batch.size = 1e6)
)

## ----gbm-plot-----------------------------------------------------------------
plot(pd.recursive)  # recursive (line)
tinyplot::tinyplot_add(yhat ~ lstat, data = pd.brute, type = "p", col = "red2")

## ----approx-------------------------------------------------------------------
partial(boston.rf, pred.var = "lstat", approx = TRUE, plot = TRUE,
        train = boston)

