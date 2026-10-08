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

## ----ice----------------------------------------------------------------------
library(pdp)
library(randomForest)

data(boston)
set.seed(101)
boston.rf <- randomForest(cmedv ~ ., data = boston, ntree = 250)

rm.ice <- partial(boston.rf, pred.var = "rm", ice = TRUE, train = boston)
head(rm.ice)  # one row per observation per grid point

## ----ice-plot-----------------------------------------------------------------
plot(rm.ice, alpha = 0.1, rug = TRUE, train = boston)

## ----cice-plot----------------------------------------------------------------
plot(rm.ice, center = TRUE, alpha = 0.1)

## ----pred-fun-----------------------------------------------------------------
# 10% trimmed mean instead of the ordinary average
pred.trimmed <- function(object, newdata) {
  mean(predict(object, newdata = newdata), trim = 0.1)
}

pd.trimmed <- partial(boston.rf, pred.var = "rm", pred.fun = pred.trimmed,
                      train = boston)
plot(pd.trimmed)

