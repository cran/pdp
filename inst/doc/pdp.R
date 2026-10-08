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

## ----install, eval=FALSE------------------------------------------------------
# # Install from r-universe (recommended):
# install.packages("pdp", repos = c("https://bgreenwell.r-universe.dev",
#                                   "https://CRAN.R-project.org"))
# 
# # Install the latest development version from GitHub:
# pak::pak("bgreenwell/pdp")

## ----boston-rf----------------------------------------------------------------
library(pdp)
library(randomForest)

data(boston)  # load the (corrected) Boston housing data
set.seed(101)  # for reproducibility
boston.rf <- randomForest(cmedv ~ ., data = boston, ntree = 250)

# Partial dependence of cmedv on lstat
pd <- partial(boston.rf, pred.var = "lstat", train = boston)
head(pd)

## ----boston-plot--------------------------------------------------------------
# tinyplot-based display; rug marks show the min/max and deciles of lstat to
# help avoid interpreting the plot where there's little data
plot(pd, rug = TRUE, train = boston)

## ----boston-plot-lattice------------------------------------------------------
# lattice-based equivalent
plot(pd, rug = TRUE, train = boston, lattice = TRUE)

## ----boston-two---------------------------------------------------------------
pd2 <- partial(boston.rf, pred.var = c("lstat", "rm"), chull = TRUE,
               train = boston)
plot(pd2, contour = TRUE)

## ----boston-wireframe---------------------------------------------------------
# 3-D surface instead of a false color level plot
plot(pd2, lattice = TRUE, levelplot = FALSE, zlab = "cmedv", drape = TRUE,
     colorkey = FALSE, screen = list(z = -20, x = -60))

## ----boston-three, fig.width=7------------------------------------------------
# Three predictors: the third is binned into overlapping intervals and used
# to panel the display (see the `number` and `overlap` arguments)
pd3 <- partial(boston.rf, pred.var = c("lstat", "rm", "age"),
               grid.resolution = 10, chull = TRUE, batch.size = 1e6,
               train = boston)
plot(pd3, lattice = TRUE)

## ----pima-rf------------------------------------------------------------------
data(pima)  # load the synthetic diabetes data
pima2 <- na.omit(pima)
set.seed(102)
pima.rf <- randomForest(diabetes ~ ., data = pima2, ntree = 250)

# Partial dependence of the probability of testing positive on glucose
partial(pima.rf, pred.var = "glucose", prob = TRUE, which.class = "pos",
        plot = TRUE, rug = TRUE, train = pima2)

## ----inv-link-----------------------------------------------------------------
fit <- glm(carb ~ ., data = mtcars, family = poisson)

# Partial dependence of the number of carburetors on mpg (response scale)
partial(fit, pred.var = "mpg", inv.link = exp, plot = TRUE, train = mtcars)

## ----grid---------------------------------------------------------------------
partial(boston.rf, pred.var = "lstat", quantiles = TRUE, probs = 1:19/20,
        plot = TRUE, train = boston)

