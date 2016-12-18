[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/pdp)](https://cran.r-project.org/package=pdp)
[![Build Status](https://travis-ci.org/bgreenwell/pdp.svg?branch=master)](https://travis-ci.org/bgreenwell/pdp)
[![Coverage Status](https://img.shields.io/codecov/c/github/bgreenwell/pdp.svg)](https://codecov.io/github/bgreenwell/pdp?branch=master)
[![Downloads](http://cranlogs.r-pkg.org/badges/pdp)](http://cranlogs.r-pkg.org/badges/pdp)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/pdp)](http://cranlogs.r-pkg.org/badges/grand-total/pdp)
[![Rdoc](http://www.rdocumentation.org/badges/version/pdp)](http://www.rdocumentation.org/packages/pdp)

pdp: An R Package for Constructing Partial Dependence Plots
================

The primary purpose of this package is to provide a general framework for constructing _partial dependence plots_ (PDPs) in R.


### Installation

The R package `pdp` is available from [CRAN](https://cran.r-project.org/package=pdp); the development version is hosted on [GitHub](https://github.com/bgreenwell/pdp). There are two ways to install:

```r
# Install latest release from CRAN (recommended)
install.packages("pdp")

# Install development version from GitHub repo
devtools::install_github("bgreenwell/pdp")
```


### Example usage

The examples below demonstrate various usages of the `pdp` package: regression, classification, and interfacing with the well-known `caret` package. To start, we need to install a few additional packages that will be required to run the examples.

```r
install.packages(c("caret", "ggmap", "kernlab", "randomForest"))
```


#### Regression example

In this example, we fit a random forest to the Boston housing data. (See `?boston` for a brief explanation of the data.) Note, for any of the following examples, you can see a progress bar by simply specifying `progress = "text"` in the call to `partial`. You may also reduce the computation time via the `grid.resolution` option in the call to `partial`.

```r
# Load required packages
library(pdp)  # for constructing PDPs
library(randomForest)  # for random forest algorithm

# Load the Boston housing data
data (boston)  # included with the pdp package

# Fit a random forest to the boston housing data
set.seed(101)  # for reproducibility
boston.rf <- randomForest(cmedv ~ ., data = boston)

# Partial dependence of lstat and rm on cmedv
grid.arrange(
  partial(boston.rf, pred.var = "lstat", plot = TRUE, rug = TRUE),
  partial(boston.rf, pred.var = "rm", plot = TRUE, rug = TRUE),
  partial(boston.rf, pred.var = c("lstat", "rm"), plot = TRUE, chull = TRUE),
  ncol = 3
)
```

![](README_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


#### Classification example 

In this example, we fit a support vector machine with a radial basis function kernel to the Pima Indians diabetes data. (See `?pima` for a brief explanation of the data.)

```r
# Load required packages
library(kernlab)  # for fitting SVMs

# Fit an SVM to the Pima Indians diabetes data
data (pima)  # load the boston housing data
pima.svm <- ksvm(diabetes ~ ., data = pima, type = "C-svc", kernel = "rbfdot",
                 C = 0.5, prob.model = TRUE)

# Partial dependence of glucose and age on diabetes test result (neg/pos). 
partial(pima.svm, pred.var = c("glucose", "age"), plot = TRUE, chull = TRUE,
        train = pima)
```

![](README_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


#### Interface with `caret`

Finally, we demonstrate the construction of PDPs from models fit using the `caret` package; `caret` is an extremetly useful package for classification and regression training that, essentially, has one function (`train`) for fitting all kinds of predictive models in R (e.g., `glmnet`, `svm`, `xgboost`, etc.). 

For illustration we use `caret`'s `train` function to tune an [XGBoost](https://github.com/dmlc/xgboost) model to the Pima Indians diabetes data using 5-fold cross-validation. We then use the final model to construct PDPs for `glucose` and `age`. Note, when training a model using `caret`'s `train` function, you can view tuning progress by setting `verboseIter = TRUE` in the call to `trainControl`.

```r
# Load required packages
library(caret)  # for model training/tuning

# Set up for 5-fold cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Grid of tuning parameter values
xgb.grid <- expand.grid(
  nrounds = 500,
  max_depth = 1:6,
  eta = c(0.001, 0.01, 0.1, 0.2, 0.3, 0.5),
  gamma = 0, 
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# Tune a support vector machine (SVM) using a radial basis function kerel to
# the Pima Indians diabetes data. This may take a few minutes!
set.seed(103)  # for reproducibility
pima.xgb <- train(diabetes ~ ., data = pima, method = "xgbTree",
                  prob.model = TRUE, na.action = na.omit, trControl = ctrl,
                  tuneGrid = xgb.grid)

# Partial dependence of glucose and age on diabetes test result (neg/pos)
grid.arrange(
  partial(pima.xgb, pred.var = "glucose", plot = TRUE, rug = TRUE),
  partial(pima.xgb, pred.var = "age", plot = TRUE, rug = TRUE),
  partial(pima.xgb, pred.var = "mass", plot = TRUE, rug = TRUE),
  ncol = 3 
)
```

![](README_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
