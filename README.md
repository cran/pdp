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

In this example, we fit a random forest to the Boston housing data. (See `?boston` for a brief explanation of the data.) Note, for any of the following examples, you can see a progress bar by simply specifying `.progress = "text"` in the call to `partial`. You may also reduce the computation time via the `grid.resolution` option in the call to `partial`.

```r
# Load required packages
library(ggmap)  # for plotting data on maps
library(pdp)  # for constructing PDPs
library(randomForest)  # for random forest algorithm

# Load the Boston housing data
data (boston)  # included with the pdp package

# Fit a random forest to the boston housing data
set.seed(101)  # for reproducibility
boston.rf <- randomForest(cmedv ~ ., data = boston)

# Using randomForest's partialPlot function
partialPlot(boston.rf, pred.data = boston, x.var = "lstat")
```

![](README_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Package pdp's partial function returns a data frame by default
head(partial(boston.rf, pred.var = "lstat"))  # returns a data frame
```

```
##    lstat        y
## 1 1.7300 31.08465
## 2 2.4548 31.08430
## 3 3.1796 31.04716
## 4 3.9044 30.73520
## 5 4.6292 29.09725
## 6 5.3540 27.06537
```

```r
# Set `plot = TRUE` to return a "trellis" object
partial(boston.rf, pred.var = "lstat", plot = TRUE, rug = TRUE)
```

![](README_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
# The partial function allows for multiple predictors
partial(boston.rf, pred.var = c("lstat", "rm"), plot = TRUE, chull = TRUE)
```

![](README_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

```r
# The plotPartial function offers more flexible plotting
pd <- partial(boston.rf, pred.var = c("lstat", "rm"))

# Default plot
plotPartial(pd)
```

![](README_files/figure-html/unnamed-chunk-3-4.png)<!-- -->

```r
# Customized plot
plotPartial(pd, levelplot = FALSE, zlab = "cmedv", drape = TRUE,
            colorkey = FALSE, screen = list(z = -20, x = -60))
```

![](README_files/figure-html/unnamed-chunk-3-5.png)<!-- -->

```r
# Look at partial dependence of median home value on location
pd.loc <- partial(boston.rf, pred.var = c("lon", "lat"), chull = TRUE)

# Overlay predictions on a map of Boston
# ll <- c(range(boston$lon), range(boston$lat))
# map <- get_map(location = ll[c(1, 3, 2, 4)], zoom = 11, maptype = "toner-lite")
# ggmap(map) + 
#   geom_point(aes(x = lon, y = lat), data = boston, alpha = 0.2) +
#   geom_tile(aes(x = lon, y = lat, z = y, fill = y), 
#             data = pd.loc, alpha = 0.3) +
#   geom_contour(color = "white", alpha = 0.5) +
#   scale_fill_distiller(palette = "Spectral", name = "Median\nvalue") +
#   coord_fixed(ratio = 1) +
#   labs(x = "Longitude", y = "Latitude")
```


#### Classification example 

In this example, we fit a random forest to the Pima Indians diabetes data. (See `?pima` for a brief explanation of the data.)

```r
# Fit a random forest to the Pima Indians diabetes data
data (pima)  # load the boston housing data
set.seed(102)  # for reproducibility
pima.rf <- randomForest(diabetes ~ ., data = pima, na.action = na.omit)

# Partial dependence of glucose on diabetes test result (neg/pos). This may take
# a couple minutes!
partial(pima.rf, pred.var = c("glucose", "age"), plot = TRUE, chull = TRUE)
```

![](README_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


#### Interface with `caret`

Finally, we demonstrate the construction of PDPs from models fit using the `caret` package; `caret` is an extremetly useful package for classification and regression training that, essentially, has one function (`train`) for fitting all kinds of predictive models in R (e.g., `glmnet`, `svm`, `xgboost`, etc.). 

For illustration we use `caret`'s `train` function to tune a support vector machine (SVM) with a radial basis function kernel to the Pima Indians diabetes data using 5-fold cross-validation. We then use the final model to construct a PDPs for `glucose` and `age`. Note, when training a model using `caret`'s `train` function, you can view tuning progress by setting `verboseIter = TRUE` in the call to `trainControl`.

```r
# Load required packages
library(caret)  # for model training/tuning

# Set up for 5-fold cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Tune a support vector machine (SVM) using a radial basis function kerel to
# the Pima Indians diabetes data
set.seed(103)  # for reproducibility
pima.svm <- train(diabetes ~ ., data = pima, method = "svmRadial",
                  prob.model = TRUE, na.action = na.omit, trControl = ctrl,
                  tuneLength = 10)

# Partial dependence of glucose on diabetes test result (neg/pos)
partial(pima.svm, pred.var = "glucose", plot = TRUE, rug = TRUE)
```

![](README_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# Partial dependence of age on diabetes test result (neg/pos)
partial(pima.svm, pred.var = "age", plot = TRUE, rug = TRUE)
```

![](README_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
# Partial dependence of glucose and age on diabetes test result (neg/pos). This
# may take a couple minutes!
partial(pima.svm, pred.var = c("glucose", "age"), plot = TRUE, chull = TRUE,
        train = pima)
```

![](README_files/figure-html/unnamed-chunk-5-3.png)<!-- -->
