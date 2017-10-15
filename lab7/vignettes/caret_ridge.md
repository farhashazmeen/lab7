---
title: <center>Ridgereg_caret Vignette </center>
subtitle: <center>farch587@student.liu.se, syesh076@student.liu.se</center>
author: <center>Farhana Chowdhury Tondra, Syeda Farha Shazmeen </center>
date: "2017-10-15"
output: rmarkdown::html_vignette
vignette: >
 %\VignetteIndexEntry{ridgereg}
  %\VignetteEngine{knitr::knitr}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}{inputenc}
  
---

```r
library(mlbench)
library(caret)
library(leaps)
library(lab7)
data("BostonHousing")
```
The caret package is used for classification and regression.

## 1. Divide the BostonHousing data (or your own API data) into a test and training dataset using the caret package.

Dividing the BostonHousing data into 70% training and 30% test data 



```r
library(mlbench)
library(caret)
```

```
## Loading required package: lattice
```

```r
data("BostonHousing")
names(BostonHousing)
```

```
##  [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"    
##  [8] "dis"     "rad"     "tax"     "ptratio" "b"       "lstat"   "medv"
```

```r
train_data <- caret::createDataPartition(BostonHousing$age, p = .7,
                                         list = FALSE,
                                         times= 1)
Trainingdata <- BostonHousing[train_data, ]
Testdata <- BostonHousing[-train_data, ]


nrow(Trainingdata)
```

```
## [1] 356
```

```r
nrow(Testdata)
```

```
## [1] 150
```



## 2. Fit a linear regression model and a linear regression model with forward selection of covariates on the training dataset.

The first two arguments of the train function are **predictor** and **outcome** data objects.Traincontrol object in train allows us to specify the resampling method.The resampling method used here is **cv-cross validation**. <br/>
For our analysis we have taken crim variable . R-squared  and RMSE(Root Mean Square Error)is used measure model performance. RSME values is low with  5.706505.

Fit a linear regression model: 


```r
ridge <- caret::train(crim~.,
                      data = Trainingdata,
                      method='lm',
                      trControl = trainControl(method = "cv")
)

print(ridge)
```

```
## Linear Regression 
## 
## 356 samples
##  13 predictor
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 320, 320, 320, 320, 321, 321, ... 
## Resampling results:
## 
##   RMSE      Rsquared   MAE     
##   4.000754  0.6719292  2.175977
## 
## Tuning parameter 'intercept' was held constant at a value of TRUE
```


Fitting a linear model with method = leapForward on the training dataset :


```r
lflmGrid <- expand.grid(nvmax=1:(ncol(Trainingdata)-1))
ridge <- caret::train(crim~.,
                      data = Trainingdata,
                      method='leapForward',
                      tuneGrid = lflmGrid
)
print(ridge)
```

```
## Linear Regression with Forward Selection 
## 
## 356 samples
##  13 predictor
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## Summary of sample sizes: 356, 356, 356, 356, 356, 356, ... 
## Resampling results across tuning parameters:
## 
##   nvmax  RMSE      Rsquared   MAE     
##    1     4.859393  0.5255376  2.121173
##    2     4.851417  0.5294181  2.131405
##    3     4.807324  0.5393622  2.135727
##    4     4.812957  0.5382858  2.163546
##    5     4.809204  0.5396366  2.195052
##    6     4.808751  0.5396812  2.217635
##    7     4.799582  0.5419616  2.233915
##    8     4.803071  0.5416059  2.252695
##    9     4.804957  0.5417214  2.268781
##   10     4.807554  0.5413670  2.284720
##   11     4.805933  0.5418136  2.287889
##   12     4.801207  0.5427537  2.284891
##   13     4.802113  0.5426356  2.286655
## 
## RMSE was used to select the optimal model using  the smallest value.
## The final value used for the model was nvmax = 7.
```

## 3. Evaluate the performance of this model on the training dataset.

Since **the RMSE & MAE is low on training of lm model compared to leapForward lm where model has good perfomance with nvmax(number of predictors) we can conclude that LM is better than leapforward LM.**

##4. Fit a ridge regression model using your ridgereg() function to the training dataset for different values of lambda.



```r
ridge <- list(type="Regression", 
              library="lab7",
              loop=NULL,
              prob=NULL)

ridge$parameters <- data.frame(parameter="lambda",
                               class="numeric",
                               label="lambda")


 ridge$grid <- function (x, y, len = NULL, search = "grid"){
    data.frame(lambda = lambda)
  } 
  
  ridge$fit <- function (x, y, wts, param, lev, last, classProbs, ...) {
    dat <- if (is.data.frame(x)) 
      x
    else as.data.frame(x)
    dat$.outcome <- y
    out <- ridgereg$new(.outcome ~ ., data=dat ,lambda = param$lambda, normalize=normalize, ...)
    
    out
  }
  
  ridge$predict <- function (modelFit, newdata, submodels = NULL) {
    if (!is.data.frame(newdata)) 
      newdata <- as.data.frame(newdata)
    newdata <- scale(newdata)
    modelFit$predict(newdata)
  }
```



###5. Find the best hyperparameter value for lambda using 10-fold cross-validation on the training set.


```
## Loading required package: lars
```

```
## Loaded lars 1.2
```

```
##           zn        indus        chas1          nox           rm 
##  0.409346263  0.000000000 -0.054251589  0.000000000 -0.306218313 
##          age          dis          rad          tax      ptratio 
##  0.009578360 -0.549067338  3.795449137  0.000000000 -0.009903944 
##            b        lstat         medv 
## -0.542092374  0.713746389 -0.310055282
```

```
## Ridge Regression 
## 
## 356 samples
##  13 predictor
## 
## Pre-processing: centered (13), scaled (13) 
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 320, 320, 321, 321, 321, 320, ... 
## Resampling results across tuning parameters:
## 
##   lambda  RMSE      Rsquared   MAE     
##   0.00    3.838942  0.7108110  2.227192
##   0.01    3.830001  0.7128372  2.212070
##   0.02    3.827489  0.7139553  2.207864
##   0.03    3.828875  0.7145387  2.209183
##   0.04    3.832806  0.7147865  2.214187
## 
## RMSE was used to select the optimal model using  the smallest value.
## The final value used for the model was lambda = 0.02.
```

So, **the best hyperparameter value for lambda is 0.03**



## 6. Evaluate the performance of all three models on the test dataset.

After evaluating datasets, it can be stated that **ridge regression model is better than lm and lm leapforward regression as it gives lower RMSE.**

