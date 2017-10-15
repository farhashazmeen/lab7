---
title: <center>"A package implementation of ridgereg(), dplyr and the caret package" </center>
subtitle: <center>farch587@student.liu.se, syesh076@student.liu.se</center>
author: "Farhana chowdhury Tondra,Syeda Farha Shazmeen"
date: "2017-10-15"
output: rmarkdown::html_vignette
vignette: >
  
%\VignetteIndexEntry{A package implementation of ridgereg(), dplyr and the caret package}
%\VignetteEngine{knitr::knitr}
%\VignetteEngine{knitr::rmarkdown}
%\VignetteEncoding{UTF-8}{inputenc}
---
  
  **RCourseLab7** is entitled for understanding of machine learning in R. The package consists of Ridge regression and plotting a large dataset using ggplot and dplyr and also a evaluating three models based on a large dataset.  
  
  As we have created packages in earlier labs , similar to that there is **/R** folder, where the ridge regression model and visualize delay files are kept as .R file. **/test** folder contains implemented testcases for the functions. All .rd files are situdated inside **/man** folder and .Rmd file containing the vignette documentation is inside the **/vignettes** folder.

  This package contains  methods for ridge regression model. We are using QR decomposition to get the basic functionality in the R package. The function is written using RC class  where fields and then methods are initialized. The methods which are implemented for calculating linear regression model is defined later.This package contains few things :
  
- A Method RC class is used here named as ridgereg(). 
-  
-

## ridgereg() Class:

The function is called **ridgereg()** and have the three arguments formula, data and lambda. The function returns an object ofclass ridgereg. First the data is normalized as below

$$ X_{norm} = \frac{x-\bar{x}}{\sqrt{V(x)}} $$


```r
  x<- ((x[,-1]-mean(x[,-1])) / sd(x[,-1]))
```


Using QR decomposition part for ridge regression,


```r
QR_ridge<- qr(x_norm)
Q<-qr.Q (QR_ridge)
R<-qr.R(QR_ridge)
RR<- ((R %*% t(Q) %*% Q) + lambda *(t(Q) %*% Q %*% solve(t(R))))
```


$$ \beta{(Q^TQR + lambda * Q^TQ{{R^T}^{-1}})} = Q^TY$$
where, $$(Q^TQR + lambda * Q^TQ{{R^T}^{-1}})=R $$

which implies QR decomposition of 

$$ R\beta = Q^TY$$

### Regressions coefficients:  
Regressions coefficient using  QR,


```r
beta_ridge_QR<<- backsolve(RR, crossprod(Q,y))
```



### The fitted values:
Fitted values using QR
  

```r
ybar_ridge_QR<<- (x_norm %*% beta_ridge_QR)
```



## Methods:
  
###ridgereg_mod$print_QR()

  It prints out the coefficients and coefficient names.
  

```r
print_QR= function(){
                             
                             
                             cat("\n","Call:","\n",
                                 paste("ridgereg(", "formula = ", formula[2]," ", formula[1], " ", formula[3],
                                       ", ", "data = ", parsedata, ", lambda = 0)",sep = "", collapse = "\n" ),
                                 "\n","Coefficients:","\n",
                                 paste(names,
                                       sep = "  ", collapse ="  " ),"\n",
                                 format(round(beta_ridge_QR,2), justify = "centre",width = 10))
                             
                           }
```



###ridgereg_mod$predict_QR()

predict_QR() returns the predicted values of ybar.


```r
  predict_QR =function(){
                             cat("\n \n Predicted values or fitted values using QR decomposition:","\n\n")
                             return(as.vector(round(ybar_ridge_QR, 2)))
                           }
```


###ridgereg_mod$coef_QR()
coef_QR() returns the coefficients as a named vector


```r
 coef_QR = function(){
                              cat("\n \nRegressions coefficients using QR decomposition:","\n\n")
                              ridge_coef_QR <-as.vector(round(beta_ridge_QR,2))
                              names(ridge_coef_QR)<-names
                             return (ridge_coef_QR)
                           }
```


##Testsuit implementation:
A testsuit has been added in the /tests folder named as test_ridgereg_ref_class.R which tests the method and class functionality of ridgereg function.

Below is one of the testcase:


```r
test_that("print_QR() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Species, data=iris,lambda=0)
  
  expect_output(ridgereg_mod$print_QR(),"ridgereg\\(formula = Petal\\.Length ~ Species, data = iris\\, lambda = 0)")
  expect_output(ridgereg_mod$print_QR()," Speciesversicolor  Speciesvirginica")
})
```


## Handling large datasets with dplyr

visualize_airport_delays() is a function which is creating a plot using ggplot by manipulating a large dataset of nycflights13 package using dplyr verbs.


```r
library(lab7)
library(nycflights13)
library(ggplot2)
library(dplyr)
 plot_delay <- ggplot(df,aes(x=df$Lat, y =df$Lon))+
  geom_point(aes(color=df$Delay),size=4)+
  labs(title= "Visualising delay ", x= "Latitude",y="Longitude")
 
 visualize_airport_delays()
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)


This graph implies that the x axis is latitude and y is longitude of the airports of newyork. And based on delay the graph plots dots. the darker color states the minimal mean delay and the ligher color of dots denotes the maximum mean delay of those airports. 


##The caret implementation:

The caret part is added on seperate file.


##Roxygen Description files:
###Package documentation : **Package.Rd**
This file includes the detail description of how package works , who worked for it and so on.

###Function documentation : **ridgereg-class.Rd**
This file contains the description of class file , fields, method, examples, references and return statements.

###Vignettes documentation : **vignettes.Rmd**
This is about how the whole package is made of and how to read it and implement it.

##Installation:
This package can be installed using below command    


```r
devtools::install_github("tondralok/GoogleAPI/Geocode")
```


##References:

The study material for this lab given below.

1. http://genomicsclass.github.io/book/pages/qr_and_regression.html
2. https://math.stackexchange.com/questions/299481/qr-factorization-for-ridge-regression


 
