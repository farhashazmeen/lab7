## ---- eval=FALSE---------------------------------------------------------
#    x<- ((x[,-1]-mean(x[,-1])) / sd(x[,-1]))

## ---- eval=FALSE---------------------------------------------------------
#  QR_ridge<- qr(x_norm)
#  Q<-qr.Q (QR_ridge)
#  R<-qr.R(QR_ridge)
#  RR<- ((R %*% t(Q) %*% Q) + lambda *(t(Q) %*% Q %*% solve(t(R))))
#  

## ----eval=FALSE----------------------------------------------------------
#  beta_ridge_QR<<- backsolve(RR, crossprod(Q,y))

## ----eval=FALSE----------------------------------------------------------
#  ybar_ridge_QR<<- (x_norm %*% beta_ridge_QR)
#  

## ---- echo=TRUE----------------------------------------------------------

print_QR= function(){
                             
                             
                             cat("\n","Call:","\n",
                                 paste("ridgereg(", "formula = ", formula[2]," ", formula[1], " ", formula[3],
                                       ", ", "data = ", parsedata, ", lambda = 0)",sep = "", collapse = "\n" ),
                                 "\n","Coefficients:","\n",
                                 paste(names,
                                       sep = "  ", collapse ="  " ),"\n",
                                 format(round(beta_ridge_QR,2), justify = "centre",width = 10))
                             
                           }


## ------------------------------------------------------------------------
  predict_QR =function(){
                             cat("\n \n Predicted values or fitted values using QR decomposition:","\n\n")
                             return(as.vector(round(ybar_ridge_QR, 2)))
                           }

## ---- echo=TRUE----------------------------------------------------------
 coef_QR = function(){
                              cat("\n \nRegressions coefficients using QR decomposition:","\n\n")
                              ridge_coef_QR <-as.vector(round(beta_ridge_QR,2))
                              names(ridge_coef_QR)<-names
                             return (ridge_coef_QR)
                           }

## ---- eval=FALSE---------------------------------------------------------
#  test_that("print_QR() method works", {
#    ridgereg_mod <- ridgereg$new(Petal.Length~Species, data=iris,lambda=0)
#  
#    expect_output(ridgereg_mod$print_QR(),"ridgereg\\(formula = Petal\\.Length ~ Species, data = iris\\, lambda = 0)")
#    expect_output(ridgereg_mod$print_QR()," Speciesversicolor  Speciesvirginica")
#  })
#  

## ------------------------------------------------------------------------
library(lab7)
library(nycflights13)
library(ggplot2)
library(dplyr)
 plot_delay <- ggplot(df,aes(x=df$Lat, y =df$Lon))+
  geom_point(aes(color=df$Delay),size=4)+
  labs(title= "Visualising delay ", x= "Latitude",y="Longitude")
 
 visualize_airport_delays()

## ---- eval=FALSE---------------------------------------------------------
#  devtools::install_github("tondralok/GoogleAPI/Geocode")

