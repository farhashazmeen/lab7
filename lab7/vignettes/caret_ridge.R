## ----eval=FALSE----------------------------------------------------------
#  library(mlbench)
#  library(caret)
#  library(leaps)
#  library(lab7)
#  data("BostonHousing")

## ------------------------------------------------------------------------
library(mlbench)
library(caret)
data("BostonHousing")
names(BostonHousing)
train_data <- caret::createDataPartition(BostonHousing$age, p = .7,
                                         list = FALSE,
                                         times= 1)
Trainingdata <- BostonHousing[train_data, ]
Testdata <- BostonHousing[-train_data, ]


nrow(Trainingdata)
nrow(Testdata)

## ------------------------------------------------------------------------
ridge <- caret::train(crim~.,
                      data = Trainingdata,
                      method='lm',
                      trControl = trainControl(method = "cv")
)

print(ridge)

## ------------------------------------------------------------------------
lflmGrid <- expand.grid(nvmax=1:(ncol(Trainingdata)-1))
ridge <- caret::train(crim~.,
                      data = Trainingdata,
                      method='leapForward',
                      tuneGrid = lflmGrid
)
print(ridge)

## ------------------------------------------------------------------------
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


## ----echo=FALSE----------------------------------------------------------
fitControl <- caret::trainControl(method = "cv",
                                    number = 10)
 lambdaGrid <- expand.grid(lambda = c(0,.01,.02,.03,.04))
  ridge <- caret::train(crim~.,
                        data = Trainingdata,
                        method='ridge',
                        trControl = fitControl,
                        tuneGrid = lambdaGrid,
                        preProcess=c('center', 'scale')
  )
  predict(ridge$finalModel, type='coef', mode='norm')$coefficients[13,]
  ridge.pred <- predict(ridge, Testdata)
  avgErrror<-2*sqrt(mean(ridge.pred - Testdata$crim)^2)
  print(ridge)


