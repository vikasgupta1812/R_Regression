# ==========================================
# GLM stepwise - based on AIC (caret)
# Generalized Linear Model with Stepwise Feature Selection
# ==========================================
# contact: Cristian R Munteanu | BiGCaT - UM | muntisa@gmail.com
#
# inputs:
# - my.datf, my.datf.train,my.datf.test = full, train and test dataset frames
# - fDet = flag for detais (TRUE/FALSE)
# - outFile = output file
# output = statistics
# ------------------------------------------
GLMreg <- function(my.datf,my.datf.train,my.datf.test,fDet=FALSE,outFile="") {
  my.stats<- list() # create empty result

  # specify CV parameters
  ctrl<- trainControl(method = 'repeatedcv', number = 10,repeats = 10,
                      summaryFunction = defaultSummary)

  # Training the model using only training set
  set.seed(2)
  attach(my.datf.train)
  lm.fit<- train(net.c~.,data=my.datf.train,
                 method = 'glmStepAIC', tuneLength = 10, trControl = ctrl,
                 metric = 'RMSE')
  # RESULTS
  #--------------
  # get statistics
  RMSE = lm.fit$results[,2]
  Rsquared = lm.fit$results[,3]
  RMSE_SD = lm.fit$results[,4]
  RsquaredSD = lm.fit$results[,5]
  
  # RMSE & R^2, for train/ test respectively
  lm.train.res<- getTrainPerf(lm.fit)
  lm.test.res <- postResample(predict(lm.fit,my.datf.test),my.datf.test[,1])
  
  if (fDet==TRUE) {
    # write RESULTS
    sink(outFile)
    summary(my.datf)
    lm.fit
    predictors(lm.fit)
    lm.train.res
    lm.test.res
    
    sink()
    # file.show(outFile)
  }
  my.stats = list("RMSE"= RMSE,"Rsquared" = Rsquared,"RMSE_SD" = RMSE_SD,"RsquaredSD" = RsquaredSD)
  
  return(my.stats)  # return statistics
}
