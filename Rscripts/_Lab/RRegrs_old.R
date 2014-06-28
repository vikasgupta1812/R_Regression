# ======================================================================
# RRegrs - R Regression Models

# Best regression models for one dataset using R methods
# Developed as tool for nano-toxicity QSAR models
# NTUA and UM groups
# 
# contact: Cristian R Munteanu | BiGCaT - UM | muntisa@gmail.com
# ======================================================================
#
# Main input file: CSV
# ----------------------------------------------------------------------
# Variable names
# -----------------------------------------------------------------------
# DataSet = dataset including header, Y and X features values
# Yname, Xnames = names of dependent and features
# NoX = no of features
# NoCases = no of cases
# YData = values of Y
# XData = values of the features
# -----------------------------------------------------------------------
# Note:
# - Input file = CSV format and extension:
#    - there is only one input file with the original dataset:
#      Dependent Variable, Feature 1, ..., Feature n
#    - the names of the variables are located in the first row
# - the main script is importing separate file functions
# - the functions are reading datafiles in specific folders
# and it is writing the output as variables or datafiles
# - it generates several dataset files for each step
# - it generates PNG plots for the correlation matrix
# before and after the correlation removal
# - all the input and output files are placed into the same folder
# -----------------------------------------------------------------------

# Option to run any step
# -----------------------------------------------------------------------
fdet = TRUE          # flat to calculate and print details for all the functions
fRemNear0Var=TRUE    # flag for Removal of near zero variance columns (2)
fScaling=TRUE        # flag for dataset Scaling                       (3)
fRemCorr=TRUE        # flag for Removal of correlated columns         (4)
cutoff=0.7           # cut off for corralations
# -----------------------------------------------------------------------
iScaling = 1 # 1 = standardization; 2 = normalization, 3 = other
iScalCol = 1 # 1: including dependent variable in scaling; 2: only features; any other: no scaling
# -----------------------------------------------------------------------
# trainFrac  = 3/4 # the fraction of training set from the entire dataset
#                # 1 - trainFrac = the rest of dataset, the test set
# -----------------------------------------------------------------------
# (1) Load dataset and parameters
# -----------------------------------------------------------------------
# Data file Parameters
PathDataSet    = "DataResults"            # dataset folder
DataFileName   = "ds.csv"                 # input step 1 = ds original file name
No0NearVarFile = "ds2.No0Var.csv"         # output step 2 = ds without zero near vars
ScaledFile     = "ds3.scaled.csv"         # output step 3 = scaled ds file name (in the same folder)
NoCorrFile     = "ds4.scaled.NoCorrs.csv" # output step 4 = dataset after correction removal
ResFile        = "RRegresRezults.txt"     # the common output file! 

# Generate path + file name = original dataset
inFile <- file.path(PathDataSet, DataFileName)

# Set the file with results (append data using: sink(outRegrFile, append = TRUE) !!!)
outRegrFile <- file.path(PathDataSet,ResFile) # the same folder as the input 

# Load the ORIGINAL DATASET
# -----------------------------
# (it can contain errors, correlations, near zero variance columns)
ds <- read.csv(inFile,header=T)

# -----------------------------------------------------------------------
# (2) Remove near zero variance columns
# -----------------------------------------------------------------------
if (fRemNear0Var==TRUE) {
  print("-> [2] Removal of near zero variance columns ...")
  outFile <- file.path(PathDataSet,No0NearVarFile) # the same folder as input
  
  # get the ds without near zero cols
  ds = RemNear0VarCols(ds,det,outFile)
}

# -----------------------------------------------------------------------
# (3) Scaling dataset: standardization, normalization, other
# -----------------------------------------------------------------------
if (fScaling==TRUE) {
  print("-> [3] Scaling original dataset ...")
  inFile  <- outFile
  outFile <- file.path(PathDataSet,ScaledFile) # the same folder as input
  
  # add s3.ScalingDataSet.R for scaling function
  source("s3.ScalingDataSet.R")
  
  # run fuction for scaling input dataset file
  ScalingDS(iScaling,iScalCol,inFile,outFile)
}

# -----------------------------------------------------------------------
# (4) Remove correlated features
# -----------------------------------------------------------------------
if (fRemCorr==TRUE) {    
  print("-> [4] Removing correlated features ...")
  
  # add s4.RemCorrFeats.R for correlation removal function
  source("s4.RemCorrFeats.R")
  
  inFile  <- outFile
  outFile <- file.path(PathDataSet,NoCorrFile) # the same folder as the input
  
  # run function to remove the correlations between the features
  RemCorrs(cutoff,inFile,outFile)
}

# -----------------------------------------------------------------------
# (5) Dataset split: Training and Test sets
# -----------------------------------------------------------------------

print("-> [5] Splitting dataset in Training and Test sets ...")

# Load the CORRECTED datest to create a regression model
# this dataset will use the last output file as the dataset that will be
# separated into training and test sets and it will be used for regressions

ds.dat0<- read.csv(outFile,header=T)                              # initial dataset from the file (corrected or not)
ds.indx<- colnames(ds.dat0)[2:dim(ds.dat0)[2]]                    # FEATURE names (no dependent variable)
ds.dat1<- ds.dat0[1:dim(ds.dat0)[1],2:dim(ds.dat0)[2]]            # dataset as columns
ds.dat1<- apply(ds.dat1,1,function(x)as.numeric(as.character(x))) # dataset as row vectors to be used with caret!!!

# dependent variable
net.c<- ds.dat0[,1]
net.c<- as.numeric(as.character(net.c)) # values

# create TRAIN and TEST sets to build a model

set.seed(1)
# creating dataset for regression models
my.datf<- as.data.frame(cbind(net.c,t(ds.dat1)))
inTrain <- createDataPartition(1:dim(my.datf)[1], p = 3/4, list = FALSE)
my.datf.train<- my.datf[inTrain,]            # TRAIN set         
my.datf.test <- my.datf[-inTrain,]           # TEST set 

# write the TRAIN and TEST set files
# the index of each row will in the dataset will not be saved (row.names=F)
outTrain <- file.path(PathDataSet,"ds5.Train.csv") # the same folder as the input
write.csv(my.datf.train,outTrain,row.names=FALSE)
outTest <- file.path(PathDataSet,"ds5.Test.csv") # the same folder as the input
write.csv(my.datf.test,outTest,row.names=FALSE) 

# -----------------------------------------------------------------------
# (6) Feature selection
# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# (7) Regressions
# -----------------------------------------------------------------------
#
print("-> [7] Run Regressions ...")

# (7.1) glm stepwise - based on AIC
#       Generalized Linear Model with Stepwise Feature Selection
# ----------------------------------------------------------------------
print("-> [7.1] glm stepwise - based on AIC ...")

# specify CV parameters
ctrl<- trainControl(method = 'repeatedcv', number = 10,repeats = 10,
                    summaryFunction = defaultSummary)

# Training the model using only training set
set.seed(2)
lm.fit<- train(net.c~.,data=my.datf.train,
               method = 'glmStepAIC', tuneLength = 10, trControl = ctrl,
               metric = 'RMSE')
# RESULTS
#--------------
# get statistics
#RMSE = lm.fit$results[,2]
#Rsquared = lm.fit$results[,3]
#RMSE_SD = lm.fit$results[,4]
#RsquaredSD = lm.fit$results[,5]

# RMSE & R^2, for train/ test respectively
lm.train.res<- getTrainPerf(lm.fit)
lm.test.res <- postResample(predict(lm.fit,my.datf.test),my.datf.test[,1])

# write RESULTS
sink(outRegrFile, append = TRUE)

summary(my.datf)
lm.fit
predictors(lm.fit)
lm.train.res
lm.test.res

sink()
file.show(outRegrFile)

# TO ADD OTHER STATISTICS !!!!!!!!!!!!!!!!


# -----------------------------------------------------------------------
# (8) Top models
# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# (9) Best model statistics
# -----------------------------------------------------------------------