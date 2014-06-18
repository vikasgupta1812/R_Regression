# ======================================================================
# RRegrs - R Regression Models

# Best regression models for one dataset using R methods
# Developed as tool for nano-toxicity QSAR models
# NTUA and UM groups
# 
# contact: Cristian R Munteanu | BiGCaT - UM | muntisa@gmail.com
# ======================================================================
#
# Basic concept: each module is reading input data from files and
# it writing outputs in files for all details 
# 
# (only CSV files)
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

# Option Selections
# -----------------------------------------------------------------------
fRemNear0Var=TRUE    # flag for Removal of near zero variance columns (2)
fScaling=TRUE        # flag for dataset Scaling                       (3)
fRemCorr=TRUE        # flag for Removal of correlated columns         (4)
cutoff=0.7           # cut off for corralations
# -----------------------------------------------------------------------
iScaling = 1 # 1 = standardization; 2 = normalization, 3 = other
iScalCol = 1 # 1: including dependent variable in scaling; 2: only features; any other: no scaling
# -----------------------------------------------------------------------
trainFrac  = 3/4 # the fraction of training set from the entire dataset
#                # 1 - trainFrac = the rest of dataset, the test set
#
# -----------------------------------------------------------------------
# (1) Load dataset and parameters
# -----------------------------------------------------------------------
# Data file Parameters
PathDataSet    = "DataResults"            # dataset folder
DataFileName   = "ds.csv"                 # input step 1 = ds original file name
No0NearVarFile = "ds2.No0Var.csv"          # output step 2 = ds without zero near vars
ScaledFile     = "ds3.scaled.csv"          # output step 3 = scaled ds file name (in the same folder)
NoCorrFile     = "ds4.scaled.NoCorrs.csv"  # output step 4 = dataset after correction removal

# Generate path + file name = original dataset
inFile <- file.path(PathDataSet, DataFileName)

# Load the original dataset
# (it can contain errors, correlations, near zero variance columns)
ds <- read.csv(inFile,header=T)

# -----------------------------------------------------------------------
# (2) Remove near zero variance columns
# -----------------------------------------------------------------------
if (fRemNear0Var==TRUE) {
  print("-> [2] Removal of near zero variance columns ...")
  
  library(caret)
  
  ds.var <- nearZeroVar(ds)
  ds.Rem0NearVar <- ds[,-(ds.var)]
  outFile <- file.path(PathDataSet,No0NearVarFile) # the same folder as input
  # write the corrected ds file
  write.csv(ds.Rem0NearVar, outFile,row.names=F, quote=F)  
}

# -----------------------------------------------------------------------
# (3) Scaling dataset: standardization, normalization, other
# -----------------------------------------------------------------------
if (fScaling==TRUE) {
  print("-> [2] Scaling original dataset ...")
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
  print("-> [3] Removing correlated features ...")
  
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
set.seed(1)

# Load the CORRECTED datest to create a regression model
# this dataset will use the last output file as the dataset that will be
# separated into training and test sets and it will be used for regressions
ds.m <- read.csv(outFile,header=T)

# create TRAIN and TEST sets to build a model
inTrain <- createDataPartition(1:dim(ds.m)[1], p = trainFrac, list = FALSE)
ds.m.train <- ds.m[inTrain,]   # TRAIN set
ds.m.test  <- ds.m[-inTrain,]  # TEST set

# write the TRAIN and TEST set files
# the index of each row will in the dataset will not be saved (row.names=F)
outTrain <- file.path(PathDataSet,"ds5.Train.csv") # the same folder as the input
write.csv(ds.m.train,outTrain,row.names=FALSE)
outTest <- file.path(PathDataSet,"ds5.Test.csv") # the same folder as the input
write.csv(ds.m.test,outTest,row.names=FALSE) 

# -----------------------------------------------------------------------
# (6) Feature selection
# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# (7) Regressions
# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# (8) Top models
# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# (9) Best model statistics
# -----------------------------------------------------------------------