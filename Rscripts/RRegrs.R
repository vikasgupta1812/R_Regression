# ======================================================================
# RRegrs - R Regression Models

# Best regression models for one dataset using R methods
# Developed as APIs for OpenTox integration for nano-toxicity
# NTUA and UM groups
# 
# contact: Cristian R Munteanu | BiGCaT - UM | muntisa@gmail.com
# ======================================================================

# Variable names
#-----------------------------------------------------------------------
# DataSet = dataset including header, Y and X features values
# Yname, Xnames = names of dependent and features
# NoX = no of features
# NoCases = no of cases
# YData = values of Y
# XData = values of the features
#-----------------------------------------------------------------------
# Note:
# - the script is importing separate file functions
# - the functions are reading datafiles in specific folders
# and it is writing the output as variables or datafiles
# - it generates several dataset files for each step
# - it generates PNG plots for the correlation matrix
# before and after the correlation removal
# - all the input and output files are placed into the same folder
#-----------------------------------------------------------------------

# FLAGS for each step
#-----------------------------------------------------------------------
fFilterErr=TRUE      # if filter for dataset errors       (2)
fScaling=TRUE        # if dataset scaling                 (3)
fRemCorr=TRUE        # if remove the correlated features  (4)

# Option Selections
#-----------------------------------------------------------------------
iScaling = 1 # 1 = standardization; 2 = normalization, 3 = other
iScalCol = 1 # 1: including dependent variable; 2: only features; any other: no scaling
#-----------------------------------------------------------------------
# (1) Loading dataset
#-----------------------------------------------------------------------
# Data file Parameters
PathDataSet  = "DataResults"        # dataset folder
DataFileName = "dataset.txt"        # ds original file name
ScaledFile   = "scaled.txt"         # scaled ds file name (in the same folder)
NoCorrFile   = "datasetNoCorr.txt"  # dataset after correction removal

# generate path + file name = original dataset
inFile1 <- file.path(PathDataSet, DataFileName)
# Read data from tab file
DataSet=read.table(inFile1,header=T)
# make available the variable names from the dataset
attach(DataSet)

#-----------------------------------------------------------------------
# (2) Filter dataset errors
#-----------------------------------------------------------------------
if (fFilterErr==TRUE) {
  print("-> [2] Filter data for errors ...")
  # PLACE the functione HERE!
}

#-----------------------------------------------------------------------
# (3) Scaling dataset: standardization, normalization, other
#-----------------------------------------------------------------------
if (fScaling==TRUE) {
  print("-> [2] Scaling original dataset ...")
  inFile3  <- inFile1
  outFile3 <- file.path(PathDataSet,ScaledFile) # the same folder as input
  
  # add s3.ScalingDataSet.R for scaling function
  source("s3.ScalingDataSet.R")
  
  # run fuction for scaling input dataset file
  ScalingDS(iScaling,iScalCol,inFile3,outFile3)
}

#-----------------------------------------------------------------------
# (4) Remove correlated features
#-----------------------------------------------------------------------
if (fRemCorr==TRUE) {    
  print("-> [3] Removing correlated features ...")
  
  # add s4.RemCorrFeats.R for correlation removal function
  source("s4.RemCorrFeats.R")
  
  OriginalFile="DataResults/scaled.txt"
  CorrectedFile="DataResults/NoCorrs.txt"
  cutoff = 0.65
  
  inFile4  <- outFile3
  outFile4 <- file.path(PathDataSet,NoCorrFile) # the same folder as input
  
  # run function to remove the correlations between the features
  RemCorrs(cutoff,inFile4,outFile4)
  
}

#-----------------------------------------------------------------------
# (5) Splits of data into: traing, cv, test
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# (6) Feature selection
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# (7) All Regressions
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# (8) Summary top models
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# (9) Best model statistics
#-----------------------------------------------------------------------