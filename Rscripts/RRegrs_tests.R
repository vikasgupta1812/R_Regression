# ======================================================================
# RRegrs - R Regression Models

# Best regression models for one dataset using R methods
# Developed as tool for nano-toxicity QSAR models
# NTUA and UM groups
# contact: Cristian R Munteanu | BiGCaT - UM | muntisa@gmail.com
# ======================================================================
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
fDet = TRUE          # flag to calculate and print details for all the functions
fFilters=TRUE        # flag to apply filters                          (2)
fScaling=TRUE        # flag for dataset Scaling                       (3)
fRemNear0Var=TRUE    # flag for Removal of near zero variance columns (4)
fRemCorr=TRUE        # flag for Removal of correlated columns         (5)
fFeatureSel=TRUE     # flag for selection before the regression       (7)
cutoff=0.7           # cut off for corralations
# -----------------------------------------------------------------------
iScaling = 1 # 1 = standardization; 2 = normalization, 3 = other; any other: no scaling
iScalCol = 1 # 1: including dependent variable in scaling; 2: only all features; etc.
# -----------------------------------------------------------------------
trainFrac  = 3/4 # the fraction of training set from the entire dataset
#                # 1 - trainFrac = the rest of dataset, the test set
# -----------------------------------------------------------------------
# (1) Load dataset and parameters
# -----------------------------------------------------------------------
# (1.1) PARAMETERS
PathDataSet    = "DataResults"            # dataset folder
DataFileName   = "ds.csv"                 # input step 1 = ds original file name
No0NearVarFile = "ds3.No0Var.csv"         # output step 3 = ds without zero near vars
ScaledFile     = "ds4.scaled.csv"         # output step 4 = scaled ds file name (in the same folder)
NoCorrFile     = "ds5.scaled.NoCorrs.csv" # output step 5 = dataset after correction removal
ResFile        = "RRegresRezults.txt"     # the common output file! 

glmFile        = "glm.res.txt"

# Generate path + file name = original dataset
inFile <- file.path(PathDataSet, DataFileName)
# Set the file with results (append data using: sink(outRegrFile, append = TRUE) !!!)
outRegrFile <- file.path(PathDataSet,ResFile) # the same folder as the input 

# (1.2) Load the ORIGINAL DATASET
# -----------------------------
# (it can contain errors, correlations, near zero variance columns)
ds.dat0 <- read.csv(inFile,header=T)                              # original dataset frame

# resolving the text to number errors for future calculations
ds.indx<- colnames(ds.dat0)[2:dim(ds.dat0)[2]]                    # FEATURE names (no dependent variable)
ds.dat1<- ds.dat0[1:dim(ds.dat0)[1],2:dim(ds.dat0)[2]]            # dataset as columns
ds.dat1<- apply(ds.dat1,1,function(x)as.numeric(as.character(x))) # dataset as row vectors to be used with caret!!!

# dependent variable
net.c<- ds.dat0[,1]
net.c<- as.numeric(as.character(net.c)) # values
# full ds frame with training and test
ds<- as.data.frame(cbind(net.c,t(ds.dat1)))

# -----------------------------------------------------------------------
# (2) FILTERS
# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# (3) Remove near zero variance columns
# -----------------------------------------------------------------------
if (fRemNear0Var==TRUE) {
  print("-> [3] Removal of near zero variance columns ...")
  outFile <- file.path(PathDataSet,No0NearVarFile) # the same folder as input  
  
  # get the ds without near zero cols 
  source("s3.RemNearZeroVar.R")                    # add function
  ds <- RemNear0VarCols(ds,fDet,outFile)           # inputs: ds, flag for details, output file
}

# -----------------------------------------------------------------------
# (4) Scaling dataset: standardization, normalization, other
# -----------------------------------------------------------------------
if (fScaling==TRUE) {
  print("-> [4] Scaling original dataset ...")
  outFile <- file.path(PathDataSet,ScaledFile)       # the same folder as input
  
  # run fuction for scaling input dataset file
  source("s4.ScalingDataSet.R")                      # add function
  ds <- ScalingDS(ds,iScaling,iScalCol,fDet,outFile) # inputs: ds, type of scaling, flag for details, starting column, output file
}

# -----------------------------------------------------------------------
# (5) Remove correlated features
# -----------------------------------------------------------------------
if (fRemCorr==TRUE) {    
  print("-> [5] Removing correlated features ...") 
  outFile <- file.path(PathDataSet,NoCorrFile)    # the same folder as the input
  
  # run function to remove the correlations between the features
  source("s5.RemCorrFeats.R")                     # add function
  ds <- RemCorrs(ds,fDet,cutoff,outFile)
}

# -----------------------------------------------------------------------
# (6) Dataset split: Training and Test sets
# -----------------------------------------------------------------------
print("-> [6] Splitting dataset in Training and Test sets ...")

source("s6.DsSplit.R")                   # add function
dsList <- DsSplit(ds,trainFrac,fDet,PathDataSet)   # return a list with 2 datasets = dsList$train, dsList$test
# get train and test from the resulted list
ds.train <- dsList$train
ds.test <- dsList$test

# -----------------------------------------------------------------------
# (7) Feature selection
# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# (8) Regressions
# -----------------------------------------------------------------------
#
print("-> [8] Run Regressions ...")

# (8.1) glm stepwise - based on AIC
#       Generalized Linear Model with Stepwise Feature Selection
# ----------------------------------------------------------------------
print("-> [8.1] glm stepwise - based on AIC ...")
outFile <- file.path(PathDataSet,glmFile)             # the same folder as the input

source("s8.1.GLM.R")                                  # add function
my.stats<- GLMreg(ds,ds.train,ds.test,fDet,outFile) 

# test the output from GLM
str(my.stats)