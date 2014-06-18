# ==========================================
# Scaling dataset
# ==========================================
# contact: Cristian R Munteanu | BiGCaT - UM | muntisa@gmail.com
#
# USE:
# ScalingDS([ScalingType],[column to start scaling],[Original File name],[Scaled fime name])
#
# s = { 1,2,3 } - type of scaling:
# 1 = standardization, 2 = normalization, 3 = other

# c = the number of column into the dataset to start scaling
# if c = 1: included the dependent variable
# if c = 2: only the features will be scaled

# inFileName  = file name      (it could include the path)
# outFileName = new file name  (it could include the path)
# ------------------------------------------

ScalingDS <- function(s,c,inFileName, outFileName) {
  
  # Read the CSV file
  DataSet <- read.csv(inFileName,header=TRUE)
  
  # DEFAULT scaled dataset = original
  # if other s diffent of 1,2,3 is used, no scaling!
  DataSet.scaled <- DataSet
  
  # if STADARDIZATION
  if (s==1) {
    # Scale all the features (from column c bacause column 1 is the predictor output)
    DataSet.scaled <- scale(DataSet[c:ncol(DataSet)],center=TRUE,scale=TRUE)  
  }
  # if NORMALIZATION
  if (s==2) {
    # Scale all the features (from feature 2 bacause feature 1 is the predictor output)
    # TO ADD THE CODE ! 
  }
  # if other scaling
  if (s==3) {
    # Scale all the features (from feature 2 bacause feature 1 is the predictor output)
    # TO ADD THE CODE ! 
  }
  
  # write the result into a separated file
  write.csv(DataSet.scaled, outFileName,row.names=F, quote=F)  
}



# ---------------------------------------------------------------
# TESTING THE FUNCTION
# ---------------------------------------------------------------
#OriginalFile="DataResults/ds.csv"
#ScaledFile="DataResults/ds.scaled.csv"
#s = 1 # type of scaling = 1:standardization, 2:normalization, 3: other to define
#c = 1 # column to start scaling: 1 = include the dependent variable, 2 = only features

#ScalingDS(s,c,OriginalFile,ScaledFile)
#file.show(ScaledFile)
