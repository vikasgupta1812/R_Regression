# ==========================================
# Scaling dataset
# ==========================================
# contact: Cristian R Munteanu | BiGCaT - UM | muntisa@gmail.com
#
# USE:
# ScalingDS([ScalingType],[column to start scaling],[Original File name],[Scaled fime name])
#
# s = { 1,2,3 } - type of scaling: 1 = standardization, 2 = normalization, 3 = other
# c = the number of column into the dataset to start scaling
# - if c = 1: included the dependent variable
# - if c = 2: only the features will be scaled

# fDet = if we need details    (TRUE/FALSE)
# inFileName  = file name      (it could include the path)
# outFileName = new file name  (it could include the path)
# ------------------------------------------

ScalingDS <- function(ds,s=1,c=1,fDet=FALSE,outFileName="") {
  
  # DEFAULT scaled dataset = original
  # if other s diffent of 1,2,3 is used, no scaling!
  DataSet.scaled <- ds
  
  # if STADARDIZATION
  if (s==1) {
    # Scale all the features (from column c bacause column 1 is the predictor output)
    DataSet.scaled <- scale(ds[c:ncol(ds)],center=TRUE,scale=TRUE)  
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
  
  # if DETAILS
  if (fDet ==TRUE) {
    # write the result into a separated file
    write.csv(DataSet.scaled, outFileName,row.names=F, quote=F)  
  }
  return (as.data.frame(DataSet.scaled))
}
