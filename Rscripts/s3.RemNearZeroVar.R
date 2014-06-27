# ==========================================
# Removal of near zero variance columns
# ==========================================
# contact: Cristian R Munteanu | BiGCaT - UM | muntisa@gmail.com
#
# USE:
# RemNear0VarCols([Frama dataset],[details flag],[output file name])
#
# inputs:
# - ds = dataset frame
# - fDet = flag for detais (TRUE/FALSE)
# - outFileName = new file name  (it could include the path)

# output = ds.Rem0NearVar  (ds without columns with near zero variance)
# if datails = TRUE, output the new ds as a file

# ------------------------------------------

RemNear0VarCols <- function(ds,fDet=FALSE,outFile="") {
  # default parameters are no details, without a file name
  ds.Rem0NearVar <- ds # default output without any modification
  
  library(caret)
  
  ds.var <- nearZeroVar(ds) # get the near zero columns
  ds.Rem0NearVar <- ds[,-(ds.var)] # get only the columns without this problem
  if (fDet == TRUE) {
    # write the corrected ds file
    write.csv(ds.Rem0NearVar, outFile,row.names=F, quote=F)
  }
  return(as.data.frame(ds.Rem0NearVar)) # return the new ds 
}
