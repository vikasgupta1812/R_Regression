# ============================================================
# Remove the correlated columns from a file
# ============================================================
# contact: Cristian R Munteanu | BiGCaT - UM | muntisa@gmail.com
#
# USE: RemCorrs(cutoff, inFileName, outFileName)
#
# cutoff = correlation cut off (ex: 0.65)
# inFileName  = file name      (it could include the path)
# outFileName = new file name  (it could include the path)

# Generates 5 file results:
# - returns a dataset without the correlated columns  (1 file)
# - generate initial correlation matrix 
#   and the one after removing the correlated features (2 files)
# - plots for the before and after correlation removal (2 files)
# ------------------------------------------------------------

RemCorrs <- function(cutoff, inFileName, outFileName) {
  
  library(corrplot) #corrplot: the library to compute correlation matrix.
  library(caret)
  
  # Read the tab file using the read table function
  DataSet <- read.table(inFileName,header=TRUE)
  
  # calculate the correlation matrix for the entire file!
  # !!! NEED TO BE CORRECTED to avoid dependent variable (first column) but to report it!
  corrMat <- cor(DataSet)
  CorrMatFile= paste(inFileName,".corrMAT.txt",sep='')
  # write correlation matrix as output file
  write.table(corrMat, CorrMatFile, sep="\t",row.names=F, quote=F)
  
  # Plot the matrix, clustering features by correlation index
  # corrplot(corrMat, order = "hclust")
  
  # plot the correlatio plot before correlation removal
  CorrPlotFile =  paste(inFileName,".corrs.png",sep='')
  png(height=1200, width=1200, pointsize=25, file=CorrPlotFile)
  col1 <-rainbow(100, s = 1, v = 1, start = 0, end = 0.9, alpha = 1)
  corrplot(corrMat,tl.cex=3,title="Initial feature correlation matrix",
           method="circle",is.corr=FALSE,type="full",
           cl.lim=c(-1,1),cl.cex=2,addgrid.col="red",
           addshade="positive",col=col1,
           addCoef.col = rgb(0,0,0, alpha = 0.6), mar=c(0,0,1,0), diag= FALSE) 
  dev.off()
  
  highlyCor <- findCorrelation(corrMat, cutoff)
  
  # Apply correlation filter with the cutoff
  # by removing all the variable correlated with more than cutoff
  DataSetFiltered.scale <- DataSet[,-highlyCor]
  corrMat <- cor(DataSetFiltered.scale)
  # plot again the rest of correlations after removing the correlated columns
  #corrplot(corrMat, order = "hclust")
  
  # plot the correlatio plot before correlation removal
  CorrPlotFile2 =  paste(inFileName,".afterRemCorr.png",sep='')
  png(height=1200, width=1200, pointsize=25, file=CorrPlotFile2)
  col1 <-rainbow(100, s = 1, v = 1, start = 0, end = 0.9, alpha = 1)
  corrplot(corrMat,tl.cex=3,title="Correlation matrix after removing correlated features",
           method="circle",is.corr=FALSE,type="full",
           cl.lim=c(-1,1),cl.cex=2,addgrid.col="red",
           addshade="positive",col=col1,
           addCoef.col = rgb(0,0,0, alpha = 0.6), mar=c(0,0,1,0), diag= FALSE) 
  dev.off()
  
  # correlation matrix for the rest of the columns after removal
  CorrMatFile2= paste(inFileName,".SelectedcorrMAT.txt",sep='')
  # write correlation matrix as output file
  write.table(corrMat, CorrMatFile2, sep="\t",row.names=F, quote=F)
  
  # write the new dataset without the correlated features
  write.table(DataSetFiltered.scale, outFileName, sep="\t",row.names=F, quote=F)
  
}

# ---------------------------------------------------------------
# TESTING THE FUNCTION
# ---------------------------------------------------------------
#OriginalFile="DataResults/scaled.txt"
#CorrectedFile="DataResults/NoCorrs.txt"
#cutoff = 0.65

#RemCorrs(cutoff,OriginalFile,CorrectedFile)
