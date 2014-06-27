# ============================================================
# Remove the correlated columns from a file
# ============================================================
# contact: Cristian R Munteanu | BiGCaT - UM | muntisa@gmail.com
#
# ds = dataset frame
# fDet = flag fro details (TRUE/FALSE)
# cutoff = correlation cut off (ex: 0.65)
# outFileName = new file name  (it could include the path)

# Generates 5 file results:
# - returns a dataset without the correlated columns  (1 file)
# - generate initial correlation matrix 
#   and the one after removing the correlated features (2 files)
# - plots for the before and after correlation removal (2 files)
# ------------------------------------------------------------

RemCorrs <- function(ds,fDet,cutoff,outFile) {
  
  library(corrplot) #corrplot: the library to compute correlation matrix.
  library(caret)
  
  DataSet <- ds               # input dataset
  DataSetFiltered.scale <- ds # default results without any modification
  
  # calculate the correlation matrix for the entire file!
  # !!! NEED TO BE CORRECTED to avoid dependent variable (first column) but to report it!
  corrMat <- cor(DataSet)                                             # get corralation matrix
  
  if (fDet==TRUE) {
    CorrMatFile <- paste(outFile,".corrMAT.csv",sep='')
    # write correlation matrix as output file
    write.csv(corrMat, CorrMatFile, row.names=F, quote=F)
  
    # Plot the matrix, clustering features by correlation index
    # corrplot(corrMat, order = "hclust")
  
    # plot the correlatio plot before correlation removal
    CorrPlotFile <-  paste(outFile,".corrs.png",sep='')
    png(height=1200, width=1200, pointsize=25, file=CorrPlotFile)
    col1 <-rainbow(100, s = 1, v = 1, start = 0, end = 0.9, alpha = 1)
    corrplot(corrMat,tl.cex=3,title="Initial feature correlation matrix",
             method="circle",is.corr=FALSE,type="full",
             cl.lim=c(-1,1),cl.cex=2,addgrid.col="red",
             addshade="positive",col=col1,
             addCoef.col = rgb(0,0,0, alpha = 0.6), mar=c(0,0,1,0), diag= FALSE) 
    dev.off()
  }
  
  highlyCor <- findCorrelation(corrMat, cutoff) # find corralated columns
  
  # Apply correlation filter with the cutoff
  # by removing all the variable correlated with more than cutoff
  DataSetFiltered.scale <- DataSet[,-highlyCor]
  
  if (fDet==TRUE) {
    corrMat <- cor(DataSetFiltered.scale)
    # plot again the rest of correlations after removing the correlated columns
    #corrplot(corrMat, order = "hclust")
  
    # plot the correlatio plot before correlation removal
    CorrPlotFile2 =  paste(outFile,".afterRemCorr.png",sep='')
    png(height=1200, width=1200, pointsize=25, file=CorrPlotFile2)
    col1 <-rainbow(100, s = 1, v = 1, start = 0, end = 0.9, alpha = 1)
    corrplot(corrMat,tl.cex=3,title="Correlation matrix after removing correlated features",
             method="circle",is.corr=FALSE,type="full",
             cl.lim=c(-1,1),cl.cex=2,addgrid.col="red",
             addshade="positive",col=col1,
             addCoef.col = rgb(0,0,0, alpha = 0.6), mar=c(0,0,1,0), diag= FALSE) 
    dev.off()
    # correlation matrix for the rest of the columns after removal
    CorrMatFile2 <- paste(outFile,".corrMAT4Selected.csv",sep='')
    # write correlation matrix as output file
    write.csv(corrMat, CorrMatFile2, row.names=F, quote=F)
    # write the new dataset without the correlated features
    write.csv(DataSetFiltered.scale, outFile, row.names=F, quote=F)
  }
  
  return(as.data.frame(DataSetFiltered.scale))
}
