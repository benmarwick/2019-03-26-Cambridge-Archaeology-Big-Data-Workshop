# test_genMCMC_ANOVA.r

library(JerimalaiStoneArtefacts)
context("genMCMC_ANOVA")

test_that("genMCMC_ANOVA works", {
  
  library(JerimalaiStoneArtefacts)
  library(coda)
  
  # from Jags-Ymet-Xnom1fac-MnormalHom-Example.R

  myDataFrame = read.csv( file="data_for_tests/FruitflyDataReduced.csv" )
  # Specify the column names in the data file relevant to the analysis:
  yName="Longevity" 
  xName="CompanionNumber" 
  # Specify desired contrasts.
  # Each main-effect contrast is a list of 2 vectors of level names, 
  # a comparison value (typically 0.0), and a ROPE (which could be NULL):
  contrasts = list( 
    list( c("Pregnant1","Pregnant8") , c("None0") , compVal=0.0 , ROPE=c(-1.5,1.5) ) ,
    list( c("Pregnant1","Pregnant8","None0") , c("Virgin1","Virgin8") , 
          compVal=0.0 , ROPE=c(-1.5,1.5) ) ,
    list( c("Pregnant1","Pregnant8","None0") , c("Virgin1") , 
          compVal=0.0 , ROPE=c(-1.5,1.5) ) ,
    list( c("Virgin1") , c("Virgin8") , compVal=0.0 , ROPE=c(-1.5,1.5) ) 
  )
  # Specify filename root and graphical format for saving output.
  # Otherwise specify as NULL or leave saveName and saveType arguments 
  # out of function calls.
  fileNameRoot = "FruitflyData-NormalHom-" 
  graphFileType = "eps" 
  

  #---------------------------------------------------------
  # Generate the MCMC chain:
  mcmcCoda_ANOVA = genMCMC_ANOVA(datFrm=myDataFrame , yName=yName , xName=xName ,
                      numSavedSteps=11000 , thinSteps=10 , saveName=fileNameRoot )
  #--------------------------------------------------------- 
#   # Display diagnostics of chain, for specified parameters:
#   parameterNames = varnames(mcmcCoda) 
#   show( parameterNames ) # show all parameter names, for reference
#   for ( parName in c("ySigma","b0","b[1]","aSigma") ) {
#     diagMCMC( codaObject=mcmcCoda , parName=parName , 
#               saveName=fileNameRoot , saveType=graphFileType )
#   }
#   #--------------------------------------------------------- 
  # Get summary statistics of chain:
  summaryInfo_ANOVA = smryMCMC_ANOVA( mcmcCoda_ANOVA , 
                          datFrm=myDataFrame , xName=xName ,
                          contrasts=contrasts , 
                          saveName=fileNameRoot )
  
  # Display posterior information:
#   plotMCMC( mcmcCoda , 
#             datFrm=myDataFrame , yName=yName , xName=xName ,
#             contrasts=contrasts , 
#             saveName=fileNameRoot , saveType=graphFileType )
  #----------------------------------------------------------
  
  # clean up by removing files...
  do.call(file.remove,list(list.files(pattern = fileNameRoot)))
  file.remove("model.txt")
  
  # simply check that the output has the right number of rows and cols
  expect_equal(dim(summaryInfo_ANOVA), c(17,14))

})