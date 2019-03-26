# test_genMCMC_ANOVA.r

library(JerimalaiStoneArtefacts)
context("genMCMC_cont_table")

test_that("genMCMC_cont_table works", {
  
  library(JerimalaiStoneArtefacts)
  library(coda)
  
  # from Jags-Ycount-Xnom2fac-MpoissonExp-Example.R
  
  #------------------------------------------------------------------------------- 
  #Load The data file 
  #.............................................................................
  myDataFrame = read.csv( file="data_for_tests/HairEyeColor.csv" )
  # Alter count by a multiplier, for demo purposes:
  countMult = 1
  myDataFrame$Count = round( myDataFrame$Count * countMult )
  fileNameRoot = paste0("HairEyeColor-",countMult,"-") 
  yName="Count" 
  x1Name="Eye" 
  x2Name="Hair"  
  x1contrasts = list( 
    list( c("Green") , c("Hazel") , compVal=0.0 , ROPE=c(-0.1,0.1) ) ,
    list( c("Blue")  , c("Green") , compVal=0.0 , ROPE=c(-0.1,0.1) )  
  )
  x2contrasts = list( 
    list( c("Black") , c("Blond") , compVal=0.0 , ROPE=c(-0.1,0.1) ) ,
    list( c("Brown") , c("Red")   , compVal=0.0 , ROPE=c(-0.1,0.1) )  
  )
  x1x2contrasts = list( 
    list( list( c("Blue") , c("Brown") ) ,
          list( c("Black") , c("Blond") ) ,
          compVal=0.0 , ROPE=c(-0.1,0.1) ) 
  ) 
  numSavedSteps = 12000
  thinSteps = 10
  
  #.............................................................................
  graphFileType = "eps" 
 
  # -------------------------------------------------------------------- 
  # Generate the MCMC chain:

  mcmcCoda_cont_table <- genMCMC_cont_table(datFrm=myDataFrame, 
                      yName=yName , x1Name=x1Name , x2Name=x2Name ,
                      numSavedSteps=numSavedSteps , thinSteps=thinSteps , 
                      saveName=fileNameRoot )
  #------------------------------------------------------------------------------- 
  # Display diagnostics of chain, for specified parameters:
#   parameterNames = varnames(mcmcCoda) 
#   show( parameterNames ) # show all parameter names, for reference
#   for ( parName in c("b0","b1[1]","b2[1]","b1b2[1,1]","ppx1x2p[1,1]",
#                      "a1SD","a1a2SD") ) {
#     diagMCMC( codaObject=mcmcCoda , parName=parName , 
#               saveName=fileNameRoot , saveType=graphFileType )
#   }
  #------------------------------------------------------------------------------- 
  # Get summary statistics of chain:
  summaryInfo_cont_table = smryMCMC_cont_table( codaSamples = mcmcCoda_cont_table , 
                          datFrm=myDataFrame , x1Name=x1Name , x2Name=x2Name ,
                          x1contrasts=x1contrasts , 
                         x2contrasts=x2contrasts , 
                         x1x2contrasts=x1x2contrasts ,
                          saveName=fileNameRoot )
  # show(summaryInfo)
  # Display posterior information:
#      plotMCMC_cont_table( mcmcCoda , 
#                datFrm=myDataFrame , yName=yName , x1Name=x1Name , x2Name=x2Name ,
#                x1contrasts=x1contrasts , 
#   #             x2contrasts=x2contrasts , 
#   #             x1x2contrasts=x1x2contrasts ,
#                saveName=fileNameRoot , saveType=graphFileType )
  #------------------------------------------------------------------------------- 
  # Other specific comparisons of cells:
#   if ( substr(fileNameRoot,1,12) == "HairEyeColor" ) {
#     # THIS x1level minus THAT x1level at AT x2level:
#     THISx1 = "Blue"
#     THATx1 = "Brown"
#     ATx2 = "Black"
#     THISidx = which(levels(myDataFrame[,x1Name])==THISx1)
#     THATidx = which(levels(myDataFrame[,x1Name])==THATx1)
#     ATidx   = which(levels(myDataFrame[,x2Name])==ATx2)
#     openGraph(height=4,width=4)
#     compInfo = plotPost( 
#       as.matrix(mcmcCoda)[,paste("b1b2[",THISidx,",",ATidx,"]",sep="")] -
#         as.matrix(mcmcCoda)[,paste("b1b2[",THATidx,",",ATidx,"]",sep="")] , 
#       main=paste(THISx1,"-",THATx1,"@",ATx2) , 
#       xlab=paste("Beta Deflect. Diff.") , 
#       compVal=0 ,ROPE=c(-0.1,0.1) )
#     show(compInfo)
#     saveGraph(file=paste(fileNameRoot,THISx1,"-",THATx1,"At",ATx2,sep=""),
#               type=graphFileType)
#     # THIS x1level minus THAT x1level at AT x2level:
#     THISx1 = "Blue"
#     THATx1 = "Brown"
#     ATx2 = "Blond"
#     THISidx = which(levels(myDataFrame[,x1Name])==THISx1)
#     THATidx = which(levels(myDataFrame[,x1Name])==THATx1)
#     ATidx   = which(levels(myDataFrame[,x2Name])==ATx2)
#     openGraph(height=4,width=4)
#     compInfo = plotPost( 
#       as.matrix(mcmcCoda)[,paste("b1b2[",THISidx,",",ATidx,"]",sep="")] -
#         as.matrix(mcmcCoda)[,paste("b1b2[",THATidx,",",ATidx,"]",sep="")] , 
#       main=paste(THISx1,"-",THATx1,"@",ATx2) , 
#       xlab=paste("Beta Deflect. Diff.") , 
#       compVal=0 ,ROPE=c(-0.1,0.1) )
#     show(compInfo)
#     saveGraph(file=paste(fileNameRoot,THISx1,"-",THATx1,"At",ATx2,sep=""),
#               type=graphFileType)
#     # THIS x2level minus THAT x2level at AT x1level:
#     THISx2 = "Black"
#     THATx2 = "Blond"
#     ATx1 = "Blue"
#     THISidx = which(levels(myDataFrame[,x2Name])==THISx2)
#     THATidx = which(levels(myDataFrame[,x2Name])==THATx2)
#     ATidx   = which(levels(myDataFrame[,x1Name])==ATx1)
#     openGraph(height=4,width=4)
#     compInfo = plotPost( 
#       as.matrix(mcmcCoda)[,paste("b1b2[",ATidx,",",THISidx,"]",sep="")] -
#         as.matrix(mcmcCoda)[,paste("b1b2[",ATidx,",",THATidx,"]",sep="")] , 
#       main=paste(THISx2,"-",THATx2,"@",ATx1) , 
#       xlab=paste("Beta Deflect. Diff.") , 
#       compVal=0 ,ROPE=c(-0.1,0.1) )
#     show(compInfo)
#     saveGraph(file=paste(fileNameRoot,THISx2,"-",THATx2,"At",ATx1,sep=""),
#               type=graphFileType)
#     # THIS x2level minus THAT x2level at AT x1level:
#     THISx2 = "Black"
#     THATx2 = "Blond"
#     ATx1 = "Brown"
#     THISidx = which(levels(myDataFrame[,x2Name])==THISx2)
#     THATidx = which(levels(myDataFrame[,x2Name])==THATx2)
#     ATidx   = which(levels(myDataFrame[,x1Name])==ATx1)
#     openGraph(height=4,width=4)
#     compInfo = plotPost( 
#       as.matrix(mcmcCoda)[,paste("b1b2[",ATidx,",",THISidx,"]",sep="")] -
#         as.matrix(mcmcCoda)[,paste("b1b2[",ATidx,",",THATidx,"]",sep="")] , 
#       main=paste(THISx2,"-",THATx2,"@",ATx1) , 
#       xlab=paste("Beta Deflect. Diff.") , 
#       compVal=0 ,ROPE=c(-0.1,0.1) )
#     show(compInfo)
#     saveGraph(file=paste(fileNameRoot,THISx2,"-",THATx2,"At",ATx1,sep=""),
#               type=graphFileType)
#   }
  #------------------------------------------------------------------------------- 

 
  # clean up by removing files...
  do.call(file.remove,list(list.files(pattern = fileNameRoot)))
  
  # simply check that the output has the right number of rows and cols
  expect_equal(dim(summaryInfo_cont_table), c(73, 14))

})