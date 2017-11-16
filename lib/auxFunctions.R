 #####################################
## Script with auxiliary functions ##
#####################################

# function to read Anonymous Microsoft Web Data dataset
readAnonymousMicrosoftWebData <- function(){
  
  ## Return:
  ## 4 datatables: trainAttrib, trainCaseVote, testAttrib, testCaseVote

  library( data.table )
  
  # read training data (attributes and case/vote)
  trainAttrib   <- fread("../data/dataset1.txt", skip = 7, sep = ",", nrow = 294)
  trainCaseVote <- fread("../data/dataset1.txt", skip = 301, sep = ",")
  
  # read test data (attributes and case/vote)
  testAttrib <- fread("../data/dataset1test.txt", skip = 7, sep = ",", nrow = 294)
  testCaseVote <- fread("../data/dataset1test.txt", skip = 301, sep = ",")
  
  return( list( "trainAttrib" = trainAttrib,
                "trainCaseVote" = trainCaseVote,
                "testAttrib" = testAttrib,
                "testCaseVote" = testCaseVote) )
  
}

# function to read EachMovie dataset
readEachMovieData <- function(){
  
  ## Return:
  ## 1 datatable containing dataset
  
  library( data.table )
  
  data <- fread("../data/Vote.txt")

  return( data )
  
}

# function to tranform dataset 1 to matrix form
convertToMatrixData1 <- function( data ){
   
   # returns:
   #  - design matrix with dim: nrow = no. of cases; ncol = no. of attributes
   
   data <- cbind( data, "index" = seq( 1, nrow(data) ) )
   
   dfIndexAttrib <- data.table("attrib" = unique( trainCaseVote[ V1 == "V" ]$V2 ),
                               "index"  = seq(1,length(unique( trainCaseVote[ V1 == "V" ]$V2 ))))
   
   casesIndex <- data[ V1 == "C" ]$index
   
   designMatrix <- matrix( NA, nrow = length( casesIndex ), ncol = nrow(dfIndexAttrib) )
   #for each case, fill columns of matrix with attributes
   for( iCase in 1:( length(casesIndex) - 1 ) ){
     
     presCase <- as.numeric( casesIndex[ iCase ] )
     nextCase <- as.numeric( casesIndex[ iCase + 1 ] )
     
     if( presCase - nextCase > 1){#presCase != length(casesIndex) - 1){
       
       valuesToFill <- data[(presCase + 1):(nextCase-1), 2]
       indexToFill  <- dfIndexAttrib[ which( dfIndexAttrib$attrib %in% valuesToFill$V2 ), 2 ]
       designMatrix[ iCase, indexToFill$index ] <- 1#valuesToFill$V2
       
     } else {
       
       valuesToFill <- data[(presCase + 1):(nextCase-1), 2]
       indexToFill  <- dfIndexAttrib[ which( dfIndexAttrib$attrib %in% valuesToFill$V2 ), 2 ]
       designMatrix[ iCase, indexToFill$index ] <- 1#valuesToFill$V2
       
     }
     
     
   }
   
   # redefining column/row names
   colnames(designMatrix) <- dfIndexAttrib$attrib
   rownames(designMatrix) <- data[ V1 == "C" ]$V2
   
   return designMatrix
   
 } 

# function to calculate Spearman Correlation
calculateSpearmanCorr <- function( data ){
  
  #cor.test(x, y,  method = "spearman")
  return data
  
}