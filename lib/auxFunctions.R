#####################################
## Script with auxiliary functions ##
#####################################

# function to read Anonymous Microsoft Web Data dataset
readAnonymousMicrosoftWebData <- function(){
  
  ## Return:
  ## list with 2 datatables: train, test

  library( data.table )
  
  # read training data (attributes and case/vote)
  #trainAttrib   <- fread("../data/dataset1.txt", skip = 7, sep = ",", nrow = 294)
  #trainCaseVote <- fread("../data/dataset1.txt", skip = 301, sep = ",")
  train <- fread("../data/data_sample/MS_sample/data_train.csv", sep = ",", select = c(2,3,4))
  
  # read test data (attributes and case/vote)
  #testAttrib <- fread("../data/dataset1test.txt", skip = 7, sep = ",", nrow = 294)
  #testCaseVote <- fread("../data/dataset1test.txt", skip = 301, sep = ",")
  test   <- fread("../data/data_sample/MS_sample/data_test.csv", sep = ",", select = c(2,3,4))
  
  return( list( "test" = test,
                "train" = train) )
  
}

# function to read EachMovie dataset
readEachMovieData <- function(){
  
  ## Return:
  ## list with 2 datatables containing train and test data sets
  
  library( data.table )
  
  train <- fread("../data/data_sample/eachmovie_sample/data_train.csv", sep = ",", select = c(2,3,4))
  test  <- fread("../data/data_sample/eachmovie_sample/data_test.csv", sep = ",", select = c(2,3,4))

  return( list( "train" = train,
                "test" = test) )
  
}

convertToMatrixData1 <- function( data ){
  
  # input:
  # matrix with input data
  
  # returns:
  #  - design matrix with dim: nrow = no. of cases; ncol = no. of attributes
  
  data <- cbind( data, "index" = seq( 1, nrow(data) ) )
  
  dfIndexAttrib <- data.table("attrib" = unique( trainCaseVote[ V1 == "V" ]$V2 ),
                              "index"  = seq(1,length(unique( trainCaseVote[ V1 == "V" ]$V2 ))))
  
  casesIndex <- data[ V1 == "C" ]$index
  
  designMatrix <- matrix( 0, nrow = length( casesIndex ), ncol = nrow(dfIndexAttrib) )
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
  
  designMatrix
  
} 

# function to calculate Spearman Correlation
calculateSpearmanCorr <- function( data ){
  
  corrMat <- matrix( NA, ncol = 5, nrow = sum( 1:(ncol(data)-2) ) )
  k <- 1
  for( i in 1:(ncol(data)-1) ){
    
    for( j in (i+1):(ncol(data)) ){
      
      # i
      corrMat[k,1] <- colnames(data)[i+1]
      
      # j
      corrMat[k,2] <- colnames(data)[j]
      
      # calculate spearman correlation
      corrMat[k,3] <- cor.test(unlist(data[[i+1]]), 
                               unlist(data[[j]]),  
                               method = "spearman")$estimate
      
      # calculate similarity vector
      
      # calculate entropy
      
      # update k
      k <- k + 1
      
    }
    
  }
  
  colnames(corrMat) <- c("i","j","spearman", "simVector", "entropy")
    
  return corrMat
  
}