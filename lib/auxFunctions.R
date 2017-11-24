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
calculateEntropy <- function( data ){
  
  ## input:
  ## - matrix/dataframe with users in rows and attirbutes in columns
  ## output
  ## - matrix with two columns (user, entropy)
  
  library( entropy )
  
  dfEntropy <- matrix( NA, nrow = nrow(data), ncol = 2 )
  for( i in 1:nrow( data ) ){
    
    # get user id
    dfEntropy[ i, 1 ] <-  as.numeric(data[i,1])
    
    # calculate entropy for user i
    dfEntropy[ i, 2 ] <- entropy( as.numeric( data[i,2:ncol(data)] ) )
  }
  
  colnames( dfEntropy ) <- c("user", "entropy")
  
  return dfEntropy
  
}