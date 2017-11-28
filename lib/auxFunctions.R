#####################################
## Script with auxiliary functions ##
#####################################

# function to read Anonymous Microsoft Web Data dataset
readAnonymousMicrosoftWebData <- function(){
  
  ## Return:
  ## list with 2 datatables: train, test

  library( data.table )
  
  # read training data (attributes and case/vote)
  train <- fread("../data/data_sample/MS_sample/data_train.csv", sep = ",", select = c(2,3,4))
  
  # read test data (attributes and case/vote)
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

# function to calculate 
varianceWeighting <- function( user_data, user_a, user_u ){
  
  #user_a <- "10021"
  #user_u <- "10019"
  #user_data <- fread("/Users/pedrohmariani/Documents/Columbia/Academic/Fall 2017/AppliedDataScience/Projects/TZstatsFolder/fall2017-project4-fall2017-project4-grp2/data/data_sample/MS_sample/matrix_train.csv", sep = ",")      
  
  # number of items
  m <- ncol(user_data) - 1
  
  # votes user_a
  votes_a <- as.numeric(user_data[ user_data$V1 == user_a ])
  
  # votes user_u
  votes_u <- as.numeric(user_data[ user_data$V1 == user_u ])
  
  # get all items variances
  vars <- apply( user_data[,2:ncol(user_data)] , 2, var )
  
  # cumulators
  cumNum <- 0
  cumDen <- 0
  for( i in 1:m ){
    
    # normalized rating for item i, user u
    z_u_i <- ( votes_u[ 1 + i ] - mean( votes_u ) ) / sd( votes_u )
    
    # normalized rating for item i, user a
    z_a_i <- ( votes_a[ 1 + i ] - mean( votes_a ) ) / sd( votes_a )
    
    # variance weight i
    v_i <- ( vars[ i ] - min( vars ) ) / max( vars )
    
    # add to cumulators
    cumNum <- cumNum + v_i * z_a_i * z_u_i
    cumDen <- cumDen + v_i
    
  }
  
  # return variance weight between users a and u
  cumNum / cumDen
  
}

# function to predict based on Spearman or Vector Similarity
collabPredict <- function( user_data, neighbors, measure, measure_data, user_a, atrib ){
  
  # inputs
  # - user_data : input data with votes and users
  # - neighbors : dataframe with two columns i, j, j being the neighbors
  # - measure : string "spearman" or "vector_similarity"
  # - measure_data : dataframe containing similarity measures
  # - user_a : user to predict 
  # - atrib : atribute to predict (rating on movie or clicking on MS website)
  
#   user_a <- "10021"
#   user_data <- fread("/Users/pedrohmariani/Documents/Columbia/Academic/Fall 2017/AppliedDataScience/Projects/TZstatsFolder/fall2017-project4-fall2017-project4-grp2/data/data_sample/eachmovie_sample/movie_train.csv", skip = 1, sep = ";")
#   measure <- "spearman"
#   measure_data <- fread("/Users/pedrohmariani/eclipse-workspace/proj4-datascience/similarity_measures_MS.csv", sep = ",")
#   atrib <- "1207"
#   neighbors <- data.table( "i" = c( rep( user_a, 30 ), rep( "10019", 30 ), rep( "10031", 30 ) ),
#                            "j" = c( user_data$V1[501:530], user_data$V1[401:430], user_data$V1[31:60] ) )
#   
  # load library
  library(data.table)
  
  # subset neighbors of user_a
  neigbors <- as.data.table( neighbors )
  neighbors <- neighbors[ i == user_a ]
  
  # subset weights between user a and rest
  sim_filt <- measure_data[ i == user_a | j == user_a ]
  
  # define column of similarity to be used
  simCol <- ifelse( measure == "spearman", 3, 4 )
  
  # calculate average rating of active user
  r_a_bar <- mean( as.numeric(user_data[ user_data$V1 == user_a ])[2:ncol(user_data)] )
  
  cumNum <- 0
  cumDen <- 0
  for( u in neighbors$j ){
    
    # calculate average rating of user u
    r_u_bar <- mean( as.numeric(user_data[ user_data$V1 == u ])[2:ncol(user_data)] )
    
    # rating of user u to atribute i
    r_u_i <- as.numeric(user_data[ user_data$V1 == u ])[which( colnames(user_data) == atrib )]
    
    # get weight between user a and u
    w_a_u <- as.numeric( sim_filt[ i== u | j == u ] )[ simCol ]
    
    # calculate numerator
    cumNum <- cumNum + ( r_u_i - r_u_bar ) * w_a_u
    
    # calculate denominator
    cumDen <- cumDen + w_a_u
  
  }
  
  #make prediction
  pred <- r_a_bar + cumNum / cumDen
  
  # return prediction
  pred
  
}


