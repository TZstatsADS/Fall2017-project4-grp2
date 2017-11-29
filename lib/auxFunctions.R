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
  
  dfEntropy
  
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

# function to convert user-rate matrix
convert_matrix_from_dataset<-function(path){
  data <- read.csv(path, header=TRUE)
  userLength <- length(unique(data$User))
  movieLength <- length(unique(data$Movie))
  
  dataframe <- data.frame(matrix(0, nrow=userLength, ncol=movieLength+1))
  colnames(dataframe) <- c("User", as.character(sort(unique(data$Movie))))
  dataframe$User <- as.character(sort(unique(data$User)))
  
  len <- nrow(data)
  for (i in 1:len){
    movie <- data$Movie[i]
    user <- data$User[i]
    dataframe[as.character(user) == dataframe$User, as.character(movie) == colnames(dataframe)] <- data$Score[i]
    print(i) #marker
  }
  str <- strsplit(path,'/')[[1]][5]
  postfix <- substr(str, 5, nchar(str))
  write.csv(dataframe, file = paste("../data/data_sample/eachmovie_sample/movie", postfix,sep = ""))
}


#directly select top 20 neighbors for each user without weighting
neighbor_select_top_MS <- function(mat, neighbor){
  userName <- unique(mat$i)
  rownum <- length(userName)
  
  mat_spearman <- matrix(NA,rownum*neighbor, 2) 
  mat_vector <- matrix(NA, rownum*neighbor, 2)
  
  
  for (i in 1:rownum){
    user <- userName[i]
    
    marker <- 1+(i-1)*neighbor
    
    
    # for spearman
    neighbor_row_spearman <- filter(mat,i==user|j==user)%>%arrange(desc(spear_corr))%>%select(i,j)%>%head(neighbor)
    
    
    ngb <- c(neighbor_row_spearman$i[neighbor_row_spearman$i != user],neighbor_row_spearman$j[neighbor_row_spearman$j != user])
    
    mat_spearman[marker:(marker+neighbor-1),1:2] <- matrix(c(rep(user,20), ngb),byrow=FALSE, ncol=2) #as.matrix(neighbor_row_spearman)
    
    
    # for vector similarity
    neighbor_row_vector <- filter(mat,i==user|j==user)%>%arrange(desc(vec_sim))%>%select(i,j)%>%head(neighbor)
    
    ngb <-  c(neighbor_row_vector$i[neighbor_row_vector$i != user],neighbor_row_vector$j[neighbor_row_vector$j != user])
    
    mat_vector[marker:(marker+neighbor-1),1:2] <-  matrix(c(rep(user,20), ngb),byrow=FALSE, ncol=2) #as.matrix(neighbor_row_vector)   
    
    # records
    print(i)
  }
  
  data_spearman <- data.frame(mat_spearman)
  data_vector <- data.frame(mat_vector)
  
  colname <- c("user","neighbor")
  
  colnames(data_spearman) <- colname
  colnames(data_vector) <- colname
  
  #saveRDS(object = data_spearman, file = paste("../data/spearman_neighbor_top", neighbor,"_withoutWeight_MS.RData", sep="")) 
  write.csv(data_spearman, file = paste("../data/spearman_neighbor_top", neighbor,"_withoutWeight_MS.csv", sep=""))
  #saveRDS(object = data_vector, file = paste("../data/vector_neighbor_top", neighbor,"_withoutWeight_MS.RData", sep=""))
  write.csv(data_vector, file = paste("../data/vector_neighbor_top", neighbor,"_withoutWeight_MS.csv", sep=""))
}



# select neighborhood by threshold
neighbor_select_threshold_MS <- function(mat, threshold){
  userName <- unique(mat$i)
  rownum <- length(userName)
  
  mat_spearman <- NULL
  mat_vector <- NULL
  
  for (i in 1:rownum){
    user <- userName[i]
    
    
    # for spearman
    neighbor_row_spearman <- filter(mat,i==user|j==user, abs(spear_corr) >= threshold)%>%select(i,j)
    
    
    ngb <- c(neighbor_row_spearman$i[neighbor_row_spearman$i != user],neighbor_row_spearman$j[neighbor_row_spearman$j != user])
    
    submat <- matrix(c(rep(user,length(ngb)), ngb),byrow=FALSE, ncol=2) #as.matrix(neighbor_row_spearman)
    
    mat_spearman<-rbind(mat_spearman,submat)
    
    
    
    # for vector similarity
    neighbor_row_vector <- filter(mat,i==user|j==user, abs(vec_sim) >= threshold)%>%select(i,j)
    
    ngb <-  c(neighbor_row_vector$i[neighbor_row_vector$i != user],neighbor_row_vector$j[neighbor_row_vector$j != user])
    
    submat <- matrix(c(rep(user,length(ngb)), ngb),byrow=FALSE, ncol=2)
    
    mat_vector <- rbind(mat_vector, submat)
    
    # records
    print(i)
  }
  
  
  
  data_spearman <- data.frame(mat_spearman)
  data_vector <- data.frame(mat_vector)
  
  colname <- c("user",paste("threshold_",threshold, sep=""))
  
  colnames(data_spearman) <- colname
  colnames(data_vector) <- colname
  
  write.csv(data_spearman, file = paste("../data/spearman_threshold_", threshold, "_withoutWeight_MS.csv", sep=""))
  
  
  write.csv(data_vector, file = paste("../data/vector_threshold_", threshold,"_withoutWeight_MS.csv", sep=""))
  
}



# neighbor selection for combo
neighbor_select_combo_MS <- function(mat, neighbor, threshold){
  userName <- unique(mat$i)
  rownum <- length(userName)
  
  mat_spearman <- NULL
  mat_vector <- NULL
  
  for (i in 1:rownum){
    user <- userName[i]
    
    #filter(similarity_measures_MS,i==10010, abs(spear_corr) >= 0.1)%>%select(i,j) %>%head(20) # arange again
    
    
    # for spearman
    neighbor_row_spearman <- filter(mat,i==user|j==user, abs(spear_corr) >= threshold)%>%arrange(desc(spear_corr))%>% select(i,j)%>%head(neighbor)
    
    
    ngb <- c(neighbor_row_spearman$i[neighbor_row_spearman$i != user],neighbor_row_spearman$j[neighbor_row_spearman$j != user])
    
    submat <- matrix(c(rep(user,length(ngb)), ngb),byrow=FALSE, ncol=2) #as.matrix(neighbor_row_spearman)
    
    mat_spearman<-rbind(mat_spearman,submat)
    
    
    
    
    # for vector similarity
    neighbor_row_vector <- filter(mat,i==user|j==user, abs(vec_sim) >= threshold)%>%arrange(desc(vec_sim))%>%select(i,j)%>%head(neighbor)
    
    ngb <-  c(neighbor_row_vector$i[neighbor_row_vector$i != user],neighbor_row_vector$j[neighbor_row_vector$j != user])
    
    submat <- matrix(c(rep(user,length(ngb)), ngb),byrow=FALSE, ncol=2)
    
    mat_vector <- rbind(mat_vector, submat)
    
    # records
    print(i)
  }
  
  
  
  data_spearman <- data.frame(mat_spearman)
  data_vector <- data.frame(mat_vector)
  
  colname <- c("user",paste("neighbor_",neighbor,"_threshold_",threshold, sep=""))
  
  colnames(data_spearman) <- colname
  colnames(data_vector) <- colname
  
  write.csv(data_spearman, file = paste("../data/spearman_neighbor_",neighbor,"_threshold_", threshold, "_withoutWeight_MS.csv", sep=""))
  
  write.csv(data_vector, file = paste("../data/vector_neighbor_", neighbor, "threshold_", threshold,"_withoutWeight_MS.csv", sep=""))
  
}



#directly select top 20 neighbors for each user without weighting
neighbor_select_top_movie <- function(mat, neighbor){
  userName <- unique(mat$i)
  rownum <- length(userName)
  
  mat_spearman <- matrix(NA,rownum*neighbor, 2) 
  mat_vector <- matrix(NA, rownum*neighbor, 2)
  
  
  for (i in 1:rownum){
    user <- userName[i]
    
    marker <- 1+(i-1)*neighbor
    
    
    # for spearman
    neighbor_row_spearman <- filter(mat,i==user|j==user)%>%arrange(desc(spear_corr))%>%select(i,j)%>%head(neighbor)
    
    
    ngb <- c(neighbor_row_spearman$i[neighbor_row_spearman$i != user],neighbor_row_spearman$j[neighbor_row_spearman$j != user])
    
    mat_spearman[marker:(marker+neighbor-1),1:2] <- matrix(c(rep(user,20), ngb),byrow=FALSE, ncol=2) #as.matrix(neighbor_row_spearman)
    
    
    # for vector similarity
    neighbor_row_vector <- filter(mat,i==user|j==user)%>%arrange(desc(vec_sim))%>%select(i,j)%>%head(neighbor)
    
    ngb <-  c(neighbor_row_vector$i[neighbor_row_vector$i != user],neighbor_row_vector$j[neighbor_row_vector$j != user])
    
    mat_vector[marker:(marker+neighbor-1),1:2] <-  matrix(c(rep(user,20), ngb),byrow=FALSE, ncol=2) #as.matrix(neighbor_row_vector)   
    
    # records
    print(i)
  }
  
  data_spearman <- data.frame(mat_spearman)
  data_vector <- data.frame(mat_vector)
  
  colname <- c("user","neighbor")
  
  colnames(data_spearman) <- colname
  colnames(data_vector) <- colname
  
  
  write.csv(data_spearman, file = paste("../data/spearman_neighbor_top", neighbor,"_withoutWeight_movie.csv", sep=""))
  
  write.csv(data_vector, file = paste("../data/vector_neighbor_top", neighbor,"_withoutWeight_movie.csv", sep=""))
}


# select neighborhood by threshold
neighbor_select_threshold_movie <- function(mat, threshold){
  userName <- unique(mat$i)
  rownum <- length(userName)
  
  mat_spearman <- NULL
  mat_vector <- NULL
  
  for (i in 1:rownum){
    user <- userName[i]
    
    
    # for spearman
    neighbor_row_spearman <- filter(mat,i==user|j==user, abs(spear_corr) >= threshold)%>%select(i,j)
    
    
    ngb <- c(neighbor_row_spearman$i[neighbor_row_spearman$i != user],neighbor_row_spearman$j[neighbor_row_spearman$j != user])
    
    submat <- matrix(c(rep(user,length(ngb)), ngb),byrow=FALSE, ncol=2) #as.matrix(neighbor_row_spearman)
    
    mat_spearman<-rbind(mat_spearman,submat)
    
    
    
    # for vector similarity
    neighbor_row_vector <- filter(mat,i==user|j==user, abs(vec_sim) >= threshold)%>%select(i,j)
    
    ngb <-  c(neighbor_row_vector$i[neighbor_row_vector$i != user],neighbor_row_vector$j[neighbor_row_vector$j != user])
    
    submat <- matrix(c(rep(user,length(ngb)), ngb),byrow=FALSE, ncol=2)
    
    mat_vector <- rbind(mat_vector, submat)
    
    # records
    print(i)
  }
  
  
  
  data_spearman <- data.frame(mat_spearman)
  data_vector <- data.frame(mat_vector)
  
  colname <- c("user",paste("threshold_",threshold, sep=""))
  
  colnames(data_spearman) <- colname
  colnames(data_vector) <- colname
  
  write.csv(data_spearman, file = paste("../data/spearman_threshold_", threshold, "_withoutWeight_movie.csv", sep=""))
  
  
  write.csv(data_vector, file = paste("../data/vector_threshold_", threshold,"_withoutWeight_movie.csv", sep=""))
}


# neighbor selection for combo
neighbor_select_combo_movie <- function(mat, neighbor, threshold){
  userName <- unique(mat$i)
  rownum <- length(userName)
  
  mat_spearman <- NULL
  mat_vector <- NULL
  
  for (i in 1:rownum){
    user <- userName[i]
    
    #filter(similarity_measures_MS,i==10010, abs(spear_corr) >= 0.1)%>%select(i,j) %>%head(20) # arange again
    
    
    # for spearman
    neighbor_row_spearman <- filter(mat,i==user|j==user, abs(spear_corr) >= threshold)%>%arrange(desc(spear_corr))%>% select(i,j)%>%head(neighbor)
    
    
    ngb <- c(neighbor_row_spearman$i[neighbor_row_spearman$i != user],neighbor_row_spearman$j[neighbor_row_spearman$j != user])
    
    submat <- matrix(c(rep(user,length(ngb)), ngb),byrow=FALSE, ncol=2) #as.matrix(neighbor_row_spearman)
    
    mat_spearman<-rbind(mat_spearman,submat)
    
    
    
    
    # for vector similarity
    neighbor_row_vector <- filter(mat,i==user|j==user, abs(vec_sim) >= threshold)%>%arrange(desc(vec_sim))%>%select(i,j)%>%head(neighbor)
    
    ngb <-  c(neighbor_row_vector$i[neighbor_row_vector$i != user],neighbor_row_vector$j[neighbor_row_vector$j != user])
    
    submat <- matrix(c(rep(user,length(ngb)), ngb),byrow=FALSE, ncol=2)
    
    mat_vector <- rbind(mat_vector, submat)
    
    # records
    print(i)
  }
  
  
  
  data_spearman <- data.frame(mat_spearman)
  data_vector <- data.frame(mat_vector)
  
  colname <- c("user",paste("neighbor_",neighbor,"_threshold_",threshold, sep=""))
  
  colnames(data_spearman) <- colname
  colnames(data_vector) <- colname
  
  write.csv(data_spearman, file = paste("../data/spearman_neighbor_",neighbor,"_threshold_", threshold, "_withoutWeight_movie.csv", sep=""))
  
  write.csv(data_vector, file = paste("../data/vector_neighbor_", neighbor, "threshold_", threshold,"_withoutWeight_movie.csv", sep=""))
  
}
