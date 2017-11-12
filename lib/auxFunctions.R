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
