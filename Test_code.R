### Create a function
jobOffers <- function(scores, lowerLimits, upperLimits){
  
  #Define matrix[1, length(lowerLimits)]
  x <- matrix(0, nrow = 1, ncol = length(lowerLimits))
  #We want to iterate as many times as we have # of limits to check
  for (j in 1:length(lowerLimits)) {
    y <- 0 # initialize/reset the value
    # Now iterate thru numbers in scores
    for(i in scores) {
      #check if the scores is between given range
      y <- append(y,ifelse(i >= lowerLimits[j] & i <= upperLimits[j], print(1), print(0)))
      #transform to numeric and sum up all cases when equal to 1 (1 means someone is in the range, so we count it)
      y<- as.numeric(y)
      y <- sum(y)
    }
    # Store result in the final matrix
    x[j] <- y
  }
  return(x)
}


### Example data (given)
scores <- c(2,3,8,3,7,4,2,10,100)
lowerLimits <- c(1,3)
upperLimits <- c(4,11)


### Call the function and show the matrix
answer <- jobOffers(scores, lowerLimits, upperLimits)
answer
