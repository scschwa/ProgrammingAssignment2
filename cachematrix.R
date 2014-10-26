## this function takes an input matrix, x, and then stores it and it's inverse 
## in two global values for quick calculations later.

makeCacheMatrix <- function(x) {
  
  ##  1. store a matrix
  ##  2. store the inverse of the current matrix
  
  ## store the current matrix for comparison in higher function
  ## using <<- so that matrixStore can be referred to globally
  matrixStore <<- x
  ## calculate the inverse for the current matrix
  matrixInverseStore <<- solve(matrixStore)  
}


## This function takes an input matrix, x, and then checks to see if:
## 1. Is this matrix invertible?  If not, will spit out an error msg.
## 2. If it is invertible, then it will check to see if it's been previously processed 
##    and exists in the cache.  If it does, values are pulled from cache.
## 3. if not, then makeCacheMatrix is called to generate the inverse.

cacheSolve <- function(x) {
  ### x is a user defined matrix
  ### returns an inverse of the matrix x if it's invertible, otherwise and error msg
  
  ## initialize matrixStore if it doesn't exist
  ## need to figure out nrows and ncols from input matrix to initialize if necessary
  if (!exists("matrixStore")) {matrixStore <- matrix(c(1),nrow=nrow(x),ncol=ncol(x))}
  
  ### check to see if it's invertible
  if (round(det(x),0) == 0) {
    message("Matrix isn't invertible.  Please try again.")
  } else{ 
    ### if it's invertable then continue
    
    message("Matrix is invertible.  Processing continues..")
    ### check to see if it's already been declared
    message("Checking...")
    
    ## check to see if it matches cached matrix
    if (!all(matrixStore == x)){
      makeCacheMatrix(x)
      
      message("First time this matrix was inverted.  Inverse calculated and cached values set.")
      matrixInverse <- matrixInverseStore
      return(matrixInverse)
    } else{
      message("This matrix has been previously processed, retrieving from cache...")
      matrixInverse <- matrixInverseStore
      return(matrixInverse)
    }
  }
}