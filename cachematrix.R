## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inverse.matrix <- NULL
  
  #Set the Matrix that we want to inverse
  set <- function(matrix) {
    x <<- matrix
    inverse.matrix <<- NULL
  }
  
      #Get the Matrix that we want to inverse
      get <- function() x
      
      #Set the inversed Matrix    
      set.inverse <- function(InvMat) inverse.matrix <<- InvMat
      
      #Get the inversed Matrix
      get.inverse <- function() inverse.matrix
      list(set = set, get = get,
           set.inverse = set.inverse,
           get.inverse = get.inverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  #Check whether the input Matrix 'x' is inversed or not. If it is already inversed then it will fetch the inversed matrix from the cache
  #if not it will proceed inverting the Matrix 'x'
  inverse.matrix <- x$get.inverse()
  if(!is.null(inverse.matrix)) {
    message("getting cached data")
    return(inverse.matrix)
  }
  
  ## Get the Matrix 'x' that we want to inverse
  mat <- x$get()
  
  ## Produce the inverse of the Matrix 'x'
  inverse.matrix <- solve(mat, ...)
  
  ## Set the inversed Matrix 'x'
  x$set.inverse(inverse.matrix)
  
  ## Return a matrix that is the inverse of Original Matrix 'x'
  inverse.matrix
  
}
