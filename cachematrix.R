
## the makeCacheMatrix function will take a numeric matrix and save the matrix and its inverse to a cache environment

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL  ##initialize the variable "I" (for inverse) with NULL value
  setcache <- function(y) {  ##create function to save variables to special cache environment
    x <<- y    ## saves matrix to the cache
    I <<- NULL ## saves initial NULL value of inverse to the cache  
  }
  
  getmatrix <- function() x ## function to get the matrix from cache
  setinverse <- function(solve) I <<- solve ##function to find the inverse of the matrix and save result to the cache
  getinverse <- function() I ## function to get the inverse value from cache
  list(setcache = setcache, getmatrix = getmatrix, 
       setinverse = setinverse, getinverse = getinverse)
  ## list() allows the values of the four functions to be accessed in the same
  ## environment in which makeCacheMatrix was defined
  
}


## This function determines whether the inverse of the specified matrix has already
## been saved to cache, and if it has, to return the inverse (without recalculating it).
## If the inverse has not already been calculated, then it calculates it for the given
## matrix and then saves that inverse to cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse() ##gets getinverse value in the list returned from makeCacheMatrix
  if(!is.null(I)) {
    message("getting cached inverse of matrix")
    return(I)
  }
  ##if the getinverse value was already available it obtains the value from cache and 
  ## prints a message stating as such, and returns the inverse value. If the value was not 
  ## available the code below will run:
  
  newinverse <- x$getmatrix() ##if new matrix assign it to 'newinverse' for calculation 
  I <- solve(newinverse, ...) ## find the inverse of the new matrix
  x$setinverse(I) ##set the value of the inverse to cache
  I ##print the result
}



