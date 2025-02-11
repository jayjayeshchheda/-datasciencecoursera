## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse as NULL
  inv <- NULL
  
  # Set the matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse cache when the matrix changes
  }
  
  # Get the matrix value
  get <- function() {
    return(x)
  }
  
  # Set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Get the cached inverse of the matrix
  getInverse <- function() {
    return(inv)
  }
  
  # Return the special matrix object with methods to set/get matrix and its inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inv <- x$getInverse()
  
  # If the inverse is cached, return it
  if(!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # Otherwise, calculate the inverse, cache it, and return it
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  
  return(inv)
}
