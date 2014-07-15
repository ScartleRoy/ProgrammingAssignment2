## Two functions to cache the inverse of a matrix
## using "<<-" operator.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cache_inv <- NULL
  
  # Set the matrix
  set <- function(matrix) {
    x <<- matrix
    cache_inv <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Cache the inverse of the matrix
  
  setInverse <- function(inv) {
    cache_inv <<- inv
  }
  
  # Get the inverse of the matrix
  
  getInverse <- function() cache_inv
  
  # Return the list containing the results above
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # Get the inverse of the matrix
  inv <- x$getInverse()
  
  # If the inverse is not NULL, just return its value
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse is NULL, get the matrix, calculate the inverse and set the inverse
  
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInverse(inv)
  inv
}
