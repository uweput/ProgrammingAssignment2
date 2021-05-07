## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {          ##sets the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x         ##gets the value of the matrix
  setInverse <- function(inverse) {inv <<- inverse}           ## sets the value of the inverse
  getInverse <- function() {inv}                              ## gets the value of the inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  ## testing to see if the inverse has already been calculated. If so, it gets the inverse from the matrix and skips the computation
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve (data, ...)        ##computing the inverse of a square matrix
  x$setInverse(inv)
  inv
}
