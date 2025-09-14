## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This script contains functions that create a special object
## that can cache a matrix and its inverse. The functions are designed
## to save computation time by storing the inverse so it doesn't have to be
## recalculated every time it's needed.
makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  ## Get the value of the matrix.  
  setinv <- function(inverse) inv <<- inverse
  ## Set the value of the matrix inverse.
  getinv <- function() inv
  ## Get the value of the matrix inverse from the cache.
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function computes the inverse of a matrix. It first checks if the
## inverse has already been computed and stored in the cache. If so, it
## retrieves the cached value, skipping the computation. Otherwise, it
## calculates the inverse and stores it in the cache for future use.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
