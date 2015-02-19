## makeCacheMatrix and cachesolve work together to find and cache inverse of a matrix
## they are used like this
## cache <- makeCacheMatrix(matrix)
## inverse <- cacheSolve(cache)

## makeCacheMatrix creates a list containing functions to
## parameters:
## x = The matrix to find the inverse of
##
## returns a list of four functions
## set = set the value of the matrix
## get = get the value of the matrix
## setinverse = set the inverse of the matrix
## getinverse = get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve finds the inverse of a matrix stored in a cache
## or returnd the cached inverse if the problem has already been solved
##
## parameters:
## x = a list containing four functions: get, set, getinverse
## and setinverse (the result of calling makeCacheMatrix)
## ... = parameters passed to the solve function
## returns the inverse of the matrix
##
## When using the cached data prints a message.


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  # check for the solution in the cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## find the inverse and cache it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
