## Functions to calculate the inverse of a matrix, with caching
## See tests for example usage

## Returns an object that caches the value and inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  get <- function() x

  setInverse <- function(i) {
    inverse <<- i
  }

  getInverse <- function() inverse

  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse of a matrix object returned from makeCacheMatrix
## Passes ... arguments through to solve
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setInverse(i)
  i
}
