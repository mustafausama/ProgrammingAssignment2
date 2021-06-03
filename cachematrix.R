## The first function returns a list of functions that manipulate
## the matrix object, and the second one uses the first function
## to cache the inverse of a matrix

## This function is used to cashe the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inver) {
    inv <<- inver
  }
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of a matrix, if not already cached
## and caches its value

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv))
    return(inv)
  mat <- x$get
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
