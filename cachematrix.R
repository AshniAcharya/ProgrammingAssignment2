## collectively, these functions eliminate the need to compute the inverse repeatedly by retriving it from cache when the matrix hasn't changed

## this function creates object "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(newMatrix) {
    matrix <<- newMatrix
    inverse <<- NULL
  }
  get <- function() matrix
  setInverse <- function(newInverse) inverse <<- solve(x)
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## this function computes the inverse of the special "matrix", retriving the inverse from cache if already calculated

cacheSolve <- function(x, ...) {
  newMatrix <- x$getInverse()
  if(!is.null(newMatrix)) {
    message("getting cached data")
    return(newMatrix)
  }
  data <- x$get()
  newMatrix <- inverse(data, ...)
  x$setInverse(newMatrix)
  newMatrix
}
