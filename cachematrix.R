## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <- y
    m <- NULL
  }
  ## Simply returns matrix x.
  get <- function() x
  ## Assign the inversed matrix to m.
  setinverse <- function(inverse) m <<- inverse
  ## Simply returns inversed matrix m.
  getinverse <- function() m
  ## Create and return a list of functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Attempt to get the inverse of matrix x.
    m <- x$getinverse()
    ## If m is not null, then there is a cached data,
    ## so simply return this data.
    if (!is.null(m)) {
      message("getting cached data")
      return (m)
    }
    ## Get matrix data.
    data <- x$get()
    ## Do inverse of matrix using built-in function solve().
    m <- solve(data, ...)
    ## Set inversed matrix to m.
    x$setinverse(m)
    ## Return inversed matrix.
    m
}
