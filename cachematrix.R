## The goal is to create a pair of functions that cache the inverse of a matrix, 
## such that the inverse of a matrix will be calculated unless it can be cached. 
## This will help to avoid computing it repeatedily.

## This function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL            # cache the inverse of the matrix
  set <- function(y) {
    x <<- y            # set matrix
    i <<- NULL         # reset cached inverse
  }
  get <- function() x                           # get matrix
  setinverse <- function(inverse) i <<- inverse # set inverse of the matrix
  getinverse <- function () i                   # get inverse of the matrix
  list( set = set, 
        get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
}

## This function (cacheSolve) computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()            # get cached inverse
  if(!is.null(i)) {
    message("using cached data") # message obtained if using cached data
    return(i)
  }
  data <- x$get()       # get matrix
  i <- solve(data, ...) # compute the inverse
  x$setinverse(i)       # cache the computed inverse
  i                     # return a matrix that is the inverse of 'x'
}
