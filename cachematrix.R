## This file contains two functions, makeCacheMatrix and cacheSolve.
## These two functions used together allow for caching a matrices inverse.

# This function creates a "matrix" object that can cache its inverse.
# It utilizes a setter instead of `<-`
# e.g. > x <- makeCacheMatrix(matrix(rnorm(4), 2, 2))
#      > x$set(matrix(rnorm(10), 5, 5))
makeCacheMatrix <- function(x = matrix()) {
  # Variable to store the matrix's inverse
  i <- NULL
  # Getter and Setter's for the matrix
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  # Getter and Setter's for the matrix's inverse
  setinverse <- function(minverse) i <<- minverse
  getinverse <- function() i
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


# This function computes the inverse of the matrix returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
# retrieve the inverse from the cache. It does so by utilizing functionality baked into the matrix returned
# from makeCacheMatrix. This function will not work on regular matrices.
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  i <- x$getinverse()

  # If i is not null, then the inverse is already cached
  if (!is.null(i)) {
    message("Getting cached inverse")
    return(i)
  }

  data <- x$get()
  # Inverse the matrix and cache it
  i <- solve(data)
  x$setinverse(i)
  return(i)
}
