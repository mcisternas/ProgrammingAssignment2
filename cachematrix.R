## The goal of these two functions is to cache the inverse of a matrix
## in order not to compute it repeatedly.
## The first function, makeCacheMatrix, creates a special "matrix" object 
## that can cache its inverse, while cacheSolve compute the inverse of the 
## special "matrix" returned by makeCacheMatrix. If the inverse has already 
## been computed before (and the matrix has not changed), then the inverse is 
## retrieved from the cache.

## makeCacheMatrix function:
## This function takes an invertible matrix as an argument, and creates a 
## special "matrix" object, which actually is a list of functions:
## set, get, setinv and getinv. These functions, can (1) set the elements 
## of the matrix, (2) get the elements of the matrix, (3) set the values
## of the inverse, and (4) get the elements of the inverse of the matrix,
## respectively.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) {
    i <<- inv
  }
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve function:
## The function below calculates the inverse of the matrix created with the
## makeCacheMatrix function. Before computing the inverse, first it checks
## whether the inverse has already been calculated before. If that's the case,
## it gets the inverse from the cache. Otherwise, it computes the inverse and 
## cache's it with the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
