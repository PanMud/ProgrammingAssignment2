##cachematrix.R  -- functions that Computes the inverse of a square matrix 


## function makeCacheMatrix:: This function creates a special "matrix" object that can cache its inverse.
## Input -> x: a square invertible matrix
## returns : a list containing functions
##  1. set the matrix
##  2. get the matrix
##  3. set the inverse
##  4. get the inverse
##  These functions are used in the cacheSolve() function


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) m <<- inverse
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached inverse matrix data")
    return(m)
  }
  mdata <- x$get()
  m <- solve(mdata, ...)
  x$setmatrix(m)
  m
}

