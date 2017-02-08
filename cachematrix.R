##################################################################
## Put comments here that give an overall description of what your
## functions do
##
## Assigment 2:
##
## The matrix inversion of a numeric vector is typically a fast
## operation. However, if the same matrix inversion has to be
## computed repeatedly (e.g. in a loop) it may make sense to
## cache the value of the Matrix inversion so that when we need
## it again, it can be looked up in the cache rather than
## recomputed.
##
## How to use this caching system:
## To compute the inverse, pass a square inversible matrix to
## the makeCacheMatrix function and capture the output:
##
## > source('~/Documents/dataS/assignment2/ProgrammingAssignment2/cachematrix.R')
## > mym1 <- matrix(1:4, nrow = 2, ncol = 2)
## > cache1 <- makeCacheMatrix(mym1)
##
## To cache it, pass the capture output from above to the
## cacheSolve function:
##
## > cacheSolve(cache1)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## To check that it was cached, repeat above step and make sure
## that the "getting cache data" message appears:
##
## > cacheSolve(cache1)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## >
##
##################################################################

##################################################################
## Write a short comment describing this function
##
## This function creates a special "matrix" object that can cache
## its inverse.
##
## Inputs:
## x is a square invertible matrix
##
## Output:
## list containing the special cachable "matrix" object
##
##################################################################
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

##################################################################
## Write a short comment describing this function
##
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.
##
## Inputs:
## x is the output of makeCacheMatrix
##
## Output:
## Return a matrix that is the inverse of 'x'
##
##################################################################
cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setSolve(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
