## cachematrix.R
## Author: Alexandre Vilcek

## This program defines 2 functions: makeCacheMatrix() and cacheSolve()
## It allows for definition of a matrix object and the calculation of its inverse

## makeCacheMatrix() takes as input an n x m matrix or an empty one (by default) and returns
## a list containing 4 functions that operate as getters and setters on that matrix:
## get(): return the matrix defined as the input argument to the makeCacheMatrix() function.
## set(): sets the matrix defined as the input argument to the makeCacheMatrix() function.
## getInverse(): return the inverse of the matrix defined as the input argument to the makeCacheMatrix() function.
## setInverse(): sets the inverse of the matrix defined as the input argument to the makeCacheMatrix() function.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(z) i <<- z
  getInverse <- function() i
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve() takes as input the return of the makeCacheMatrix() function, which is a list
## containing the get(), set(), getInverse(), and setInverse() functions, then calculate and
## returns the inverse of the matrix defined as the input argument to the makeCacheMatrix() function.
## cacheSolve() also caches the inverse of that matrix into the makeCacheMatrix() function through its
## setInverse() function, so that the inverse needs to be recalculated only when the matrix change,
## and can be efficiently retrieved by the getInverse() function.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
