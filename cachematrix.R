## This pair of functions work together to create a 'special' matrix object 
## which stores it's inverse for future use, reducing the number of times a 
## computationally intensive function needs to be run.
##
## To use these functions you need a matrix: 
##    e.g. matrix <- matrix(1:4,2,2) ## creates a 2x2 matrix
## First store the matrix with makeCacheMatrix:
##    specialmatrix <- makeCacheMatrix(matrix) ## this creates the special matrix we will work with
## Then the inverse can be found using cacheSolve:
##    inverse1 <- cacheSolve(specialmatrix) ##storing the inverse, here the inverse is calculated
## If we use cacheSolve again the cached version of the matrix will be retrieved:
##    inverse2 <- cacheSolve(specialmatrix) ##again storing the inverse, in this case the cached version is returned.


## makeCacheMatrix:
## Creates a special version of a matrix which can be used by the cacheSolve
## function to cache the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setinv <- function(inv) cache <<- inv
  getinv <- function() cache
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve:
## This function interacts with the special matrix created by makeCacheMatrix
## to return the inverse of the matrix. If the inverse has not been calculated
## previously the function uses solve to find the inverse, stores it within the 
## special matrix object, and returns the inverse. If the inverse has been 
## previously calculated the funtion retreives it from the special matrix and 
## returns it without having to compute it again.
cacheSolve <- function(x, ...) {
    cache <- x$getinv()
    if(!is.null(cache)) {   
      message("getting cached data")
      return(cache)
    }
    data <- x$get()
    cache <- solve(data, ...)
    x$setinv(cache)
    cache
  }
