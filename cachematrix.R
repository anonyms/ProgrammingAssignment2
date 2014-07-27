## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix() function creates a list enabaling certain actions on a matrix
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the interse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inverse <<- solve
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## The cacheSolve function returns the inverse of a matrix (supposition: matrix is invertible)
## It does so by firstly looking up if the inverse has already been calcuated, 
## if yes, it returns the already calculated inverse matrix
## if no, it calculates the inverse matrix (with the solve() function)

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
