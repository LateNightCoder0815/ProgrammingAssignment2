## These functions calculate the inverse of a matrix and can be used to cache
## results in order to save computation time. For the caching I am using the
## lexical scoping concept of R.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(input) inverse <<- input
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of makeCacheMatrix. 
## Retrieves the cached value if the inverse has been calculated before.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}



## Usage example


## mymatrix <- matrix( c(2, 4, 3, 1), nrow=2, ncol=2) 
## x <- makeCacheMatrix(mymatrix)
## cacheSolve(x)

## Result
## [,1] [,2]
## [1,] -0.1  0.3
## [2,]  0.4 -0.2

## cacheSolve(x)

## Result
## getting cached data
## [,1] [,2]
## [1,] -0.1  0.3
## [2,]  0.4 -0.2