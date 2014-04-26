## Writing two functions: 1. makeCacheMatrix and 2. cacheSolve

## 1. The makeCacheMatrix function will be used to cache an invertible matrix
## It will contain four anonymous functions

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## 2. The cacheSolve function will return the inverse of a matrix
## It will first search for a cached matrix using an if statement
## It will use the solve function to return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
}
