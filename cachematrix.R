## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## There are 2 functions
## 1. makeCacheMatrix consists of set,get

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


## Write a short comment describing this function
## used to get cache data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}
