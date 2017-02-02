##R-Programming - Assignment 2 - Lexical Scoping 

## The following functions will create a special matrix and will calculate the inverse of that matrix
## by caching it if it's already been calculated


## This function creates a special matrix, which in essence is a list

makeCacheMatrix <- function(x = matrix()) {
          
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m  <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This functional caches the inverse of the special matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){ 
    message("getting cached data")
    return(m)            
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
