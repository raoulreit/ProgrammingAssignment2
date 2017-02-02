##R-Programming - Assignment 2 - Lexical Scoping 

## The following functions will create a special matrix and will calculate the inverse of that matrix
## by caching it if it has already been calculated.


## This function creates a special matrix, which in essence is a list containing 4 functions that: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

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


## This function uses the output of the function above, cacheSolve(makeCacheMatrix(x)), to:
## return the inverse of the matrix from the cache if it has already been calculated
## calculate the inverse if not

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
