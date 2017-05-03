## Put comments here that give an overall description of what your
## functions do

##The cacheSolve function works with a makeCacheMatrix object, (function)
##getting the value of the inverse matrix of the makeCacheMatrix 
##object if it is available and setting it when it is not.

## Write a short comment describing this function

## In general, we are creating a makeCacheMatrix object.  
## It includes the data of the original matrix and the inverse
## of the matrix.  It also includes the methods to get and set 
## both the original matrix and the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

##The cacheSolve function works with a makeCacheMatrix object, (function)
##getting the value of the inverse matrix of the makeCacheMatrix 
##object if it is available and setting it when it is not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
