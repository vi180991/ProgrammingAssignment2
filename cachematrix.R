## We have two functions one cache the matrix and other to compute the inverse and cache it 

## makeCacheMatrix function performs the following:
## set the matrix val
## get the matrix val
## set the inverse matrix
## get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The function makeCacheMatrix will check for the cached value of matrix inverse
## If its not available it computes the inverse using solve function, else gets from the cache
## This function assumes that the matrix is always invertible 

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("Fetching cached Matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}