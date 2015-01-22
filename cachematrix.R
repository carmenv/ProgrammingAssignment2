## makeCacheMatrix and cacheSolve are functions to cache 
## the inverse of a matrix rather than computing it repeatedly


##  makeCacheMatrix creates a special matrix" object 
##  that can cache its inverse.
##  It receives the matrix as a parameter. It assumes the matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inversem) im <<- inversem
  getinverse <-  function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## cacheSolve computes the inverse of a matrix object 
## returned by makeCacheMatrix
## If the inverse has been calculated previously, the
## cached result is returned, making it more efficient.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("Getting cached inverse matrix")
    return(im)
  }
  xmatrix <- x$get()
  im <- solve(xmatrix, ...)
  x$setinverse(im)
  im
}
