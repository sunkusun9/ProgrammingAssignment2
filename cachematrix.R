## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # cache the inverse matrix of x
  inv <- NULL
  # set matrix and clear inverse matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # return cached inverse matrix
  get <- function() x
  # set inverse matrix cache
  setinverse <- function(inv_) inv <<- inv_
  # get cached inverse matrix cache
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  # check the inverse matrix cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # get the matrix to solve inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  # store the inverse matrix to the cache
  x$setinverse(inv)
  inv
}
