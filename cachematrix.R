## Functions that cache the inverse of a matrix


## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(j) {
    i <<- j
  }
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Check if the inverse is cached
  i <- x$getinverse()
  if (!is.null(i)) {
    message("Getting cached inverse matrix")
    return(i) 
  }
  
  # Generate and store the inverse
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  # Return a matrix that is the inverse of 'x'
  i
}
