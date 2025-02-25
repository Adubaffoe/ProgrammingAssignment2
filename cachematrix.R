## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse cache when matrix changes
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the cached inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # If inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # Otherwise, compute the inverse and cache it
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
