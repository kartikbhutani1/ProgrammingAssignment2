
## makeCacheMatrix function creates a special matrix object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve function computes the inverse of a source matrix
## and caches the result for the future use. 

## If the inverse has already been calculated,
## then the cache value is retrieved.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  message("No cached inverse matrix, calculating new one")
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
