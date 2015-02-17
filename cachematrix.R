## There are two functions in this file.
##
## makeCacheMatrix - puts the matrix and return the "matrix" object 
##                   that is the list with special get/set functions.
##
## cacheSolve      - computes the inverse of the special "matrix" returned 
##                   by makeCacheMatrix. If the inverse has already been calculated
##                   (and the matrix has not changed), then the cachesolve should 
##                   retrieve the inverse from the cache.

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m   <<- y
    inv <<- NULL
  }
  get <- function() { m }
  setinv <- function(value) { inv <<- value }
  getinv <- function() { inv }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(m, ...) {
  inv <- m$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- m$get()
  inv <- solve(matrix, ...)
  m$setinv(inv)
  inv
}

