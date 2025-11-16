## makeCacheMatrix creates a special matrix object capable of caching its
## inverse. cacheSolve uses that special matrix object in order to calculate
## the matrix's inverse via from the cached value or actually calculating it.

## Creates special matrix object capable of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


## Calculates the inverse of x (special matrix object) via using a cached
## inverse stored in x or calculates and caches the inverse if it already
## does not exist in x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
