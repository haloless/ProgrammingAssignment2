## Calculate and cache matrix invertion



## Create matrix object with cached inverse.
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  
  # getter & setter of matrix
  get <- function() { x }
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  # getter & setter of inverse
  getinv <- function() { xinv }  
  setinv <- function(inv) { xinv <<- inv }
  
  # return wrapped object
  list(get = get, set = set, getinv = getinv, setinv = setinv)
}


## Get cached inverse or calculate and cache.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  if (!is.null(xinv)) {
    message("getting cached data")
    xinv
  } else {
    message("solve and cache data")
    m <- x$get()
    xinv <- solve(m)
    x$setinv(xinv)
    xinv
  }
}
