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
    xinv <- solve(m, ...)
    x$setinv(xinv)
    xinv
  }
}


testCacheMatrix <- function() {
  nn <- 20 # matrix size
  
  # random matrix are in general invertible by theory
  mraw <- matrix(runif(nn*nn), nrow = nn, ncol = nn)
  
  x <- makeCacheMatrix()
  
  # now x has nothing inside
  if (!is.null(x$getinv())) stop("x should cached nothing")
  
  # set matrix
  x$set(mraw)
  if (!all(x$get() == mraw)) stop("x should set target matrix")
  if (!is.null(x$getinv())) stop("x should cached nothing")
  
  # calculate inverse
  minv <- cacheSolve(x)
  
  # check it is really the inverse
  merr <- norm(mraw %*% minv - diag(nn))
  if (merr > 1e-8) stop("matrix inverse check failed")
  else message(paste("inverse matrix * original matrix err=", merr))
  
  # get cached matrix
  minvcached <- cacheSolve(x)
  if (!all(minv == minvcached)) stop("cached matrix is not as expected")
}


