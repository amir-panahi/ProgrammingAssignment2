## The purpose of these functions is to cache the inverse of an invertible 
## matrix. These functions help reduce computation time by reducing
## repetitive calculation especially when it comes to large matrices

## this function creates a special "matrix" which is really a list
## containing 4 functions : set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the special
## matrix created with baove function. It first checks to see 
## if the inverse is calculated, if so it gets the inverse from
## the cache and skips computation, otherwise it calculates the
## inverse and updates the value in the cache and return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
