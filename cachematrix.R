## Put comments here that give an overall description of what your
## functions do

## Creates Matrix

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invs <<- inverse
  getinv <- function() invs
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          invs <- x$getinv()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setinv(invs)
  invs
}
