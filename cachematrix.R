## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
  # following the mean function implementation, this function will perform:
    # set the matrix
    # get the matrix
    # set the inverse of the matrix
    # get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.  If existing, retrieve from cache.
        ## if not existing, derive the inverse and return output
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
