## These functions together calculate the inverse of a matrix (and stores)
## it in a cache. The first function stores a special matrix object.
## The second function calculates the inverse and stores it in a cache.
## If it has already been calculated and stored, then it will instead recall
## it from the cache. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinver <- function(inver) i <<- inver
  getinver <- function() i
  list(set = set, get = get, setinver = setinver, getinver = getinver)
}


## This function computes the inverse of the special
## matrix returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinver()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinver(i)
  i
}
