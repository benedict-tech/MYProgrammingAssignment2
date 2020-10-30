## Put comments here that give an overall description of what your
## functions do

##  makeCacheMatrix :  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  set <- function(y) {
    x <<- y
    r <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) r <<- inverse
  getinverse <- function() r
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve : This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  r <- x$getinverse()
  if (!is.null(r)) {
    message("getting cached data")
    return(r)
  }
  data <- x$get()
  r <- solve(data, ...)
  x$setinverse(r)
  r
}
