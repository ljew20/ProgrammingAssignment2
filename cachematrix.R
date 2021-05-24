## Pair of functions that cache the inverse of a matrix 
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If inverse has already been calculated, this function 
## should retrieve the inverse from the cache. 

## makeCacheMatrix caches the inverse of a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL 
 set <- function(y) {
   x <<- y 
   i <<- NULL
 }
 get <- function() x
 setsolve <- function(solve) i <<- solve
 getsolve <- function() i 
 list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve computes the inverse of the special "matrix", returning the
## inverse from the cache if it has already been calculated. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  ## If the inverse of 'x' has already been calculated return this 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}