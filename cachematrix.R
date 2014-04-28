
## This function creates a special "matrix" object that can cache its inverse,
## Since matrix conversion is a costly computation and their may be some benefit to caching the inverse of 
## a matrix rather than compute it repeaetedly.


## The first function makeCacheMatrix creates a special "matrix" object, which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the special matrix returned
## get the value of the inverse of the special matrix returned

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()              ## query the x matrix's cache  
  if(!is.null(m)) {                ## if there is a cache
    message("getting cached data")
    return(m)                      ## just return the cache, no computation needed
  }
  data <- x$get()                  ## if there's no cache
  m <- solve(data, ...)            ## compute them here
  x$setInverse(m)                  ## save the result back to x's cache
  m
        ## Return a matrix that is the inverse of 'x'
}
