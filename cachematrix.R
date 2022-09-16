## The following function, makeCacheMatrix, creates a special "matrix" object, 
## which is a list containing a function to
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse of the matrix
##   4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL 
  }
  get <- function() x
  setSolve <-  function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}


## The following function computes the inverse of the special "matrix" returned by 
## the above function makeCacheMatrix. However, it first checks to see if the inverse 
## has already been calculated (and the matrix has not changed). If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it calculates the inverse
## and sets the inverse in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <-  x$get()
  s <-  solve(data, ...)
  x$setSolve(s)
  s
}
