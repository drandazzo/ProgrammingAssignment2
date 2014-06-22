## Comments included
## First function makeCacheMatrix: this creates a special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # set the stored inverse value to NULL to begin
  invm <- NULL
  
  # set the value of the matrix here
  setm <- function(y) {
    x <<- y
    invm <<- NULL
  }
  # get the value of the matrix here
  getv <- function() x
  
  # set the value of the inverse of the matrix here
  setinvm <- function(solve) invm <<- solve
  
  # pull value of invers of matrix here  
  getinvm <- function() invm
  
  # return a list with the results here
  list(setm = setm, getv = getv,
       setinvm = setinvm,
       getinvm = getinvm)
  
}

## Second function casheSolve: calculates the inverse of the special matrix unless it has 
## already been calculated. if so, then it returns the calculated inverse, skipping the inverting step.

cacheSolve <- function(x,...) {
  # here is the check to see if the inverse has already been calculated
  z <- x$getinvm()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  # if it does not find the cashed record, calculate it
  b <- x$getinvm()
  
  # calcuate the inverse here
  z <- solve(b, ...)
  
  # set the cache for use
  x$setinvm(z)
  
  # return the inverse
  z
}
