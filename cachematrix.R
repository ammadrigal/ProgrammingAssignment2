## Put comments here that give an overall description of what your
## functions do

# These functions are for DataScience  second module second programming assigment
# The functions help store the result of inverting a matrix in cache memory and 
# retrieve the value from cache when the calculation has already been done, to save time.

## Write a short comment describing this function

# make CacheMatrix creates the list of functions used to set and get the value of the 
# inversed Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Write a short comment describing this function
# cacheSolve returns the value of the inverse Matrix, from cache (if previously calculated)
# or from fresh calculation (if first time calculated)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}



