## Put comments here that give an overall description of what your
## functions do

# This pair of functions make it possible to create a special matrix
# using the makeCacheMatrix function and subsequently find its
# inverse using the cacheSolve function
# it allows reduced computing power if the inverse is already stored
# in the cached data

## Write a short comment describing this function

# makeCacheMatrix is used to create a special matrix 
# The output of the function returns a list of functions
# these functions can be used to set and retrieve the 
# input matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function (y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

# cacheSolve takes the special matrix created by the function
# makeCacheMatrix and finds the inverse of the input
# if this function was already performed the inverse which was
# stored in the cache is retrieved instead of reperforming the 
# calculation

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)){
            message("Getting Cached Data")
            return(m)
      }
      data <- x$get()
      m <- solve(data,...)
      x$setinv(m)
      m
}
