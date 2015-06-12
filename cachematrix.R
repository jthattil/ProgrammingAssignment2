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
      m <- NULL                          #Sets "m" to "NULL" vector
      set <- function (y){               #Creates function "set" which has argument "y"
            x <<- y                      #Caches "x" as value of "y"
            m <<- NULL                   #Caches "m" as value "NULL"
      }
      get <- function() x                #Create function "get" to return value of "x"
      setinv <- function(inv) m <<- inv  #Create function "setinv" to chache value of matrix inverse as "m"
      getinv <- function() m             #Create function "getinv" to retrieve cached value of "m"
      list(set = set, get = get, setinv = setinv, getinv = getinv)
            #Output of the function will be list of four functions "set", "get", "setinv", "getinv"

}


## Write a short comment describing this function

      # cacheSolve takes the special matrix created by the function
      # makeCacheMatrix and finds the inverse of the input
      # if this function was already performed the inverse which was
      # stored in the cache is retrieved instead of reperforming the 
      # calculation

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      m <- x$getinv()                      #Sets the local value of "m" as value obtained from makeCachedMatrix
      if(!is.null(m)){                     #Logical statement to check if "m" is not NULL
            message("Getting Cached Data") #If logical is TRUE return message and value of "m"
            return(m)
      }
      data <- x$get()                      #Sets "data" as value obtained from makeCachedMatrix
      m <- solve(data,...)                 #Solves "data" to find inverse and stores the value as "m"
      x$setinv(m)                          #Sets cached value of inverse using makeCachedMatrix nested function setinv
      m                                    #Returns value of inverse matrix
}
