## This function creates a special "matrix" object that can cache its inverse.
## Makes a cacheMatrix object that caches (e.g. remembers) whether the inverse has been calculated)
## First call computes the inverse and remembers it. Subsequent calls return previously computed values

## The abstraction creates a get, set and caching mechanism.
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing 
## functions to:

##     set the value of the matrix     (set)
##     get the value of the matrix     (get)
##     set the value of the inverse    (setinv)
##     get the value of the inverse    (getinv)

makeCacheMatrix <- function(x = matrix()) {

      inv <- NULL
  
      # When the value of the matrix changes, reset cache to uncomputed (NULL)
      set <- function(y) {
          x <<- y
          inv <<- NULL
      }
  
      # Returns the cached matrix object
      get <- function() x
  
      # Record cached value of inverse
      setinv <- function(i) inv <<- i
  
      # Get cached value of inverse
      getinv <- function() inv
  
      list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}



## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  
      # See if the inverse has been cached
      i <- x$getinv()
  
      # If so return it
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
       }
  
       # Otherwise compute it and cache it
      data <- x$get()
       i <- solve(data, ...)
        x$setinv(i)
  
       # Return cached value
     i
}
