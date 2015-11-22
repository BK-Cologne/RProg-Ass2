## 
## The script contains two functions: makeCacheMatrix and cacheSolve.
## makeCacheMatrix creates a matrix object that can cache an inverse matrix.  
## 

makeCacheMatrix <- function(x = matrix()) {

      m <- NULL
      get <- function() x      
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      getinverse <- function() m
      setinverse <- function(solve) m <<- solve
      list (set = set, get = get, 
            setinverse = setinverse, 
            getinverse = getinverse)
      
}

## cacheSolve computes the inverse of the matrix returned by the function above.  
## If the inverse is already calculated then cacheSolve retrieves the inverse from the cache.
## If the inverse is not calculated yet, cacheSolve gets the matrix "x" and calculates the inverse. 
## The function returns the inverse stored in "m" 

cacheSolve <- function(x, ...) {
      
      m <- x$getinverse()
      if(!is.null(m)) {
            message("Retrieving cached data")
            return(m)
      }      
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      return(m)      
}


