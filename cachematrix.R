## cachematrix contains functions allowing to limit matrix inversions to only one time

## makeCacheMatrix creates a special Matrix object containing functions allowing to retrieve initial matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      ## set inversion variable to NULL      
      inv = NULL
      
      ## prepare functions for special matrix object
      set = function(y) {
            x <<- y
            inv <<- NULL
      }
      get = function() x
      setinv = function(inverse) inv <<- inverse 
      getinv = function() inv
      
      ## return special matrix object
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The special object created by makeCacheMatrix is fed into CacheSolve, which computes the inverse or retrieves it the inverse if already computed

cacheSolve <- function(x, ...) {
      ## Access special object by makeCacheMatrix
      inv = x$getinv()
      
      ## Check if the inverse is already in the cacheMatrix
      if (!is.null(inv)){
            # Fetch available data in the cacheMatrix 
            return(inv)
      }
      ## if unavailable...
      else {
      ## compute inverse...
      cachematrixdata = x$get()
      inv = solve(cachematrixdata, ...)
      
      ## ...and save the inverse in the INV value
      x$setinv(inv)
      
      ## Return inverse
      return(inv)      
      }
}
