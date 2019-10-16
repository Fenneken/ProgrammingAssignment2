## Put comments here that give an overall description of what your
## functions do
## This set of functions works together to make use of cache, when inverting matrices

## makeCacheMtrix prepares a matrix for keeping the inverse of it in cache

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL                ## initialises s 
     set <- function(y) {     ## setter = function to put y on x in the parent environment
          x <<- y
          s <<- NULL
     }
     get <- function() x      ## getter
     setinverse <- function(solve) s <<- solve    ## obtains thsolve(n1)e inverse of and puts it in the parent environment
     getinverse <- function() s                   ## gives access to the inverse in the cache
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)                ##forms it all into a list set/get/setinverse/getinverse
}


## CacheSolve Caches the inverse of the matrix, or retrieves it from cache

cacheSolve <- function(x, ...) {
                         ## Return a matrix that is the inverse of '
                         ## First it gets access to the inverse of the matrix
     m <- x$getinverse()
                         ##If it is not empty, it was already in cache, and can be used
     if(!is.null(m)) {
          message("getting cached data")
          return(m)      ##this value is returned
     }
                         ##Otherwise, it gets the original matrix
     data <- x$get()
                         ##and it solves the original matrix
     m <- solve(data, ...)
                         ##and puts the inverse into cache
     x$setinverse(m)
                         ##returns/prints the inverse
     m
}
