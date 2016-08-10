## Calculates the inverse of a matrix and caches it to avoid recalculating
## the same inverse multiple times.
## Example: a <- makeCacheMatrix()
##          a$set(matrix(rnorm(4^2),nrow = 4,ncol = 4))
##          b<-cacheSolve(a)
##          a$get() %*% b ## identity matrix.


## makeCacheMatrix creates a special "matrix" that is a list of functions that
## sets the value of the matrix (set), gets the value of the matrix (get), sets
## the value of the inverse of the matrix (setminv), and gets the value of the
## inverse of the matrix (getminv).

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setminv <- function(minv) m <<- minv
     getminv <- function() m
     list(set = set, get = get,
          setminv = setminv,
          getminv = getminv)
}


## cacheSolve returns the inverse of the matrix "x$get()". First, it checks if 
## the inverse of the matrix has previously been calculated and cached. If so,
## it returns the cached inverse, otherwise, calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
     m <- x$getminv()
     if(!is.null(m)) { ## Checks if the inverse has been previously calculated.
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...) ## Calculates inverse of the matrix.
     x$setminv(m) ## Caches inverse.
     m
}
