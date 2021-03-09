## makeCacheMatrix() and cacheSolve() work together to store in memory
## a list containing the inverse of a specified matrix, and then return
## that same inverse without recalculation if cacheSolve() is subsequently
## called without rerunning makeCacheMatrix() with a different matrix.

## makeCacheMatrix() creates, and returns, a list to store in memory 
## the inverse of a matrix provided to makeCacheMatrix() as an
## argument.  makeCacheMatrix() is designed to work with cacheSolve() 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {
      	x <<- y
            m <<- NULL
	}
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
      	setsolve = setsolve,
            getsolve = getsolve)
}


## cacheSolve() checks makeCacheMatrix() for an already-calculated 
## inverse of the specified matrix and then either returns the previously
## calculated solution or calculates the inverse, saves it within the list
## returned by makeCacheMatrix() and then returns the inverse.

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
      	message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
	x$setsolve(m)
	m
}
