## The two functions below provide an infrastructure to compute the matrix inverse of
# an input matrix x, by only executing the computation once, caching the result, and
# reading the result from cache should the matrix inverse be required subsequently.

## makeCacheMatrix initializes the special data structure the caching infrastructure
# relies on, e.g., we create an empty input matrix mat <- makeCacheMatrix(), which
# contains four functions that handle data reading and writing for the input and
# inverse matrices. Thereupon, we can assign the actual input-matrix values by
# calling the function mat$set(matrix(rnorm(10^2),10,10))), with the matrix data
# as its argument; we can access the data by calling mat$get(). The inverse matrix
# is initialized as variable m equaling NULL, and reset to NULL whenever the input matrix
# is updated. The actual inverse-matrix data gets assigned by the function call 
# mat$setinverse(inverse).
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) m <<- inverse
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)   
}

## cacheSolve computes the matrix inverse of an input matrix only if the computation
# has not been carried out before, i.e., as long as x$getinverse() yields NULL. In the
# event it does not, the cached inverse matrix is returned instead as well as a 
# message. The computation uses the built-in function solve(x), however, configured to
# handle the special data structure of the input and inverse matrices, i.e., reading
# the former through x$get() and writing out the latter with x$setinverse(m).
cacheSolve <- function(x, ...) {
   m <- x$getinverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
}
