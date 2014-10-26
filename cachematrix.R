## 1.`makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      
      ## set the matrix:
      setup <- function(matrix) {
            mtrx <<-matrix
            i <<- NULL
      }
      ## get the matrix & return it:
      get <- function() {
            mtrx
      }
      ## invert the matrix & return inverse matrix:
      setInverse <- function () {
            i <<- inverse
      }
      ## get inverse of matrix:
      getInverse <- function() {
            i
      }
      ## list of all methods:
      list(setup=setup, get=get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## 2.`cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## return matrix (=inverse of x):
      mtrx <- x$getInverse()
      
      ## return inverse if already set:
      if(!is.null(m) ) {
            message("retrieving cached data")
            return(mtrx)
      }
      
      ## get matrix:
      data <- x$get()
      ## use matrix multiplication to calculate inverse:
      mtrx <- solve(data) %*% data
      ## set inverse to object:
      x$setInverse(m)
      ## ...and return our matrix:
      mtrx
}
