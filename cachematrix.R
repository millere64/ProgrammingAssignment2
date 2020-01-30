## These functions compute the inverse of a matrix and 
## save it to be used in future computations

## This function creates a matrix to store the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <-function(y){
    x <<- y
    inv <<- NULL
  }
 get <- function() x
 setInverse <- function(solveMatrix) inv <<- solveMatrix
 getInverse <- function() inv
 list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function checks to see if an inverse has been caluculated. If not it calculates the inverse of the matrix created above.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
