## Matrix inversion can be very costly. This functions attempts to solve that problem
## by caching it

## This function sets the value of the matrix, gets the matrix,
## sets the value of the inverse of the matrix and then gets the value 
## of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  set = function(y) {
    x <<- y ## assign to different environment
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inversematrix) inverse <<- inversematrix
  getinv <- function() inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##The following function checks to see if an inverse has been calculated. If so, 
## it prints that value from cache. If not, it calculates it. 

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    if(!is.null(inverse)) {
      message("getting cached data.")
      return(inverse)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
  }

