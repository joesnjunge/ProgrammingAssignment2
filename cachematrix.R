## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  mat_inverse <- NULL

  #set the matrix
  set <- function(m) {
    # assign x out of the current scope.
    x <<- m
    # Reset the global inverse variable
    mat_inverse <<- NULL
  }

  get <- function() {
    # return value of x - global
    x
  }


  setInverse <- function(m) {
    #set the global inverse variable
    mat_inverse <<- m
  }

  getInverse <- function() {
    # get the global inverse variable
    mat_inverse
  }

  # expose these functions for later call

  list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse,
        about = "cache and inverse of matrix")
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversemat <- x$getInverse()
  if (!is.null(inversemat)) {
    # getting cached matrix and check if they are identical
        if (identical(x$get() %*% inversemat, inversemat %*% x$get())) {
                return(inversemat)
        }
  }
  # if there is not inverse cached or is not identical
  # calculate the inverse for the current matrix
  content <- x$get()
  inversemat <- solve(content, ...)
  #cache the inverse.
  x$setInverse(inversemat)

  return(inversemat)
}
