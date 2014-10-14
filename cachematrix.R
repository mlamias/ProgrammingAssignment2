## These two functions are used to cache the inverse of a matrix

## makeCacheMatrix creates a special 'matrix', which is really a list containing a function to get and set the
## inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the inverse
  m <- NULL
  
  # This is a function that sets the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # This is a function that gets the matrix
  get <- function() {
    x
  }
  
  # This is a function that sets the *inverse* of the matrix
  setinverse <- function(inverse) {
    m <<- inverse
  }
  
  # This is a function that gets the *inverse* of the matrix
  getinverse <- function() {
    m
  }
  
  ## Return a list of the functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve is a function that calculates the inverse of the special 'matrix' created with the above function.
## However, it first checks to see if the inverse has already been calculated.  If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of x
  m <- x$getinverse()
  
  
  ## If the inverse is set, simply return the inverse
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the special matrix
  data <- x$get()
  
  ## Obtain the inverse of the matrix
  m <- solve(data)
  
  ## Set the inverse of the special matrix
  x$setinverse(m)
  
  ## Return the matrix
  m
} 