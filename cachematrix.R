## makeCacheMatrix creates a matrix, which can cache it's inverse;
## It contains a function to returns and changes the vector x 
## stored in the main function, and than it stores
##the value of the input in a variable m.
##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setin <- function(solve) m <<- solve
  getin <- function() m
  list(set = set, get = get,
       setin = setin,
       getin = getin)
}


## cacheSolve calculates the inverse of the matrix returned 
## by makeCacheMatrix; if the inverse has already been calculated 
## it retrieves the inverse from the cache otherwise it  calculates 
## the inverse and it stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
    m <- x$getin()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setin(m)
    m
}
