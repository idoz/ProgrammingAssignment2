## Put comments here that give an overall description of what your
## functions do
# With these function we use the <<- operator to assign the inverse of an invertible matrix in an environment that is different from the current environment.

## Write a short comment describing this function
# makeCacheMatrix creates a special "vector" of the functions: set (the value of the matrix), get (the value of the matrix), 
# set (the value of the matrix inverse), get (the value of the matrix inverse)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(Solve) m <<- Solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)  
}


## Write a short comment describing this function
# cacheSolve calculates the inverse of the special "matrix" created with the above function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m  
}