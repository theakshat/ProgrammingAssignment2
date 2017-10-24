## makeCacheMatrix will create a special matrix which can cache its own inverse to same computation
## example makeCacheMatrix(matrix(rnorm(4*4),4,4))

makeCacheMatrix <- function(x = matrix(), ...) {
  m <- NULL
  set <- function(y) {
    x <<- matrix(y)
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve will solve for inverse of the special matrix created above and will look for cached inverse if it already exists in the environment

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
