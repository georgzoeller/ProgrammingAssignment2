# Caching matrix function
# solve(A)	Inverse of A where A is a square matrix.

makeCacheMatrix <- function(x = matrix()) {
  result <- NULL
  #setter
  set <- function(y) {
    x <<- y
    result <<- NULL
  }
  
  #getter
  get <- function() x
  setSolve <- function(solve) result <<- solve
  getSolve <- function() result
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)  
}


cacheSolve <- function(x, ...) {
  # check cache
  result <- x$getSolve()
  if(!is.null(result)) {
    #cache hit
    return(result)
  }
  #cache miss
  data <- x$get()
  result <- solve(data, ...)
  x$setSolve(result)
  return(result)
}
