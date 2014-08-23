##  The function makeCacheMatrix generates a list containing a function which
##  1. sets and caches the matrix
##  2. gets the matrix
##  3. solves and caches the matrix
##  4. gets the solved matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

##  The function cacheSolve checks if a matrix has been cached. If so, it returns the matrix.
##  Otherwise, the inverse of the matrix is obtained using the solve() function, assuming that the
##  matrix is invertible. The solved matrix is returned.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  else {
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
  }
}