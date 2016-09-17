# Note:
# Assignment is to write a pair of functions that 
# cache the inverse of a matrix with the assumption that
# input matrix is invertible.

#makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set the values of the matrix
  set <- function(y) {
    x <<- y  # Operator <<- is used to assign the value from memory when available
    m <<- NULL
  }
  
  # get the values of the matrix
  
  get <- function() x
  
  # set the values of the matrix
  
  setmat <- function(mat) m <<- mat
  
  # get the values of the matrix
  
  getmat <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}

#cacheSolve: This function computes the inverse of the special
#"matrix" returned by makeCacheMatrix above. If the inverse has 
#already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  # Computing the inverse of a square matrix can be done with the 
  # solve function in R. For example, if X is a square invertible 
  # matrix, then solve(X) returns its inverse.
  m <- solve(data)
  x$setmat(m)
  m
}
