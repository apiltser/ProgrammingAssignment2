## Caching the Inverse of a Matrix
makeCacheMatrix <- function(x = matrix()) {
  inv_mtrx <- NULL
  set <- function(y) {
    x <<- y
    inv_mtrx <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_mtrx <<- inverse
  getInverse <- function() inv_mtrx
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## This function computes the inverse of the matrix created by 
## makeCacheMatrix. If the inverse has already been calculated it will retrieve it from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_mtrx <- x$getInverse()
  if (!is.null(inv_mtrx)) {
    message("getting cached data")
    return(inv_mtrx)
  }
  mtrx <- x$get()
  inv_mtrx <- solve(mtrx, ...)
  x$setInverse(inv_mtrx)
  inv_mtrx
}

