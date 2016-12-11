## Function implementing a cached matrix with the ability to cache its inverse +
## function that retrieves the cached inverse or calculates the inverse of the matrix

## Returns a list of functions which act on cached x and inv variables
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		  x <<- y
          inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Calculates the inverse of CacheMatrix x or retrieves it from cache if previously calculated
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Getting inverse from cache.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
