## Matrix inversion is a costly process and usually
## takes too long if it has to be done repeatedly
## To reduce the processing time, caching is
## a feasible solution. The following two functions
## stores a matrix and caches it

## makeCacheMatrix function creates a matrix and 
## stores its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## cacheSolve function checks if there is inverse of the
## specified matrix in cache. If present, returns the same;
## if not present then creates the inverse and stores in cache

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}
