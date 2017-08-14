## These two functions will cache the inverse of a matrix and return the cached value
## it will only calculate an inverse if no inverse has been previously calculated

## note that the first function does NOT calculate the inverse
## it returns a special "matrix" but it's really a list of functions

makeCacheMatrix <- function(x=matrix()) {
	inv <- NULL
	set <- function(y)
		{x <<- y
		inv <<- NULL}
	get <- function()x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}

## this function actually computes the inverse of the special matrix returned by the previous function
## it checks whether or not there is an inverse already calculated - and returns it if so
## if empty cacheSolve will get the original matrix values from the previous function and set the inverse 


cacheSolve <- function(x, ...) {
	inv <- x$getinverse
	if(!is.null(inv))
		{message("getting cached data")
		return(inv)}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
