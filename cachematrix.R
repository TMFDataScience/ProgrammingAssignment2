## Coursera programming assignment 2: Lexical Scoping 
## This script contains two functions:
## makeCacheMatrix - creates a specail "matric" object that can cache its inverse
## cacheSolve - computes the inverse of the special matrix returned by the 
## first function. If this already exists, return this rather 
## than re-calculating.

## makeCacheMatrix - Creates a vector of functions to get the matric, set the
## matric, get the inverse and set the inverse

makeCacheMatrix <- function(x = numeric()) {
	c <- NULL
	set <- function(y) {
		x <<- y
		c <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) c<<-inverse
	getInverse <- function() c
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve - this function calculates the inverse of a matrix.
## In doing this, it uses the functions returned by the function above 
## to return the value, picking up and returning a cached value instead 
## if it exists.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	c <- x$getInverse()
	if(!is.null(c)) {
		message("Returning cached data")
		return(c)
	}
	data <- x$get()
	c <- solve(data)
	x$setInverse(c)
	c
}
