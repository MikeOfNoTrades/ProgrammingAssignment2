## Sun Feb 22 16:32:15 EST 2015
## This file contains two functions, makeCacheMatrix and cacheSolve.
## makeCacheMatrix creates a matrix object which can cache its inverse.
## cacheSolve provides the inverse of  the matrix, either by
## returning the cached version, or by using the solve function
## to calculate the matrix' inverse.

## These functions are written as a solution to the week 2
## programming assignment for the Coursera course: R Programming

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## return a list of functions
        inv <- NULL
	set <- function(y) {
                x <<- y
                inv <<- NULL
        }
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(	set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## created by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the function should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
	## This function returns a matrix that is the inverse of a matrix accessed
	## through a function 'x'.
	## parameter 'x' is a function that contains a list of functions
	## that this function uses to cache an inverse of a matrix
        
	inv <- x$getinverse()
	if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}

