# This file contains a pair of functions that cache the inverse of a matrix.
# The function makeCacheMatrix creates a special matrix object.
# The funtion cacheSolve computes the inverse of the above matrix.
# The code assumes that the matrix supplied is always invertible.

# This function creates a special matrix object that can cache its inverse.
# This special matrix object is a list containing functions to set and get the
# value of the matrix as well as set and get the value of the inverse of the
# above matrix.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL	# variable to hold the matrix inverse
	# function to set the value of the matrix
	setmatrix <- function(y) {
		x <<- y
		i <- NULL
	}
	# function to get the value of the matrix
	getmatrix <- function() { x }
	# function to set the value of the inverse of the matrix
	setinv <- function(inv) { i <<- inv }
	# function to get the value of the inverse of the matrix
	getinv <- function() { i }
	
	# create the special matrix object
	list(setmatrix = setmatrix, getmatrix = getmatrix,
		setinv = setinv, getinv = getinv)
}


# This function computes the inverse of the special matrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	# get the value of the matrix inverse
	i <- x$getinv()
	
	if (!is.null(i)) {
	# the inverse is already computed
		message("getting cached data")
		return(i)
	}
	
	# calculate the inverse

	# get the matrix
	m <- x$getmatrix()
	# find the inverse of the matrix
	i <- solve(m)
	# cache the value of inverse in the special matrix object
	x$setinv(i)
	# return the value of the inverse
	i
}
