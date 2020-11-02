## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function returns an object that contains getter and setter functions
## for a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse_matrix <- NULL

	set <- function(y) {
		x <<- y
		inverse_matrix <<- NULL
	}

	get <- function() x
	setSolve <- function(solve) inverse_matrix <<- solve
	getSolve <- function() inverse_matrix
	list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## The cacheSolve function takes an object created with the makeCacheMatrix function
## It first tries to get the cached inverse of the matrix passed as an argument to it.
## If it is not NULL, then it returns the cached value.
## If this returns NULL (i.e. if the inverse has not been set yet), this function calculates it
## and sets this value as the cache for future use.
## This function returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
	inverse_matrix <- x$getSolve()

	if(!is.null(inverse_matrix)) {
		message("Getting cached data")
		return(inverse_matrix)
	}

	current_matrix <- x$get()
	inverse_matrix <- solve(current_matrix)
	x$setSolve(inverse_matrix)
	inverse_matrix
}