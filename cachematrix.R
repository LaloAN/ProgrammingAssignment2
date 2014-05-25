##
## This function creates a special "matrix" object that can cache its inverse.
##
## In this example we introduce the <<- operator which can be used to assign a value
## to an object in an environment that is different from the current environment.
## Below are two functions that are used to create a special object that stores
## a square matrix and cache's its inverse.
##
## The first function, makeCacheMatrix creates a special "matrix", which is really
## a list containing a function to
##
##    set the value of the matrix  ->  set
##    get the value of the matrix  ->  get
##    set the value of the inverse ->  setSolved
##    get the value of the inverse ->  getSolved
##
## The x variable stores the matrix
## The mSolved variable stores the inverse
##

makeCacheMatrix <- function(x = matrix()) {
    # initialize mSolved to NULL
	mSolved <- NULL
	
	# assign a value in this enviroment of matrix value
	set <- function(y) {
		x <<- y
		mSolved <<- NULL
	}
	
	# obtain the value of matrix stored in this enviroment
	get <- function() x
	
	# assign the value of inverse in this enviroment
	setSolve <- function(solve) mSolved <<- solve
	
	# obtain the value of inverse stored in this enviroment
	getSolve <- function() mSolved
	
	# create a list with previuos functions
	list(set = set, get = get,
		setSolve = setSolve,
		getSolve = getSolve)
}

##
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##
## The inverse of a square matrix can be computed with the solve function. 
##

The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
	# Return a matrix that is the inverse of 'x'
	mSolved <- x$getSolve()
	
	# verify if mSolved is not empty and return the inverse matrix
	if(!is.null(mSolved)) {
		message("getting Matrix Inverse cached data")
		return(mSolved)
	}
	
	#obtain of special matrial the value of matrix
	data <- x$get()
	
	#solve the matrix for obtain the inverse value
	mSolved <- solve(data, ...)
	
	# store in special matrix the value of inverse 
	x$setSolve(mSolved)
	# print value on screen
	mSolved
}
