## makeCacheMatrix(x) - Create list of functions to store and retrieve matrix
## cacheSolve(x) - Return cached inverse of matrix or compute and cache

## makeCacheMatrix creates a list of functions used to store and retrieve
## a matrix.

makeCacheMatrix <- function(x = matrix()) {
	# Initilize inverse matrix "i"
	i <- NULL

	# Set value of the matrix
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	# Get value of the matrix
	get <- function() x

	# Set value of the inverse matrix
	setinv <- function(inv) i <<- inv

	# Get value of the inverse matrix
	getinv <- function() i

	# Combine functions into list
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve(x) - Returns inverse of matrix. If inverse is already cached,
## return cached value. Otherwise compute inverse and cache result.

cacheSolve <- function(x, ...) {

        ## Get cached value of inverse if available and return
	i <- x$getinv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	## Get matrix
	data <- x$get()

	## Compute inverse
	i <- solve(data, ...)

	## Cache and return inverse
	x$setinv(i)
	i
}
