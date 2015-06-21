## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	## create a matrix object x and some associated sub-functions/methods define the cache m        
	m <- NULL        
	set <- function(y) {                
		## assign the input matrix y to the variable x in the parent environment
		x <<- y               
	
		## re-initialize m in the parent environment to null 
		m <<- NULL        
	}

	## return the matrix x
	get <- function() x

	## set the cache m equal to the inverse of the matrix x
	setMatrix <- function(inverse) m <<- inverse 
	
	## return the cached inverse of x
	getMatrix <- function() m 
	list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
	m <- x$getMatrix()
	if(!is.null(m)) {
		message("gettign cached data")
		return(m)
	}

	data <- x$get()
	m <- solve(data, ...)
	x$setMatrix(m)
	m
}
