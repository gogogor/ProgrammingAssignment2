## ProgrammingAssignment2

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	##initialization of inverse
	i <- NULL

	##set method of matrix
	set <- function( matrix ){
		m <<- matrix
		i <<- NULL
	}

	##get method of matrix
	get <- function(){
		##return the matrix
		m
	}

	##set method of inverse matrix
	setInverse <- function(inverse){
		i <<- inverse
	}

	##get method of inverse matrix
	getInverse <- function(){
		i
	}
	
	##return a list of methods
	list(set = set, get = get,
	     setInverse = setInverse,
	     getInverse = getInverse)
	



}


## This function compute the inverse of special matrix returned by"makeCacheMatrix"

cacheSolve <- function(x, ...) {

	## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()

	##if the inverse is already set, return it
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}

	##get the matrix from object
	data <- x$get()

	##calculate the inverse 
	m <- solve(data) %*% data

	##set the inverse to the object
	x$setInverse(m)

	##return the matrix
	m
	  
}
