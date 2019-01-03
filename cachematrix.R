## Put comments here that give an overall description of what your
## functions do


## creates a special matrix to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) 
{
        
        m <- null
	set <- function(y)
	{
		x <<- y
		m <<- null
	}
	get <- function() x
	getInverse <- function() m 
	setInverse <- function(inv) m <- inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Computes inverse of a special matrix obtained from makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
	if (!is.null(m))
	{
		message("getting cached data")
		return(m)
	}
	data <- x$get
	m <- solve(data)
	x$setInverse(m)
	m
}
