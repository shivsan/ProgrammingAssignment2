## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		
		##function to set(populate) the data(matrix)
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
		
		##return the date(matrix)
        get <- function() x
		
		##sets the variable 'inv' in the parent environments with the inverse
        setinverse <- function(inverse) inv <<- inverse
		
		##returns the variable inv, which is presumed to be the inverse
        getinverse <- function() inv
		
		###return the list with all supporting functions and data members encapsulated
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function searches for the inverse, if it has already been populated (non-null), then returnss that value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
        if(!is.null(inv)) {
				##Already calculated inverse
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
