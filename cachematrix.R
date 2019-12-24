## Redesigned the orginal cachemean.R code to be matrix friendly and 
## relabel some of the list functions


## Performs the exact same way as makeVector, builds a list of 4 functions that 
## set the matrix, get the matrix, set the inverse of the matrix, and get the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
 	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(x) inv <<- solve(x)
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Performs the exact same way as cachemean by 
## substituting the solve function for the mean function. Also
## relabeled mean to inv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
