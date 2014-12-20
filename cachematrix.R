## makeCacheMatrix and cacheSolve create a list of functions create a matrix,
## calculate its inverse, and cache the inverse so it doesn't have to be 
## calculated each time it is needed.

## makeCacheMatrix is a function that takes an (empty) matrix as its formal
## argument generates a list of functions as its return value

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- solve(x)
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is a function that take a list of functions as its formal
## argument, searches through the environment in which the function was called
## by calling getinverse() itself, and either returns the pre-existing value
## of the matrix inverse or
## calcuclates the matrix inverse is none already exists in the calling
## environment.

cacheSolve <- function(x, ...) {
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
