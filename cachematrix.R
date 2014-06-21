## These functions create a special "matrix" object that can cache the matrix, return
## the matrix, compute the inverse of the matrix, cache the inverse, and return 
## the inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inverse) {
                i <<- inverse
        }
        getinverse <- function() {
                i
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" object 
## returned by makeCacheMatrix above if it has not already been calculated; 
## otherwise it will return the cached inverse matrix
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
        i
}
