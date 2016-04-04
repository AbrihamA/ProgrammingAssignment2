## the first function "makeCacheMatrix" returns a list object which is composed of
## some functions which are used to set and return a matrix and its inverse. it will also catches the inverse
## a call to this function should pass an invertible matrix to "makeCacheMatrix" 
## eg. x<-makeCacheMatrix(y)    where y is an invertible matrix and x gets the list object returned by the function 
## and x will be passed to the second function "cacheSolve"

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) im <<- inverse
        getinverse <- function() im
        list(set = set, get = get,
             setinverse= setinverse,
             getinverse = getinverse)
}

## a call to this function should pass what is returned by the first function. 
## eg cacheSolve(x)    where x is an object returned by the first function.
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
        ## Return a matrix that is the inverse of 'x'	
}
