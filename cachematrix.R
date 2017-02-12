
## This function creates a special matrix object that can cache its inverse.
## It also builds and returns 4 setter and getters functions within a list 
makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function(y) {
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invr <<- inverse
        getinverse <- function() invr
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the matrix created by the function MakeCacheMatrix. 
## If the inverse of the matrix has already been calculated (and is identical) it return the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invr <- x$getinverse()
        if(!is.null(invr)) {
                message("getting cached data")
                return(invr)
        }
        data <- x$get()
        invr <- solve(data, ...)
        x$setinverse(invr)
        invr
        }