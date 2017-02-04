## To return the inverse of a matrix

## makeCacheMatrix creates an object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
        minv <- NULL
        set <- function(y){
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinv <- function(z) minv <<- z
        getinv <- function() minv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve solves for the inverse of the matrix which is returned
## by makeCacheMatrix. If inverse has already been calculated, it will
## retrieve it from the cache.

cacheSolve <- function(x, ...) {
        minv <- x$getinv()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv
}
