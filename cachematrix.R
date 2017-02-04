## To return the inverse of a matrix

## makeCacheMatrix creates an object that can cache its inverse

# first, we create a function which returns an empty matrix by default
makeCacheMatrix <- function(x = matrix()){
        minv <- NULL
        
        # the set function here allows us to "redefine" x should we wish to change the contents of the matrix
        # if we decide to change the contents of x, it returns the new value and sets the inverse to NULL
        set <- function(y){
                x <<- y
                minv <<- NULL
        }
        # get and getinv allocates x to get and minv to getinv respectively
        get <- function() x
        
        # setinv takes an argument z and allocates it to minv
        setinv <- function(z) minv <<- z
        getinv <- function() minv
        
        # we create a list to 'sort' the functions that makeCacheMatrix will have
                
        # as we have named the function and stored it in a list, 
        # we can use $[functionnname] instead of [[number]] to call the function
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve solves for the inverse of the matrix which is returned
## by makeCacheMatrix. If inverse has already been calculated, it will
## retrieve it from the cache.

cacheSolve <- function(x, ...) {
        # attempt to getinv (which is minv) from makeCacheMatrix
        minv <- x$getinv()
        
        # if getinv exists and is not NULL, return minv. Else, continue with the function call
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        
        # at this point in time, getinv does not exist. we set data to be the matrix x, and solve for minv.
        data <- x$get()
        minv <- solve(data, ...)
        
        # we set setinv to take on the value of minv in makeCacheMatrix, which then gets assigned to getinv.
        x$setinv(minv)
        minv
}
