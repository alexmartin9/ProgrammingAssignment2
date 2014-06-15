## This pair of functions cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setinv <- function(I) Inv <<- I
        getinv <- function() Inv
        list(set = set, get = get, 
                     setinv = setinv,
                     getinv = getinv)
        }      
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x,...) {
        I <- x$getinv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        Inv <- solve(data,...)
        x$setinv(Inv)
        Inv
}
