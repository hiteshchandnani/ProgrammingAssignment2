## Below are a pair of functions that help create a cache for the inverse of a matrix
## Matrix inversion is generally very costly to compute and hence caching the inverse will prove effective 

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setinverse <- function(minv) matinv  <<- minv
        getinverse <- function() matinv 
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinverse()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data)
        x$setinverse(minv)
        minv
}
