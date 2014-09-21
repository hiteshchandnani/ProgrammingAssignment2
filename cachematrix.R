## Below are a pair of functions that help create a cache for the inverse of a matrix
## Matrix inversion is generally very costly to compute and hence caching the inverse will prove effective 

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Creates a special matrix for cache inversion purpose
        ## Initialize matrix inversion paramter (matinv) to null
        matinv <- NULL
        ## set function to explicitly create a new matrix with y as the new matrix, set matrix inverse to null 
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        ## returns input matrix
        get <- function() x
        ## set inverse function to assign matrix inverse with the value that is passed to it
        setinverse <- function(minv) matinv  <<- minv
        ## get inverse function to return the matrix inverse
        getinverse <- function() matinv 
        ## returns a list of functions
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Tries to get the inverse of matrix by calling getinverse function in makeCacheMatrix
        minv <- x$getinverse()
        ## Checks if matrix inverse is not null - If not null, returns the cached value
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        ## If getinverse returns null, then determine inverse using solve function
        ## set matrix inverse using setinverse function in makeCacheMatrix
        data <- x$get()
        minv <- solve(data)
        x$setinverse(minv)
        minv
}
