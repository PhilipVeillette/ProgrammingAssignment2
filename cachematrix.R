## The first function, makeCacheMatrix, creates a matrix that can cache its
## inverse. The second function, cacheSolve, coomputes the inverse of the matrix
## that was returned by the first function. If the inverse of the matrix has been
## previously solved and the matrix is unchanged, the cacheSolve function will
## retrieve the answer from the cache.

## Create a special matrix and calculate its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## Compute the inverse of the matrix or retrieve it from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}