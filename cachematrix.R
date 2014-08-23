## The following two functions are used to cache the inverse of a matrix
## This way can avoid to repeatly calculate matrix inversion

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invmatrix) inv <<- invmatrix
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Function CacheSolve compute the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv

}


