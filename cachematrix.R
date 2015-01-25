## makeCacheMatrix() and cacheSolve() are a set of functions that work 
## together to cache the inverse of a particular matrix and return this
## inverse from memory without recomputing each time it is called

## makeCacheMatrix() takes a matrix object and returns a list of functions
## that cacheSolve() subsequently uses. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve takes a list of functions that makeCacheMatrix creates and uses
## to check if the cache matrix is cached. If it is then the inverse is 
## returned from cache else the inverse is computed, stored in cache and 
## returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
