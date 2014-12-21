## These functions calculates the inverse of a matrix, and saves the result in
## cache so that it can be reused without the need to calculate it.  


## The makeMatrix function creates an object from the input vector and adds and 
## element, m, to store the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The cacheSolve function, then, first fetches the inverse (m) from the 
## makeMatrix. If it is not null (i.e. it has been stored in cache), the 
## function returns the inverse. If it is not null, the inverse is calculated 
## and stored in m.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}