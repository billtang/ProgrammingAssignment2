makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    getsolve <- function() m
    setsolve <- function(solve) m <<- solve

    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if ( !is.null(m)) {
        message('getting cached data')
        return (m)
    }
    data <- x$get()
    m <-solve(data, ...)
    x$setsolv(m)
    m
}

#### Examples of solve:
## >foo=matrix(1:4, 2)
## >foo
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## >solve(foo)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


#### test cacheSolve() function:
## >foo=matrix(1:4, 2)
## >foo
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## >bar=makeCacheMatrix(foo)
## >cacheSolve(bar)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
