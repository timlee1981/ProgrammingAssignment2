## This function will cache the inverse of the matrix

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

## This function will return a matrix that is the inverse of 'x'.
## If the inverse was already cached, then it will return the 
## cached value. Otherwise it will do the calculation.

cacheSolve <- function(x, ...) {
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

## This is to test my work
m <- matrix(c(1,2,4,6,3,2,1,1,2,5,6,8,2,1,1,1),4,4)
m
m1 <- makeCacheMatrix(m)
m1
m2 <- cacheSolve(m1)
m2