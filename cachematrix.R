# This is a matrix object with functions for getting/setting the 
# object and its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # getter: returns the object
        get <- function() x
        # setter: sets the inverse
        setinverse <- function(inv) m <<- inv
        # returns the inverse object 
        getinverse <- function() m
        # returns a list of 4 functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# Find the inverse of a square matrix x and cache it
# argument x must be an object returned by makeCacheMatrix()
# assumption: x is non-singular
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)  # return the cached inverse (after the first invokation)
        }
        data <- x$get()
        m <- solve(data, ...)  # compute the inverse of the matrix (on first invocation)
        x$setinverse(m)  # store it for futuer use
        m  # return the inverse
}

# a quick test
set.seed(1)
x = matrix(rnorm(16,10,2), 4,4)
x
solve(x) # to ensure it is not singular
# test the functions
mvlist = makeCacheMatrix(x)
str(mvlist)
mvlist$set(x)
cacheSolve(mvlist)  # compute and cache the inverse
cacheSolve(mvlist)  # return inverse from cache

