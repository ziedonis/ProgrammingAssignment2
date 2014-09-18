## Programming Assignment 2
## AAZ, 9/17/2014

## This assignment is to write functions to calculate the inverse of a matrix 'x'
## using a cached value of the inverse if it has already been calculated.

## makeCacheMatrix is a function that creates a list containing four functions that:
## 1. set the value of the matrix 'x'
## 2. get the value of the matrix 'x'
## 3. set the value of the inverse 'x'
## 4. get the value of the inverse 'x'

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## 'CacheSolve' solves the inverse of a matrix 'x'.  If the inverse has already been
## calculated by the function 'makeCacheMatrix', then 'CacheSolve' uses the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
