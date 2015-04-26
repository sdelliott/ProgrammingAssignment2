## Coursera R Programming Assignment 2
## Author: Scott Elliott

## SUMMARY 
## These functions are used to take a square invertible matrix object, 
## calculate the inverse, then store the inverted matrix in a cache
## for quick retreival without having to recalculate the inverse.


## makeCacheMatrix function accepts a matrix object as an argument and 
## uses it to create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function will first check to see if the inverse of the special
## matrix returned by makeCacheMatrix has been cached, if so simply returning 
## that value. 
## If it has not yet been cached, it will compute the inverse and cache it.

cacheSolve <- function(x, ...) {
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
