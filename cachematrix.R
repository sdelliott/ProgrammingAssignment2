## Coursera R Programming Assignment 2
## Author: Scott Elliott

## SUMMARY 
## These functions are used to take a square invertible matrix object, 
## calculate the inverse, then store the inverted matrix in a cache
## for quick retreival without having to recalculate the inverse.


## makeCacheMatrix function accepts a matrix object as an argument and 
## uses it to create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL       # when initially creating the new matrix object, 
                        # this sets the stored value of the inverse "i" to NULL
        set <- function(y) {    # the SET function takes the user-provided 
                                # object or value and makes the 'special' matrix
                                # equal to it. Also sets the global variable "i"
                                # to NULL
                x <<- y
                i <<- NULL
        }
        get <- function() x     # the GET function simply returns the special
                                # matrix object
        setinverse <- function(inverse) i <<- inverse # the SETINVERSE function
                                                # manually sets the global 
                                                # inverse varaiable "i" to 
                                                # match a user-defined or passed
                                                # value.
        getinverse <- function() i      # the GETINVERSE function simply returns
                                        # the value of the global inverse 
                                        # variable "i", then lists the code of
                                        # these defined functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function will first check to see if the inverse of the special
## matrix returned by makeCacheMatrix has been cached, if so simply returning 
## that value. 
## If it has not yet been cached, it will compute the inverse, cache it and 
## return it.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()     # first check to see if the inverse has been 
                                # cached
        if(!is.null(i)) {       # if it has been cached, send a message that 
                                # it's coming from the cache and return the 
                                # stored inverse value
                message("getting cached data")
                return(i)
        }
        data <- x$get() # if "i" is NULL (i.e., the inverse is not cached), then
                        # call the GET function to load the matrix...
        i <- solve(data, ...)   # ...calculate the inverse...
        x$setinverse(i)         # and cache the value of the inverse
        i               # then return the value of the inverse 
}
