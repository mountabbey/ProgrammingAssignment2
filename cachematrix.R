# Coursera R Programming Assignment 2
# Bob Mitchell

# The honor code suggests that you don't use this code for your own assignment

# Matrix inversion is usually a costly computation 
# and there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly 
# (there are also alternatives to matrix inversion that we will not discuss here). 
#
# The assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# makeCacheMatrix: 
# This function creates a special "matrix" object that can cache its inverse.
#
# cacheSolve: 
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
#
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied is always invertible.
# 
# Notes: 
#
# A test file exists in test.R
#
# I try to follow Google's codings standards when possible
# https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml


makeCacheMatrix <- function(x = matrix()) {
    # creates a special matrix object that can cache its inverse
    #
    # Args:
    #   x: a matrix
    #
    # Returns:
    #   the matrix
    
    # data
    inverse <- NULL
    
    # methods
    set <- function(y) {
        #print("cacheMatrix set")
        x <<- y
        inverse <<- NULL  # clear cache
    }
    get <- function() x
    
    # getter/setter for the cached inverse
    # application should use cacheSolve
    setinverse <- function(inv) {
        inverse <<- inv
    }
    getinverse <- function() inverse
    
    # solve the inverse
    solve <- function(...) cacheSolve(obj, ...)

    # setup a new object
    obj = list(set=set,
            get=get,
            setinverse=setinverse,
            getinverse=getinverse,
            solve=solve)
    class(obj) = "cacheMatrix"
    
    # return the new list object
    obj
}


cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    # This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
    # If the inverse has already been calculated (and the matrix has not changed), 
    # then the cachesolve should retrieve the inverse from the cache.
    # If x is not a makeCacheMatrix, then the inverse is always computed
    #
    # Args:
    #   x: a matrix or makeCacheMatrix
    #   ...: other solve arguments
    # 
    # Returns:
    #   the inverse

    # check if our special matrix type
    if(class(x) == "cacheMatrix") {
        # see if the inverse has been set
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
            print("getting the cached inverse")
            return(inverse)
        }
        
        # see if x is a CacheMatrix 
        data <- x$get()
        if(!is.null(data)) {
            # solve it
            inverse <- solve(data, ...)
            
            # set the inverse cache
            x$setinverse(inverse)
            print("setting the cached inverse")
            return(inverse)
        }
    }
    
    # solve the inverse for a normal matrix
    inverse <- solve(x, ...)
    inverse
}

