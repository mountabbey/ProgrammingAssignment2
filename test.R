# Coursera R Programming Assignment 2
# Bob Mitchell

# Test function for the cacheMatrix assignment
# Not a proper unit test. Will look to add RUnit later
# To run a suite of code: test()

test <- function() {
    # Test cases for the cacheMatrix object
    
    m = matrix(c(1:4),nrow=2,ncol=2)

    # Call cacheSolve on a regular matrix
    print(cacheSolve(m))
    
    # create a CacheMatrix object
    cm = makeCacheMatrix(m)
    
    # solve
    print(cacheSolve(cm))
    
    # solve but gets the inverse from the cache
    print(cacheSolve(cm))

    # change the matrix
    m2 = matrix(c(5:8),nrow=2,ncol=2)
    print("changing matrix")
    cm$set(m2)
    
    # solve. the cache has been flused
    print(cacheSolve(cm))
    print("using cm$solve...")
    cm$solve()
}