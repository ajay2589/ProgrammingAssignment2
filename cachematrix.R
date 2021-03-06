# This R code computes Inverse of an Invertible Square Matrix by making use
# of cache and leaxical scoping operator in R (<<-).
# 
# It is beneficial to use this method to if inverse of a 
# given matrix needs to be calculated over and over again.
#
# This code consists of two functions, namely: 
# - cacheSolve
# - makeCacheMatrix


# The makeCacheMatrix function serves as the cache for storing the 
# inverse of a square matrix. It accepts a matrix and returns a list 
# object consisting of the following components:
#  - a function to set the vaule of the matrix
#  - a function to get the value of the matrix
#  - a function to get the inverse of the matrix
#  - a function to set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# The cacheSolve function checks if the inverse of the matrix is already 
# computed. If yes, it returns the inverse, else it computes the inverse 
# of the matrix using solve method, stores it and returns the inverse. It 
# accepts an object of type returned by makeCacheMatrix as input and returns 
# the inverse of a matrix as the output.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}
