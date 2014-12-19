# This R code computes Inverse of a Square Matrix by making use
# of cache and leaxical scoping operator in R (<<-).
# 
# It is beneficial to use this method to if inverse of a 
# given matrix needs to be calculated over and over again.
#
# This code consists of two functions, namely: 
# - cacheSolve
# - makeCacheMatrix

## Write a short comment describing this function

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


## Write a short comment describing this function

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
