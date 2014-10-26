## makeCacheMatrix creates a special matrix object that can cache its inverse
## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache

## Takes in a matrix and creates a special object from it that can cache its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
        ## clears out the matrix 'inv' that stores the inverse
        inv <- NULL
        
        ## when called, sets a new matrix and clears out 'inv'
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## when called, recalls matrix
        get <- function() x
        ## when called, sets the matrix inverse based on input
        setInverse <- function(inverse) inv <<- inverse
        ## when called, recalls inverse of matrix
        getInverse <- function() inv
        ## 
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## Computes the inverse of the matrix stored by makeCacheMatrix. If the inverse
## has already been computed, it should retrieve the cached matrix inverse 
## instead of computing it again. 

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
