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
        ## Loads inverse matrix if already stored in makeCacheMatrix object
        inv <- x$getInverse()
        ## Tests to see if an inverse matrix was present, loads it, and displays
        ## message stating it's being loaded 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Loads matrix stored in makeCacheMatrix object
        data <- x$get()
        ## Computes the inverse of the supplied matrix
        inv <- solve(data, ...)
        ## Stores the computed matrix in the makeCacheMatrix object
        x$setInverse(inv)
        ## Returns the matrix
        inv
}
