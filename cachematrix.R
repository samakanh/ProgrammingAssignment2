## This function will create a special matrix (object), which is containing 
## a list of function to get/set the value of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        imatrix <- NULL
        set <- function(m) {
                x <<- m
                imatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse_matrix) {
                imatrix <<- inverse_matrix
        }
        getInverse <- function() imatrix
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}

## This function calculates the inverse of the special matrix. Before
## calculation, it will determine whether the inverse already calculated.
## If the inverse already calculated, it will retreive from the cache.
## Otherwise, it calculates the inverse of the special matrix and set to cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$getInverse()
        if(!is.null(inverse_matrix)) {
                message("Getting cached data")
                return(inverse_matrix)
        }
        matrix <- x$get()
        inverse_matrix <- solve(matrix, ...)
        x$setInverse(inverse_matrix)
        inverse_matrix
}
