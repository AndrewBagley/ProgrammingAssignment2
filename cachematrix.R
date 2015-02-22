## https://github.com/AndrewBagley/ProgrammingAssignment2/tree/Edit

##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 

##Your assignment is to write a pair of functions that cache the inverse of a matrix.
##For this assignment, assume that the matrix supplied is always invertible.

## reference: http://masterr.org/r/how-to-cache-a-matrix-inversion-in-r/

## makeCacheMatrix: This function creates a special [square] "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
    inv <- x$getinverse()
    if(!is.null(inv)) {   ##Check for existing data
        message("retrieving cached data")
        return(inv)
    } ##calculate the inverse when no data already exists
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

##Testing:
##> x = rbind(c(1, 2), c(3, 4))
##> matrix = makeCacheMatrix(x)
##> matrix$get()
##     [,1] [,2]
##[1,]    1    2
##[2,]    3    4
##> cacheSolve(matrix)
##     [,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5
##> cacheSolve(matrix)
##retrieving cached data
##     [,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5
