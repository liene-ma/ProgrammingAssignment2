## Put comments here that give an overall description of what your
## functions do - MakeCacheMatrix will create a special matrix object that can cache an inverse.


## Write a short comment describing this function - we imply that matrix is invertible,
## set the value of the matrix to y using another function, double arrow assignment 
## is useful it is a function written by another function, allow to have parallel 
## arguments.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
## > mymatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
##> mymatrix$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> mymatrix$getInverse()
##NULL
##> cacheSolve(mymatrix)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
>