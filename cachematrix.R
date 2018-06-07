## makeCacheMatrix and cacheSolve are helper functions used to cache the 
## inverse of an input matrix so that the calculation of the inverse (which 
## can be resource-intensive) can be skipped if we have to calculate the 
## same inverse more than once.

## makeCacheMatrix takes the input matrix and creates a list object that 
## contains references to the functions that set and retrieve the input 
## matrix and its inverse (setinv/getinv)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes a makeCacheMatrix object, determines if an inverse has
## already been calculated, and either returns the stored value or forces
## calculation of the matrix inverse and then stores it.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)){
        message("Retrieving stored inverse ...")
        return(i)
    }
    in_mat <- x$get()
    i <- solve(in_mat)
    x$setinv(i)
    i
}
