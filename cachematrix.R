## Functions cache the inverse of a matrix.

## makeCacheMatrix creates a "matrix" object that can cache its inverse (assumed
## that the matrix supplied is always invertible), and a list that contains a function to
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    setMat <- function(y){
        x <<- y
        invMat <<- NULL
    }
    getMat <- function() x
    setInv <- function(inverse) invMat <<-inverse
    getInv <- function() invMat
    list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix. If the 
## inverse has already been calculated (and the matrix has not changed), then the 
## cacheSolve retrieves the inverse from the cache. If not, it computes the inverse, 
## sets the value in the cache via setInv function.
cacheSolve <- function(x, ...){
    invMat <- x$getInv()
    if(!is.null(invMat)){
        message("getting cached data")
        return(invMat)
    }
    data <- x$getMat()
    invMat <- solve(data, ...)
    x$setInv(invMat)
    invMat
}