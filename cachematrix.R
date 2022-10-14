## Function makeCacheMatrix to store input matrix and inverse matrix values
## and a second function cacheSolve to return inverse matrix in case it
## exists on makeCacheMatrix, or to calculate it and store it in case it
## doesn't exist

## Store input matrix and inverse matrix values

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(get=get,setinverse=setinverse,getinverse=getinverse)
}


## Calculate and store matrix in case it doesn't exist in makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)){
        message('Retrieving existing inverse cached')
        return(inverse)
    }
    matriz <- x$get()
    inverse <- solve(matriz)
    x$setinverse(inverse)
    inverse
}
