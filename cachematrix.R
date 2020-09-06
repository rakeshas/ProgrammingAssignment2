## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    datamatrix <- NULL
    setmatrix <- function(y){
        x <<- y
        datamatrix <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(solve) datamatrix <<-solve
    getinverse <- function() datamatrix
    list(setmatrix = setmatrix,
         getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        datamatrix <- x$getinverse()
        if (!is.null(datamatrix)){
            message (" getting cached data")
            return(datamatrix)
        }
        data1 <- x$getmatrix()
        datamatrix <- solve(data1, ...)
        x$setinverse (datamatrix)
        datamatrix
}
