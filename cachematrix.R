## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function given below creates a special "makeCacheMatrix" object that can cache the inverse of a matrix rather than computing repeatedly.

makeCacheMatrix <- function(x = matrix()) {      ## making a cache matrix
    datamatrix <- NULL     ## initialize the cache matrix 'datamatrix' with NULL value
    setmatrix <- function(y){    ## defining the method 'setmatrix' to set the value of the matrix
        x <<- y
        datamatrix <<- NULL
    }
    getmatrix <- function() x     ## defining the method 'getmatrix' to get the value of the matrix
    setinverse <- function(solve) datamatrix <<-solve     ## defining the method 'setinverse' to set the value of the inverse
    getinverse <- function() datamatrix   ## defining the method 'getinverse' to get the value of the inverse
    list(setmatrix = setmatrix,     ## list of names of all methods defined above
         getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.Computing the inverse of a square matrix can be done with the solve function.

cacheSolve <- function(x, ...) {    ## defined function to find the inverse of the matrix using 'cacheSolve'
        ## Return a matrix that is the inverse of 'x'
        datamatrix <- x$getinverse()  ## checking the data in the 'datamatrix'
        if (!is.null(datamatrix)){   ## if the 'datamatrix' is not NULL, then return the result stored as cache 
            message (" getting cached data")
            return(datamatrix)
        }
        data1 <- x$getmatrix()   ## if the 'datamatrix' is NULL, then get the value of matrix, set the value matrix, set the value of inverse, get the value of inverse and return the inverse fo the matrix
        datamatrix <- solve(data1, ...)
        x$setinverse (datamatrix)
        datamatrix
}
