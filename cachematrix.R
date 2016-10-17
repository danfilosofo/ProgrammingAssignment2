## Put comments here that give an overall description of what your
## functions do

## This function will create a list (special 'matrix') that has four functions:
## set/get the value of matrix and set/get the inverse of the cached matrix

makeCacheMatrix <- function(m = matrix()) {
        im <- NULL
        set <- function(y) {
                m <<- y
                im <<- NULL
        }
        get <- function() m
        setinverse <- function(inversem) im <<- inversem
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will return the cached inverse matrix if it isn't null, if so
## then the function will be set the inverse matrix and then returns it.

cacheSolve <- function(m = makeCacheMatrix(matrix(NA, 2,2)), ...) {
        ## Return a matrix that is the inverse of 'm'
        im <- m$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- m$get()
        im <- solve(data)
        m$setinverse(im)
        im
}