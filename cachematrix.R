## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#The first function, makeCacheMatrix creates a special "matrix" object  
#which is really a list containing a function to
# set the value of the matrix = input 
# get the value of the matrix = input 
# set the value of the inverse matrix 
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" instantiated after 
## calling the makeCacheMatrix function above. If the inverse has already been  
## calculated once, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' or take the cached inverse matrix
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

###
###Examples of the function test
###
# > m2
# [,1]       [,2]      [,3]
# [1,] -1.1488569 -1.8109773  1.571623
# [2,] -0.8636319  0.6955867  0.728342
# [3,] -0.2123694  0.7080289 -0.123064
# > source("cachematrix.R")
# > tt <- makeCacheMatrix(m2)
# > tt$get()
# [,1]       [,2]      [,3]
# [1,] -1.1488569 -1.8109773  1.571623
# [2,] -0.8636319  0.6955867  0.728342
# [3,] -0.2123694  0.7080289 -0.123064
# > tt$getinverse()
# NULL
# > cacheSolve(tt)
# [,1]     [,2]      [,3]
# [1,] -1.3837414 2.047894 -5.551203
# [2,] -0.6005441 1.093453 -1.197922
# [3,] -1.0672358 2.756994 -5.438293
# > cacheSolve(tt)
# getting cached data
# [,1]     [,2]      [,3]
# [1,] -1.3837414 2.047894 -5.551203
# [2,] -0.6005441 1.093453 -1.197922
# [3,] -1.0672358 2.756994 -5.438293
# > tt$getinverse()
# [,1]     [,2]      [,3]
# [1,] -1.3837414 2.047894 -5.551203
# [2,] -0.6005441 1.093453 -1.197922
# [3,] -1.0672358 2.756994 -5.438293

