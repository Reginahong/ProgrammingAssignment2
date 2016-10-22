## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation
## There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## The following two functions are used to cache the inverse of a matrix

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
                }
        get <- function () x
        setinverse <- function (inverse) inv <<- inverse
        getinverse <- function () inv
        list (set = set,
              get = get,
              setinverse = setinverse,
              getinverse = getinverse)               
}


## The following function returns the inverse of the matrix.
## It first checks if the inverse has already been computed.
## If so, it gets the result and skips the computation.
## If not, it computes the inverse, sets the value in the cache via setinverse function.

## This function assumes that the matrix is always invertible.
                
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse ()
        if (!is.null(inv)) {
                message ("getting cached data")
                return(inv)
             }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}

## Sample run
## > matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## > matrix$get()
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > matrix$getinverse()
## NULL

## No cache in the first run
## > cacheSolve(matrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Retrieving from the cache in the second run 
## > cacheSolve(matrix)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

