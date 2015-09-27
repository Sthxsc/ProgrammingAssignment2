## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# # set          set the value of a matrix
# # get          get the value of a matrix
# # setInverse   set the inverse of the matrix
# # getInverse   get the inverse of the matrix)

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
    initial <- NULL
    
    ## Store the matrix
    set <- function(y) {
        x <<- y
        initial <<- NULL
    }
    
    ## Get the stored matrix
    get <- function() {
        x
    }
    
    ## Cache the argument 
    setInverse <- function(inverse) initial <<- inverse
    
    ## Get the cached value
    getInverse <- function() {
        initial
    }
    
    ## Return a list of functions. 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return the inversed matrix
    inverse <- x$getInverse()
    
    ## Return if a cached value exists 
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## Get the matrix, caclulate the inverse and store otherwise
    newdata <- x$get()
    inverse <- solve(newdata,...)
    x$setInverse(inverse)
    
    ## Get the inverse
    inverse
}

