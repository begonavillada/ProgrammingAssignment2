## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

m <- NULL
  set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then
## the cacheSolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	 message("x is a ", class(x))
         message("x has size ", nrow(x), " by " , ncol(x))
         message(summary(x))
         m <- x$getinverse()
            if(!is.null(m)){  
                message("getting cached inverse matrix")
                return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
}
