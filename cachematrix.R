## These two functions when used together retrieve the inverse of a matrix
## stored in cache (if it exists), else they compute it and save it to cache



# creates a special vector object that can cache its inverse
# coerce input to function into a matrix
# initializes list m with a null object in this function
# get: returns matrix x provided as input to makeCacheMatrix()
# set: initializes m variable to a NULL object in parent environment (m stores the inverse matrix later)
# setinverse is called from within cacheSolve()
#            assigns the inverse matrix input to this function to variable m (in parent environment)
# getinverse  retrieves (cached) inverse matrix
makeCacheMatrix <- function(x = matrix()){
        message("x is a ", class(x))
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

## cacheSolve
## takes as input the special "vector" object returned by makeCacheMatrix
## if the inverse has already been calculated (and the matrix has not changed), then
## cacheSolve retrieves the inverse from the cache, else inverse is calculated and stored in cache
cacheSolve <- function(x, ...){
        message("x is a ", class(x))
        message("x has size", length(x))
        message(summary(x))
            m <- x$getinverse()
            if(!is.null(m)){  
                message("getting cached inverse matrix")
                return(m)
            }
            message("inverse matrix not available, get matrix, compute inverse matrix")
            data <- x$get()
            m <- solve(data, ...)
            # store inverse matrix in list argument x$setinverse and return inverse matrix
            x$setinverse(m)
            m
        }
