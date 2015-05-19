## Cache the inverse of a matrix so do not need to repeatedly compute it
## as matrix conversion can be a costly computation.
## The first function stores the function/values and the 
## second function calculates the inverse of the matrix from the first function.

## example of call to functions
##      d<-makeCacheMatrix(matrix(1:4,2,2))
##      cacheSolve(d)
##      when you run cacheSolve(d) a second time in the same session, you
##      will get message "getting cached data" prior to the result

## sets the value of a matrix
## gets the value of a matrix
## sets the value of the inverse
## gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
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


## checks to see if the inverse has already been calculcated
## if it has, gets the inverse from the cache, otherwise,
## calculates it and sets the value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
                
}
