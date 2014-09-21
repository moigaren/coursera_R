## These functions allows to cache the inverse of a matrix so it does not need
## to be calculated again if needed, thus saving time.

## This function, makeCacheMatrix, creates a list of functions to store a matrix
## 'x' and its inverse 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL 
        ## Set the value of variable 's' (the inverse) to NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        ## The function 'set' is used to create a new matrix, which substitutes 
        ## any previous matrix stored, and sets the value of 's' to NULL
        get <- function() x
        ## The function 'get' shows the matrix which is stored in cache
        setinverse <- function(solve) s <<- solve
        ## The function 'setinverse' calculates the inverse of the matrix and
        ## stores it in cache
        getinverse <- function() s
        ## The function 'getinverse' shows the inverse of the matrix which is
        ## stored in cache
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        ## Creates a list showing the four functions
}


## CacheSolve tries to look in stored to check whether the inverse of a certain
## matrix has been already calculated, thus saving time. Otherwise, calculates 
## the inverse.
cacheSolve <- function(x, ...) {
        s <- x$getinverse() 
        ## Tries to get the inverse of 'x' if calculated before and stored 
        ## in cache
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        # If not null, it means that 's' is stored in chache and returns 
        # the inverse of x
        data <- x$get()
        # If null, get data from 'x' in makeCacheMatrix
        s <- solve(data)
        # Calculates the inverse of the matrix
        x$setinverse(s)
        # Stores the new value of the inverse in cache
        s
        ## Return a matrix that is the inverse of 'x'
}
