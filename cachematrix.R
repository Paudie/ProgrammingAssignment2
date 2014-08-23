## This is for programming assignment 2 for the R Programming
## Coursera course: https://class.coursera.org/rprog-006
## There are 2 functions as part of this assignment. The first 
## creates a Matrix object, the 2nd returning the inverse of that 
## matrix (from the cache if available)

## This function creates a special "matrix" object 
makeCacheMatrix <- function(x = matrix()) {
        
        ## Initiate i
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        
        ## Return the matrix object which is really list to containing a function to
        ## 1. set the value of the matrix
        ## 2. get the value of the matrix
        ## 3. solve the inverse value of the matrix
        ## 4. get the solved inverse value of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache. Otherwise, it goes ahead and solves the 
## inverse
cacheSolve <- function(x, ...) {
        ## initialise i from the cache or with null
        i <- x$getinverse()
        if(!is.null(i)) {
                ## print a message and return solution from the cache immediately
                message("getting cached data")
                return(i)
        }
        
        ## get the value of the matrix we now need to solve
        data <- x$get()
        
        ## print a message and solve the solution
        message("now solving the matrix")
        i <- solve(data, ...)
        
        ## put the solution into the cache for future reference
        message("caching the solution...")
        x$setinverse(i)
        
        ## Return the solution
        i
        
}
