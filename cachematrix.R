## This assignment is divided into two different steps, 
## each of which is a different function. The first function
## creates an object that will contain a matrix and its inverse.
## The second function basically accesses an argument returned by
## the first function and retrieves the inverse from the cahed 
## value stored in the first function's environment

## This first function creates a list that contains a function 
## that will (1) set the value of the matrix,  (2) get the value 
## of the matrix, (3) set the value of the inverse, and (4) get the value
## of the inverse. SO basically, if a create an object by using the function
## with a specific matrix as argument, set will allow me to change the
## values of the matrix, get will give me the values of the specified matrix
## and getinv will return NULL as long as I do not use the second function. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
        
}


## This second function checks whether the inverse has already been computed
## and cache stored, and if not, retrieve the matrix from the first function
## and compute the inverse. After that, it sill use the setinv function of 
## the first function and will store the newly computed inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
## Return a matrix that is the inverse of 'x'

## A little example:

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow=2, ncol=2)     # We first create a Matrix

myMatrixObject <- makeCacheMatrix(m1)                   # We then use the matrix as
# argument in the first function to create the set, get, setinv and getinv functions

cacheSolve(myMatrixObject)                              # FInally, we compute the 
# inverse of m1 and store it.