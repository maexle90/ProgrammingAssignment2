## The following function "makeCacheMatrix" creates a special "matrix" object
## and calculates the inverse of this matrix and caches this inverse matrix.
## This function also generates the according special "matrix" by typing 
## the command 'x$get()'. <--See example at the end.

## The next time one attempts to calculate the matrix inverse of the special
## "matrix", the previously saved value (in the function makeCacheMatrix) is
## returned instead of repeating the calculation, but only if the inverse 
## matrix has already been calculated and the matrix has not changed.  
## The function "cacheSolve" will then just retrieve the inverse matrix from
## the cache. 


## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {  
        ## initializing a function with just one argument: creating a matrix object 'x' 
        m <- NULL  ## the local variable 'm' is set to NULL
        
        set <- function(y) {
                ## function 'set' resets 'm' to NULL and sets the cached x value 
                ## to the passed in matrix
                x <<- y 
                ## assigns the matrix 'y' to the variable 'x' in the parent environment
                m <<- NULL 
        }
        
        get <- function() x ## return the matrix 'x'
        
        setinverse <- function(inverse) m <<- inverse 
        ## this function sets the cache 'm' equal to the inverse matrix 'x'
        getinverse <- function() m 
        ## this function returns the cached inverse matrix 'x'
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse() 
        ## this sets the local 'm' vector to the makeCacheMatrix's inverse matrix
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        ## if 'm' is not null, it will print the following message and returns 'm', 
        ## thus the inverse matrix. Then, the programm will exit. 
        }
        
        data <- x$get() 
        ## if 'm' is null, the function obtains the matrix from the makeCacheMatrix 
        ## object x and assigns it to the variable 'data'
        m <- solve(data, ...) 
        ## calculates the inverse matrix of 'data' via the 'solve()' command 
        x$setinverse(m) ## assigns the inverse matrix 'm' to the makeCacheMatrix inverse.
        m ## returns the inverse matrix 'm'
}


## Example 

m <- matrix(c(-1, -2, 1, 1), 2,2)
x <- makeCacheMatrix(m)
x$get()

inv <- cacheSolve(x)
inv
