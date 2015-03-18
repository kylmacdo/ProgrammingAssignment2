## ***********************************************************************
##
## This set of functions serve to create a cache which can store the 
## mathematical inverse of a matrix.
##
## ***********************************************************************



makeCacheMatrix <- function(x = matrix()) {
        ## Creates a cache to store the value of the inverse of a matrix
        ## and a set of functions to interact with it
        ##
        ## Args:
        ## x: Matrix to be cached.  Will be coerced to a matrix. 
        ##    should be a square matrix
        ##
        ## Returns:
        ##  A list of functions to interact with the cache
        ##
        ## Child Functions:
        ##  set, get, setinverse, getinverse
        
        x <- as.matrix(x)
        ## Error handling, if matrix isn't square, generate a warning but
        ## continue execution.
        if(dim(x)[1] != dim(x)[2]) {
                warning("Matrix is not square, it cannot be inversed")
        }
        
        inverse_m <- NULL
        set <- function(y = matrix()) {
                ## API which allows the parent environment matrix x to be
                ## set to any matrix
                ## 
                ## Args:
                ## y: Matrix to be set to the cache.  Will be coerced to a 
                ##    matrix.  Should be a square matrix
                ##
                ## Returns:
                ##  None
                
                ## Error handling, if matrix isn't square, generate a 
                ## warning but continue execution.
                if(dim(x)[1] != dim(x)[2]) {
                        warning("Matrix is not square, it cannot be inversed")
                }
                                
                x <<- as.matrix(y)
                inverse_m <<- NULL
        }
        get <- function() x
        ## API which returns the matrix in the parent environment
        ## 
        ## Args:
        ## None 
        ##   
        ## Returns:
        ##  Matrix x from parent environment
        
        setinverse <- function(solve) inverse_m <<- solve
        ## API which allows the cached inverse matrix to be set
        ## 
        ## Args:
        ## solve: inverse matrix
        ##   
        ## Returns:
        ##  None
        
        getinverse <- function() inverse_m
        ## API which returns the cached inverse matrix 
        ## 
        ## Args:
        ##  None
        ##   
        ## Returns:
        ##  Parent environment cached inverse matrix
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
        ## Accesses the cached inverse matrix.  If it exists, return 
        ## the inverse matrix.  If it doesn't exist, calculate and return it
        ##
        ## Args:
        ## x: environment variable created using makeCacheMatrix     
        ## ...: additional arguments to be passed to solve if desired
        ##
        ## Returns:
        ##  Inverse matrix for the matrix stored in makeCacheMatrix env.
        ##

        
        ## Return a matrix that is the inverse of 'x'
        inverse_m <- x$getinverse()
        if(!is.null(inverse_m)) {
                message("getting cached data")
                return(inverse_m)
        }
        data <- x$get()
        inverse_m <- solve(data, ...)
        x$setinverse(inverse_m)
        inverse_m
}

runtest <- function(x){
        ## Function to test the operation of the functions in this file
        ## displays to the screen the inverse matrix and cached inverse matrix
        ## 
        ##
        ## Args:
        ## x: matrix to be used for testing    
        ##
        ## Returns:
        ##  None
        ##

        x_Matrix<-makeCacheMatrix(x)
        print(x_Matrix)
        print("************ Pass 1 **************")
        print(cacheSolve(x_Matrix))
        print("************ Pass 2 **************")
        print(cacheSolve(x_Matrix))
        print("************ Double matrix *******")
        x_Matrix$set(x*2)
        print(x_Matrix$get())
        
        print("************ Pass 3 **************")
        print(cacheSolve(x_Matrix))
        print("************ Pass 2 **************")
        print(cacheSolve(x_Matrix))
}
