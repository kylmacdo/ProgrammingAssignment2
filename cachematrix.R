## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse_m <- NULL
        set <- function(y) {
                x <<- y
                inverse_m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse_m <<- solve
        getinverse <- function() inverse_m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
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