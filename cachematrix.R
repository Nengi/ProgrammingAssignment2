## setmatrix creates a matrix x
## getmatrix returns the matrix x
## setinverse calculates the inverse of the original matrix x
## getinverse returns the inverse of the original matrix x


## makeCacheMatrix is function to create, return and cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL
        oldmatrix <- NULL
        setmatrix <- function (d, n){
                
                x <<- matrix(d, nrow =n, ncol =n)
                inversematrix <<- NULL
              
        }
        getmatrix <- function() x 
        setinverse <- function(solve) inversematrix <<- solve 
        getinverse <- function() inversematrix
        list(set=setmatrix, get=getmatrix, setinv=setinverse, getinv=getinverse)      
}


## Calculate the inverse of a matrix or return the cached value if it has previously
## been calculated and the matrix has not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inversematrix <- x$getinv()
        
        if(!is.null(inversematrix)) {
              
                message("getting cached data")
                return(inversematrix)
                
        }
        
        data <- x$get()
        inversematrix <- solve(data, ...)
        x$setinv(inversematrix)
        inversematrix
}
