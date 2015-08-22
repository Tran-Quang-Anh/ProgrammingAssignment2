## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly.The below 2 functions are used to create a 
## special object that stores a numeric matrix and cache's its inverse. For these 2 functions to
## work, we assume that the matrix supplied is always invertible.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse, 
## which is really a list containing 4 functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inver_matrix <- NULL
    
    set_matrix <- function(y){
        x <<- y
        inver_matrix <<- NULL
    }
    
    get_matrix <- function(){
        x
    }
    
    set_inver <- function(temp_inverse){
        inver_matrix <<- temp_inverse
    }
    
    get_inverse <-function(){
        inver_matrix
    }
    
    list( set_matrix=set_matrix, get_matrix=get_matrix, set_inver=set_inver,get_inverse=get_inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If 
## the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Get a matrix that is the inverse of 'x' or may get NULL
    inver_matrix2 <- x$get_inverse()
    
    ## If the inversing matrix in cache is NOT NULL
    if( !is.null(inver_matrix2) ){
        message("Inverse matrix is withdrawed from cache")
        return(inver_matrix2)
    }
    
    ## If the inversing matrix in cache is NULL
    inver_matrix2 <- solve( x$get_matrix(), ... )
    x$set_inver(inver_matrix2)
    inver_matrix2
}
