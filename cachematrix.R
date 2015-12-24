## The function creates the inverse of a inverse of a matrix and cache it.
## so instead of computing the inverse of that matrix again,
##it will be accessed from the cache.

## The function makeCachematrix creates a list containing a function to:
## a) set the value of a matrix
## b) get the value of a matrix
## c) set inverse of the matrix
## d) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function (y){
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setmatinv <- function(solve) minv <<- solve
    getmatinv <- function() minv
    list(set=set,get=get,setmatinv=setmatinv,getmatinv=getmatinv)
    
}


## The function below calculates the inverse of matrix "x".
## The calculation is done only after checking if inverse already exist or not.
## If inverse exist then it is accessed from cache avoiding computation.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getmatinv()
    if(!is.null(minv)){
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data,...)
    x$setmatinv(minv)
    minv
}
