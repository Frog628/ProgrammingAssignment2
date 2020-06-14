## makeCacheMatrix makes a list of four functions based on a matrix.(ex: mymat)
## Store this list of functions in a variable. 
## (ex: mymatfunc <- makeCacheMatrix(mymat))
## This item (mymatfunc) will be the input for cacheSolve.
## The input to makeCacheMatrix must be an invertible matrix. 
## (An input of matrix with no inverse will make the list of four functions,
## but will cause an error when you run cacheSolve on that list.)
## To change the base matrix to another (ex: mymat2), without keeping the
## inverse of the original (mymat), just use mymatfunc$set(mymat2).
## To make another list of functions to use with another matrix (mymat2) 
## while keeping the functions working for the first matrix(mymat),
## make another list that can be input to cacheSolve
## (ex: mymat2func <- makeCacheMatrix(mymat2))

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setinv <- function(result) inv_mat <<- result
        getinv <- function() inv_mat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function takes the list of functions from makeCacheMatrix.
## The output is the inverse of the current invertible matrix.
## The current invertible matrix is either the one first sent to 
## makeCacheMatrix, or a replacement matrix that used the 
## set function inside the list of functions we will send to cacheSolve.
## The current value of the inverse is found (using the $getinv)
## If this is null, the actual matrix is put into a temporary variable, data,
## with $get. Then the inverse is calculated. That value is stored with the 
## setinv function so that it will be there the next time we look.
## If the current value of the inverse is a stored vaule that is not null, 
## that value is returned with a message that a cached value is being used. 
##

cacheSolve <- function(x, ...) {
        inv_mat <- x$getinv()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data, ...)
        x$setinv(inv_mat)
        inv_mat
}

