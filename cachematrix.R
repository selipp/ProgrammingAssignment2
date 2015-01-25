## This script contains two main functions.

## - One to create a a containter object which takes a matrix as input.
##   The container object is used to
##   - store the input matrix itself
##   - as well as the inversed matrix once it has been calculated.
##   The funtion returns a list of four fuctions:
##   - get: Is used to return the matrix.
##   - set: Stores a new matrix in the container object. Since the inversed
##          matrix for the new value will most likely differ from the inversed
##          matrix if the old value i (which contains the inversed matrix will
##          be set to NULL. This makes sure that solve() has to be executed to
##          get the inversed matrix of our new value.
##   - setinverse: Stores the inversed matrix.
##   - getinverse: Returns the inversed matrix.
##   Since it ist stored in the container object the inversed matrix doesn't
##   have to be newly calculated everytime it is needed  but can be stored
##   from the container.

## - The second function returns the inversed matrix. Therefore it
##   does either calculate the values and immediately store it in the container
##   object or - if it has already been calculated and stored before - it just
##   reads the value from the container object and returns it.


## makeCacheMatrix
## Created and returns a list which is used as a container object to store a
## matrix and its inverse. For more details see the comment above and the
## inline comments in the function.
makeCacheMatrix <- function(x = matrix()) {
    # This function creates a special "matrix" object that can cache its inverse. 
    i <- NULL # i refers to the inversed matrix
    set <- function(y = matrix) {
        ## function takes a matri as argument and stores it in the x variable of the
        ## parent function makeCacheMatrix.
        ## Furthermore it resets its parent's i variable which refers to the inversed
        ## matrix to NULL. That means once a new matrix is stored the inversed matrix
        ## has to be calculated again.
        x <<- y 
        i <<- NULL
    }
    get <- function() x # Returns the matrix
    setinverse <- function(solve) i <<- solve
    ## Sets the value of the inversed function.
    ## Be careful here:
    ## Same as I assume that the matrix supplied is always invertible I do also assume that
    ## cached_matrix$setinverse is only called by the cacheSolve function.
    ## Otherwise I would cange the previous line (setinverse <- function...) into the following
    ## lines to make sure that when a there is a value assigned to i this value always fits
    ## together wirth my matrix x. Anyway for this assignment I assumed that we do not want to
    ## change our matrix x that way:
    ## setinverse <- function(solve) {
    ##     i <<- solve
    ##     x <<- solve(i)
    ## }
    getinverse <- function() i # Returns the inversed matrix.

    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)      
    # Returns a list the four functions.
}


## cacheSolve
## Reads the inversed matrix from the container object.
## If the container doesn't provide that information yet,
## the inversed matrix is calculated and stored in the container
## object. Returns the inversed matrix.
## For more details see the comment above and the
## inline comments in the function.
cacheSolve <- function(x, ...) {

    i <- x$getinverse() # i is set to the inversed matrix stored
                        # in the container object
    # since there might be no inversed matrix provided in the container
    # object yet we have to check whether i set (not NULL).
    if(!is.null(i)) {   
        # If that's the case we just return i along with a short message.
        message("getting cached data")
        return(i)
    }
    # If i hast been NULL in the previous check we arrive here. We store
    # our matrix in a variable called data and store it's inversed version
    # in i. We do also store it in the container object calling its
    # setinverse function and return i in the end.
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

