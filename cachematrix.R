## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # int inverse variable
        i <- NULL
        # set matrix        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # get matrix
        get <- function() {
                x
        }
        # set i to the inversed matrix
        setInversed <- function(inversed) {
                i <<- inversed
        }
        # get the inversed matrix
        getInversed <- function() {
                i
        }
        
        #return functionality list
        list( set = set, 
              get = get, 
              setInversed = setInversed, 
              getInversed = getInversed )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        # set i to inversed matrix
        i <- x$getInversed()
        
        # check if inversed is already calculated
        # if yes return value from cache and exit the function
        if( !is.null(i) ) {
                message("getting inversed matrix from cache")
                return(i)
        }
        
        # get matrix
        matrix <- x$get()
        # calculate inverse of the matrix
        i <- solve(matrix, ...)
        # set inversed to the calculated i
        x$setInversed(i)
        
        ## Return a matrix that is the inverse of 'x'
        i
}

# test code
matrix <- matrix(sample(1:200, 9, replace=T), nrow = 3, ncol = 3)
my_matrix <- makeCacheMatrix()
my_matrix$set( matrix )
my_matrix$get( )

# cache inverse
cacheSolve( my_matrix ) 
cacheSolve( my_matrix ) %*% matrix 