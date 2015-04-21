## Function makeChacheMatrix takes a matrix as an argument. 
## It has a set and get functions to get and set the matrix
## setInversed sets i to the inversed matrix
## getInversed gets inversed matrix
## makeChacheMatrix returns a list of the four functions.

makeCacheMatrix <- function(x = matrix()) {
        # instantiate variable to later hold inverse matrix
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


## cacheSolve checks if the matrix was already inversed.
## if yes, returns the inversed matrix from cache.
## if no, performs the inversion and caches it
## and returns the inversed matrix

cacheSolve <- function(x) {
        
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
        i <- solve(matrix)
        # caches the inversion 
        x$setInversed(i)
        
        ## Returns the inversed matrix
        i
}

# test code
matrix <- matrix(sample(1:200, 9, replace=T), nrow = 3, ncol = 3)
my_matrix <- makeCacheMatrix(matrix)
my_matrix$get( )

# cache inverse
cacheSolve( my_matrix ) 
# use cached matrix to get the identity matrix. just a contrived test case
cacheSolve( my_matrix ) %*% matrix 
