## The cachematrix.R file provides two functions which computes and caches matrix
## and its inverse.




## makeCacheMatrix method takes in a matrix and provides an ability to cache matrix
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## set inverse matrix to null
        inversedMatrix <- NULL
        
        ##set function to reset value of x
        set <- function(y) {
                x <<- y
                inversedMatrix <<- NULL
        }
        
        #returns value of matrix
        get <- function() x
        
        ##set inverse (cache)
        setInverseMatrix <- function(matrix) inversedMatrix <<- matrix
        
        ##return inverse matrix
        getInverseMatrix  <- function() inversedMatrix
        
        ##list of all functions
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}



## cacheSolve function returns cached inverse of a matrix if available else
## computes inverse of the matrix and caches the value.

cacheSolve <- function(x, ...) {
        
        ##get the matrix inversion
        im <- x$getInverseMatrix()
        ##check if value is not null
        ##if not return cached data
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        ##get the matrix
        matrix <- x$get()
        
        ##Check if matrix is null then return
        if(is.null(matrix))
        {
                message("matrix is null. Please provide a matrix")
                return(im)
        }
        ##compute inverse
        im <- solve(matrix,...)
        ##cache the value
        x$setInverseMatrix(im)
        ## Return a matrix that is the inverse of 'x'
        im
        
}
