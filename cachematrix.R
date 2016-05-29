# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

# makeCacheMatrix function is composed of other 4 functions:
        # 1. setMatrix: set a new square invertible matrix
        # 2. getMatrix: get the new square invertible matrix
        # 3. setMatrixInv: set the inverse of matrix
        # 4. getMatrixInv: get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        
        # Define the inverse outside the scope of other functions
        invMatrix <- NULL
 
        # 1. setMatrix       
        setMatrix <- function(y) {
                # '<<-' operator can be used to assign a value to an object in an environment 
                # that is different from the current environment
                x <<- y
                # Once setMatrix is new defined, the saved Inverse is not more useful. Therefore,
                # we have to empty it
                invMatrix <<- NULL
        }
        
        # 2. getMatrix 
        getMatrix <- function() x
        
        # 3. setMatrixInv(inverseMatrix = inverse of our square invertible matrix)
        # We pass it to our variable invMatrix, which was out of the scope of this function
        setMatrixInv <- function(inverseMatrix) invMatrix <<- inverseMatrix 
        
        # 4. getMatrixInv
        getMatrixInv = function() invMatrix
        
        # The output of the function is a list of 4 elements which will be used in our cacheSolve function
        list(setMatrix=setMatrix, getMatrix=getMatrix, setMatrixInv=setMatrixInv, getMatrixInv=getMatrixInv)
}



# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should 
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # x <- list(setMatrix=setMatrix, getMatrix=getMatrix, setMatrixInv=setMatrixInv, getMatrixInv=getMatrixInv)
        
        # Get content of getMatrixInv
        invCacheSolve <- x$getMatrixInv()
        
        # If invCacheSolve is not NULL, that means that it has already been calculated and, therefore,
        # we can obtain its cached value without extra calculation
        if (!is.null(invCacheSolve)){
                message("Inverse already calculated. Getting the cached data")
                return(invCacheSolve)
                # If the function reaches this, the function will stop running but delivering the cached inverse
        }
        
        # If the function reaches this, that means that the variable invCacheSolve was empty and, therefore,
        # R has to calculare it
        data = x$getMatrix()
        invCacheSolve = solve(data, ...)
        
        # Defines the inverse of the matrix using the function setMatrixInv defined in the scope
        # of our makeCacheMatrix() function
        x$setMatrixInv(invCacheSolve)
        
        # return the inverse of the original matrix to function makeCacheMatrix()
        return(invCacheSolve)
}

# How efficient is our cache? We have to test it and, for it, we write following function

testCache <- function(elements=250000) {
        
        options(scipen=999)
        
        # Get number of elements that the matrix should have
        givenElements <- elements

        # How many rows and columns should the square matrix contain to reach the number of 
        # givenElements? Square Root can help us to determinate
        rowsColumns <- sqrt(givenElements)
        
        # As the user can input a number of elements which cannot be converted in a square matrix,
        # we round the rowsColumns to get the closest, safe, achievable elements
        safeRowsColumns <- round(rowsColumns)
        
        # This step is needed to recalculate the elements the square matrix must have
        endElements <- safeRowsColumns^2
        
        # Output in console how many elements will our test matrix have        
        print(sprintf("Generating square invertible matrix with %s elements", endElements))

        # randomize endElements and create the matrix         
        rndNorm <- rnorm(endElements)
        testRMatrix <- matrix(rndNorm, nrow=safeRowsColumns, ncol=safeRowsColumns)

        # Call our makeCacheMatrix() function with the new created matrix
        cacheMatrix <- makeCacheMatrix(testRMatrix)

        # Make use of Sys.time() to simulate a stopwatch - Actual time is?       
        sTime = Sys.time()
        
        # Call our cacheSolve() function. As the inverse has not been calculated, it will require some time
        # Time runs, tick tack tick tack...
        cacheSolve(cacheMatrix)
 
        # Make use of Sys.time() to simulate a stopwatch - Actual time is? (cacheSolve() is ended)         
        eTime = Sys.time()
        timeSpent = eTime - sTime
        # Output time difference
        print(timeSpent)

        # Make use of Sys.time() to simulate a stopwatch - Actual time is?
        sTime = Sys.time()
        # Call our cacheSolve() function. As the inverse is cached, it will not require extra time
        cacheSolve(cacheMatrix)
        # Make use of Sys.time() to simulate a stopwatch - Actual time is? (cacheSolve() is ended)         
        eTime = Sys.time()
        timeSpent = eTime - sTime
        # Output time difference
        print(timeSpent)
        
        options(scipen=0)
}
