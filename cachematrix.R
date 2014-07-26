## cacheMatrix.R
##
## Contains 2 functions.  The first, makeCacheMatrix(x), creates
## a matrix object with member values, and member values for its
## inverse.  The second, cacheSolve(x, ...), checks if a matrix 
## object has values for its inverse, then either retrieves those
## values or solves for them.
#################################################################### 



## function makeCacheMatrix(x) - a list of subfunctions (methods) to
## 1. set the values of a matrix
## 2. get the values of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
##
## IN :	matrix x - probably must be numeric to work
## OUT: the list of named (object) methods
## 
## METHODS:
## s(aMatrix) - stores the values from an input matrix
## g() - returns the values of the stored matrix
## setI(anInverse) - stores the inverse of the matrix
## getI() - returns the inverse of the stored matrix
##
## SAMPLE USE:
## myMatrix <- makeCacheMatrix( matrix(1:4,2,2) )
## myMatrix$set( 2*myMatrix$get() )
## myMatrix$setinverse( solve( myMatrix$get() ) )
####################################################################
makeCacheMatrix <- function(x = matrix()) {
	
	I <- NULL # initialize the inverse
	
	s <- function(aMatrix) {
		x <<- aMatrix # store matrix in parent frame
		I <<- NULL # initialize inverse in parent frame
	}

	g <- function() {
		x   #return the stored matrix value
	}

	setI <- function(theInverse) {
		I <<- theInverse # store the inverse of the matrix in parent frame
	}

	getI <- function() {
		I   #return the stored inverse of the matrix
	} 

	# return a list of the named subfunctions (methods)
	# allows us to access methods by myMatrix$methodName()
	list(set = s, get = g, setinverse = setI, getinverse = getI)
}




## function cacheSolve(x, ...) - sets and returns the inverse of a 
## matrix object
##
## IN :	list x - a matrix object created by the makeCacheMatrix() fcn 
## OUT:	matrix I - the inverse of the matrix contained in the input object
##
## Checks the input object for an inverse value.  If there is a 
## value, retrieve it from the object and return it.  If there isn't
## a value for the inverse, solve for it, bind it to the object, and
## return it.
##
## SAMPLE USE:
## myMatrixInverse <- cacheSolve( myMatrix )
## myMatrixInverse == myMatrix$getinverse()
####################################################################
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        I <- x$getinverse()

        #check if the inverse has already been solved
        if(!is.null(I)) {    #if it has, return the cached value (from object)
        	message("retrieving inverse from cache")
        	return(I)     #break function and return inverse
        }
        # else, if it hasn't, solve for the inverse 
        message("solving for inverse")
        myMatrix <- x$get() 	# retrieve the matrix
        I <- solve(myMatrix)  	# solve for the inverse
        x$setinverse(I)     	# bind the inverse to the matrix object
        I 		     	# return the inverse
}




makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}