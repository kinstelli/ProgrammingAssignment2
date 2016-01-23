## There are two functions here for setting a matrix,  finding its inverse, and caching that inverse

## * function makeCacheMatrix()  
##		param:	 a standard matrix
##		returns: a 'special' matrix, complete with four functions...
##			getMatrix() and setMatrix() -- these get and set the value of the matrix itself, respectively
##			getInverse() and setInverse() -- these get and set the inverse of the matrix, computed externally and cached 

makeCacheMatrix <- function(x = matrix()) {

	cachedInverse <- NULL				# initialize the cached inverse of the matrix to NULL
	setMatrix <- function(theMatrix)	# this function sets the value of the matrix to our special matrix, x
	{
		x <<- theMatrix				
		cachedInverse <<- NULL 			# and since we just set x to a new values, so we must erase this cached inverse
	}     

	getMatrix <- function()				# this simply returns the value of the matrix itself
	{
		 x	#return x
	}

	setInverse <- function(computedInverse) # this allows us to set a cached inverse of the matrix in x
	{
		cachedInverse <<- computedInverse 	#note that this is setting it to the scoped cachedInverse
	}

	getInverse <- function()			#returns the cached inverse of x...whether or not its NULL
	{
	 	cachedInverse 					# return this cachedInverse
	}

	# finally, we return this special matrix: its matrix value, plus this list of functions (and their environments)
	list( 	setMatrix = setMatrix, 
					getMatrix = getMatrix,
					setInverse = setInverse,
					getInverse =  getInverse )
}

## function cacheSolve()
##		this function can compute and cache the inverse of a 'special' matrix created by makeCacheMatrix()
##		param: 		accepts a matrix (assumed to be invertible)
##		returns:	an inverse of the matrix in param, 
##		post-condition: the given param has its inverse cached 

cacheSolve <- function(x, ...) {
        if(!is.null(x$getInverse() )) 			# if the inverse isn't null
        {
        	message('We are using the cached value!')
        	return(x$getInverse()) 				# then it must have been cached, so let's return that directly
        }
        										#otherwise....
        message('There is no cached value!')

        someMatrix <- x$getMatrix()				# get the current matrix associated with x
        newInverse <- solve(someMatrix) 		# set newInverse to be the value from solve(), which is supposed to return the inverse of a matrix

        x$setInverse(newInverse) 				# set this value to the environment in x, so it can be cached
        newInverse 								# return this new value
}









