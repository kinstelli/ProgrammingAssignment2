## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	cachedInverse <- NULL		# initialize the cached inverse of the matrix to NULL

	#setMatrix
	setMatrix <- function(theMatrix)
	{
		x <<- theMatrix
		cachedInverse <<- NULL 		# we just set x to all new values, so we must erase this cached value

	}     

	#getMatrix
	getMatrix <- function()
	{
		 x	#return x
	}

	#setInverse
	setInverse <- function(computedInverse)
	{
		message("Now we set the cached inverse matrix to the newly computed inverse value.")
		cachedInverse <<- computedInverse 		#note that this is setting it to the retained value of x, by using <<-
	}

	#getInverse -- returns the cached inverse of x if it exists
	getInverse <- function()
	{
	 	cachedInverse # return this cachedInverse
	}

	#returns this special matrix: its matrix value, plus this list of functions (and their environments)
	list( 	setMatrix = setMatrix, 
					getMatrix = getMatrix,
					setInverse = setInverse,
					getInverse =  getInverse )
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(x$getInverse() )) # if the inverse isn't null
        {
        	message('We are using the cached value!')
        	return(x$getInverse()) # then it must be cached, so let's return that directly
        }
        #otherwise....
        message('There is no cached value!')

        someMatrix <- x$getMatrix()		# get the current matrix associated with x
        newInverse <- solve(someMatrix) # set newInverse to be the value from solve(), which is supposed to return the inverse of a matrix

        x$setInverse(newInverse) # set this value to the environment in x, so it can be cached
        newInverse # return this new value
}









