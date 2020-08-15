#A pair of functions that cache the inverse of a matrix

#Creates a special matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	ans<-NULL
	
	set<-function(y){
		x<<-y
		#Assign the function argument "y" to variable "x"
		#Variable "x" is originally in a different environment
		ans<<-NULL
	}
	get<-function()x
	
	setinverse<-function(inverse)ans<<-inverse
	getinverse<-function()ans

	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


#Computes the inverse of the special matrix returned by makeCacheMatrix above
#Retrieve the inverse directly from the cache if the "matrix" has already been computed

cacheSolve <- function(x, ...) {
	ans<-x$getinverse()

	if(!is.null(ans)){
		message("getting cached data")
		return(ans)
	}

	data<-x$get()
	ans<-solve(data,...)
	x$setinverse(ans)

	ans
}
