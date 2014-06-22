## Put comments here that give an overall description of what your
## functions do

##THE FUNCTION IS USE TO OBTAIN THE INVERSE OF THE MATRIX AND
## TO  CALL THE SAME FROM THE CACHE TO SAVE MEMORY

## Write a short comment describing this function

## makeCacheMatrix return a list of function fort: 
## 1. Set the value of the matrix
## 2. Get tje value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
}
	get<-function() x
	setinv<-function(inverse) m <<- inverse
	getinv<-function () m
		list(set = set, get = get,
			setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

##cacheSolve is use to calculate the inverse of the matrix, and if the inverse 
##already exist, it returns the cached inverse.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m<-x$getinv()
	if(!is.null(m)) {
		message("Getting cached data")
		return (m)
	}
	data<-x$get()
	m<-solve(data,...)
	x$setinv(m)
	m
}
