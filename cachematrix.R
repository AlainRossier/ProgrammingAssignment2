## The goal here is to compute the inverse of an invertible square matrix.
## We also want to store it somewhere, so we don't have to compute the
## inverse more than once, which can be useful. Indeed, for large matrices,
## computing its inverse can take a lot of space and time.

## This function creates a new object, which consists of 4 functions that 
## can set and access to the value of a given matrix which is stocked outside 
## its environnement, the same for its inverse.

makeCacheMatrix <- function(m = matrix()) {
	x<-NULL
	set<-function(y) {
		m<<-y
		x<<-NULL
	}
	get<-function() {m}
	setinv<-function(inv) {x<<-inv}
	getinv<-function() {x}
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function actually inverts a given matrix. The argument is this object
## we've defined before, so we can check if the inverse of this matrix have been
## already computed. In that case, the function the inverse and prints "getting
## cached data". Otherwise, it simply computes the inverse of the matrix and stores
## the inverse outside this environnement, so we can access to it later on.

cacheSolve <- function(m, ...) {
## Return a matrix that is the inverse of 'm'
	x<-m$getinv()
	if(!is.null(x)) {
		message("getting cached data")
		return(x)
		}
	data<-m$get()
	x<-solve(data)
	m$setinv(x)
	x
}
