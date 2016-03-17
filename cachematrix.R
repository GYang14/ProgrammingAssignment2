## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix creates a special "vector", which is a list containing a function to
##1.set the matrix
##2.get the matrix
##3.set the inversed matrix
##4.get the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  ##setting the inverse to NULL as a placeholder for a future value
  inv<-NULL
  ##Define the function to set the matrix
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  ##Define the function to get the matrix
  get<-function()x 
  ##Define the function to set the inversed matrix
  setinv<-function(inverse)inv<<-inverse
  ##Define the function to get the inversed matrix
  getinv<-function()inv
  ##Return the 'special vector' containing all of the functions
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #get the inverse of 'x'
        inv<-x$getinv()
        #if the inversed matrix is available then return the existed inverse
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        #if not,use x$get to get the matrix object, 
        #solve it and then use x$setinv to set it to the object  
        data <-x$get()
        inv<-solve(data)
        x$setinv(inv)
        #return inverse
        inv
}
