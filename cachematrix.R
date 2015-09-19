## Hi there I am Nanda.
##So,the function 'makeCacheMatrix' helps set and retrieve values for the
##input matrix and its inverse.

## Unlike the example provided,I have calculated
##the inverse and not just set it to a value.

makeCacheMatrix<-function(x = matrix()){
  inv_mtrx <- matrix()
  set<-function(y = matrix()){
    x <<- y
    inv_mtrx <<- NULL
  }
  get<-function(){
    x
  }
  setinv <- function(){
    inv_mtrx <<- solve(x,matrix(rep(1,nrow(x)*ncol(x)),nrow(x),ncol(x)))
  }
  getinv <- function(){
    inv_mtrx
  }
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


##The function 'cacheSolve' retrieves
##the inverse if not null,otherwise calculates and returns it.

cacheSolve<-function(z1,...){
  invmtrx <- z1$getinv()
  if(!is.null(invmtrx)){
    print("Fetching matrix inverse:")
    return(invmtrx)
  }
  else{
    invmtrx <- z1$setinv()
  }
  invmtrx
}