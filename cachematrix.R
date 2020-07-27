

makeCacheMatrix <- function(z = matrix()) {
  a <- NULL
  set <- function(x){
    z <<- x
    a <<- NULL
  }
  get <- function()z
  setreturn <- function(inv) a <<- inv
  getreturn <- function() a 
  list(set = set, get = get, 
       setreturn = setreturn, 
       getreturn = getreturn)
}


cacheSolve <- function(z, ...) {
  ## This operation return a matrix that is the inverse of matrix z
  a <- z$getreturn()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  mat <- z$get()
  a <- solve(mat,...)
  z$setreturn(a)
  a
}
