## Dan Weinblatt
## Programming Assignment 2

##


## makeCacheMatrix is a function to 
## 1. Set the value of a matrix
## 2. Get the value of a matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <-function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setinv <-function(solve) m <<- solve
  getinv <-function()m
  
  list (set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## cacheSolve is a function to calculate the inverse
## of the matrix defined through makeCacheMatrix.
## It will use the cached inverse when possible
## (i.e., the inverse has already been calculated
## and the matrix hasn't changed).

cacheSolve <- function(x, ...) {
  m <-x$getinv()
  if(!is.null(m)){
    message ("getting cached data")
    return (m)
  }
  inverse <- x$get()
  m <-solve(inverse,...)
  x$setinv(m)
  m
}
