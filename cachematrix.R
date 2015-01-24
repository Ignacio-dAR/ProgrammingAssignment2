## This function takes an input in matrix format,
## and creates an special matrix that cache its inverse as "I".


makeCacheMatrix <- function(x = matrix()) {
  I <- NULL                   ##This command creates an empty space to store the inverse matrix
  
##the following 4 functions set the value of the matrix, collect it, set its reverse and collect it  
  
  set <- function(y) {        ##collect the input matrix from an outside environment and set I to NULL
    x <<- y
    I <<- NULL
  }  
  get <- function() {x}       ##return the input matrix 
  setI <- function(solve) {I <<- solve} ##calculate the inverse matrix and store it in "I"
  getI <- function() {I}      ##get the value of the inverse matrix "I"
  list(set = set, get = get,setI = setI,getI = getI) ##return a list with the output of the previous functions as output
}
##now these 4 functions can be applied to a matrix "a" by the command "b<-makeCacheMatrix(a)".

## cacheSolve computes the inverse of the special "matrix" returned by `makeCacheMatrix` above ("b")
## simply using the following command (let's call "c" the variable where the inverse matrix is stored:
## c<-cacheSolve(b)

cacheSolve <- function(x=matrix(), ...) {
  I <- x$getI()                       ##Search for an existing inverse matrix in I
  if(!is.null(I)) {                   ##if I isn't empty, returns the cached value
    message("getting cached matrix")
    return(I)
  }
  matrix <- x$get()                   ## apply the get() function to collect the matrix
  I <- solve(matrix, ...)             ## store the value of the inverse matrix
  x$setI(I)                           ## apply the setI value to set it to the object
  I                                   ## returns the inverse matrix as result
}
