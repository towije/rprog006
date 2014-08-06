## These functions saving runtime by computing only when necessary.

## Function makeCacheMatrix makes a copy of x matrix.
## The new matrix is a kind of "rich" type, similar to class with methods.
## Methods are:
### getting oryginal matrix;
### solve and caching solved matrix;
### getting solved matrixh from cache.
## I think the "set" method from makeVector example is unnecessary,
## then I skipped it.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL # clear cache in the global (for this function) env.
  get <- function() x # fun. returns oryginal matrix from argument x
  setsolve <- function(solve) s <<- solve # fun. stores solution in global s
  getsolve <- function() s # fun. returns solved matrix from global s variable
  list(get = get,
       setsolve = setsolve,
       getsolve = getsolve) # defining list (+/- class with methods)
}


## Function checks the existence of solution, computes if necessary, and return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve() # getting solution from cache of "rich" matrix x
  if(!is.null(s)) { # if x is already solved
    message("getting cached data") # print message,
    return(s) # return cached result, and exit function
  } # if not
  data <- x$get() # getting oryginal matrix from "get" position of "rich" matrix
  s <- solve(data, ...) # run solve function and write solution to s
  x$setsolve(s) # cacheing solution from s to global s of "rich" matrix
  s # print result
}


### Trying...
mx<-round(matrix(abs(rnorm(25)),5)*100)
smx<-solve(mx)
cmx<-makeCacheMatrix(mx)
smx2<-cacheSolve(cmx) # 1st run - computing solution
smx2<-cacheSolve(cmx) # next getting solution from cache
identical(smx,smx2)
# [1] TRUE






