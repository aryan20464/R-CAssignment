## NOTE: This assignment is done by keeping in mind the sample program code given in the assignment page 
##       parts of code from there is taken, so iam here by referencing the code taken from assignment page

## the program assumes that matrix is a square matrix. solve is the function used for computing the inverse of a matrix

## The first function, makeCacheMatrix creates vector, which is really a list containing arguments for functions
## set the value of the vector : set
## set the value of the inverse: setinv_val
## get the value of the vector : get
## get the value of the inverse: getinv_val

makeCacheMatrix <- function(x = matrix()) {
  
  ## inv_val is the variable taken for holding the inverse of matrix if calculated or else the matrix in beginnig.
  inv_val<-NULL 
  set = function(a)
  {
    x<<-a
    inv_val<-NULL
  }
  setinv_val = function(inverse)
  {
    inv_val<<-inverse
  }
  get = function() x    
  
  getinv_val = function() inv_val
  
  list(get=get,set=set,setinv_val=setinv_val,getinv_val=getinv_val)
}


## It computes the inverse of a matrix returned from makeCacheMatrix(). If inverse is not yet calcualted it does that
## or else if inverse is calculated the function retrieves it from former function and thus saves time of computation
## which we say as caching in this case!!

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv_val <- x$getinv_val()
  
  if(!is.null(inv_val)) ## if this is null inverse is not yet computed.
  {
    message("getting cached data")
    return (inv_val)
  }
  
  mat.data <- x$get()
  inv_val <-solve(mat.data,...)
  x$setinv_val(inv_val)
  return (inv_val)
}

## this is a function to check the cache functionality and display it to the user about how the caching is achived
## by showing the times of computation.
cache_check = function(compute_matrix){
  
  first_data <- makeCacheMatrix(compute_matrix)
  initial = Sys.time()
  cacheSolve(first_data)
  final = Sys.time()
  print("before caching")
  print(final-initial)
  
  initial = Sys.time()
  cacheSolve(first_data)
  final = Sys.time()
  print("after caching")
  print(final-initial)
  
  
}
## these are some test runs about how much time does different matrix sizes take for computation 

set.seed(1)
maxi_gen = rnorm(2500)
generated = matrix(maxi_gen, nrow=50, ncol=50)
cache_check(generated)

set.seed(1)
maxi_gen = rnorm(10000)
generated1 = matrix(maxi_gen, nrow=100, ncol=100)
cache_check(generated1)

set.seed(1)
maxi_gen2 = rnorm(250000)
generated2 = matrix(maxi_gen2, nrow=500, ncol=500)
cache_check(generated2)

set.seed(1)
maxi_gen3 = rnorm(1000000)
generated3 = matrix(maxi_gen3, nrow=1000, ncol=1000)
cache_check(generated3)

