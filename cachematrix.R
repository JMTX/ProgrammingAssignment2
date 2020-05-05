##Below are 2 functions that are used to create a special object that stores a matrix and caches its inverse.
##A third function was added to check if a matrix is squared with 0 NaN values

##The first function makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
##1- set the value of the matrix
##2- get the value of the matrix
##3- set the value of the inverse of the matrix
##4- get the value of the inverse of the matrix

## this list is created only if the arg x is a squared non NaN matrix else the function returns NULL.
##To check this fact, the third function "is_sq_matrix" is used.
makeCacheMatrix <- function(x = matrix()) {
    if (is_sq_matrix(x)){
        inv_x <- NULL
        set <- function(y) {
            if(is_sq_matrix(y)){
                x <<- y
                inv_x <<- NULL
            }
        }
        get <- function() x
        set_inv <- function(inv) inv_x <<- inv
        get_inv <- function() inv_x
        list(set = set, get = get,
            set_inv = set_inv,
            get_inv = get_inv)
    }else{
        NULL
    }
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the set_inv function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$get_inv()
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data, ...)
    x$set_inv(inv_x)
    inv_x
}

##function to check if an arg x is a squared non NaN matrix. 
##Return the boolean value r_value: TRUE if x is a squared matrix with 0 Nan values
##or FALSE if not.
##This function check also if the determinant of the matrix is equal to 0 but
## only print a warning in this case to say that the matrix is not invertible
is_sq_matrix<-function(x){
    
    r_value = FALSE
    eps<-10**(-9)
    #check if the arg is a matrix
    if(class(x)=="matrix"){
        #check if the matrix is squared
        dim_x <- dim(x)
        if( mean(dim_x==dim_x[1]) > (1-eps)){
            #check if the matrix have NaN value
            if (mean(is.nan(x))<(0+eps)){
                r_value=TRUE
                #check if determinant is equal to 0 and warn that x is not invertible if it is the case.
                if (det(x) ==0){
                    warning("the matrix x is not invertible")
                }
            }
            else{
                message("x contains NaN values.")
            }
        }
        else{
            message("The matrix x is not squared.")
        }
    }
    else{
        message("x is not a matrix.")
    }
    if (!r_value){
        message("x must be a squared matrix with non NaN Values.")
    }
    r_value
    
}