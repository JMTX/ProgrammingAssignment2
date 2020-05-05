## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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


is_sq_matrix<-function(x){
    #function to check if an arg is a square non NaN matrix. 
    #Return the boolean value r_value: TRUE if x is a square matrix with non Nan values
    #or FALSE if not.
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