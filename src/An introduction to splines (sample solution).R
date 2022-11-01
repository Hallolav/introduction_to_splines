###########################
#     load packages       #
###########################
source('load_data.R')


###########################
#   setting variables     #
###########################
df <- load_data(distance = 10000)
knots = c(
  1924, # artificial ice rinks
  1954, # nordic skate
  1974, # aerodynamic suits
  1986, # artificial ice rinks - with climate control
  1996 # clap skate
)


###########################
#defining helper functions#
###########################
check_conditions <- function(x, y, knots, degree){
  # this function checks some basic conditions required in order to do
  # regression. You don't have to complete this function.
  # check x/y conditions
  if (length(x) != length(y)){
    msg = 'x and y lengths differ'
    return(c(T, msg))
  }
  
  # check degree conditions
  if (degree%%1 != 0){
    msg = sprintf('Degree %.3f not integer. Please select degree 1, 2, 3, or 4', degree)
    return(c(T, msg))
  }
  
  if(degree <= 0){
    msg = sprintf('Degree %i too low. Please select degree 1, 2, 3, or 4', degree)
    return(c(T, msg))
  } 
  
  if (degree >6){
    msg = sprintf('Degree %i too high. This might result in rounding errors. Please select degree 1, 2, 3, or 4, 5, 6', degree)
    return(c(T, msg))
  }
  
  return(c(F, ''))
  
}

shift <- function(x){
  # this function performs min-max scaling. That is:
  #     $$x_scaled = (x - min(x)) / (max(x) - min(x))$$
  # please finish the function
  
  min_x = min(x)
  max_x = max(x)
  x = (x - min_x) / (max_x - min_x)
  
  # return x, but also min_x and max_x so that we can apply it to the knots
  return(list(min_x = min_x, max_x = max_x, x = x))
}

shift_back <- function(min_x = min_x, max_x = max_x, x = x){
  # this function reverts back the min-max scaling from before. Please finish this function
  x = (max_x - min_x) * x + min_x
  
  return(x)
}

get_datasubset <- function(x, y, knots, knotindex, na.rm){
  # this function takes a subset based on the current knot. We want to fit a
  # polynomial to each segment, so we only select the data within the current
  # segment. You don't have to complete this function.
  if (knotindex == 0){
    datasubset_x = x[which(x < knots[knotindex + 1])]
    datasubset_y = y[which(x < knots[knotindex + 1])]
    
    datasubset = cbind(datasubset_x, datasubset_y)
    colnames(datasubset) = c('year', 'time')
  }
  else if (knotindex == length(knots)){
    datasubset_x = x[which(x >= knots[knotindex])]
    datasubset_y = y[which(x >= knots[knotindex])]
    
    datasubset = cbind(datasubset_x, datasubset_y)
    colnames(datasubset) = c('year', 'time')
  }
  else{
    datasubset_x = x[which(x >= knots[knotindex])]
    datasubset_y = y[which(x >= knots[knotindex])]
    
    datasubset_x = datasubset_x[which(datasubset_x < knots[knotindex + 1])]
    datasubset_y = datasubset_y[which(datasubset_x < knots[knotindex + 1])]
    
    datasubset = data.frame(cbind(datasubset_x, datasubset_y))
    colnames(datasubset) = c('year', 'time')
    
  }
  
  if (na.rm){
    dss <- datasubset
    return(na.omit(datasubset))
  } else {
    return(datasubset)
  }
}

add_piecewise_basis <- function(degree, x){
  # this function adds the polynomial basis functions $h_1 = x^0$, ...,
  # $h_{degree} = x^{degree - 1}$. Please finish this function.
  basis_functions = as.matrix(rep(1, length(x)))
  colnames(basis_functions) <- c('x^0')
  
  if (degree > 1){
    for (power in 1 : (degree - 1)){
      new_col = as.matrix(x^power)
      cols <- c(colnames(basis_functions), sprintf("x^%i", power))
      basis_functions = cbind(basis_functions, new_col)
      colnames(basis_functions) <- cols
    }
  }
  
  return(basis_functions)
}

find_beta <- function(x, y){
  # this function fits a linear model to the basis functions and y values.
  # Please finish this function.
  beta = solve(t(x) %*% x, tol = 1e-40) %*% t(x) %*% y
  
  return(beta)
}

make_prediction = function(x, beta)  {
  # this function makes a prediction based $\hat{y} = \beta * x$. Pleae finish
  # this function
  cols = colnames(x)
  y = 0
  for (power in 1:ncol(x)){
    col_beta = beta[power] # find the beta corresponding to the current basis function
    col_x = x[,c(cols[power])] # find the current basis function
    beta_x = col_beta * col_x # multiply the basis function and beta
    y = y + beta_x # increment our prediction one step
  }
  
  years = x[,c('x^1')]
  result <- matrix(c(years, y), ncol = 2)
  colnames(result) <- c('year', 'time')
  
  return(result)
}

plot_prediction <- function(x, y, predictions, knots){
  # this function plots the prediction made before. The line segments for
  # different knots have a different colour
  plot(x, y) # scatterplot of the values
  
  # add line segments for every knot
  for (knotindex in 0:length(knots)){
    this_prediction = get_datasubset(predictions$year, predictions$time, knots, knotindex, na.rm = F)
    lines(x = this_prediction[,1], y = this_prediction[,2], col = knotindex + 2, lwd = 5)
  }
  
  # add a single line through all line segments
  lines(predictions)
}


###########################
#piecewise polynomial func#
###########################
my_piecewise_polynomial <- function(x, y, knots, degree = 3, ylim = NULL){
  # check conditions
  stop_value = check_conditions(x, y, knots, degree)
  if (stop_value[1]){
    stop(stop_value[2])
  }
  
  # in order to prevent a numerical underflow when calculating the coefficient beta, we need the x values to be normalized
  this_shift <- shift(x)
  x <- this_shift$x
  min_x <- this_shift$min_x
  max_x <- this_shift$max_x
  knots <- (knots - min_x) / (max_x - min_x)
  
  # initialize empty dataframe which will contain predictions
  predictions = data.frame(matrix(nrow = 0, ncol = 2))
  colnames(predictions) = c('year', 'time')
  
  # for every knot, find a prediction
  for (knotindex in 0:length(knots)){
    datasubset_train <- get_datasubset(x, y, knots, knotindex, na.rm = T)
    datasubset_test  <- get_datasubset(x, y, knots, knotindex, na.rm = F)
    
    x_train <- add_piecewise_basis(degree, datasubset_train[,1])
    y_train <- as.matrix(datasubset_train[,2])
    x_test <- add_piecewise_basis(degree, datasubset_test[,1])
    
    beta <- find_beta(x_train, y_train)
    
    prediction <- make_prediction(x = x_test, beta = beta)
    
    predictions = rbind(predictions, prediction)
  }
  
  # undoing the shift from before
  x <- shift_back(min_x, max_x, x)
  predictions$year <- x
  knots = (max_x - min_x) * knots + min_x
  
  # finally, visualizing the piecewise polynomials
  plot_prediction(x, y, predictions, knots)
  
  return(predictions)
}

res <- my_piecewise_polynomial(df$year, df$time, knots, degree = 4)


###############################
#           splines           #
###############################
add_spline_basis <- function(degree, x, knots){
  # this function takes the degree and knots of the speedskating data as input,
  # as well as the polynomial basis functions up to degree d-1.
  years = x[,c('x^1')]
  for (knot in knots){
    new_col = c()
    for (year in years){
      value = year - knot
      value = max(0, value)
      value = value^(degree - 1)
      new_col = c(new_col, value)
    }
    new_col = as.matrix(new_col)
    cols = c(colnames(x), sprintf("knot_%.3f", knot))
    
    x = cbind(x, new_col)
    colnames(x) <- cols
  }
  
  return(x)
}

my_spline <- function(x, y, knots, degree = 3){
  stop_value = check_conditions(x, y, knots, degree)
  if (stop_value[1]){
    stop(stop_value[2])
  }
  
  # in order to prevent a numerical underflow when calculating the coefficient beta, we need the x values to be normalized
  this_shift <- shift(x)
  x <- this_shift$x
  min_x <- this_shift$min_x
  max_x <- this_shift$max_x
  knots <- (knots - min_x) / (max_x - min_x)
  
  # initialize empty dataframe which will contain predictions
  predictions = data.frame(matrix(nrow = 0, ncol = 2))
  colnames(predictions) = c('year', 'time')
  
  train_index <- which(!is.na(y))
  x_train = x[train_index]
  y_train = y[train_index]
  
  x_train <- add_piecewise_basis(degree, x_train)
  x_train <- add_spline_basis(degree, x_train, knots)
  
  x_test <- add_piecewise_basis(degree, x)
  x_test <- add_spline_basis(degree, x_test, knots)
  
  beta <- find_beta(x_train, y_train)
  
  prediction <- data.frame(make_prediction(x = x_test, beta = beta))
  
  # undoing the shift from before
  x <- shift_back(min_x, max_x, x)
  prediction$year <- x
  knots = (max_x - min_x) * knots + min_x
  
  # finally, visualizing the piecewise polynomials
  plot_prediction(x, y, prediction, knots)
  
  lines(prediction)
  return(prediction)
}

res <- my_spline(df$year, df$time, knots, degree = 4)


###############################
#       natural splines       #
###############################
d <- function(degree, years, knot, last_knot){
  # this function corresponds to d_j(X) - d_{K-1}(X) in the reader. Watch
  # closely how it is implemented, and use this function to finish the next
  # function add_natural_spline_basis()
  knot_col = c()
  for (year in years){
    value = year - knot
    value = max(0, value)
    value = value^(degree - 1)
    knot_col = c(knot_col, value)
  }
  
  last_col = c()
  for (year in years){
    value = year - last_knot
    value = max(0, value)
    value = value^(degree - 1)
    last_col = c(last_col, value)
  }
  
  d = (knot_col - last_col) / (last_knot - knot)
  
  return(d)
}

add_natural_spline_basis <- function(degree, years, knots){
  # this function adds the natural spline basis functions. It is your task to
  # finish its implementation.
  x = as.matrix(rep(1, length(years)))
  colnames(x) <- c('x^0')
  
  new_col = as.matrix(years)
  cols <- c(colnames(x), 'x^1')
  x <- cbind(x, new_col)
  colnames(x) <- cols
  
  knotsubset = knots[-c(length(knots), length(knots) - 1)] # taking d_1 until d_{d-2}
  for (knot in knotsubset){
    newcol <- d(degree, years, knot, knots[length(knots)]) -
      d(degree, years, knots[length(knots) - 1], knots[length(knots)])
    cols <- c(colnames(x), sprintf('knot_%.3f', knot))
    x <- cbind(x, newcol)
    colnames(x) <- cols
  }
  
  return(x)
}

my_natural_spline <- function(x, y, knots, degree = 3){
  stop_value = check_conditions(x, y, knots, degree)
  if (stop_value[1]){
    stop(stop_value[2])
  }
  
  # in order to prevent a numerical underflow when calculating the coefficient beta, we need the x values to be normalized
  this_shift <- shift(x)
  x <- this_shift$x
  min_x <- this_shift$min_x
  max_x <- this_shift$max_x
  knots <- (knots - min_x) / (max_x - min_x)
  
  # initialize empty dataframe which will contain predictions
  predictions = data.frame(matrix(nrow = 0, ncol = 2))
  colnames(predictions) = c('year', 'time')
  
  train_index <- which(!is.na(y))
  x_train = x[train_index]
  y_train = y[train_index]
  
  x_train <- add_natural_spline_basis(degree, x_train, knots)
  x_test  <- add_natural_spline_basis(degree, x,       knots)
  
  beta <- find_beta(x_train, y_train)
  
  prediction <- data.frame(make_prediction(x = x_test, beta = beta))
  
  # undoing the shift from before
  x <- shift_back(min_x, max_x, x)
  prediction$year <- x
  knots = (max_x - min_x) * knots + min_x
  
  # finally, visualizing the piecewise polynomials
  plot_prediction(x, y, prediction, knots)
  
  lines(prediction)
  return(prediction)
}

res <- my_natural_spline(df$year, df$time, knots, degree = 4)
