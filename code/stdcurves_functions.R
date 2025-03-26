#' Convert between curves.
#' @description
#' Convert mfi on new plate to the dilution on the reference plate.
#'
#' @param mfi Known mfi of samples to be converted
#' @param params_new Known parameters for five parameter logistic fit.
#' @param params_ref_new Known parameters for five parameter logistic fit on reference plate
#' @param params_ref_old Known parameters for five parameter logistic fit of the old beads on the sample plate.
#' @return Returns the predicted dilution in comparison to the reference plate
#' @export
convert_between_curves <- function(mfi, params_new, params_ref_new, params_ref_old) {
  dilution <- convert_mfi_to_dilution(mfi,params_new)
  ref_mfi <- convert_dilution_to_mfi(dilution,params_ref_new)
  convert_mfi_to_dilution(ref_mfi,params_ref_old)
}

#' Convert known dilution to mfi from fitted standard curve
#' @description
#' Convert dilution to predicted mfi using known standard curve fit.
#'
#' @param dilution Known dilution of samples
#' @param params Known parameters for five parameter logistic fit.
#' @return Returns the predicted mfi of a sample with known dilution.
#' @export
convert_dilution_to_mfi <- function(dilution, params) {
  if (is.null(dilution) || is.null(params)) {
    error("Require both mfi and params to run.")
  }
  exp(log_logistic_5p(dilution, params[1], params[2], params[3], params[4], exp(params[5])))
}

#' Convert mfi to dilution using known standard curve fit.
#' @description
#' Convert mfi to dilution using known standard curve fit.
#'
#' @param mfi Known mfi of samples
#' @param params Known parameters for five parameter logistic fit.
#' @return Returns the dilution of each sample in mfi.
#' @export
convert_mfi_to_dilution <- function(mfi, params) {
  if (is.null(mfi) | is.null(params)) {
    error("Require both mfi and params to run.")
  }
  y <- log(mfi)
  result <- inverse_log_logistic_5p(
    y,
    params[1],
    params[2],
    params[3],
    params[4],
    exp(params[5])
  )
  result[y > (params[2] + params[3])] <- 1.0
  result[y < params[2]] <- 0.0
  result[result > 1.0] <- 1.0
  return(result)
}

#' Fit a standard curve to known mfi and dilution values.
#' @description
#' We wish to convert the standard curve samples to a five parameter logistic curve.
#' This function takes those values and calls optim to determine the fit.
#'
#' @param mfi Known mfi of samples
#' @param dilution Known dilution of samples
#' @param init Initial guess for solution of fit.
#' @param control Optional list of control parameters for the underlying call to optim.
#' @export
fit_standard_curve <- function(mfi, dilution, control = NULL) {
  if (is.null(mfi) | is.null(dilution)) {
    error("Require both mfi and dilution to run.")
  }
  
  y1 <- log(mfi)
  initial_solution <- c(-1.0, 0.0, max(y1), 0.0, 0.0)
  
  error_func <- function(x) {
    f1 <- log_logistic_5p(dilution, x[1], x[2], x[3], x[4], exp(x[5]))
    sum((y1 - f1)^2.0)
  }
  
  solution <- optim(par = initial_solution, fn = error_func, control = control)
  if (solution$convergence != 0) {
    stop("Standard curve failed to converge. Look at data and possibly change control parameters from default.")
  }
  solution$par
}

inverse_log_logistic_5p <- function(y,b,c,d,e,f){
  A <- (d/(y-c))^(1/f)-1
  return(exp(-e) *A^(1/b))
}

log_logistic_5p <- function(x, b, c, d, e, f) {
  return(c + d / (1.0 + exp(b * (log(x) + e)))^f)
}