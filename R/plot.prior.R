#' @title Plot prior distribution
#' @description
#' This function provides a simple \code{plot} method for the \code{prior} object class.
#' 
#' @param x an \code{\link{prior}} object class
#' @param ... additional arguments for \code{\link[graphics]{hist}}
#' 
#' @examples
#' # create object containing
#' # vector of r values
#' iter <- 1000
#' mu <- 0.1
#' cv <- 0.2
#' sd <- sqrt(log(1+cv^2))
#' x <- rlnorm(iter,log(mu)-sd^2/2,sd)
#' r <- prior(x)
#' 
#' # plot
#' \dontrun{
#' plot(r, xlab = 'r', ylab = '', yaxt = 'n', main = '')
#' }
#' 
#' @include prior-class.R
#' 
#' @importFrom graphics plot axis curve hist
#' @importFrom stats dlnorm sd
#' 
#' @export
plot.prior <- function(x, ...)
{
    
    if (length(x@lognormal_par) < 5) {
        
        # transform to normal
        y <- log(x@.Data)
        
        # estimate parameters of
        # normal distribution log(x)
        mu     <- mean(y)
        sigma  <- sd(y)
        sigma2 <- sigma^2
        
        # estimate parameters of
        # log-normal distribution
        theta <- exp(mu + sigma2/2)
        nu    <- exp(2*mu + sigma2)*(exp(sigma2) - 1)
        cv    <- sqrt(exp(sigma2) - 1)
        
        # assign
        x@lognormal_par <- list('E[log(x)]' = mu, 'SD[log(x)]' = sigma, 'E[x]' = theta, 'VAR[x]' = nu, 'CV[x]' = cv)
    }
    
    logmu    <- x@lognormal_par[['E[log(x)]']]
    logsigma <- x@lognormal_par[['SD[log(x)]']]
    
    hist(x@.Data, freq = FALSE, ..., main = "", xlab = "", ylab = "", axes = FALSE, cex = 2)
    axis(1)
    curve(dlnorm(x, logmu, logsigma), col = 2, lwd = 4, add = TRUE)
    
    invisible(x)
}