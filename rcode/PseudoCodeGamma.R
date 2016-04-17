gamma.sim <- function(lambda, m) {
# sim a gamma(lambda, m) rv using rejection with an exp envelope
# assumes m > 1 and lambda > 0
# generate f(x)=lambda^m*x^(m-1)*exp(-lambda*x)/gamma(m)     --- the gamma density at x
# generate h(x)=lambda/m*exp(-lambda/m*x)                   ---  the exponential density at x
# generate  k=m^m*exp(1-m)/gamma(m)                      --- in general. for m=2, λ=1 we have k=4/e
  while (TRUE) {               # keep sampling x’s from h(x) and testing them until you accept one
    X <- -log(runif(1))*m/lambda             # generate an x from h, the exponential density              
    # Generate y from Unif[0,Kh(x)]
    K <- m^m*exp(1-m)/gamma(m)
    Y <- runif(1,0,K*h(X, m, lambda))
    if (Y < f(X, m, lambda)) {
        return(X)
    }
  }
}

h<-function(x, m, lambda){
    return(lambda/m*exp(-lambda/m*x))
}

f<-function(x, m, lambda){
    return(lambda^m*x^(m-1)*exp(-lambda*x)/gamma(m))
}

set.seed(1999)
n <- 10000        # number of replicates
g <- rep(0, n)      # create somewhere to keep the answers
for (i in 1:n) g[i] <- gamma.sim(1, 2)    # generate your 10000 gamma r.v.s

hist(g, breaks=20, freq=F, xlab="x", ylab="pdf f(x)",
  main="theoretical and simulated gamma(1, 2) density")
x <- seq(0, max(g), .1)
lines(x, dgamma(x, 2, 1))
