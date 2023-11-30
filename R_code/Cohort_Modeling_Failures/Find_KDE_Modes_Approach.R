# Generate random values from a mixture distribution.
rmix <- function(n, mu, sigma, p) {
  matrix(rnorm(length(mu)*n, mu, sigma), ncol=n)[
    cbind(sample.int(length(mu), n, replace=TRUE, prob=p), 1:n)]
}
mu <- c(25, 60, 130, 190) # Means
sigma <- c(8, 13, 15, 19) # SDs
p <- c(.18, .2, .24, .28) # Relative proportions (needn't sum to 1)
n <- 1e4                  # Sample size
x <- rmix(n, mu, sigma, p)

# Find the modes of a KDE.
# (Quick and dirty: it assumes no mode spans more than one x value.)
findmodes <- function(kde) {
  kde$x[which(c(kde$y[-1],NA) < kde$y & kde$y > c(NA,kde$y[-length(kde$y)]))]
}

# Compute the mode trace by varying the bandwidth within a factor of 10 of
# the default bandwidth.  Track the modes as the bandwidth is decreased from
# its largest to its smallest value.
# This calculation is fast, so we can afford a detailed search.
m <- mean(x)
id <- 1
bw <- density(x)$bw * 10^seq(1,-1, length.out=101) 
modes.lst <- lapply(bw, function(h) {
  m.new <- sort(findmodes(density(x, bw=h)))
  # -- Associate each previous mode with a nearest new mode.
  if (length(m.new)==1) delta <- Inf else delta <- min(diff(m.new))/2
  d <- outer(m.new, m, function(x,y) abs(x-y))
  i <- apply(d, 2, which.min)
  g <- rep(NA_integer_, length(m.new))
  g[i] <- id[1:ncol(d)]
  #-- Create new ids for new modes that appear.
  k <- is.na(g)
  g[k] <- (sum(!k)+1):length(g)
  id <<- g
  m <<- m.new
  data.frame(bw=h, Mode=m.new, id=g)
})
X <- do.call(rbind, args=modes.lst)
X$id <- factor(X$id)

# Locate the modes at the most vertical portions of their traces.
minslope <- function(x, y) {
  f <- splinefun(x, y)
  e <- diff(range(x)) * 1e-4
  df2 <- function(x) ((f(x+e)-f(x-e)) / (2*e))^2 # Numerical derivative, squared
  v <- optimize(df2, c(min(x),max(x)))
  c(bw=v$minimum, slope=v$objective, Mode=f(v$minimum))
}

# Retain the desired modes.
n.modes <- 4 # USER SELECTED: Not automatic
bw.max <- max(subset(X, id==n.modes)$bw)
modes <- sapply(1:n.modes, function(i) {
  Y <- subset(X, id==i & bw <= bw.max)
  minslope(Y$bw, Y$Mode)
})
#
# Plot the results.
#
library(ggplot2)
ggplot(X, aes(bw, Mode)) +
  geom_line(aes(col=id), size=1.2, show.legend=FALSE) +
  geom_point(aes(bw, Mode), data=as.data.frame(t(modes)), size=3, col="Black", alpha=1/2) +
  scale_x_log10() +
  coord_flip() +
  ggtitle("Mode Trace")

ggplot(data.frame(x), aes(x, ..density..)) +
  geom_histogram(bins=500, fill="#2E75B2") +
  geom_vline(data=as.data.frame(t(modes)),
             mapping=aes(xintercept=Mode), col="#D18A4e", size=1) +
  ggtitle("Histogram With Modes")
