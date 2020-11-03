
## Make correlations - population

n     <- 20000                    # length of vector
rho   <- 0.5                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
x1    <- rnorm(n, 1, 1)        # fixed given data
x2    <- rnorm(n, 2, 0.5)      # new random data
X     <- cbind(x1, x2)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

x <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(x1, x)                                    # check correlation = rho

df <- data.frame(x = x, y= x1)
df$x <- df$x*1000
df$y <- df$y*10

range(df$x)
range(df$y)

df$x <- df$x + abs(min(df$x))
df$y <- df$y + abs(min(df$y))

cor(df$x,df$y)
cor.test(df$x,df$y)[[9]][[1]]
cor.test(df$x,df$y)[[9]][[2]]

head(df)

## population

ggplot(df, aes(x=x,y=y)) + geom_point(alpha=.01) + 
  stat_smooth(method='lm') +
  theme_minimal() +
  ggtitle("Population N=20,000  r = 0.5")


# Collect samples of size n = 10
pres<-NULL
for(i in 1:6){
df1[[i]] <- df[sample(nrow(df), 10), ]
pres[[i]]<-ggplot(df1[[i]], aes(x=x,y=y)) + geom_point(alpha=.5) + 
  stat_smooth(method='lm', alpha=.3) +
  theme_minimal() +
  ggtitle( paste0( "r =", round(cor(df1[[i]]$x,df1[[i]]$y),2),
                  "[",
                  round(cor.test(df1[[i]]$x,df1[[i]]$y)[[9]][[1]],2),
                  ", ",
                  round(cor.test(df1[[i]]$x,df1[[i]]$y)[[9]][[2]],2),
                  "]"
                  ))

}

library(gridExtra)
grid.arrange(pres[[1]], pres[[2]], pres[[3]], 
             pres[[4]], pres[[5]], pres[[6]], nrow=2)
