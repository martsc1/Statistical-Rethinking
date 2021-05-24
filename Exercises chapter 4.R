### Chapter 4
library(rethinking)
pos <- replicate( 1000 , sum( runif(16,-1,1) ) )
hist(pos)
plot(density(pos))


prod( 1 + runif(12,0,0.1) )
growth <- replicate( 10000 , prod( 1 + runif(12,0,0.1) ) )
dens( growth , norm.comp=TRUE )

big <- replicate( 10000 , prod( 1 + runif(12,0,0.5) ) )
small <- replicate( 10000 , prod( 1 + runif(12,0,0.01) ) )
dens( big , norm.comp=TRUE )
dens( small , norm.comp=TRUE )

log.big <- replicate( 10000 , log(prod(1 + runif(12,0,0.5))) )
dens( log.big , norm.comp=TRUE )


data(Howell1)
d <- Howell1
str(d)
precis(d, hist=FALSE)
d2 <- d[ d$age >= 18 , ]
dens(d2$height)

curve( dnorm( x , 178 , 20 ) , from=100 , to=250 ) # mean prior
curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )    # SD prior


sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

sample_mu <- rnorm( 1e4 , 178 , 100 ) 
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )


mu.list     <- seq( from=150, to=160 , length.out=100 ) 
sigma.list  <- seq( from=7 , to=9 , length.out=100 )
post        <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL     <- sapply( 1:nrow(post) , function(i) sum(
               dnorm( d2$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) )
post$prod   <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
               dunif( post$sigma , 0 , 50 , TRUE )
post$prob   <- exp( post$prod - max(post$prod) )

contour_xyz( post$mu , post$sigma , post$prob )
image_xyz( post$mu , post$sigma , post$prob )

sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE , prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )
dens( sample.mu )
dens( sample.sigma )
PI( sample.mu )
PI( sample.sigma )

d3 <- sample( d2$height , size=20 )
mu.list <- seq( from=150, to=170 , length.out=200 ) 
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i)
  sum( dnorm( d3 , mean=post2$mu[i] , sd=post2$sigma[i] ,
              log=TRUE ) ) )
post2$prod <- post2$LL + dnorm( post2$mu , 178 , 20 , TRUE ) +
  dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
                        prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="mu" , ylab="sigma" , pch=16 )
dens( sample2.sigma , norm.comp=TRUE )



flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu     ~ dnorm( 178 , 20 ) ,
  sigma  ~ dunif( 0 , 50 )
)

m4.1 <- quap( flist , data=d2 )
precis( m4.1 )

m4.2 <- quap( 
              alist(
                height ~ dnorm( mu , sigma ) ,
                mu ~ dnorm( 178 , 0.1 ) ,
                sigma ~ dunif( 0 , 50 )
              ) , data=d2 )
precis( m4.2 )

vcov( m4.1 )
diag( vcov( m4.1 ) )
cov2cor( vcov( m4.1 ) )
post <- extract.samples( m4.1 , n=1e4 )
head(post)
precis(post, hist=F)



data(Howell1); d <- Howell1; d2 <- d[ d$age >= 18 , ]
plot( d2$height ~ d2$weight )


set.seed(2971) 
N <- 100 # 100 lines
a <- rnorm( N , 178 , 20 )
b <- rnorm( N , 0 , 10 )

plot( NULL , xlim=range(d2$weight) , ylim=c(-100,400) , 
      xlab="weight" , ylab="height" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
                        from=min(d2$weight) , to=max(d2$weight) , add=TRUE ,
                        col=col.alpha("black",0.2) )


b <- rlnorm( N , 0 , 1 )
dens( b , xlim=c(0,5) , adj=0.1 )



data(Howell1); d <- Howell1; d2 <- d[ d$age >= 18 , ]
# define the average weight, x-bar
xbar <- mean(d2$weight)
# fit model
m4.3 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu     <- a + b*( weight - xbar ) ,
    a      ~ dnorm( 178 , 20 ) ,
    b      ~ dlnorm( 0 , 1 ) ,
    sigma  ~ dunif( 0 , 50 )
  ) , data=d2 )

precis( m4.3 )
round( vcov( m4.3 ) , 3 )


plot( height ~ weight , data=d2 , col=rangi2 )
post <- extract.samples( m4.3 )
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*(x - xbar) , add=TRUE )


N <- 10
dN <- d2[ 1:N , ]
mN <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - mean(weight) ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=dN )

# extract 20 samples from the posterior
post <- extract.samples( mN , n=20 )
# display raw data and sample size
plot( dN$weight , dN$height ,
      xlim=range(d2$weight) , ylim=range(d2$height) ,
      col=rangi2 , xlab="weight" , ylab="height" )
mtext(concat("N = ",N))
# plot the lines, with transparency
for ( i in 1:20 )
  curve( post$a[i] + post$b[i]*(x-mean(dN$weight)) ,
         col=col.alpha("black",0.3) , add=TRUE )


post <- extract.samples( m4.3 )
mu_at_50 <- post$a + post$b * ( 50 - xbar )
dens( mu_at_50 , col=rangi2 , lwd=2 , xlab="mu|weight=50" )

mu <- link( m4.3 )
str(mu)


# define sequence of weights to compute predictions for 
# these values will be on the horizontal axis
weight.seq <- seq( from=25 , to=70 , by=1 )
# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link( m4.3 , data=data.frame(weight=weight.seq) )
str(mu)

# use type="n" to hide raw data 
plot( height ~ weight , d2 , type="n" )
# loop over samples and plot each mu value
for ( i in 1:100 )
  points( weight.seq , mu[i,] , pch=16 , col=col.alpha(rangi2,0.1) )

# summarize the distribution of mu 
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )

# plot raw data
# fading out points to make line and interval more visible
plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( weight.seq , mu.mean )
# plot a shaded region for 89% PI
shade( mu.PI , weight.seq )



sim.height <- sim( m4.3 , data=list(weight=weight.seq) )
str(sim.height)
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

# plot raw data
plot( height ~ weight , d2 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( weight.seq , mu.mean )
# draw HPDI region for line
shade( mu.HPDI , weight.seq )
# draw PI region for simulated heights
shade( height.PI , weight.seq )

data(Howell1)
d <- Howell1
plot( height ~ weight , d )

d$weight_s <- ( d$weight - mean(d$weight) )/sd(d$weight) 
d$weight_s2 <- d$weight_s^2
m4.5 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight_s + b2*weight_s2 ,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d )

precis( m4.5 )

weight.seq <- seq( from=-2.2 , to=2 , length.out=30 )
pred_dat <- list( weight_s=weight.seq , weight_s2=weight.seq^2 )
mu <- link( m4.5 , data=pred_dat )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.height <- sim( m4.5 , data=pred_dat )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

plot( height ~ weight_s , d , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq )

d$weight_s3 <- d$weight_s^3 
m4.6 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight_s + b2*weight_s2 + b3*weight_s3 ,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm( 0 , 10 ) ,
    b3 ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d )


data(cherry_blossoms)
d <- cherry_blossoms
precis(d, hist=FALSE)

d2 <- d[ complete.cases(d$doy) , ] # complete cases on doy 
num_knots <- 15
knot_list <- quantile( d2$year , probs=seq(0,1,length.out=num_knots) )

library(splines)
B <- bs(d2$year,
        knots=knot_list[-c(1,num_knots)] ,
        degree=3 , intercept=TRUE )

plot( NULL , xlim=range(d2$year) , ylim=c(0,1) , xlab="year" , ylab="basis" ) 
for ( i in 1:ncol(B) ) lines( d2$year , B[,i] )


m4.7 <- quap( 
              alist(
                D ~ dnorm( mu , sigma ) ,
                mu <- a + B %*% w ,
                a ~ dnorm(100,10),
                w ~ dnorm(0,10),
                sigma ~ dexp(1)
              ), data=list( D=d2$doy , B=B ) ,
              start=list( w=rep( 0 , ncol(B) ) ) )


post <- extract.samples( m4.7 ) 
w <- apply( post$w , 2 , mean )
plot( NULL , xlim=range(d2$year) , ylim=c(-6,6) ,
      xlab="year" , ylab="basis * weight" )
for ( i in 1:ncol(B) ) lines( d2$year , w[i]*B[,i] )

mu <- link( m4.7 ) 
mu_PI <- apply(mu,2,PI,0.97)
plot( d2$year , d2$doy , col=col.alpha(rangi2,0.3) , pch=16 )
shade( mu_PI , d2$year , col=col.alpha("black",0.5) )




################# Exercises ################# 
### Easy



### Medium
# 4M1. For the model definition below, simulate observed y values from the prior (not the posterior).
sample_mu <- rnorm( 1e4 , 0 , 10 )
sample_sigma <- rexp( 1e4 , 1 )
sample_y <- rnorm(1e4, sample_mu, sample_sigma)
dens(sample_y)
HPDI(sample_y)



# 4M2. Translate the model just above into a quap formula.
y      ~ dnorm( mu , sigma )
mu     ~ dnorm( 0, 10 )
sigma  ~ dexp( 1 )


# 4M3. Translate the quap model formula below into a mathematical model definition.
#yi    ~ Normal(mu,sigma)
#mui   = alpha + beta*xi
#alpha ~ Normal(0,10)
#beta  ~ Uniform(0,1)
#sigma ~ Exponential(1)



### Hard
# 4H1. The weights listed below were recorded in the !Kung census, but heights were not recorded for
# these individuals. Provide predicted heights and 89% intervals for each of these individuals. That is,
# fill in the table below, using model-based predictions.

# Take the textbook model
data(Howell1); d <- Howell1; d2 <- d[ d$age >= 18 , ]
# define the average weight, x-bar
xbar <- mean(d2$weight)
# fit model
m4.3 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu     <- a + b*( weight - xbar ) ,
    a      ~ dnorm( 178 , 20 ) ,
    b      ~ dlnorm( 0 , 1 ) ,
    sigma  ~ dunif( 0 , 50 )
  ) , data=d2 )

# Enter given weight sequence
weight.seq <- c(46.95, 43.72, 64.78, 32.59, 54.63)

# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link( m4.3 , data=data.frame(weight=weight.seq) )
str(mu)
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )

# Expected height: 
#         [,1]     [,2]     [,3]     [,4]     [,5]
#     156.3667 153.4464 172.4870 143.3837 163.3103
# 89 % interval: 
#         [,1]     [,2]     [,3]     [,4]     [,5]
# 5%  155.9152 153.0220 171.1003 142.4617 162.5385
# 94% 156.8299 153.8921 173.9141 144.3109 164.1249


# 4H2. Select out all the rows in the Howell1 data with ages below 18 years of age. If you do it right,
# you should end up with a new data frame with 192 rows in it.
# (a) Fit a linear regression to these data, using quap. Present and interpret the estimates. For
# every 10 units of increase in weight, how much taller does the model predict a child gets?

```{r}
data(Howell1); d <- Howell1; d3 <- d[ d$age < 18 , ]
# define the average weight, x-bar
xbar <- mean(d3$weight)
# fit model
H4.2 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu     <- a + b*( weight - xbar ) ,
    a      ~ dnorm( 128 , 35 ) ,
    b      ~ dlnorm( 0 , 1 ) ,
    sigma  ~ dunif( 0 , 50 )
  ) , data=d3 )
```


# define sequence of weights to compute predictions for 
# these values will be on the horizontal axis
```{r}
weight.seq <- seq( from=2 , to=52 , by=1 )
# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link( H4.2 , data=data.frame(weight=weight.seq) )
str(mu)

# Increase in height every 10 weight units
post <- extract.samples( H4.2 )
b_map <- mean(post$b)
b_map * 10
```


# (b) Plot the raw data, with height on the vertical axis and weight on the horizontal axis. Superimpose
# the MAP regression line and 89% interval for the mean. Also superimpose the 89% interval
# for predicted heights.

```{r}
# use type="n" to hide raw data 
plot( height ~ weight , d3 , type="n" )
# loop over samples and plot each mu value
for ( i in 1:100 )
  points( weight.seq , mu[i,] , pch=16 , col=col.alpha(rangi2,0.1) )

# summarize the distribution of mu 
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )

# plot raw data
# fading out points to make line and interval more visible
plot( height ~ weight , data=d3 , col=col.alpha(rangi2,0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( weight.seq , mu.mean )
# plot a shaded region for 89% PI
shade( mu.PI , weight.seq )
```



# (c) What aspects of the model fit concern you? Describe the kinds of assumptions you would
# change, if any, to improve the model. You don't have to write any new code. Just explain what the
# model appears to be doing a bad job of, and what you hypothesize would be a better model.

# The data does not look like it fits a straight line very well, there is too much of a curvature.
# especially at the ends, the model predictions are therefore quite far off. 
# A curved model (polynomial?), would probably predict the data much more accurately



4H3. Suppose a colleague of yours, who works on allometry, glances at the practice problems just
above. Your colleague exclaims, "That's silly. Everyone knows that it's only the logarithm of body
weight that scales with height!" Let's take your colleague's advice and see what happens.

(a) Model the relationship between height (cm) and the natural logarithm of weight (log-kg). Use
the entire Howell1 data frame, all 544 rows, adults and non-adults. Can you interpret the resulting
estimates?


```{r}
data(Howell1); d <- Howell1

plot(log(d$weight),d$height)

xbar <- mean(log(d$weight))
# fit model
H4.3 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu     <- a + b*( log(weight) - xbar ) ,
    a      ~ dnorm( 128 , 35 ) ,
    b      ~ dlnorm( 0 , 1 ) ,
    sigma  ~ dunif( 0 , 50 )
  ) , data=d )

post <- extract.samples( H4.3 )
a_map <- mean(post$a)
b_map <- mean(post$b)
curve(a_map + b_map*(x-xbar), add=TRUE)

```
According to the model, the intercept is 138 cm at the average weight of 3.4 log.
The slope is 47 cm, meaning that the height will increase by 47 cm if the weight increases by 1 on the logarithmic scale. It seems like the model which takes the log weight, indeed scales much better with height.



(b) Begin with this plot: plot( height ~ weight , data=Howell1 ). Then use samples
from the quadratic approximate posterior of the model in (a) to superimpose on the plot: (1) the
predicted mean height as a function of weight, (2) the 97% interval for the mean, and (3) the 97%
interval for predicted heights.

```{r}
plot( height ~ weight , data=d )
weight.seq <- seq( from=2 , to=65 , by=1 ) # choose weights based on weights in the data
mu <- link( H4.3 , data=data.frame(weight=weight.seq) )
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.97 )

sim.height <- sim( H4.3 , data=list(weight=weight.seq) )
height.PI <- apply( sim.height , 2 , PI , prob=0.97 )

# draw MAP line (1)
lines( weight.seq , mu.mean )
# draw HPDI region for line (2)
shade( mu.HPDI , weight.seq, col = col.alpha("green",0.15) )
# draw PI region for simulated heights (3)
shade( height.PI , weight.seq )

```









