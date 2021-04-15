### Rethinking chapter 2

## grid approximation
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- rep( 1 , 20 )
prior <- ifelse( p_grid < 0.5 , 0 , 1 )
prior <- exp( -5*abs( p_grid - 0.5 ) )
# compute likelihood at each value in grid
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# plot
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )


## Quadratic approximation
library(rethinking)
globe.qa <- quap(
  alist(
    W ~ dbinom( W+L ,p) , # binomial likelihood
    p ~ dunif(0,1) # uniform prior
  ) ,
  data=list(W=6,L=3) )
# display summary of quadratic approximation
precis( globe.qa )


# analytical calculation 2.7
W <- 6
L <- 3
curve( dbeta( x , W+1 , L+1 ) , from=0 , to=1 )
# quadratic approximation
curve( dnorm( x , 0.67 , 0.16 ) , lty=2 , add=TRUE )


################ Exercises 
### Easy

# 2E1. (4) Pr(rain, Monday)/ Pr(Monday)

# 2E2. (3) The probability that it is Monday, given that it is raining.

# 2E3. (1) Pr(Monday|rain)

# 2E4. Given an infinite number of globe tossed we would expect 70 % of the tosses to land on water. However, 
# because probability "doesn't exist" p = 0.7 does not mean that we will always get 7 out of 10 tosses to land on water.


### Medium

#2M1. 
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- rep( 1 , 20 )
# compute likelihood at each value in grid
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
likelihood <- dbinom( 3 , size=4 , prob=p_grid )
likelihood <- dbinom( 5 , size=7 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# plot
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )


#2M2.
prior <- ifelse( p_grid < 0.5 , 0 , 1 )


#2M3.
p_grid <- c(1, 0.3) # Mars, Earth
# define prior
prior <- c( 0.5 , 0.5 )
# compute likelihood at each value in grid
likelihood <- dbinom( 1, size=1 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
posterior[2] # posterior Earth


#2M4.
# There are 3 black sides in the bag, 2 of which are on the B/B card, and 1 is on the B/W card.
# Therefore, drawing a black side can happen in 2 ways for the B/B card, and in 1 way for the B/W card.
# --> 2/3 probability that it is the B/B card.


#2M5.
# There are 5 black sides in the bag, 4 of which are on B/B cards, and 1 is on the B/W card.
# Therefore, drawing a black side can happen in 4 ways for the B/B cards, and in 1 way for the B/W card.
# --> 4/5 probability that it is the B/B card.


#2M6.
# There are 3 black sides in the bag, 2 of which are on the B/B card, and 1 is on the B/W card.
# Therefore, drawing a black side can happen in 2 ways for the B/B card, and in 1 way for the B/W card.
# However, drawing B/W is twice as likely as drawing B/B: 1*2 B/B = 2 B/B ways, 2*1 B/W = 2 B/W ways
# --> 2/4 probability that it is the B/B card.


#2M7.
# B/B B/W W/W # Cards
# 2 - 1 - 0 # First draw black
# 3 - 2 - 0 # second draw white
# B - W
# 2*3 B/B + 1*2 B/W = 8 total ways, 6 of which B/B = 0.75



### Hard
# 2H1. # Pr(newTwins|oldTwins)
p_grid <- c(0.1, 0.2) # A, B
# define prior
prior <- c( 1/3 , 2/3 )
# compute likelihood at each value in grid
likelihood <- dbinom( 1, size=1 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
posterior[1] * p_grid[1] + posterior[2] * p_grid[2] # chance of getting twins again



# 2H2.
p_grid <- c(0.1, 0.2) # A, B
# define prior
prior <- c( 0.5 , 0.5 )
# compute likelihood at each value in grid
likelihood <- dbinom( 1, size=1 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
posterior[1]


# 2H3.
p_grid <- c(0.1, 0.2) # Panda A, B
# define prior
prior <- c( 0.5 , 0.5 )
# compute likelihood at each value in grid
likelihood <- dbinom( 1, size=2 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
posterior[1]



# 2H4.
p_grid <- c(0.1, 0.2) # A, B
# define prior
prior <- c( 0.725 , 0.275 )
# compute likelihood at each value in grid
likelihood <- dbinom( 2, size=2 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)
posterior[1]

# 72.5%
# 1 Twin, 1 singleton Pr(A) = 0.597
# 2 Twins, Pr(A) = 0.397


