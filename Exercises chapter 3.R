p_grid <- seq( from=0 , to=1 , length.out=1000 )
prob_p <- rep( 1 , 1000 )
prob_data <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
plot(samples)
library(rethinking)
dens( samples )

sum( posterior[ p_grid < 0.5 ] )
sum( samples < 0.5 ) / 1e4
sum( samples > 0.5 & samples < 0.75 ) / 1e4
quantile( samples , 0.8 )
quantile( samples , c( 0.1 , 0.9 ) )
PI( samples , 0.8 )
HPDI( samples , 0.5 )


loss <- sapply( p_grid , function(d) sum( posterior*abs( d - p_grid ) ) )
p_grid[ which.min(loss) ]


dbinom( 0:2 , size=2 , prob=0.7 )
rbinom( 1 , size=2 , prob=0.7 )


dummy_w <- rbinom( 1e5 , size=9 , prob=0.7 ) 
simplehist( dummy_w , xlab="dummy water count" )


################ Exercises ###############################

### Easy

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

dens(samples, show.HPDI = 0.95)

# 3E1. How much posterior probability lies below p = 0.2?
mean(samples<0.2)
# 3E2. How much posterior probability lies above p = 0.8?
mean(samples>0.8)

# 3E3. How much posterior probability lies between p = 0.2 and p = 0.8?
mean(samples>0.2 & samples<0.8)

#  3E4. 20% of the posterior probability lies below which value of p?
quantile(samples, 0.2)

#  3E5. 20% of the posterior probability lies above which value of p?
1-quantile(samples, c(0.8,1))

#  3E6. Which values of p contain the narrowest interval equal to 66% of the posterior probability?
HPDI(samples, 0.66)

#  3E7. Which values of p contain 66% of the posterior probability, assuming equal posterior probability
# both below and above the interval?
quantile(samples, c(0.17,0.83))


### Medium

# 3M1. Suppose the globe tossing data had turned out to be 8 water in 15 tosses. Construct the posterior
# distribution, using grid approximation. Use the same flat prior as before.

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(p_grid, posterior)


# 3M2. Draw 10,000 samples from the grid approximation from above. Then use the samples to calculate
# the 90% HPDI for p.
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
HPDI(samples, 0.9 )
dens(samples, show.HPDI = 0.9)


# 3M3. Construct a posterior predictive check for this model and data. This means simulate the distribution
# of samples, averaging over the posterior uncertainty in p. What is the probability of observing
# 8 water in 15 tosses?
w <- rbinom( 1e4 , size=15 , prob=samples )
simplehist(w)
sum(w==8)/1e4


# 3M4. Using the posterior distribution constructed from the new (8/15) data, now calculate the probability
# of observing 6 water in 9 tosses.
w <- rbinom( 1e4 , size=9 , prob=samples )
simplehist(w)
sum(w==6)/1e4


# 3M5. Start over at 3M1, but now use a prior that is zero below p = 0.5 and a constant above p = 0.5.
# This corresponds to prior information that a majority of the Earth's surface is water. Repeat each
# problem above and compare the inferences. What difference does the better prior make? If it helps,
# compare inferences (using both priors) to the true value p = 0.7.
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- ifelse( p_grid < 0.5 , 0 , 1 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(p_grid, posterior)

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
HPDI(samples, 0.9 )
dens(samples, show.HPDI = 0.9)

w <- rbinom( 1e4 , size=15 , prob=samples )
simplehist(w)
sum(w==8)/1e4

w <- rbinom( 1e4 , size=9 , prob=samples )
simplehist(w)
sum(w==6)/1e4


# 3M6. Suppose you want to estimate the Earth's proportion of water very precisely. Specifically, you
# want the 99% percentile interval of the posterior distribution of p to be only 0.05 wide. This means
# the distance between the upper and lower bound of the interval should be 0.05. How many times will
# you have to toss the globe to do this?
  
# globe toss function
globe_toss_function = function(prior, tosses) {
  sample = rbinom(1e4 , size = tosses , prob = prior) # simulate tossing the Earth 1e4 times the number of given tosses and counting the number of water
  posterior = sample / tosses # Calculate the proportion of water for each simulation
  
  percentile = HPDI(posterior, 0.99) # calculate the 99th percentile
  if ((percentile[2] - percentile[1]) < 0.05) {
    # check if the 99th percentile spans < 0.05
    return(tosses)
  } else {
    tosses = tosses + 10 # if the condition is not met, repeat the simulation but with 10 more tosses
    globe_toss_function(prior, tosses)
  }
}

prior = 0.7
tosses = 1000
nr_tosses_needed = round(mean(replicate(100,globe_toss_function(prior,tosses)))) # run the function 100 times to account for variations

sprintf(paste("You need ",nr_tosses_needed, " tosses to estimate the Earth's proportion of water very precisely, given a prior of ",prior))



### Hard

data(homeworkch3)

# 3H1. Using grid approximation, compute the posterior distribution for the probability of a birth
# being a boy. Assume a uniform prior probability. Which parameter value maximizes the posterior
# probability?
  
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep(1 ,1000)
likelihood <- dbinom( 111 , size=200 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(p_grid, posterior)
  
# max posterior probability:
p_grid[which.max(posterior)]
  

# 3H2. Using the sample function, draw 10,000 random parameter values from the posterior distribution
#  calculated above. Use these samples to estimate the 50%, 89%, and 97% highest posterior
#  density intervals.

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
HPDI(samples, c(0.5, 0.89, 0.97))


# 3H3. Use rbinom to simulate 10,000 replicates of 200 births. You should end up with 10,000 numbers,
# each one a count of boys out of 200 births. Compare the distribution of predicted numbers
# of boys to the actual count in the data (111 boys out of 200 births). There are many good ways to
# visualize the simulations, but the dens command (part of the rethinking package) is probably the
# easiest way in this case. Does it look like the model fits the data well? That is, does the distribution
# of predictions include the actual observation as a central, likely outcome?

samples = rbinom(1e4, 200, 0.55)
dens(samples)
abline(v=sum(birth1 + birth2))


# 3H4. Now compare 10,000 counts of boys from 100 simulated first borns only to the number of boys
# in the first births, birth1. How does the model look in this light?
samples = rbinom(1e4, 100, 0.55)
dens(samples)
abline(v=sum(birth1))


# 3H5. The model assumes that sex of first and second births are independent. To check this assumption,
# focus now on second births that followed female first borns. Compare 10,000 simulated counts
# of boys to only those second births that followed girls. To do this correctly, you need to count the
# number of first borns who were girls and simulate that many births, 10,000 times. Compare the
# counts of boys in your simulations to the actual observed count of boys following girls. How does the
# model look in this light? Any guesses what is going on in these data?

samples = rbinom(1e4, length(birth2[birth1==0]), 0.55)
dens(samples)
abline(v=sum(birth2[birth1==0]))

# It looks like parents who gave birth to a girl at first have a much higher likelihood to give birth 
# to sons the second time around than what the model would predict.

