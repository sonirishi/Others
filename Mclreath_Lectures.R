source("E:/Documents/Practice/standard_code.R")

dbinom(6,9,0.5)

### Density can be thought of as rate of change of probablity

p <- seq(0,1,length.out = 1000)

prior <- rep(1,1000)

likelihood <- dbinom(6,9,prob=p)  ## THis is like trying different values of p (or beta) to get the
### best fit for given data

posterior <- likelihood*prior

posterior <- posterior/sum(posterior)

plot(likelihood/sum(likelihood),type='l')

plot(posterior,type='l')   ### normal stats where prior is flat means no prior at all

samples <- sample(p,prob = posterior,size = 1e4,replace = T)

plot(samples)

plot(density(samples))

######

p <- seq(0,1,length.out = 1000)

prior <- rep(1,1000)

likelihood <- dbinom(6,9,prob=p)

posterior1 <- likelihood*prior/sum(likelihood)

sum(posterior1 - posterior)  ### basically i needed to normalize the prior to convert to density

