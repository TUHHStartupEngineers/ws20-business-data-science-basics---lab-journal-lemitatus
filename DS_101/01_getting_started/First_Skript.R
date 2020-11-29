# Introduction Data Science



# Exercise 1 - EOQ 
D <- 1000
K <- 5
h <- 0.25
Q <- sqrt(2*D*K/h)


# A few commands/functions
die <- 1:6
dice <- sample(die, size=2, replace = TRUE)
sum(dice)


# My first function
roll <- function(die = 1:6, rolls = 2) {
dice <- sample(die, size=rolls, replace = TRUE)
sum(dice)
}

# EOQ formula function
EOQ <- function(D=1000) {
  K <- 5
  h <- 0.25
  Q <- sqrt(2*D*K/h)
  Q
}

roll2 <- function(rolls = 1) {
  die <- 1:6
  probab <- c(1/10, 1/10, 1/10, 1/10, 1/10, 1/2)
  dice <- sample(die, size=rolls, replace = TRUE, prob=probab)
  sum(dice)
}
results <- replicate(n=100, expr=roll2(), simplify=TRUE)
hist(results)
