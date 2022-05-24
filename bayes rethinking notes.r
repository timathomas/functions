# ==========================================================================
# Bayes rethinking notes
# ==========================================================================

p_grid <- seq(from=0, to = 1, len = 1000)
prob_p <- rep(1, 1000)
prob_data <- dbinom(6,9,prob = p_grid)
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)
plot(posterior)

p_grid <- seq(from=0, to = 1, len = 1000)
prob_p <- dbeta(p_grid, 3, 1) # prior
prob_data <- dbinom(6,9,prob = p_grid)
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)
plot(posterior)

samples <- sample(p_grid, prob=posterior, size=1e4, replace = TRUE)

w <- rbinom(1e4, size = 9, prob=samples)