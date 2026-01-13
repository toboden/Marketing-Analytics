






## Define function for Successive Rejects algorithm
get_draws_per_phase <- function(K, n) {
  stopifnot(K >= 2, n >= K)
  
  # \bar{log}(K) = 1/2 + sum_{i=2}^K 1/i
  logK_bar <- 0.5 + sum(1 / (2:K))
  
  k <- seq_len(K - 1)  # 1,2,...,K-1
  
  # n_k (cumulative pulls per active arm up to phase k)
  n_k <- ceiling((1 / logK_bar) * ((n - K) / (K + 1 - k)))
  n_k <- c(0, n_k)
  
  # phase increments: n_k - n_{k-1}, with n_0 = 0
  inc <- diff(n_k)
  
  list(n_k = n_k, inc = inc)
}

K = 7
n = 60

SR_parameters <- get_draws_per_phase(K,n)
print(SR_parameters)







