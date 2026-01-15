

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
  
  total_pulls <- sum((K:2) * inc)
  
  list(n_k = n_k, inc = inc, total_pulls = total_pulls)
}
















## -------------------------------
## Setup: SR bandit implementation 
## -------------------------------

# ---------- helpers ----------
append_pulls <- function(arms, pulls) {
  # only update arms that are still active
  for (nm in intersect(names(pulls), names(arms))) {
    arms[[nm]] <- c(arms[[nm]], pulls[[nm]])
  }
  return(arms)
}

eliminate_worst <- function(arms) {
  m <- sapply(arms, mean)
  max_m <- max(m)                            # worst = largest mean (since lower lap time is better)
  worst_candidates <- names(m)[m == max_m]
  worst <- sample(worst_candidates, 1)        # random tie-break
  return(
    list(
      arms = arms[setdiff(names(arms), worst)],
      eliminated = worst,
      means = m
    )
  )
}

# ---------- parameters ----------
K_setup = 7 # number of arms
n_setup = 60 # number of possible draws

SR_Setup_parameters <- get_draws_per_phase(K_setup,n_setup)

cat(paste0("Pulls phase ", seq_along(SR_Setup_parameters$inc), ": ", SR_Setup_parameters$inc), sep = "\n")
paste0("Total pulls: ", SR_Setup_parameters$total_pulls)


# ------------------------------------------------------------------------------



# lilst to store arms, means, eliminated arms
history_setup <- setNames(vector("list", (K_setup-1)), paste0("phase", 1:(K_setup-1)))



## first pull - phase 1---------------------------------------------------------
a1 <- c(109.22064735, 108.66264735, 110.10564735, 108.60064735)
a2 <- c(112.0086993, 113.65269925, 113.41069925, 112.56469925)
a3 <- c(114.94669925, 114.25469925, 114.8456993, 114.9606993)
a4 <- c(111.7976993, 111.0846993, 110.7916993, 110.8996993)
a5 <- c(111.2011993, 110.2471993, 110.4151993, 110.0841993)
a6 <- c(109.502565, 109.908565, 108.721565, 108.935565)
a7 <- c(111.4971474, 111.0961474, 110.8611474, 111.0951474)


# list for laptimes of each arm
arms <- list(
  A1 = a1,
  A2 = a2,
  A3 = a3,
  A4 = a4,
  A5 = a5,
  A6 = a6,
  A7 = a7
)

phase_1 <- eliminate_worst(arms)

history_setup[["phase1"]] <- phase_1


# eliminated arm
paste0("Eliminated arm: ", phase_1$eliminated)


## first pull - phase 2---------------------------------------------------------
a1 <- c(108.99364735)
a2 <- c(112.30769925)
a4 <- c(112.11569925)
a5 <- c(110.16719925)
a6 <- c(109.03556495)
a7 <- c(111.71514735)


pull2 <- list(A1 = a1, A2 = a2, A4 = a4, A5 = a5, A6 = a6, A7 = a7)

arms <- append_pulls(phase_1$arms, pull2)

phase_2 <- eliminate_worst(arms)

history_setup[["phase2"]] <- phase_2


# eliminated arm
paste0("Eliminated arm: ", phase_2$eliminated)


## first pull - phase 3---------------------------------------------------------
a1 <- c(108.24064735)
a4 <- c(111.07869925)
a5 <- c(109.45319925)
a6 <- c(109.94656495)
a7 <- c(110.04114735)

pull3 <- list(A1 = a1, A4 = a4, A5 = a5, A6 = a6, A7 = a7)

arms <- append_pulls(phase_2$arms, pull3)

phase_3 <- eliminate_worst(arms)

history_setup[["phase3"]] <- phase_3


# eliminated arm
paste0("Eliminated arm: ", phase_3$eliminated)


## first pull - phase 4---------------------------------------------------------
a1 <- c(109.97264735)
a5 <- c(110.21219925)
a6 <- c(110.01156495)
a7 <- c(110.28614735)


pull4 <- list(A1 = a1, A5 = a5, A6 = a6, A7 = a7)

arms <- append_pulls(phase_3$arms, pull4)

phase_4 <- eliminate_worst(arms)

history_setup[["phase4"]] <- phase_4


# eliminated arm
paste0("Eliminated arm: ", phase_4$eliminated)


## first pull - phase 5---------------------------------------------------------
a1 <- c(108.57064735, 108.90064735)
a5 <- c(110.70919925, 110.92019925)
a6 <- c(108.80356495, 110.11456495)


pull5 <- list(A1 = a1, A5 = a5, A6 = a6)

arms <- append_pulls(phase_4$arms, pull5)

phase_5 <- eliminate_worst(arms)

history_setup[["phase5"]] <- phase_5


# eliminated arm
paste0("Eliminated arm: ", phase_5$eliminated)


## first pull - phase 6---------------------------------------------------------
a1 <- c(109.10764735, 108.14064735, 109.72064735, 109.83464735)
a6 <- c(109.02056495, 109.01656495, 109.48856495, 110.60456495)


pull6 <- list(A1 = a1, A6 = a6)

arms <- append_pulls(phase_5$arms, pull6)

phase_6 <- eliminate_worst(arms)

history_setup[["phase6"]] <- phase_6


# eliminated arm
paste0("Eliminated arm: ", phase_6$eliminated)

# selected arm and lap time
paste0("Selected arm: ", names(phase_6$arms[1]), " - Lap-time: ", phase_6$means[names(phase_6$arms[1])])



















############## Strategie

z = 120 - 116.77089672593
w_extrasoft <- 100 - 87.329884303744
w_soft <- 100 - 94.489656748349
w_medium <- 100 - 95.542888870211
w_hard <- 100 - 96.326579937969



split_laps <- function(b, n = 63) {
  
  S <- b + 1
  
  stopifnot(length(S) == 1, is.numeric(S), S >= 1, S == as.integer(S))
  stopifnot(is.numeric(n), n >= 1, n == as.integer(n))
  
  q <- n %/% S
  r <- n %% S
  
  sizes <- c(rep(q + 1, r), rep(q, S - r))
  names(sizes) <- paste0("part", seq_len(S))
  return(sizes)
}

# get the partition of race laps for b pit stops
max(split_laps(2))
split_laps(3)
split_laps(4)



minimalFuelLoad <- function(w, b, z = 3.22910327407){
  
  # maximum number of laps per stint
  laps <- max(split_laps(b))
  
  minimal_load <- c()
  
  if(laps*w > 100){
    stop("Stint length not possible due to tire degradation!")
  } else if (laps*z > 120){
    stop("Stint length not possible due to fuel consumption!")
  } else{
    # minimal fuel load
    minimal_load <- ceiling(laps * z)
  }
  
  minimal_load
}

### 1 stop -------------------------------
## one stop strategy with hard
#minimalFuelLoad(w_hard, 1) # => not possible


### 2 stop -------------------------------
## two stop strategy with hard
minimalFuelLoad(w_hard, 2) # => 68

## two stop strategy with medium
#minimalFuelLoad(w_medium, 2) # => 68

## two stop strategy with soft
#minimalFuelLoad(w_soft, 2) # => not possible


### 3 stop ------------------------------
## three stop strategy with medium
minimalFuelLoad(w_medium, 3) # => 52

## three stop strategy with soft
minimalFuelLoad(w_soft, 3) # => 52

## three stop strategy with extrasoft
#minimalFuelLoad(w_extrasoft, 3) # => not possible


### 4 stop ------------------------------
## four stop strategy with medium
#minimalFuelLoad(w_medium, 4) # => 42

## four stop strategy with soft
minimalFuelLoad(w_soft, 4) # => 42

## four stop strategy with extrasoft
#minimalFuelLoad(w_extrasoft, 4) # => not possible



K_strategy <- 4
stint_length_per_draw <- 4
n_strategy <- floor(60/stint_length_per_draw)

draws_per_arm <- floor(n_strategy/K_strategy)
draws_per_arm



##


get_draws_per_phase(K_strategy,n_strategy)












































































































# ------------------------------------------------------------------------------



# lilst to store arms, means, eliminated arms
history_setup <- setNames(vector("list", (K_setup-1)), paste0("phase", 1:(K_setup-1)))



## first pull - phase 1---------------------------------------------------------
a1 <- c(0, 0, 0, 0)
a2 <- c(0, 0, 0, 0)
a3 <- c(0, 0, 0, 0)
a4 <- c(0, 0, 0, 0)
a5 <- c(0, 0, 0, 0)
a6 <- c(0, 0, 0, 0)
a7 <- c(0, 0, 0, 0)


# list for laptimes of each arm
arms <- list(
  A1 = a1,
  A2 = a2,
  A3 = a3,
  A4 = a4,
  A5 = a5,
  A6 = a6,
  A7 = a7
)

phase_1 <- eliminate_worst(arms)

history_setup[["phase1"]] <- phase_1


# eliminated arm
paste0("Eliminated arm: ", phase_1$eliminated)


## first pull - phase 2---------------------------------------------------------
ax <- c(0)
ax <- c(0)
ax <- c(0)
ax <- c(0)
ax <- c(0)
ax <- c(0)


pull2 <- list(Ax = ax, Ax = ax, Ax = ax, Ax = ax, Ax = ax, Ax = ax)

arms <- append_pulls(phase_1$arms, pull2)

phase_2 <- eliminate_worst(arms)

history_setup[["phase2"]] <- phase_2


# eliminated arm
paste0("Eliminated arm: ", phase_2$eliminated)


## first pull - phase 3---------------------------------------------------------
ax <- c(0)
ax <- c(0)
ax <- c(0)
ax <- c(0)
ax <- c(0)

pull3 <- list(Ax = ax, Ax = ax, Ax = ax, Ax = ax, Ax = ax)

arms <- append_pulls(phase_2$arms, pull3)

phase_3 <- eliminate_worst(arms)

history_setup[["phase3"]] <- phase_3


# eliminated arm
paste0("Eliminated arm: ", phase_3$eliminated)


## first pull - phase 4---------------------------------------------------------
ax <- c(0)
ax <- c(0)
ax <- c(0)
ax <- c(0)


pull4 <- list(Ax = ax, Ax = ax, Ax = ax, Ax = ax)

arms <- append_pulls(phase_3$arms, pull4)

phase_4 <- eliminate_worst(arms)

history_setup[["phase4"]] <- phase_4


# eliminated arm
paste0("Eliminated arm: ", phase_4$eliminated)


## first pull - phase 5---------------------------------------------------------
ax <- c(0, 0)
ax <- c(0, 0)
ax <- c(0, 0)


pull5 <- list(Ax = ax, Ax = ax, Ax = ax)

arms <- append_pulls(phase_4$arms, pull5)

phase_5 <- eliminate_worst(arms)

history_setup[["phase5"]] <- phase_5


# eliminated arm
paste0("Eliminated arm: ", phase_5$eliminated)


## first pull - phase 6---------------------------------------------------------
ax <- c(0, 0, 0, 0)
ax <- c(0, 0, 0, 0)


pull6 <- list(Ax = ax, Ax = ax)

arms <- append_pulls(phase_5$arms, pull6)

phase_6 <- eliminate_worst(arms)

history_setup[["phase6"]] <- phase_6


# eliminated arm
paste0("Eliminated arm: ", phase_6$eliminated)

# selected arm and lap time
paste0("Selected arm: ", names(phase_6$arms[1]), " - Lap-time: ", phase_6$means[names(phase_6$arms[1])])








