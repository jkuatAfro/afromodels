library(quadprog)
set.seed(123)

# Republic of the Congo HRH MPT model with CHWs
# Modeled cadres sum to alpha (< 1), leaving residual share for other unmodeled cadres in the health workforce.
#  total modeled budget share
alpha <- 0.70   #modeled cadres represent 70% of total HRH budget

#  Republic of the Congo Stock 2024 
cadres <- c("CHWs", "Nurses", "Midwives", "Dentists", "Pharmacists", "Doctors")

# Republic of the Congo-specific stock values
stock <- c(
  CHWs        =058567,
  Nurses      = 16252,
  Midwives    = 2236,
  Dentists    =307,
  Pharmacists = 1367,
  Doctors     = 2517
)[cadres]

# Mix within modeled cadres only
mix_all <- stock / sum(stock)

# ---- Monthly cost per worker (USD)
#obtained from just searching

cost_low_m <- c(
  CHWs        = 100,
  Nurses      = 300,
  Midwives    = 300,
  Dentists    = 500,
  Pharmacists = 600,
  Doctors     = 800
)[cadres]

cost_high_m <- c(
  CHWs        = 500,
  Nurses      = 1200,
  Midwives    = 1400,
  Dentists    = 3000,
  Pharmacists = 3500,
  Doctors     = 5000
)[cadres]

# Annual cost per worker (USD/year)
cost_low  <- 12 * cost_low_m
cost_high <- 12 * cost_high_m

stopifnot(all(cost_low > 0), all(cost_high >= cost_low))

#Scenario generation for DALYs per worker-year 
make_scenarios <- function(
    n_scen = 3000,
    cadres_in = cadres,
    work_days = 252,
    hours_day = 8,
    util = 0.75,
    t_service = c(
      CHWs        = 20,
      Nurses      = 15,
      Midwives    = 30,
      Dentists    = 4,
      Pharmacists = 5,
      Doctors     = 22
    ),
    cv = c(
      CHWs        = 0.25,
      Nurses      = 0.20,
      Midwives    = 0.25,
      Dentists    = 0.30,
      Pharmacists = 0.20,
      Doctors     = 0.25
    ),
    daly_per_service_mean = c(
      CHWs        = 0.0035,
      Nurses      = 0.0060,
      Midwives    = 0.0120,
      Dentists    = 0.0020,
      Pharmacists = 0.0040,
      Doctors     = 0.0100
    ),
    daly_per_service_sd = c(
      CHWs        = 0.0012,
      Nurses      = 0.0020,
      Midwives    = 0.0040,
      Dentists    = 0.0010,
      Pharmacists = 0.0015,
      Doctors     = 0.0030
    ),
    shock_prob = 0.12,
    shock_prod_multiplier_mean = 0.85,
    shock_prod_multiplier_sd = 0.06,
    avail_mean = c(
      CHWs        = 0.85,
      Nurses      = 0.90,
      Midwives    = 0.89,
      Dentists    = 0.92,
      Pharmacists = 0.90,
      Doctors     = 0.88
    ),
    avail_sd = c(
      CHWs        = 0.07,
      Nurses      = 0.05,
      Midwives    = 0.05,
      Dentists    = 0.04,
      Pharmacists = 0.05,
      Doctors     = 0.06
    ),
    avail_floor = 0.50
) {
  k <- length(cadres_in)
  out <- matrix(NA_real_, n_scen, k)
  colnames(out) <- cadres_in
  
  t_service <- t_service[cadres_in]
  cv <- cv[cadres_in]
  daly_per_service_mean <- daly_per_service_mean[cadres_in]
  daly_per_service_sd   <- daly_per_service_sd[cadres_in]
  avail_mean <- avail_mean[cadres_in]
  avail_sd   <- avail_sd[cadres_in]
  
  minutes_year <- work_days * hours_day * 60 * util
  prod_mean <- minutes_year / t_service
  prod_sd   <- prod_mean * cv
  
  for (s in seq_len(n_scen)) {
    shock <- runif(1) < shock_prob
    shock_mult <- if (shock) {
      rnorm(1, shock_prod_multiplier_mean, shock_prod_multiplier_sd)
    } else {
      1.0
    }
    shock_mult <- max(0.4, min(1.1, shock_mult))
    
    prod  <- rnorm(k, prod_mean, prod_sd)
    eff   <- rnorm(k, daly_per_service_mean, daly_per_service_sd)
    avail <- rnorm(k, avail_mean, avail_sd)
    
    prod  <- pmax(0, prod)
    eff   <- pmax(0, eff)
    avail <- pmin(1, pmax(avail_floor, avail))
    
    out[s, ] <- prod * eff * avail * shock_mult
  }
  
  out
}

daly_per_worker_scen <- make_scenarios(n_scen = 3000)

# Cost scenarios (USD/year)
make_cost_scenarios_uniform <- function(n_scen, cost_low, cost_high) {
  k <- length(cost_low)
  out <- matrix(NA_real_, n_scen, k)
  colnames(out) <- names(cost_low)
  for (s in seq_len(n_scen)) {
    out[s, ] <- runif(k, min = cost_low, max = cost_high)
  }
  out
}

cost_scen <- make_cost_scenarios_uniform(
  n_scen = nrow(daly_per_worker_scen),
  cost_low = cost_low,
  cost_high = cost_high
)

# DALYs per USD 
daly_per_USD_scen <- daly_per_worker_scen / cost_scen
mu <- colMeans(daly_per_USD_scen)
Sigma <- stats::cov(daly_per_USD_scen)
Sigma <- Sigma + diag(1e-12, length(cadres))

#  Feasible bounds around current mix 
# Bounds are first defined within the modeled sub-portfolio, then scaled by alpha to make the total modeled share sum to alpha.

min_floor <- 0.001
floor_vec <- ifelse(mix_all >= min_floor, min_floor, 0)

lower_raw <- pmax(floor_vec, 0.60 * mix_all)
upper_raw <- pmin(0.80, 1.60 * mix_all)

# This ensures per-cadre feasibility before scaling
lower_within <- pmin(lower_raw, upper_raw)
upper_within <- upper_raw

# This scales to total modeled share alpha
lower <- lower_within * alpha
upper <- upper_within * alpha

names(lower) <- cadres
names(upper) <- cadres

stopifnot(all(lower <= upper + 1e-12))
stopifnot(sum(lower) <= alpha + 1e-12)
stopifnot(sum(upper) >= alpha - 1e-12)

# QP solver 
solve_hrh_mpt <- function(mu, Sigma, lower, upper, alpha, target_return = NULL) {
  k <- length(mu)
  D <- (Sigma + t(Sigma)) / 2
  d <- rep(0, k)
  
  if (is.null(target_return)) {
    # Minimize risk subject to sum(w)=alpha and bounds
    Amat <- cbind(rep(1, k), diag(k), -diag(k))
    bvec <- c(alpha, lower, -upper)
    meq <- 1
  } else {
    # Minimize risk subject to sum(w)=alpha, return >= target, and bounds
    Amat <- cbind(rep(1, k), mu, diag(k), -diag(k))
    bvec <- c(alpha, target_return, lower, -upper)
    meq <- 1
  }
  
  sol <- quadprog::solve.QP(
    Dmat = D,
    dvec = d,
    Amat = Amat,
    bvec = bvec,
    meq  = meq
  )
  
  w <- sol$solution
  w[abs(w) < 1e-12] <- 0
  
  list(
    w = w,
    exp_return = sum(mu * w),
    variance = as.numeric(t(w) %*% Sigma %*% w),
    sd = sqrt(as.numeric(t(w) %*% Sigma %*% w))
  )
}

#Sample feasible portfolios
sample_feasible <- function(n = 12000, lower, upper, alpha) {
  k <- length(lower)
  W <- matrix(NA_real_, n, k)
  
  for (i in seq_len(n)) {
    x <- lower + runif(k) * (upper - lower)
    x <- x * (alpha / sum(x))
    
    for (iter in 1:50) {
      x <- pmin(pmax(x, lower), upper)
      x <- x * (alpha / sum(x))
    }
    
    W[i, ] <- x
  }
  
  colnames(W) <- names(lower)
  W
}

W_samp <- sample_feasible(
  n = 12000,
  lower = lower,
  upper = upper,
  alpha = alpha
)

ret_samp <- as.vector(W_samp %*% mu)

targets <- seq(
  as.numeric(quantile(ret_samp, 0.10)),
  as.numeric(quantile(ret_samp, 0.90)),
  length.out = 30
)

# Efficient frontier 
frontier <- data.frame(
  target = targets,
  exp_return = NA_real_,
  sd = NA_real_
)

for (i in seq_along(targets)) {
  res <- tryCatch(
    solve_hrh_mpt(
      mu = mu,
      Sigma = Sigma,
      lower = lower,
      upper = upper,
      alpha = alpha,
      target_return = targets[i]
    ),
    error = function(e) NULL
  )
  
  if (!is.null(res)) {
    frontier$exp_return[i] <- res$exp_return
    frontier$sd[i] <- res$sd
  }
}

frontier <- frontier[complete.cases(frontier), ]

#  Plot
par(mar = c(4, 4, 2.2, 1), cex = 0.85)
plot(
  frontier$sd, frontier$exp_return, type = "b",
  xlab = "Risk (SD of DALYs per USD)",
  ylab = "Expected DALYs per USD",
  main = paste0(
    "Republic of the Congo HRH Efficient Frontier with CHWs\n",
    "(modeled share = ", alpha * 100, "% of total HRH budget)"
  ),
  cex.main = 0.9, cex.lab = 0.9, cex.axis = 0.85
)

# Choose target portfolio (80th percentile)
target80 <- as.numeric(quantile(ret_samp, 0.80))

best <- solve_hrh_mpt(
  mu = mu,
  Sigma = Sigma,
  lower = lower,
  upper = upper,
  alpha = alpha,
  target_return = target80
)

names(best$w) <- cadres

# Within-modeled-cadres normalized mix
w_modeled_only <- best$w / sum(best$w)
names(w_modeled_only) <- cadres

# Residual share for unmodeled cadres
other_cadres_share <- 1 - sum(best$w)

cat("\n--- Republic of the Congo: Target return portfolio with CHWs ---\n")
cat("Modeled share alpha:", alpha, "\n")
cat("Residual share for other cadres:", round(other_cadres_share, 4), "\n\n")

cat("--- Budget shares as proportion of TOTAL HRH budget ---\n")
print(round(best$w, 4))
cat("Total modeled share:", round(sum(best$w), 4), "\n\n")

cat("--- Budget shares normalized WITHIN modeled cadres only ---\n")
print(round(w_modeled_only, 4))
cat("Within-modeled total:", round(sum(w_modeled_only), 4), "\n\n")

cat("Expected DALYs per USD:", signif(best$exp_return, 6), "\n")
cat("Risk (SD):", signif(best$sd, 6), "\n")

cat("\n--- Bounds used (shares of TOTAL HRH budget) ---\n")
print(data.frame(
  cadre = cadres,
  stock = stock,
  modeled_mix = round(mix_all, 6),
  lower_total_budget = round(lower, 6),
  upper_total_budget = round(upper, 6)
))