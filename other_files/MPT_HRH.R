library(quadprog)

# Rwanda Stock 2024
cadres <- c("Nurses", "Midwives", "Dentists", "Pharmacists", "Doctors")
stock  <- c(16252, 2236, 307, 1367, 2517)
mix_all <- stock / sum(stock)

# Annual cost per worker (USD) - placeholders
cost <- c(
  Nurses = 12000,
  Midwives = 14000,
  Dentists = 15000,
  Pharmacists = 10000,
  Doctors = 25000
)[cadres]

# ---- Scenario generator that MATCHES your cadres ----
make_scenarios <- function(
    n_scen = 3000,
    cadres_in = cadres,
    # services per worker-year (replace with Rwanda estimates)
    prod_mean = c(Nurses=2600, Midwives=2000, Dentists=900, Pharmacists=1500, Doctors=1800),
    prod_sd   = c(Nurses=350,  Midwives=300,  Dentists=180, Pharmacists=250,  Doctors=250),
    # DALYs averted per service (replace with evidence/model outputs)
    daly_per_service_mean = c(Nurses=0.006, Midwives=0.012, Dentists=0.002, Pharmacists=0.004, Doctors=0.010),
    daly_per_service_sd   = c(Nurses=0.002, Midwives=0.004, Dentists=0.001, Pharmacists=0.0015, Doctors=0.003),
    # shocks
    shock_prob = 0.12,
    shock_prod_multiplier_mean = 0.85,
    shock_prod_multiplier_sd   = 0.06,
    # availability
    avail_mean = c(Nurses=0.90, Midwives=0.89, Dentists=0.92, Pharmacists=0.90, Doctors=0.88),
    avail_sd   = c(Nurses=0.05, Midwives=0.05, Dentists=0.04, Pharmacists=0.05, Doctors=0.06)
) {
  k <- length(cadres_in)
  out <- matrix(NA_real_, n_scen, k)
  colnames(out) <- cadres_in
  
  # align everything to cadres order
  prod_mean <- prod_mean[cadres_in]; prod_sd <- prod_sd[cadres_in]
  daly_per_service_mean <- daly_per_service_mean[cadres_in]; daly_per_service_sd <- daly_per_service_sd[cadres_in]
  avail_mean <- avail_mean[cadres_in]; avail_sd <- avail_sd[cadres_in]
  
  for (s in 1:n_scen) {
    shock <- runif(1) < shock_prob
    shock_mult <- if (shock) rnorm(1, shock_prod_multiplier_mean, shock_prod_multiplier_sd) else 1.0
    shock_mult <- max(0.4, min(1.1, shock_mult))
    
    prod  <- rnorm(k, prod_mean, prod_sd)
    eff   <- rnorm(k, daly_per_service_mean, daly_per_service_sd)
    avail <- rnorm(k, avail_mean, avail_sd)
    
    prod  <- pmax(0, prod)
    eff   <- pmax(0, eff)
    avail <- pmin(1, pmax(0.5, avail))
    
    out[s, ] <- prod * eff * avail * shock_mult
  }
  out
}

# Scenario DALYs per worker-year
daly_per_worker_scen <- make_scenarios(n_scen = 3000)

# Scenario DALYs per $ (worker-year DALYs / annual cost)
daly_per_dollar_scen <- sweep(daly_per_worker_scen, 2, cost, "/")

# MPT inputs from scenarios
mu <- colMeans(daly_per_dollar_scen)
Sigma <- stats::cov(daly_per_dollar_scen)

# make Sigma numerically safe for quadprog
k <- length(cadres)
Sigma <- Sigma + diag(1e-12, k)

# Bounds around current mix
lower <- pmax(0.02, 0.60 * mix_all); names(lower) <- cadres
upper <- pmin(0.80, 1.60 * mix_all); names(upper) <- cadres

# QP solver
solve_hrh_mpt <- function(mu, Sigma, lower, upper, target_return = NULL) {
  k <- length(mu)
  D <- Sigma
  d <- rep(0, k)
  
  A <- cbind(
    rep(1, k),
    rep(-1, k),
    diag(k),
    -diag(k)
  )
  b <- c(1, -1, lower, -upper)
  
  if (!is.null(target_return)) {
    A <- cbind(mu, A)
    b <- c(target_return, b)
  }
  
  sol <- quadprog::solve.QP(Dmat = D, dvec = d, Amat = A, bvec = b, meq = 0)
  w <- sol$solution
  w[w < 0 & w > -1e-10] <- 0
  
  list(
    w = w,
    exp_return = sum(mu * w),
    variance = as.numeric(t(w) %*% Sigma %*% w),
    sd = sqrt(as.numeric(t(w) %*% Sigma %*% w))
  )
}

# Efficient frontier
set.seed(123)
sample_feasible <- function(n = 12000, lower, upper) {
  k <- length(lower)
  W <- matrix(NA_real_, n, k)
  for (i in 1:n) {
    x <- lower + runif(k) * (upper - lower)
    x <- x / sum(x)
    for (iter in 1:25) {
      x <- pmin(pmax(x, lower), upper)
      x <- x / sum(x)
    }
    W[i,] <- x
  }
  W
}

W_samp <- sample_feasible(12000, lower, upper)
ret_samp <- as.vector(W_samp %*% mu)
targets <- seq(quantile(ret_samp, 0.05), quantile(ret_samp, 0.95), length.out = 30)

frontier <- data.frame(target=targets, exp_return=NA_real_, sd=NA_real_)
for (i in seq_along(targets)) {
  res <- solve_hrh_mpt(mu, Sigma, lower, upper, target_return = targets[i])
  frontier$exp_return[i] <- res$exp_return
  frontier$sd[i] <- res$sd
}

# Plot with fit-friendly fonts + margins
par(mar = c(4, 4, 2.2, 1), cex = 0.85)
plot(frontier$sd, frontier$exp_return, type="b",
     xlab="Risk (SD of DALYs per $)",
     ylab="Expected DALYs per $",
     main="Rwanda HRH Efficient Frontier (DALYs outcome)",
     cex.main=0.9, cex.lab=0.9, cex.axis=0.85)

# Choose target portfolio (80th percentile)
target80 <- as.numeric(quantile(ret_samp, 0.80))
best <- solve_hrh_mpt(mu, Sigma, lower, upper, target_return = target80)

cat("\n--- Rwanda: Target return portfolio (budget shares) ---\n")
names(best$w) <- cadres
print(round(best$w, 4))
cat("Expected DALYs per $:", signif(best$exp_return, 6), "\n")
cat("Risk (SD):", signif(best$sd, 6), "\n")
