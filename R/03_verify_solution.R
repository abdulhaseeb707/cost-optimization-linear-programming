# Step 3: independent check. Rebuilds the LP cell by cell and solves it with a different solver (GLPK via Rglpk).
# Run after 02_optimization_model.R: source("R/03_verify_solution.R")

suppressPackageStartupMessages({
  library(Rglpk)
  library(jsonlite)
})

src <- read.csv("data/model_inputs/fuel_sources.csv", stringsAsFactors = FALSE)
pl <- read.csv("data/model_inputs/plants.csv", stringsAsFactors = FALSE)
rt <- read.csv("data/model_inputs/routes.csv", stringsAsFactors = FALSE)
pr <- read.csv("data/model_inputs/parameters.csv", stringsAsFactors = FALSE)
s_max <- as.numeric(pr$value[pr$parameter == "sulphur_limit_coal_blend_pct"])
tsr_max <- as.numeric(pr$value[pr$parameter == "tsr_max"])

solve_glpk <- function(capacity, demand) {
  fuels <- src$source_id; sites <- pl$plant_id
  n <- length(fuels) * length(sites)
  col <- function(i, p) (match(i, fuels) - 1) * length(sites) + match(p, sites)
  obj <- numeric(n)
  for (i in fuels) for (p in sites) obj[col(i, p)] <- rt$delivered_cost_pkr_per_t[rt$source_id == i & rt$plant_id == p]

  A <- matrix(0, nrow = 0, ncol = n); dir <- character(0); rhs <- numeric(0)
  for (p in sites) {
    heat <- numeric(n); sul <- numeric(n); alt <- numeric(n)
    for (k in seq_along(fuels)) {
      i <- fuels[k]; e <- src$ncv_gj_per_t[k]; coal <- src$fuel_type[k] == "coal"
      heat[col(i, p)] <- e
      if (coal) sul[col(i, p)] <- src$sulphur_pct[k] - s_max
      alt[col(i, p)] <- if (coal) -tsr_max * e else (1 - tsr_max) * e
    }
    A <- rbind(A, heat, sul, alt)
    dir <- c(dir, ">=", "<=", "<=")
    rhs <- c(rhs, demand[p], 0, 0)
  }
  for (i in fuels) {
    row <- numeric(n)
    for (p in sites) row[col(i, p)] <- 1
    A <- rbind(A, row); dir <- c(dir, "<="); rhs <- c(rhs, capacity[i])
  }
  res <- Rglpk_solve_LP(obj, A, dir, rhs, max = FALSE)
  excess <- ifelse(dir == "<=", A %*% res$solution - rhs, rhs - A %*% res$solution)
  list(status = res$status, cost = res$optimum, max_excess = max(excess))
}

capacity <- setNames(src$supply_capacity_t, src$source_id)
demand <- setNames(pl$heat_demand_gj, pl$plant_id)
base <- solve_glpk(capacity, demand)
summary <- fromJSON("results/summary.json")
duals <- read.csv("results/shadow_prices.csv", stringsAsFactors = FALSE)

gap <- abs(base$cost - summary$optimized_cost_pkr) / summary$optimized_cost_pkr
cap_bio <- capacity; cap_bio["BIO"] <- cap_bio["BIO"] + 1000
dem_jb <- demand; dem_jb["JB"] <- dem_jb["JB"] + 1000
d_bio <- solve_glpk(cap_bio, demand)$cost - base$cost
d_jb <- solve_glpk(capacity, dem_jb)$cost - base$cost

cat("\n============ INDEPENDENT CHECK ============\n")
cat(sprintf("GLPK status                : %s\n", ifelse(base$status == 0, "optimal", base$status)))
cat(sprintf("GLPK minimum cost          : Rs %s\n", format(round(base$cost), big.mark = ",")))
cat(sprintf("lpSolve minimum cost       : Rs %s\n", format(summary$optimized_cost_pkr, big.mark = ",")))
cat(sprintf("Relative difference        : %.2e\n", gap))
cat(sprintf("Worst constraint excess    : %.2e\n", base$max_excess))
cat(sprintf("+1,000 t biomass changes cost by Rs %s (shadow price predicts %s)\n",
            format(round(d_bio), big.mark = ","), format(round(1000 * duals$dual[duals$constraint == "supply" & duals$member == "BIO"]), big.mark = ",")))
cat(sprintf("+1,000 GJ heat at JB changes cost by Rs %s (shadow price predicts %s)\n",
            format(round(d_jb), big.mark = ","), format(round(1000 * duals$dual[duals$constraint == "heat" & duals$member == "JB"]), big.mark = ",")))
stopifnot(base$status == 0, gap < 1e-6, base$max_excess < 1e-3)
cat("All checks passed.\n")
