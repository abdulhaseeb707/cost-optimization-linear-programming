# Step 2: multi-plant kiln-fuel blending LP for FCCL (FY2025 simulation anchored to public data).
# Solver: lp_solve via the lpSolve package. Run from the project folder: source("R/02_optimization_model.R")

suppressPackageStartupMessages({
  library(lpSolve)
  library(jsonlite)
})

load_inputs <- function(dir = "data/model_inputs") {
  read <- function(f) read.csv(file.path(dir, f), stringsAsFactors = FALSE)
  params_raw <- read("parameters.csv")
  list(
    sources  = read("fuel_sources.csv"),
    plants   = read("plants.csv"),
    routes   = read("routes.csv"),
    params   = setNames(suppressWarnings(as.numeric(params_raw$value)), params_raw$parameter),
    baseline = read("baseline_allocation.csv")
  )
}

decision_variables <- function(inp) {
  vars <- expand.grid(plant_id = inp$plants$plant_id, source_id = inp$sources$source_id,
                      stringsAsFactors = FALSE)[, c("source_id", "plant_id")]
  key <- paste(inp$routes$source_id, inp$routes$plant_id)
  vars$cost <- inp$routes$delivered_cost_pkr_per_t[match(paste(vars$source_id, vars$plant_id), key)]
  s <- match(vars$source_id, inp$sources$source_id)
  vars$ncv <- inp$sources$ncv_gj_per_t[s]
  vars$sulphur <- inp$sources$sulphur_pct[s]
  vars$is_coal <- inp$sources$fuel_type[s] == "coal"
  vars
}

build_model <- function(inp, s_max, tsr_max) {
  vars <- decision_variables(inp)
  rows <- list(); dir <- c(); rhs <- c(); name <- c()
  add <- function(coef, d, r, n) {
    rows[[length(rows) + 1]] <<- coef; dir <<- c(dir, d); rhs <<- c(rhs, r); name <<- c(name, n)
  }
  for (p in inp$plants$plant_id) {
    at_p <- vars$plant_id == p
    add(ifelse(at_p, vars$ncv, 0), ">=", inp$plants$heat_demand_gj[inp$plants$plant_id == p], paste0("heat_", p))
    add(ifelse(at_p & vars$is_coal, vars$sulphur - s_max, 0), "<=", 0, paste0("sulphur_", p))
    add(ifelse(at_p, ifelse(vars$is_coal, -tsr_max * vars$ncv, (1 - tsr_max) * vars$ncv), 0), "<=", 0, paste0("altfuel_", p))
  }
  for (i in inp$sources$source_id) {
    add(as.numeric(vars$source_id == i), "<=", inp$sources$supply_capacity_t[inp$sources$source_id == i], paste0("supply_", i))
  }
  list(vars = vars, obj = vars$cost, A = do.call(rbind, rows), dir = dir, rhs = rhs, name = name)
}

solve_model <- function(m) {
  sol <- lp("min", m$obj, m$A, m$dir, m$rhs, compute.sens = TRUE)
  m$vars$tonnes <- sol$solution
  list(status = sol$status, cost = sol$objval, vars = m$vars,
       duals = data.frame(constraint = sub("_.*", "", m$name), member = sub("^[^_]*_", "", m$name),
                          dual = sol$duals[seq_along(m$rhs)],
                          slack = ifelse(m$dir == "<=", m$rhs - m$A %*% sol$solution, m$A %*% sol$solution - m$rhs)))
}

fix_alt_fuel_at_baseline <- function(m, baseline) {
  alt <- which(!m$vars$is_coal)
  for (k in alt) {
    t <- baseline$tonnes[baseline$source_id == m$vars$source_id[k] & baseline$plant_id == m$vars$plant_id[k]]
    m$A <- rbind(m$A, as.numeric(seq_len(nrow(m$vars)) == k))
    m$dir <- c(m$dir, "="); m$rhs <- c(m$rhs, t); m$name <- c(m$name, paste0("fixed_", m$vars$source_id[k], "_", m$vars$plant_id[k]))
  }
  m
}

constraint_report <- function(vars, tonnes, inp, s_max, tsr_max) {
  out <- list()
  for (p in inp$plants$plant_id) {
    at_p <- vars$plant_id == p
    heat <- sum(vars$ncv[at_p] * tonnes[at_p])
    coal <- at_p & vars$is_coal
    alt <- at_p & !vars$is_coal
    out[[length(out) + 1]] <- data.frame(constraint = "heat", member = p, value = heat, sense = ">=",
                                         limit = inp$plants$heat_demand_gj[inp$plants$plant_id == p], unit = "GJ")
    out[[length(out) + 1]] <- data.frame(constraint = "coal_blend_sulphur", member = p,
                                         value = sum(vars$sulphur[coal] * tonnes[coal]) / sum(tonnes[coal]),
                                         sense = "<=", limit = s_max, unit = "%")
    out[[length(out) + 1]] <- data.frame(constraint = "alt_fuel_heat_share", member = p,
                                         value = sum(vars$ncv[alt] * tonnes[alt]) / heat,
                                         sense = "<=", limit = tsr_max, unit = "fraction")
  }
  for (i in inp$sources$source_id) {
    out[[length(out) + 1]] <- data.frame(constraint = "supply", member = i, value = sum(tonnes[vars$source_id == i]),
                                         sense = "<=", limit = inp$sources$supply_capacity_t[inp$sources$source_id == i], unit = "t")
  }
  rep <- do.call(rbind, out)
  rep$slack <- ifelse(rep$sense == "<=", rep$limit - rep$value, rep$value - rep$limit)
  scale <- pmax(abs(rep$limit), 1)
  rep$satisfied <- rep$slack >= -1e-6 * scale
  rep$binding <- abs(rep$slack) <= 1e-6 * scale
  rep
}

baseline_tonnes <- function(vars, baseline) {
  t <- baseline$tonnes[match(paste(vars$source_id, vars$plant_id), paste(baseline$source_id, baseline$plant_id))]
  ifelse(is.na(t), 0, t)
}

run_model <- function(write_results = TRUE) {
  inp <- load_inputs()
  s_max <- inp$params[["sulphur_limit_coal_blend_pct"]]
  tsr_max <- inp$params[["tsr_max"]]

  m <- build_model(inp, s_max, tsr_max)
  opt <- solve_model(m)
  stopifnot(opt$status == 0)
  coal_only <- solve_model(fix_alt_fuel_at_baseline(m, inp$baseline))
  stopifnot(coal_only$status == 0)

  vars <- opt$vars
  base_t <- baseline_tonnes(vars, inp$baseline)
  base_cost <- sum(base_t * vars$cost)
  opt_cost <- sum(vars$tonnes * vars$cost)

  rep_base <- constraint_report(vars, base_t, inp, s_max, tsr_max)
  rep_opt <- constraint_report(vars, vars$tonnes, inp, s_max, tsr_max)
  stopifnot(all(rep_base$satisfied), all(rep_opt$satisfied))

  allocation <- data.frame(source_id = vars$source_id, plant_id = vars$plant_id,
                           baseline_t = round(base_t, 4), optimized_t = round(vars$tonnes, 4),
                           baseline_cost_pkr = round(base_t * vars$cost, 4),
                           optimized_cost_pkr = round(vars$tonnes * vars$cost, 4),
                           optimized_heat_gj = round(vars$tonnes * vars$ncv, 4))
  heat_total <- sum(inp$plants$heat_demand_gj)
  summary <- list(
    status = "Optimal",
    solver = "lp_solve 5.5 via R lpSolve",
    settings = list(s_max = s_max, tsr_max = tsr_max),
    baseline_cost_pkr = round(base_cost),
    optimized_cost_pkr = round(opt_cost),
    saving_pkr = round(base_cost - opt_cost),
    saving_pct = round((base_cost - opt_cost) / base_cost * 100, 3),
    saving_from_coal_reallocation_pkr = round(base_cost - coal_only$cost),
    saving_from_extra_alt_fuel_pkr = round(coal_only$cost - opt_cost),
    total_heat_gj = round(heat_total),
    baseline_cost_per_gj = round(base_cost / heat_total, 2),
    optimized_cost_per_gj = round(opt_cost / heat_total, 2),
    binding_constraints = paste(rep_opt$constraint, rep_opt$member, sep = ":")[rep_opt$binding]
  )

  if (write_results) {
    dir.create("results", showWarnings = FALSE)
    write.csv(allocation, "results/allocation_comparison.csv", row.names = FALSE)
    write.csv(transform(rep_base, value = round(value, 6), slack = round(slack, 6)), "results/constraints_baseline.csv", row.names = FALSE)
    write.csv(transform(rep_opt, value = round(value, 6), slack = round(slack, 6)), "results/constraints_optimized.csv", row.names = FALSE)
    write.csv(transform(opt$duals, dual = round(dual, 6), slack = round(slack, 6)), "results/shadow_prices.csv", row.names = FALSE)
    write_json(summary, "results/summary.json", auto_unbox = TRUE, pretty = TRUE, digits = NA)
  }
  invisible(list(summary = summary, allocation = allocation, report = rep_opt, duals = opt$duals))
}

if (sys.nframe() == 0 || identical(environment(), globalenv())) {
  res <- run_model()
  s <- res$summary
  cat("\n================ RESULT ================\n")
  cat(sprintf("Baseline fuel cost : Rs %s\n", format(s$baseline_cost_pkr, big.mark = ",")))
  cat(sprintf("Optimized fuel cost: Rs %s\n", format(s$optimized_cost_pkr, big.mark = ",")))
  cat(sprintf("Saving             : Rs %s (%.2f%%)\n", format(s$saving_pkr, big.mark = ","), s$saving_pct))
  cat(sprintf("  from coal reallocation : Rs %s\n", format(s$saving_from_coal_reallocation_pkr, big.mark = ",")))
  cat(sprintf("  from extra alt fuel    : Rs %s\n", format(s$saving_from_extra_alt_fuel_pkr, big.mark = ",")))
  cat("\nOptimized tonnes (rows = fuel, columns = plant):\n")
  print(round(xtabs(optimized_t ~ source_id + plant_id, res$allocation)))
  cat("\nBinding constraints:", paste(s$binding_constraints, collapse = ", "), "\n")
}
