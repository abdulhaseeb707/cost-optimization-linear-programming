# Step 5: scenarios. Re-solves the same LP with changed inputs and compares each run with the base case.
# Run after 02_optimization_model.R: source("R/05_scenarios.R")

options(scipen = 99)   # write 300000, not 3e+05
model <- new.env()
sys.source("R/02_optimization_model.R", envir = model)   # functions only; the script's own run is skipped

base <- model$load_inputs()
S_MAX <- base$params[["sulphur_limit_coal_blend_pct"]]
TSR_MAX <- base$params[["tsr_max"]]
HEAT <- sum(base$plants$heat_demand_gj)

# Delivered cost = price at origin x coal_price (coal only) + freight x freight. Factors of 1 give the base case.
reprice <- function(inp, coal_price = 1, freight = 1) {
  price <- setNames(inp$sources$price_pkr_per_t, inp$sources$source_id)
  coal <- inp$sources$source_id[inp$sources$fuel_type == "coal"]
  factor <- ifelse(inp$routes$source_id %in% coal, coal_price, 1)
  inp$routes$delivered_cost_pkr_per_t <- unname(price[inp$routes$source_id]) * factor + inp$routes$freight_pkr_per_t * freight
  inp
}

set_supply <- function(inp, supply) {
  for (i in names(supply)) inp$sources$supply_capacity_t[inp$sources$source_id == i] <- supply[[i]]
  inp
}

run_scenario <- function(coal_price = 1, freight = 1, supply = NULL, tsr = TSR_MAX) {
  inp <- reprice(base, coal_price, freight)
  if (!is.null(supply)) inp <- set_supply(inp, supply)
  sol <- model$solve_model(model$build_model(inp, S_MAX, tsr))
  if (sol$status != 0) return(NULL)
  v <- sol$vars
  base_t <- model$baseline_tonnes(v, inp$baseline)
  # The FY25 baseline mix is only a fair comparison if the scenario still allows it
  base_ok <- all(tapply(base_t, v$source_id, sum)[inp$sources$source_id] <= inp$sources$supply_capacity_t + 1e-6)
  heat <- v$tonnes * v$ncv
  list(cost = sum(v$tonnes * v$cost),
       baseline_cost = if (base_ok) sum(base_t * v$cost) else NA,
       alt_fuel_share = sum(heat[!v$is_coal]) / sum(heat),
       tonnes = tapply(v$tonnes, v$source_id, sum))
}

scenarios <- list(
  list(name = "Base case"),
  list(name = "Afghan coal unavailable", supply = c(AFG = 0)),   # border closed to coal in Oct 2025 (S23)
  list(name = "Coal prices +10%", coal_price = 1.10),
  list(name = "Coal prices -10%", coal_price = 0.90),
  list(name = "Freight rates +25%", freight = 1.25),
  list(name = "Alt-fuel cap 10% of heat", tsr = 0.10),
  list(name = "Alt-fuel cap 20% of heat", tsr = 0.20),
  list(name = "Biomass and tyre supply +50%",
       supply = with(base$sources, setNames(supply_capacity_t[fuel_type == "alt_fuel"] * 1.5, source_id[fuel_type == "alt_fuel"])))
)

rows <- lapply(scenarios, function(s) {
  r <- run_scenario(coal_price = if (is.null(s$coal_price)) 1 else s$coal_price,
                    freight = if (is.null(s$freight)) 1 else s$freight,
                    supply = s$supply,
                    tsr = if (is.null(s$tsr)) TSR_MAX else s$tsr)
  if (is.null(r)) stop("Scenario has no feasible solution: ", s$name)
  data.frame(scenario = s$name,
             optimized_cost_pkr = round(r$cost),
             cost_per_gj = round(r$cost / HEAT, 2),
             baseline_cost_pkr = round(r$baseline_cost),
             saving_vs_baseline_pct = round((r$baseline_cost - r$cost) / r$baseline_cost * 100, 3),
             alt_fuel_heat_share = round(r$alt_fuel_share, 4),
             afghan_t = round(r$tonnes[["AFG"]]), balochistan_t = round(r$tonnes[["BAL"]]),
             salt_range_t = round(r$tonnes[["SRC"]]), imported_t = round(r$tonnes[["IMP"]]),
             biomass_t = round(r$tonnes[["BIO"]]), tyre_t = round(r$tonnes[["TDF"]]))
})
out <- do.call(rbind, rows)
out$change_vs_base_pkr <- out$optimized_cost_pkr - out$optimized_cost_pkr[1]
out$change_vs_base_pct <- round(out$change_vs_base_pkr / out$optimized_cost_pkr[1] * 100, 3)

summary <- jsonlite::fromJSON("results/summary.json")
stopifnot(abs(out$optimized_cost_pkr[1] - summary$optimized_cost_pkr) <= 1)   # base case must match step 2

# Sensitivity: how the optimum moves as the alternative-fuel cap is loosened or tightened
caps <- seq(0.05, 0.25, by = 0.01)
sweep <- data.frame(alt_fuel_cap = caps, optimized_cost_pkr = sapply(caps, function(k) round(run_scenario(tsr = k)$cost)))
sweep$cost_per_gj <- round(sweep$optimized_cost_pkr / HEAT, 2)

write.csv(out, "results/scenarios.csv", row.names = FALSE, na = "")
write.csv(sweep, "results/sensitivity_alt_fuel_cap.csv", row.names = FALSE)

cat("\n================ SCENARIOS ================\n")
show <- transform(out[, c("scenario", "optimized_cost_pkr", "change_vs_base_pct", "saving_vs_baseline_pct", "imported_t")],
                  optimized_cost_pkr = format(optimized_cost_pkr, big.mark = ","))
print(show, row.names = FALSE)
cat("\nAlt-fuel cap sweep (Rs/GJ):", paste(sprintf("%.0f%%=%.0f", 100 * sweep$alt_fuel_cap, sweep$cost_per_gj), collapse = "  "), "\n")
