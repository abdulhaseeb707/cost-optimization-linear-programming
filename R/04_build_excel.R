# Step 4: build excel/cost_optimization_model.xlsx, the Excel Solver version of the same LP.
# Run after 02_optimization_model.R: source("R/04_build_excel.R")

suppressPackageStartupMessages({
  library(openxlsx)
  library(jsonlite)
})

sources <- read.csv("data/model_inputs/fuel_sources.csv", stringsAsFactors = FALSE)
plants <- read.csv("data/model_inputs/plants.csv", stringsAsFactors = FALSE)
routes <- read.csv("data/model_inputs/routes.csv", stringsAsFactors = FALSE)
params_raw <- read.csv("data/model_inputs/parameters.csv", stringsAsFactors = FALSE)
param <- function(name) as.numeric(params_raw$value[params_raw$parameter == name])
alloc <- read.csv("results/allocation_comparison.csv", stringsAsFactors = FALSE)
duals <- read.csv("results/shadow_prices.csv", stringsAsFactors = FALSE)
summary <- fromJSON("results/summary.json")

FUELS <- sources$source_id
SITES <- plants$plant_id
COLS <- c("B", "C", "D", "E")
FUEL_ROW <- setNames(6:(5 + length(FUELS)), FUELS)
SITE_ROW <- setNames(15:(14 + length(SITES)), SITES)

FMT_T <- '#,##0;(#,##0);"-"'
FMT_PCT <- '0.00%;(0.00%);"-"'
COLOUR <- c(body = "#000000", input = "#0000FF", link = "#008000", note = "#595959")

wb <- createWorkbook(creator = "Abdul Haseeb Khan")
style <- function(kind = "body", fmt = NULL, fill = NULL, bold = FALSE, size = 10, italic = kind == "note", wrap = FALSE) {
  deco <- c(if (bold) "bold", if (italic) "italic")
  createStyle(fontName = "Arial", fontSize = size, fontColour = COLOUR[[kind]],
              numFmt = if (is.null(fmt)) "GENERAL" else fmt, fgFill = fill,
              textDecoration = if (length(deco)) deco else NULL, wrapText = wrap)
}

put <- function(sheet, ref, value, kind = "body", fmt = NULL, fill = NULL, bold = FALSE, size = 10, note = NULL) {
  row <- as.integer(gsub("[A-Z]", "", ref))
  col <- col2int(gsub("[0-9]", "", ref))
  if (is.character(value) && startsWith(value, "=")) {
    writeFormula(wb, sheet, x = substring(value, 2), startCol = col, startRow = row)
  } else {
    writeData(wb, sheet, x = value, startCol = col, startRow = row, colNames = FALSE)
  }
  addStyle(wb, sheet, style(kind, fmt, fill, bold, size), rows = row, cols = col)
  if (!is.null(note)) writeComment(wb, sheet, col = col, row = row, comment = createComment(note, author = "Abdul Haseeb Khan", visible = FALSE))
}

header_row <- function(sheet, row, labels, start_col = 1) {
  writeData(wb, sheet, x = t(labels), startCol = start_col, startRow = row, colNames = FALSE)
  addStyle(wb, sheet, createStyle(fontName = "Arial", fontSize = 10, textDecoration = "bold", fgFill = "#D9E1F2",
                                  border = "TopBottomLeftRight", borderColour = "#BFBFBF", wrapText = TRUE, valign = "center"),
           rows = row, cols = start_col:(start_col + length(labels) - 1))
}

# README
addWorksheet(wb, "README")
setColWidths(wb, "README", 1, 110)
readme <- list(
  c("Kiln Fuel Blending Model (FY2025) - Excel Solver version", "title"),
  c("Simulation anchored to public data. Not FCCL's actual procurement plan; FCCL did not use or endorse this model.", "note"),
  c("", "body"),
  c("Colour legend", "head"),
  c("Blue text = input (published, calculated in R/01_build_dataset.R, assumed or simulated - see cell notes and data/data_dictionary.csv)", "input"),
  c("Black text = formula   |   Green text = link to another sheet   |   Yellow fill = decision variables Solver changes", "body"),
  c("", "body"),
  c("Sheets", "head"),
  c("Inputs - fuel quality, capacities, prices, freight, heat demand, limits", "body"),
  c("Model - decision variables (tonnes), objective (total cost) and every constraint", "body"),
  c("Baseline - FY25 disclosed fuel mix applied at every plant (the comparison case)", "body"),
  c("Results - baseline vs optimized, saving, allocation, shadow prices, R cross-check", "body"),
  c("", "body"),
  c("How to run Solver (Windows Excel)", "head"),
  c("1. File > Options > Add-ins > Manage Excel Add-ins > Go > tick Solver Add-in > OK.", "body"),
  c("2. On the Model sheet, select B5:E10 and press Delete (sets all decision variables to zero).", "body"),
  c("3. Data > Solver. Set Objective: $B$13. To: Min. By Changing Variable Cells: $B$5:$E$10.", "body"),
  c("4. Add constraints:  $B$17:$E$17 >= $B$18:$E$18 (heat)  |  $B$19:$E$19 <= 0 (sulphur)  |  $B$22:$E$22 <= 0 (alt-fuel cap)  |  $F$5:$F$10 <= $G$5:$G$10 (supply).", "body"),
  c("5. Tick 'Make Unconstrained Variables Non-Negative'. Select a Solving Method: Simplex LP. Click Solve.", "body"),
  c("6. Choose 'Keep Solver Solution' and tick 'Sensitivity' under Reports to get shadow prices.", "body"),
  c("7. Check Model!B30 (difference vs R). It should be about 0. A different biomass/tyre split per plant with the same cost is an alternative optimum.", "body"),
  c("Steps 3-5 may already be filled in when you open Solver: the workbook stores these settings.", "note"),
  c("", "body"),
  c("No Excel? LibreOffice Calc: Tools > Solver, same settings, engine 'LibreOffice Linear Solver'.", "note")
)
for (k in seq_along(readme)) {
  kind <- readme[[k]][2]
  put("README", paste0("A", k), readme[[k]][1],
      kind = if (kind %in% c("title", "head")) "body" else kind,
      bold = kind %in% c("title", "head"), size = if (kind == "title") 14 else 10)
}

# Inputs
addWorksheet(wb, "Inputs")
setColWidths(wb, "Inputs", 1:10, c(12, 30, 12, 13, 12, 16, 16, 18, 16, 48))
put("Inputs", "A1", "Inputs - FY2025", bold = TRUE, size = 14)
put("Inputs", "A2", "Sources and data types: references/sources.md and data/data_dictionary.csv. V = verified, C = calculated, A = assumption, S = simulated.", "note")
put("Inputs", "A4", "Fuel sources", bold = TRUE)
header_row("Inputs", 5, c("ID", "Fuel", "Type", "NCV (GJ/t)", "Sulphur (%)", "Supply capacity (t/yr)",
                          "Price at origin (Rs/t)", "Sulphur minus limit (coal only)", "Alt-fuel NCV (alt only)", "Data types"))
for (k in seq_along(FUELS)) {
  s <- sources[k, ]; r <- FUEL_ROW[[k]]
  put("Inputs", paste0("A", r), s$source_id)
  put("Inputs", paste0("B", r), s$source_name)
  put("Inputs", paste0("C", r), s$fuel_type)
  put("Inputs", paste0("D", r), s$ncv_gj_per_t, "input", "0.000", note = sprintf("%s %s (%s); converted in R/01_build_dataset.R", s$cv_basis, s$cv_reported, s$cv_source))
  put("Inputs", paste0("E", r), s$sulphur_pct, "input", "0.00", note = paste("Source", s$sulphur_source))
  put("Inputs", paste0("F", r), s$supply_capacity_t, "input", FMT_T, note = "Simulated (S)")
  put("Inputs", paste0("G", r), round(s$price_pkr_per_t), "input", FMT_T, note = paste0(s$price_type, ": ", s$price_basis))
  put("Inputs", paste0("H", r), sprintf('=IF(C%d="coal",E%d-$C$24,0)', r, r), fmt = "0.0000")
  put("Inputs", paste0("I", r), sprintf('=IF(C%d="alt_fuel",D%d,0)', r, r), fmt = "0.000")
  put("Inputs", paste0("J", r), paste("NCV/sulphur V or C; capacity S; price", s$price_type), "note")
}

put("Inputs", "A13", "Plants", bold = TRUE)
header_row("Inputs", 14, c("ID", "Plant", "Capacity (t/day)", "Clinker (t)", "Heat demand (GJ)"))
for (k in seq_along(SITES)) {
  p <- plants[k, ]; r <- SITE_ROW[[k]]
  put("Inputs", paste0("A", r), p$plant_id)
  put("Inputs", paste0("B", r), p$plant_name)
  put("Inputs", paste0("C", r), p$cement_capacity_tpd, "input", FMT_T, note = "V: FCCL Manufacturing Plants page (S20)")
  put("Inputs", paste0("D", r), p$clinker_t, "input", FMT_T, note = "C: company clinker x capacity share (R/01_build_dataset.R)")
  put("Inputs", paste0("E", r), p$heat_demand_gj, "input", FMT_T, note = "C: clinker x 3.0295 GJ/t clinker (R/01_build_dataset.R)")
}
put("Inputs", "B19", "Total", bold = TRUE)
put("Inputs", "E19", "=SUM(E15:E18)", fmt = FMT_T, bold = TRUE)

put("Inputs", "A21", "Parameters", bold = TRUE)
header_row("Inputs", 22, c("Parameter", "", "Value", "Unit", "Type / note"))
param_rows <- list(
  list(23, "Freight multiplier (scenario lever)", 1, "x", "A: 1.00 = base case; 1.25 = freight +25%"),
  list(24, "Sulphur limit, coal blend", "=SUMPRODUCT(C30:C33,E6:E9)", "%", "C: baseline coal-blend sulphur (no dirtier than baseline)"),
  list(25, "Max alt-fuel share of heat", param("tsr_max"), "fraction", "A: technical cap"),
  list(28, "Baseline alt-fuel share of heat", param("tsr_fy25"), "fraction", "V: FY25 thermal substitution rate (S19)"),
  list(29, "Baseline tyre share of alt-fuel heat", param("tdf_heat_share_of_af"), "fraction", "C: calibrated to 88,727 t alt fuels (S19)"),
  list(30, "Baseline coal share - Afghan", param("baseline_afghan_share"), "of coal mass", "V: FY25 25% Afghan (S7)"),
  list(31, "Baseline coal share - Balochistan", param("baseline_local_share") * param("baseline_local_split"), "of coal mass", "V 75% local (S7) x A 50% split"),
  list(32, "Baseline coal share - Salt Range", param("baseline_local_share") * (1 - param("baseline_local_split")), "of coal mass", "V 75% local (S7) x A 50% split"),
  list(33, "Baseline coal share - Imported", 0, "of coal mass", "None imported in FY25 baseline")
)
for (pr in param_rows) {
  r <- pr[[1]]
  put("Inputs", paste0("A", r), pr[[2]])
  put("Inputs", paste0("C", r), pr[[3]], if (is.character(pr[[3]])) "body" else "input", "0.0000")
  put("Inputs", paste0("D", r), pr[[4]])
  put("Inputs", paste0("E", r), pr[[5]], "note")
}

put("Inputs", "A35", "Freight to plant (Rs/t) - road km x Rs 6.92/t-km, rounded (R/01_build_dataset.R)", bold = TRUE)
header_row("Inputs", 36, c("Fuel", SITES))
put("Inputs", "A44", "Delivered cost (Rs/t) = price at origin + freight x multiplier  -> objective coefficients", bold = TRUE)
header_row("Inputs", 45, c("Fuel", SITES))
for (k in seq_along(FUELS)) {
  i <- FUELS[k]; fr <- 36 + k; dr <- 45 + k
  put("Inputs", paste0("A", fr), i)
  put("Inputs", paste0("A", dr), i)
  for (j in seq_along(SITES)) {
    rr <- routes[routes$source_id == i & routes$plant_id == SITES[j], ]
    put("Inputs", paste0(COLS[j], fr), rr$freight_pkr_per_t, "input", FMT_T,
        note = if (is.na(rr$road_km)) "Alt fuels: freight in price" else paste("C:", rr$road_km, "road km"))
    put("Inputs", paste0(COLS[j], dr), sprintf("=$G%d+%s%d*$C$23", FUEL_ROW[[i]], COLS[j], fr), fmt = FMT_T)
  }
}
COST <- "Inputs!$B$46:$E$51"

grid_sheet <- function(sheet, title, subtitle) {
  addWorksheet(wb, sheet)
  setColWidths(wb, sheet, 1:8, c(44, rep(15, 7)))
  put(sheet, "A1", title, bold = TRUE, size = 14)
  put(sheet, "A2", subtitle, "note")
  header_row(sheet, 4, c("Tonnes delivered (t/yr)", SITES, "Total used", "Capacity", "Spare"))
  for (k in seq_along(FUELS)) {
    r <- 4 + k
    put(sheet, paste0("A", r), sprintf("=Inputs!B%d", FUEL_ROW[[k]]), "link")
    put(sheet, paste0("F", r), sprintf("=SUM(B%d:E%d)", r, r), fmt = FMT_T)
    put(sheet, paste0("G", r), sprintf("=Inputs!F%d", FUEL_ROW[[k]]), "link", FMT_T)
    put(sheet, paste0("H", r), sprintf("=G%d-F%d", r, r), fmt = FMT_T)
  }
  put(sheet, "A12", "Objective", bold = TRUE)
  put(sheet, "A13", "Total fuel cost (Rs/yr)", bold = TRUE, size = 11)
  put(sheet, "B13", sprintf("=SUMPRODUCT(%s,B5:E10)", COST), fmt = FMT_T, fill = "#E2EFDA", bold = TRUE, size = 11)
  put(sheet, "A14", "Cost per GJ of kiln heat (Rs/GJ)")
  put(sheet, "B14", "=B13/Inputs!E19", fmt = "#,##0.00")
  header_row(sheet, 16, c("Constraints by plant", SITES))
  labels <- c("Heat supplied (GJ)  [must be >= row 18]", "Heat demand (GJ)", "Sulphur constraint value  [must be <= 0]",
              "Coal-blend sulphur (%)", "Sulphur limit (%)", "Alt-fuel constraint value  [must be <= 0]",
              "Alt-fuel share of heat", "Alt-fuel cap", "All plant constraints met?")
  for (k in seq_along(labels)) put(sheet, paste0("A", 16 + k), labels[k])
  for (j in seq_along(SITES)) {
    c <- COLS[j]; rng <- sprintf("%s5:%s10", c, c); coal <- sprintf("%s5:%s8", c, c)
    put(sheet, paste0(c, 17), sprintf("=SUMPRODUCT(Inputs!$D$6:$D$11,%s)", rng), fmt = FMT_T)
    put(sheet, paste0(c, 18), sprintf("=Inputs!$E$%d", SITE_ROW[[j]]), "link", FMT_T)
    put(sheet, paste0(c, 19), sprintf("=SUMPRODUCT(Inputs!$H$6:$H$11,%s)", rng), fmt = "#,##0.00;(#,##0.00);0.00")
    put(sheet, paste0(c, 20), sprintf("=IFERROR(SUMPRODUCT(Inputs!$E$6:$E$9,%s)/SUM(%s),0)", coal, coal), fmt = "0.000")
    put(sheet, paste0(c, 21), "=Inputs!$C$24", "link", "0.000")
    put(sheet, paste0(c, 22), sprintf("=SUMPRODUCT(Inputs!$I$6:$I$11,%s)-Inputs!$C$25*%s17", rng, c), fmt = "#,##0.00;(#,##0.00);0.00")
    put(sheet, paste0(c, 23), sprintf("=IFERROR(SUMPRODUCT(Inputs!$I$6:$I$11,%s)/%s17,0)", rng, c), fmt = FMT_PCT)
    put(sheet, paste0(c, 24), "=Inputs!$C$25", "link", FMT_PCT)
    put(sheet, paste0(c, 25), sprintf('=IF(AND(%s17>=%s18-1,%s19<=1,%s22<=1),"OK","CHECK")', c, c, c, c), bold = TRUE)
  }
  put(sheet, "A26", "All supply limits met?")
  put(sheet, "B26", '=IF(MIN(H5:H10)>=-1,"OK","CHECK")', bold = TRUE)
  freezePane(wb, sheet, firstActiveRow = 5, firstActiveCol = 2)
}

grid_sheet("Model", "Model - decision variables, objective, constraints",
           "Yellow cells B5:E10 = decision variables. They currently hold the optimum found in R; clear them and re-run Solver to reproduce.")
for (k in seq_along(FUELS)) for (j in seq_along(SITES)) {
  t <- alloc$optimized_t[alloc$source_id == FUELS[k] & alloc$plant_id == SITES[j]]
  put("Model", paste0(COLS[j], 4 + k), round(t, 3), "input", FMT_T, fill = "#FFFF00")
}
put("Model", "A28", "Cross-check with R (R/02_optimization_model.R)", bold = TRUE)
put("Model", "A29", "R optimum (Rs/yr)")
put("Model", "B29", summary$optimized_cost_pkr, "input", FMT_T, note = "results/summary.json")
put("Model", "A30", "Excel minus R (Rs/yr)")
put("Model", "B30", "=B13-B29", fmt = "#,##0;(#,##0);0")
put("Model", "A31", "Difference as % of R optimum")
put("Model", "B31", "=B30/B29", fmt = "0.0000%")
put("Model", "A32", "R decision values are rounded to 0.001 t here, so a difference of a few rupees is expected.", "note")

grid_sheet("Baseline", "Baseline - FY25 disclosed fuel mix applied at every plant",
           "Alt fuels give 9.4% of heat (tyres 26% of that heat, biomass the rest); coal is 25% Afghan / 37.5% Balochistan / 37.5% Salt Range by mass.")
put("Baseline", "G13", "Coal-mix NCV (GJ/t)")
put("Baseline", "H13", "=SUMPRODUCT(Inputs!$C$30:$C$33,Inputs!$D$6:$D$9)", fmt = "0.0000")
coal_ids <- c("AFG", "BAL", "SRC", "IMP")
for (j in seq_along(SITES)) {
  h <- sprintf("Inputs!$E$%d", SITE_ROW[[j]])
  for (k in seq_along(FUELS)) {
    i <- FUELS[k]
    f <- if (i %in% coal_ids) {
      sprintf("=Inputs!$C$%d*%s*(1-Inputs!$C$28)/$H$13", 29 + match(i, coal_ids), h)
    } else if (i == "TDF") {
      sprintf("=%s*Inputs!$C$28*Inputs!$C$29/Inputs!$D$%d", h, FUEL_ROW[["TDF"]])
    } else {
      sprintf("=%s*Inputs!$C$28*(1-Inputs!$C$29)/Inputs!$D$%d", h, FUEL_ROW[["BIO"]])
    }
    put("Baseline", paste0(COLS[j], 4 + k), f, fmt = FMT_T)
  }
}

addWorksheet(wb, "Results")
setColWidths(wb, "Results", 1:6, c(46, 18, 18, 18, 18, 60))
put("Results", "A1", "Results - baseline vs optimized (FY2025 simulation)", bold = TRUE, size = 14)
put("Results", "A2", "Savings are measured against a rule-based baseline, not against FCCL's actual purchasing.", "note")
header_row("Results", 4, c("Measure", "Baseline", "Optimized", "Difference", "% change"))
for (r in 5:6) {
  fmt <- if (r == 5) FMT_T else "#,##0.00"
  put("Results", paste0("A", r), if (r == 5) "Total fuel cost (Rs/yr)" else "Cost per GJ of kiln heat (Rs/GJ)")
  put("Results", paste0("B", r), sprintf("=Baseline!B%d", r + 8), "link", fmt)
  put("Results", paste0("C", r), sprintf("=Model!B%d", r + 8), "link", fmt)
  put("Results", paste0("D", r), sprintf("=C%d-B%d", r, r), fmt = fmt)
  put("Results", paste0("E", r), sprintf("=D%d/B%d", r, r), fmt = FMT_PCT)
}
put("Results", "A7", "Cost reduction (%) = (Baseline - Optimized) / Baseline", bold = TRUE, size = 11)
put("Results", "B7", "=(B5-C5)/B5", fmt = FMT_PCT, fill = "#E2EFDA", bold = TRUE, size = 11)

put("Results", "A9", "Where the saving comes from (R, results/summary.json)", bold = TRUE)
put("Results", "A10", "Reallocating coal only (alt fuels fixed at baseline)")
put("Results", "B10", summary$saving_from_coal_reallocation_pkr, "input", FMT_T)
put("Results", "C10", "=B10/$B$5", fmt = FMT_PCT)
put("Results", "A11", "Using more alternative fuel")
put("Results", "B11", summary$saving_from_extra_alt_fuel_pkr, "input", FMT_T)
put("Results", "C11", "=B11/$B$5", fmt = FMT_PCT)
put("Results", "A12", "Total")
put("Results", "B12", "=B10+B11", fmt = FMT_T, bold = TRUE)
put("Results", "C12", "=B12/$B$5", fmt = FMT_PCT, bold = TRUE)

header_row("Results", 14, c("Fuel", "Baseline (t)", "Optimized (t)", "Change (t)", "Supply capacity (t)"))
for (k in seq_along(FUELS)) {
  r <- 14 + k
  put("Results", paste0("A", r), sprintf("=Inputs!B%d", FUEL_ROW[[k]]), "link")
  put("Results", paste0("B", r), sprintf("=Baseline!F%d", 4 + k), "link", FMT_T)
  put("Results", paste0("C", r), sprintf("=Model!F%d", 4 + k), "link", FMT_T)
  put("Results", paste0("D", r), sprintf("=C%d-B%d", r, r), fmt = FMT_T)
  put("Results", paste0("E", r), sprintf("=Inputs!F%d", FUEL_ROW[[k]]), "link", FMT_T)
}

put("Results", "A22", "Shadow prices (R lpSolve; Excel gives the same in Solver's Sensitivity Report)", bold = TRUE)
header_row("Results", 23, c("Constraint", "Shadow price", "Unit", "", "", "Meaning"))
meaning <- list(
  heat = c("Rs per GJ", "Cost of one more GJ of heat at this plant"),
  supply = c("Rs per t", "Cost change if this source could supply one more tonne (negative = saving)"),
  altfuel = c("Rs per GJ", "Cost change if the alt-fuel cap were loosened by one GJ (negative = saving)"),
  sulphur = c("Rs per %-tonne", "Cost change if the sulphur limit were loosened by one %-tonne")
)
r <- 24
for (k in seq_len(nrow(duals))) {
  d <- duals[k, ]
  if (abs(d$dual) < 1e-9) next
  put("Results", paste0("A", r), paste(d$constraint, "-", d$member))
  put("Results", paste0("B", r), round(d$dual, 2), "input", "#,##0.00;(#,##0.00)")
  put("Results", paste0("C", r), meaning[[d$constraint]][1])
  put("Results", paste0("F", r), meaning[[d$constraint]][2], "note")
  r <- r + 1
}

model_sheet_id <- which(names(wb) == "Model") - 1
solver <- c(
  solver_opt = "Model!$B$13", solver_typ = "2", solver_val = "0", solver_adj = "Model!$B$5:$E$10", solver_num = "4",
  solver_lhs1 = "Model!$B$17:$E$17", solver_rel1 = "3", solver_rhs1 = "Model!$B$18:$E$18",
  solver_lhs2 = "Model!$B$19:$E$19", solver_rel2 = "1", solver_rhs2 = "0",
  solver_lhs3 = "Model!$B$22:$E$22", solver_rel3 = "1", solver_rhs3 = "0",
  solver_lhs4 = "Model!$F$5:$F$10", solver_rel4 = "1", solver_rhs4 = "Model!$G$5:$G$10",
  solver_neg = "1", solver_eng = "2", solver_ver = "3"
)
wb$workbook$definedNames <- c(wb$workbook$definedNames,
  sprintf('<definedName name="%s" localSheetId="%d" hidden="1">%s</definedName>', names(solver), model_sheet_id, solver))

dir.create("excel", showWarnings = FALSE)
saveWorkbook(wb, "excel/cost_optimization_model.xlsx", overwrite = TRUE)
cat("Saved excel/cost_optimization_model.xlsx\n")
