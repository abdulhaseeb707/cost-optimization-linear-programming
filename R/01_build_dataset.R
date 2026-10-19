# Step 1: build every CSV in data/ from documented inputs.
# Tags: V = verified public, C = calculated, A = assumption, S = simulated. Source IDs: references/sources.md
# Run from the project folder: source("R/01_build_dataset.R")

dir.create("data/model_inputs", recursive = TRUE, showWarnings = FALSE)

KCAL_KG_TO_GJ_T <- 4.1868 / 1000
num <- function(x) if (is.numeric(x)) format(x, scientific = FALSE, trim = TRUE, digits = 15) else x

P <- list(
  base_year                   = list("FY2025 (1 Jul 2024 - 30 Jun 2025)", "-", "A", "-", "Latest year with a disclosed coal-source mix"),
  cement_production_t         = list(5383885, "t", "V", "S19", "FY25 cement production"),
  scm_used_t                  = list(514502, "t", "V", "S19", "Supplementary materials (limestone, slag, fly ash, pozzolana) used in FY25"),
  gypsum_share                = list(0.05, "fraction of cement", "A", "S6", "OPC recipe is 95% clinker + 5% gypsum (V); applied to all cement types (A)"),
  total_energy_gj             = list(15612732, "GJ", "V", "S19", "Total energy consumption within the organisation, FY25"),
  whr_electricity_gj          = list(519728, "GJ", "V", "S19", "Electricity from waste heat recovery, FY25"),
  whr_share_of_electricity    = list(0.31, "fraction", "V", "S19", "WHR share of FY25 power mix"),
  tsr_fy25                    = list(0.094, "fraction of thermal energy", "V", "S19", "Thermal substitution rate by alternative fuels, FY25"),
  af_consumed_fy25_t          = list(88727, "t", "V", "S19", "Alternative fuels consumed, FY25"),
  operating_days              = list(300, "days/yr", "C", "S12;S20", "DG Khan 2.05 Mtpa / 6,825 TPD = 300 days"),
  ncv_to_gcv_ratio            = list(0.90, "fraction", "A", "-", "Converts reported gross CV to net CV as received"),
  road_circuity               = list(1.30, "road km per straight km", "A", "-", "Road distance = straight-line distance x factor"),
  freight_ref_pkr_t           = list(10000, "PKR/t", "V", "S13", "Coal freight Karachi port to northern region (Jan 2024)"),
  intl_coal_fy26_usd_t        = list(97, "USD/t", "V", "S14", "International coal FY26 average"),
  intl_coal_yoy_change        = list(-0.05, "fraction", "V", "S14", "FY26 vs FY25 change"),
  fx_pkr_per_usd              = list(279, "PKR/USD", "A", "-", "Approximate FY25 average"),
  sea_freight_insurance_usd_t = list(15, "USD/t", "A", "-", "South Africa to Karachi"),
  port_handling_pkr_t         = list(2000, "PKR/t", "A", "-", "Port, customs clearance and handling"),
  coal_price_low_pkr_t        = list(35000, "PKR/t", "V", "S7", "Low end of FCCL coal price range (Sep 2025 briefing)"),
  coal_price_high_pkr_t       = list(39000, "PKR/t", "V", "S7", "High end of FCCL coal price range (Sep 2025 briefing)"),
  baseline_local_share        = list(0.75, "fraction of coal mass", "V", "S7", "FY25 local coal share; mass basis assumed (A)"),
  baseline_afghan_share       = list(0.25, "fraction of coal mass", "V", "S7", "FY25 Afghan coal share; mass basis assumed (A)"),
  baseline_local_split        = list(0.50, "fraction", "A", "-", "Local coal split equally between Balochistan and Salt Range"),
  tsr_max                     = list(0.15, "fraction of thermal energy", "A", "-", "Technical cap on alternative-fuel heat share"),
  fuel_cost_9m_fy25_pkr000    = list(16079940, "PKR '000", "V", "S1", "Fuel consumed, 9 months to 31 Mar 2025 (calibration check only)")
)
v <- lapply(P, `[[`, 1)

plants <- data.frame(
  plant_id            = c("JB", "WAH", "NZP", "DGK"),
  plant_name          = c("Jhang Bahtar", "Wah", "Nizampur", "Dera Ghazi Khan"),
  location_reference  = c("Fateh Jang (Attock)", "Taxila (Rawalpindi)", "Nizampur (Nowshera)", "Dera Ghazi Khan"),
  lat                 = c(33.565, 33.746, 33.790, 30.046),
  lon                 = c(72.642, 72.840, 72.030, 70.640),
  cement_capacity_tpd = c(11865, 3675, 12495, 6825)
)

sources <- data.frame(
  source_id         = c("AFG", "BAL", "SRC", "IMP", "BIO", "TDF"),
  source_name       = c("Afghan coal", "Local coal - Balochistan", "Local coal - Salt Range",
                        "Imported coal (South African type)", "Biomass (rice husk)", "Tyre-derived fuel"),
  fuel_type         = c("coal", "coal", "coal", "coal", "alt_fuel", "alt_fuel"),
  origin_point      = c("Torkham border", "Duki coalfield", "Choa Saidan Shah", "Port Qasim, Karachi", "Local collection", "Local collection"),
  lat               = c(34.131, 30.153, 32.720, 24.780, NA, NA),
  lon               = c(71.092, 68.573, 72.980, 67.330, NA, NA),
  cv_reported       = c(6000, 6158, 4972, 6290, 13.63, 35.15),
  cv_basis          = c("NCV", "GCV", "GCV", "GCV", "unspecified_MJ_kg", "NCV_MJ_kg"),
  cv_source         = c("S17", "S15", "S24", "S15", "S25", "S26"),
  sulphur_pct       = c(0.80, 4.90, 5.32, 0.80, 0.11, 1.57),
  sulphur_source    = c("S17", "S15", "S24", "S15", "S25", "S26"),
  ash_pct           = c(12.0, 8.43, 24.23, NA, 15.20, NA),
  ash_source        = c("S17", "S15", "S24", "-", "S25", "-"),
  supply_capacity_t = c(200000, 300000, 300000, 1000000, 100000, 15000),
  stringsAsFactors  = FALSE
)

haversine_km <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * 6371 * asin(sqrt(a))
}

ncv_raw <- with(sources, ifelse(cv_basis == "NCV", cv_reported * KCAL_KG_TO_GJ_T,
                         ifelse(cv_basis == "GCV", cv_reported * v$ncv_to_gcv_ratio * KCAL_KG_TO_GJ_T,
                         ifelse(cv_basis == "NCV_MJ_kg", cv_reported, cv_reported * v$ncv_to_gcv_ratio))))
sources$ncv_gj_per_t <- round(ncv_raw, 3)
ncv <- setNames(sources$ncv_gj_per_t, sources$source_id)

# Plants: capacity -> clinker -> heat demand
clinker_total  <- v$cement_production_t * (1 - v$gypsum_share) - v$scm_used_t
electricity_gj <- v$whr_electricity_gj / v$whr_share_of_electricity
thermal_gj     <- v$total_energy_gj - electricity_gj
shc            <- thermal_gj / clinker_total

plants$cement_capacity_tpa <- plants$cement_capacity_tpd * v$operating_days
plants$capacity_share      <- plants$cement_capacity_tpa / sum(plants$cement_capacity_tpa)
plants$clinker_t           <- round(clinker_total * plants$capacity_share, 0)
plants$heat_demand_gj      <- round(plants$clinker_t * shc, 0)

# Routes and freight
routes <- expand.grid(plant_id = plants$plant_id, source_id = sources$source_id, stringsAsFactors = FALSE)[, c("source_id", "plant_id")]
s_idx <- match(routes$source_id, sources$source_id)
p_idx <- match(routes$plant_id, plants$plant_id)
is_coal <- sources$fuel_type[s_idx] == "coal"
km <- ifelse(is_coal, haversine_km(sources$lat[s_idx], sources$lon[s_idx], plants$lat[p_idx], plants$lon[p_idx]), NA)
rkm <- km * v$road_circuity

north <- c("JB", "WAH", "NZP")
avg_north_road <- function(sid) mean(rkm[routes$source_id == sid & routes$plant_id %in% north])
freight_rate <- v$freight_ref_pkr_t / avg_north_road("IMP")

fob_fy25 <- v$intl_coal_fy26_usd_t / (1 + v$intl_coal_yoy_change)
price <- c(
  AFG = v$coal_price_high_pkr_t - avg_north_road("AFG") * freight_rate,
  BAL = v$coal_price_low_pkr_t - avg_north_road("BAL") * freight_rate
)
price["SRC"] <- price[["BAL"]] * ncv[["SRC"]] / ncv[["BAL"]]
price["IMP"] <- (fob_fy25 + v$sea_freight_insurance_usd_t) * v$fx_pkr_per_usd + v$port_handling_pkr_t
price["BIO"] <- 12000
price["TDF"] <- 20000

sources$price_pkr_per_t <- round(price[sources$source_id], 0)
sources$price_type <- c(AFG = "C", BAL = "C", SRC = "C", IMP = "C", BIO = "S", TDF = "S")[sources$source_id]
sources$price_basis <- c(
  AFG = "S7 high price (delivered north, A) minus avg freight Torkham->north plants",
  BAL = "S7 low price (delivered north, A) minus avg freight Duki->north plants",
  SRC = "Balochistan ex-mine price scaled by NCV ratio (equal price per GJ, A)",
  IMP = "FY25 FOB (S14, C) + sea freight (A) x FX (A) + port handling (A)",
  BIO = "Simulated delivered price incl. local collection",
  TDF = "Simulated delivered price incl. local collection"
)[sources$source_id]
sources$supply_capacity_type <- "S"

routes$straight_km <- round(km, 1)
routes$road_km <- round(rkm, 1)
routes$freight_pkr_per_t <- ifelse(is_coal, round(rkm * freight_rate), 0)
routes$delivered_cost_pkr_per_t <- round(price[routes$source_id]) + routes$freight_pkr_per_t
routes$delivered_cost_pkr_per_gj <- round(routes$delivered_cost_pkr_per_t / ncv[routes$source_id], 1)
cost <- setNames(routes$delivered_cost_pkr_per_t, paste(routes$source_id, routes$plant_id))

# Baseline mix (sets the sulphur limit and is the comparison case)
coal_mix <- c(AFG = v$baseline_afghan_share,
              BAL = v$baseline_local_share * v$baseline_local_split,
              SRC = v$baseline_local_share * (1 - v$baseline_local_split))
sulphur <- setNames(sources$sulphur_pct, sources$source_id)
s_max <- sum(coal_mix * sulphur[names(coal_mix)])
coal_ncv_mix <- sum(coal_mix * ncv[names(coal_mix)])

af_avg_ncv <- v$tsr_fy25 * thermal_gj / v$af_consumed_fy25_t
tdf_mass_share <- (af_avg_ncv - ncv[["BIO"]]) / (ncv[["TDF"]] - ncv[["BIO"]])
tdf_heat_share <- tdf_mass_share * ncv[["TDF"]] / af_avg_ncv

baseline <- do.call(rbind, lapply(seq_len(nrow(plants)), function(k) {
  pid <- plants$plant_id[k]
  h <- plants$heat_demand_gj[k]
  h_af <- v$tsr_fy25 * h
  m_coal <- (h - h_af) / coal_ncv_mix
  t <- c(coal_mix * m_coal,
         TDF = h_af * tdf_heat_share / ncv[["TDF"]],
         BIO = h_af * (1 - tdf_heat_share) / ncv[["BIO"]])
  data.frame(source_id = names(t), plant_id = pid, tonnes = round(unname(t), 6),
             cost_pkr = round(unname(t) * cost[paste(names(t), pid)], 2))
}))
baseline_by_source <- tapply(baseline$tonnes, baseline$source_id, sum)
capacity <- setNames(sources$supply_capacity_t, sources$source_id)
stopifnot(all(baseline_by_source <= capacity[names(baseline_by_source)]))

baseline_cost <- sum(baseline$cost_pkr)
public_fuel_annual <- v$fuel_cost_9m_fy25_pkr000 * 1000 * 4 / 3
derived <- list(
  clinker_production_t              = list(round(clinker_total), "t", "C", "cement x (1 - gypsum) - SCM"),
  electricity_consumed_gj           = list(round(electricity_gj), "GJ", "C", "WHR GJ / WHR share"),
  kiln_thermal_energy_gj            = list(round(thermal_gj), "GJ", "C", "total energy - electricity (A: energy total counts electricity at delivered value)"),
  specific_heat_gj_per_t_clinker    = list(round(shc, 4), "GJ/t clinker", "C", "thermal energy / clinker"),
  specific_heat_kcal_per_kg_clinker = list(round(shc / KCAL_KG_TO_GJ_T, 1), "kcal/kg clinker", "C", "unit conversion"),
  specific_power_kwh_per_t_cement   = list(round(electricity_gj / 0.0036 / v$cement_production_t, 1), "kWh/t cement", "C", "electricity / cement"),
  implied_utilisation               = list(round(v$cement_production_t / sum(plants$cement_capacity_tpa), 4), "fraction", "C", "production / capacity; public FY25 figure is 51% (S7)"),
  freight_rate_pkr_per_t_km         = list(round(freight_rate, 3), "PKR/t-km", "C", "S13 freight / avg road km Port Qasim->north plants"),
  fob_coal_fy25_usd_t               = list(round(fob_fy25, 2), "USD/t", "C", "FY26 avg / (1 - 5%)"),
  af_avg_ncv_fy25_gj_t              = list(round(af_avg_ncv, 3), "GJ/t", "C", "TSR x thermal energy / AF tonnes"),
  tdf_mass_share_of_af              = list(round(tdf_mass_share, 8), "fraction", "C", "mixing rule on NCVs"),
  tdf_heat_share_of_af              = list(round(tdf_heat_share, 8), "fraction", "C", "TDF mass share x TDF NCV / AF average NCV"),
  sulphur_limit_coal_blend_pct      = list(round(s_max, 4), "% (mass)", "C", "baseline coal-blend sulphur (no-worse-than-baseline rule)"),
  baseline_fuel_cost_pkr            = list(round(baseline_cost), "PKR", "C", "calibration check"),
  public_fuel_cost_annualised_pkr   = list(round(public_fuel_annual), "PKR", "C", "9M FY25 x 4/3 (A: even quarters)"),
  calibration_gap                   = list(round(baseline_cost / public_fuel_annual - 1, 4), "fraction", "C", "baseline / public annualised - 1")
)

params <- rbind(
  data.frame(parameter = names(P), value = sapply(P, function(x) num(x[[1]])), unit = sapply(P, `[[`, 2),
             data_type = sapply(P, `[[`, 3), source_id = sapply(P, `[[`, 4), note = sapply(P, `[[`, 5)),
  data.frame(parameter = names(derived), value = sapply(derived, function(x) num(x[[1]])), unit = sapply(derived, `[[`, 2),
             data_type = sapply(derived, `[[`, 3), source_id = "-", note = sapply(derived, `[[`, 4))
)

public <- data.frame(
  id = sprintf("P%02d", 1:41),
  metric = c("Revenue, net", "Revenue, net", "Profit after tax", "Profit after tax", "Cement production", "Cement sales",
             "Cement sales - local", "Cement sales - export", "Cement sales", "Capacity utilisation",
             "Cement capacity - Jhang Bahtar", "Cement capacity - Wah", "Cement capacity - Nizampur", "Cement capacity - Dera Ghazi Khan",
             "Local coal share of coal", "Afghan coal share of coal", "Local coal share", "Coal price range - low", "Coal price range - high",
             "Thermal substitution rate (alternative fuels)", "Alternative fuels consumed", "Alternative fuels consumed",
             "Total energy consumption", "Total energy consumption (as restated in FY26 briefing)", "Energy intensity", "WHR electricity",
             "Power mix - grid", "Power mix - solar", "Power mix - WHR", "Power mix - captive thermal",
             "Supplementary cementitious materials used", "Portland composite cement share of output",
             "Fuel consumed (cost of sales note)", "Power consumed (cost of sales note)", "Raw material consumed (cost of sales note)",
             "Total manufacturing cost", "Cost of sales", "Freight (selling & distribution)", "Fuel consumed (cost of sales note)",
             "Total manufacturing cost", "Afghan border closed (Afghan coal imports halted)"),
  value = c("88956", "93690", "13326", "16183", "5383885", "5368785", "4.81", "0.56", "5700796", "51",
            "11865", "3675", "12495", "6825", "75", "25", "70", "35000", "39000", "9.4", "88727", "39097",
            "15612732", "15093004", "2.9", "519728", "41", "18", "31", "10", "514502", "31",
            "16079940", "6885630", "7825338", "43381235", "44099993", "1699481", "5273337", "14605714", "11 Oct 2025"),
  unit = c("PKR mn", "PKR mn", "PKR mn", "PKR mn", "t", "t", "Mt", "Mt", "t", "%", "t/day", "t/day", "t/day", "t/day",
           "%", "%", "%", "PKR/t", "PKR/t", "%", "t", "t", "GJ", "GJ", "GJ/t cement", "GJ", "%", "%", "%", "%", "t", "%",
           "PKR '000", "PKR '000", "PKR '000", "PKR '000", "PKR '000", "PKR '000", "PKR '000", "PKR '000", "date"),
  period = c("FY25", "FY26", "FY25", "FY26", "FY25", "FY25", "FY25", "FY25", "FY26", "FY25", "2026", "2026", "2026", "2026",
             "FY25", "FY25", "FY26", "Sep 2025", "Sep 2025", "FY25", "FY25", "FY26", "FY25", "FY25", "FY25", "FY25",
             "FY25", "FY25", "FY25", "FY25", "FY25", "FY25", "9M FY25", "9M FY25", "9M FY25", "9M FY25", "9M FY25", "9M FY25",
             "Q1 FY26", "Q1 FY26", "FY26"),
  source_id = c("S8", "S9", "S4", "S22", "S19", "S19", "S4", "S4", "S21", "S7", "S20", "S20", "S20", "S20", "S7", "S7", "S21",
                "S7", "S7", "S19", "S19", "S5", "S19", "S5", "S19", "S19", "S19", "S19", "S19", "S19", "S19", "S19",
                "S1", "S1", "S1", "S1", "S1", "S1", "S3", "S3", "S23"),
  data_type = "V"
)

write.csv(sources, "data/model_inputs/fuel_sources.csv", row.names = FALSE, na = "")
write.csv(plants, "data/model_inputs/plants.csv", row.names = FALSE, na = "")
write.csv(routes, "data/model_inputs/routes.csv", row.names = FALSE, na = "")
write.csv(params, "data/model_inputs/parameters.csv", row.names = FALSE, na = "")
write.csv(baseline, "data/model_inputs/baseline_allocation.csv", row.names = FALSE, na = "")
write.csv(public, "data/company_public_data.csv", row.names = FALSE, na = "")

cat("\nDataset built.\n")
for (k in names(derived)) cat(sprintf("%-36s %20s %s\n", k, format(derived[[k]][[1]], big.mark = ",", scientific = FALSE), derived[[k]][[2]]))
