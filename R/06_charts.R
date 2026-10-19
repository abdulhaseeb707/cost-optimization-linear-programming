# Step 6: charts for the README. Reads the CSVs written by steps 1, 2 and 5 and saves PNGs to results/figures/.
# Run after 05_scenarios.R: source("R/06_charts.R")

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

sources <- read.csv("data/model_inputs/fuel_sources.csv", stringsAsFactors = FALSE)
plants <- read.csv("data/model_inputs/plants.csv", stringsAsFactors = FALSE)
routes <- read.csv("data/model_inputs/routes.csv", stringsAsFactors = FALSE)
alloc <- read.csv("results/allocation_comparison.csv", stringsAsFactors = FALSE)
duals <- read.csv("results/shadow_prices.csv", stringsAsFactors = FALSE)
scen <- read.csv("results/scenarios.csv", stringsAsFactors = FALSE)
sweep <- read.csv("results/sensitivity_alt_fuel_cap.csv", stringsAsFactors = FALSE)
summary <- jsonlite::fromJSON("results/summary.json")

# Colours: one fixed colour per fuel in every chart (checked for colour-blind separation)
SURFACE <- "#fcfcfb"; INK <- "#0b0b0b"; INK_2 <- "#52514e"; MUTED <- "#898781"; GRID <- "#e1e0d9"; AXIS <- "#c3c2b7"
UP <- "#e34948"; DOWN <- "#2a78d6"
FUEL_COL <- c(AFG = "#2a78d6", BAL = "#eb6834", SRC = "#1baf7a", IMP = "#eda100", BIO = "#e87ba4", TDF = "#008300")
FUEL_NAME <- c(AFG = "Afghan coal", BAL = "Balochistan coal", SRC = "Salt Range coal",
               IMP = "Imported coal", BIO = "Biomass (rice husk)", TDF = "Tyre-derived fuel")
PLANT_NAME <- setNames(plants$plant_name, plants$plant_id)
NCV <- setNames(sources$ncv_gj_per_t, sources$source_id)
SOURCE_NOTE <- "Model year FY2025. Inputs from company reports and published fuel data; supplier capacities and alt-fuel prices are simulated."

theme_chart <- function() {
  theme_minimal(base_size = 11, base_family = "sans") +
    theme(plot.background = element_rect(fill = SURFACE, colour = NA),
          panel.background = element_rect(fill = SURFACE, colour = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = GRID, linewidth = 0.35),
          axis.text = element_text(colour = INK_2, size = 9.5),
          axis.title = element_text(colour = INK_2, size = 9.5),
          axis.title.x = element_text(margin = margin(t = 8)),
          plot.title = element_text(colour = INK, face = "bold", size = 13.5),
          plot.subtitle = element_text(colour = INK_2, size = 10, margin = margin(t = 2, b = 12)),
          plot.caption = element_text(colour = MUTED, size = 8, hjust = 0, margin = margin(t = 10)),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.position = "top",
          legend.justification = "left",
          legend.title = element_blank(),
          legend.text = element_text(colour = INK_2, size = 9.5),
          legend.key.size = unit(10, "pt"),
          strip.text = element_text(colour = INK, face = "bold", size = 10, hjust = 0),
          plot.margin = margin(16, 20, 12, 16))
}

save_chart <- function(plot, file, height) {
  ggsave(file.path("results/figures", file), plot, width = 8, height = height, dpi = 200,
         device = ragg::agg_png, bg = SURFACE)
  cat("Saved results/figures/", file, "\n", sep = "")
}

# Text inside a coloured fill: white on dark fills, ink on light ones
label_ink <- function(hex) {
  rgb <- col2rgb(hex) / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  ifelse(0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ] > 0.2, INK, "#ffffff")
}

rs <- function(x, accuracy = 1) paste0("Rs ", comma(x, accuracy = accuracy))

# 1. Cost per GJ of kiln heat by plant, baseline vs optimized -------------------------------------
heat <- setNames(plants$heat_demand_gj, plants$plant_id)
by_plant <- data.frame(
  plant_id = plants$plant_id,
  baseline = tapply(alloc$baseline_cost_pkr, alloc$plant_id, sum)[plants$plant_id] / heat,
  optimized = tapply(alloc$optimized_cost_pkr, alloc$plant_id, sum)[plants$plant_id] / heat
)
by_plant <- rbind(by_plant, data.frame(plant_id = "ALL", baseline = summary$baseline_cost_per_gj,
                                       optimized = summary$optimized_cost_per_gj))
by_plant$label <- c(PLANT_NAME[plants$plant_id], "All plants")
by_plant$saving <- (by_plant$baseline - by_plant$optimized) / by_plant$baseline
by_plant$label <- factor(by_plant$label, levels = rev(by_plant$label[order(c(-by_plant$saving[1:4], Inf))]))
dots <- rbind(data.frame(label = by_plant$label, case = "FY25 fuel mix (baseline)", value = by_plant$baseline),
              data.frame(label = by_plant$label, case = "Optimized mix", value = by_plant$optimized))

p1 <- ggplot(by_plant, aes(y = label)) +
  geom_segment(aes(x = optimized, xend = baseline, yend = label), colour = AXIS, linewidth = 1.1) +
  geom_point(data = dots, aes(x = value, fill = case), shape = 21, size = 4, stroke = 1, colour = SURFACE) +
  geom_text(aes(x = baseline, label = paste0("-", percent(saving, accuracy = 0.1))),
            nudge_x = 6, hjust = 0, size = 3.4, colour = INK_2) +
  scale_fill_manual(values = c("FY25 fuel mix (baseline)" = MUTED, "Optimized mix" = DOWN)) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0.05, 0.12))) +
  labs(title = "Fuel cost per GJ of kiln heat, by plant",
       subtitle = sprintf("The optimized mix cuts the fuel bill by %s (%s a year). Wah and Jhang Bahtar gain the most.",
                          percent(summary$saving_pct / 100, accuracy = 0.1), paste0("Rs ", round(summary$saving_pkr / 1e6), " million")),
       x = "Rs per GJ", y = NULL, caption = SOURCE_NOTE) +
  theme_chart()
save_chart(p1, "fig1_cost_per_gj_by_plant.png", 4.2)

# 2. Fuel mix by plant (share of kiln heat), baseline vs optimized ------------------------------
mix <- rbind(
  data.frame(plant_id = alloc$plant_id, source_id = alloc$source_id, case = "Baseline", gj = alloc$baseline_t * NCV[alloc$source_id]),
  data.frame(plant_id = alloc$plant_id, source_id = alloc$source_id, case = "Optimized", gj = alloc$optimized_t * NCV[alloc$source_id])
)
mix <- mix[mix$gj > 0, ]
mix$share <- mix$gj / ave(mix$gj, mix$plant_id, mix$case, FUN = sum)
mix$plant <- factor(PLANT_NAME[mix$plant_id], levels = PLANT_NAME[plants$plant_id])
mix$case <- factor(mix$case, levels = c("Optimized", "Baseline"))
used <- names(FUEL_COL)[names(FUEL_COL) %in% mix$source_id]
mix$fuel <- factor(FUEL_NAME[mix$source_id], levels = rev(FUEL_NAME[used]))
mix$text <- ifelse(mix$share >= 0.09, percent(mix$share, accuracy = 1), "")
mix$ink <- label_ink(FUEL_COL[mix$source_id])

p2 <- ggplot(mix, aes(x = share, y = case, fill = fuel)) +
  geom_col(width = 0.72, colour = SURFACE, linewidth = 0.7) +
  geom_text(aes(label = text, colour = ink, group = fuel), position = position_stack(vjust = 0.5), size = 3.1, show.legend = FALSE) +
  facet_wrap(~plant, ncol = 1) +
  scale_fill_manual(values = setNames(unname(FUEL_COL[used]), FUEL_NAME[used]), breaks = unname(FUEL_NAME[used])) +
  scale_colour_identity() +
  scale_x_continuous(labels = percent, expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "Where each plant's kiln heat comes from",
       subtitle = "Share of heat by fuel. The optimizer moves the northern plants onto Salt Range coal and alternative fuels,\nand sends Balochistan coal to Dera Ghazi Khan, the plant closest to the mines. No imported coal is used.",
       x = "Share of kiln heat", y = NULL, caption = SOURCE_NOTE) +
  theme_chart() +
  theme(panel.grid.major.x = element_blank(), panel.spacing.y = unit(8, "pt"))
save_chart(p2, "fig2_fuel_mix_by_plant.png", 6.4)

# 3. Delivered cost per GJ of each fuel (one dot per plant) --------------------------------------
dc <- routes[, c("source_id", "plant_id", "delivered_cost_pkr_per_gj")]
rng <- do.call(rbind, lapply(split(dc, dc$source_id), function(d)
  data.frame(source_id = d$source_id[1], lo = min(d$delivered_cost_pkr_per_gj), hi = max(d$delivered_cost_pkr_per_gj))))
rng$fuel <- FUEL_NAME[rng$source_id]
order_fuel <- rng$fuel[order(-rng$lo)]
rng$fuel <- factor(rng$fuel, levels = order_fuel)
dc$fuel <- factor(FUEL_NAME[dc$source_id], levels = order_fuel)
rng$text <- ifelse(rng$hi - rng$lo < 1, rs(rng$lo), paste0(rs(rng$lo), " to ", comma(rng$hi)))

p3 <- ggplot() +
  geom_segment(data = rng, aes(x = lo, xend = hi, y = fuel, yend = fuel), colour = AXIS, linewidth = 1.1) +
  geom_point(data = dc, aes(x = delivered_cost_pkr_per_gj, y = fuel, fill = source_id),
             shape = 21, size = 3.8, stroke = 1, colour = SURFACE, show.legend = FALSE) +
  geom_text(data = rng, aes(x = hi, y = fuel, label = text), nudge_x = 45, hjust = 0, size = 3.3, colour = INK_2) +
  scale_fill_manual(values = FUEL_COL) +
  scale_x_continuous(labels = comma, limits = c(500, 2300), breaks = seq(500, 2000, 500)) +
  labs(title = "Delivered cost of each fuel, per GJ of heat",
       subtitle = "Each dot is one plant; the spread comes from freight distance. Alternative fuels are the cheapest heat\nbut supply is limited, and Salt Range coal is the cheapest coal for the three northern plants.",
       x = "Rs per GJ delivered to the plant", y = NULL, caption = SOURCE_NOTE) +
  theme_chart()
save_chart(p3, "fig3_delivered_cost_per_gj.png", 4.2)

# 4. Value of one more tonne of supply (shadow prices) -------------------------------------------
sp <- duals[duals$constraint == "supply", ]
sp$value <- -sp$dual
sp <- sp[order(sp$value), ]
sp$fuel <- factor(FUEL_NAME[sp$member], levels = FUEL_NAME[sp$member])
sp$text <- ifelse(sp$value > 0.5, paste0(rs(sp$value), " per tonne"), "Not a limit")

p4 <- ggplot(sp, aes(x = value, y = fuel, fill = member)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = text), hjust = 0, nudge_x = 500, size = 3.3, colour = INK_2) +
  scale_fill_manual(values = FUEL_COL) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.3))) +
  labs(title = "What one more tonne of each fuel would save",
       subtitle = "Shadow prices on the supply limits. Tyre-derived fuel, biomass and Salt Range coal are all used up,\nso securing more of them is where extra savings would come from.",
       x = "Saving in Rs per extra tonne", y = NULL, caption = SOURCE_NOTE) +
  theme_chart()
save_chart(p4, "fig4_value_of_extra_supply.png", 3.8)

# 5. Scenarios: change in the optimized fuel bill vs the base case --------------------------------
sc <- scen[scen$scenario != "Base case", ]
sc$scenario <- factor(sc$scenario, levels = sc$scenario[order(sc$change_vs_base_pct)])
sc$dir <- ifelse(sc$change_vs_base_pct > 0, "up", "down")
bn <- function(x) ifelse(abs(x) >= 1e9, paste0("Rs ", format(round(abs(x) / 1e9, 2), nsmall = 2), " bn"),
                         paste0("Rs ", round(abs(x) / 1e6), " mn"))
sc$text <- sprintf("%s%s%%  (%s)", ifelse(sc$change_vs_base_pct > 0, "+", "-"),
                   ifelse(abs(sc$change_vs_base_pct) < 0.1, sprintf("%.2f", abs(sc$change_vs_base_pct)), sprintf("%.1f", abs(sc$change_vs_base_pct))),
                   bn(sc$change_vs_base_pkr))
sc$hj <- ifelse(sc$change_vs_base_pct > 0, -0.06, 1.06)

p5 <- ggplot(sc, aes(x = change_vs_base_pct, y = scenario, fill = dir)) +
  geom_vline(xintercept = 0, colour = AXIS, linewidth = 0.5) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = text, hjust = hj), size = 3.3, colour = INK_2) +
  scale_fill_manual(values = c(up = UP, down = DOWN)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(-18, 18), breaks = seq(-10, 10, 5)) +
  labs(title = "How the cheapest fuel bill moves under different conditions",
       subtitle = "Change in the optimized annual fuel cost compared with the base case. Losing Afghan coal forces about\n153,000 t of imported coal to keep sulphur within the limit. A higher alternative-fuel cap barely helps\nbecause biomass and tyre supply runs out first.",
       x = "Change in optimized fuel cost", y = NULL, caption = SOURCE_NOTE) +
  theme_chart()
save_chart(p5, "fig5_scenarios.png", 4.6)

# 6. Sensitivity to the alternative-fuel cap -------------------------------------------------------
fy25 <- 0.094
p6 <- ggplot(sweep, aes(x = alt_fuel_cap, y = cost_per_gj)) +
  geom_vline(xintercept = c(fy25, 0.15), colour = GRID, linewidth = 0.6) +
  geom_line(colour = DOWN, linewidth = 1) +
  geom_point(data = sweep[sweep$alt_fuel_cap %in% c(0.05, 0.15, 0.25), ], shape = 21, size = 3.6, stroke = 1,
             fill = DOWN, colour = SURFACE) +
  annotate("text", x = fy25 + 0.003, y = max(sweep$cost_per_gj), label = "FY25 actual: 9.4%", hjust = 0, vjust = 1, size = 3.2, colour = INK_2) +
  annotate("text", x = 0.153, y = max(sweep$cost_per_gj), label = "Cap used in the model: 15%", hjust = 0, vjust = 1, size = 3.2, colour = INK_2) +
  annotate("text", x = 0.20, y = min(sweep$cost_per_gj) + 4, label = "Flat from about 14%: biomass and\ntyre supply is used up", hjust = 0.5, vjust = 0, size = 3.2, colour = INK_2) +
  scale_x_continuous(labels = percent, breaks = seq(0.05, 0.25, 0.05)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Optimized fuel cost as the alternative-fuel cap changes",
       subtitle = "Cost per GJ of kiln heat for different limits on how much of each plant's heat can come from biomass and tyres.",
       x = "Maximum share of heat from alternative fuels", y = "Rs per GJ", caption = SOURCE_NOTE) +
  theme_chart() +
  theme(panel.grid.major.y = element_line(colour = GRID, linewidth = 0.35), panel.grid.major.x = element_blank())
save_chart(p6, "fig6_alt_fuel_cap_sensitivity.png", 4.2)
