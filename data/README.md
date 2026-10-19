# Data

Base year: **FY2025** (1 Jul 2024 – 30 Jun 2025). Period: one year. Currency: PKR.

## Data-type tags

| Tag | Meaning |
|---|---|
| V | Verified public figure (FCCL report, regulator, peer-reviewed or official source) |
| C | Calculated from V values (formula shown in `parameters.csv` or `data_dictionary.csv`) |
| A | Assumption chosen by the author; the important ones are varied in `R/05_scenarios.R` |
| S | Simulated value (company data not public) |

## Files

| File | Content | Types |
|---|---|---|
| `company_public_data.csv` | FCCL-published figures only | V |
| `model_inputs/plants.csv` | Capacity, clinker, kiln heat demand per plant | V, C, A |
| `model_inputs/fuel_sources.csv` | Fuel quality, price, supply capacity | V, C, A, S |
| `model_inputs/routes.csv` | Distance, freight, delivered cost per source–plant pair | C, A |
| `model_inputs/parameters.csv` | Every scalar input and derived value | V, C, A |
| `model_inputs/baseline_allocation.csv` | Baseline fuel allocation (FY25 mix rule) | C |
| `data_dictionary.csv` | Definition, unit, type and source of every column | – |

Rebuild everything in R: `source("run_all.R")` (or only the data: `source("R/01_build_dataset.R")`)

## Key derivations

1. Clinker = cement × (1 − 5% gypsum) − supplementary materials = **4,600,189 t**
2. Electricity = WHR GJ ÷ WHR share = **1,676,542 GJ** (86.5 kWh/t cement)
3. Kiln heat = total energy − electricity = **13,936,190 GJ**
4. Specific heat = kiln heat ÷ clinker = **3.03 GJ/t clinker (724 kcal/kg)**
5. Freight rate = Rs 10,000/t ÷ average road km Karachi → northern plants = **Rs 6.92/t-km**
6. Sulphur limit = baseline coal-blend sulphur = **4.03%** (optimized blend may not be dirtier than baseline)

## Validation against public figures

| Check | Model | Public | Gap |
|---|---|---|---|
| Capacity utilisation | 51.5% | 51% (S7) | +0.5 pt |
| Baseline fuel bill vs fuel consumed (9M FY25 × 4/3) | Rs 19.76 bn | Rs 21.44 bn | −7.8% |

## Known limitations

- Supplier capacities, alternative-fuel prices and coordinates are not public (S/A).
- Coal quality values are literature values for each coal type, not FCCL's purchased coal.
- The 75/25 local/Afghan split is assumed to be by mass; the source does not say.
- Two published FY25 energy totals differ: 15,612,732 GJ (S19) and 15,093,004 GJ (S5). S19 is used because its components reconcile.
- No public FY25 Afghan coal price index exists (S16).
