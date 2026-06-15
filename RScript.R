# ==============================================================================
# Global PV waste and repowering demand
# ==============================================================================
# This script estimates global PV waste generation and repowering demand using
# IEA PV deployment scenarios and either a Weibull or Gaussian failure model.
#
# Main outputs:
#   - repowering demand by segment and in total
#   - annual PV waste by segment and in total
#   - cumulative PV waste over time
#
# Segments:
#   - rooftop c-Si
#   - rooftop CdTe
#   - ground-mounted c-Si
#   - ground-mounted CdTe
# ==============================================================================

rm(list = ls())

library(readxl)
library(dplyr)

setwd("set path here")

source("filter_rows_by_reference_rownames.R")
source("calculate_pv_waste.R")
source("calculate_repowering_capacity.R")

# ------------------------------------------------------------------------------
# 1. Parameters
# ------------------------------------------------------------------------------

# Only global calculations are retained. The workbook sheet is still named "Welt".
capacity_region_sheet <- "Global"

# IEA PV deployment scenario:
#   IEA_Current = Current Policies Scenario
#   IEA_Stated  = Stated Policies Scenario
#   IEA_NetZero = Net Zero Emissions Scenario
pv_forecast <- "IEA_Current"

# Failure model:
#   Weibull  = Weibull distribution following the Weckend et al. approach
#   Gaussian = Gaussian degradation model following the Dirr et al. approach
methodology <- "Gaussian"

first_year <- 2003
observation_year <- 2025
last_year <- 2050

# Weibull shape parameter:
#   Regular-Loss: 5.3759
#   Early-Loss:  2.4928
shape_value <- 5.3759

# If TRUE, all Weibull scale parameters are set to 30 for comparability with
# the original Weckend et al. assumptions.
fix_scale_value <- FALSE

if (!pv_forecast %in% c("IEA_Current", "IEA_Stated", "IEA_NetZero")) {
  stop("pv_forecast must be one of: IEA_Current, IEA_Stated, IEA_NetZero.")
}

if (!methodology %in% c("Weibull", "Gaussian")) {
  stop("methodology must be either 'Weibull' or 'Gaussian'.")
}

if (last_year > 2050) {
  stop("IEA scenario data are only available up to 2050.")
}

# ------------------------------------------------------------------------------
# 2. Degradation rates and failure-model scale parameters
# ------------------------------------------------------------------------------

# Global degradation rates by segment.
degradation_rate_rooftop_csi <- 0.0103
degradation_rate_rooftop_cdte <- 0.0216
degradation_rate_ground_csi <- 0.0134
degradation_rate_ground_cdte <- 0.0246

# Service life is defined as the time until 20% performance loss. For the Weibull
# model, the service life is converted into the characteristic scale parameter.
service_life_rooftop_csi <- 0.20 / degradation_rate_rooftop_csi
scale_value_rooftop_csi <- service_life_rooftop_csi / (log(2)^(1 / shape_value))

service_life_rooftop_cdte <- 0.20 / degradation_rate_rooftop_cdte
scale_value_rooftop_cdte <- service_life_rooftop_cdte / (log(2)^(1 / shape_value))

service_life_ground_csi <- 0.20 / degradation_rate_ground_csi
scale_value_ground_csi <- service_life_ground_csi / (log(2)^(1 / shape_value))

service_life_ground_cdte <- 0.20 / degradation_rate_ground_cdte
scale_value_ground_cdte <- service_life_ground_cdte / (log(2)^(1 / shape_value))

if (isTRUE(fix_scale_value)) {
  scale_value_rooftop_csi <- 30
  scale_value_rooftop_cdte <- 30
  scale_value_ground_csi <- 30
  scale_value_ground_cdte <- 30
}

# ------------------------------------------------------------------------------
# 3. Load input data
# ------------------------------------------------------------------------------

# Annual rooftop and ground-mounted shares.
roof_ground_share_raw <-data.frame( 
  read_excel(
  "pv_roof_ground.xlsx",
  sheet = pv_forecast,
  col_names = FALSE,
  col_types = "numeric"
))

colnames(roof_ground_share_raw) <- c("Year", "rooftop", "ground_mount")

roof_ground_share <- roof_ground_share_raw %>%
  filter(Year >= first_year, Year <= last_year) %>%
  select(rooftop, ground_mount) %>%
  as.data.frame()

rownames(roof_ground_share) <- seq(first_year, last_year, 1)

# Annual c-Si and CdTe technology shares.
technology_share_raw <- data.frame(
  read_excel(
  "pv_cSi_thinfilm.xlsx",
  col_names = FALSE,
  col_types = "numeric"
))

colnames(technology_share_raw) <- c("Year", "cSi", "CdTe")

technology_share <- technology_share_raw %>%
  filter(Year >= first_year, Year <= last_year) %>%
  select(cSi, CdTe) %>%
  as.data.frame()

rownames(technology_share) <- seq(first_year, last_year, 1)

# Historical cumulative installed PV capacity in MW.
installed_capacity_raw <- data.frame(
  read_excel(
  "pv_capacity_IEA.xlsx",
  sheet = capacity_region_sheet,
  col_names = FALSE,
  col_types = "numeric"
))

colnames(installed_capacity_raw) <- c("Year", "MW")

installed_capacity <- installed_capacity_raw %>%
  filter(Year >= first_year, Year <= observation_year) %>%
  select(MW)

rownames(installed_capacity) <- seq(first_year, observation_year, 1)
# ------------------------------------------------------------------------------
# 4. Construct annual net PV additions
# ------------------------------------------------------------------------------

# Historical annual net additions are derived from cumulative installed capacity.
annual_net_capacity <- diff(installed_capacity$MW)

# Future annual net additions are taken from the selected IEA scenario.
iea_forecast_table_raw <- data.frame(
  read_excel(
  "pv_forecast_IEA.xlsx",
  col_names = FALSE,
  col_types = "numeric",
  sheet = "IEA"
))

colnames(iea_forecast_table_raw) <- c(
  "Year",
  "IEA_Current",
  "IEA_Stated",
  "IEA_NetZero"
)

iea_forecast_table <- iea_forecast_table_raw %>%
  filter(Year >= observation_year+1, Year <= last_year) %>%
  select(IEA_Current, IEA_Stated, IEA_NetZero) %>%
  as.data.frame()

forecast_net_capacity <- data.frame(iea_forecast_table[, pv_forecast])

annual_net_capacity <- c(annual_net_capacity, forecast_net_capacity[, 1])
annual_net_capacity <- as.data.frame(annual_net_capacity)

rownames(annual_net_capacity) <- seq(first_year + 1, last_year, 1)
colnames(annual_net_capacity) <- "MW"

# ------------------------------------------------------------------------------
# 5. Estimate specific module mass in tonnes per MW
# ------------------------------------------------------------------------------

# Anchor points from Weckend et al. for the specific module mass over time.
anchor_years <- c(1980, 1990, 1995, 2000, 2005, 2010, 2012, 2015, 2020, 2025, 2030, 2050)
anchor_tonnes_per_mw <- c(169.000, 148.000, 122.000, 110.000, 98.900, 93.300,
                          78.800, 65.700, 65.700, 65.700, 60.100, 43.700)

module_mass_points <- data.frame(
  year = anchor_years,
  tonnes_per_mw = anchor_tonnes_per_mw
)

# Fit an exponential decline in specific module mass and predict annual values.
module_mass_model <- lm(log(tonnes_per_mw) ~ year, data = module_mass_points)

module_mass_t_per_mw <- as.data.frame(
  exp(predict(module_mass_model, data.frame(year = seq(1990, 2050, 1))))
)
module_mass_t_per_mw <- cbind(seq(1990, 2050, 1), module_mass_t_per_mw)
colnames(module_mass_t_per_mw) <- c("year", "tonnes_per_mw")
rownames(module_mass_t_per_mw) <- seq(1990, 2050, 1)

# Keep only the years required for the annual capacity series.
module_mass_t_per_mw <- filter_rows_by_reference_rownames(
  module_mass_t_per_mw,
  annual_net_capacity
)

# ------------------------------------------------------------------------------
# 6. Align share data with annual net additions
# ------------------------------------------------------------------------------

# The first row is removed because annual additions start in first_year + 1.
roof_ground_share <- roof_ground_share[-1, ]
rownames(roof_ground_share) <- seq(first_year + 1, last_year, 1)

technology_share <- technology_share[-1, ]
rownames(technology_share) <- seq(first_year + 1, last_year, 1)

# ------------------------------------------------------------------------------
# 7. Split annual net additions by segment
# ------------------------------------------------------------------------------

net_capacity_rooftop_csi <-
  roof_ground_share[, "rooftop"] *
  technology_share[, "cSi"] *
  annual_net_capacity[, "MW"]

net_capacity_rooftop_cdte <-
  roof_ground_share[, "rooftop"] *
  technology_share[, "CdTe"] *
  annual_net_capacity[, "MW"]

net_capacity_ground_csi <-
  roof_ground_share[, "ground_mount"] *
  technology_share[, "cSi"] *
  annual_net_capacity[, "MW"]

net_capacity_ground_cdte <-
  roof_ground_share[, "ground_mount"] *
  technology_share[, "CdTe"] *
  annual_net_capacity[, "MW"]

# ------------------------------------------------------------------------------
# 8. Estimate annual failure probabilities by segment
# ------------------------------------------------------------------------------

# pof_t is the probability of failure in operating year t:
# pof_t = F(t) - F(t - 1), where F(t) is the cumulative failure probability.
if (methodology == "Weibull") {
  
  failure_cdf_rooftop_csi <- pweibull(
    0:nrow(annual_net_capacity),
    shape = shape_value,
    scale = scale_value_rooftop_csi,
    log = FALSE
  )
  failure_probability_rooftop_csi <- diff(failure_cdf_rooftop_csi)
  
  failure_cdf_rooftop_cdte <- pweibull(
    0:nrow(annual_net_capacity),
    shape = shape_value,
    scale = scale_value_rooftop_cdte,
    log = FALSE
  )
  failure_probability_rooftop_cdte <- diff(failure_cdf_rooftop_cdte)
  
  failure_cdf_ground_csi <- pweibull(
    0:nrow(annual_net_capacity),
    shape = shape_value,
    scale = scale_value_ground_csi,
    log = FALSE
  )
  failure_probability_ground_csi <- diff(failure_cdf_ground_csi)
  
  failure_cdf_ground_cdte <- pweibull(
    0:nrow(annual_net_capacity),
    shape = shape_value,
    scale = scale_value_ground_cdte,
    log = FALSE
  )
  failure_probability_ground_cdte <- diff(failure_cdf_ground_cdte)
  
} else if (methodology == "Gaussian") {
  
  # Dirr et al. apply fixed initial values for early failures in years 1 to 5.
  initial_failure_probability <- c(0.013, 0.004, 0.004, 0.004, 0.004)
  
  failure_cdf_rooftop_csi <- pnorm(
    0.8,
    mean = 1 * (1 - degradation_rate_rooftop_csi * (0:nrow(annual_net_capacity))),
    sd = 0.0167 * (1 + 0.05 * (0:nrow(annual_net_capacity)))
  )
  failure_probability_rooftop_csi <- diff(failure_cdf_rooftop_csi)
  failure_probability_rooftop_csi[1:5] <- initial_failure_probability
  
  failure_cdf_rooftop_cdte <- pnorm(
    0.8,
    mean = 1 * (1 - degradation_rate_rooftop_cdte * (0:nrow(annual_net_capacity))),
    sd = 0.0167 * (1 + 0.05 * (0:nrow(annual_net_capacity)))
  )
  failure_probability_rooftop_cdte <- diff(failure_cdf_rooftop_cdte)
  failure_probability_rooftop_cdte[1:5] <- initial_failure_probability
  
  failure_cdf_ground_csi <- pnorm(
    0.8,
    mean = 1 * (1 - degradation_rate_ground_csi * (0:nrow(annual_net_capacity))),
    sd = 0.0167 * (1 + 0.05 * (0:nrow(annual_net_capacity)))
  )
  failure_probability_ground_csi <- diff(failure_cdf_ground_csi)
  failure_probability_ground_csi[1:5] <- initial_failure_probability
  
  failure_cdf_ground_cdte <- pnorm(
    0.8,
    mean = 1 * (1 - degradation_rate_ground_cdte * (0:nrow(annual_net_capacity))),
    sd = 0.0167 * (1 + 0.05 * (0:nrow(annual_net_capacity)))
  )
  failure_probability_ground_cdte <- diff(failure_cdf_ground_cdte)
  failure_probability_ground_cdte[1:5] <- initial_failure_probability
}

# ------------------------------------------------------------------------------
# 9. Calculate repowering demand by segment
# ------------------------------------------------------------------------------

# calculate_repowering_capacity() returns annual net, repowering, and gross capacity.
repowering_rooftop_csi <- calculate_repowering_capacity(
  failure_probability_rooftop_csi,
  net_capacity_rooftop_csi
)

repowering_rooftop_cdte <- calculate_repowering_capacity(
  failure_probability_rooftop_cdte,
  net_capacity_rooftop_cdte
)

repowering_ground_csi <- calculate_repowering_capacity(
  failure_probability_ground_csi,
  net_capacity_ground_csi
)

repowering_ground_cdte <- calculate_repowering_capacity(
  failure_probability_ground_cdte,
  net_capacity_ground_cdte
)

repowering_sum <- data.frame(
  year = seq(first_year + 1, last_year, 1),
  
  net_capacity =
    repowering_rooftop_csi$net_capacity +
    repowering_rooftop_cdte$net_capacity +
    repowering_ground_csi$net_capacity +
    repowering_ground_cdte$net_capacity,
  
  repowering_capacity =
    repowering_rooftop_csi$repowering_capacity +
    repowering_rooftop_cdte$repowering_capacity +
    repowering_ground_csi$repowering_capacity +
    repowering_ground_cdte$repowering_capacity,
  
  gross_capacity =
    repowering_rooftop_csi$gross_capacity +
    repowering_rooftop_cdte$gross_capacity +
    repowering_ground_csi$gross_capacity +
    repowering_ground_cdte$gross_capacity
)

# ------------------------------------------------------------------------------
# 10. Convert gross capacity into installed module mass
# ------------------------------------------------------------------------------

# Installed mass in tonnes:
# gross capacity in MW multiplied by specific module mass in tonnes per MW.
installed_mass_rooftop_csi <- data.frame(
  amount = module_mass_t_per_mw[, "tonnes_per_mw"] * repowering_rooftop_csi$gross_capacity
)

installed_mass_rooftop_cdte <- data.frame(
  amount = module_mass_t_per_mw[, "tonnes_per_mw"] * repowering_rooftop_cdte$gross_capacity
)

installed_mass_ground_csi <- data.frame(
  amount = module_mass_t_per_mw[, "tonnes_per_mw"] * repowering_ground_csi$gross_capacity
)

installed_mass_ground_cdte <- data.frame(
  amount = module_mass_t_per_mw[, "tonnes_per_mw"] * repowering_ground_cdte$gross_capacity
)

rownames(installed_mass_rooftop_csi) <- seq(first_year + 1, last_year, 1)
rownames(installed_mass_rooftop_cdte) <- seq(first_year + 1, last_year, 1)
rownames(installed_mass_ground_csi) <- seq(first_year + 1, last_year, 1)
rownames(installed_mass_ground_cdte) <- seq(first_year + 1, last_year, 1)

installed_mass <- data.frame(
  amount =
    installed_mass_rooftop_csi[, "amount"] +
    installed_mass_rooftop_cdte[, "amount"] +
    installed_mass_ground_csi[, "amount"] +
    installed_mass_ground_cdte[, "amount"]
)
rownames(installed_mass) <- seq(first_year + 1, last_year, 1)

amount_vector_rooftop_csi <- installed_mass_rooftop_csi[, "amount"]
amount_vector_rooftop_cdte <- installed_mass_rooftop_cdte[, "amount"]
amount_vector_ground_csi <- installed_mass_ground_csi[, "amount"]
amount_vector_ground_cdte <- installed_mass_ground_cdte[, "amount"]

# ------------------------------------------------------------------------------
# 11. Calculate annual and cumulative PV waste
# ------------------------------------------------------------------------------

# calculate_pv_waste() converts installation cohorts into annual waste using the
# segment-specific failure probabilities. 
waste_mass_rooftop_csi <- calculate_pv_waste(
  failure_probability_rooftop_csi,
  amount_vector_rooftop_csi,
  first_year,
  last_year
)
waste_mass_rooftop_csi <- waste_mass_rooftop_csi[-nrow(waste_mass_rooftop_csi), ]

waste_mass_rooftop_cdte <- calculate_pv_waste(
  failure_probability_rooftop_cdte,
  amount_vector_rooftop_cdte,
  first_year,
  last_year
)
waste_mass_rooftop_cdte <- waste_mass_rooftop_cdte[-nrow(waste_mass_rooftop_cdte), ]

waste_mass_ground_csi <- calculate_pv_waste(
  failure_probability_ground_csi,
  amount_vector_ground_csi,
  first_year,
  last_year
)
waste_mass_ground_csi <- waste_mass_ground_csi[-nrow(waste_mass_ground_csi), ]

waste_mass_ground_cdte <- calculate_pv_waste(
  failure_probability_ground_cdte,
  amount_vector_ground_cdte,
  first_year,
  last_year
)
waste_mass_ground_cdte <- waste_mass_ground_cdte[-nrow(waste_mass_ground_cdte), ]

# Total annual PV waste across all four segments.
annual_waste_amount <- data.frame(
  total_waste_amount =
    waste_mass_rooftop_csi[, 2] +
    waste_mass_ground_csi[, 2] +
    waste_mass_rooftop_cdte[, 2] +
    waste_mass_ground_cdte[, 2]
)
rownames(annual_waste_amount) <- seq(first_year + 2, last_year, 1)

# Cumulative PV waste over time.
cumulative_waste_amount <- data.frame(
  cumulative_total_waste_amount = cumsum(annual_waste_amount$total_waste_amount)
)
rownames(cumulative_waste_amount) <- seq(first_year + 2, last_year, 1)

