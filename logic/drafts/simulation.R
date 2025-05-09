library(tidyr)
library(dplyr)
library(ggplot2)

# Parameters ----------------------------------------------------------------------------------

simmulate_retirement <- function(
    start_balance = 10000,
    annual_return = 0.12,
    sd_anual_return = 0.03,
    annual_inflation = 0.04,
    sd_annual_inflation = 0.01,
    n_simulations = 500,
    years = 20,
    montly_Withdrawals = 60
) {

  montly_periods <- years * 12
  simulation_dim <- dplyr::lst(
    rows = montly_periods,
    cols = n_simulations,
    n = rows * cols
  )
  
  monthly_return <- annual_return / 12
  sd_monthly_return <- sd_anual_return / sqrt(12)
  
  montly_inflation <- annual_inflation / 12
  sd_monthly_inflation <- sd_annual_inflation / sqrt(12)
  
  returns <- rnorm(
    simulation_dim$n, 
    mean = monthly_return,
    sd = sd_monthly_return
  ) |>
    matrix(simulation_dim$rows, simulation_dim$cols)
  
  inflation <- rnorm(
    simulation_dim$n, 
    mean = montly_inflation,
    sd = sd_monthly_inflation
  ) |>
    matrix(simulation_dim$rows, simulation_dim$cols)
  
  real_returns <- returns - inflation
  
  balance_evolution <- matrix(
    start_balance,
    simulation_dim$rows + 1,
    simulation_dim$cols
  )
  
  for (period in seq_len(simulation_dim$rows)) {
    balance_evolution[period + 1, ] <- 
      balance_evolution[period, ] * (1 + real_returns[period, ]) - montly_Withdrawals
  }
  
  
  balance_evolution[ balance_evolution < 0 ] = NA
  
  balance_evolution |>
    as.data.frame() |>
    tibble::rowid_to_column("month") |> 
    tidyr::pivot_longer(
      cols = -month,
      values_to = "balance",
      names_to = "simulation"
    ) |>
    dplyr::mutate(simulation = readr::parse_number(simulation))
}

n_simulations <- 100
start_capital <- 10e6
montly_Withdrawals <- 90e3

annual_return <- 0.12
monthly_return <- annual_return / 12
sd_annual_return <- 0.03
sd_monthly_return <- sd_annual_return / sqrt(12)

annual_inflation  <- 0.04
montly_inflation <- annual_inflation / 12
sd_annual_inflation <- 0.01
sd_monthly_inflation <- sd_annual_inflation / sqrt(12)

periods_in_years <- 20
periods_in_months <- periods_in_years * 12


returns <- rnorm(
  periods_in_months * n_simulations, 
  mean = monthly_return,
  sd = sd_monthly_return
) |>
  matrix(periods_in_months, n_simulations)

inflation <- rnorm(
  periods_in_months * n_simulations, 
  mean = montly_inflation,
  sd = sd_monthly_inflation
) |>
  matrix(periods_in_months, n_simulations)


result <- matrix(start_capital, periods_in_months + 1, n_simulations)

for (period in seq_len(periods_in_months)) {
  print(period)
  result[period + 1, ] <- 
    result[period, ] * (1 + returns[period, ] - inflation[period, ]) - montly_Withdrawals
}

result[ result < 0 ] = NA

result_df <- result |>
  as.data.frame() |>
  tibble::rowid_to_column("month") |> 
  pivot_longer(
    cols = -month,
    values_to = "balance",
    names_to = "simulation"
  ) |>
  mutate(simulation = readr::parse_number(simulation))

result_df |>
  ggplot(aes(x = month, y = balance, group = simulation)) +
  geom_line()
