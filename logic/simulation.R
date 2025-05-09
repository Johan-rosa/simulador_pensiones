#' Simulate Retirement Account Depletion Over Time
#'
#' This function performs Monte Carlo simulations of a retirement account's balance 
#' over a fixed number of years, accounting for inflation, market returns, and 
#' regular monthly withdrawals.
#'
#' @param start_balance Numeric. Initial retirement account balance. Default is 10,000.
#' @param annual_return Numeric. Expected annual return rate (as a decimal). Default is 0.12.
#' @param sd_annual_return Numeric. Standard deviation of annual return. Default is 0.03.
#' @param annual_inflation Numeric. Expected annual inflation rate. Default is 0.04.
#' @param sd_annual_inflation Numeric. Standard deviation of annual inflation. Default is 0.01.
#' @param n_simulations Integer. Number of simulation paths to run. Default is 500.
#' @param years Integer. Number of years to simulate. Default is 20.
#' @param monthly_withdrawals Numeric. Fixed monthly withdrawal amount. Default is 60.
#'
#' @return A long-format data frame with the simulated monthly balance evolution.
#' @examples
#' sim_data <- simulate_retirement()
#' library(ggplot2)
#' ggplot(sim_data, aes(month, balance, group = simulation)) +
#'   geom_line(alpha = 0.1) +
#'   labs(title = "Simulated Retirement Account Balance")
simulate_retirement <- function(
    start_balance = 10000,
    annual_return = 0.12,
    sd_annual_return = 0.03,
    annual_inflation = 0.04,
    sd_annual_inflation = 0.01,
    n_simulations = 500,
    years = 20,
    monthly_withdrawals = 60
) {
  monthly_periods <- years * 12
  simulation_dim <- list(
    rows = monthly_periods,
    cols = n_simulations,
    n = monthly_periods * n_simulations
  )
  
  monthly_return <- annual_return / 12
  sd_monthly_return <- sd_annual_return / sqrt(12)
  
  monthly_inflation <- annual_inflation / 12
  sd_monthly_inflation <- sd_annual_inflation / sqrt(12)
  
  returns <- rnorm(simulation_dim$n, mean = monthly_return, sd = sd_monthly_return) |>
    matrix(nrow = simulation_dim$rows, ncol = simulation_dim$cols)
  
  inflation <- rnorm(simulation_dim$n, mean = monthly_inflation, sd = sd_monthly_inflation) |>
    matrix(nrow = simulation_dim$rows, ncol = simulation_dim$cols)
  
  real_returns <- returns - inflation
  
  balance_evolution <- matrix(start_balance, nrow = simulation_dim$rows + 1, ncol = simulation_dim$cols)
  
  for (period in seq_len(simulation_dim$rows)) {
    balance_evolution[period + 1, ] <- 
      balance_evolution[period, ] * (1 + real_returns[period, ]) - monthly_withdrawals
  }
  
  balance_evolution[balance_evolution < 0] <- NA
  
  balance_evolution |>
    as.data.frame() |>
    tibble::rowid_to_column("month") |> 
    tidyr::pivot_longer(
      cols = -month,
      names_to = "simulation",
      values_to = "balance"
    ) |>
    dplyr::mutate(simulation = readr::parse_number(simulation))
}
