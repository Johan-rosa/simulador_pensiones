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

#' Simulate Work-Life Savings with Uncertainty
#'
#' This function simulates the evolution of an individual's retirement savings
#' from a starting age until retirement, accounting for salary growth, employment gaps,
#' variable returns, inflation, and optional extra savings.
#'
#' @param age Numeric. Starting age of the individual. Default is 20.
#' @param start_salary Numeric. Monthly salary at starting age. Default is 1000.
#' @param retirement_age Numeric. Age at which the individual retires. Default is 60.
#' @param start_balance Numeric. Initial balance of the savings account. Default is 0.
#' @param salary_growth Numeric. Annual growth rate of salary (as a proportion). Default is 0.01.
#' @param employment_rate Numeric. Proportion of time employed during working years (between 0 and 1). Default is 1.
#' @param extra_save_rate Numeric. Proportion of salary saved in addition to the default 8%. Default is 0.00.
#' @param annual_return Numeric. Expected annual return on investment. Default is 0.09.
#' @param sd_annual_return Numeric. Standard deviation of annual returns. Default is 0.03.
#' @param annual_inflation Numeric. Expected annual inflation rate. Default is 0.04.
#' @param sd_annual_inflation Numeric. Standard deviation of annual inflation. Default is 0.01.
#' @param n_simulations Integer. Number of Monte Carlo simulations to run. Default is 500.
#'
#' @return A long-format tibble with columns `month`, `simulation`, and `balance` representing the evolution of savings over time across simulations.
#'
#' @importFrom purrr imap_dbl
#' @importFrom tibble rowid_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom readr parse_number
#'
#' @export
simulate_worklife <- function(
    age = 20,
    start_salary = 1000,
    retirement_age = 60,
    start_balance = 0,
    salary_growth = 0.01,
    employment_rate = 1,
    extra_save_rate = 0.00,
    annual_return = 0.09,
    sd_annual_return = 0.03,
    annual_inflation = 0.04,
    sd_annual_inflation = 0.01,
    n_simulations = 500
) {
  SAVE_PROPORTION <- 0.08
  
  monthly_return <- annual_return / 12
  sd_monthly_return <- sd_annual_return / sqrt(12)
  monthly_inflation <- annual_inflation / 12
  sd_monthly_inflation <- sd_annual_inflation / sqrt(12)
  
  working_years_left <- retirement_age - age
  total_months <- working_years_left * 12
  n_off_months <- round(total_months * (1 - employment_rate))
  off_months <- sort(sample(seq_len(total_months), n_off_months))
  
  salaries_by_year <- rep(start_salary, working_years_left) |> 
    purrr::imap_dbl(\(start_salary, years) {
      start_salary * (1 + salary_growth) ^ (years - 1)
    })
  
  salaries_by_month <- rep(salaries_by_year, each = 12)
  salaries_by_month[off_months] <- 0
  
  monthly_periods <- length(salaries_by_month)
  ordinary_deposits <- salaries_by_month * SAVE_PROPORTION
  extra_deposits <- salaries_by_month * extra_save_rate
  
  n <- monthly_periods * n_simulations
  returns <- rnorm(n, mean = monthly_return, sd = sd_monthly_return) |>
    matrix(monthly_periods, n_simulations)
  inflation <- rnorm(n, mean = monthly_inflation, sd = sd_monthly_inflation) |>
    matrix(monthly_periods, n_simulations)
  
  real_returns <- returns - inflation
  
  balance_evolution <- matrix(start_balance, monthly_periods + 1, n_simulations)
  
  for (period in seq_len(monthly_periods)) {
    balance_evolution[period + 1, ] <- 
      balance_evolution[period, ] * (1 + real_returns[period, ]) +
      ordinary_deposits[period] + extra_deposits[period]
  }
  
  balance_evolution[balance_evolution < 0] <- NA
  
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
