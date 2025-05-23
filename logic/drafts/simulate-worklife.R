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
  
  # reived values
  monthly_return <- annual_return / 12
  sd_monthly_return <- sd_annual_return / sqrt(12)
  monthly_inflation <- annual_inflation / 12
  sd_monthly_inflation <- sd_annual_inflation / sqrt(12)
  
  working_years_left <- retirement_age - age
  off_years <- working_years_left - (working_years_left * employment_rate)
  off_months <- sample( seq_len(working_years_left * 12), off_years * 12) |>
    sort()
  
  salaries_by_year <- rep(start_salary, working_years_left) |> 
    purrr::imap_dbl(
      \(start_salary, years) {
        start_salary * (1 + salary_growth) ^ (years - 1)
      }
    )
  
  salaries_by_month <- rep(salaries_by_year, each = 12)
  salaries_by_month[off_months] <- 0
  
  montly_periods <- length(salaries_by_month)
  
  ordinary_deposits <- salaries_by_month * SAVE_PROPORTION
  extra_deposits <- salaries_by_month * extra_save_rate
  
  simulation_dim <- dplyr::lst(
    rows = montly_periods,
    cols = n_simulations,
    n = rows * cols
  )
  
  
  returns <- rnorm(
    simulation_dim$n, 
    mean = monthly_return,
    sd = sd_monthly_return
  ) |>
    matrix(simulation_dim$rows, simulation_dim$cols)
  
  inflation <- rnorm(
    simulation_dim$n, 
    mean = monthly_inflation,
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
      balance_evolution[period, ] * 
      (1 + real_returns[period, ]) + 
      ordinary_deposits[period] + 
      extra_deposits[period]
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
