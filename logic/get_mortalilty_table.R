#' Get Mortality Table by Year and Sex
#'
#' Reads and returns a mortality table from an Excel file based on the given year and sex.
#'
#' @param year Integer. The year of the mortality table. Default is 2025.
#' @param sex Character. Sex for which the table is extracted. Either `"female"` or `"male"`.
#' @param file Character. Path to the Excel file containing mortality tables. 
#'   Default is `"data/cuadro-tablas-completas-de-mortalidad-1950-2050.xlsx"`.
#'
#' @return A tibble with 8 columns: `edad`, `mx`, `qx`, `lx`, `dx`, `lx_i`, `tx`, and `ex`.
#' @export
get_mortality_table <- function(
    year = 2025, 
    sex = c("female", "male"),
    file = "data/cuadro-tablas-completas-de-mortalidad-1950-2050.xlsx"
) {

  sex <- match.arg(sex)
  if (!file.exists(file)) stop(glue::glue("File {file} doesn't exist"))
  if (!is.numeric(year) || length(year) != 1) {
    stop("`year` must be a single numeric value.")
  }
  
  MALE_COLS   <- 1:8
  FEMALE_COLS <- 9:16
  
  col_index <- switch(
    sex,
    male    = MALE_COLS,
    female  = FEMALE_COLS
  )
  
  table_name <-  c(
    "edad",
    "mx",
    "qx",
    "lx",
    "dx",
    "lx_i",
    "tx",
    "ex"
  )
  
  sheet <- get_sheet(year)
  
  readxl::read_excel(
    file, 
    sheet = sheet,
    col_names = FALSE
  ) |>
    janitor::remove_empty() |> 
    tidyr::drop_na() |>
    dplyr::slice(-1) |>
    dplyr::mutate(across(everything(), readr::parse_number)) |>
    dplyr::select(dplyr::all_of(col_index)) |>
    setNames(table_name) |> 
    suppressMessages()  
}

sheets_df <- tibble::tribble(
  ~sheet, ~low,  ~hi,
  "1950-1955", 1950, 1955,
  "1955-1960", 1955, 1960,
  "1960-1965", 1960, 1965,
  "1965-1970", 1965, 1970,
  "1970-1975", 1970, 1975,
  "1975-1980", 1975, 1980,
  "1980-1985", 1980, 1985,
  "1985-1990", 1985, 1990,
  "1990-1995", 1990, 1995,
  "1995-2000", 1995, 2000,
  "2000-2005", 2000, 2005,
  "2005-2010", 2005, 2010,
  "2010-2015", 2010, 2015,
  "2015-2020", 2015, 2020,
  "2020-2025", 2020, 2025,
  "2025-2030", 2025, 2030,
  "2030-2035", 2030, 2035,
  "2035-2040", 2035, 2040,
  "2040-2045", 2040, 2045,
  "2045-2050", 2045, 2050
)

get_sheet <- function(year) {
  year <- as.numeric(year)
  if (year > 2050 || year < 1950) stop("Pick a year between 1950 and 2050")
  
  sheets_df$sheet[sheets_df$low < year & sheets_df$hi >= year]
}
