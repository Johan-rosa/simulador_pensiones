library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

tablas_file <- "data/cuadro-tablas-completas-de-mortalidad-1950-2050.xlsx"

sheets <- excel_sheets(tablas_file) |>
  str_subset("^\\d{4}")


sheets_df <- tibble::tibble(
  sheet = sheets,
  range = sheets
) |>
  separate(range, into = c("low", "hi"), sep = "-") |>
  mutate(across(-sheet, as.numeric))

cols_masculino <- c(1:8)
cols_femenino  <- c(9:16)
tabla_names <-  c(
  "edad",
  "mx",
  "qx",
  "lx",
  "dx",
  "lx_i",
  "tx",
  "ex"
)

tabla_raw <- read_excel(
  tablas_file, 
  sheet = "2025-2030",
  col_names = FALSE
) |>
  janitor::remove_empty() |> 
  drop_na() |>
  slice(-1) |>
  mutate(across(everything(), readr::parse_number)) |>
  suppressMessages() |> 
  select(all_of(cols_masculino)) |>
  setNames(tabla_names)



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

get_mortality_table <- function(year = 2025, sex = c("female", "male")) {
  
  col_index <- switch(
    sex,
    male    = 1:8,
    female  = 9:16
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
  
  read_excel(
    tablas_file, 
    sheet = sheet,
    col_names = FALSE
  ) |>
    janitor::remove_empty() |> 
    drop_na() |>
    slice(-1) |>
    mutate(across(everything(), readr::parse_number)) |>
    select(all_of(col_index)) |>
    setNames(tabla_names) |> 
    suppressMessages()  
}

get_mortality_table(sex = "female")

