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
    stats::setNames(table_name) |> 
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

#' Tabla de mortalidad oficial
#' 
#' fuente: https://www.sipen.gob.do/documentos/norm_circular_15_03.pdf
#' @export
get_mortality_table_sipen <- function(sex = "male") {
 mortality_table_sipen |>
   dplyr::select(dplyr::all_of(c("edad", ifelse(sex == "male", "qx_hombres", "qx_mujeres")))) |>
   stats::setNames(c("edad", "qx")) |>
   dplyr::mutate(qx = qx / 1000)
}

mortality_table_sipen <- tibble::tribble(
   ~edad, ~qx_hombres, ~qx_mujeres,
        15,    0.4255,      0.1546,
        16,    0.4555,      0.1538,
        17,    0.4915,      0.1578,
        18,    0.5335,      0.1667,
        19,    0.5814,      0.1795,
        20,    0.6347,      0.1942,
        21,    0.6934,      0.2092,
        22,    0.7570,      0.2236,
        23,    0.8253,      0.2367,
        24,    0.8980,      0.2485,
        25,    0.9747,      0.2595,
        26,    1.0554,      0.2705,
        27,    1.1397,      0.2822,
        28,    1.2275,      0.2956,
        29,    1.3188,      0.3112,
        30,    1.4135,      0.3296,
        31,    1.5117,      0.3512,
        32,    1.6136,      0.3763,
        33,    1.7192,      0.4051,
        34,    1.8290,      0.4378,
        35,    1.9434,      0.4818,
        36,    2.0627,      0.5313,
        37,    2.1878,      0.5964,
        38,    2.3192,      0.6703,
        39,    2.4580,      0.7535,
        40,    2.6050,      0.8468,
        41,    2.7616,      0.9509,
        42,    2.9290,      1.0667,
        43,    3.1088,      1.1949,
        44,    3.3027,      1.3367,
        45,    3.5127,      1.4930,
        46,    3.7410,      1.6649,
        47,    3.9900,      1.8537,
        48,    4.2624,      2.0608,
        49,    4.5612,      2.2877,
        50,    4.8897,      2.5359,
        51,    5.2517,      2.8072,
        52,    5.6512,      3.1037,
        53,    6.0928,      3.4274,
        54,    6.5813,      3.7807,
        55,    7.1224,      4.1663,
        56,    7.7221,      4.5868,
        57,    8.3871,      5.0455,
        58,    9.1248,      5.5457,
        59,    9.9435,      6.1030,
        60,   10.8521,      6.7189,
        61,   11.8606,      7.3998,
        62,   12.9800,      8.1526,
        63,   14.2222,      8.9850,
        64,   15.6007,      9.9055,
        65,   17.1300,     10.9233,
        66,   18.8260,     12.0486,
        67,   20.7064,     13.2927,
        68,   22.7904,     14.6678,
        69,   25.0988,     16.1874,
        70,   27.6547,     17.8661,
        71,   30.4829,     19.7199,
        72,   33.6104,     21.7665,
        73,   37.0665,     24.0247,
        74,   40.8827,     26.5152,
        75,   45.0931,     29.2603,
        76,   49.7339,     32.2843,
        77,   54.8439,     35.6130,
        78,   60.4644,     39.2744,
        79,   66.6387,     43.2982,
        80,   73.4124,     47.7163,
        81,   80.8330,     52.5623,
        82,   88.9495,     57.8716,
        83,   97.8119,     63.6814,
        84,  107.4708,     70.0304,
        85,  117.8897,     76.9996,
        86,  129.1023,     84.6450,
        87,  141.1402,     93.0268,
        88,  154.0318,    102.2090,
        89,  167.8022,    112.2596,
        90,  182.4724,    123.2500,
        91,  198.0582,    135.2552,
        92,  214.5695,    148.3528,
        93,  232.0100,    162.6226,
        94,  250.3759,    178.1455,
        95,  269.6554,    195.0024,
        96,  289.8282,    213.2722,
        97,  310.8646,    233.0306,
        98,  332.7255,    254.3467,
        99,  355.3618,    277.2810,
        100,  378.7140,    301.8813,
        101,  402.7130,    328.1793,
        102,  427.2796,    356.1856,
        103,  452.3255,    385.8853,
        104,  477.7537,    417.2323,
        105,  503.4594,    450.1443,
        106,  529.3317,    484.4971,
        107,  555.2543,    520.1196,
        108,  581.1075,    556.7900,
        109,  606.7700,    594.2333,
        110, 1000.0000,   1000.0000
)
