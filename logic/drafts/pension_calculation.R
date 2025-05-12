box::use(
  logic / get_mortality_table[...]
)

get_mortality_table()
get_mortality_table_sipen()

calcular_pension_mensual <- function(
    edad_actual = 60, 
    saldo_cci = 10e6, 
    tasa_interes = 0.05,
    tabla_mortalidad = get_mortality_table_sipen()
) {

  tasa_mensual <- (1 + tasa_interes)^(1/12) - 1
  
  # Filtrar la tabla de mortalidad para edades >= edad_actual
  tabla_filtrada <- tabla_mortalidad[tabla_mortalidad$edad >= edad_actual, ]
  
  # Calcular lx (número de sobrevivientes) a partir de qx (probabilidad de fallecer)
  tabla_filtrada$lx <- cumprod(c(1, 1 - head(tabla_filtrada$qx, -1)))
  
  # Calcular los pagos mensuales, incluyendo el pago adicional de Navidad
  pagos_anuales <- rep(1, 12)
  pagos_anuales[12] <- 2  # Pago doble en diciembre
  pagos_mensuales <- rep(pagos_anuales, length.out = nrow(tabla_filtrada) * 12)
  
  # Calcular el valor presente de los pagos
  v <- 1 / (1 + tasa_mensual)
  n <- length(pagos_mensuales)
  descuentos <- v^(0:(n - 1))
  
  # Ajustar lx para cada mes
  lx_mensual <- rep(tabla_filtrada$lx, each = 12)[1:n]
  
  # Calcular rpx (valor presente actuarial)
  rpx <- sum(pagos_mensuales * descuentos * lx_mensual)
  
  # Calcular la pensión mensual
  pension_mensual <- saldo_cci / rpx
  pension_mensual
}

calcular_pension_mensual()
