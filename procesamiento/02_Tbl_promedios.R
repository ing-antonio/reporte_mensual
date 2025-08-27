# ───────────────────────────────────────────────
# 1. Cargar paquetes necesarios
# ───────────────────────────────────────────────
pacman::p_load(
  here, showtext, sysfonts,dplyr, lubridate, DBI, RPostgreSQL, janitor, gt, stringr, purrr, webshot2
)
here()

# ───────────────────────────────────────────────
# 2. Cargar fuente y funciones 
# ───────────────────────────────────────────────
# Agregar Roboto desde Google Fonts
font_add_google("Roboto", "roboto")
# Activar uso de showtext
showtext_auto()
#script de funciones se ubica en carpeta de auxiliares
source(here('auxiliares', "funciones_mensual.R"))

# ───────────────────────────────────────────────
# 3. Conexión a la BD
# ───────────────────────────────────────────────
#Cargar archivo config que debe estar en carpeta de auxiliares
config_file <- here("auxiliares/config.yml")
config <- yaml::read_yaml(config_file)

# Conectar a la base de datos usando los valores del archivo de configuración
conn <- dbConnect(
  "PostgreSQL", 
  dbname = config$db_name, 
  host = config$db_host,
  port = config$db_port, 
  user = config$db_username, 
  password = config$db_password
)
dbSendQuery(conn, "SET client_encoding = 'UTF8';")


# ───────────────────────────────────────────────
# 4. Sacar periodos con corte al mes completo
# ───────────────────────────────────────────────
# Corte al final del mes anterior
fecha_corte <- today() - day(today())  

mes_actual <- month(fecha_corte)
mes_anterior <- ifelse(mes_actual == 1, 12, mes_actual - 1)
anio_actual <- year(fecha_corte)
anio_anterior <- anio_actual - 1
anio_pasado <- anio_actual - 2

meses_espaniol <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
mes_actual_esp <- meses_espaniol[mes_actual]
mes_anterior_esp <- meses_espaniol[mes_anterior]


# Días de cada mes
dias_mes_actual <- days_in_month(ymd(sprintf("%04d-%02d-01", anio_actual, mes_actual)))
dias_mes_anterior <- days_in_month(ymd(sprintf("%04d-%02d-01", anio_actual, mes_anterior)))
dias_mes_anio_prev <- days_in_month(ymd(sprintf("%04d-%02d-01", anio_anterior, mes_actual)))
dias_mes_anio_past <- days_in_month(ymd(sprintf("%04d-%02d-01", anio_pasado, mes_actual)))

# ───────────────────────────────────────────────
# 5. Consulta base de carpetas
# ───────────────────────────────────────────────
#Carpetas de delitos de alo impacto de 2023 a la fecha
carpetas <- dbGetQuery(conn, paste0(
  "SELECT DATE(fecha_inicio) AS fecha_inicio, categoria_delito
   FROM dashboard_seguridad.carpetas_directas_cc_cdmx
   WHERE fecha_inicio >= '2023-01-01'
   AND categoria_delito != 'Delito de bajo impacto'"
)) %>% as_tibble()


# ───────────────────────────────────────────────
# 6. Agrupar datos por periodos 
# ───────────────────────────────────────────────

#Aqui se usan algunas de las funciones del script funciones_mesual.R
df_mes_actual     <- filtrar_mes(carpetas, fecha_inicio, anio_actual, mes_actual)
df_mes_anterior   <- filtrar_mes(carpetas, fecha_inicio, anio_actual, mes_anterior)
df_mes_anio_prev  <- filtrar_mes(carpetas, fecha_inicio, anio_anterior, mes_actual)
df_mes_anio_past  <- filtrar_mes(carpetas, fecha_inicio, anio_pasado, mes_actual)

#se suman los delitos por periodo
actual   <- contar_por_delito(df_mes_actual, "Actual")
previo   <- contar_por_delito(df_mes_anterior, "Mes_Anterior")
anterior <- contar_por_delito(df_mes_anio_prev, "Anterior")
pasado   <- contar_por_delito(df_mes_anio_past, "Pasado")

#se sacan los promedios diarios de los delitos por periodo
actual <- actual %>% mutate(Actual = Actual / dias_mes_actual)
previo <- previo %>% mutate(Mes_Anterior = Mes_Anterior / dias_mes_anterior)
anterior <- anterior %>% mutate(Anterior = Anterior / dias_mes_anio_prev)
pasado <- pasado %>% mutate(Pasado = Pasado / dias_mes_anio_past)


#Usa la función reduce() del paquete purrr para hacer una cadena de uniones (joins).
# full_join junta todos los data frames por la columna categoria_delito
# Si alguna categoría está en uno y no en otro grupo, se conserva igual (por eso es "full").


df <- reduce(list(previo, actual, anterior, pasado), full_join, by = "categoria_delito") %>%
  replace(is.na(.), 0) %>%
  mutate(
    variacion_mensual = variaciones(Actual, Mes_Anterior),
    variacion_anual = variaciones(Actual, Anterior)
  )

# ───────────────────────────────────────────────
# 7. Consulta a la tabla de victimas 
# ───────────────────────────────────────────────

victimas <- dbGetQuery(conn, "
  SELECT fechainicio FROM dashboard_seguridad.victimas
  WHERE fechainicio >= '2023-01-01'
") %>% clean_names()

# ───────────────────────────────────────────────
# 8. Agrupar datos de victimas por periodos
# ───────────────────────────────────────────────

df_victimas <- tibble(
  categoria_delito = "Homicidio doloso (víctimas)",
  Actual = victimas_mes(anio_actual, mes_actual) / dias_mes_actual,
  Mes_Anterior = victimas_mes(anio_actual, mes_anterior) / dias_mes_anterior,
  Anterior = victimas_mes(anio_anterior, mes_actual) / dias_mes_anio_prev,
  Pasado = victimas_mes(anio_pasado, mes_actual) / dias_mes_anio_past
) %>%
  mutate(
    variacion_mensual = variaciones(Actual, Mes_Anterior),
    variacion_anual = variaciones(Actual, Anterior)
  )

# ───────────────────────────────────────────────
# 9. Se unen las dos tablas:carpetas y víctimas
# ───────────────────────────────────────────────
df <- bind_rows(df, df_victimas)

# Fila resumen: Total Alto Impacto 
fila_alto_impacto <- df %>%
  filter(categoria_delito != 'Homicidio doloso (víctimas)') %>%
  summarise(
    categoria_delito = "Alto Impacto",
    Mes_Anterior = sum(Mes_Anterior),
    Actual = sum(Actual),
    Anterior = sum(Anterior),
    Pasado = sum(Pasado),
    variacion_mensual = variaciones(sum(Actual), sum(Mes_Anterior)),
    variacion_anual = variaciones(sum(Actual), sum(Anterior))
  )

# Fila robo de vehículo c/s violencia 
fila_robo <- df %>%
  filter(categoria_delito == "Robo de vehículo con violencia" | categoria_delito == 'Robo de vehículo sin violencia') %>%
  summarise(
    categoria_delito = "Robo de vehículo con y sin violencia",
    Mes_Anterior = sum(Mes_Anterior),
    Actual = sum(Actual),
    Anterior = sum(Anterior),
    Pasado = sum(Pasado),
    variacion_mensual = variaciones(sum(Actual), sum(Mes_Anterior)),
    variacion_anual = variaciones(sum(Actual), sum(Anterior))
  )

# ───────────────────────────────────────────────
# 10. Se agregan las filas de total alto impacto y robo con y sin violencia
# ───────────────────────────────────────────────

df <- bind_rows(fila_alto_impacto,fila_robo, df)

# ───────────────────────────────────────────────
# 11. Tabla final con gt()
# ───────────────────────────────────────────────
# Se ordenan los delitos para que así salgan en la tabla
orden_delitos <- c(
  "Alto Impacto" = 1,
  "Homicidio doloso" = 2,
  "Homicidio doloso (víctimas)" = 3,
  "Lesiones dolosas por disparo de arma de fuego" = 4,
  "Robo a transeunte en vía pública con y sin violencia" = 5,
  "Robo a transeunte en vía pública con violencia" = 6,
  "Robo a transeunte en vía pública sin violencia" = 7,
  "Robo de vehículo con y sin violencia" = 8,
  "Robo de vehículo con violencia" = 9,
  "Robo de vehículo sin violencia" = 10,
  "Secuestro" = 11,
  "Violación" = 12,
  "Robo a negocio con violencia" = 13,
  "Robo a pasajero a bordo del metro con y sin violencia" = 14,
  "Robo a repartidor con y sin violencia" = 15,
  "Robo a pasajero a bordo de microbus con y sin violencia" = 16,
  "Robo a cuentahabiente saliendo del cajero con violencia" = 17,
  "Robo a pasajero a bordo de taxi con violencia" = 18,
  "Robo a casa habitación con violencia" = 19,
  "Robo a transportista con y sin violencia" = 20,
  "Robo a transeúnte en vía pública con y sin violencia" = 21,
  "Robo a pasajero a bordo de microbús con y sin violencia" = 22,
  "Delito de bajo impacto" = 23,
  "Robo a transeúnte en vía pública con violencia" = 24,
  "Robo a transeúnte en vía pública sin violencia" = 25,
  "Robo a pasajero a bordo del metro con violencia" = 26,
  "Robo a pasajero a bordo del metro sin violencia" = 27
)

# Aplicar el orden en las columnas
df <- df %>%
  mutate(orden = orden_delitos[categoria_delito]) %>%
  arrange(orden) %>%
  mutate(Actual_repetido = Actual) %>%   # 👈 crea una nueva columna para que salga de nuevo el mes actual
  select(
    categoria_delito,
    Mes_Anterior,
    Actual,
    variacion_mensual,
    Pasado,
    Anterior,
    Actual_repetido,
    variacion_anual
  )



tabla_gt <- df %>%
  gt() %>%
  cols_label(
    categoria_delito = "Categoría del Delito",
    Mes_Anterior = paste0(mes_anterior_esp, " ", anio_actual),
    Actual = paste0(mes_actual_esp, " ", anio_actual),
    Anterior = paste0(mes_actual_esp, " ", anio_anterior),
    Pasado = paste0(mes_actual_esp, " ", anio_pasado),
    Actual_repetido = paste0(mes_actual_esp, " ", anio_actual),
    variacion_mensual = paste0("Var ", mes_anterior_esp, " ", anio_actual, " vs ", mes_actual_esp, " ", anio_actual),
    variacion_anual = paste0("Var ", mes_actual_esp, " ", anio_anterior, " vs ",mes_actual_esp, " ", anio_actual)
  ) %>%
  
  ## Formato numérico
  fmt_number(
    columns = c(Mes_Anterior, Actual, Pasado, Anterior, Actual_repetido),
    decimals = 1,
    sep_mark = ",",
    drop_trailing_zeros = TRUE
  ) %>%
  ## Formato para las variaciones
  fmt_number(
    columns = c(variacion_mensual,variacion_anual),
    decimals = 1,
    sep_mark = ",",
    drop_trailing_zeros = TRUE
  ) %>%
  ## Encabezados color vino
  tab_style(
    style = list(
      cell_fill(color = "#9d2041ff"),
      cell_text(color = "white", weight = "bold", size = px(16))
    ),
    locations = cells_column_labels(everything())
  ) %>%
  
  ## Total de alto impacto
  tab_style(
    style =list(cell_fill(color = "#DAD1C5"), cell_text(weight = "bold")),
    locations = cells_body(rows = categoria_delito == "Alto Impacto")
  ) %>%
  
  # Sangría para todos excepto "Alto Impacto" y subtipos de robo de vehículo
  tab_style(
    style = cell_text(indent = px(16)),
    locations = cells_body(columns = categoria_delito, 
                           rows = categoria_delito != "Alto Impacto" &
                             categoria_delito != "Robo de vehículo con violencia" &
                             categoria_delito != "Robo de vehículo sin violencia")
  ) %>%
  # Sangría grande para los subtipos de robo de vehículo y víctimas
  tab_style(
    style = cell_text(indent = px(32)),
    locations = cells_body(
      columns = categoria_delito,
      rows = categoria_delito %in% c("Homicidio doloso (víctimas)", "Robo de vehículo con violencia", "Robo de vehículo sin violencia")
    )
  ) %>%
  
  ## Variaciones negativas (verde)
  tab_style(
    style = list(cell_fill(color = "#E7FBF1"), cell_text(color = "#2A6F4D")),
    locations = list(
      cells_body(columns = variacion_mensual, rows = str_detect(variacion_mensual, "-")),
      cells_body(columns = variacion_anual, rows = str_detect(variacion_anual, "-"))
    )
  ) %>%
  
  ## Variaciones positivas (rojo)
  tab_style(
    style = list(cell_fill(color = "#FCDADE"), cell_text(color = "#940B1C")),
    locations = list(
      cells_body(columns = variacion_mensual, rows = !str_detect(variacion_mensual, "-") & variacion_mensual != "0%"),
      cells_body(columns = variacion_anual, rows = !str_detect(variacion_anual, "-") & variacion_anual != "0%")
    )
  ) %>%
  
  ## Variaciones sin cambio (gris)
  tab_style(
    style = list(cell_fill(color = "#DDDDDD"), cell_text(color = "#252627")),
    locations = list(
      cells_body(columns = variacion_mensual, rows = variacion_mensual == "0.0%"),
      cells_body(columns = variacion_anual, rows = variacion_anual == "0.0%")
    )
  ) %>%
  
  ## Opciones generales
  cols_width(
    categoria_delito ~ px(320),
    variacion_mensual ~ px(120),
    variacion_anual ~ px(120),
    everything() ~ px(100)  # las demás columnas
  ) %>%
  tab_options(
    table.font.names = "roboto", 
    table.width = "100%",
    table.font.size = px(13),
    data_row.padding = px(4)
  )

tabla_gt
# ───────────────────────────────────────────────
# 12. Guardar la tabla como png
# ───────────────────────────────────────────────
ruta_salida <- here("salidas", "tabla_variacion_mensual_promedios.png")

if (file.exists(ruta_salida)) {
  file.remove(ruta_salida)
}

gtsave(
  tabla_gt,
  filename = ruta_salida,
  vwidth = 1200,
  vheight = 1000
)


