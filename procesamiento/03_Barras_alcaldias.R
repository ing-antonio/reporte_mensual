# ───────────────────────────────────────────────
# 1. Cargar paquetes necesarios
# ───────────────────────────────────────────────
pacman::p_load(
  here, showtext,ggplot2, ggtext, patchwork, sysfonts,dplyr, lubridate, DBI, RPostgreSQL, janitor, gt, stringr, purrr, webshot2
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
# 4. Consultar BD
# ───────────────────────────────────────────────
# Cargar datos por alcaldía
df_alcaldia <- dbGetQuery(
  conn,
  paste0(
    "SELECT
        DATE(fecha_inicio) AS fecha_inicio,
        alcaldia_hecho
     FROM dashboard_seguridad.carpetas_directas_cc_cdmx
     WHERE 
        fecha_inicio >= '2023-01-01'
        AND categoria_delito != 'Delito de bajo impacto'"
  )
) %>% as_tibble()

# ───────────────────────────────────────────────
# 5. Definición de fechas de corte
# ───────────────────────────────────────────────
# Último día completo del mes anterior
fecha_corte <- today() - day(today())
anio_actual <- year(fecha_corte)
anio_anterior <- anio_actual - 1
anio_pasado <- anio_actual - 2
mes_actual <- month(fecha_corte)
dia_final_mes <- day(fecha_corte)

# Formatos
mes_fmt <- sprintf("%02d", mes_actual)
dia_fmt <- sprintf("%02d", dia_final_mes)

# ───────────────────────────────────────────────
# 3. Periodos mensuales para gráfico "mensual"

# ───────────────────────────────────────────────
mes_anterior <- ifelse(mes_actual == 1, 12, mes_actual - 1)

df_mensual_alcaldia <- df_alcaldia %>%
  filter(fecha_inicio >= as.Date("2024-12-30")) %>%
  mutate(
    mes = month(fecha_inicio),
    periodo = case_when(
      mes == mes_anterior ~ "Anterior",
      mes == mes_actual ~ "Actual"
    )
  ) %>%
  filter(!is.na(periodo))

# ───────────────────────────────────────────────
# 4. Periodos anuales comparables (mismo mes, distinto año)
# ───────────────────────────────────────────────
df_anual_alcaldia <- df_alcaldia %>%
  mutate(periodo = case_when(
    between(fecha_inicio,
            as.Date(paste0(anio_anterior, "-", mes_fmt, "-01")),
            as.Date(paste0(anio_anterior, "-", mes_fmt, "-", dia_fmt))) ~ "Anterior",
    between(fecha_inicio,
            as.Date(paste0(anio_actual, "-", mes_fmt, "-01")),
            as.Date(paste0(anio_actual, "-", mes_fmt, "-", dia_fmt))) ~ "Actual"
  )) %>%
  filter(!is.na(periodo))
# ───────────────────────────────────────────────
# 5. Generar gráficos comparativos
# (requiere funciones externas definidas en script de funciones_mensual.R)
# ───────────────────────────────────────────────

grafico_mensual <- grafico_comparada(df_mensual_alcaldia, "mensual")  # antes llamado "semanal"
grafico_anual <- grafico_comparada(df_anual_alcaldia, "anual")

grafico_combinado <- grafico_mensual | grafico_anual

# Guardar
ggsave(
  here('salidas', 'graficas_combinadas.png'),
  plot = grafico_combinado,
  width = 15,
  height = 7
)
