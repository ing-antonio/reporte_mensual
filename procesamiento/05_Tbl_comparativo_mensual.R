# ___________________________________________________________
# 💡 CÓDIGO PARA TABLA DE TOTALES DEL úLTIMO MES COMPLETO (esta no la han pedido pero sirve de referencia)💡
# ___________________________________________________________
# ───────────────────────────────────────────────
# 1. Cargar paquetes necesarios
# ─────────────────────────────────────────────── 
pacman::p_load(
  RPostgreSQL, dplyr, tidyr, gt, stringr, scales,
  yaml, here, glue, lubridate,
  showtext, sysfonts, readr, purrr
)

here() 

# ──────────────────────────────────────────────
# 2. Cargar fuente y funciones 
# ───────────────────────────────────────────────
# Agregar Roboto desde Google Fonts
font_add_google("Roboto", "roboto")
# Activar uso de showtext
showtext_auto()
#script de funciones se ubica en carpeta de auxiliares
source(here('auxiliares', "funciones_mensual.R"))

# ───────────────────────────────────────────────
# 3. Conexión y consulta a la BD
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
  
  #  Query carpetas
  
  # Obtener el mes anterior completo
  fecha_ultimo_mes <- floor_date(Sys.Date(), "month") - months(1)
  mes_objetivo <- month(fecha_ultimo_mes)
  anio_objetivo <- year(fecha_ultimo_mes)
  nombre_mes <- format(fecha_ultimo_mes, "%B") |> tools::toTitleCase()
  
  anios_objetivo <- c(2019, 2023, 2024, 2025)
  
  # Filtros para solo ese mes en cada año
  filtros_fecha <- paste0(
    "(fecha_inicio >= '", anios_objetivo, "-", sprintf("%02d", mes_objetivo), "-01' AND ",
    "fecha_inicio < '", anios_objetivo, "-", sprintf("%02d", mes_objetivo + 1), "-01')"
  )
  
  
  # Unir condiciones con OR
  condicion_fecha <- paste(filtros_fecha, collapse = " OR ")
  
  query_carpetas <- glue::glue("
    SELECT
      CASE 
        WHEN categoria_delito = 'Homicidio doloso' THEN 'Homicidio doloso'
        ELSE categoria_delito
      END AS categoria_delito_modificada,
      EXTRACT(YEAR FROM fecha_inicio) AS anio,
      COUNT(*) AS total_carpetas
    FROM dashboard_seguridad.carpetas_directas_cc_cdmx
    WHERE
      categoria_delito != 'Delito de bajo impacto'
      AND (
        {condicion_fecha}
      )
    GROUP BY categoria_delito_modificada, anio
    
    UNION ALL
    
    SELECT
      'Robo a pasajero a bordo del metrobús con violencia' AS categoria_delito_modificada,
      EXTRACT(YEAR FROM fecha_inicio) AS anio,
      COUNT(*) AS total_carpetas
    FROM dashboard_seguridad.carpetas_directas_cc_cdmx
    WHERE
      delito IN ('Robo a pasajero a bordo de Metrobús con violencia')
      AND (
        {condicion_fecha}
      )
    GROUP BY anio
    ")
  
  datos_carpetas <- dbGetQuery(conn, query_carpetas)
  
  #  Query víctimas
  filtros_fecha2 <- paste0(
    "(fechainicio >= '", anios_objetivo, "-", sprintf("%02d", mes_objetivo), "-01' AND ",
    "fechainicio < '", anios_objetivo, "-", sprintf("%02d", mes_objetivo + 1), "-01')"
  )
  # Unir condiciones con OR
  condicion_fecha2 <- paste(filtros_fecha2, collapse = " OR ")
  query_victimas <- glue::glue( "
    SELECT
      'Homicidio doloso (víctimas)' AS categoria_delito_modificada,
      EXTRACT(YEAR FROM fechainicio) AS anio,
      COUNT(*) AS total_carpetas
    FROM dashboard_seguridad.victimas
    WHERE
       {condicion_fecha2}
    GROUP BY anio
    ")
  
  datos_victimas <- dbGetQuery(conn, query_victimas)
  
  #  Combinar carpetas + víctimas 
  datos <- bind_rows(datos_carpetas,datos_victimas)
  
  #Guardar rdata
  save(datos, file = here("datos", "datos_crudos_semestral.RData"))


# ───────────────────────────────────────────────
# 4. Pivotear y calcular variaciones
# ───────────────────────────────────────────────

tabla <- datos %>%
  pivot_wider(
    names_from = anio,
    values_from = total_carpetas,
    names_prefix = "total_"
  ) %>%
  mutate(
    total_2019 = replace_na(total_2019, 0),
    total_2023 = replace_na(total_2023, 0),
    total_2024 = replace_na(total_2024, 0),
    total_2025 = replace_na(total_2025, 0),
    var_2025_vs_2019 = variaciones0(total_2025, total_2019),
    var_2025_vs_2023 = variaciones0(total_2025, total_2023),
    var_2025_vs_2024 = variaciones0(total_2025, total_2024)
  ) %>%
  arrange(desc(total_2025))


# ───────────────────────────────────────────────
# 5. Sacar totales de Alto impacto y de robo con y sin violencia
# ───────────────────────────────────────────────

#  Fila Alto impacto
fila_total <- tabla %>%
  filter(categoria_delito_modificada != "Homicidio doloso (víctimas)" & categoria_delito_modificada != "Robo a pasajero a bordo del metrobús con violencia") %>%   # 👈 excluye víctimas y robo metrobus
  summarise(
    categoria_delito_modificada = "Alto Impacto",
    total_2019 = sum(total_2019, na.rm = TRUE),
    total_2023 = sum(total_2023, na.rm = TRUE),
    total_2024 = sum(total_2024, na.rm = TRUE),
    total_2025 = sum(total_2025, na.rm = TRUE)
  ) %>%
  mutate(
    var_2025_vs_2019 = variaciones0(total_2025, total_2019),
    var_2025_vs_2023 = variaciones0(total_2025, total_2023),
    var_2025_vs_2024 = variaciones0(total_2025, total_2024)
  )

#  Fila robo de vehículo c/s violencia
fila_robo <- tabla %>%
  filter(categoria_delito_modificada == "Robo de vehículo con violencia" | categoria_delito_modificada == "Robo de vehículo sin violencia") %>% 
  summarise(
    categoria_delito_modificada = "Robo de vehículo con y sin violencia",
    total_2019 = sum(total_2019, na.rm = TRUE),
    total_2023 = sum(total_2023, na.rm = TRUE),
    total_2024 = sum(total_2024, na.rm = TRUE),
    total_2025 = sum(total_2025, na.rm = TRUE)
  ) %>%
  mutate(
    var_2025_vs_2019 = variaciones0(total_2025, total_2019),
    var_2025_vs_2023 = variaciones0(total_2025, total_2023),
    var_2025_vs_2024 = variaciones0(total_2025, total_2024)
  )

# ───────────────────────────────────────────────
# 6. Unir las tablas
# ───────────────────────────────────────────────
tabla_formateada <- bind_rows(fila_total,fila_robo, tabla)

# Crear vector de orden
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
  "Robo a pasajero a bordo del metro sin violencia" = 27,
  "Robo a pasajero a bordo de metrobús con violencia" = 28
)


# ───────────────────────────────────────────────
# 7. Formato de tabla y nombres de columnas
# ───────────────────────────────────────────────

#Aqui se pueden quitar delitos de la tabla (si es que lo piden)
tabla_formateada_sin_visibles <- tabla_formateada %>%
  filter(
    !categoria_delito_modificada %in% c("")
  ) %>%
  mutate(
    orden = orden_delitos[categoria_delito_modificada]
  ) %>%
  arrange(orden)

# Fecha de corte: primer día del mes actual
fecha_corte <- floor_date(Sys.Date(), unit = "month")
# Mes anterior completo (último mes con datos cerrados)
fecha_ultimo_mes <- fecha_corte - months(1)
# Obtener nombre completo del mes (en español)
nombre_mes <- format(fecha_ultimo_mes, "%B") |> tools::toTitleCase()

# Años relevantes
anios <- c(2019,2023, 2024, 2025)
etiquetas_totales <- setNames(
  glue("{nombre_mes} {anios_objetivo}"),
  paste0("total_", anios_objetivo)
)

# ───────────────────────────────────────────────
# 8. Secrea la tabla gt
# ───────────────────────────────────────────────
gt_tabla <- tabla_formateada_sin_visibles %>%
  select(
    categoria_delito_modificada,
    total_2019, total_2023, total_2024, total_2025,
    var_2025_vs_2019, var_2025_vs_2023, var_2025_vs_2024
  ) %>%
  gt() %>%
  cols_label(.list = c(
    categoria_delito_modificada = "Categoría del delito",
    etiquetas_totales,
    var_2025_vs_2019 = "Var % vs 2019",
    var_2025_vs_2023 = "Var % vs 2023",
    var_2025_vs_2024 = "Var % vs 2024"
  )) %>%
  
  fmt_number(
    columns = c(total_2025, total_2024, total_2019),
    decimals = 0,
    sep_mark = ",",
    drop_trailing_zeros = TRUE
  ) %>%
  
  ##  Estilo encabezados
  tab_style(
    style = list(
      cell_fill(color = "#9d2041ff"),
      cell_text(color = "white", weight = "bold", size = px(16))
    ),
    locations = cells_column_labels(everything())
  ) %>%
  
  ##  Fondo fila total delitos de alto impacto
  tab_style(
    style = list (cell_fill(color = "#DAD1C5"), cell_text(weight = "bold")),
    locations = cells_body(rows = categoria_delito_modificada == "Alto Impacto")
  ) %>%
  
  # Sangría para todos excepto "Alto Impacto" y subtipos de robo de vehículo
  tab_style(
    style = cell_text(indent = px(16)),
    locations = cells_body(columns = categoria_delito_modificada, 
                           rows = categoria_delito_modificada != "Alto Impacto" &
                             categoria_delito_modificada != "Robo de vehículo con violencia" &
                             categoria_delito_modificada != "Robo de vehículo sin violencia")
  ) %>%
  # Sangría grande para los subtipos de robo de vehículo y víctimas
  tab_style(
    style = cell_text(indent = px(32)),
    locations = cells_body(
      columns = categoria_delito_modificada,
      rows = categoria_delito_modificada %in% c("Homicidio doloso (víctimas)", "Robo de vehículo con violencia", "Robo de vehículo sin violencia")
    )
  ) %>%
  
  ## Fondo verde para variación negativa (disminución)
  tab_style(
    style = list(cell_fill(color = "#E7FBF1"), cell_text(color = "#2A6F4D")),
    locations = list(
      cells_body(columns = var_2025_vs_2019, rows = str_detect(var_2025_vs_2019, "-")),
      cells_body(columns = var_2025_vs_2023, rows = str_detect(var_2025_vs_2023, "-")),
      cells_body(columns = var_2025_vs_2024, rows = str_detect(var_2025_vs_2024, "-"))
    )
  ) %>%
  
  ## Fondo rojo para variación positiva (aumento)
  tab_style(
    style = list(cell_fill(color = "#FCDADE"), cell_text(color = "#940B1C")),
    locations = list(
      cells_body(columns = var_2025_vs_2019, rows = !str_detect(var_2025_vs_2019, "-") & var_2025_vs_2019 != "0%"),
      cells_body(columns = var_2025_vs_2023, rows = !str_detect(var_2025_vs_2023, "-") & var_2025_vs_2023 != "0%"),
      cells_body(columns = var_2025_vs_2024, rows = !str_detect(var_2025_vs_2024, "-") & var_2025_vs_2024 != "0%")
    )
  ) %>%
  
  ## Fondo gris para sin cambio (0%)
  tab_style(
    style = list(cell_fill(color = "#DDDDDD"), cell_text(color = "#252627")),
    locations = list(
      cells_body(columns = var_2025_vs_2019, rows = var_2025_vs_2019 == "0%"),
      cells_body(columns = var_2025_vs_2023, rows = var_2025_vs_2023 == "0%"),
      cells_body(columns = var_2025_vs_2024, rows = var_2025_vs_2024 == "0%")
    )
  )%>%
  tab_options(
    table.font.names = "roboto", 
    table.width = "100%",
    table.font.size = px(13),
    data_row.padding = px(4)
  )

# Mostrar
gt_tabla


# ───────────────────────────────────────────────
# 💡 CÓDIGO PARA MINI TABLAS DE ACUMULADOS 💡
# ───────────────────────────────────────────────
# ───────────────────────────────────────────────
# 1. Definir etiquetas dinámicas por año
# ───────────────────────────────────────────────
fecha_corte <- floor_date(Sys.Date(), unit = "month")
fecha_ultimo_mes <- fecha_corte - months(1)
nombre_mes <- format(fecha_ultimo_mes, "%B") |> tools::toTitleCase()  # "Junio", "Julio", etc.

# Etiquetas de totales acumulados
anios <- c(2019,2023,2024, 2025)
etiquetas_totales <- setNames(
  glue("{nombre_mes} {anios}"),
  paste0("total_", anios)
)

# Etiquetas de variaciones
etiquetas_variaciones <- c(
  var_2025_vs_2019 = "Var % vs 2019",
  var_2025_vs_2023 = "Var % vs 2023",
  var_2025_vs_2024 = "Var % vs 2024"
)

# Todas las etiquetas juntas
etiquetas_finales <- c(
  categoria_delito_modificada = "Delito",
  etiquetas_totales,
  etiquetas_variaciones
)

# ───────────────────────────────────────────────
# Leer CSV existente generado por acumulados 
#(donde se ponen títulos que dicen si hubo incremento o disminución y 
#se asigna la flecha del color que corresponda)
# ───────────────────────────────────────────────

ruta_csv <- here("auxiliares", "titulos_tarjetas.csv")
titulos_existentes <- read_csv(ruta_csv, show_col_types = FALSE)

# ───────────────────────────────────────────────
# 2. Función para generar mini tabla gt por delito
# ───────────────────────────────────────────────
generar_mini_tabla_delito <- function(nombre_delito, df) {
  mini_tabla <- df %>%
    filter(categoria_delito_modificada == nombre_delito) %>%
    select(
      categoria_delito_modificada,
      total_2019, total_2023, total_2024, total_2025,
      var_2025_vs_2019, var_2025_vs_2023, var_2025_vs_2024
    ) %>%
    gt() %>%
    cols_label(.list = etiquetas_finales) %>%
    fmt_number(
      columns = starts_with("total_"),
      decimals = 0,
      sep_mark = ","
    ) %>%
    ##  Estilo encabezados
    tab_style(
      style = list(
        cell_fill(color = "#9d2041ff"),
        cell_text(color = "white", weight = "bold", size = px(24))
      ),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = categoria_delito_modificada == nombre_delito)
    ) %>%
    tab_style(
      style = list(cell_fill(color = "#E7FBF1"), cell_text(color = "#2A6F4D")),
      locations = list(
        cells_body(columns = var_2025_vs_2019, rows = str_detect(var_2025_vs_2019, "-")),
        cells_body(columns = var_2025_vs_2023, rows = str_detect(var_2025_vs_2023, "-")),
        cells_body(columns = var_2025_vs_2024, rows = str_detect(var_2025_vs_2024, "-"))
      )
    ) %>%
    tab_style(
      style = list(cell_fill(color = "#FCDADE"), cell_text(color = "#940B1C")),
      locations = list(
        cells_body(columns = var_2025_vs_2019, rows = !str_detect(var_2025_vs_2019, "-") & var_2025_vs_2019 != "0%"),
        cells_body(columns = var_2025_vs_2023, rows = !str_detect(var_2025_vs_2023, "-") & var_2025_vs_2023 != "0%"),
        cells_body(columns = var_2025_vs_2024, rows = !str_detect(var_2025_vs_2024, "-") & var_2025_vs_2024 != "0%")
      )
    ) %>%
    tab_style(
      style = list(cell_fill(color = "#DDDDDD"), cell_text(color = "#252627")),
      locations = list(
        cells_body(columns = var_2025_vs_2019, rows = var_2025_vs_2019 == "0%"),
        cells_body(columns = var_2025_vs_2023, rows = var_2025_vs_2023 == "0%"),
        cells_body(columns = var_2025_vs_2024, rows = var_2025_vs_2024 == "0%")
      )
    )  %>%
    tab_style(
      style = list(cell_fill(color="#f2f2f2ff"), cell_text(color = "#58595A")), #color gris para el cuerpo de la tabla
      locations = cells_body(
        columns = c(categoria_delito_modificada,total_2019, total_2023, total_2024, total_2025)
      )
    ) %>%
    tab_options(
      table.font.names = "roboto", 
      table.width = "100%",
      table.font.size = px(24),
      data_row.padding = px(4)
    )
  
  # Nombre limpio para el archivo
  abreviaciones <- c(
    "Homicidio doloso" = "HD",
    "Homicidio doloso (víctimas)" = "HDV",
    "Lesiones dolosas por disparo de arma de fuego" = "LD",
    "Robo de vehículo con y sin violencia" = "RV",
    "Alto Impacto" = "AI"
    # agrega más si se necesitan
  )
  
  nombre_archivo <- if (!is.na(abreviaciones[nombre_delito])) {
    abreviaciones[nombre_delito]
  } else {
    str_replace_all(str_to_lower(nombre_delito), "[^a-z0-9]+", "_")
  }
  
  # Guardar como PNG
  gtsave(
    data = mini_tabla,
    filename = here("salidas", glue("TblMes_{nombre_archivo}.png")),
    vwidth = 1150,
    vheight = 200
  )
  
  # Extraer variación y generar tarjeta
  variacion19 <- df %>%
    filter(categoria_delito_modificada == nombre_delito) %>%
    pull(var_2025_vs_2019)
 
  
  if (length(variacion19) > 0 && !is.na(variacion19)) {
   
    generar_tarjeta_variacion(
      porcentaje = variacion19,
      anio_base = 2019,
      mes_corte = fecha_corte,
      nombre_delito = nombre_delito,
      tipo_periodo = "mensual",
      tipo_tarjeta = "T"
    )
  }
  
  if (length(variacion19) > 0 && !is.na(variacion19)) {
    generar_tarjeta_variacion(
      porcentaje = variacion19,
      anio_base = 2019,
      mes_corte = fecha_corte,
      nombre_delito = nombre_delito,
      tipo_periodo = "mensual",
      tipo_tarjeta = "G"
    )
  }
  
  
  variacion24 <- df %>%
    filter(categoria_delito_modificada == nombre_delito) %>%
    pull(var_2025_vs_2024)
  if (length(variacion24) > 0 && !is.na(variacion24)) {
    
    generar_tarjeta_variacion(
      porcentaje = variacion24,
      anio_base = 2024,
      mes_corte = fecha_corte,
      nombre_delito = nombre_delito,
      tipo_periodo = "mensual",
      tipo_tarjeta = "G"
    )
  }
  
  interpretar_titulo <- function(valor) {
    if (str_detect(valor, "-")) {
      return("Continúa la disminución")
    } else if (valor == "0.0%" || valor == "0%") {
      return("Sin cambio mensual")
    } else if (!is.na(valor) && valor != "") {
      return("Incremento mensual")
    } else {
      return("Evolución reciente")
    }
  }
  
  asignar_flecha <- function(valor) {
    if (str_detect(valor, "-")) {
      return("../auxiliares/verde.png")
    } else if (valor == "0.0%" || valor == "0%") {
      return("../auxiliares/gris.png")
    } else if (!is.na(valor) && valor != "") {
      return("../auxiliares/rojo.png")
    } else {
      return(NA_character_)
    }
  }
  
  
  # Construir nombre del archivo de la tarjeta mensual
  archivo_tarjeta <- glue("VarMes_2019T_{nombre_archivo}.png")
  
  # Calcular título y flecha
  titulo_mensual <- interpretar_titulo(variacion24)
  flecha_mensual <- asignar_flecha(variacion24)
  
  # Retornar como tibble
  return(tibble(
    archivo = nombre_archivo,
    titulo_mensual = titulo_mensual,
    flecha_mensual = flecha_mensual
  ))
  
  
}

# ───────────────────────────────────────────────
#  3. Ejecutar para todos los delitos
# ───────────────────────────────────────────────
delitos_deseados <- c(
  "Alto Impacto",
  "Homicidio doloso",
  "Homicidio doloso (víctimas)",
  "Lesiones dolosas por disparo de arma de fuego",
  "Robo de vehículo con y sin violencia"
  # Agrega más según lo necesites
)

titulos_mensuales <- map_dfr(delitos_deseados,generar_mini_tabla_delito,df = tabla_formateada_sin_visibles)
titulos_actualizados <- titulos_existentes %>%
  left_join(titulos_mensuales, by = "archivo")

write_csv(titulos_actualizados, ruta_csv)

