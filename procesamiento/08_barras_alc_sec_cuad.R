pacman::p_load(RPostgreSQL, showtext, sysfonts, 
               here, stringr, highcharter, 
               htmlwidgets, readr,webshot2)

here() 
font_add_google(c("Bebas Neue","Poppins", "Montserrat","Overpass","Roboto", "Lexend", "Manrope","Cabin"))
showtext_auto()

source( here('auxiliares', "funciones.R"))

meses_espaniol <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
                    "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")


dias_a_restar <- day(today()) %>% as.numeric()
fecha_corte_actual <- today() - dias_a_restar

fecha_corte <- today() - dias_a_restar

# Obtener número del mes (1-12)
num_mes_actual <- month(fecha_corte_actual)
num_mes_anterior <- ifelse(num_mes_actual == 1, 12, num_mes_actual - 1)

# Obtener nombre del mes en español
mes_actual_espaniol <- str_to_title(meses_espaniol[num_mes_actual])
mes_anterior_espaniol <- str_to_title(meses_espaniol[num_mes_anterior])

dias_a_restar <- day(today()) %>% as.numeric()
fecha_corte <- today()-dias_a_restar
fecha_inicio <- fecha_corte-60

#----- Obtener datos -----

# Leer config.yml y conectar
config_file <- here("auxiliares", "config.yml")
config <- yaml::read_yaml(config_file)

conn <- dbConnect(
  "PostgreSQL", 
  dbname = config$db_name, 
  host = config$db_host,
  port = config$db_port, 
  user = config$db_username, 
  password = config$db_password
)

Sys.setlocale("LC_ALL", "es_ES.UTF-8")

# 1. Consultar los datos
semanal <- dbGetQuery(
  conn, paste0(
    "SELECT alcaldia_hecho, date(fecha_inicio) fecha_inicio, categoria_delito
            
    FROM dashboard_seguridad.carpetas_directas_cc_cdmx
    WHERE 
      DATE(fecha_inicio) between '",fecha_inicio,"' and '",fecha_corte,"'  and categoria_delito != 'Delito de bajo impacto'"
  )
) %>% as_tibble()


semanal_cuadrante <- dbGetQuery(
  conn, paste0(
    "SELECT alcaldia_hecho, nombre_sec, \"Nomenclatu\", date(fecha_inicio) fecha_inicio, categoria_delito, colonia_hecho
            
    FROM dashboard_seguridad.carpetas_directas_cc_cdmx
    WHERE 
      DATE(fecha_inicio) between '",fecha_inicio,"' and '",fecha_corte,"'  and categoria_delito != 'Delito de bajo impacto'"
  )
) %>% as_tibble() %>% 
  mutate(nombre_sec = if_else(nombre_sec == "" | is.na(nombre_sec), "Sin Sector", nombre_sec)) %>% 
  mutate(alcaldia_hecho = if_else(alcaldia_hecho == "" | is.na(alcaldia_hecho), "Sin Alcaldía", alcaldia_hecho))

semanal_sector <- dbGetQuery(
  conn, paste0(
    "SELECT alcaldia_hecho, nombre_sec,  date(fecha_inicio) fecha_inicio, categoria_delito
            
    FROM dashboard_seguridad.carpetas_directas_cc_cdmx
    WHERE 
      DATE(fecha_inicio) >= '2024-12-01' and DATE(fecha_inicio) <=  '",fecha_corte,"' and categoria_delito != 'Delito de bajo impacto'"
  )
) %>% as_tibble() %>% 
  mutate(nombre_sec = if_else(nombre_sec == "" | is.na(nombre_sec), "Sin Sector", nombre_sec)) %>% 
  mutate(alcaldia_hecho = if_else(alcaldia_hecho == "" | is.na(alcaldia_hecho), "Sin Alcaldía", alcaldia_hecho))


# Calcular el mes actual y el mes pasado (respetando meses cruzando años)
semana_actual <- as.Date(cut(Sys.Date()-dias_a_restar, "month"))
semana_pasada <- as.Date(cut(Sys.Date()-dias_a_restar, "month")) %m-% months(1)

# Filtrar los datos para los meses actual y pasado
semanal_filtrado <- semanal %>% 
  filter(fecha_inicio >= semana_pasada & fecha_inicio < semana_actual %m+% months(1)) %>%
  mutate(semana = ifelse(fecha_inicio < semana_actual, "Mayo", "Junio"))

# Generar tabla por alcaldía, categoría y mes
tabla_alcaldia <- semanal_filtrado %>%
  group_by(alcaldia_hecho, categoria_delito, semana) %>%
  summarise(cantidad = n(), .groups = "drop") %>%
  mutate(alcaldia_hecho = ifelse(is.na(alcaldia_hecho), "Sin Alcaldía", alcaldia_hecho)) %>%
  arrange(desc(cantidad))

tabla_alcaldia

totales_alcaldia <- tabla_alcaldia %>%
  group_by(alcaldia_hecho, semana) %>%
  summarise(total = sum(cantidad, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  ungroup()


categorias_ordenadas <- totales_alcaldia %>%
  pull(alcaldia_hecho) %>%
  unique()

data_series <- totales_alcaldia %>%
  split(.$semana) %>%
  lapply(function(df) {
    list(
      name = as.character(df$semana[1]),
      data = lapply(1:nrow(df), function(i) {
        list(
          name = df$alcaldia_hecho[i],
          y = df$total[i],
          drilldown = paste(df$alcaldia_hecho[i], df$semana[i], sep = "-")
        )
      }),
      color = ifelse(df$semana[1] == paste0(mes_anterior_espaniol), "#bc955c", "#9f2241")
    )
  })

drilldown_series <- tabla_alcaldia %>%
  group_by(alcaldia_hecho, semana, categoria_delito) %>%
  summarise(cantidad = sum(cantidad, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(cantidad)) %>%
  group_split(alcaldia_hecho, semana)

drilldown_series <- lapply(drilldown_series, function(df) {
  alcaldia_semana <- unique(paste(df$alcaldia_hecho, df$semana, sep = "-"))
  list(
    id = alcaldia_semana,
    name = paste("Delitos en", alcaldia_semana),
    data = lapply(1:nrow(df), function(i) {
      list(
        name = df$categoria_delito[i],
        y = df$cantidad[i]
      )
    })
  )
})

grafico <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = paste0("Delitos por Alcaldía (",mes_anterior_espaniol," y ",mes_actual_espaniol,")"), style = list(fontSize = "40px")) %>%
  hc_xAxis(type = "category",labels = list(style = list(fontSize = "16px"))) %>%
  hc_yAxis(
    title = list(text = "Total de delitos", style = list(fontSize = "20px")),
    allowDecimals = FALSE,
    labels = list(style = list(fontSize = "24px"))
  ) %>%
  hc_plotOptions(column = list(
    grouping = TRUE,
    dataLabels = list(enabled = TRUE,style = list(fontSize = "21px", color = "black"))
  )) %>%
  hc_tooltip(
    pointFormat = "<b>{point.name}</b>: {point.y}",
    style = list(fontSize = "25px")
  ) %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = drilldown_series
  )

for (serie in data_series) {
  grafico <- grafico %>% hc_add_series(
    name = serie$name,
    data = serie$data,
    color = serie$color
  )
}

grafico <- grafico %>%
  hc_legend(
    itemStyle = list(
      fontSize = "25px"  # Cambia el tamaño aquí
    )
  )


saveWidget(grafico, 
           file = here('salidas/grafico_alcaldias.html') , 
           selfcontained = TRUE)

# Capturar el HTML como PNG
webshot2::webshot(
  url = here('salidas/grafico_alcaldias.html'),
  file = here('salidas/grafico_alcaldias.png'),
  vwidth = 2500,
  vheight = 1000
)

#############################
#####             APARTIR DE AQUI TRABAJAR FUNCION 
#############################


#-------------------- INVOCA FUNCION TABLA DE SECTOR ----------------------------------------
fecha_inicio2 <- Sys.Date()-dias_a_restar
semana_actual <- as.Date(cut(Sys.Date()-dias_a_restar, "month"))

# Filtrar los datos para el mes actual
semanal_sector_actual <- semanal_sector %>%
  filter(fecha_inicio >= semana_actual & fecha_inicio < semana_actual %m+% months(1))


sectores <- read_csv( here('auxiliares', 'top_sectores-top_sectores.csv') )

prueba_1_1 <- semanal_sector_actual %>%
  left_join(sectores , by = c("nombre_sec" = "SECTORES"))

prueba1 <- prueba_1_1 %>%
  mutate(comparacion_alcaldias = ifelse(alcaldia_hecho == Alcaldia_Camello, TRUE, FALSE))

prueba2 <- prueba1 %>% 
  mutate(alcaldia_modificada = ifelse(is.na(nombre_sec) & Alcaldia_Camello == "Na", alcaldia_hecho, Alcaldia_Camello)) %>% 
  dplyr::select(nombre_sec, categoria_delito, alcaldia_hecho = alcaldia_modificada)


# Crear la tabla de sectores para la semana actual
tabla_sec <- treemap_tabla_sector(prueba2)
tabla_sec

# Verificar duplicados antes de agrupar
tabla_sec <- tabla_sec %>%
  distinct()  # Elimina filas duplicadas

# Crear totales por sector, incluyendo alcaldía y delitos
totales_sector <- tabla_sec %>%
  group_by(nombre_sec, alcaldia_hecho) %>%
  summarise(
    total = sum(cantidad, na.rm = TRUE), # Suma de la cantidad (valores únicos)
    delitos = paste0("<b>", unique(categoria_delito), ":</b> ", cantidad, collapse = "<br>") # Concatenar sin duplicar
  ) %>%
  arrange(desc(total)) %>%
  ungroup()

# Validar los totales calculados
print(sum(totales_sector$total)) # Esto debería coincidir con la suma manual de `cantidad` en `semanal_sector_actual`


# Crear gráfico con totales corregidos
grafico <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Delitos por Sectores (Mes Actual)", style = list(fontSize = "27px")) %>%
  hc_xAxis(type = "category",labels = list(style = list(fontSize = "16px"))) %>%
  hc_yAxis(title = list(text = "Total de delitos", style = list(fontSize = "17px")), labels = list(style = list(fontSize = "24px"))) %>%
  hc_tooltip(
    useHTML = TRUE,
    style = list(fontSize = "17px"),
    pointFormat = "<b>Alcaldía:</b> {point.alcaldia}<br>
                   <b>Sector:</b> {point.name}<br>
                   <b>Delitos:</b><br>{point.customTooltip}",
    headerFormat = "<span style='font-size:14px'><b>{point.key}</b></span><br>"
  ) %>%
  hc_plotOptions(series = list(
    borderWidth = 0,
    dataLabels = list(enabled = TRUE,style = list(fontSize = "21px", color = "black"))
  )) %>%
  hc_add_series(
    name = "Sector",
    data = lapply(1:nrow(totales_sector), function(i) {
      list(
        name = totales_sector$nombre_sec[i],
        y = totales_sector$total[i],
        alcaldia = totales_sector$alcaldia_hecho[i],
        customTooltip = totales_sector$delitos[i]
      )
    }),
    color = "#B02858"
  )

print(grafico)

# Guardar el gráfico
saveWidget(grafico, 
           file = here('salidas/grafico_sectores.html') , 
           selfcontained = TRUE)

# Capturar el HTML como PNG
webshot2::webshot(
  url = here('salidas/grafico_sectores.html'),
  file = here('salidas/grafico_sectores.png'),
  vwidth = 1500,
  vheight = 700
)

#-------------------- INVOCA FUNCION TABLA DE CUADRANTE ----------------------------------------

semana_pasada <- month(Sys.Date()-dias_a_restar) - 2
semana_anterior <- month(Sys.Date()-dias_a_restar)-1
semana_actual <- month(Sys.Date()-dias_a_restar) %>% as.numeric()


semanal_cuadrante2 <- tabla_semanal_cuadrante(semanal_cuadrante, semana_actual, semana_anterior) 

tabla_cuadrante <- semanal_cuadrante2 %>%
  mutate(nueva_columna = paste(Nomenclatu, colonia_hecho, sep = ", "))

# Filtrar para excluir la categoría "DELITOS"
tabla_cuadrante <- tabla_cuadrante %>%
  filter(categoria_delito != "DELITOS")  # Excluir filas con "DELITOS"

# Agrupar y resumir los datos corregidos
grafico_data <- tabla_cuadrante %>%
  group_by(nueva_columna, alcaldia_hecho, nombre_sec) %>%
  summarise(
    total_pasado = sum(total_delitos_anterior, na.rm = TRUE),
    total_actual = sum(total_delitos_actual, na.rm = TRUE),
    delitos_pasado = paste0(
      "<b>", unique(categoria_delito), ":</b> ", 
      total_delitos_anterior[match(unique(categoria_delito), categoria_delito)], 
      collapse = "<br>"
    ),
    delitos_actual = paste0(
      "<b>", unique(categoria_delito), ":</b> ", 
      total_delitos_actual[match(unique(categoria_delito), categoria_delito)], 
      collapse = "<br>"
    )
  ) %>%
  ungroup() %>%
  arrange(desc(total_actual))  # Ordenar de mayor a menor según el total_actual

grafico <- highchart() %>%
  hc_chart(
    type = "column",
    zoomType = "x"  # Habilitar zoom en el eje X
  ) %>%
  hc_title(text = "Total de Delitos por Cuadrante", style = list(fontSize = "27px")) %>%
  hc_xAxis(
    categories = grafico_data$nueva_columna,  # Categorías (cuadrantes)
    title = list(text = ""),labels = list(style = list(fontSize = ""))
  ) %>%
  hc_yAxis(
    title = list(text = "Total de Delitos", style = list(fontSize = "17px")),labels = list(style = list(fontSize = "24px"))
  ) %>%
  hc_tooltip(
    useHTML = TRUE,
    pointFormat = "<b>Alcaldía:</b> {point.alcaldia}<br>
                           <b>Sector:</b> {point.sector}<br>
                           <b>Delitos:</b><br>{point.customTooltip}",
    headerFormat = "<span style='font-size:14px'><b>{point.key}</b></span><br>",
    style = list(fontSize = "17px")
  ) %>%
  hc_add_series(
    name = "Periodo Pasado",
    data = lapply(1:nrow(grafico_data), function(i) {
      list(
        name = grafico_data$nueva_columna[i],  # Nombre del cuadrante
        y = grafico_data$total_pasado[i],      # Total de delitos en el periodo pasado
        alcaldia = grafico_data$alcaldia_hecho[i],  # Alcaldía
        sector = grafico_data$nombre_sec[i],      # Sector
        customTooltip = grafico_data$delitos_pasado[i]  # Tooltip del Periodo Pasado
      )
    }),
    color = "#bc955c"  # Color para Periodo Pasado
  ) %>%
  hc_add_series(
    name = "Periodo Actual",
    data = lapply(1:nrow(grafico_data), function(i) {
      list(
        name = grafico_data$nueva_columna[i],  # Nombre del cuadrante
        y = grafico_data$total_actual[i],       # Total de delitos en el periodo actual
        alcaldia = grafico_data$alcaldia_hecho[i],   # Alcaldía
        sector = grafico_data$nombre_sec[i],       # Sector
        customTooltip = grafico_data$delitos_actual[i]  # Tooltip del Periodo Actual
      )
    }),
    color = "#9f2241"  # Color para Periodo Actual
  ) %>%
  hc_plotOptions(
    column = list(
      dataLabels = list(enabled = TRUE,style = list(fontSize = "19px", color = "black"))  # Mostrar etiquetas con valores
    )
  )
grafico


# Guardar el gráfico
saveWidget(grafico, 
           file = here('salidas/grafico_cuadrantes.html') , 
           selfcontained = TRUE)

# Capturar el HTML como PNG
webshot2::webshot(
  url = here('salidas/grafico_cuadrantes.html'),
  file = here('salidas/grafico_cuadrantes.png'),
  vwidth = 1500,
  vheight = 700
)

