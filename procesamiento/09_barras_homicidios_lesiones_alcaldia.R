pacman::p_load(RPostgreSQL, showtext, sysfonts, here, stringr, highcharter, htmlwidgets, readr,webshot2)

here() 

font_add_google(c("Bebas Neue","Poppins", "Montserrat","Overpass","Roboto", "Lexend", "Manrope","Cabin"))
showtext_auto()

source( here('auxiliares', "funciones.R") )


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


# 1. Consultar los datos
semanal <- dbGetQuery(
  conn, paste0(
    "SELECT
            date(fecha_inicio) fecha_inicio, alcaldia_hecho, nombre_sec, colonia_hecho, \"Nomenclatu\", categoria_delito
    FROM dashboard_seguridad.carpetas_directas_cc_cdmx
    WHERE 
      DATE(fecha_inicio) >= '2025-01-01' and DATE(fecha_inicio) <= '",fecha_corte_actual,"'"
  )
) %>% as_tibble()




# 3. Reemplazar valores nulos, vacíos o NA en 'alcaldia_hecho' por "Sin Alcaldía"
semanal$alcaldia_hecho[semanal$alcaldia_hecho %in% c(NA, "", "NA", "nulo")] <- "Sin Alcaldía"


fecha_actual <- Sys.Date() - dias_a_restar
semana_actual <- month(fecha_actual)  # Mes actual
semana_anterior <- month(fecha_actual %m-% months(1))  # Mes anterior
semana_pasada <- month(fecha_actual %m-% months(2))  # Dos meses antes


# 5. Crear un dataframe con los tres periodos semanales
df_semanal <- semanal %>% 
  mutate(
    numero_semana = month(fecha_inicio),
    periodo = case_when(
      numero_semana == semana_anterior ~ "Anterior",
      numero_semana == semana_actual ~ "Actual"
    )
  ) %>% 
  filter(!is.na(periodo)) 


agrupado <- df_semanal %>% 
      filter(categoria_delito == "Homicidio doloso" | categoria_delito == 'Lesiones dolosas por disparo de arma de fuego') %>% 
      procesar_datos(agrupaciones = c("alcaldia_hecho", "colonia_hecho", "nombre_sec", "Nomenclatu", "categoria_delito", "periodo"), 
                     periodo = c("categoria_delito")) %>% 
      pivotear_por_delito(periodo = "periodo", columnas = c("Homicidio doloso", "Lesiones dolosas por disparo de arma de fuego"))



#Genera Grafica particionada
datos <- tabla_desglosada_final(agrupado)

datos <- datos %>% slice(-1)

datos <- datos %>%
      filter(Nomenclatu != "TOTALES")

datos <- datos[, -5]
datos <- datos[, -7]
datos <- datos %>%
      group_by(Nomenclatu) %>%
      mutate(Nomenclatu = ifelse(Nomenclatu == "SIN CUADRANTE (SIN COLONIA, SIN SECTOR)",
                                 paste0(Nomenclatu, "_", row_number()),
                                 Nomenclatu)) %>%
      ungroup()

# Transformar los datos para incluir ambos periodos en el primer nivel
filtered_data <- datos %>%
      mutate(
            Homicidios2023 = `Homicidio doloso_Anterior`,
            Homicidios2024 = `Homicidio doloso_Actual`,
            Lesiones2023 = `Lesiones dolosas por disparo de arma de fuego_Anterior`,
            Lesiones2024 = `Lesiones dolosas por disparo de arma de fuego_Actual`
      )
# Nivel 1: Generar datos para las barras de 2023 y 2024, ordenados por el valor total
nivel1 <- filtered_data %>%
      group_by(alcaldia_hecho) %>%
      summarise(
            total_delitos_2023 = sum(Homicidios2023 + Lesiones2023, na.rm = TRUE),
            total_delitos_2024 = sum(Homicidios2024 + Lesiones2024, na.rm = TRUE),
            Homicidios2023 = sum(Homicidios2023, na.rm = TRUE),
            Homicidios2024 = sum(Homicidios2024, na.rm = TRUE),
            Lesiones2023 = sum(Lesiones2023, na.rm = TRUE),
            Lesiones2024 = sum(Lesiones2024, na.rm = TRUE),
            .groups = "drop"
      ) %>%
      arrange(desc(total_delitos_2023 + total_delitos_2024)) %>%
      mutate(
            name = alcaldia_hecho,
            drilldown_2023 = paste0(alcaldia_hecho, "_2023"),
            drilldown_2024 = paste0(alcaldia_hecho, "_2024"),
            tooltip = paste0(
                  "<b>Alcaldía:</b> ", alcaldia_hecho, "<br>",
                  "<b>Homicidios mes actual:</b> ", Homicidios2024, "<br>",
                  "<b>Homicidios mes pasado:</b> ", Homicidios2023, "<br>",
                  "<b>Lesiones mes actual:</b> ", Lesiones2024, "<br>",
                  "<b>Lesiones mes pasado:</b> ", Lesiones2023
                  
            )
      )

# Nivel 2: Detalles por cuadrante, ordenados de mayor a menor dentro de cada alcaldía
nivel2 <- filtered_data %>%
      group_by(alcaldia_hecho, Nomenclatu) %>%
      summarise(
            total_delitos_2023 = sum(Homicidios2023 + Lesiones2023, na.rm = TRUE),
            total_delitos_2024 = sum(Homicidios2024 + Lesiones2024, na.rm = TRUE),
            Homicidios2023 = sum(Homicidios2023, na.rm = TRUE),
            Homicidios2024 = sum(Homicidios2024, na.rm = TRUE),
            Lesiones2023 = sum(Lesiones2023, na.rm = TRUE),
            Lesiones2024 = sum(Lesiones2024, na.rm = TRUE),
            .groups = "drop"
      ) %>%
      arrange(desc(total_delitos_2023 + total_delitos_2024))

# Crear los drilldowns para 2023 y 2024
drilldown_nivel2 <- list()

for (periodo in c("2023", "2024")) {
      drilldown_nivel2[[periodo]] <- nivel2 %>%
            group_by(alcaldia_hecho) %>%
            group_split() %>%
            lapply(function(data) {
                  data <- data %>% arrange(desc(data[[paste0("total_delitos_", periodo)]]))
                  list(
                        id = paste0(unique(data$alcaldia_hecho), "_", periodo),
                        data = lapply(1:nrow(data), function(i) {
                              list(
                                    name = data$Nomenclatu[i],
                                    y = data[[paste0("total_delitos_", periodo)]][i],
                                    customTooltip = paste0(
                                          "<b>", data$Nomenclatu[i], "</b><br>",
                                          "Homicidios mes actual: ", data$Homicidios2024[i], "<br>",
                                          "Homicidios mes pasado: ", data$Homicidios2023[i], "<br>",
                                          "Lesiones mes actual: ", data$Lesiones2024[i], "<br>",
                                          "Lesiones mes pasado: ", data$Lesiones2023[i]
                                          
                                    )
                              )
                        })
                  )
            })
}

# Crear el gráfico con colores específicos para cada serie
grafico <- highchart() %>%
      hc_chart(type = "column", zoomType = "xy") %>%
      hc_title(text = "Total de homicidios y lesiones dolosas por arma de fuego por alcaldía", style = list(fontSize = "35px")) %>%
      hc_xAxis(type = "category",labels = list(style = list(fontSize = "16px"))) %>%
      hc_yAxis(labels = list(style = list(fontSize = "24px")))%>% 
      hc_tooltip(
            useHTML = TRUE,
            style = list(fontSize = "17px"),
            formatter = JS(
                  "function() {
        if (this.point.customTooltip) {
          return this.point.customTooltip;
        } else {
          return '<b>' + this.point.name + '</b><br>Total: ' + this.y;
        }
      }"
            )
      ) %>%
      hc_plotOptions(
            series = list(
                  dataLabels = list(enabled = TRUE,style = list(fontSize = "21px", color = "black")),
                  keys = c("name", "y", "customTooltip", "drilldown")
            )
      ) %>%
      hc_add_series(
            name = "Total delitos mes pasado",
            color = "#BC955C",
            data = nivel1 %>%
                  select(name, total_delitos_2023, drilldown_2023, tooltip) %>%
                  rename(y = total_delitos_2023, drilldown = drilldown_2023, customTooltip = tooltip) %>%
                  list_parse()
      ) %>%
      hc_add_series(
            name = "Total delitos mes actual",
            color = "#B02858",
            data = nivel1 %>%
                  select(name, total_delitos_2024, drilldown_2024, tooltip) %>%
                  rename(y = total_delitos_2024, drilldown = drilldown_2024, customTooltip = tooltip) %>%
                  list_parse()
      ) %>%
      hc_drilldown(
            series = c(drilldown_nivel2[["2023"]], drilldown_nivel2[["2024"]])
      )

# Guardar el gráfico como HTML

grafico <- grafico %>%
  hc_legend(
    itemStyle = list(
      fontSize = "25px"  # 👈 Ajusta aquí el tamaño
    )
  )

# Guardar el gráfico
saveWidget(grafico, 
           file = here('salidas/barras_homi_les_alc.html') , 
           selfcontained = TRUE)

# Capturar el HTML como PNG
webshot2::webshot(
  url = here('salidas/barras_homi_les_alc.html'),
  file = here('salidas/barras_homi_les_alc.png'),
  vwidth = 2500,
  vheight = 1000
)
