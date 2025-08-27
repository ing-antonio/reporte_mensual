library(dplyr)
library(tidyr)
#library(treemap)
library(lubridate)

# ───────────────────────────────────────────────
# Funciones que se usan en script 01, 02
# Se usan en el punto 6
# ───────────────────────────────────────────────
filtrar_mes <- function(df, fecha_col, anio, mes) {
  inicio <- as.Date(sprintf("%04d-%02d-01", anio, mes))
  fin <- inicio + months(1)
  df %>%
    filter({{fecha_col}} >= inicio, {{fecha_col}} < fin)
}

contar_por_delito <- function(df, etiqueta) {
  df %>%
    group_by(categoria_delito) %>%
    summarise(!!etiqueta := n(), .groups = "drop")
}

variaciones <- function(actual, previo) {
  ifelse(previo == 0, "100.0%", paste0(sprintf("%.1f", (actual - previo) / previo * 100), "%"))
}

variaciones0 <- function(actual, previo) {
  ifelse(previo == 0, "100%", paste0(round((actual - previo) / previo * 100), "%"))
}

victimas_mes <- function(anio, mes) {
  inicio <- as.Date(sprintf("%04d-%02d-01", anio, mes))
  fin <- inicio + months(1) 
  nrow(victimas %>% filter(fechainicio >= inicio, fechainicio < fin))
}

# ───────────────────────────────────────────────

# ───────────────────────────────────────────────
# Función para tarjetas informativas de mini tablas
# ───────────────────────────────────────────────
generar_tarjeta_variacion <- function(porcentaje, anio_base = 2019, mes_corte = Sys.Date(), nombre_delito = "general", tipo_periodo, tipo_tarjeta, tipo_tarjeta2 = 'G') {
  library(ggplot2)
  library(glue)
  library(showtext)
  library(stringr)
  
  

  font_add_google("Roboto", "roboto")
  showtext_auto()
  
  valor <- as.numeric(str_replace(porcentaje, "%", ""))
  tipo <- case_when(
    valor > 0 ~ "aumento",
    valor < 0 ~ "disminución",
    TRUE      ~ "sin cambio"
  )
  
  color <- case_when(
    tipo == "aumento"     ~ "#940B1C",  # rojo
    tipo == "disminución" ~ "#2A6F4D",  # verde
    TRUE                  ~ "#58595A"   # gris
  )
  
  fecha_corte <- floor_date(mes_corte, "month")
  fecha_fin <- fecha_corte - days(1)
  if (anio_base == 2019 & tipo_periodo == "acumulado" & tipo_tarjeta == "T") {
    
    periodo_texto <- glue("De enero a {format(fecha_fin, '%B %Y')}")
    texto_inferior <- glue("mismo periodo {anio_base}")
    
  } else if (anio_base == 2024 & tipo_periodo == "acumulado" & tipo_tarjeta == "T") {
    
    periodo_texto <- glue("")
    texto_inferior <- glue("mismo periodo {anio_base}")
    
  } else if(anio_base == 2019 & tipo_periodo == "acumulado" & tipo_tarjeta == "G") {
    
    periodo_texto <- glue("")
    texto_inferior <- glue("respecto a {anio_base}")
    
  } else if (anio_base == 2024 & tipo_periodo == "acumulado" & tipo_tarjeta == "G") {
    
    periodo_texto <- glue("")
    texto_inferior <- glue("respecto a {anio_base}")
    
  } else if(anio_base == 2019 & tipo_periodo == "mensual" & tipo_tarjeta == "T" ) {
    
    periodo_texto <- glue("{format(fecha_fin, '%B %Y')}")
    texto_inferior <- glue("respecto a {format(fecha_fin, '%B')} {anio_base}")
    
  } else if(anio_base == 2019 & tipo_periodo == "mensual" & tipo_tarjeta == "G" ) {
    
    periodo_texto <- glue("")
    texto_inferior <- glue("respecto a {anio_base}")
    
  } else if(anio_base == 2024 & tipo_periodo == "mensual" & tipo_tarjeta == "G") {
    
    periodo_texto <- glue("")
    texto_inferior <- glue("respecto a {anio_base}")
  } else {
    periodo_texto <- ""
    texto_inferior <- ""
  }
  
  
  texto_central  <- if (tipo == "sin cambio") "sin cambio" else glue("{tipo} de")
  valor_texto    <- glue("{abs(valor)}%")
  
  # Plot
  if (tipo_tarjeta == "T") {
    
    p <- ggplot() +
      # Texto superior (fuera del recuadro)
      
      annotate("text", x = 0.5, y = 0.7, label = periodo_texto,
               size = 10, fontface = "bold", family = "roboto", color = "#58595A") +
      
      # Recuadro amarillo ajustado
      annotate("rect", xmin = 0.2, xmax = 0.8, ymin = 0.25, ymax = 0.65,
               fill = "#fdf8e7", color = "#c8bda6", linewidth = 0.2) +
      
      # Texto: tipo de variación
      annotate("text", x = 0.5, y = 0.58, label = texto_central,
               size = 10, fontface = "bold", family = "roboto", color = color) +
      
      # Texto: número grande
      annotate("text", x = 0.5, y = 0.45, label = valor_texto,
               size = 30, fontface = "bold", family = "roboto", color = color) +
      
      # Texto: comparativo
      annotate("text", x = 0.5, y = 0.32, label = texto_inferior,
               size = 10, fontface = "bold", family = "roboto", color = "#58595A") +
      
      coord_fixed() +
      theme_void()
  } else {
    p <- ggplot() +
      # Texto superior (fuera del recuadro)
      
      annotate("text", x = 0.5, y = 0.7, label = periodo_texto,
               size = 10, fontface = "bold", family = "roboto", color = "#58595A") +
      
      # Recuadro amarillo ajustado
      annotate("rect", xmin = 0.2, xmax = 0.8, ymin = 0.25, ymax = 0.65,
               fill = "#f2f2f2", color = "#f2f2f2", linewidth = 0.2) +
      
      # Texto: tipo de variación
      annotate("text", x = 0.5, y = 0.58, label = texto_central,
               size = 10, fontface = "bold", family = "roboto", color = color) +
      
      # Texto: número grande
      annotate("text", x = 0.5, y = 0.45, label = valor_texto,
               size = 30, fontface = "bold", family = "roboto", color = color) +
      
      # Texto: comparativo
      annotate("text", x = 0.5, y = 0.32, label = texto_inferior,
               size = 10, fontface = "bold", family = "roboto", color = "#58595A") +
      
      coord_fixed() +
      theme_void()
  }
  
  
  
  # Nombre del archivo: depende si es mensual o acumulado
  abreviaciones <- c(
    "Homicidio doloso" = "HD",
    "Homicidio doloso (víctimas)" = "HDV",
    "Lesiones dolosas por disparo de arma de fuego" = "LD",
    "Robo de vehículo con y sin violencia" = "RV",
    "Alto Impacto" = "AI",
    "Robo a personas en el espacio público" = "RPEP"
    # agrega más si los necesitas
  )
  
  nombre_archivo <- if (!is.na(abreviaciones[nombre_delito])) {
    abreviaciones[nombre_delito]
  } else {
    str_replace_all(str_to_lower(nombre_delito), "[^a-z0-9]+", "_")
  }

  prefijo <- ifelse(tipo_periodo == "mensual", "VarMes", "VarAcum")
  
 
  
  if (tipo_tarjeta == "T"){
    ggsave(
      filename = here("salidas", glue("{prefijo}_{anio_base}{tipo_tarjeta}_{nombre_archivo}.png")),
      plot = p,
      width = 1.5,
      height = 1,
      dpi = 400,
      bg = "transparent"
    )
  } else {
    # Guardar
    ggsave(
      filename = here("salidas", glue("{prefijo}_{anio_base}{tipo_tarjeta}_{nombre_archivo}.png")),
      plot = p,
      width = 1.7,
      height = 1,
      dpi = 400,
      bg = "transparent"
    )
  }
  
  
}



# ───────────────────────────────────────────────




# Función para agrupar por las variables deseadas
agrupar_datos <- function(df, agrupaciones) {
  # Agrupar según las variables pasadas en 'agrupaciones'
  df_agrupado <- df %>%
    group_by(across(all_of(agrupaciones))) %>%
    summarise(cantidad = n(), .groups = 'drop') 
  
  return(df_agrupado)
}

# Función para pivotear los datos por periodo
pivotear_por_periodo <- function(df, periodo) {
  # Pivotear por la columna 'periodo'
  df_pivotado <- df %>%
    pivot_wider(names_from = !!sym(periodo), values_from = cantidad, values_fill = list(cantidad = 0)) 
  
  return(df_pivotado)
}

# Función principal que agrupa y luego pivotea
procesar_datos <- function(df, agrupaciones, periodo) {
  df_agrupado <- agrupar_datos(df, agrupaciones)
  df_final <- pivotear_por_periodo(df_agrupado, periodo)
  
  return(df_final)
}


# Función para redondear valores a 2 decimales
redondea <- function(valor) {
  valor <- round(valor, 2)
  return(valor)
}

# Función para calcular la variación porcentual
variacion <- function(val_1, val_2) {
  ifelse(
    is.na(val_1) | is.na(val_2), NA,
    ifelse(
      val_1 == 0 & val_2 == 0, 0,  # Ambos son 0
      ifelse(
        val_2 != 0 & val_1 != 0, redondea(-100 * (1 - (val_1 / val_2))),
        ifelse(
          val_2 == 0, 100,  # Si el valor anterior es 0
          ifelse(val_1 == 0, -100, 0)  # Si el valor actual es 0
        )
      )
    )
  )
}

porcentaje_color <- function(x) {
  # Asegúrate de que 'x' sea numérico
  x <- as.numeric(x)
  
  if (is.na(x)) {
    return('<span style="color:#939393; font-weight: bold;">0%</span>')  # Si el valor es NA, muestra 0%
  } else if (x > 0) {
    text <- paste0('<span style="color:#C00101; font-weight: bold;">', round(x, 1), '%</span>')
  } else if (x == 0) {
    text <- paste0('<span style="color:#939393; font-weight: bold;">', round(x, 1), '%</span>')
  } else if (x < 0) {
    text <- paste0('<span style="color:#329C00; font-weight: bold;">', round(x, 1), '%</span>')
  }
  return(text)
}




pivotear_por_delito <- function(df, periodo, columnas) {
  # Pivotear por la columna 'periodo' y varias columnas en 'values_from'
  df_pivotado <- df %>%
    pivot_wider(names_from = !!sym(periodo), 
                values_from = all_of(columnas), 
                values_fill = list(cantidad = 0)) 
  
  return(df_pivotado)
}


grafico_comparada <- function(df, periodo){
  
  
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
  
  
  
  #Sección para obtener los datos 
  ci_actual <- df %>% 
    filter( periodo == 'Actual')
  
  ci_anterior <- df %>% 
    filter( periodo == 'Anterior')
  
  g<- df %>%
    agrupar_datos(agrupaciones = c("alcaldia_hecho", "periodo")) %>% 
    mutate(alcaldia_hecho = ifelse(is.na(alcaldia_hecho), "Sin Alcaldía", alcaldia_hecho)) %>% 
    pivot_wider(names_from = periodo, values_from = cantidad) %>%
    ungroup() %>%
    mutate(Actual=ifelse(is.na(Actual),0,Actual)) %>%
    mutate(Actual=ifelse(is.na(Actual),0,Actual)) %>%
    mutate(
      Actual = ifelse(is.na(Actual), 0, Actual),
      Anterior = ifelse(is.na(Anterior), 0, Anterior) 
    ) %>%
    mutate(porc = variacion(Actual, Anterior)) %>% 
    mutate(porc=ifelse(Actual==0 & Anterior>0,-100,porc)) %>% 
    mutate(porc=ifelse(Actual>0 & Anterior==0,100,porc)) %>% 
    mutate(colores=ifelse(porc<0, "#B02858", "#000000")) %>% 
    pivot_longer(cols = c(Actual, Anterior), names_to ="periodo",  
                 values_to = "total") %>% 
    mutate(cond=ifelse(periodo=="Actual", "#9f2241", "#ac6d14")) %>% 
    mutate(alcaldia_hecho = ifelse(is.na(alcaldia_hecho) | alcaldia_hecho == "", "Sin Alcaldía", alcaldia_hecho)) %>% 
  dplyr::select(alcaldia_hechos = alcaldia_hecho, everything())
  
  
  con_alcaldia<- g %>% filter(alcaldia_hechos!="Sin Alcaldía")
  
  
  orden<- con_alcaldia %>% 
    filter(periodo=="Actual") %>% 
    group_by(alcaldia_hechos) %>% 
    arrange(total)
  orden <- orden$alcaldia_hechos
  orden<-c("Sin Alcaldía", orden)
  
  g<-g %>% mutate(alcaldia_hechos = factor(alcaldia_hechos, 
                                           levels = orden))
  
  
  
  M <- max(g$total)
  
  black_text<- -(M*.21)
  black_text2<- -(M*.16)
  
  point_size<-40
  text_size<-12
  
  if(M>=100){
    text_size<-10
  }
  if(periodo=="anual"){
    margen<-0
  } else{
    margen<-0.8
  }
  
  
  if(periodo=="mensual"){
    semana<-ceiling(yday(fecha_corte)/7)
    inicial_Actual<-paste0(mes_anterior_espaniol)
    final_Actual<-paste0(mes_actual_espaniol)
    texto_label<-paste0("Comparativo ",inicial_Actual," vs ",final_Actual," de 2025.")
    ajuste<- -1
  } else{
    semana<-ceiling(yday(fecha_corte)/7)
    inicial_Actual<-year(fecha_corte-years(1))
    final_Actual<-year(fecha_corte)
    texto_label<-paste0("Comparativo ",inicial_Actual," vs ",final_Actual,".")
    ajuste<- 0.5
  }
  
  G1 <- ggplot(g) +
    geom_bar(aes(x = alcaldia_hechos, y = total, fill = cond),
             stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +  # Reducir el ancho de las barras
    scale_fill_identity() +
    coord_flip() +
    # Etiqueta de delitos actuales
    geom_text(aes(alcaldia_hechos, black_text, label = scales::comma(total), group = cond, color = cond),
              position = position_dodge(width = 1),
              hjust = 0.5, vjust = 0.5,
              size = 15, fontface = "bold",
              family = "Cabin") +
    scale_color_identity() +  # Esto aplica los colores definidos en 'cond'
    geom_text(data = g %>% filter(periodo == "Anterior" & porc > 0),
              aes(alcaldia_hechos, black_text2, label = paste0("(", porc, "%)"), group = cond),
              position = position_dodge(width = 1),
              hjust = 0, vjust = 0.5,
              color = "#E5074C",
              size = 13, fontface = "bold",
              family = "Cabin") +
    geom_text(data = g %>% filter(periodo == "Anterior" & porc < 0),
              aes(alcaldia_hechos, black_text2, label = paste0("(", porc, "%)"), group = cond),
              position = position_dodge(width = 1),
              hjust = 0, vjust = 0.5,
              color = "#027A35",
              size = 13, fontface = "bold",
              family = "Cabin") +
    geom_text(data = g %>% filter(periodo == "Anterior" & porc == 0),
              aes(alcaldia_hechos, black_text2, label = paste0("(", porc, "%)"), group = cond),
              position = position_dodge(width = 1),
              hjust = 0, vjust = 0.5,
              color = "black",
              size = 13, fontface = "bold",
              family = "Cabin") 
  
  if(periodo == 'mensual'){
    
    G1 <- G1 +
      geom_richtext(
        aes(
          x = levels(g$alcaldia_hechos)[1],
          y = M * 0.9
        ),
        label = paste0(
          scales::comma(nrow(ci_actual)),
          "<span style='color:black;'> carpetas iniciadas </span>",
          "<span style='color:#888888;'>en el mes </span> ",mes_actual_espaniol," <br>",
          scales::comma(nrow(ci_anterior)), 
          "<span style='color:black;'> carpetas iniciadas </span>",
          "<span style='color:#888888;'>en el mes </span> ",mes_anterior_espaniol," <br>",
          "<span style='color:black;'>Variación del </span>",
          "<span style='color:",
          ifelse(
            round((((nrow(ci_actual) - nrow(ci_anterior)) / nrow(ci_anterior)) * 100), 2) < 0, 
            "#027A35", 
            ifelse(
              round((((nrow(ci_actual) - nrow(ci_anterior)) / nrow(ci_anterior)) * 100), 2) > 0, 
              "#E5074C", 
              "black"
            )
          ), 
          ";'>", 
          round((((nrow(ci_actual) - nrow(ci_anterior)) / nrow(ci_anterior)) * 100), 2), 
          "%</span>"
        ),
        hjust = 1,
        vjust = 0,
        size = 13.5,
        color = "black",
        family = "Cabin",
        fontface = "bold",
        label.color = NA, # Sin borde en el texto
        fill = NA         # Sin fondo en el texto
      ) +
      theme_minimal() +
      labs(title = texto_label,
           subtitle = "",
           y = "",
           x = "",
           caption = "") +
      theme(plot.title = element_text(hjust = 0.5,  # Centramos el título
                                      vjust = -0.5,
                                      size = 45,
                                      face = "bold"),
            panel.background = element_rect(fill = "transparent", color = NA), # Fondo transparente
            plot.background = element_rect(fill = "transparent", color = NA),   # Fondo transparente
            plot.subtitle = element_text(hjust = 0,
                                         size = 35,
                                         face = "italic"),
            plot.caption = element_text(hjust = 0, 
                                        size = 5),
            panel.border = element_blank(),
            legend.position = "none",
            text = element_text(family = "Cabin"),
            axis.text.y = element_text(face = "bold", color = "black", size = 28),
            axis.text.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            axis.line = element_line(),
            plot.margin = margin(t = 0,
                                 r = 0,
                                 b = 0,
                                 unit = "cm"))
  }else{
    
    
    G1 <- G1 +
      geom_richtext(
        aes(
          x = levels(g$alcaldia_hechos)[1],
          y = M * 0.9
        ),
        label = paste0(
          scales::comma(nrow(ci_actual)),
          "<span style='color:black;'> carpetas iniciadas </span>",
          "<span style='color:#888888;'>en el año </span>", anio_actual, "<br>",
          scales::comma(nrow(ci_anterior)), 
          "<span style='color:black;'> carpetas iniciadas </span>",
          "<span style='color:#888888;'>en el año </span>", anio_anterior, "<br>",
          "<span style='color:black;'>Variación del </span>",
          "<span style='color:",
          ifelse(
            round((((nrow(ci_actual) - nrow(ci_anterior)) / nrow(ci_anterior)) * 100), 2) < 0, 
            "#027A35", 
            ifelse(
              round((((nrow(ci_actual) - nrow(ci_anterior)) / nrow(ci_anterior)) * 100), 2) > 0, 
              "#E5074C", 
              "black"
            )
          ), 
          ";'>", 
          round((((nrow(ci_actual) - nrow(ci_anterior)) / nrow(ci_anterior)) * 100), 2), 
          "%</span>"
        ),
        hjust = 1,
        vjust = 0,
        size = 13.5,
        color = "black",
        family = "Cabin",
        fontface = "bold",
        label.color = NA, # Sin borde en el texto
        fill = NA         # Sin fondo en el texto
      ) +
      theme_minimal() +
      labs(title = texto_label,
           subtitle = "",
           y = "",
           x = "",
           caption = "") +
      theme(plot.title = element_text(hjust = 0.5,  # Centramos el título
                                      vjust = -0.5,
                                      size = 45,
                                      face = "bold"),
            panel.background = element_rect(fill = "transparent", color = NA), # Fondo transparente
            plot.background = element_rect(fill = "transparent", color = NA),   # Fondo transparente
            plot.subtitle = element_text(hjust = 0,
                                         size = 35,
                                         face = "italic"),
            plot.caption = element_text(hjust = 0, 
                                        size = 5),
            panel.border = element_blank(),
            legend.position = "none",
            text = element_text(family = "Cabin"),
            axis.text.y = element_text(face = "bold", color = "black", size = 28),
            axis.text.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            axis.line = element_line(),
            plot.margin = margin(t = 0,
                                 r = 0,
                                 b = 0,
                                 unit = "cm"))
    
    
  }
  
  
  
  return(G1)
}




#FUCNION  TABLA DEL SECTOR

treemap_tabla_sector <- function(df){
  
  datos_semanal <- agrupar_datos(df, c("nombre_sec","alcaldia_hecho" )) %>% 
    filter(nombre_sec != 'Sin Sector') %>% 
    arrange(desc(cantidad))
  
  # Obtener el valor del total del décimo lugar
  decimo_valor <- datos_semanal$cantidad[10]
  
  # Filtrar todos los registros que tengan un valor mayor o igual que el décimo lugar
  resultado_filtrado  <- datos_semanal %>%
    filter(cantidad >= decimo_valor)
  
  # Tomar solo los primeros 16 registros (top 10 + 6 adicionales)
  resultado_final <- resultado_filtrado %>% slice(1:16) %>% 
  dplyr::select(Sector = nombre_sec, alcaldia_hechos = alcaldia_hecho, total = cantidad, )
  
  tabla_gt_1 <- resultado_final %>%
    head(16) %>%  # Mostrar las primeras 16 filas
    gt() %>%
    cols_label(
      Sector = "Sector",
      alcaldia_hechos = "Alcaldía",
      total = "Total"
    ) %>%
    # Estilo para el cuerpo de la tabla
    tab_style(
      style = list(
        cell_text(color = "black", weight = "bold", size = px(50), align = "center", font = "Cabin"),  # Fuente Cabin
        cell_fill(color = "white")
      ),
      locations = cells_body(columns = everything())  # Aplica a todas las columnas
    ) %>%
    # Estilo para los encabezados de columna
    tab_style(
      style = list(
        cell_text(color = "white", weight = "bold", font = "Cabin", align = "center", size = px(60)),  # Fuente Cabin
        cell_fill(color = "#BC955C")
      ),
      locations = cells_column_labels(columns = everything())
    ) %>%
    cols_width(
      total ~ px(200)
    ) %>%
    # Cambiar el color de las líneas divisorias
    tab_options(
      table_body.hlines.style = "solid",
      table_body.hlines.width = px(1),
      table_body.hlines.color = "#BC955C",  # Color personalizado para las líneas horizontales
      table_body.vlines.style = "solid",
      table_body.vlines.width = px(1),
      table_body.vlines.color = "#BC955C",  # Color personalizado para las líneas verticales
      column_labels.border.top.style = "solid",
      column_labels.border.top.width = px(2),
      column_labels.border.top.color = "#BC955C",  # Color personalizado para el borde superior
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "#BC955C",  # Color personalizado para el borde inferior
      column_labels.vlines.style = "solid",
      column_labels.vlines.width = px(1),
      column_labels.vlines.color = "#BC955C",  # Color personalizado para las líneas verticales en encabezados
      data_row.padding = px(40)  # Ajusta el espaciado del cuerpo de la tabla
    )
  
  tabla_gt_1
  
}

# FUNCION TREEMAP DEL SECTOR

treemap_sector <- function(df){
  
  datos_semanal <- agrupar_datos(df, c("nombre_sec","alcaldia_hecho" )) %>% 
    filter(nombre_sec != 'Sin Sector') %>% 
    arrange(desc(cantidad))
  
  # Obtener el valor del total del décimo lugar
  decimo_valor <- datos_semanal$cantidad[10]
  
  # Filtrar todos los registros que tengan un valor mayor o igual que el décimo lugar
  resultado_filtrado  <- datos_semanal %>%
    filter(cantidad >= decimo_valor)
  
  
  # Tomar solo los primeros 16 registros (top 10 + 6 adicionales)
  resultado_final <- resultado_filtrado %>% 
    slice(1:16) %>% 
    mutate(
      total_global = sum(cantidad),  # Calculamos el total global basado en 'cantidad'
      porcentaje = round((cantidad / total_global) * 100, 1),  
      etiqueta = paste0(nombre_sec, " \n", cantidad)) %>%
    dplyr::select(Sector = nombre_sec, alcaldia_hechos = alcaldia_hecho, total = cantidad, porcentaje, etiqueta)
  
  
  valor_top3 <- resultado_final$total[3]
  
  # Filtrar registros hasta que el valor deje de coincidir con el tercer lugar
  resultado_top <- resultado_final %>%
    filter(total >= valor_top3)
  
  palette <- colorRampPalette(colors=c ("#9f2241", "#de7aaf"))
  colores <- palette(nrow(resultado_top))
  
  interseccion_aux11<-resultado_top %>% 
    ungroup() %>% 
    mutate(color=colores[1:nrow(resultado_top)])
  
  
  
  
  treemap <- ggplot(data = interseccion_aux11,
                    aes(area = total,
                        fill = color,
                        label = etiqueta)) +
    geom_treemap(colour = "white", start = "topleft") +
    geom_treemap_text(colour = "white",
                      place = "centre",
                      size = 5, # Reducir el tamaño de la letra aquí
                      family = "Cabin",
                      fontface = "bold",
                      grow = TRUE,
                      reflow = TRUE,
                      padding.x = grid::unit(10, "mm"), # Reducir el padding si es necesario
                      padding.y = grid::unit(10, "mm"),
                      angle = 0,
                      start = "topleft",
                      lineheight = 0.5) +
    scale_fill_manual(values = interseccion_aux11$color) +
    theme(text = element_text(family = "Cabin", 
                              size = 5, 
                              face = "bold"),  
          strip.background = element_blank(),
          legend.position = "none",        
          legend.title = element_blank(),
          panel.spacing = unit(5, "lines"))+
    theme(aspect.ratio = .8) 
  
  return(treemap)
  
}


# FUNCION Tabla POR CUADRANTE
tabla_semanal_cuadrante <- function(semanal_cuadrante, semana_actual, semana_anterior) {
  
  # Obtención de datos
  semanal_cuadrante <- semanal_cuadrante %>%
    mutate(anio = year(fecha_inicio),
           mes = month(fecha_inicio)) %>%
    filter(anio == 2024) %>%
    mutate(
      numero_semana = isoweek(fecha_inicio),
      periodo = case_when(
        numero_semana == semana_anterior ~ "Anterior",
        numero_semana == semana_actual ~ "Actual"
      )
    ) %>%
    filter(!is.na(periodo))
  
  # Top 4 cuadrantes con empates
  top_cuadrantes <- semanal_cuadrante %>%
    filter(periodo == 'Actual') %>%
    dplyr::select(Sector = nombre_sec, Cuadrante = Nomenclatu, alcaldia_hechos = alcaldia_hecho, Colonia = colonia_hecho) %>%
    agrupar_datos(c("Sector", "Cuadrante", "alcaldia_hechos", "Colonia")) %>%
    filter(Sector != 'Sin Sector') %>%
    arrange(desc(cantidad))
  
  top_4_dinamico <- top_cuadrantes %>%
    slice_max(cantidad, n = 4, with_ties = TRUE) %>% 
    mutate(key_cuadrante_colonia = paste0(Cuadrante, "_", Colonia))
  
  lista_top4 <- top_4_dinamico$key_cuadrante_colonia
  
  cuadrantes_completos <- tibble(key_cuadrante_colonia = lista_top4)
  
  # Datos del periodo anterior
  top_cuadrantes_anterior_filtrado <- semanal_cuadrante %>%
    filter(periodo == 'Anterior') %>%
    mutate(key_cuadrante_colonia = paste0(Nomenclatu, "_", colonia_hecho)) %>%
    agrupar_datos(c("nombre_sec", "Nomenclatu", "alcaldia_hecho", "colonia_hecho", "key_cuadrante_colonia")) %>%
    right_join(cuadrantes_completos, by = "key_cuadrante_colonia") %>%
    mutate(
      cantidad = replace_na(cantidad, 0),
      Sector = replace_na(nombre_sec, "Sin información"),
      alcaldia_hecho = replace_na(alcaldia_hecho, "Sin información"),
      Colonia = replace_na(colonia_hecho, "Sin información")
    ) %>%
    arrange(desc(cantidad)) %>%
    dplyr::select(Sector, Nomenclatu, alcaldia_hecho, Colonia, total = cantidad, key_cuadrante_colonia)
  
  # Unión de periodos actual y anterior
  unir_cuadrantes <- top_4_dinamico %>%
    mutate(key_cuadrante_colonia = paste0(Cuadrante, "_", Colonia)) %>%
    full_join(
      top_cuadrantes_anterior_filtrado %>% 
        select(key_cuadrante_colonia, total_anterior = total), 
      by = "key_cuadrante_colonia"
    ) %>%
    mutate(categoria_delito = 'DELITOS') %>%
    dplyr::select(Sector, Cuadrante, alcaldia_hechos, Colonia, categoria_delito, total_actual = cantidad, total_anterior)
  
  # Paso de desglose
  # Paso 4: Desglose por categorías
  desglose_actual <- semanal_cuadrante %>%
    filter(periodo == 'Actual') %>%
    mutate(key_cuadrante_colonia = paste0(Nomenclatu, "_", colonia_hecho)) %>%
    filter(key_cuadrante_colonia %in% lista_top4) %>%
    agrupar_datos(c("nombre_sec", "Nomenclatu", "alcaldia_hecho", "colonia_hecho", "categoria_delito"))
  
  desglose_anterior <- semanal_cuadrante %>%
    filter(periodo == 'Anterior') %>%
    mutate(key_cuadrante_colonia = paste0(Nomenclatu, "_", colonia_hecho)) %>%
    filter(key_cuadrante_colonia %in% lista_top4) %>%
    agrupar_datos(c("nombre_sec", "Nomenclatu", "alcaldia_hecho", "colonia_hecho", "categoria_delito"))
  
  # Combinar desgloses
  desglose_combinado <- full_join(
    desglose_actual, 
    desglose_anterior, 
    by = c("nombre_sec", "Nomenclatu", "alcaldia_hecho", "colonia_hecho", "categoria_delito")
  ) %>%
    replace_na(list(total_actual = 0, total_anterior = 0)) %>%
    dplyr::select(Sector = nombre_sec, Cuadrante = Nomenclatu, alcaldia_hechos = alcaldia_hecho, Colonia = colonia_hecho, categoria_delito,
           total_actual = cantidad.x, total_anterior = cantidad.y)
  
  # Agrupación final
  agrupacion_final_cuadrantes <- bind_rows(unir_cuadrantes, desglose_combinado) %>%
    group_by(Cuadrante, Colonia) %>%
    mutate(
      total_delitos = max(if_else(categoria_delito == "DELITOS", total_actual, NA_real_), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(
      desc(total_delitos), 
      Cuadrante, 
      if_else(categoria_delito == "DELITOS", 1, 2), 
      desc(total_actual)
    ) %>%
    mutate(total_anterior = ifelse(is.na(total_anterior),0,total_anterior),
           total_actual = ifelse(is.na(total_actual),0,total_actual)) %>% 
    
    mutate(
      variacion_actual = variacion(total_actual, total_anterior),
      cuadrante_colonia = paste0(Cuadrante, " (", Colonia, ")")
    ) %>%
    dplyr::select(
      cuadrante_colonia, 
      Sector, 
      alcaldia_hechos, 
      categoria_delito, 
      total_anterior, 
      total_actual, 
      variacion_actual
    )
  
  assign("agrupacion_final_cuadrantes", agrupacion_final_cuadrantes, envir = .GlobalEnv)
  
  # Crear tabla 
  tabla_gt_1 <- agrupacion_final_cuadrantes %>%
    # head(50) %>%
    gt() %>%
    cols_label(
      cuadrante_colonia = "CUADRANTE \n (COLONIA)",
      Sector = "SECTOR",
      alcaldia_hechos = "ALCALDÍA",
      categoria_delito = "DELITO",
      total_actual = paste("TOTAL SEMANA ",semana_actual),
      total_anterior = paste("TOTAL SEMANA ", semana_anterior),
      variacion_actual = paste0("VARIACIÓN ", semana_anterior , " vs ", semana_actual)
      
    ) %>%
    text_transform(
      locations = cells_body(columns = c("variacion_actual")),
      fn = function(x) {
        sapply(x, porcentaje_color)  # Aplica la función porcentaje_color
      }
    ) %>% 
    tab_style(
      style = list(
        cell_text(color = "black", weight = "bold", size = px(60), align = "center", font = "Cabin"),
        cell_fill(color = "white")
      ),
      locations = cells_body(columns = everything())
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "white", weight = "bold", font = "Cabin", align = "center", size = px(60)),
        cell_fill(color = "#BC955C")
      ),
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "#B02858", weight = "bold"),
        cell_fill(color = "#EAD1DC")
      ),
      locations = cells_body(
        rows = categoria_delito == "DELITOS" # Condición para resaltar filas con "Delito"
      )
    ) %>%
    cols_width(
      total_actual ~ px(350),
      total_anterior ~ px(350),
      Sector ~ px(350),
      variacion_actual ~ px(350)
      
    ) %>%
    tab_options(
      table_body.hlines.style = "solid",
      table_body.hlines.width = px(1),
      table_body.hlines.color = "#BC955C",
      table_body.vlines.style = "solid",
      table_body.vlines.width = px(1),
      table_body.vlines.color = "#BC955C",
      column_labels.border.top.style = "solid",
      column_labels.border.top.width = px(2),
      column_labels.border.top.color = "#BC955C",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "#BC955C",
      column_labels.vlines.style = "solid",
      column_labels.vlines.width = px(1),
      column_labels.vlines.color = "#BC955C",
      data_row.padding = px(40)
    )  
  return(agrupacion_final_cuadrantes)
}


tablas_partidas <- function(agrupacion_final_cuadrantes, semana_actual, semana_anterior) {
  # Definir número de filas por tabla
  filas_por_tabla <- 7
  
  # Dividir la tabla en subtables
  subtablas <- split(agrupacion_final_cuadrantes, ceiling(seq_along(1:nrow(agrupacion_final_cuadrantes)) / filas_por_tabla))
  
  # Establecer directorio para guardar imágenes (directamente en el código)
  setwd("G:/.shortcut-targets-by-id/1o3RW9XtzyaaBxBvSYFPTHgER_wJ6O_VR/Analisis/Gabinete de Seguridad/20241203_incidencia_delictiva_semanal/03_output/tabla_cuadrante")
  
  
  for (i in seq_along(subtablas)) {
    # Crear el objeto gt para la subtabla actual
    tabla_gt <- subtablas[[i]] %>% 
      gt() %>%
      cols_label(
        cuadrante_colonia = "CUADRANTE \n (COLONIA)",
        Sector = "SECTOR",
        alcaldia_hechos = "ALCALDÍA",
        categoria_delito = "DELITO",
        total_actual = paste("TOTAL SEMANA ",semana_actual),
        total_anterior = paste("TOTAL SEMANA ", semana_anterior),
        variacion_actual = paste0("VARIACIÓN ", semana_anterior , " vs ", semana_actual)
        
      ) %>%
      text_transform(
        locations = cells_body(columns = c("variacion_actual")),
        fn = function(x) {
          sapply(x, porcentaje_color)  # Aplica la función porcentaje_color
        }
      ) %>% 
      tab_style(
        style = list(
          cell_text(color = "black", weight = "bold", size = px(60), align = "center", font = "Cabin"),
          cell_fill(color = "white")
        ),
        locations = cells_body(columns = everything())
      ) %>%
      tab_style(
        style = list(
          cell_text(color = "white", weight = "bold", font = "Cabin", align = "center", size = px(60)),
          cell_fill(color = "#BC955C")
        ),
        locations = cells_column_labels(columns = everything())
      ) %>%
      tab_style(
        style = list(
          cell_text(color = "#B02858", weight = "bold"),
          cell_fill(color = "#EAD1DC")
        ),
        locations = cells_body(
          rows = categoria_delito == "DELITOS" # Condición para resaltar filas con "Delito"
        )
      ) %>%
      cols_width(
        total_actual ~ px(350),
        total_anterior ~ px(350),
        Sector ~ px(350),
        variacion_actual ~ px(350)
        
      ) %>%
      tab_options(
        table_body.hlines.style = "solid",
        table_body.hlines.width = px(1),
        table_body.hlines.color = "#BC955C",
        table_body.vlines.style = "solid",
        table_body.vlines.width = px(1),
        table_body.vlines.color = "#BC955C",
        column_labels.border.top.style = "solid",
        column_labels.border.top.width = px(2),
        column_labels.border.top.color = "#BC955C",
        column_labels.border.bottom.style = "solid",
        column_labels.border.bottom.width = px(2),
        column_labels.border.bottom.color = "#BC955C",
        column_labels.vlines.style = "solid",
        column_labels.vlines.width = px(1),
        column_labels.vlines.color = "#BC955C",
        data_row.padding = px(40)
      ) 
    
    
    # Guardar la tabla como imagen usando una ruta absoluta
    filename <- file.path(getwd(), paste0("tabla_temp_", i+1, ".png"))  #Change 12
    gtsave(tabla_gt, filename = filename, vwidth = 2700, vheight = 3700)
  }
  
}
