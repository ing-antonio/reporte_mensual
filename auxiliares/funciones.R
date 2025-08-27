library(dplyr)
library(tidyr)
#library(treemap)
library(lubridate)

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



#Periodo es un caracter que definira el periodo del tiempo en la tabla.
tabla <- function(df, periodo) {
      names(df) <- c("categoria_delito", "Pasado", "Anterior", "Actual", "variacion_pasado_anterior", "variacion_anterior_actual")
      
      
      # Obtener el mes actual según tu lógica (mes anterior real)
      mes_actual <- format(today() %m-% months(1), "%B")
      
      # Obtener el mes anterior según tu lógica (hace dos meses reales)
      mes_anterior <- format(today() %m-% months(2), "%B")
      
      # Asegurar nombres en español
      Sys.setlocale("LC_TIME", "es_ES.UTF-8")
      
      # Convertir primera letra en mayúscula
      mes_actual <- tools::toTitleCase(tolower(mes_actual))
      mes_anterior <- tools::toTitleCase(tolower(mes_anterior))
      
      print(mes_actual)   # "Febrero" si hoy es marzo
      print(mes_anterior) # "Enero" si hoy es marzo
      
      # Crear la tabla base
      tabla_gt_variacion <- df %>%
            gt()
      
      if (periodo == 'semana') {
            # Etiquetas de las columnas para el periodo semanal
            tabla_gt_variacion <- tabla_gt_variacion %>%
                  cols_label(
                        categoria_delito = "Categoría del Delito",
                        Actual = mes_actual,
                        Anterior = mes_anterior,
                        Pasado = paste0("Mes"),
                        variacion_anterior_actual = paste0("Var ", mes_anterior, " vs ", mes_actual),
                        variacion_pasado_anterior = paste0("Var ", mes_anterior, " vs ",  mes_actual)
                  ) %>%
                  # Ocultar columna de variación pasada
                  cols_hide(columns = c("Pasado","variacion_pasado_anterior"))
      } else {
            # Etiquetas de las columnas para otro periodo
            tabla_gt_variacion <- tabla_gt_variacion %>%
                  cols_label(
                        categoria_delito = "Categoría del Delito",
                        Actual = paste0("Año ", " ", anio_actual),
                        Anterior = paste0("Año ", " ", anio_anterior),
                        Pasado = paste0("Año ", " ", anio_pasado),
                        variacion_anterior_actual = paste0("Var ", anio_anterior, " vs ", anio_actual),
                        variacion_pasado_anterior = paste0("Var ", anio_pasado, " vs ", anio_anterior)
                  )
      }
      
      
      # Aplicar estilos y transformaciones adicionales
      tabla_gt_variacion <- tabla_gt_variacion %>%
            text_transform(
                  locations = cells_body(columns = c("variacion_anterior_actual", "variacion_pasado_anterior")),
                  fn = function(x) {
                        sapply(x, porcentaje_color)  # Aplica la función porcentaje_color
                  }
            ) %>%
            tab_style(
                  style = list(
                        cell_text(color = "black", weight = "bold", size = px(20), align = "center", font = "Cabin"),  
                        cell_fill(color = "white")
                  ),
                  locations = cells_body(columns = c("Actual", "Anterior", "Pasado", "variacion_anterior_actual", "variacion_pasado_anterior"))
            ) %>%
            tab_style(
                  style = list(
                        cell_text(color = "black", weight = "bold", size = px(20), align = "left", font = "Cabin"),  
                        cell_fill(color = "white")
                  ),
                  locations = cells_body(columns = c("categoria_delito"))
            ) %>%
            tab_style(
                  style = list(
                        cell_fill(color = "#EAD1DC")
                  ),
                  locations = cells_body(columns = c("Actual", "variacion_anterior_actual"))
            ) %>%
         #   tab_style(
          #        style = list(
           #             cell_fill(color = "#faeedc"),  # Fondo del color especificado
            #            cell_borders(
             #                 sides = c("top", "bottom"),  # Bordes solo en la parte superior e inferior de la fila
              #                color = "#b27d49",           # Color del borde
               #               weight = px(4),              # Grosor del borde
                #              style = "solid"              # Estilo del borde
                 #       )
                  #),
                  #locations = cells_body(
                  #      rows = variacion_anterior_actual > 0  # Condición para aplicar el estilo
                  #)
            #) %>%
            tab_style(
                  style = list(
                        cell_text(color = "white", weight = "bold", font = "Cabin", align = "center", size = px(30)),  
                        cell_fill(color = "#BC955C")
                  ),
                  locations = cells_column_labels(columns = everything())
            ) %>%
            cols_width(
                  categoria_delito ~ px(320),
                  Actual ~ px(150),
                  Anterior ~ px(150),
                  Pasado ~ px(100),
                  variacion_anterior_actual ~ px(150),
                  variacion_pasado_anterior ~ px(150)
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
                  column_labels.vlines.color = "#BC955C"
            ) %>%
            tab_options(
                  table.width = "100%"
            )
      
      return(tabla_gt_variacion)
}

procesar_datos_periodo <- function(data, tipo_periodo, victimas) {
      victimas_pasado <- victimas %>% filter(periodo == "Pasado")
      victimas_anterior <- victimas %>% filter(periodo == "Anterior")
      victimas_actual <- victimas %>% filter(periodo == "Actual")
      
      
      # Agrupación y procesamiento
      resultado <- data %>%
            filter(categoria_delito != 'Delito de bajo impacto') %>%
            procesar_datos(agrupaciones = c("categoria_delito", "periodo"), periodo = "periodo")
      
      # Calcular la fila de totales
      fila_total <- c("Alto Impacto", colSums(resultado[,-1], na.rm = TRUE))
      
      # Agregar la fila de totales
      resultado <- rbind(resultado, fila_total)
      
      # Obtener variaciones
      resultado <- resultado %>%
            mutate(across(-categoria_delito, as.numeric)) %>%
            add_row(
                  categoria_delito = 'Homicidio doloso (víctimas)', # Crear una columna con el valor fijo
                  Actual = nrow(victimas_actual),
                  Anterior = nrow(victimas_anterior),
                  Pasado = nrow(victimas_pasado)   
                  
                  
            ) %>% 
            mutate(
                  variacion_pasado_anterior = variacion(Anterior, Pasado),
                  variacion_anterior_actual = variacion(Actual, Anterior)
            ) %>%
            select(categoria_delito, Pasado, Anterior, Actual, variacion_pasado_anterior, variacion_anterior_actual) %>%
            mutate(lvl = case_when(
                  categoria_delito == "Alto Impacto" ~ 1,
                  categoria_delito == "Homicidio doloso" ~ 2,
                  categoria_delito == "Homicidio doloso (víctimas)" ~ 3,
                  categoria_delito == "Lesiones dolosas por disparo de arma de fuego" ~ 4,
                  categoria_delito == "Robo a transeunte en vía pública con y sin violencia" ~ 5,
                  categoria_delito == "Robo a transeunte en vía pública con violencia" ~ 6,
                  categoria_delito == "Robo a transeunte en vía pública sin violencia" ~ 7,
                  categoria_delito == "Robo de vehículo con y sin violencia" ~ 8,
                  categoria_delito == "Robo de vehículo con violencia" ~ 9,
                  categoria_delito == "Robo de vehículo sin violencia" ~ 10,
                  categoria_delito == "Secuestro" ~ 11,
                  categoria_delito == "Violación" ~ 12,
                  categoria_delito == "Robo a negocio con violencia" ~ 13,
                  categoria_delito == "Robo a pasajero a bordo del metro con y sin violencia" ~ 14,
                  categoria_delito == "Robo a repartidor con y sin violencia" ~ 15,
                  categoria_delito == "Robo a pasajero a bordo de microbus con y sin violencia" ~ 16,
                  categoria_delito == "Robo a cuentahabiente saliendo del cajero con violencia" ~ 17,
                  categoria_delito == "Robo a pasajero a bordo de taxi con violencia" ~ 18,
                  categoria_delito == "Robo a casa habitación con violencia" ~ 19,
                  categoria_delito == "Robo a transportista con y sin violencia" ~ 20,
                  categoria_delito == "Robo a transeúnte en vía pública con y sin violencia" ~ 21,
                  categoria_delito == "Robo a pasajero a bordo de microbús con y sin violencia" ~ 22,
                  categoria_delito == "Delito de bajo impacto" ~ 23,
                  TRUE ~ NA_real_
            )) %>%
            arrange(lvl, desc(Actual)) %>%
            select(-lvl)
      
      
      resultado <- resultado %>%
            mutate(
                  across(c(Pasado, Anterior, Actual), ~ scales::comma(.))
            )
      
      # Llamada a la función tabla()
      return(tabla(resultado, tipo_periodo))
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
            select(alcaldia_hechos = alcaldia_hecho, everything())
      
      
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
      
      
      if(periodo=="semanal"){
            semana<-ceiling(yday(fecha_corte)/7)
            inicial_Actual<-paste0("Mes ",semana-1)
            final_Actual<-paste0("Mes ",semana)
            texto_label<-paste0("Comparativo ",semana_anterior," de 2024 vs ",semana_actual," de 2025.")
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
      
      if(periodo == 'semanal'){
            
            G1 <- G1 +
                  geom_richtext(
                        aes(
                              x = levels(g$alcaldia_hechos)[1],
                              y = M * 0.9
                        ),
                        label = paste0(
                              scales::comma(nrow(ci_actual)),
                              "<span style='color:black;'> carpetas iniciadas </span>",
                              "<span style='color:#888888;'>en el mes </span>", semana_actual, "<br>",
                              scales::comma(nrow(ci_anterior)), 
                              "<span style='color:black;'> carpetas iniciadas </span>",
                              "<span style='color:#888888;'>en el mes </span>", semana_anterior, "<br>",
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



# Función para el grafico treemap
F_treemap <- function(data, fecha_inicio = NULL, fecha_fin = NULL) {
      c("FECHA DE INICIO", "DELEGACIÓN HECHOS") %in% colnames(data)
      
      
      #Validación de Fechas
      if (!is.null(fecha_inicio) && !is.null(fecha_fin)) {
            data <- data %>%
                  filter(`FECHA DE INICIO` >= as.Date(fecha_inicio) & `FECHA DE INICIO` <= as.Date(fecha_fin))
      }
      
      hech_alcal <- data %>%  #Agrupación y frecuencia de delitos por alcaldia, obtenemos los 3 mas altos
            group_by(`DELEGACIÓN HECHOS`) %>%
            summarise(Frecuencia = n()) %>%
            arrange(desc(Frecuencia)) %>%
            slice(1:3)
      
      hech_alcal <- hech_alcal[1:3, ]
      
      hech_alcal<- hech_alcal %>%
            mutate(Nombre = paste(`DELEGACIÓN HECHOS`, Frecuencia))
      
      treemap(   #Atributos de treemap
            hech_alcal,
            index = "Nombre",       
            vSize = "Frecuencia",             
            title = "ALCALDIAS CON MATOR CANTIDAD DE DELITO",   )
}  


treemap_tabla_alcaldia <- function(df){
      datos_semanal <- df %>%
            group_by(alcaldia_hecho, categoria_delito) %>%
            summarise(cantidad = n(), .groups = "drop") %>% # Calcula la cantidad por grupo
            mutate(alcaldia_hecho = ifelse(is.na(alcaldia_hecho), "Sin Alcaldía", alcaldia_hecho)) %>%
            arrange(desc(cantidad)) # Ordena en orden descendente por cantidad
      
      
      
      return(datos_semanal)
      
}


treemap_alcaldia <- function(df){
      
      
      agrupacion_alcaldia <- agrupar_datos(df, c("alcaldia_hecho" ) )  %>% 
            mutate(alcaldia_hecho = ifelse(is.na(alcaldia_hecho), "Sin Alcaldía", alcaldia_hecho)) %>% 
            arrange(desc(cantidad))
      
      # Ordenar de mayor a menor el total
      agrupacion_ordenada <- agrupacion_alcaldia %>% arrange(desc(cantidad))
      
      # Obtener el valor del total del décimo lugar
      decimo_valor <- agrupacion_ordenada$cantidad[10]
      
      # Filtrar todos los registros que tengan un valor mayor o igual que el décimo lugar
      resultado_filtrado  <- agrupacion_ordenada %>%
            filter(cantidad >= decimo_valor)
      
      # Tomar solo los primeros 16 registros (top 10 + 6 adicionales)
      resultado_final <- resultado_filtrado %>% slice(1:16) %>% 
            mutate(total_global = sum(cantidad),
                   porcentaje = round((cantidad / total_global) * 100, 1),
                   etiqueta = paste0(alcaldia_hecho," \n",cantidad))
      
      # Encontrar el valor mínimo del top 3
      valor_top3 <- resultado_final$cantidad[3]
      
      # Filtrar registros hasta que el valor deje de coincidir con el tercer lugar
      resultado_top <- resultado_final %>%
            filter(cantidad >= valor_top3)
      
      palette <- colorRampPalette(colors=c ("#9f2241", "#de7aaf"))
      colores <- palette(nrow(resultado_top))
      
      interseccion_aux11<-resultado_top %>% 
            ungroup() %>% 
            mutate(color=colores[1:nrow(resultado_top)])
      
      
      
      
      treemmap <- ggplot(data = interseccion_aux11,
                         aes(area = cantidad,
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
                              padding.x = grid::unit(8, "mm"), # Reducir el padding si es necesario
                              padding.y = grid::unit(8, "mm"),
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
            theme(aspect.ratio = .8)  # Cambiar la relación de aspecto para hacerlo más grande
      
      treemmap
      
      
      setwd("G:/.shortcut-targets-by-id/1o3RW9XtzyaaBxBvSYFPTHgER_wJ6O_VR/Analisis/Gabinete de Seguridad/20241203_incidencia_delictiva_semanal/03_output/tabla_alcadia") # Establecer un directorio temporal
      
      # Cargar la tabla como una imagen
      tabla_img <- grid::rasterGrob(png::readPNG("tabla_alcaldia.png"), interpolate = TRUE)
      
      
      # Crear el espaciador
      espaciador <- ggplot() + theme_void()
      
      # Combinar treemap y tabla con proporciones ajustadas
      final_output <- cowplot::plot_grid(
            treemmap,                         # Treemap a la izquierda
            espaciador,                      # Separación entre treemap y tabla
            ggdraw() + draw_grob(tabla_img), # Tabla a la derecha
            nrow = 1,                        # Combinar horizontalmente
            rel_widths = c(1, 0.1, 1.5)      # Ajustar proporciones (espacio relativo para el separador)
      )
      
      return(final_output)
}


##### Funciones adicionales #####
agregar_suma_filas <- function(df) {
      # Esta función agrega una fila al final del dataframe que contiene los totales para todas las columnas numéricas.
      # Para las columnas no numéricas, asigna valores predeterminados como "Totales" para alcaldía y NA para las demás.
      # Es útil para generar una fila resumen al final de los datos originales.
      
      # Calcular las sumas solo para columnas numéricas
      fila_total <- df %>%
            summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
      
      # Crear una fila con el mismo número de columnas
      fila_total <- as.data.frame(cbind(
            alcaldia_hecho = "Totales",  # Asignar un valor a las columnas no numéricas
            colonia_hecho = NA,          # Agregar el campo colonia_hecho
            nombre_sec = NA,
            Nomenclatu = NA,
            fila_total
      ))
      
      # Asegurar que los nombres de las columnas coincidan
      colnames(fila_total) <- colnames(df)
      
      # Concatenar el dataframe original con la fila de totales
      df_sumas <- rbind(df, fila_total)
      
      return(df_sumas)
}


agregar_totales_por_alcaldia <- function(df) {
      # Esta función agrega totales por alcaldía al dataframe. Para cada alcaldía, calcula la suma de las columnas numéricas.
      # Además, rellena las columnas no numéricas con valores específicos:
      # - "SIN COLONIA" para la columna "colonia_hecho".
      # - NA para las demás columnas no numéricas.
      # Finalmente, concatena las filas de totales al final del dataframe original y asegura que las columnas estén en el mismo orden.
      
      # Sumar totales por alcaldía para las columnas numéricas
      totales_por_alcaldia <- df %>%
            group_by(alcaldia_hecho) %>%
            summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
            ungroup()
      
      # Agregar columnas no numéricas (rellenadas con valores apropiados)
      columnas_no_numericas <- setdiff(names(df), names(totales_por_alcaldia))
      
      for (columna in columnas_no_numericas) {
            if (columna == "Nomenclatu") {
                  totales_por_alcaldia[[columna]] <- NA
            } else if (columna == "colonia_hecho") {
                  totales_por_alcaldia[[columna]] <- "SIN COLONIA"
            } else {
                  totales_por_alcaldia[[columna]] <- NA
            }
      }
      
      # Asegurarse de que las columnas estén en el mismo orden
      totales_por_alcaldia <- totales_por_alcaldia %>%
            select(all_of(names(df)))
      
      # Concatenar el dataframe original con los totales por alcaldía
      df_sumas_actualizado <- rbind(df, totales_por_alcaldia)
      
      return(df_sumas_actualizado)
}


limpiar_na <- function(df) {
      # Esta función limpia los valores NA (faltantes) en un dataframe.
      # - En la columna "Nomenclatu", reemplaza los NA con "SIN CUADRANTE".
      # - En la columna "colonia_hecho", reemplaza los NA con "SIN COLONIA".
      # - Para todas las columnas numéricas, reemplaza los NA con 0.
      # Devuelve el dataframe con los valores faltantes limpios.
      
      # Reemplazar NA en Nomenclatu por "SIN CUADRANTE"
      if ("Nomenclatu" %in% colnames(df)) {
            df$Nomenclatu[is.na(df$Nomenclatu)] <- "SIN CUADRANTE"
      }
      
      # Reemplazar NA en colonia_hecho por "SIN COLONIA"
      if ("colonia_hecho" %in% colnames(df)) {
            df$colonia_hecho[is.na(df$colonia_hecho)] <- "SIN COLONIA"
      }
      
      # Reemplazar NA en columnas numéricas por 0
      df[] <- lapply(df, function(col) {
            if (is.numeric(col)) {
                  col[is.na(col)] <- 0
            }
            return(col)
      })
      
      return(df)
}


agregar_variaciones <- function(df) {
      # Esta función agrega columnas de variación al dataframe:
      # - Calcula la variación porcentual de "Homicidio doloso" entre los valores actuales y anteriores,
      #   almacenándola en la columna "Variacion_Homicidios_Dolosos".
      # - Calcula la variación porcentual de "Lesiones dolosas por disparo de arma de fuego" entre los valores actuales y anteriores,
      #   almacenándola en la columna "Variacion_Lesiones_Arma_Fuego".
      # - Combina los valores de "Nomenclatu", "colonia_hecho", y "nombre_sec" en una sola columna "Nomenclatu",
      #   con el formato: "Nomenclatu (colonia_hecho, nombre_sec)".
      # - Reemplaza valores NA en "nombre_sec" con "SIN SECTOR".
      # - Reordena las columnas para organizar el dataframe.
      # - Elimina la columna "colonia_hecho" después de la transformación.
      # Devuelve el dataframe con las nuevas columnas y la estructura reorganizada.
      
      # Calcular la variación de homicidios dolosos
      df <- df %>%
            mutate(
                  nombre_sec = ifelse(is.na(nombre_sec),"SIN SECTOR", nombre_sec),
                  Nomenclatu = paste0(Nomenclatu," (", colonia_hecho, ", ", nombre_sec, ")"),
                  Variacion_Homicidios_Dolosos = variacion(`Homicidio doloso_Actual`, `Homicidio doloso_Anterior`),
                  Variacion_Lesiones_Arma_Fuego = variacion(`Lesiones dolosas por disparo de arma de fuego_Actual`, `Lesiones dolosas por disparo de arma de fuego_Anterior`)
            ) %>% 
            select(alcaldia_hecho, nombre_sec, Nomenclatu, `Homicidio doloso_Anterior`, `Homicidio doloso_Actual`, Variacion_Homicidios_Dolosos,
                   `Lesiones dolosas por disparo de arma de fuego_Anterior`, `Lesiones dolosas por disparo de arma de fuego_Actual`,
                   Variacion_Lesiones_Arma_Fuego, -colonia_hecho)
      
      
      return(df)
}


# Función para ordenar filas por grupos con "Totales" al inicio y eliminar la segunda fila si la primera es "Totales"
ordenar_por_grupo <- function(dataframe) {
      # Esta función organiza un dataframe agrupando y priorizando las filas:
      # - Asigna una prioridad para mover las filas con "Totales" en la columna "alcaldia_hecho" al inicio.
      # - Ordena el dataframe por:
      #   1. Prioridad (colocando "Totales" al inicio del dataframe).
      #   2. Alcaldía ("alcaldia_hecho") en orden alfabético.
      #   3. Valores numéricos en columnas específicas ("Homicidio doloso_Anterior", "Homicidio doloso_Actual",
      #      "Lesiones dolosas por disparo de arma de fuego_Anterior", "Lesiones dolosas por disparo de arma de fuego_Actual")
      #      en orden descendente.
      # - Elimina la columna auxiliar de prioridad después del ordenamiento.
      # - Verifica si la primera fila contiene "Totales" en "alcaldia_hecho" y, si es el caso, elimina la segunda fila para evitar duplicados.
      # Devuelve el dataframe ordenado y procesado.
      
      library(dplyr)
      
      dataframe_ordenado <- dataframe %>%
            mutate(
                  prioridad = ifelse(alcaldia_hecho == "Totales", 1, 2)  # Asignar prioridad a "Totales"
            ) %>%
            arrange(
                  prioridad,  # Forzar "Totales" al inicio
                  alcaldia_hecho,  # Ordenar por alcaldía después
                  desc(`Homicidio doloso_Anterior`), 
                  desc(`Homicidio doloso_Actual`),
                  desc(`Lesiones dolosas por disparo de arma de fuego_Anterior`),
                  desc(`Lesiones dolosas por disparo de arma de fuego_Actual`)
            ) %>%
            select(-prioridad)  # Eliminar columna auxiliar de prioridad
      
      # Verificar si la primera fila contiene "Totales" y eliminar la segunda fila si es el caso
      if (dataframe_ordenado$alcaldia_hecho[1] == "Totales") {
            dataframe_ordenado <- dataframe_ordenado[-2, ]
      }
      
      return(dataframe_ordenado)
}


ajustar_totales <- function(df) {
      # Sustituir "Totales (SIN COLONIA)" por "TOTAL" en la columna Nomenclatu
      df$Nomenclatu[df$Nomenclatu == "Totales (SIN COLONIA)"] <- "TOTAL"
      
      # Dejar vacía la columna Nomenclatu si en alcaldia_hecho aparece "Totales"
      df$Nomenclatu[df$alcaldia_hecho == "Totales"] <- ""
      
      df <- df %>% 
            select(-nombre_sec)
      return(df)
}


formatear_porcentaje <- function(dataframe) {
      # Función para formatear columnas de variación como porcentaje
      
      dataframe <- dataframe %>%
            mutate(
                  Variacion_Homicidios_Dolosos = paste0(Variacion_Homicidios_Dolosos, "%"),
                  Variacion_Lesiones_Arma_Fuego = paste0(Variacion_Lesiones_Arma_Fuego, "%")
            )
      
      return(dataframe)
}


ordenar_por_totales_y_alcaldia <- function(df) {
      library(dplyr)
      # Esta función organiza un dataframe priorizando filas específicas según su contenido:
      # - Crea dos columnas auxiliares:
      #   1. "es_totales_alcaldia": Identifica si "alcaldia_hecho" contiene "Totales".
      #   2. "es_total_nomenclatu": Identifica si "Nomenclatu" contiene "TOTAL".
      # - Ordena el dataframe con las siguientes prioridades:
      #   1. Las filas donde "alcaldia_hecho" es "Totales" tienen mayor prioridad (descendente).
      #   2. Las filas se agrupan por "alcaldia_hecho".
      #   3. Dentro de cada grupo, las filas donde "Nomenclatu" es "TOTAL" tienen mayor prioridad (descendente).
      # - Elimina las columnas auxiliares tras el ordenamiento para mantener limpio el dataframe.
      # Devuelve el dataframe ordenado y listo para su uso.
      
      # Crear un campo auxiliar para identificar si la columna alcaldia_hecho es "Totales"
      df <- df %>%
            mutate(
                  es_totales_alcaldia = ifelse(alcaldia_hecho == "Totales", 1, 0),
                  es_total_nomenclatu = ifelse(Nomenclatu == "TOTAL", 1, 0)
            )
      
      # Ordenar el dataframe:
      # 1. Primero por "Totales" en alcaldia_hecho.
      # 2. Luego por alcaldía y "TOTAL" en Nomenclatu.
      df <- df %>%
            arrange(desc(es_totales_alcaldia), alcaldia_hecho, desc(es_total_nomenclatu))
      
      # Eliminar los campos auxiliares
      df <- df %>%
            select(-es_totales_alcaldia, -es_total_nomenclatu)
      
      return(df)
}


ordenar_por_suma_y_totales <- function(df) {
      # Esta función ordena un dataframe según ciertas prioridades específicas:
      # - Crea un campo auxiliar "es_totales_alcaldia" para identificar filas donde "alcaldia_hecho" es "Totales".
      # - Calcula la suma de "Homicidio doloso_Actual" y "Lesiones dolosas por disparo de arma de fuego_Actual"
      #   para cada grupo de "alcaldia_hecho", excluyendo las filas donde "alcaldia_hecho" es "Totales".
      # - Une las sumas calculadas con el dataframe original para utilizarlas en el ordenamiento.
      # - Ordena el dataframe según las siguientes prioridades:
      #   1. Las filas con "alcaldia_hecho" igual a "Totales" tienen la mayor prioridad (descendente).
      #   2. Las alcaldías con la mayor suma total ("total_suma_actual") se priorizan.
      #   3. En caso de empate, las filas con mayor valor en "Homicidio doloso_Actual" se priorizan.
      #   4. Finalmente, las filas se agrupan por alcaldía en orden alfabético.
      # - Elimina las columnas auxiliares generadas durante el proceso para mantener el dataframe limpio.
      # Devuelve el dataframe ordenado según las prioridades definidas.
      
      library(dplyr)
      
      # Crear un campo auxiliar para identificar si la fila es "Totales" en alcaldia_hecho
      df <- df %>%
            mutate(es_totales_alcaldia = ifelse(alcaldia_hecho == "Totales", 1, 0))
      
      # Calcular las sumas de las columnas "Homicidio doloso_Actual" y "Lesiones dolosas por disparo de arma de fuego_Actual"
      # para cada alcaldía (excluyendo "Totales")
      sumas_alcaldias <- df %>%
            filter(alcaldia_hecho != "Totales") %>%
            group_by(alcaldia_hecho) %>%
            summarise(
                  total_homicidio_actual = sum(`Homicidio doloso_Actual`, na.rm = TRUE),
                  total_lesiones_actual = sum(`Lesiones dolosas por disparo de arma de fuego_Actual`, na.rm = TRUE),
                  total_suma_actual = total_homicidio_actual + total_lesiones_actual,
                  .groups = "drop"
            )
      
      # Combinar las sumas con el dataframe original
      df <- df %>%
            left_join(sumas_alcaldias, by = "alcaldia_hecho")
      
      # Asegurarse de que "Totales" se agrupe correctamente
      df <- df %>%
            mutate(
                  total_suma_actual = ifelse(is.na(total_suma_actual), 0, total_suma_actual),
                  total_homicidio_actual = ifelse(is.na(total_homicidio_actual), 0, total_homicidio_actual)
            )
      
      # Ordenar el dataframe:
      # 1. Primero por "Totales" en alcaldia_hecho (es_totales_alcaldia desc).
      # 2. Luego por la suma total más grande (total_suma_actual desc).
      # 3. En caso de empate, por el valor más grande en "Homicidio doloso_Actual".
      # 4. Finalmente, agrupar por alcaldía.
      df <- df %>%
            arrange(
                  desc(es_totales_alcaldia),
                  desc(total_suma_actual),
                  desc(total_homicidio_actual),
                  alcaldia_hecho
            )
      
      # Eliminar las columnas auxiliares
      df <- df %>%
            select(-es_totales_alcaldia, -total_homicidio_actual, -total_lesiones_actual, -total_suma_actual)
      
      return(df)
}


ordenar_y_reestructurar <- function(df) {
      library(dplyr)
      
      # Paso 1: Crear un campo auxiliar para identificar si la fila es "Totales" en alcaldia_hecho
      df <- df %>%
            mutate(es_totales_alcaldia = ifelse(alcaldia_hecho == "Totales", 1, 0))
      
      # Paso 2: Calcular las sumas de homicidios y lesiones para cada alcaldía (excluyendo "Totales")
      sumas_alcaldias <- df %>%
            filter(alcaldia_hecho != "Totales") %>%
            group_by(alcaldia_hecho) %>%
            summarise(
                  total_homicidio_actual = sum(`Homicidio doloso_Actual`, na.rm = TRUE),
                  total_lesiones_actual = sum(`Lesiones dolosas por disparo de arma de fuego_Actual`, na.rm = TRUE),
                  total_suma_actual = total_homicidio_actual + total_lesiones_actual,
                  .groups = "drop"
            )
      
      # Paso 3: Combinar las sumas con el dataframe original
      df <- df %>%
            left_join(sumas_alcaldias, by = "alcaldia_hecho")
      
      # Paso 4: Asegurarse de que las filas "Totales" tengan valores auxiliares adecuados
      df <- df %>%
            mutate(
                  total_suma_actual = ifelse(is.na(total_suma_actual), 0, total_suma_actual),
                  total_homicidio_actual = ifelse(is.na(total_homicidio_actual), 0, total_homicidio_actual)
            )
      
      # Paso 5: Reorganizar las filas dentro de cada alcaldía
      df <- df %>%
            group_by(alcaldia_hecho) %>%
            mutate(
                  # Identificar si es la primera aparición de "SIN CUADRANTE (SIN COLONIA, SIN SECTOR)"
                  EsPrimeraAparicion = cumsum(Nomenclatu == "SIN CUADRANTE (SIN COLONIA, SIN SECTOR)") == 1,
                  Orden_Temp = case_when(
                        Nomenclatu == "SIN CUADRANTE (SIN COLONIA, SIN SECTOR)" & EsPrimeraAparicion ~ 1, # Primera aparición
                        TRUE ~ 2
                  ),
                  # Cambiar "SIN CUADRANTE" solo si es la primera aparición
                  Nomenclatu = ifelse(Nomenclatu == "SIN CUADRANTE (SIN COLONIA, SIN SECTOR)" & EsPrimeraAparicion, "TOTALES", Nomenclatu)
            ) %>%
            arrange(Orden_Temp, .by_group = TRUE) %>%
            ungroup() %>%
            select(-Orden_Temp, -EsPrimeraAparicion)  # Eliminar columnas auxiliares
      
      # Paso 6: Ordenar el dataframe globalmente:
      # 1. Primero por "Totales" en alcaldia_hecho (es_totales_alcaldia desc).
      # 2. Luego por la suma total más grande (total_suma_actual desc).
      # 3. En caso de empate, por el valor más grande en "Homicidio doloso_Actual".
      df <- df %>%
            arrange(
                  desc(es_totales_alcaldia),
                  desc(total_suma_actual),
                  desc(total_homicidio_actual),
                  alcaldia_hecho
            )
      
      # Paso 7: Eliminar las columnas auxiliares
      df <- df %>%
            select(-es_totales_alcaldia, -total_homicidio_actual, -total_lesiones_actual, -total_suma_actual)
      
      return(df)
}


dividir_dataframe <- function(dataframe, filas_por_tabla = 15) {
      # Función para dividir el dataframe en subtablas de 18 filas
      
      # Dividir el dataframe en partes de `filas_por_tabla`
      subtablas <- split(dataframe, ceiling(seq_along(1:nrow(dataframe)) / filas_por_tabla))
      return(subtablas)
}



aplicar_estilos_a_subtablas <- function(subtablas) {
      # Solo estilos de encabezados
      library(showtext)
      
      # Registrar la fuente Cabin
      font_add_google("Cabin", "cabin")
      showtext_auto()
      # Aplicar estilos a cada subtabla
      subtablas_estilizadas <- lapply(subtablas, function(subtabla) {
            subtabla %>%
                  gt() %>%
                  cols_label(
                        alcaldia_hecho = "Alcaldía",
                        Nomenclatu = "Cuadrante (Sector, Colonia)",
                        `Homicidio doloso_Anterior` = "Homicidio Semana Anterior",
                        `Homicidio doloso_Actual` = "Homicidio Semana Actual",
                        Variacion_Homicidios_Dolosos = "Variación Homicidio",
                        `Lesiones dolosas por disparo de arma de fuego_Anterior` = "Lesiones Semana Anterior",
                        `Lesiones dolosas por disparo de arma de fuego_Actual` = "Lesiones Semana Actual",
                        Variacion_Lesiones_Arma_Fuego = "Variación Lesiones"
                  ) %>%
                  tab_style(
                        style = list(
                              cell_text(
                                    color = "white", 
                                    weight = "bold", 
                                    font = "cabin", 
                                    align = "center", 
                                    size = px(50)
                              ),
                              cell_fill(color = "#BC955C")
                        ),
                        locations = cells_column_labels()
                  )
      })
      
      return(subtablas_estilizadas)
}


aplicar_estilos_sin_duplicar <- function(subtablas) {
      # Esta función aplica estilos personalizados a una lista de subtablas generadas con `gt`. 
      # Incluye personalización para encabezados, filas específicas, y colores para columnas de variación.
      #
      # Funcionalidades:
      # 1. Configura encabezados con estilos específicos, como fuente "Cabin" y colores de fondo personalizados.
      # 2. Aplica un estilo base a todas las celdas del cuerpo de la tabla.
      # 3. Resalta la primera fila de cada alcaldía con un fondo y texto específicos, excepto si la fila pertenece a "Totales".
      # 4. Aplica colores a los porcentajes de las columnas "Variacion_Homicidios_Dolosos" y "Variacion_Lesiones_Arma_Fuego":
      #    - Verde (#027a35) para porcentajes negativos.
      #    - Rojo (#B02858) para porcentajes positivos.
      # 5. Si es la primera subtabla, aplica un estilo especial a las filas que contengan "Totales" en `alcaldia_hecho`.
      # 6. Oculta la columna auxiliar `EsPrimera` utilizada para identificar la primera fila estilizada.
      #
      # Retorna una lista de tablas estilizadas listas para exportar o visualizar.
      
      library(gt)
      library(dplyr)
      library(showtext)
      
      # Registrar la fuente Cabin
      font_add_google("Cabin", "cabin")
      showtext_auto()
      
      # Lista para llevar un registro de las alcaldías ya estilizadas
      alcaldias_estilizadas <- c()
      
      # Aplicar estilos completos a cada subtabla
      subtablas_estilizadas <- lapply(seq_along(subtablas), function(i) {
            subtabla <- subtablas[[i]]
            
            # Identificar las primeras filas de cada alcaldía que aún no han sido estilizadas
            subtabla <- subtabla %>%
                  mutate(
                        EsPrimera = alcaldia_hecho != lag(alcaldia_hecho, default = "") &
                              !alcaldia_hecho %in% alcaldias_estilizadas &
                              alcaldia_hecho != "Totales"
                  )
            
            # Actualizar el registro de alcaldías estilizadas
            alcaldias_estilizadas <<- unique(c(alcaldias_estilizadas, subtabla$alcaldia_hecho[subtabla$EsPrimera]))
            
            # Crear tabla estilizada
            tabla_estilizada <- subtabla %>%
                  gt() %>%
                  # Personalizar encabezados
                  cols_label(
                        alcaldia_hecho = "Alcaldía",
                        Nomenclatu = "Cuadrante (Sector, Colonia)",
                        `Homicidio doloso_Anterior` = "Homicidio Semana Anterior",
                        `Homicidio doloso_Actual` = "Homicidio Semana Actual",
                        Variacion_Homicidios_Dolosos = "Variación Homicidio",
                        `Lesiones dolosas por disparo de arma de fuego_Anterior` = "Lesiones Semana Anterior",
                        `Lesiones dolosas por disparo de arma de fuego_Actual` = "Lesiones Semana Actual",
                        Variacion_Lesiones_Arma_Fuego = "Variación Lesiones"
                  ) %>%
                  tab_style(
                        style = list(
                              cell_text(
                                    color = "white", 
                                    weight = "bold", 
                                    font = "cabin", 
                                    align = "center", 
                                    size = px(50)
                              ),
                              cell_fill(color = "#BC955C")
                        ),
                        locations = cells_column_labels()
                  ) %>%
                  # Estilo base
                  tab_style(
                        style = list(
                              cell_text(
                                    color = "black", 
                                    weight = "bold", 
                                    size = px(29), 
                                    font = "cabin"
                              ),
                              cell_fill(color = "white")
                        ),
                        locations = cells_body()
                  ) %>%
                  # Estilo específico para primeras filas de cada alcaldía (excepto Totales)
                  tab_style(
                        style = list(
                              cell_text(
                                    color = "#B02858", 
                                    weight = "bold", 
                                    size = px(29), 
                                    font = "cabin"
                              ),
                              cell_fill(color = "#EAD1DC")
                        ),
                        locations = cells_body(
                              rows = which(subtabla$EsPrimera)
                        )
                  ) %>%
                  # Cuadrícula
                  tab_style(
                        style = list(
                              cell_borders(
                                    sides = c("top", "bottom", "left", "right"),
                                    color = "#BC955C",
                                    weight = px(2)
                              )
                        ),
                        locations = cells_body()
                  ) %>%
                  # Colores para columnas de variación
                  tab_style(
                        style = list(
                              cell_text(color = "#027a35", weight = "bold", 
                                        font = "cabin")
                        ),
                        locations = cells_body(
                              columns = c(Variacion_Homicidios_Dolosos),
                              rows = as.numeric(gsub("%", "", subtabla$Variacion_Homicidios_Dolosos)) < 0
                        )
                  ) %>%
                  tab_style(
                        style = list(
                              cell_text(color = "#B02858", weight = "bold", 
                                        font = "cabin")
                        ),
                        locations = cells_body(
                              columns = c(Variacion_Homicidios_Dolosos),
                              rows = as.numeric(gsub("%", "", subtabla$Variacion_Homicidios_Dolosos)) > 0
                        )
                  ) %>%
                  tab_style(
                        style = list(
                              cell_text(color = "#027a35", weight = "bold", 
                                        font = "cabin")
                        ),
                        locations = cells_body(
                              columns = c(Variacion_Lesiones_Arma_Fuego),
                              rows = as.numeric(gsub("%", "", subtabla$Variacion_Lesiones_Arma_Fuego)) < 0
                        )
                  ) %>%
                  tab_style(
                        style = list(
                              cell_text(color = "#B02858", weight = "bold", 
                                        font = "cabin")
                        ),
                        locations = cells_body(
                              columns = c(Variacion_Lesiones_Arma_Fuego),
                              rows = as.numeric(gsub("%", "", subtabla$Variacion_Lesiones_Arma_Fuego)) > 0
                        )
                  )
            
            # Si es la primera subtabla, aplicar estilo especial a filas que contengan "Totales" y sobrescribir estilos anteriores
            if (i == 1) {
                  tabla_estilizada <- tabla_estilizada %>%
                        tab_style(
                              style = list(
                                    cell_text(
                                          color = "black", 
                                          weight = "bold", 
                                          size = px(29), 
                                          font = "cabin"
                                    ),
                                    cell_fill(color = "white")
                              ),
                              locations = cells_body(
                                    rows = which(subtabla$alcaldia_hecho == "Totales")
                              )
                        )
            }
            
            # Ocultar columna EsPrimera si existe
            tabla_estilizada <- tabla_estilizada %>%
                  cols_hide(columns = vars(EsPrimera))
            
            # Retornar tabla estilizada
            return(tabla_estilizada)
      })
      
      return(subtablas_estilizadas)
}


aplicar_estilo_cero_porcentaje <- function(subtablas) {
      #Función para aplicar estilos de porcentaje según las condiciones de porcentaje
      library(gt)
      
      # Iterar sobre todas las subtablas
      subtablas_actualizadas <- lapply(subtablas, function(tabla_gt) {
            # Extraer datos subyacentes para referencia
            datos_tabla <- tabla_gt[["_data"]]
            
            # Estilizar "0%" en Variacion_Homicidios_Dolosos
            if ("Variacion_Homicidios_Dolosos" %in% names(datos_tabla)) {
                  filas_homicidios <- which(datos_tabla[["Variacion_Homicidios_Dolosos"]] == "0%")
                  if (length(filas_homicidios) > 0) {
                        tabla_gt <- tabla_gt %>%
                              tab_style(
                                    style = list(
                                          cell_text(color = "black", weight = "bold", size = px(29))
                                    ),
                                    locations = cells_body(
                                          columns = "Variacion_Homicidios_Dolosos",
                                          rows = filas_homicidios
                                    )
                              )
                  }
            }
            
            # Estilizar "0%" en Variacion_Lesiones_Arma_Fuego
            if ("Variacion_Lesiones_Arma_Fuego" %in% names(datos_tabla)) {
                  filas_lesiones <- which(datos_tabla[["Variacion_Lesiones_Arma_Fuego"]] == "0%")
                  if (length(filas_lesiones) > 0) {
                        tabla_gt <- tabla_gt %>%
                              tab_style(
                                    style = list(
                                          cell_text(color = "black", weight = "bold", size = px(29))
                                    ),
                                    locations = cells_body(
                                          columns = "Variacion_Lesiones_Arma_Fuego",
                                          rows = filas_lesiones
                                    )
                              )
                  }
            }
            
            return(tabla_gt)
      })
      
      return(subtablas_actualizadas)
}


centrar_valores_subtablas <- function(subtablas) {
      # Función para centrar los valores en todas las columnas de una tabla
      library(gt)
      
      # Aplicar el estilo centrado a todas las subtablas
      subtablas_centradas <- lapply(subtablas, function(tabla_gt) {
            tabla_gt %>%
                  tab_style(
                        style = cell_text(align = "center"), # Centrar los valores
                        locations = cells_body() # Aplica el centrado a todas las celdas del cuerpo
                  )
      })
      
      return(subtablas_centradas)
}


ajustar_tamano_letra <- function(subtablas) {
      # Ajusta el tamaño de las letras
      library(gt)
      
      # Ajustar el tamaño de letra en todas las filas, respetando el estilo existente
      subtablas_modificadas <- lapply(subtablas, function(subtabla) {
            subtabla %>%
                  tab_style(
                        style = list(
                              cell_text(size = px(29))  # Solo modifica el tamaño de la letra
                        ),
                        locations = cells_body()  # Aplica el estilo a todas las celdas del cuerpo
                  )
      })
      
      return(subtablas_modificadas)
}



modificar_primera_fila_subtabla <- function(subtablas_estilizadas) {
      # Esta función modifica únicamente la primera fila de la primera subtabla en una lista de subtablas estilizadas.
      #
      # Funcionalidades:
      # 1. Aplica estilos a las columnas "Variacion_Homicidios_Dolosos" y "Variacion_Lesiones_Arma_Fuego" en la primera fila:
      #    - Rojo (#B02858) para porcentajes positivos.
      #    - Verde (#027a35) para porcentajes negativos.
      # 2. Asegura que las modificaciones solo se realicen en la primera subtabla y en la primera fila de las columnas especificadas.
      # 3. Actualiza la primera subtabla en la lista de subtablas estilizadas con los cambios aplicados.
      #
      # Retorna la lista de subtablas con la primera subtabla actualizada.
      
      library(gt)
      
      # Verificar que existan subtablas
      if (length(subtablas_estilizadas) == 0) {
            stop("La lista de subtablas está vacía.")
      }
      
      # Seleccionar la primera subtabla
      primera_subtabla <- subtablas_estilizadas[[1]]
      
      # Aplicar estilos a las columnas especificadas en la primera fila
      primera_subtabla <- primera_subtabla %>%
            tab_style(
                  style = cell_text(color = "#B02858", weight = "bold"), # Rojo para valores positivos
                  locations = cells_body(
                        columns = vars(Variacion_Homicidios_Dolosos),
                        rows = 1 & as.numeric(gsub("%", "", .data$Variacion_Homicidios_Dolosos)) > 0
                  )
            ) %>%
            tab_style(
                  style = cell_text(color = "#027a35", weight = "bold"), # Verde para valores negativos
                  locations = cells_body(
                        columns = vars(Variacion_Homicidios_Dolosos),
                        rows = 1 & as.numeric(gsub("%", "", .data$Variacion_Homicidios_Dolosos)) < 0
                  )
            ) %>%
            tab_style(
                  style = cell_text(color = "#B02858", weight = "bold"), # Rojo para valores positivos
                  locations = cells_body(
                        columns = vars(Variacion_Lesiones_Arma_Fuego),
                        rows = 1 & as.numeric(gsub("%", "", .data$Variacion_Lesiones_Arma_Fuego)) > 0
                  )
            ) %>%
            tab_style(
                  style = cell_text(color = "#027a35", weight = "bold"), # Verde para valores negativos
                  locations = cells_body(
                        columns = vars(Variacion_Lesiones_Arma_Fuego),
                        rows = 1 & as.numeric(gsub("%", "", .data$Variacion_Lesiones_Arma_Fuego)) < 0
                  )
            )
      
      # Retornar la subtabla modificada en la lista de subtablas
      subtablas_estilizadas[[1]] <- primera_subtabla
      return(subtablas_estilizadas)
}


ajustar_ancho_columnas <- function(subtablas_estilizadas, ruta_salida = "C:/Users/reshi/Downloads/imagenes") {
      # Esta función ajusta el ancho de las columnas en las subtablas estilizadas y exporta cada subtabla como una imagen PNG.
      #
      # Funcionalidades:
      # 1. Ajusta el ancho de las columnas específicas:
      #    - Columna "Nomenclatu" se ajusta a 670 px.
      #    - Columna "alcaldia_hecho" se ajusta a 260 px.
      #    - Las demás columnas se ajustan a 180 px.
      # 2. Modifica el tamaño y el estilo del texto:
      #    - Encabezados con tamaño de fuente de 30 px y en negritas.
      #    - Texto del cuerpo con tamaño de fuente de 25 px.
      # 3. Configura la tabla para ocupar el 100% del ancho disponible.
      # 4. Exporta las tablas ajustadas directamente como archivos PNG:
      #    - Ancho de la imagen: 2500 px.
      #    - Altura de la imagen: 1200 px.
      #    - Sin márgenes adicionales alrededor de la tabla.
      # 5. Asegura que las imágenes se exporten a la carpeta especificada, creando la carpeta si no existe.
      #
      # Retorna un mensaje confirmando que las subtablas han sido exportadas exitosamente.
      
      library(gt)
      
      # Asegurar que la carpeta de salida exista
      if (!dir.exists(ruta_salida)) {
            dir.create(ruta_salida, recursive = TRUE)
      }
      
      # Exportar cada subtabla estilizada como imagen PNG
      for (i in seq_along(subtablas_estilizadas)) {
            nombre_archivo <- file.path(ruta_salida, paste0("subtabla_ajustada_", i, ".png"))
            
            # Ajustar el ancho de las columnas y modificar el texto
            subtabla_ajustada <- subtablas_estilizadas[[i]] %>%
                  cols_width(
                        `Nomenclatu` ~ px(670), # Ajustar ancho para la columna específica
                        `alcaldia_hecho` ~ px(260),
                        everything() ~ px(180)  # Ajustar ancho constante para las demás
                  ) %>%
                  tab_style(
                        style = cell_text(size = px(30), weight = "bold"), # Modificar tamaño y estilo del texto en encabezados
                        locations = cells_column_labels()
                  ) %>%
                  tab_style(
                        style = cell_text(size = px(25)), # Modificar tamaño del texto en el cuerpo
                        locations = cells_body(columns = everything()) # Aplicar a todas las columnas del cuerpo
                  ) %>%
                  tab_options(
                        table.width = pct(100) # Ocupa todo el ancho disponible
                  )
            
            # Exportar tabla directamente a PNG
            gtsave(
                  data = subtabla_ajustada,
                  filename = nombre_archivo,
                  vwidth = 2500,  # Ajustar ancho del archivo exportado
                  vheight = 1200, # Ajustar altura del archivo exportado
                  expand = 0      # Sin márgenes adicionales
            )
      }
      
      message("Subtablas ajustadas y exportadas en formato PNG en la carpeta: ", ruta_salida)
}


tabla_desglosada_final <- function(df){
      
      
      df_estructurado <- agregar_suma_filas(df)
      
      df_estructurado <- agregar_totales_por_alcaldia(df_estructurado)
      
      df_estructurado <- limpiar_na(df_estructurado)
      
      df_estructurado <- agregar_variaciones(df_estructurado)
      
      df_estructurado <- ordenar_por_grupo(df_estructurado)
      
      df_estructurado <- ajustar_totales(df_estructurado)
      
      df_estructurado <- formatear_porcentaje(df_estructurado)
      
      df_estructurado <- ordenar_por_totales_y_alcaldia(df_estructurado)
      
      df_estructurado <- ordenar_por_suma_y_totales(df_estructurado)
      
      df_estructurado <- ordenar_y_reestructurar(df_estructurado)
      

      
}

#FUCNION  TABLA DEL SECTOR
treemap_tabla_sector <- function(df){
      
      datos_semanal <- agrupar_datos(df, c("nombre_sec","alcaldia_hecho","categoria_delito" )) %>% 
            filter(nombre_sec != 'Sin Sector') %>% 
            arrange(desc(cantidad)) %>% 
            slice_head(n = 50)
      
      lista <- datos_semanal %>% pull(nombre_sec)
      
      datos_semanal <- agrupar_datos(df, c("nombre_sec","alcaldia_hecho","categoria_delito" )) %>% 
            filter(nombre_sec != 'Sin Sector') %>% 
            arrange(desc(cantidad)) %>% 
            filter(nombre_sec %in% lista)
      
      return(datos_semanal)
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
            select(Sector = nombre_sec, alcaldia_hechos = alcaldia_hecho, total = cantidad, porcentaje, etiqueta)
      
      
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
tabla_semanal_cuadrante <- function(df, semana_actual, semana_anterior) {
      
      
      # Paso 1: Filtrar y preparar los datos iniciales
      semanal_cuadrante <- df %>%
            mutate(
                  anio = year(fecha_inicio),
                  mes = month(fecha_inicio),
                  numero_semana = month(fecha_inicio),
                  periodo = case_when(
                        numero_semana %in% semana_anterior ~ "Anterior",
                        numero_semana %in% semana_actual ~ "Actual"
                  )
            ) %>%
            #filter(anio == 2024, !is.na(periodo)) %>%
            mutate(cantidad = 1) %>%
            filter(
                  !is.na(Nomenclatu),            # Eliminar nulos en Cuadrante
                  !is.na(nombre_sec),            # Eliminar nulos en Sector
                  nombre_sec != "Sin Sector"     # Eliminar registros con "Sin Sector"
            )
      
      
      cuadrantes_totales_actual <- semanal_cuadrante %>%
            filter(periodo == "Actual") %>%
            group_by(Nomenclatu, alcaldia_hecho, nombre_sec, colonia_hecho, categoria_delito) %>%
            summarise(total_delitos = sum(cantidad, na.rm = TRUE), .groups = "drop") %>%
            group_by(Nomenclatu, nombre_sec, categoria_delito) %>%
            summarise(
                  alcaldia_hecho = first(alcaldia_hecho),
                  colonia_hecho = colonia_hecho[which.max(total_delitos)], # Elegir la colonia con más delitos
                  total_delitos = sum(total_delitos), # Sumar total_delitos por nombre_sec y categoria_delito
                  .groups = "drop"
            ) %>%
            group_by(Nomenclatu, categoria_delito) %>%
            summarise(
                  alcaldia_hecho = first(alcaldia_hecho),
                  nombre_sec = nombre_sec[which.max(total_delitos)], # Elegir nombre_sec con más delitos
                  colonia_hecho = colonia_hecho[which.max(total_delitos)], # Elegir colonia con más delitos
                  total_delitos = sum(total_delitos), # Sumar total_delitos por Nomenclatu y categoria_delito
                  .groups = "drop"
            ) %>%
            arrange(desc(total_delitos), nombre_sec, colonia_hecho) %>% 
         dplyr::select(total_delitos_actual = total_delitos, everything())
      
      
      # Para cuadrantes_totales_actual
      cuadrantes_totales_actual <- cuadrantes_totales_actual %>%
            group_by(Nomenclatu) %>%
            mutate(
                  colonia_hecho = colonia_hecho[which.max(total_delitos_actual)], # Elegir colonia con el mayor total
                  colonia_hecho = ifelse(sum(total_delitos_actual == max(total_delitos_actual)) > 1, 
                                         min(colonia_hecho[total_delitos_actual == max(total_delitos_actual)]), # En caso de empate, elegir alfabéticamente
                                         colonia_hecho)
            ) %>%
            ungroup()
      
      actual_agrupacion <- cuadrantes_totales_actual %>% 
            group_by(Nomenclatu) %>% 
            summarize(total_delitos_suma = sum(total_delitos_actual, na.rm = TRUE)) %>% 
            arrange(desc(total_delitos_suma))
      
      cuarto_lugar <- actual_agrupacion %>%
            slice_head(n = 50) %>%
            pull(total_delitos_suma)
      
      # Filtrar los cuadrantes hasta que el total_delitos cambie
      top_4_cuadrantes <- actual_agrupacion %>%
            filter(total_delitos_suma >= cuarto_lugar) %>%
            arrange(desc(total_delitos_suma))
      
      
      lista_top_9 <- top_4_cuadrantes %>% 
            pull(Nomenclatu)
      
      
      cuadrantes_totales_actual <- cuadrantes_totales_actual %>% 
            filter(Nomenclatu %in% lista_top_9)
      
      
      actual_agrupacion <- cuadrantes_totales_actual %>% 
            group_by(Nomenclatu) %>% 
            summarize(total_delitos_suma = sum(total_delitos_actual, na.rm = TRUE)) %>% 
            arrange(desc(total_delitos_suma))
      
      #////////////
      
      cuadrantes_totales_anterior <- semanal_cuadrante %>%
            filter(periodo == "Anterior") %>%
            group_by(Nomenclatu, alcaldia_hecho, nombre_sec, colonia_hecho, categoria_delito) %>%
            summarise(total_delitos = sum(cantidad, na.rm = TRUE), .groups = "drop") %>%
            group_by(Nomenclatu, nombre_sec, categoria_delito) %>%
            summarise(
                  alcaldia_hecho = first(alcaldia_hecho),
                  colonia_hecho = colonia_hecho[which.max(total_delitos)], # Elegir la colonia con más delitos
                  total_delitos = sum(total_delitos), # Sumar total_delitos por nombre_sec y categoria_delito
                  .groups = "drop"
            ) %>%
            group_by(Nomenclatu, categoria_delito) %>%
            summarise(
                  alcaldia_hecho = first(alcaldia_hecho),
                  nombre_sec = nombre_sec[which.max(total_delitos)], # Elegir nombre_sec con más delitos
                  colonia_hecho = colonia_hecho[which.max(total_delitos)], # Elegir colonia con más delitos
                  total_delitos = sum(total_delitos), # Sumar total_delitos por Nomenclatu y categoria_delito
                  .groups = "drop"
            ) %>%
            arrange(desc(total_delitos), nombre_sec, colonia_hecho) %>% 
        dplyr::select(total_delitos_anterior = total_delitos, everything())
      
      # Para cuadrantes_totales_anterior
      cuadrantes_totales_anterior <- cuadrantes_totales_anterior %>%
            group_by(Nomenclatu) %>%
            mutate(
                  colonia_hecho = colonia_hecho[which.max(total_delitos_anterior)], # Elegir colonia con el mayor total
                  colonia_hecho = ifelse(sum(total_delitos_anterior == max(total_delitos_anterior)) > 1, 
                                         min(colonia_hecho[total_delitos_anterior == max(total_delitos_anterior)]), # En caso de empate, elegir alfabéticamente
                                         colonia_hecho)
            ) %>%
            ungroup()
      
      
      
      cuadrantes_totales_anterior <- cuadrantes_totales_anterior %>% 
            filter(Nomenclatu %in% lista_top_9)
      
      
      anterior_agrupacion <- cuadrantes_totales_anterior %>% 
            group_by(Nomenclatu) %>% 
            summarize(total_delitos_suma = sum(total_delitos_anterior, na.rm = TRUE)) %>% 
            arrange(desc(total_delitos_suma))
      
      
      
      
      
      
      
      cuadrantes_totales_combinado <- cuadrantes_totales_actual %>%
            full_join(cuadrantes_totales_anterior %>% 
                        dplyr::select(Nomenclatu, categoria_delito, total_delitos_anterior, alcaldia_hecho_anterior = alcaldia_hecho, 
                                   nombre_sec_anterior = nombre_sec, colonia_hecho_anterior = colonia_hecho), 
                      by = c("Nomenclatu", "categoria_delito")) %>%
            mutate(
                  alcaldia_hecho = coalesce(alcaldia_hecho, alcaldia_hecho_anterior, 'Sin Alcaldía'),
                  nombre_sec = coalesce(nombre_sec, nombre_sec_anterior, 'Sin Sector'),
                  colonia_hecho = coalesce(colonia_hecho, colonia_hecho_anterior, 'Sin Colonia'),
                  total_delitos_anterior = ifelse(is.na(total_delitos_anterior), 0, total_delitos_anterior),
                  total_delitos_actual = ifelse(is.na(total_delitos_actual), 0, total_delitos_actual)
            ) %>%
        dplyr::select(Nomenclatu, alcaldia_hecho, nombre_sec, colonia_hecho, categoria_delito, total_delitos_anterior, total_delitos_actual)
      
      cuadrantes_totales_combinado <- cuadrantes_totales_combinado %>%
            group_by(Nomenclatu) %>%
            mutate(
                  # Elegir alcaldia_hecho con el mayor total_delitos_actual
                  alcaldia_hecho = alcaldia_hecho[
                        which.max(total_delitos_actual)],
                  alcaldia_hecho = ifelse(
                        sum(total_delitos_actual == max(total_delitos_actual)) > 1,
                        min(alcaldia_hecho[total_delitos_actual == max(total_delitos_actual)]), # En caso de empate, alfabéticamente menor
                        alcaldia_hecho
                  ),
                  # Elegir nombre_sec con el mayor total_delitos_actual
                  nombre_sec = nombre_sec[
                        which.max(total_delitos_actual)],
                  nombre_sec = ifelse(
                        sum(total_delitos_actual == max(total_delitos_actual)) > 1,
                        min(nombre_sec[total_delitos_actual == max(total_delitos_actual)]), # En caso de empate, alfabéticamente menor
                        nombre_sec
                  ),
                  # Elegir colonia_hecho con el mayor total_delitos_actual
                  colonia_hecho = colonia_hecho[
                        which.max(total_delitos_actual)],
                  colonia_hecho = ifelse(
                        sum(total_delitos_actual == max(total_delitos_actual)) > 1,
                        min(colonia_hecho[total_delitos_actual == max(total_delitos_actual)]), # En caso de empate, alfabéticamente menor
                        colonia_hecho
                  )
            ) %>%
            ungroup()
      
      
      
      cuadrantes_totales_combinado <- cuadrantes_totales_combinado %>%
            group_by(Nomenclatu) %>%
            summarize(
                  alcaldia_hecho = first(alcaldia_hecho), # Tomar el primer valor como representativo
                  nombre_sec = first(nombre_sec), # Tomar el primer valor como representativo
                  colonia_hecho = first(colonia_hecho), # Tomar el primer valor como representativo
                  total_delitos_anterior = sum(total_delitos_anterior, na.rm = TRUE), # Sumar total_delitos_anterior
                  total_delitos_actual = sum(total_delitos_actual, na.rm = TRUE), # Sumar total_delitos_actual
                  categoria_delito = "DELITOS" # Nueva categoría
            ) %>%
            bind_rows(cuadrantes_totales_combinado) %>% # Agregar los nuevos renglones al dataframe original
            arrange(Nomenclatu, desc(categoria_delito == "DELITOS")) %>% # Ordenar para que DELITOS quede al principio
            ungroup()
      
      cuadrantes_totales_combinado <- cuadrantes_totales_combinado %>%
            group_by(Nomenclatu) %>%
            mutate(
                  # Calcular total_delitos solo para los registros donde categoria_delito es "DELITOS"
                  total_delitos = max(
                        if_else(categoria_delito == "DELITOS", total_delitos_actual, NA_real_),
                        na.rm = TRUE
                  )
            ) %>%
            ungroup() %>%
            # Ordenar primero por Nomenclatu, luego por "DELITOS" primero y después por total_delitos_actual
            arrange(
                  Nomenclatu,
                  if_else(categoria_delito == "DELITOS", 1, 2)
            )
      
      agrupacion_final_cuadrantes <- cuadrantes_totales_combinado %>% 
            arrange(desc(total_delitos)) %>% 
            mutate(cuadrante_colonia = paste0(Nomenclatu, " (",colonia_hecho,")"),
                   variacion_actual = variacion(total_delitos_actual, total_delitos_anterior)) %>% 
        dplyr::select(
                  cuadrante_colonia, 
                  Sector = nombre_sec, 
                  alcaldia_hechos = alcaldia_hecho, 
                  categoria_delito, 
                  total_anterior = total_delitos_anterior , 
                  total_actual = total_delitos_actual, 
                  variacion_actual
            )%>% 
            filter(categoria_delito != 'DELITOS') %>% 
            slice_head(n = 50)
      
      
      
      return(cuadrantes_totales_combinado)
      
      
      
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

agrupacion_func <- function(semanal_cuadrante) {
      
      
      # Paso 1: Filtrar y preparar los datos iniciales
      semanal_cuadrante <- semanal_cuadrante %>%
            mutate(
                  anio = year(fecha_inicio),
                  mes = month(fecha_inicio),
                  numero_semana = isoweek(fecha_inicio),
                  periodo = case_when(
                        numero_semana == semana_anterior ~ "Anterior",
                        numero_semana == semana_actual ~ "Actual"
                  )
            ) %>%
            filter(anio == 2024, !is.na(periodo)) %>%
            mutate(cantidad = 1) %>%
            filter(
                  !is.na(Nomenclatu),            # Eliminar nulos en Cuadrante
                  !is.na(nombre_sec),            # Eliminar nulos en Sector
                  nombre_sec != "Sin Sector"     # Eliminar registros con "Sin Sector"
            )
      
      
      
      # Paso 2: Calcular totales de delitos por cuadrante
      cuadrantes_totales <- semanal_cuadrante %>%
            filter(periodo == "Actual") %>%
            group_by(Nomenclatu) %>%
            summarise(total_delitos = sum(cantidad, na.rm = TRUE), .groups = "drop") %>%
            arrange(desc(total_delitos))
      
      # Paso 3: Obtener el cuarto lugar y filtrar todos los cuadrantes hasta que cambie
      cuarto_lugar <- cuadrantes_totales %>%
            slice(4) %>%
            pull(total_delitos)
      
      # Filtrar los cuadrantes hasta que el total_delitos cambie
      top_4_cuadrantes <- cuadrantes_totales %>%
            filter(total_delitos >= cuarto_lugar) %>%
            arrange(desc(total_delitos))
      
      # Paso 3: Obtener los 4 cuadrantes con más delitos
      #top_4_cuadrantes <- cuadrantes_totales %>%
      #     slice_max(total_delitos, n = 4, with_ties = TRUE)
      
      # Paso 4: Datos dinámicos de los 4 cuadrantes principales
      top_4_dinamico <- semanal_cuadrante %>%
            filter(periodo == "Actual", Nomenclatu %in% top_4_cuadrantes$Nomenclatu) %>%
            select(
                  Sector = nombre_sec,
                  Cuadrante = Nomenclatu,
                  alcaldia_hechos = alcaldia_hecho,
                  Colonia = colonia_hecho,
                  cantidad
            ) %>%
            arrange(Cuadrante, desc(cantidad)) %>%
            mutate(key_cuadrante_colonia = paste0(Cuadrante, "_", Colonia))
      
      lista_top4 <- top_4_dinamico$key_cuadrante_colonia %>% unique()
      
      
      
      
      
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
                  Nomenclatu = replace_na(nombre_sec, "Sin información"),
                  alcaldia_hecho = replace_na(alcaldia_hecho, "Sin información"),
                  Colonia = replace_na(colonia_hecho, "Sin información")
            ) %>%
            arrange(desc(cantidad)) %>%
            select(Sector, Cuadrante = Nomenclatu, alcaldia_hecho, Colonia, total = cantidad, key_cuadrante_colonia)
      
      unir_cuadrantes <- top_4_dinamico %>%
            mutate(key_cuadrante_colonia = paste0(Cuadrante, "_", Colonia)) %>%
            full_join(
                  top_cuadrantes_anterior_filtrado %>%
                        mutate(key_cuadrante_colonia = paste0(Cuadrante, "_", Colonia)) %>%
                        select(key_cuadrante_colonia, total_anterior = total), 
                  by = "key_cuadrante_colonia"
            ) %>%
            mutate(categoria_delito = 'DELITOS') %>%
            filter(
                  !is.na(Sector) & str_trim(Sector) != "",
                  !is.na(Colonia) & str_trim(Colonia) != "",
                  !is.na(alcaldia_hechos) & str_trim(alcaldia_hechos) != "",
                  !is.na(Cuadrante) & str_trim(Cuadrante) != ""
            ) %>%
            group_by(Sector, alcaldia_hechos, Cuadrante, Colonia, categoria_delito) %>%
            summarise(
                  total_actual = sum(cantidad, na.rm = TRUE),
                  total_anterior = sum(total_anterior, na.rm = TRUE),
                  .groups = "drop"
            ) %>%
            # Ahora sumar por Cuadrante y conservar las demás variables
            group_by(Cuadrante) %>%
            mutate(
                  total_actual_por_cuadrante = sum(total_actual, na.rm = TRUE),
                  total_anterior_por_cuadrante = sum(total_anterior, na.rm = TRUE)
            ) %>%
            ungroup() %>%
            # Ordenar por total_actual descendente y Cuadrante
            arrange(Cuadrante, desc(total_actual)) %>%
            # Dejar solo el primer registro por Cuadrante (el que tiene el total_actual más alto)
            distinct(Cuadrante, .keep_all = TRUE) %>% 
            select( total = total_actual_por_cuadrante, everything(), -total_actual, -total_anterior, -total_anterior_por_cuadrante )
      
      
      
      
      # Calcular top dinámico
      resultado_top <- unir_cuadrantes %>%
            arrange(desc(total)) %>%
            slice(1:16) %>% 
            select(Cuadrante, Sector, alcaldia_hechos, Colonia, total) %>%
            mutate(total_global = sum(total),
                   porcentaje = round((total / total_global) * 100, 1),
                   etiqueta = paste0(Cuadrante, " \n\n", Colonia, "\n\n ", total))
      
      # Encontrar el valor mínimo del top 3
      valor_top3 <- resultado_top$total[3]
      
      # Filtrar registros hasta que el valor deje de coincidir con el tercer lugar
      resultado_top <- resultado_top %>%
            filter(total >= valor_top3)
      
      return(resultado_top)
}

treemap_cuadrante <- function(resultado_top) {
      if (nrow(resultado_top) == 0) stop("El objeto resultado_top no contiene datos.")
      
      palette <- colorRampPalette(colors = c("#9f2241", "#de7aaf"))
      colores <- palette(nrow(resultado_top))
      
      interseccion_aux11 <- resultado_top %>% 
            ungroup() %>% 
            mutate(color = colores[1:nrow(resultado_top)])
      treemap <- ggplot(data = interseccion_aux11,
                        aes(area = total, fill = color, label = etiqueta)) +
            geom_treemap(colour = "white", start = "topleft") +
            geom_treemap_text(colour = "white",
                              place = "centre",
                              size = 10,
                              family = "Cabin",
                              fontface = "bold",
                              grow = TRUE,
                              reflow = TRUE,
                              padding.x = grid::unit(1, "mm"),
                              padding.y = grid::unit(1, "mm"),
                              angle = 0,
                              start = "topleft",
                              lineheight = 0.5) +
            scale_fill_manual(values = interseccion_aux11$color) +
            theme(text = element_text(family = "Cabin", size = 5, face = "bold"),
                  strip.background = element_blank(),
                  legend.position = "none",
                  legend.title = element_blank(),
                  panel.spacing = unit(5, "lines")) +
            theme(aspect.ratio = 1)
      
      return(treemap)
}


## Función para eliminar los archvios de un directorio  (MUCHO CUIDADO)
eliminar_archivos <- function(directorio) {
      # Verifica si el directorio existe
      if (!dir.exists(directorio)) {
            stop("El directorio especificado no existe.")
      }
      
      # Obtiene la lista de archivos en el directorio
      archivos <- list.files(directorio, full.names = TRUE)
      
      # Elimina los archivos
      sapply(archivos, unlink)
      
      cat("Todos los archivos en el directorio han sido eliminados.\n")
}

# Ejemplo de uso:
# eliminar_archivos("ruta/al/directorio")
