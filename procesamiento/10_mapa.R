# Carga de bibliotecas

pacman::p_load(RPostgreSQL, 
               sf, 
               lubridate, 
               dplyr, 
               leaflet, 
               paletteer, 
               leaflegend, 
               htmlwidgets,
               webshot,
               here,
               stringr)


# Establecer conexión

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

## Cargar capas
alcaldias <- st_read( here('datos/capas',"limite_de_las_alcaldias.shp") )
colonias <- st_read(here('datos/capas',"cuadrante_colonia.shp") )

# Reproyección de capa
colonias <- colonias %>%  
              st_transform(4326) %>% 
              select(alcaldia=Deleg, colonia=X_NOMUT, Sector, Cuadrante=Nomenclatu, geometry)

sf_use_s2(FALSE)

##Mensual

fecha_corte<-floor_date(today(), unit="months")-1
fecha_aux<-floor_date(fecha_corte, unit="months")

### Consulta

fgj <- dbGetQuery(
  conn, paste0(
    "SELECT fecha_inicio,
      categoria_delito, delito, alcaldia_hecho as alcaldia_hechos, latitud as lat, longitud as lon 
     FROM reportes_seguridad.reporte_semanal 
    WHERE 
    fecha_inicio <= '",fecha_corte+1,"'
    and fecha_inicio >= '",fecha_aux,"'
    and latitud is not null 
    and longitud is not null
    and latitud!='Inf'
    and longitud!='Inf'")) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_intersection(colonias) %>% 
  mutate(tooltip=paste0("<b>","Categoria delito: ","</b>", str_to_sentence(categoria_delito),"<br>",
                        "<b>","Delito: ","</b>", str_to_sentence(delito),"<br>",
                        "<b>","AlcaldC-a: ","</b>", str_to_sentence(alcaldia),"<br>",
                        "<b>","Sector: ","</b>", str_to_sentence(Sector),"<br>",
                        "<b>","Colonia: ","</b>", str_to_sentence(colonia),"<br>",
                        "<b>","Cuadrante: ","</b>", Cuadrante,"<br>",
                        "<b>","Fecha inicio: ","</b>", fecha_inicio,"<br>"
  )) %>% filter(categoria_delito!="DELITO DE BAJO IMPACTO") %>% 
  mutate(categoria_delito=case_when(
    categoria_delito=="ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA" ~ delito,
    categoria_delito=="ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA" ~ delito,
    categoria_delito=="ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA" ~ delito,
    categoria_delito=="ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA" ~ delito,
    categoria_delito=="ROBO DE VEHÍCULO CON Y SIN VIOLENCIA" ~ delito,
    categoria_delito=="ROBO A REPARTIDOR CON Y SIN VIOLENCIA" ~ delito,
    T ~ categoria_delito
  )) %>% 
  mutate(categoria_delito=case_when(
    (grepl("VEHICULO", categoria_delito ) & grepl("CON VIOLENCIA|C/V", categoria_delito ))  ~ "ROBO DE VEHÍCULO CON VIOLENCIA",
    (grepl("VEHICULO", categoria_delito ) & grepl("SIN VIOLENCIA|S/V", categoria_delito ))  ~ "ROBO DE VEHÍCULO SIN VIOLENCIA",
    (grepl("MOTOCICLETA", categoria_delito ) & grepl("CON VIOLENCIA", categoria_delito ))  ~ "ROBO DE MOTOCICLETA CON VIOLENCIA",
    (grepl("MOTOCICLETA", categoria_delito ) & grepl("SIN VIOLENCIA", categoria_delito ))  ~ "ROBO DE MOTOCICLETA SIN VIOLENCIA",
    (grepl("PESERO", categoria_delito ) & grepl("SIN VIOLENCIA", categoria_delito ))  ~ "ROBO A PASAJERO A BORDO DE MICROBUS",
    (grepl("PESERO", categoria_delito ) & grepl("CON VIOLENCIA", categoria_delito ))  ~ "ROBO A PASAJERO A BORDO DE MICROBUS",
    (grepl("TRANSEUNTE", categoria_delito ) & grepl("CON VIOLENCIA", categoria_delito ))  ~ "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON VIOLENCIA",
    (grepl("TRANSEUNTE", categoria_delito ) & grepl("SIN VIOLENCIA", categoria_delito ))  ~ "ROBO A TRANSEUNTE EN VÍA PÚBLICA SIN VIOLENCIA",
    categoria_delito=="ROBO A PASAJERO A BORDO DE METRO SIN VIOLENCIA"  ~ "ROBO A PASAJERO A BORDO DEL METRO SIN VIOLENCIA",
    categoria_delito=="ROBO A PASAJERO A BORDO DE METRO CON VIOLENCIA"  ~ "ROBO A PASAJERO A BORDO DEL METRO CON VIOLENCIA",
    T ~ categoria_delito
  )) %>% 
  select(categoria_delito, geometry, tooltip, Cuadrante, colonia, alcaldia, Sector) %>% 
  filter(categoria_delito != 'ROBO DE MERCANCIA A TRANSPORTISTA C/V') %>% 
  filter(categoria_delito != 'ROBO DE MAQUINARIA SIN VIOLENCIA')  
      

## Reasignación de DF

fgj_aux<-fgj

# %>% filter(alto_bajo %in% tipo_delito & categoria_delito %in% delito, )

#alcaldias <- st_read("G:/.shortcut-targets-by-id/1o3RW9XtzyaaBxBvSYFPTHgER_wJ6O_VR/Analisis/Gabinete de Seguridad/20241112_Incidencia_Delictiva/02_scripts/Alcaldias/limite_de_las_alcaldias.shp")
#colonias <- st_read("G:/.shortcut-targets-by-id/1o3RW9XtzyaaBxBvSYFPTHgER_wJ6O_VR/Analisis/Gabinete de Seguridad/20241112_Incidencia_Delictiva/02_scripts/Alcaldias/cuadrante_colonia.shp") %>% 
#st_transform(4326) %>% select(alcaldia=Deleg, colonia=X_NOMUT, Sector, Cuadrante=Nomenclatu, geometry)
#sf_use_s2(FALSE)

# Se simplifican geometrías para reducr el peso

simplified_polygon <- st_simplify(alcaldias, dTolerance = 20)

## Se establece paleta de colores

Colores<-data.frame(delito=c("ROBO A PASAJERO A BORDO DE TAXI CON VIOLENCIA", 
                             "VIOLACIÓN", 
                             "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON VIOLENCIA", 
                             "ROBO A TRANSEUNTE EN VÍA PÚBLICA SIN VIOLENCIA", 
                             "HECHO NO DELICTIVO", 
                             "ROBO DE VEHÍCULO CON VIOLENCIA", 
                             "ROBO DE VEHÍCULO SIN VIOLENCIA", 
                             "ROBO DE MOTOCICLETA CON VIOLENCIA", 
                             "ROBO DE MOTOCICLETA SIN VIOLENCIA", 
                             "ROBO A NEGOCIO CON VIOLENCIA", 
                             "HOMICIDIO DOLOSO",
                             "ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA", 
                             "ROBO A CASA HABITACIÓN CON VIOLENCIA", 
                             "LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO", 
                             "ROBO A REPARTIDOR CON VIOLENCIA", 
                             "ROBO A REPARTIDOR SIN VIOLENCIA", 
                             "ROBO A PASAJERO A BORDO DE MICROBUS",
                             "ROBO A PASAJERO A BORDO DEL METRO CON VIOLENCIA", 
                             "ROBO A PASAJERO A BORDO DEL METRO SIN VIOLENCIA", 
                             "SECUESTRO", 
                             "ROBO A TRANSPORTISTA CON VIOLENCIA",
                             "ROBO A TRANSPORTISTA SIN VIOLENCIA", 
                             NA)) %>% arrange(delito)

Colores_aux<-Colores %>% filter(delito %in% fgj_aux$categoria_delito)

# Reasignación de DF
fgj_colonia<-fgj_aux

# Se elimna la geometry?
st_geometry(fgj_colonia) <- NULL

# Agregados?
fgj_colonia <-fgj_colonia %>% 
  select(Cuadrante, categoria_delito) %>% 
  group_by(Cuadrante, categoria_delito) %>% 
  summarise(tot=n()) %>% 
  left_join(colonias, by="Cuadrante")

cuadrantes<-data.frame(tooltip=character(), Cuadrante=character(), tot=integer())
for(i in unique(fgj_colonia$Cuadrante)){
  # i<-unique(fgj_colonia$Cuadrante)[2]
  aux_colonia<-fgj_colonia %>% filter(Cuadrante==i) %>% arrange(desc(tot))
  texto<-paste0("<b>","AlcaldC-a: ","</b>",str_to_sentence(unique(aux_colonia$alcaldia)),"<br>",
                "<b>","Sector: ","</b>", str_to_sentence(unique(aux_colonia$Sector)),"<br>",
                "<b>","Colonia: ","</b>", str_to_sentence(unique(aux_colonia$colonia)),"<br>",
                "<b>","Cuadrante: ","</b>", unique(aux_colonia$Cuadrante),"<br>",
                "<b>","Total de carpetas: ","</b>", sum(aux_colonia$tot),"<br>")
  for(x in 1:length(aux_colonia$categoria_delito)){
    # aux_colonia$categoria_delito[[1]]
    texto<-paste0(texto,"<b>-",str_to_sentence( aux_colonia$categoria_delito[[x]]),":</b>", aux_colonia$tot[[x]],"<br>")
  }
  aux_texto<-data.frame(tooltip=texto, Cuadrante=unique(aux_colonia$Cuadrante), tot=sum(aux_colonia$tot))
  cuadrantes<-rbind(cuadrantes, aux_texto)
  
}

## Unión de datos
cuadrantes<- colonias %>% 
  left_join(cuadrantes, by="Cuadrante")  %>% 
  select(tooltip, geometry, tot) %>% 
  filter(!is.na(tooltip))

# Colores
paletteer_c("grDevices::YlOrRd", 5)
conpal <- colorNumeric(palette = "Reds", domain = c(min(cuadrantes$tot), max(cuadrantes$tot)))


# Se elabora el mapa de Leaflet

leyenda<-fgj_aux %>% select(categoria_delito) 

st_geometry(leyenda) <- NULL 

leyenda<-leyenda %>% unique() %>% arrange(categoria_delito) %>% pull()

backg <- htmltools::tags$style(".leaflet-container { background: #FFFFFF; }" )

## Objeto Leaflet

# Crear el mapa leaflet
icon_paths <- file.path(here("auxiliares/pins"), paste0(fgj_aux$categoria_delito, ".png"))

mapa <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group="Carto DB Positron") %>%
      setView( -99.15, 19.35, 10, zoom = 11 ) %>%
      addPolygons(data = cuadrantes, group = "Cuadrantes",
                  stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.4,
                  fillColor = ~conpal(cuadrantes$tot)) %>%
      addPolygons(data = colonias, weight = 0.5, fillOpacity = -1, color = 'black' )%>%
      addPolygons(data = alcaldias, weight = 2, fillOpacity = -1, color = 'black' )%>%
      addPolygons(data = cuadrantes, group = "Cuadrantes",
                  stroke = FALSE, smoothFactor = 0.2, fillOpacity = -1,
                  popup = paste0(cuadrantes$tooltip)) %>%
      addMarkers(data= fgj_aux, group = fgj_aux$categoria_delito, 
                 icon = makeIcon(
                   iconUrl= icon_paths,
                   iconWidth = 27, iconHeight = 28.5,
                       iconAnchorX = 0, iconAnchorY = 0) ,
                 popup  = paste0(fgj_aux$tooltip)) %>% 
      hideGroup(c(Colores_aux %>% filter(delito!="HOMICIDIO DOLOSO" & delito!="LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO") %>% select(delito) %>% pull())) %>%
      addLegend("topleft", pal = conpal, values = c(min(cuadrantes$tot), max(cuadrantes$tot)), 
                title = "Carpetas", opacity = 1) %>% 
      addLayersControl(overlayGroups = c(Colores_aux$delito, "Cuadrantes", "Carto DB Positron"),
                       options = layersControlOptions(collapsed = T),
                       position = 'bottomleft')%>%
      htmlwidgets::prependContent(backg)


# Guardar como HTML
saveWidget(mapa, here('salidas', "mapa_interactivo_homicidio_lesiones.html"))

# Capturar el HTML como PNG
webshot2::webshot(
  url = here('salidas/mapa_interactivo_homicidio_lesiones.html'),
  file = here('salidas/mapa_interactivo_homicidio_lesiones.png'),
  vwidth = 1600,
  vheight = 750
)

######## ENTIENDO QUE SE USA EL MAPA DE ARRIBA, NO ÉSTE #################
mapa <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group="Carto DB Positron") %>%
  setView( -99.15, 19.35, 10, zoom = 11 ) %>%
  addPolygons(data = cuadrantes, group = "Cuadrantes",
              stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.4,
              fillColor = ~conpal(cuadrantes$tot)) %>%
  addPolygons(data = colonias, weight = 0.5, fillOpacity = -1, color = 'black' )%>%
  addPolygons(data = alcaldias, weight = 2, fillOpacity = -1, color = 'black' )%>%
  addPolygons(data = cuadrantes, group = "Cuadrantes",
              stroke = FALSE, smoothFactor = 0.2, fillOpacity = -1,
              popup = paste0(cuadrantes$tooltip)) %>%
  addMarkers(data= fgj_aux, group = fgj_aux$categoria_delito, 
             icon = makeIcon(
               iconUrl= paste0( here('auxiliares', 'pins/'), fgj_aux$categoria_delito, ".png"),
               iconWidth = 27, iconHeight = 28.5,
               iconAnchorX = 0, iconAnchorY = 0) ,
             popup  = paste0(fgj_aux$tooltip)) %>% 
  hideGroup(c(Colores_aux %>% filter(delito!="HOMICIDIO DOLOSO" & delito!="LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO") %>% select(delito) %>% pull())) %>%
  addLegend("topleft", pal = conpal, values = c(min(cuadrantes$tot), max(cuadrantes$tot)), 
            title = "Carpetas", opacity = 1) %>% 
  addLegendImage(title="Tipo de delito",
                 images = paste0(here('auxiliares', 'pins/'), leyenda, ".png"),
                 labels =  paste0(str_to_sentence(leyenda)),
                 labelStyle = "font-size: 15px; vertical-align: middle;",
                 width = 35,
                 height = 37.5,
                 orientation = 'vertical',
                 position = 'topright')%>%
  addLayersControl(overlayGroups = c(Colores_aux$delito, "Cuadrantes", "Carto DB Positron"),
                   options = layersControlOptions(collapsed = T),
                   position = 'bottomleft')%>%
  htmlwidgets::prependContent(backg)

# Ver el mapa
mapa

# Guardar como HTML
saveWidget(mapa, 
           here('salidas' , "mapa_2_borrar.html") )

