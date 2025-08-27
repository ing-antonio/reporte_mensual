# Repositorio para generar reportes mensuales
![Estatus](https://img.shields.io/badge/Estatus-desarrollo-yellow)

##  Introducción

Este repositorio contiene scripts en R que permiten generar las tablas y gráficas necesarias para la elaboración del reporte mensual de incidencia delictiva de la Ciudad de México. La presentación final se construye a partir de un archivo .py, que integra estos insumos de manera automatizada.

El objetivo principal es optimizar el flujo de trabajo, automatizando todo el proceso: desde la consulta de datos hasta la generación de productos visuales y la presentación final.


##  Requerimientos técnicos

### Requisitos generales
- [R (> 4.0)](https://www.r-project.org/)
- [Python 3.8+](https://www.python.org/downloads/) (si usas Anaconda también funciona)
- Conexión VPN activa a la red de Vallejo (ADIP)
- Credenciales de acceso a la base de datos PostgreSQL
- IDE recomendado: [RStudio](https://www.rstudio.com/products/rstudio/)
- [Here] para uso de rutas relativas

### Paquetes de R

El repositorio utiliza el paquete `pacman` para cargar automáticamente todos los paquetes necesarios. Si no lo tienes, instálalo primero:
```r
install.packages("pacman")
```

Los scripts utilizan las librerías:
```r
pacman::p_load(
  here, showtext, sysfonts, dplyr, lubridate, DBI, RPostgreSQL,  janitor, gt, stringr, purrr, webshot2, ggplot2, ggtext, patchwork,  tidyr, scales, yaml, glue, readr, highcharter, htmlwidgets,  sf, leaflet, paletteer, leaflegend, webshot
)
```

No todos los scripts requieren cargar la totalidad de las librerías listadas anteriormente; las dependencias específicas pueden variar según el tipo de procesamiento y las salidas que genera cada script.

### Paquetes de python:
El script `Crear_ppt.py`, que genera la presentación final, requiere:

- pip install python-pptx pandas


##  Sobre los datos

Se utiliza como insumo la tabla `carpetas_directas_cc_cdmx` del esquema `dashboard_seguridad`. Para poder realizar las consultas y correr lo scripts es necesario:

- Tener acceso a la VPN de ADIP.
- Las credenciales deben ser gestionadas con la persona DBA responsable.
- Tener un archivo `config.yml`, y colocarlo en la carpeta `auxiliares/`. éste contempla los parámetros de conexión y rutas locales personalizadas. Debes generarlo tú misma/o y la estructura es la siguiente: 

```r
db_username: 
db_password: 
db_name: 
db_host: 
db_port:
```

##  Estructura del proyecto

Este proyecto está conformado por las siguientes carpetas:

```
reporte_mensual/
  ├── auxiliares/
  │   ├── pins/     # Carpeta con íconos de leyendas para elaboración de un mapa
  │   └── config.yml # Archivo con tus credenciales
  │   └── Plantilla1.ppt # Insumo del .py para generar presentación 
  │   └── funciones.R # Listado de funciones propias usadas en scripts de gráficas de barras
  │   └── funciones_mensual.R # Listado de funciones propias usadas en scripts de tablas y gráficas de línea
  │   └── verde.png # Imagen de flecha que indica variación a la baja de incidencia delictiva
  │   └── rojo.png # Imagen de flecha que indica variación a la alza de incidencia delictiva
  │   └── verde.png # Imagen de flecha que indica nula variación de incidencia delictiva
  │  
  ├── datos/
  │   ├── capas/   # Archivos shape de alcaldías y cuadrantes para la elaboración de mapa 
  │  
  ├── procesamiento/          # Scripts .R y .py que generan tablas, graficas, archivos Rdata, csv y ppt
  │ 
  ├── salidas/ # Imágenes de tablas, gráficos y tarjetas para construir las presentaciones y ppt mensual
  │   
  └── README.md
```

- `datos/`: guarda los archivos `.RData` o `.csv` que alimentan las tablas y gráficas. Son generados mediante los scripts mensualmente.
- `procesamiento/`: scripts en R que realizan la consulta, limpieza, análisis y generación de productos de salida.
- `auxiliares/`: incluye funciones auxiliares utilizadas por los scripts de `procesamiento`, el archivo `config.yml` con parámetros para realizar la conexión a la base de datos, y  el archivo base de la presentación en PowerPoint.
- `salidas/`:  guarda la presentación mensual completa y archivos de salida como tablas y gráficas que sirven de insumo para la ppt. 

##  Instrucciones de uso

1 . Abre una terminal o Git Bash, navega con cd hasta donde quieras trabajar y clona el repositorio:

 **Por SSH:**
  ```bash
git clone git@codigofuente.adip.cdmx.gob.mx:gobierno-digital/reportes-gabinete/mensual.git
```

**Por HTTPS:**
  ```bash
git clone https://codigofuente.adip.cdmx.gob.mx/gobierno-digital/reportes-gabinete/mensual.git
```

2. Asegúrate de tener el archivo config.yml dentro de la carpeta auxiliares/ con tus credenciales y rutas locales. 
(Este archivo no viene incluido en el repositorio por seguridad, debes generarlo tú misma/o).

3. Instala los paquetes de R y Python necesarios (ver sección "Requerimientos técnicos").

4. Desde terminal o RStudio, ejecuta el script `orquestador.R` Este orquestador:

	- Corre secuencialmente los scripts del 01 al 10 ubicados en procesamiento/, que generan tablas, gráficas y tarjetas informativas.
	- Luego se ejecuta el script `Crear_ppt.py`, que inserta todos los productos generados en una presentación de PowerPoint usando la `Plantilla1.pptx` base.
	- La presentación final se guarda automáticamente como: `salidas/presentacion_mensual.pptx`


## Acerca del proyecto

Siempre corre el `orquestador.R`, a menos que quieras sacar algún elemento específico de la presentación. A continuación se detalla que hace cada script.

**01_Tbl_absolutos.R:** Genera la tabla de conteo de delitos del último mes completo y el mes completo anterior a ese con sus respectivas variaciones 
 ![](https://raw.githubusercontent.com/AnaLauraBG/imagenesMuestra/main/tabla_variacion_mensual_abs.png
)

**02_Tbl_promedios.R:** Genera la tabla de promedio de delitos del último mes completo y el mes completo anterior a ese, con sus respectivas variaciones
 ![](https://raw.githubusercontent.com/AnaLauraBG/imagenesMuestra/main/tabla_variacion_mensual_prom.png
)
**03_Barras_alcaldias.R:** Crea un gráfico combinado, en donde se muestran los totales de delitos de Alto Impacto por Alcaldía.
![](https://raw.githubusercontent.com/AnaLauraBG/imagenesMuestra/main/graficas_combinadas.png)

**04_Tbl_acumulados.R:** Este script trae el código para generar la tabla de la diapositiva 3 (Tbl_Acumulados) 

Genera una tabla de conteos acumulados de enero al último mes completo que se tenga con sus respectivas variaciones porcentuales.
![](https://raw.githubusercontent.com/AnaLauraBG/imagenesMuestra/main/Tbl_acumulados.png)
Los acumulados son de los años 2019,2023,2024 y 2025. Pero pueden ser incluidos más años u ocultarse.

Además, también genera las mini tablas de acumulados por delitos (enero - último mes completo disponible). Ejemplo de mini tabla, delitos de Alto Impacto:

![Ejemplo de mini tabla, delitos de Alto Impacto](https://raw.githubusercontent.com/AnaLauraBG/imagenesMuestra/main/TblAcum_AI.png)

**IMPORTANTE**: Si ejecutas los scripts por separado **siempre corre el script 04 y 05** (aunque solo quieras sacar datos de alguno de ellos siempre corre ambos, comparten un csv y condiciones al momento de generar la ppt)

- El script 04 está dividido en 3 secciones(💡Las ubicarás porque tienen simbolos de foquitos 💡): **a) Código para tabla de acumulados; b) Código para mini tablas de acumulados  y c) Código para mini tabla de delitos en el espacio público.** 

1. 💡**Código para tabla de acumulados**💡: Saca la tabla de la diapositiva 4

Todo el código está comentado, cada sección especifica qué se hace sin embargo puede haber requerimientos específicos. ¿qué hay que considerar aquí?
 
 - El script usa una función llamada `variaciones0` que se encuentra en la carpeta de `auxiliares/funciones_mensual` esta sirve para sacar las variaciones porcentuales entre periodos y redondea las operaciones porcentuales para no considerar **ningún decimal**. 
 - Si quieres que saque un decimal solo usa la función `variaciones` en su lugar (también existe en el script de `funciones_mensual`)
 - Si quieres que saque más decimales solo cambia: "%.1f" por "%.2f" (si deseas 2 decimales)
```r
 variaciones <- function(actual, previo) {
  ifelse(previo == 0, "100.0%", paste0(sprintf("%.1f", (actual - previo) / previo * 100), "%"))
}
```
 2. 💡**Código para las mini tablas**💡: Saca las tablas de acumulados (enero - al último mes completo) que aparecen en las diapositivas 5,12,17,21. ¿qué hay que considerar aqui?

 - La lista `delitos_deseados` te va a ayudar: Como su nombre lo indica, es el listado de los delitos de los que necesitas sacar tablas, tarjetas con variaciones y flechas de variación se ubica en la línea 548, se llama `delitos_deseados`:
```r
delitos_deseados <- c(
  "Alto Impacto",
  "Homicidio doloso",
  "Homicidio doloso (víctimas)",
  "Lesiones dolosas por disparo de arma de fuego",
  "Robo de vehículo con y sin violencia"
  # Agrega más según lo necesites
)
```
- Debajo de ese  bloque de código se llama a la función  `generar_mini_tabla_delito` que comienza desde la línea 390, esa función recorre la lista de delitos previos y toma el data frame que se generó desde el apartado de 💡**Tabla de acumulados**💡 (primera sección). 

- Hay una sección de abreviaciones que se utiliza para que los nombres de las tablas no sean tan largos. La lógca de las abreviaciones es colocar sus iniciales en mayúsculas (ej: Alto Impacto -> AI)
```r
abreviaciones <- c(
    "Homicidio doloso" = "HD",
    "Homicidio doloso (víctimas)" = "HDV",
    "Lesiones dolosas por disparo de arma de fuego" = "LD",
    "Robo de vehículo con y sin violencia" = "RV",
    "Alto Impacto" = "AI"
    # agrega más si los necesitas
  )
  ```
- Debajo encontrarás también la función de `generar_tarjeta_variacion`, la cual saca imágenes con las variaciones porcentuales que hay entre el periodo a analizar: enero- ultimo mes completo disponible del presente año vs enero- ultimo mes completo disponible del 2019  y enero- ultimo mes completo disponible del presente año vs enero- ultimo mes completo disponible del 2024. ¿Esa función qué información contiene?

  - **porcentaje** = Es la variación del periodo que nos interesa
  - **anio_base** = Puede ser 2019 o 2024
  - **mes corte** = Es la fecha que corresponde al primer día del mes completo que existe
  - **nombre_delito** = Se obtiene del ciclo que va recorriendo el listado de `delitos_deseados`
  - **tipo_perido** = puede ser "acumulado" o "mensual". En este script es "acumulado" pero en el script 05 es "mensual", ambos scripts usan la misma función-
  - **tipo_tarjeta** = Puede ser "G" o "T", G significa que es una tarjeta para el apartado de gráficas, por lo tanto es una tarjeta gris, si es T es para las diapositivas que muestran tablas y la tarjeta es amarilla y trae más texto

 Ejemplo de función:

```r
   variacion19 <- df_filtrado$var_2025_vs_2019
  if (length(variacion19) > 0 && !is.na(variacion19)) {
    generar_tarjeta_variacion(
      porcentaje = variacion19,
      anio_base = 2019,
      mes_corte = fecha_corte,
      nombre_delito = nombre_delito,
      tipo_periodo = "acumulado",
      tipo_tarjeta = "T"
    )
  }
  ```

- Posteriormente encontrarás una sección donde hay condiciones para poner los títulos y las flechas de colores que aprecen en la ppt (verde para aumento, roja para disminución o signo igual si no hubo cambio). Esos títulos se van a guardar en un csv en la carpeta de `auxiliares`. El csv se llama `titulos_tarjetas.csv`

```r
interpretar_titulo <- function(valor) {
    if (str_detect(valor, "-")) {
      return("Continúa la disminución")
    } else if (valor == "0.0%" || valor == "0%") {
      return("No se observaron cambios")
    } else if (!is.na(valor) && valor != "") {
      return("Se observa un incremento")
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
 ```

**05_Tbl_comparativo_mensual:** El código es similar al anterior en estructuray funciones, solo que los cortes de tiempo en este caso solo consideran los registros del último mes completo y lo comparan con los registros de ese mismo mes pero de años previos. 

El objetivo es generar mini tablas por cada uno de los delitos que resulten de interés, actualmente son: Alto Impacto, Homicidio doloso, Lesiones dolosas por disparo de arma de fuego y Robo de vehículo con y sin violencia.

Un ejemplo de las tablas generadas es el siguiente: 

![](https://raw.githubusercontent.com/AnaLauraBG/imagenesMuestra/main/TblMes_AI.png)

**06_Graf_lineas_acum:** Este script genera gráficas de líneas de promedios acumulados de enero al último mes completo existente. Para hacerlo se suman los registros de cada delito en ese intervalo de tiempo y se divide entre el número de días transcurridos en el periodo. 

![](https://raw.githubusercontent.com/AnaLauraBG/imagenesMuestra/main/grafico_acum_AI.png)

Para distinguir qué gráfica corresponde a cada delito solo identifica el sufijo con el que fue guardado el archivo. Todos siguen la misma lógica de contemplar como nombre la abreviación del delito, por lo tanto aquellos que terminen con AI se referirán a Alto Impato; los que terminen con HD se referirán a Homicidio Doloso; RV a Robo de Vehículo con y sin violencia

**07_Graf_lineas_mes:** Este script es igual al anterior, salvo que en este caso se generan gráficas de líneas de promedios acumulados *solo del último mes completo existente*. Por ejemplo, si el último mes completo que  existe en Julio, se muestran los promedios de todos los julios desde 2019 a la fecha. Para hacerlo se suman los registros de cada delito en ese intervalo de tiempo y se divide entre el número de días transcurridos en el periodo. 

**08_barras_alc_sec_cuad:** Este sript **genera tres gráficas dinámicas** que necesitan ser consultadas en un link que se adjunta en los títulos de las mismas. Para ello el script genera como salidas una imagen en formato .png que es la que se muestra en la ppt y además se generan archivos html que es necesario cargar en un repositorio público para que posteriormente puedan ser consultados con ayuda de un servicio (como raw.githack) que permita alojar el archivo .html

- En el caso de la gráfica de alcaldís, cada mes tienes que cambiar las leyendas del gráfico colocando los meses correspondientes
![](https://raw.githubusercontent.com/AnaLauraBG/imagenesMuestra/main/grafico_alcaldias.png)

- Gráfica de sectores
![](https://raw.githubusercontent.com/AnaLauraBG/imagenesMuestra/main/grafico_sectores.png)

-Gráfica de cuadrantes
![](https://raw.githubusercontent.com/AnaLauraBG/imagenesMuestra/main/grafico_cuadrantes.png)


**09_barras_homicidios_lesiones_alcaldia:** Sigue la misma lógica que el script 08, solo que en este caso las consultas son por alcaldía en lugar de cuadrantes. 

![](https://raw.githubusercontent.com/AnaLauraBG/imagenesMuestra/main/barras_homi_les_alc.png)

**10_mapa:** Genera un mapa en leaflet con los homicidios y lesiones dolosas. 
![](https://raw.githack.com/AnaLauraBG/imagenesMuestra/main/mapa_interactivo_homicidio_lesiones.png)

**Crear_ppt:** Puedes abrir el archivo .ipynb en un notebok con ayuda de Anaconda o correr el .py 
Este archivo genera la ppt tomando como insumo la `Plantilla1.pptx` que se encuentra en la carpeta de /auxiliares. 

¿Qué hay que considerar en este script?

- Existen 3 funciones básicas: **una** que inserta todas las diapostivas que **solo llevan una imagen**; **otra** que **inserta tres imágenes** (que corresponde a las diapositivas que contienen gráficas) y **otra** función que **inserta 4 imágenes** (que corresponde a las diapositivas que muestran las tablas de acumulados y la del último mes completo que se está evaluando)
-  En el caso de las diapositivas que llevan 4 imágenes se coloca además una flecha que indica si la variación mensual fue positiva (color rojo); negativa (color verde) o nula (ícono gris con un signo de igual)
- Los títulos que se agregan en las diapositivas y las imágenes correspondientes a las variaciones (flechas verdes, rojas o signos grises) se obtienen del archivo .csv que se construye en el script 04. 


## Soporte Técnico

- Óscar Hernández (ohernandezm@cdmx.gob.mx)
- Ana Laura Bernal (abernalg@cdmx.gob.mx )
- Víctor Segoviano (vsegovianog@cdmx.gob.mx)
- Jesús Andrés Barranco (jbarrancop@cdmx.gob.mx )

## Líder de Proyecto (2025)

- Isaías Morales López (imoralesl@cdmx.gob.mx)
