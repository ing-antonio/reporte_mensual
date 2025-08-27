library(here)

## Cargar las funciones
here::i_am( "orquestador.R")

#lista_archivos <- list.files( here() , full.names = TRUE)

scripts <- c("01_Tbl_absolutos.R" ,
             "02_Tbl_promedios.R",
             '03_Barras_alcaldias.R', 
             '04_Tbl_acumulados.R',
             '05_Tbl_comparativo_mensual.R',
             '06_Graf_lineas_acum.R',
             '07_Graf_lineas_mes.R',
             '08_barras_alc_sec_cuad.R',
             '09_barras_homicidios_lesiones_alcaldia.R',
             '10_mapa.R')
#path_archivos <- './procesamiento'

# Esto sirve para el debuggeo en local o desde bash
for (script in scripts) {
  ruta_script <- file.path( script)
  cat("Ejecutando", ruta_script, "\n")
  
  result <- system2("Rscript", ruta_script, stdout = TRUE, stderr = TRUE)
  cat("Resultado de", script, ":\n", paste(result, collapse = "\n"), "\n")
}


cat("▶ Ejecutando script Python para generar presentación...\n")
result_py <- system2("python", args = "Crear_ppt.py", stdout = TRUE, stderr = TRUE)
cat("Resultado del script Python:\n", paste(result_py, collapse = "\n"), "\n")
