
# Cargar el csv con los datos

#     url
data <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/casos-covid-19/casos_covid19.csv", TRUE)

#     local (cambiar directorio)
data <- read.csv("/Users/nico/Downloads/casos_covid19.csv", TRUE)


# Castear las fechas

dateFromString <- function(string) {
  date <- as.Date(gsub(":00:00:00.000000", "", string), "%d%b%Y")
  return(date)
}

data$fecha_apertura_snvs <- dateFromString(data$fecha_apertura_snvs)
data$fecha_toma_muestra <- dateFromString(data$fecha_toma_muestra)
data$fecha_clasificacion <- dateFromString(data$fecha_clasificacion)
data$fecha_fallecimiento <- dateFromString(data$fecha_fallecimiento)
data$fecha_alta <- dateFromString(data$fecha_alta)


# Criterio de fecha (fecha de isopado o de clasificación)

fechaCriterio <- data$fecha_toma_muestra
fechaCriterio <- data$fecha_clasificacion
fechaCriterio <- data$fecha_apertura_snvs


# Plots por barrios, comunas, edades

barrio <- function(unBarrio) {
  table <- table(fechaCriterio [data$barrio == unBarrio] [data$clasificacion == "confirmado"] [data$fecha_apertura_snvs > "2020-03-01"] )
  plot(table, col = "#18588E")
}

comuna <- function(unaComuna) {
  table <- table(fechaCriterio [data$comuna == unaComuna] [data$clasificacion == "confirmado"] )
  plot(table)
}

barrioEdades <- function(unBarrio, edadMin, edadMax) {
  table <- table(fechaCriterio
                 [data$barrio == unBarrio]
                 [data$clasificacion == "confirmado"]
                 [data$edad >= edadMin]
                 [data$edad <= edadMax]
  )
  plot(table)
}

comunaEdades <- function(unaComuna, edadMin, edadMax) {
  table <- table(fechaCriterio
                 [data$comuna == unaComuna]
                 [data$clasificacion == "confirmado"]
                 [data$edad >= edadMin]
                 [data$edad <= edadMax]
  )
  plot(table)
}

edades <- function(edadMin, edadMax) {
  table <- table(fechaCriterio
                 [data$clasificacion == "confirmado"]
                 [data$edad >= edadMin]
                 [data$edad <= edadMax]
  )
  plot(table)
}

edad <- function(unaEdad) {
  table <- table(fechaCriterio
                 [data$clasificacion == "confirmado"]
                 [data$edad == unaEdad]
  )
  plot(table)
}
