#' 1. Rellenar huecos
#'
#' \code{fill_sinks} procesa un Modelo Digital de Elevaciones (DEM)
#' para eliminar imperfecciones y hoyos, evitando que el agua se estanque
#'
#' @param dem un objeto SpatRaster (elevacion)
#' @return un objeto SpatRaster con los sumideros rellenados
#' @export
#' @importFrom terra focal
#' @importFrom terra ifel
#' @examples
#' \dontrun{
#' dem <- rast("C:/Users/AlumnoMaster/dem.tif")
#' dem_fill <- fill_sinks(dem)
#' }

fill_sinks <- function(dem) {
  # Nota: usamos w = 3 porque es el estandar para conectividad de 8 vecinos, usa el que requiera
  min_vecinos <- focal(dem, w = 3, fun = "min", na.rm = TRUE)

  dem_fill <- ifel(dem < min_vecinos, min_vecinos, dem)

  return(dem_fill)
}

#' 2. Direccion de flujo
#'
#' \code{flow_direct} calcula hacia cual de los 8 píxeles vecinos fluira el agua por gravedad
#'
#' @param dem_fill un objeto SpatRaster sin sumideros, resultado de la funcion 'Rellenar sumideros'
#' @return un objeto SpatRaster con códigos de direccion
#' @export
#' @importFrom terra terrain
#' @examples
#' \dontrun{
#' dir_flow <- flow_direct(dem_fill)
#' }

flow_direct <- function(dem_fill) {
  dir_flow <- terrain(dem_fill, v = "flowdir")

  return(dir_flow)
}

#' 3. Acumulacion de flujo
#'
#' \code{flow_accumul} calcula cuantas celdas vierten agua en cada celda individual.
#'
#' @param dir_flow un raster de direccion de flujo, resultado de la funcion 'Direccion de flujo'
#' @return un objeto SpatRaster con el conteo de celdas acumuladas
#' @export
#' @importFrom terra flowAccumulation
#' @examples
#' \dontrun{
#' acu_flow <- flow_accumul(dir_flow)
#' }

flow_accumul <- function(dir_flow) {
  acu_flow <- flowAccumulation(dir_flow)

  return(acu_flow)
}

#' 4. Definir el umbral
#'
#' \code{threshold} filtra el raster para quedarse solo con los pixeles que superen un valor
#'
#' @param acu_flow un raster de acumulacion de flujo, resultado de la funcion 'Acumulacion de flujo'
#' @param umbral numerico. Valor minimo para considerar que es un rio (ej. 5000)
#' @return Un objeto SpatRaster binario (1 es rio, NA es no rio)
#' @export
#' @importFrom terra ifel
#' @examples
#' \dontrun{
#' raster_rios <- threshold(acu_flow, umbral = 500)
#' }

threshold <- function(acu_flow, umbral = 5000) {
  raster_rios <- ifel(acu_flow > umbral, 1, NA)

  return(raster_rios)
}

#' 5. Vectorizacion
#'
#' \code{rios_vect} convierte los pixeles filtrados en polilineas vectoriales
#'
#' @param raster_rios un raster binario de rios filtrados, resultado de la funcion 'Definir el umbral'
#' @return un objeto sf de tipo multilinestring
#' @export
#' @importFrom terra as.polygons
#' @importFrom sf st_as_sf
#' @importFrom sf st_cast
#' @examples
#' \dontrun{
#' rios_dem <- rios_vect(raster_rios)
#' }

rios_vect <- function(raster_rios) {
  rios_poly <- as.polygons(raster_rios)

  rios_sf <- st_as_sf(rios_poly)

  rios_dem <- st_cast(rios_sf, "MULTILINESTRING")

  return(rios_dem)
}
