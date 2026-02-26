#' 1. Rellenar sumideros
#'
#' \code{fill_sinks} procesa un Modelo Digital de Elevaciones (DEM)
#' para eliminar imperfecciones y hoyos, evitando que el agua se estanque
#'
#' @param dem un objeto SpatRaster (elevacion)
#' @return un objeto SpatRaster con los sumideros rellenados
#' @export
#' @importFrom terra rast
#' @importFrom terra writeRaster
#' @importFrom whitebox wbt_fill_depressions

fill_sinks <- function(dem) {
  obj_in <- tempfile(fileext = ".tif")
  obj_out <- tempfile(fileext = ".tif")

  writeRaster(dem, obj_in, overwrite = TRUE)
  wbt_fill_depressions(dem = obj_in, output = obj_out)

  return(rast(obj_out))
}

#' 2. Direccion de flujo
#'
#' \code{flow_direct} calcula hacia cual de los 8 píxeles vecinos fluira el agua por gravedad
#'
#' @param dem_fill un objeto SpatRaster sin sumideros, resultado de la funcion 'Rellenar sumideros'
#' @return un objeto SpatRaster con códigos de direccion
#' @export
#' @importFrom terra terrain

flow_direct <- function(dem_fill) {
  terrain(dem_fill, v = "flowdir")
}

#' 3. Acumulacion de flujo
#'
#' \code{flow_accumul} calcula cuantas celdas vierten agua en cada celda individual.
#'
#' @param dir_flow un raster de direccion de flujo, resultado de la funcion 'Direccion de flujo'
#' @return un objeto SpatRaster con el conteo de celdas acumuladas
#' @export
#' @importFrom terra flowAccumulation

flow_accumul <- function(dir_flow) {
  flowAccumulation(dir_flow)
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

threshold <- function(acu_flow, umbral = 5000) {
  ifel(acu_flow > umbral, 1, NA)
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

rios_vect <- function(raster_rios) {
  rios_poly <- as.polygons(raster_rios)

  rios_sf <- st_as_sf(rios_poly)

  rios_dem <- st_cast(rios_sf, "MULTILINESTRING")

  return(rios_dem)
}
