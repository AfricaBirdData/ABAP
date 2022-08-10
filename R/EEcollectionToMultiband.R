#' Convert a GEE collection to multiband image
#'
#' @param collection Either a character string with the name of the collection
#' we want to use or a GEE collection produced with \code{ee$ImageCollection()}.
#' See \href{https://developers.google.com/earth-engine/datasets/catalog}{GEE catalog}.
#' @param dates A character vector with two elements c(start, end). Format must be
#' "yyyy-mm-dd".
#' @param band Select a specific band from the collection. Only one band from
#' the collection can be processed at a time.
#' @param group_type The type of grouping we want to perform. This has to be a
#' temporal grouping, either: 'day', 'week', 'month', or 'year'.
#' @param groups An integer vector with values for the group. e.g.
#' \code{2008:2019} will group data into years from 2008 to 2019.
#' @param reducer The summarizing function applied to each group. If reducer is
#' NULL, then the collection is transformed into a multiband image without
#' applying any reduction (each image goes to a band).
#' @param unmask GEE masks missing values, which means they are not used for
#' computing means, counts, etc. Sometimes we might want to avoid this behaviour
#' and use 0 instead of NA. If so, set unmask to TRUE.
#'
#' @return This function transforms an image collection made of a number of images,
#' each representing a variable at different times (e.g. NDVI measures in different
#' months) into a multiband image. Each band in the new image represent a different
#' time. The advantage of a multiband image over an image collection is that we can
#' annotate data with all the bands of an image on a single function call, saving
#' time and data traffic between our machine and GEE servers.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a multi-band image with mean NDVI for each year
#' multiband <- EEcollectionToMultiband(collection = "MODIS/006/MOD13A2",
#'                                      dates = c("2008-01-01", "2020-01-01"),
#'                                      band = "NDVI",                       # You can find what bands are available from GEE catalog
#'                                      group_type = "year",
#'                                      groups = 2008:2019,
#'                                      reducer = "mean",
#'                                      unmask = FALSE)
#'
#' Find mean (mean) NDVI for each pentad and year
#' ee_data <- ee$FeatureCollection(assetId)  # assetId must correspond to an asset in your GEE account
#' pentads_ndvi <- addVarEEimage(ee_data, multiband, "mean")
#'
#' }
EEcollectionToMultiband <- function(collection, dates, band,
                                    group_type, groups,
                                    reducer = NULL,
                                    unmask = FALSE){

  # Get image
  if(is.character(collection)){
    ee_layer <- rgee::ee$ImageCollection(collection)$
      select(band)$
      filterDate(dates[1], dates[2])
  } else if("ee.imagecollection.ImageCollection" %in% class(collection)){
    ee_layer <- collection$
      select(band)$
      filterDate(dates[1], dates[2])
  } else {
    stop("collection must be either a character string or a GEE image collection")
  }

  # Get nominal scale for the layer (native resolution) and projection
  ee_proj <- rgee::ee$Projection(ee_layer$first()$projection())

  # Remove missing values (this will depend on the layer)
  if(unmask){
    ee_layer <- ee_layer$map(
      rgee::ee_utils_pyfunc(
        function(image){
          return(image$unmask())
        }))
  }

  if(!is.null(reducer)){

  # Create a list of groups
  ee_groups <-  rgee::ee$List(groups)

  # Set filter
  filter <- paste0("ee_layer$filter(rgee::ee$Filter$calendarRange(m, m,'", group_type,"'))$", reducer,"()")

  # Group and reduce within groups by reducer
  byGroup <- rgee::ee$ImageCollection$fromImages(
    ee_groups$map(
      rgee::ee_utils_pyfunc(
        function(m){
          return(eval(parse(text = filter)))
        })))

  } else {

    byGroup <- ee_layer

  }

  # Transform into a multiband image and reproject
  stackCollection <- byGroup$toBands()$rename(as.character(groups))$
    reproject(ee_proj)

  return(stackCollection)

}
