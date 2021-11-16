#' Add variable from Google Earth Engine Collection
#'
#' @param ee_pentads A feature collection with the pentads we want to annotate.
#' We need to upload an sf object with the pentads to GEE.
#' @param collection Either a character string with the name of the collection
#' we want to use or a GEE collection produced with \code{ee$ImageCollection()}.
#' See \href{https://developers.google.com/earth-engine/datasets/catalog}{GEE catalog}.
#' @param dates A character vector with two elements c(start, end). Format must be
#' "yyyy-mm-dd".
#' @param temp_reducer A character string specifying the function to summarize
#' collection across time. It is common to use "mean", "sum" or "count", but
#' there are many others, see 'ee.Reducer' under Client Libraries at
#' \url{https://developers.google.com/earth-engine/apidocs}.
#' @param spt_reducer A character string specifying the function apply when
#' extracting values for each pentad. It is common to use "mean", "sum" or
#' "count", but there are many others, see 'ee.Reducer' under Client Libraries at
#' \url{https://developers.google.com/earth-engine/apidocs}.
#' @param bands Select specific bands from the image. If not specified, pentads
#' will be annotated with all bands (one column for each band).
#' @param unmask GEE masks missing values, which means they are not used for
#' computing means, counts, etc. Sometimes we might want to avoid this behaviour
#' and use 0 instead of NA. If so, set unmask to TRUE.
#'
#' @return
#' @export
#'
#' @examples
addVarEEcollection <- function(ee_pentads, collection, dates,
                               temp_reducer, spt_reducer,
                               bands = NULL, unmask = FALSE){

  # Get image
  if(is.character(collection)){
    ee_layer <- ee$ImageCollection(collection)$
      filterDate(dates[1], dates[2])
  } else if("ee.imagecollection.ImageCollection" %in% class(collection)){
    ee_layer <- collection$
      filterDate(dates[1], dates[2])
  } else {
    stop("collection must be either a character string or a GEE image collection")
  }

  # Get nominal scale for the layer (native resolution) and projection
  scale <- ee_layer$first()$projection()$nominalScale()$getInfo()

  # Subset bands
  if(!is.null(bands)){
    ee_layer <- ee_layer$select(bands)
  }

  # Remove missing values (this will depend on the layer)
  if(unmask){
    ee_layer <- ee_layer$unmask()
  }

  # Reduce to image
  temp_reducer <- paste0("ee$Reducer$", temp_reducer, "()")
  ee_layer <- ee_layer$reduce(eval(parse(text = temp_reducer)))

  # Extract layer values
  spt_reducer <- paste0("ee$Reducer$", spt_reducer, "()")
  pentads_layer <- ee_layer %>%
    ee$Image$reduceRegions(ee_pentads,
                           eval(parse(text = spt_reducer)),
                           scale = scale) %>%
    ee_as_sf(via = "drive")

  # Fix layer name
  if(!is.null(bands) & length(bands) == 1){
    pentads_layer <- pentads_layer %>%
      dplyr::rename("{bands}" := ncol(pentads_layer) - 1)
  }

  # This should do something similar but get problems with maxFeatures
  # pentads_layer <- ee_extract(x = ee_layer,
  #                                   y = ee_pentads,
  #                                   fun = eval(parse(text = reducer)),
  #                                   scale = scale,
  #                                   sf = TRUE,
  #                                   via = "drive")

  return(pentads_layer)


}
