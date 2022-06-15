#' Add variable from Google Earth Engine Image
#'
#' @param ee_pentads A feature collection with the pentads we want to annotate.
#' We need to upload an sf object with the pentads to GEE.
#' @param image Either a character string with the name of the image we want to
#' use or an GEE image produced with \code{ee$Image()}. See
#' \href{https://developers.google.com/earth-engine/datasets/catalog}{GEE catalog}.
#' @param reducer A character string specifying the function apply when
#' extracting values for each pentad. It is common to use "mean", "sum" or
#' "count". But there are many other, see 'ee.Reducer' under Client Libraries at
#' \url{https://developers.google.com/earth-engine/apidocs}.
#' @param bands Select specific bands from the image. If not specified, pentads
#' will be annotated with all bands (one column for each band).
#' @param unmask GEE masks missing values, which means they are not used for
#' computing means, counts, etc. Sometimes we might want to avoid this behaviour
#' and use 0 instead of NA. If so, set unmask to TRUE.
#'
#' @return A dataframe similar to \code{ee_pentads} with variables added from the
#' \code{bands} selected from \code{collection}. Note that following \href{https://github.com/r-spatial/rgee}{rgee}
#' the name of the new variables will be the selected band (\code{bands} or else
#' all bands from \code{collection} followed by the spatial reducer \code{reducer}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the remote data asset
#' ee_data <- ee$FeatureCollection(assetId)  # assetId must correspond to an asset in your GEE account
#'
#'# Annotate with surface water occurrence image
#' pentads_tmmn <- addVarEEcollection(ee_pentads = ee_data,
#'                                    image = "JRC/GSW1_3/GlobalSurfaceWater",
#'                                    reducer = "mean",
#'                                    bands = "occurrence")
#' }
addVarEEimage <- function(ee_pentads, image, reducer,
                              bands = NULL, unmask = FALSE){

  # Get image
  if(is.character(image)){
    ee_layer <- rgee::ee$Image(image)
  } else if("ee.image.Image" %in% class(image)){
    ee_layer <- image
  } else {
    stop("image must be either a character string or a GEE image")
  }

  # Subset bands
  if(!is.null(bands)){
    ee_layer <- ee_layer$select(bands)
  }

  # Remove missing values (this will depend on the layer)
  if(unmask){
    ee_layer <- ee_layer$unmask()
  }

  # Get nominal scale for the layer (native resolution)
  scale <- ee_layer$projection()$nominalScale()$getInfo()

  # Extract layer values
  eeReducer <- paste0("rgee::ee$Reducer$", reducer, "()")
  pentads_layer <- ee_layer %>%
    rgee::ee$Image$reduceRegions(ee_pentads,
                                 eval(parse(text = eeReducer)),
                                 scale = scale) %>%
    rgee::ee_as_sf(via = "drive")

  # Fix layer name
  if(!is.null(bands) & length(bands) == 1){

      layer_name <- paste0(bands, "_", reducer)

      pentads_layer <- pentads_layer %>%
          dplyr::rename("{layer_name}" := reducer)

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
