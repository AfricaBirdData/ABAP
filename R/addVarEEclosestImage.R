#' Add variable from closest image ina Google Earth Engine Collection
#'
#' @description Each feature in a feature collection (i.e. row in asimple feature
#' collection) is matched with the image in an image collection that is closest
#' to it in time.
#' @param ee_pentads A feature collection with the pentads we want to annotate.
#' We need to upload an sf object with the pentads to GEE. This object must
#' also have character column with the dates that need to be matched against the
#' image collection dates. The format must be "yyyy-mm-dd" and the column must
#' be named "Date".
#' @param collection Either a character string with the name of the collection
#' we want to use or a GEE collection produced with \code{ee$ImageCollection()}.
#' See \href{https://developers.google.com/earth-engine/datasets/catalog}{GEE catalog}.
#' @param reducer A character string specifying the function apply when
#' extracting values for each pentad. It is common to use "mean", "sum" or
#' "count". But there are many other, see 'ee.Reducer' under Client Libraries at
#' \url{https://developers.google.com/earth-engine/apidocs}.
#' @param maxdiff Maximum difference in days allowed for an image to be matched with
#' data.
#' @param bands Select specific bands from the image. Only one band at a time is
#' allowed for now.
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
#' # Annotate with TerraClimate dataset
#' visit_new <- addVarEEclosestImage(ee_pentads = ee_data,
#'                                   collection = "IDAHO_EPSCOR/TERRACLIMATE",
#'                                   reducer = "mean",                          # We only need spatial reducer
#'                                   maxdiff = 15,                              # This is the maximum time difference that GEE checks
#'                                   bands = c("tmmn"))
#' }
addVarEEclosestImage <- function(ee_pentads, collection, reducer, maxdiff,
                                 bands = NULL, unmask = FALSE){

    lifecycle::deprecate_warn(
        when = "0.3.0",
        what = "ABAP::addVarEEclosestImage()",
        with = "ABDtools::addVarEEclosestImage()",
        details = "A new version of addVarEEclosestImage() is now on package ABDtools (https://github.com/AfricaBirdData/ABDtools).
        The ABAP function will be discontinued",
        id = NULL,
        always = FALSE,
        env = rlang::caller_env(),
        user_env = rlang::caller_env(2)
    )

  # Get image
  if(is.character(collection)){
    ee_layer <- rgee::ee$ImageCollection(collection)
  } else if("ee.imagecollection.ImageCollection" %in% class(collection)){
    ee_layer <- collection
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

  # Function to add date in milliseconds
  addTime <- function(feature) {
    datemillis <- rgee::ee$Date(feature$get('Date'))$millis()
    return(feature$set(list('date_millis' = datemillis)))
  }

  # Add date in milliseconds
  ee_pentads <- ee_pentads$map(addTime)

  # Set filter to select images within max time difference
  maxDiffFilter = rgee::ee$Filter$maxDifference(
    difference = maxdiff*24*60*60*1000,        # days * hr * min * sec * milliseconds
    leftField = "date_millis",                 # Timestamp of the visit
    rightField = "system:time_start"           # Image date
  )

  # Set a saveBest join that finds the image closest in time
  saveBestJoin <- rgee::ee$Join$saveBest(
    matchKey = "bestImage",
    measureKey = "timeDiff"
  )

  # Apply the join
  best_matches <- saveBestJoin$apply(ee_pentads, ee_layer, maxDiffFilter)

  # Function to add value from the matched image
  reducer <- paste0("rgee::ee$Reducer$", reducer, "()")
  add_value <- function(feature){

    # Get the image selected by the join
    img <- rgee::ee$Image(feature$get("bestImage"))$select(bands)

    # Reduce values within pentad
    pentad_val <- img$reduceRegion(eval(parse(text = reducer)),
                                   feature$geometry(),
                                   scale)

    # Return the data containing value and image date.
    return(feature$set('val', pentad_val$get(bands),
                       'DateTimeImage', img$get('system:index')))

  }

  # Add values to the data and download
  out <- best_matches$map(add_value) %>%
    rgee::ee_as_sf(via = 'drive')

  # Fix names and variables
  layer_name <- paste0(bands, "_", reducer)

  out <- out %>%
      dplyr::rename_with(~gsub("val", layer_name, .x), .cols = dplyr::starts_with("val")) %>%
      dplyr::select(-c(id, date_millis, bestImage)) %>%
      dplyr::select(dplyr::everything(), DateTimeImage)

  return(out)

}
