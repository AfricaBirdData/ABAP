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
#' #' @param reducer A character string specifying the function apply when
#' extracting values for each pentad. It is common to use "mean", "sum" or
#' "count". But there are many other, see 'ee.Reducer' under Client Libraries at
#' \url{https://developers.google.com/earth-engine/apidocs}.
#' @param collection The name of the collection we want to use. See
#' \href{https://developers.google.com/earth-engine/datasets/catalog}{GEE catalog}.
#' @maxdiff Maximum difference in days allowed for an image to be matched with
#' data.
#' @param bands Select specific bands from the image. Only one band at a time is
#' allowed for now.
#' @param unmask GEE masks missing values, which means they are not used for
#' computing means, counts, etc. Sometimes we might want to avoid this behaviour
#' and use 0 instead of NA. If so, set unmask to TRUE.
#'
#' @return
#' @export
#'
#' @examples
addVarFromEEclosestImage <- function(ee_pentads, collection, reducer, maxdiff,
                                     bands = NULL, unmask = FALSE){

  # Get image
  ee_layer <- ee$ImageCollection(collection)

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
    datemillis <- ee$Date(feature$get('Date'))$millis()
    return(feature$set(list('date_millis' = datemillis)))
  }

  # Add date in milliseconds
  ee_pentads <- ee_pentads$map(addTime)

  # Set filter to select images within max time difference
  maxDiffFilter = ee$Filter$maxDifference(
    difference = maxdiff*24*60*60*1000,        # days * hr * min * sec * milliseconds
    leftField = "date_millis",                 # Timestamp of the visit
    rightField = "system:time_start"           # Image date
  )

  # Set a saveBest join that finds the image closest in time
  saveBestJoin <- ee$Join$saveBest(
    matchKey = "bestImage",
    measureKey = "timeDiff"
  )

  # Apply the join
  best_matches <- saveBestJoin$apply(ee_pentads, ee_layer, maxDiffFilter)

  # Function to add value from the matched image
  reducer <- paste0("ee$Reducer$", reducer, "()")
  add_value <- function(feature){

    # Get the image selected by the join
    img <- ee$Image(feature$get("bestImage"))$select(bands)

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
    ee_as_sf(via = 'drive')

  return(out)

}
