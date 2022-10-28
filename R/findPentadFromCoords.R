#' Find the ABAP pentad that contains a spatial location
#'
#' @param x Longitude of the point contained by the pentad, in decimal degrees.
#' @param y Longitude of the point contained by the pentad, in decimal degrees.
#'
#' @return The code of the pentad containing the location defined by `x` and `y`.
#' @export
#'
#' @examples
#' findPentadFromCoords(-25.68606, 29.011337)
findPentadFromCoords <- function(x, y){

    x <- abs(x)
    x1 <- floor(6 * (x - floor(x)))

    if(x1 == 0){
        x2 <- floor(60 * (x - floor(x)))
    } else {
        x2 <- floor(60 * (x - floor(x))) %% (x1 * 10)
    }

    x2 <- ifelse(x2 < 5, 0, 5)

    x <- paste(floor(x), x1, x2, sep="")

    y1 <- floor(6 * (y - floor(y)))

    if(y1 == 0){
        y2 <- floor(60 * (y - floor(y)))
    } else {
        y2 <- floor(60 * (y - floor(y))) %% (y1 * 10)
    }

    y2 <- ifelse(y2 < 5, 0, 5)

    y <- paste(floor(y), y1, y2, sep="")

    paste(x, "_", y, sep = "")

    # return(paste(x, "_", y, sep=""))
    # rm(x, x1, x2, y, y1, y2)

}
