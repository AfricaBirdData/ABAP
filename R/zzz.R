.onLoad <- function(libname, pkgname) {

    packageStartupMessage(
        strwrap(prefix = " ", width = 70,
                "NOTE: in ABAP version 0.1 we have changed the naming convention
                for variables coming out of Google Earth Engine-related functions.
                Previous code might not work as expected.")
    )

}
