# ABAP 0.4.1

-   searchAbapSpecies() uses a single API call for all requests. This implies that field names might have changed

# ABAP 0.4.0

-   Added abapToOccuR()

-   Removed rgee as a dependency. It is now a dependency of ABDtools package

# ABAP 0.3.0

-   Deprecated ABAP-rgee functions (use ABDtools now)

-   Added function to prepare spOccupancy multi-season data

# ABAP 0.2.0

-   Added function to download all cards from a given pentad

-   Added function to find a pentad from lon-lat coordinates

-   Eliminated dependency from data.table

# ABAP 0.1.1

-   Speed up 'abapTo' functions

-   spOccupancy and unmarked objects are ordered by pentad

# ABAP 0.1.0

-   Set a standard for variables coming out of Google Earth Engine images. Variables will always be named as "\<bandname\>\_\<spt_reducer\>". **Note that this change could break some existing code**

-   Added functions to format ABAP data into objects used by popular occupancy modelling R packages, such as *unmarked* or *spOccupancy*.

# ABAP 0.0.5

-   Added a `NEWS.md` file to track changes to the package.
-   rgee:: called explicitly
