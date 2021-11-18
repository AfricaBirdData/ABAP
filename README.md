
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SABAP

<!-- badges: start -->
<!-- badges: end -->

This packages provides functionality to access and download data from
the Southern Africa Bird Atlas Project. Most of its functionality is
experimental (especially that using Google Earth Engine) and under
development, so use with caution and please send any feedback!

## INSTRUCTIONS TO INSTALL

``` r
# In the call below you must replace 'yourtoken' with your GitHub Personal Authorisation Token (PAT)
remotes::install_github("AfricanBirdData/SABAP", type = "source", auth_token = 'yourtoken')
```

## DOWNLOAD SABAP DATA

A typical workflow entails defining a region and a species of interest,
e.g. say we are interested in the occupancy of the African Black Duck in
the North West province of South Africa:

First find the SABAP2 code for the species:

``` r
library(SABAP)
library(sf)
library(dplyr, warn.conflicts = FALSE)

# We can search for all duck species
ducks <- searchSabapSpecies("Duck")

# Then we can extract the code we are interested in
ducks[ducks$Common_species == "African Black", "SAFRING_No"]
```

With our code (95) we can download the data recorded for the region of
interest:

``` r
my_det_data <- getSabapData(.spp_code = 95, .region_type = "province", .region = "North West")
```

Note the trailing dots in the argument names. This makes it easier to
incorporate this function into other larger functions and workflows.

We may be interested in detection data in a set of pentads that do not
correspond to any particular region. Although getSabapData allows you to
download data from any one pentad, it is not advised to use this
functionality to loop over a set of pentads (unless it is a small set).
this is because the algorithm will create a query for each pentad!
Resulting in a very slow process. The easiest and fastest way to obtain
these data is to download a larger region that contains our pentads and
then filter only those we are interested in.

SABAP comes with the pentads installed as data in
[sf](https://r-spatial.github.io/sf/) format (POLYGON).

``` r
class(pentads_sabap)
unique(st_geometry_type(pentads_sabap))
plot(st_geometry(pentads_sabap), lwd = 0.1)
```

For demonstration purposes let’s subset ten random pentads in the North
West province using the data we just downloaded and download Black Duck
data for them. This doesn’t make any sense, but hopefully shows the
point.

``` r
set.seed(8476)

pentads_sel <- unique(my_det_data$Pentad) %>% 
  sample(10)

# We can now subset those pentads from the original data
det_data_sel <- my_det_data[my_det_data$Pentad %in% pentads_sel,]
```

To illustrate the entire workflow with
[dplyr](https://dplyr.tidyverse.org/), we will use the same selection of
pentads.

``` r
# Find species code
my_det_data <- searchSabapSpecies("Duck") %>% 
  filter(Common_species == "African Black") %>% 
  pull(SAFRING_No) %>% 
  # Download SABAP data for the whole North West province
  getSabapData(.region_type = "province", .region = "North West") %>% 
  # Filter pentads of interest  
  filter(Pentad %in% pentads_sel)
```

## ANNOTATE WITH GOOGLE EARTH ENGINE

### Installation

We have added some basic functionality to annotate pentads with
environmental data from Google Earth Engine (GEE). This should make the
analysis of SABAP data easier and more reproducible. This functionality
uses the package `rgee`, which translates R code into Python code using
`reticulate`, and allows us to use the GEE Python client libraries from
R! You can find extensive documentation about `rgee`
[here](https://github.com/r-spatial/rgee).

But first, we need to create a GEE account, install `rgee` (and
dependencies) and configure our Google Drive to upload and download data
to and from GEE. There are other ways of upload and download but we
recommend Google Drive, because it is simple and effective.
Configuration is a bit of a process, but you will have to do this only
once.

-   To create a GEE account, please follow these
    [instructions](https://earthengine.google.com/signup/).
-   To install `rgee`, follow these
    [instructions](https://github.com/r-spatial/rgee#installation).
-   To configure Google Drive, follow these
    [instructions](https://r-spatial.github.io/rgee/articles/setup.html).

We have nothing to do with the above steps, so if you get stuck along
the way, please search the web or contact the developers directly.

Phew, well done if you managed that! With configuration out of the way,
let’s see how to annotate some pentads. We’ve coded some wrappers around
basic functions from the `rgee` package to provide some **basic**
functionality without having to know almost anything about GEE. However,
if you want more complicated workflows, we totally recommend learning
how to use GEE and `rgee` and exploit their enormous power. It does take
some time though and if you just want to annotate your data with some
precipitation values, you can totally do it with the functions we
provide.

### Initialize

Most image processing and handling of spatial features happens in GEE
servers. Therefore, there will be constant flow of information between
our computer (client) and GEE servers (server). This information flow
happens through Google Drive. So when we start our session we need to
run.

``` r
# Initialize Earth Engine
library(rgee)

# Check installation
ee_check()
#> ◉  Python version
#> ✓ [Ok] /home/pachorris/.virtualenvs/rgee/bin/python v3.6
#> ◉  Python packages:
#> ✓ [Ok] numpy
#> ✓ [Ok] earthengine-api

# Initialize rgee and Google Drive
ee_Initialize(drive = TRUE)
#> ── rgee 1.1.0 ─────────────────────────────────────── earthengine-api 0.1.277 ── 
#>  ✓ user: not_defined
#>  ✓ Google Drive credentials: ✓ Google Drive credentials:  FOUND
#>  ✓ Initializing Google Earth Engine: ✓ Initializing Google Earth Engine:  DONE!
#>  ✓ Earth Engine account: users/ee-assets 
#> ────────────────────────────────────────────────────────────────────────────────
```

Make sure that all tests and checks are passed. If so, you are good to
go!

### Uploading data to GEE

Firstly, you will need to upload the data you want to annotate to GEE.
These data will go to your assets directory in GEE server and it will
stay there until you remove it. So if you have uploaded some data, you
don’t have to upload it again. You typically want to make some spatial
analysis or match of some sort, so you probably want to upload an `sf`
or `raster` object. In fact, all GEE functions from the SABAP package
work with pentads, and they must be uploaded from an `sf` object. For
example to upload all SABAP pentads (remember that these are already on
an `sf` format!), we can use:

``` r
library(SABAP)
library(sf)
library(dplyr, warn.conflicts = FALSE)

# Load SABAP pentads
pentads <- SABAP::pentads_sabap

# Set an ID for your remote asset (data in GEE)
assetId <- sprintf("%s/%s", ee_get_assethome(), 'pentads')

# Upload to GEE (if not done already - do this only once)
pentads %>%
  sf_as_ee(assetId = assetId,
           via = "getInfo_to_asset")

# Load the remote asset to you local computer to work with it
ee_pentads <- ee$FeatureCollection(assetId)
```

Now, the object `pentads` lives in your machine, but the object
ee\_pentads lives in the GEE server. You only have a “handle” to it in
your machine to manipulate it. This might seem a bit confusing at first
but you will get used to it.

### Annotate pentads with a GEE image

An image in GEE jargon is the same thing as a raster in R. There are
also image collections which are like raster stacks (we’ll see more
about these later). You can find a full catalog of what is available in
GEE [here](https://developers.google.com/earth-engine/datasets/catalog).
If you want to use data from a single image you can use the function
`addVarEEimage`.

For example, let’s annotate our SABAP pentads with [surface water
occurrence](https://developers.google.com/earth-engine/datasets/catalog/JRC_GSW1_3_GlobalSurfaceWater),
which is the frequency with which water was present in each pixel. We’ll
need the name of the layer in GEE, which is given in the field “Earth
Engine Snippet”. Images can have multiple bands – in this case, we
select “occurrence”.

Finally, images have pixels but our spatial objects are polygons, so we
need to define some type of summarizing function. The same thing that
`raster::extract()` would need. In GEE this is called a “reducer”. In
this case, we will select the mean (i.e., mean water occurrence per
pixel within each pentad).

``` r
pentads_water <- addVarEEimage(ee_pentads = ee_pentads,                   # Note that we need our remote asset here
                               image = "JRC/GSW1_3/GlobalSurfaceWater",   # You can find this in the code snippet
                               reducer = "mean",
                               bands = "occurrence")
```

### Annotate pentads with a GEE collection

Sometimes data don’t come in a single image, but in multiple images. For
example, we might have one image for each day, week, month, etc. Again,
we can check all available data in the [GEE
catalog](https://developers.google.com/earth-engine/datasets/catalog).
When we want to annotate data with a collection, SABAP offers two
options:

-   We can use `addVarEEcollection` to summarize the image collection
    over time ( e.g. calculate the mean over a period of time)

-   Or we can use `addVarEEclosestImage` to annotate each row in the
    data with the image in the collection that is closest in time. In
    the SABAP context this is particularly useful to annotate visit
    data, where we are interested in the conditions observers found
    during their surveys.

We demonstrate both annotating data with the [TerraClimate
dataset](https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_TERRACLIMATE).
If we were interested in the mean minimum temperature across the year
2010, we could use

``` r
pentads_tmmn <- addVarEEcollection(ee_pentads = ee_pentads,                    # Note that we need our remote asset here
                                   collection = "IDAHO_EPSCOR/TERRACLIMATE",   # You can find this in the code snippet
                                   dates = c("2010-01-01", "2011-01-01"),
                                   temp_reducer = "mean",
                                   spt_reducer = "mean",
                                   bands = "tmmn")
```

Note that in this case, we had to specify both a temporal reducer to
summarize pixel values over time and a spatial reducer, to summarize
pixel values within pentad. The `dates` argument subsets the whole
TerraClimate dataset to those images between `dates[1]` (inclusive) and
`dates[2]` (exclusive).

If we wanted to annotate with the closest image, instead of with a
summary over time, then we need to upload visit data with an associated
date to GEE. Dates must be in a character format (“yyyy-mm-dd”).

As an example, let’s download SABAP2 data for the Maccoa Duck in 2010
and annotate these data with TerraClimate’s minimum temperature data.

``` r
# Load SABAP pentads
pentads <- SABAP::pentads_sabap

# Download Maccoa Duck
id <- SABAP::searchSabapSpecies("Duck") %>% 
  filter(Common_species == "Maccoa") %>% 
  pull(SAFRING_No)

visit <- SABAP::getSabapData(.spp_code = id,
                             .region_type = "country",
                             .region = "South Africa",
                             .years = 2008)

# Make spatial object
visit <- visit %>% 
   left_join(pentads, by = c("Pentad" = "Name")) %>% 
   st_sf() %>% 
   filter(!st_is_empty(.))   # Remove rows without geometry

# NOTE: TerraClimate offers monthly data. The date of each image is the beginning of
# the month, which means that dates after the 15th will be matched against the
# next month. I will change all dates to be on the first of the month for the
# analysis
visit <- visit %>% 
   dplyr::select(CardNo, StartDate, Pentad, TotalHours, Spp) %>% 
   mutate(Date = lubridate::floor_date(StartDate, "month"))

# Load to EE (if not done already)
assetId <- sprintf("%s/%s", ee_get_assethome(), 'visit2008')

# Format date and upload to GEE
visit %>%
      dplyr::select(CardNo, Pentad, Date) %>%
      mutate(Date = as.character(Date)) %>%   # GEE doesn't like dates
      sf_as_ee(assetId = assetId,
               via = "getInfo_to_asset")

# Load the remote data asset
ee_visit <- ee$FeatureCollection(assetId)

# Annotate with GEE TerraClimate
visit_new <- addVarEEclosestImage(ee_pentads = ee_visit,
                                  collection = "IDAHO_EPSCOR/TERRACLIMATE",
                                  reducer = "mean",                          # We only need spatial reducer
                                  maxdiff = 15,                              # This is the maximum time difference that GEE checks
                                  bands = c("tmmn"))
```

### Convert image collection to a multi-band image

Lastly, we have made a convenience function that converts an image
collection into a multi-band image. This is useful because you can only
annotate one image at a time, but all the bands in the image get
annotated. So if you want to add several variables to your data, you can
first create a multi-band image and then annotate with all at once. This
way you minimize the traffic between your machine and GEE servers saving
precious time and bandwidth.

Here we show how to find the mean NDVI for each year between 2008 and
2010, and with this find the mean NDVI per pentad.

``` r
# Create a multi-band image with mean NDVI for each year
multiband <- EEcollectionToMultiband(collection = "MODIS/006/MOD13A2",
                                           dates = c("2008-01-01", "2020-01-01"),
                                           band = "NDVI",                       # You can find what bands are available from GEE catalog
                                           group_type = "year",
                                           groups = 2008:2019,
                                           reducer = "mean",
                                           unmask = FALSE)

# Find mean (mean) NDVI for each pentad and year
pentads_ndvi <- addVarEEimage(ee_pentads, multiband, "mean")
```

## INSTRUCTIONS TO CONTRIBUTE CODE

First clone the repository to your local machine:

-   In RStudio, create a new project
-   In the ‘Create project’ menu, select ‘Version Control’/‘Git’
-   Copy the repository URL (click on the ‘Code’ green button and copy
    the link)
-   Choose the appropriate directory and ‘Create project’
-   Remember to pull the latest version regularly

For site owners:

There is the danger of multiple people working simultaneously on the
project code. If you make changes locally on your computer and, before
you push your changes, others push theirs, there might be conflicts.
This is because the HEAD pointer in the main branch has moved since you
started working.

To deal with these lurking issues, I would suggest opening and working
on a topic branch. This is a just a regular branch that has a short
lifespan. In steps:

-   Open a branch at your local machine
-   Push to the remote repo
-   Make your changes in your local machine
-   Commit and push to remote
-   Create pull request:
    -   In the GitHub repo you will now see an option that notifies of
        changes in a branch: click compare and pull request.
-   Delete the branch. When you are finished, you will have to delete
    the new branch in the remote repo (GitHub) and also in your local
    machine. In your local machine you have to use Git directly, because
    apparently RStudio doesn´t do it:
    -   In your local machine, change to master branch.
    -   Either use the Git GUI (go to branches/delete/select
        branch/push).
    -   Or use the console typing ‘git branch -d your\_branch\_name’.
    -   It might also be necessary to prune remote branches with ‘git
        remote prune origin’.

Opening branches is quick and easy, so there is no harm in opening
multiple branches a day. However, it is important to merge and delete
them often to keep things tidy. Git provides functionality to deal with
conflicting branches. More about branches here:

<https://git-scm.com/book/en/v2/Git-Branching-Branches-in-a-Nutshell>

Another idea is to use the ‘issues’ tab that you find in the project
header. There, we can identify issues with the package, assign tasks and
warn other contributors that we will be working on the code.
