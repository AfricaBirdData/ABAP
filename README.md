
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SABAP

<!-- badges: start -->
<!-- badges: end -->

This packages provides functionality to access and download data from
the Southern Africa Bird Atlas Project.

A typical workflow entails defining a region and a species of interest,
e.g. say we are interested in the occupancy of the African Black Duck in
the North West province of South Africa:

First find the SABAP2 code for the species:

``` r
library(SABAP)

# We can search for all duck species
ducks <- searchSabapSpecies("Duck")

# Then we can extract the code we are interested in
ducks[ducks$Common_species == "African Black", "SAFRING_No"]
#> # A tibble: 1 × 1
#>   SAFRING_No
#>   <chr>     
#> 1 95
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
library(sf)
#> Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1
class(pentads_sabap)
#> [1] "sf"         "data.frame"
unique(st_geometry_type(pentads_sabap))
#> [1] POLYGON
#> 18 Levels: GEOMETRY POINT LINESTRING POLYGON MULTIPOINT ... TRIANGLE
plot(st_geometry(pentads_sabap), lwd = 0.1)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

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
library(dplyr, warn.conflicts = FALSE)

# Find species code
my_det_data <- searchSabapSpecies("Duck") %>% 
  filter(Common_species == "African Black") %>% 
  pull(SAFRING_No) %>% 
  # Download SABAP data for the whole North West province
  getSabapData(.region_type = "province", .region = "North West") %>% 
  # Filter pentads of interest  
  filter(Pentad %in% pentads_sel)
```

## INSTRUCTIONS TO INSTALL

``` r
# In the call below you must replace 'yourtoken' with your GitHub Personal Authorisation Token (PAT)
remotes::install_github("AfricanBirdData/SABAP", type = "source", auth_token = 'yourtoken')
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
-   Merge your changes:
    -   In the GitHub repo you will now see an option that notifies of
        changes in a branch: click compare and pull request.
    -   If there are no conflicts ‘merge pull request’
-   Delete the branch. You will have to delete it in the remote repo
    (GitHub) and also in your local machine. In your local machine you
    have to use Git directly, apparently RStudio doesn´t do it:
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
