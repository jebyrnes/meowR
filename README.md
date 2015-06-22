A library to work with Spalding et al.'s Marine Ecoregions of the world
-----------------------------------------------------------------------

This library was developed as I used the MEOW spatial data more and more
and wrote a few general functions. The data are from
<http://www.marineregions.org/downloads.php> downloaded on June 1st
2015. See the original paper at
<http://www.nature.org/ourinitiatives/regions/northamerica/unitedstates/colorado/scienceandstrategy/marine-ecoregions-of-the-world.pdf>

Installation
------------

    #install.packages("devtools")
    library(devtools)
    install_github("meowR", "jebyrnes")

Plotting the SpatialPolygons
----------------------------

    library(meowR)
    data(regions)
    data(provinces)
    data(realms)

    par(mfrow=c(2,2), mar=c(0,1,2,0))
    plot(regions, main="Ecoregion")
    plot(provinces, lwd=2, border="red", main="Province")
    plot(realms, lwd=2, border="blue", main="Realm")
    par(mfrow=c(1,1), mar=c(5, 4, 4, 2) + 0.1)

![](README_files/figure-markdown_strict/unnamed-chunk-3-1.png)

Using data.frame representations with ggplot2 and external data to make a choropleth plot
-----------------------------------------------------------------------------------------

Each SpatialPolygonsDataFrame has a parallel dataset where the regional
information is in a data frame. These have the same names as the sp
objects, but with .df in their object name. These were designed to use
with plot or ggplot2.

    data(provinces.df) 
     
    base_province_ggplot <- ggplot(provinces.df) + theme_bw() +
    aes(long,lat,group=group) + 
      geom_polygon(fill=NA) +
      geom_path(color="black") +
      coord_equal() 
      
      base_province_ggplot

![](README_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    data(regions.df)

    ndf <- data.frame(Ecoregions = levels(regions.df$ECOREGION), 
    Values = runif(length(levels(regions.df$ECOREGION)), 0,100))

    makeMEOWmap(ndf, fillColName="Values", regionColName="Ecoregions")

    ## Joining by: Ecoregions

![](README_files/figure-markdown_strict/unnamed-chunk-5-1.png)

Version numbers
---------------

`meowR` uses [semantic versioning](http://semver.org/). The version
numbering scheme is `major`.`minor`.`revision`. Unless `major` is 1, the
package should not be considered stable. All releases with the same
`major` versions are compatible. Increases in `minor` represents the
addition of backwards-compatible additions. Increases in `revision`
represents either bug fixes or improvements.

Contributions
-------------

People wanting to contribute are welcome to do so by forking the
repository, and submitting a pull request when their work is done.
Please also edit the `DESCRIPTION` file to add your name to the
`Authors` field.
