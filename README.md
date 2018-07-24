[![Travis-CI Build Status](https://travis-ci.org/EhrmannS/rasterTools.svg?branch=master)](https://travis-ci.org/EhrmannS/rasterTools)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/EhrmannS/rasterTools?branch=master&svg=true)](https://ci.appveyor.com/project/EhrmannS/rasterTools)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rasterTools)](https://cran.r-project.org/package=rasterTools)
[![Coverage Status](https://img.shields.io/codecov/c/github/EhrmannS/rasterTools/master.svg)](https://codecov.io/github/EhrmannS/rasterTools?branch=master)
[![](http://cranlogs.r-pkg.org/badges/grand-total/rasterTools)](http://cran.rstudio.com/web/packages/rasterTools/index.html)

# rasterTools

***obtain and process earth observation data***

The rasterTools package provides the toolchain for a transparent and reproducible workflow to obtain and process spatial (earth observation) data for an evidence based approach to landscape ecological questions.


## Getting started

1) Install the development version from github via:

        devtools::install_github("EhrmannS/rasterTools")

2) Read a **brief introduction** to understand the philosophy of `rasterTools`:

        ?`rasterTools-package`

3) The vignettes given an in detail [Introduction](https://ehrmanns.github.io/rasterTools/articles/introduction.html) and explain what the logic behind [landscape metrics](https://ehrmanns.github.io/rasterTools/articles/landscape_metrics.html) is.

## Example workflow

1) (Down)load a range of gridded datasets:

        myDatasets <- list(list(operator = "oGFC", years = 2006)),
                           list(operator = "oMODIS", product = "mod17a3",
                                period = 2006, layer = 2),
                           ...)
        myData <- obtain(data = myDatasets, mask = aMask)
                           
2) Determine forest patches in a raster with continuous integer values:

        get_patches <- list(list(operator = "rBinarise", thresh = 30),
                            list(operator = "rPatches"))
        myInput <- rtData$continuous
        myPatches <- modify(input = myInput, by = get_patches, sequential = TRUE)
        visualise(raster::stack(myInput, myPatches))

<img src="https://raw.githubusercontent.com/EhrmannS/rasterTools/master/vignettes/readme.png"  width="100%" />

3) Compute the *Class proportional area* in a raster with categorial values:

        myInput <- rtData$categorial
        myMetric <- list(a_w = list(operator = "mArea", scale = "window"),
                         a_c = list(operator = "mArea", scale = "class"),
                         mCPA = "a_c / a_w * 100")
        measure(input = myInput, with = myMetrics)


## Contribute
In case you have a suggestion for a feature or function you think is missing, please file an [issue](https://github.com/EhrmannS/rastertools/issues) and I try to implement it. If you want to contribute, [develop your own addition](/vignettes/contribute.Rmd) to `rasterTools` and share it with the community by creating a [pull request](https://github.com/EhrmannS/rastertools/pulls) on github (I will include you as author of the respective function).

## Planned for future versions
- Support of the Sentinel (high prio), Landsat and Lidar datasets.

- `geomCurve()` to create lines and curves (high prio); `geomVoronoi()` to create a pattern of voronoi polygons with a random or given point pattern as anchor

- new functions to simulate neutral and process oriented landscape models (high prio).

- Support of various "national forest inventory" datasets (Germany, France, Italy, Spain, yours?)

- `rTilify()` to segregate a gidded dataset into another tiling, for instance to align datasets to each other or produce a hexagonal tiling of a rectangluarly tiled dataset.

- in-R georeferencing.

- more `Rcpp` and parallelisation.

## Citation
Steffen Ehrmann (2018). rasterTools: obtain and process earth observation data. R package version 0.8.0. https://github.com/EhrmannS/rastertools

```
@Manual{,
  title = {rasterTools: obtain and process earth observation data},
  author = {Steffen Ehrmann},
  year = {2018},
  note = {R package version 0.8.0},
  url = {https://github.com/EhrmannS/rastertools},
}
```

## Acknowledgements
I am grateful for financial support from the PROFOUND Cost-action, which gave me the opportunity to work in a concentrated effort a large part of the functionality. This package has been developed in support of the [FunBo Project](https://www.researchgate.net/project/Do-small-mammals-think-big-The-multiscale-ecology-of-small-mammals-and-their-functional-role-for-Borrelia-burgdorferi-FunBo), which was made possible by the Grünewald-Zuberbier Scholarship handed out by the University of Freiburg.

Thanks are also due to Prof. Arne Pommerening who was a great source of inspiration for what `rasterTools` is now.