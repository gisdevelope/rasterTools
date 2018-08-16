#' An example raster stack
#'
#' A set of two conceptually different types of raster.
#'
#' @format The object of class RasterStack has no projection and is a
#'   RasterStack object of 56 by 60 cells. The first raster represents
#'   land-use classes and the second raster contains a continuous scale of
#'   vegetation cover.
"rtData"

#' Dataset paths
#'
#' All the paths to the datasets that are of interest.
#'
#' @format A list with as many elements as there are datasets currently defined.
#'   Each list has the elements 'local', where the path to the local storage of
#'   the dataset is given, and 'online', where the dataset can be donwloaded.
"rtPaths"

#' Metadata of the MODIS datasets
#'
#' @format The dataframe with 13 variables and 97 entries contains meta-data for
#'   a couple of predefined MODIS products. \describe{ \item{\code{product}}{the
#'   MODIS-product name as defined by NASA} \item{\code{newest_version}}{the
#'   newest version for the respective product}
#'   \item{\code{temporal_granularity}}{the number of days between two
#'   data-files} \item{\code{pixel_size}}{the spatial resolution, i.e. the size
#'   of one raster cell} \item{\code{sds_layer_name}}{the machine-readable name
#'   of the layer} \item{\code{description_raw}}{a human-readable description of
#'   the layer} \item{\code{units}}{the unit in which the data are saved}
#'   \item{\code{data_type}}{bit number which determines the length of the
#'   values} \item{\code{fill}}{the fill value} \item{\code{valid_min}}{the
#'   minimum valid value} \item{\code{valid_max}}{the maximum valid value}
#'   \item{\code{scaling_factor}}{a correction factor} \item{start_terra}{the
#'   launch date for the Terra satellite} \item{start_aqua}{the launch date for
#'   the Aqua satellite}}
"meta_modis"

#' Metadata of the Corine Land Cover dataset
#'
#' @format The \code{data.frame} with 4 variables and 44 entries contains
#'   meta-data for the classes of land cover in the clc dataset. \describe{
#'   \item{\code{level1}}{level 1 description of the land cover class}
#'   \item{\code{level2}}{level 2 description of the land cover class}
#'   \item{\code{clc_code}}{numeric code of the level 3 description}
#'   \item{\code{level3}}{most detailed description of the land cover class}}
"meta_clc"

#' Metadata of the EMMA datasets
#'
#' @format The \code{data.frame} with 5 variables and 245 entries contains
#'   meta-data for the mammals covered in the Atlas of European Mammals.
#'   \describe{ \item{\code{original}}{the species name as noted in the Atlas of
#'   European Mammals} \item{\code{english}}{the english name}
#'   \item{\code{author}}{the species author} \item{\code{order}}{the species
#'   order} \item{\code{family}}{the species family} \item{\code{iucn}}{the
#'   conservation status according to the IUCN Red List of Threatened Species}}
"meta_emma"

#' Metadata of the EFTA datasets
#'
#' @format The \code{data.frame} with 4 variables and 43 entries contains
#'   meta-data for the tree species covered in the European Atlas of Forest Tree
#'   Species. \describe{ \item{\code{botanical}}{the species name as noted in
#'   the European Atlas of Forest Tree Species} \item{\code{english}}{the
#'   english name} \item{\code{rpp}}{a binary variable whether the 'relative
#'   probability of presence' dataset is available for this species}
#'   \item{\code{mhs}}{a binary variable whether the 'maximum habitat
#'   suitability' dataset is available for this species}}
"meta_efta"

#' References of the MODIS dataset
#'
#' @format The \code{list} with 12 elements contains the \code{bibentry} objects
#'   that are needed to include a reference for the supported MODIS datasets.
"ref_modis"

#' Projections which are in use in lomm
#'
#' @format List with 5 elemens. \describe{ \item{\code{laea}}{Lambert azimuthal
#'   equal-area projection,\cr Based on the Geodetic Reference System 1980 (GRS
#'   80) Ellipsoid} \item{\code{longlat}}{Longitude/Latitude "projection",\cr
#'   Based on the World Geodetic System 1985 (WGS 84) Ellipsoid}
#'   \item{\code{utm}}{Universal Transverse Mercator "projection", zone 32,\cr
#'   Based on the World Geodetic System 1985 (WGS 84) Ellipsoid}
#'   \item{\code{sinu}}{Sinusoidal projection,\cr Based on a pseudocylinder}
#'   \item{\code{tmerc}}{Transverse Mercator projection,\cr Based on the Bessel
#'   ellipsoid} }
"projs"

#' Default visualising theme
#'
"theme_rt"