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
#'   of the layer} \item{\code{description}}{a human-readable description of the
#'   layer} \item{\code{units}}{the unit in which the data are saved}
#'   \item{\code{data_type}}{bit number which determines the length of the
#'   values} \item{\code{fill_min}}{minimum fill value}
#'   \item{\code{fill_max}}{maximum fill value} \item{\code{valid_min}}{the
#'   minimum valid value} \item{\code{valid_max}}{the maximum valid value}
#'   \item{\code{correction_factor}}{a correction factor}}
"meta_modis"

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