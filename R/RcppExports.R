# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

matInGeomC <- function(mat, geom, negative) {
    .Call('_rasterTools_matInGeomC', PACKAGE = 'rasterTools', mat, geom, negative)
}

cellToPointsC <- function(mat) {
    .Call('_rasterTools_cellToPointsC', PACKAGE = 'rasterTools', mat)
}

countCellsC <- function(mat) {
    .Call('_rasterTools_countCellsC', PACKAGE = 'rasterTools', mat)
}

countEdgesC <- function(mat) {
    .Call('_rasterTools_countEdgesC', PACKAGE = 'rasterTools', mat)
}

countAdjacenciesC <- function(mat, countDouble) {
    .Call('_rasterTools_countAdjacenciesC', PACKAGE = 'rasterTools', mat, countDouble)
}

diamondSquareC <- function(mat, stepSize, roughness, startDev) {
    .Call('_rasterTools_diamondSquareC', PACKAGE = 'rasterTools', mat, stepSize, roughness, startDev)
}

getValuesMatC <- function(mat) {
    .Call('_rasterTools_getValuesMatC', PACKAGE = 'rasterTools', mat)
}

isBinaryC <- function(mat) {
    .Call('_rasterTools_isBinaryC', PACKAGE = 'rasterTools', mat)
}

meijsterDistanceC <- function(mat, method) {
    .Call('_rasterTools_meijsterDistanceC', PACKAGE = 'rasterTools', mat, method)
}

morphC <- function(mat, kernel, value, blend, merge, rotateKernel, strictKernel) {
    .Call('_rasterTools_morphC', PACKAGE = 'rasterTools', mat, kernel, value, blend, merge, rotateKernel, strictKernel)
}

scaleMatrixC <- function(mat, range) {
    .Call('_rasterTools_scaleMatrixC', PACKAGE = 'rasterTools', mat, range)
}

subNumNumC <- function(mat, replace, with) {
    .Call('_rasterTools_subNumNumC', PACKAGE = 'rasterTools', mat, replace, with)
}

subChrIntC <- function(mat, replace, with) {
    .Call('_rasterTools_subChrIntC', PACKAGE = 'rasterTools', mat, replace, with)
}

