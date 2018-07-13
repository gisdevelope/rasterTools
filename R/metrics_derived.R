#' Derived metric: Class proportion of the Landscape
#'
#' Proportion of the landscape window occupied by classes.
#' @format A list of 3 elements:
#' \describe{
#'   \item{a_c}{Area per class: list(operator = "mArea", scale = "class")}
#'   \item{a_p}{Area of the window: list(operator = "mArea", scale = "window")}
#'   \item{equation}{"a_c / a_w * 100"}
#' }
#' @export

mCPA <- list(a_c = list(operator = "mArea", scale = "class"),
             a_w = list(operator = "mArea", scale = "window"),
             equation = "a_c / a_w * 100")

#' Derived metric: Largest Patch Index
#'
#' Proportion of the landscape window occupied by the largest patch.
#' @format A list of 3 elements:
#' \describe{
#'   \item{a_w}{Area of the window: list(operator = "mArea", scale = "window")}
#'   \item{a_p}{Area per patch: list(operator = "mArea", scale = "patch")}
#'   \item{equation}{"max(a_p) / a_w * 100"}
#' }
#' @export

mLPI <- list(a_w = list(operator = "mArea", scale = "window"),
             a_p = list(operator = "mArea", scale = "patch"),
             equation = "max(a_p) / a_w * 100")

#' Derived metric: Edge density
#'
#' Length of edges of patches per hectare of the landscape window.
#' @format A list of 3 elements:
#' \describe{
#'   \item{p_p}{Perimeter per patch: list(operator = "mPerimeter", scale = "patch")}
#'   \item{a_w}{Area of the window: list(operator = "mArea", scale = "window")}
#'   \item{equation}{"p_p / a_w * 10000"}
#' }
#' @export

mDE <- list(p_p = list(operator = "mPerimeter", scale = "patch"),
            a_w = list(operator = "mArea", scale = "window"),
            equation = "p_p / a_w * 10000")

#' Derived metric: Edge density
#'
#' Length of edges of classes per hectare of the landscape window.
#' @format A list of 3 elements:
#' \describe{
#'   \item{p_c}{Perimeter per class: list(operator = "mPerimeter", scale = "class")}
#'   \item{a_w}{Area of the window: list(operator = "mArea", scale = "window")}
#'   \item{equation}{"p_c / a_w * 10000"}
#' }
#' @export

mDEc <- list(p_c = list(operator = "mPerimeter", scale = "class"),
             a_w = list(operator = "mArea", scale = "window"),
             equation = "p_c / a_w * 10000")

#' Derived metric: Patch density
#'
#' Number of patches per hectare of the landscape window.
#' @format A list of 3 elements:
#' \describe{
#'   \item{n_p}{Number of patches: list(operator = "mNumber", scale = "patch")}
#'   \item{a_w}{Area of the window: list(operator = "mArea", scale = "window")}
#'   \item{equation}{"n_p / a_w * 10000"}
#' }
#' @export

mDP <- list(n_p = list(operator = "mNumber", scale = "patch"),
            a_w = list(operator = "mArea", scale = "window"),
            equation = "n_p / a_w * 10000")

#' Derived metric: Perimeter-Area ratio
#'
#' Ratio of the perimeter per area of each patch.
#' @format A list of 3 elements:
#' \describe{
#'   \item{p_p}{Perimeter per patch: list(operator = "mPerimeter", scale = "patch")}
#'   \item{a_p}{Area per patch: list(operator = "mArea", scale = "patch")}
#'   \item{equation}{"p_p / a_p * 10000"}
#' }
#' @export

mPAR <- list(p_p = list(operator = "mPerimeter", scale = "patch"),
             a_p = list(operator = "mArea", scale = "patch"),
             equation = "p_p / a_p * 10000")

#' Derived metric: Percentage of Like Adjacencies
#'
#' Percent of the adjacencies per class that are like adjacencies.
#' @format A list of 3 elements:
#' \describe{
#'   \item{g_ii}{like adjacencies based on doble count: list(operator = "mAdjacency")}
#'   \item{g_ik}{sum of paired adjacencies based on double count: list(operator = "mAdjacency", type = "pairedSum")}
#'   \item{equation}{"(g_ii / g_ik) * 100"}
#' }
#' @export

mPLA <- list(g_ii = list(operator = "mAdjacency"),
             g_ik = list(operator = "mAdjacency", type = "pairedSum"),
             equation = "(g_ii / g_ik) * 100")


# aggregation.index = function(a, g) {
#   n = trunc(sqrt(a))
#   m = a - n^2
#   if (m == 0)
#     maxg = 2 * n * (n - 1)
#   if (m <= n)
#     maxg = 2 * n * (n - 1) + 2 * m - 1
#   if (m > n)
#     maxg = 2 * n * (n - 1) + 2 * m - 2
#   return((g/maxg) * 100)
# }
# tout$aggregation.index = aggregation.index(sum(out.patch$n.cell),
#                                            sum(out.patch$n.edges.internal)/2)
