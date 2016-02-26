#' Life Expectancy Lost in Age x
#'
#' Life expectancy lost by those who die in age interval [x, x+w).
#'
#' @param x  start of age interval
#' @param ex life expectancy at age x
#' @param wx width of age interval x [x, x+wx)
#' @param ax average time spent in age interval x before dying in that interval
#'
#' @details We assume that `x`, `ex`, `wx` and `ax` are columns of a single
#'   standard format lifetable: 1) age groups are ordered from low to high. 2)
#'   there are no gaps between subsequent age groups. 3) The last age group is
#'   open to the right.
#'
#'   If only `x` and `ex` are specified an age interval of 1, and an ax of 0.5
#'   are assumed.
#'
#' @examples
#' # life expectancy lost in age x for a 1x1 lifetable of Swedish females
#' swe      <- subset(sweden1x1, period == 1757 & sex == "female")
#' ExDagger(x = swe$x, ex = swe$ex)
#'
#' # life expectancy lost in age [x,x+wx) for a 5x5 lifetable of Swedish females
#' swe      <- subset(sweden5x5, period == "1755-1759" & sex == "female")
#' swe$wx[is.na(swe$wx)] <- 2 # last age group (110+) is assumed to be 2 years wide
#' ExDagger(x = swe$x, ex = swe$ex, wx = swe$wx, ax = swe$ax)
#'
#' @export
ExDagger <- function (x, ex, wx = 1, ax = 0.5) {
  exwx <- c(ex[-1], ex[length(ex)])
  A <- ax/wx
  return(
    A * exwx + (1 - A) * ex
  )
}

#' Total Life Expectancy Lost
#'
#' Life expectancy lost due to death.
#'
#' @param dx       age distribution of deaths
#' @param exdagger life expectancy lost at age x
#' @param radix    initial lifetable population (dx scaling factor)
#'
#' @examples
#' # total life expectancy lost for a 1x1 lifetable of Swedish females
#' swe      <- subset(sweden1x1, period == 1757 & sex == "female")
#' exdagger <- ExDagger(x = swe$x, ex = swe$ex)
#' EDagger(dx = swe$dx, exdagger = exdagger, radix = 100000)
#'
#' # total life expectancy lost for a 5x5 lifetable of Swedish females
#' swe      <- subset(sweden5x5, period == "1755-1759" & sex == "female")
#' swe$wx[is.na(swe$wx)] <- 2 # last age group (110+) is assumed to be 2 years wide
#' exdagger <- ExDagger(x = swe$x, ex = swe$ex, wx = swe$wx, ax = swe$ax)
#' EDagger(dx = swe$dx, exdagger = exdagger, radix = 100000)
#'
#' @export
EDagger <- function (dx, exdagger, radix = 1) {
  return(
    sum(dx*exdagger) / radix
  )
}

#' Keyfitz's Entropy
#'
#' A measure for the lifespan equality in a population.
#'
#' @param edagger total life expectancy lost due to death
#' @param e0      life expectancy at birth
#'
#' @examples
#' # lifespan equality for a 1x1 lifetable of Swedish females
#' swe      <- subset(sweden1x1, period == 1757 & sex == "female")
#' exdagger <- ExDagger(x = swe$x, ex = swe$ex)
#' edagger  <- EDagger(dx = swe$dx, exdagger = exdagger, radix = 100000)
#' KeyfzEntro(edagger = edagger, e0 = swe$ex[1])
#'
#' # lifespan equality for a 5x5 lifetable of Swedish females
#' swe      <- subset(sweden5x5, period == "1755-1759" & sex == "female")
#' swe$wx[is.na(swe$wx)] <- 2 # last age group (110+) is assumed to be 2 years wide
#' exdagger <- ExDagger(x = swe$x, ex = swe$ex, wx = swe$wx, ax = swe$ax)
#' edagger  <- EDagger(dx = swe$dx, exdagger = exdagger, radix = 100000)
#' KeyfzEntro(edagger = edagger, e0 = swe$ex[1])
#'
#' @export
KeyfzEntro <- function (edagger, e0) {
  return(
    edagger / e0
  )
}

#' Single Year Period Life-tables for Sweden 1751-2014 by Sex
#'
#' A dataset containing period life-tables for Sweden years 1751-2014 by sex in
#' single year period and age intervals.
#'
#' @details
#'   The dataset has been altered from the original form provided by the Human
#'   Mortality database.
#'
#' @format A data frame with 58,608 rows and 11 variables:
#' \describe{
#'   \item{sex}{females or males}
#'   \item{period}{period in years}
#'   \item{x}{start of age interval in years}
#'   \item{mx}{mortality rate in age interval [x, x+wx)}
#'   \item{qx}{probability of death in age interval [x, x+wx)}
#'   \item{ax}{average time spent in age interval [x, x+wx) when dying in that interval}
#'   \item{lx}{survivors at age x}
#'   \item{dx}{deaths in age interval [x, x+wx)}
#'   \item{Lx}{total person-years lived in age interval [x, x+wx)}
#'   \item{Tx}{total person-years yet to live starting age x}
#'   \item{ex}{life-expectancy at age x}
#' }
#'
#' @source The Human Mortality Database \url{http://www.mortality.org/}
"sweden1x1"

#' Five Year Period Life-tables for Sweden 1755-2014 by Sex
#'
#' A dataset containing period life-tables for Sweden years 1755-2014 by sex in
#' five year period and age intervals.
#'
#' @details
#'   Infant and early childhood mortality are given in 1 and 4 year age intervals.
#'
#'   The dataset has been altered from the original form provided by the Human
#'   Mortality database.
#'
#' @format A data frame with 2,496 rows and 12 variables:
#' \describe{
#'   \item{sex}{females or males}
#'   \item{period}{period range in years [start, end]}
#'   \item{x}{start of age interval in years}
#'   \item{wx}{width of age interval in years}
#'   \item{mx}{mortality rate in age interval [x, x+wx)}
#'   \item{qx}{probability of death in age interval [x, x+wx)}
#'   \item{ax}{average time spent in age interval [x, x+wx) when dying in that interval}
#'   \item{lx}{survivors at age x}
#'   \item{dx}{deaths in age interval [x, x+wx)}
#'   \item{Lx}{total person-years lived in age interval [x, x+wx)}
#'   \item{Tx}{total person-years yet to live starting age x}
#'   \item{ex}{life-expectancy at age x}
#' }
#'
#' @source The Human Mortality Database \url{http://www.mortality.org/}
"sweden5x5"