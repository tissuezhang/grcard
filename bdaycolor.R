#' Birthday Card Colors
#'
#' @param cakebottom cake bottom color
#' @param caketop cake top color
#' @param candle1 candle1 color
#' @param candle2 candle2 color
#' @param candle3 candle3 color
#' @param candle4 candle4 color
#' @param candle5 candle5 color
#' @param candle6 candle6 color
#' @param line1 line1 color
#' @param line2 line2 color
#' @param line3 line3 color
#' @param star1 star1 color
#' @param star2 star2 color
#'
#' @return
#' @export
#'
#' @examples
bdaycolor<-function(cakebottom, caketop, candle1, candle2,candle3, candle4,
                    candle5, candle6, line1, line2, line3, star1, star2)
{
  c(candle1, candle2,candle3, candle4, candle5, candle6,
    caketop, cakebottom, line1, line2, line3, star1, star2)
}
