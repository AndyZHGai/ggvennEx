#' Transform a tiff plot to a ggplot2 object, that can be used by patchwork package.
#'
#' @param bitmap a tiff picture
#'
#' @return a ggplot2 object
#' @export
#'
#' @author ZhonghuiGai
#' @examples
#' ggbitmap("venn.tiff")
ggbitmap <- function(bitmap){
  p1 <- ggplot() + theme_void() +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme(plot.margin = unit(rep(0, 4), "lines"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background =  element_blank(),
          panel.border = element_blank())
  p2 <- tiff::readTIFF(source = bitmap)
  p <- p1 + annotation_custom(grid::rasterGrob(p2),
                              xmin = 0,
                              xmax = 1,
                              ymin = 0,
                              ymax = 1)
  return(p)
}
