#' Insert a plot to another plot using ggplot2
#'
#' @param mother.plot The plot which to be inserted
#' @param son.plot The plot to be inserted into other plot
#' @param xmin the position of the son.plot
#' @param xmax the position of the son.plot
#' @param ymin the position of the son.plot
#' @param ymax the position of the son.plot
#' @param venn boolean value
#'
#' @author Zhonghui Gai
#' @return
#' @export
#'
#' @examples
#' p1 <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
#' geom_point() + theme_transparent()
#' p2 <- ggvennplot(data = data[data$group != "3W", ], col = c("red", "blue", "orange"))
#' son.plot(p2, p1, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
son.plot <- function(mother.plot, son.plot, xmin, xmax, ymin, ymax,
                     venn = FALSE){
  if(venn){
    p1 <- mother.plot
    p2 <- tiff::readTIFF(source = son.plot)
    p <- p1 + annotation_custom(grid::rasterGrob(p2),
                                xmin = xmin,
                                xmax = xmax,
                                ymin = ymin,
                                ymax = ymax)
  }else{
    p1 <- mother.plot
    p2 <- son.plot
    p2 <- ggplot2::ggplotGrob(p2)
    p <- p1 + ggplot2::annotation_custom(grob = p2,
                                         xmin = xmin,
                                         xmax = xmax,
                                         ymin = ymin,
                                         ymax = ymax)
  }
  return(p)
}
