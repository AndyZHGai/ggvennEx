#' A transparent layer function
#'
#' @param ... other paramaters
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' theme_transparent()
theme_transparent <- function (...) {
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        ...)
}
