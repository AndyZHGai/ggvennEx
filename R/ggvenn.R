#' Venn plot based on ggplot2, forked from yanlinlin82/ggvenn
#'
#' @param data
#' @param text.size the size of label and text size, default value is 4
#'
#' @return
#' @export
#'
#' @author ZHonghui Gai
#' @examples
#' data <- read.csv(file = "genus.Syn.csv", row.names = 1)
#' data <- ggvennEx:::vennlist(data)
#' v.d <- venn.data(data)
#' ggvenn(v.d)
ggvenn <- function(data, text.size = 4,
                   label.color = "white") {
  if (label.color == "white") {
    color <- "white"
  }else{
    color <- "black"
  }
  data$shapes  |>
    transform(group = LETTERS[group])  |>
    ggplot() +
    geom_polygon(aes(x = x, y = y, group = group, fill = group),
                 alpha = 0.5) +
    geom_polygon(aes(x = x, y = y, group = group, fill = NA),
                 color = "white", size = 0.5, alpha = 0.1, linetype = 1) +
    geom_text(data = data$labels,
              aes(x = x, y = y, label = text, hjust = hjust, vjust = vjust),
              color = color, fontface = "bold.italic", size = text.size*1.2) +
    geom_text(data = data$texts,
              aes(x = x, y = y, label = n, hjust = hjust, vjust = vjust),
              color = "black", fontface = "bold", size = text.size) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    scale_y_continuous(expand = c(0.01, 0.01)) +
    coord_fixed() + theme_void() + theme(legend.position = 0)
}
