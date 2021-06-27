#' Draw a venn plot using VennDiagram and transform it to ggplot2 object
#'
#' @param data a data frame containing grouping information
#' @param col colour used to draw venn plot, the length is equal to the levels of group
#' @param delete boolean value to delete the original .tiff file, default value id TRUE
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' data <- read.csv(file = "genus.Syn.csv", row.names = 1)
#' vennlist(data)
#' ggvennplot(data = data, col = c("red", "green", "blue", "orange"))
#' ggvennplot(data = data[data$group != "3W", ], col = c("red", "blue", "orange"))

ggvennplot <- function(data, col = c("#5dcca0", "#ff445d"),
                       delete = TRUE){
data.list <- vennlist(data)
file.name <- paste0(sample(LETTERS, 1), sample(letters, 1), sample(1:10, 1), ".tiff")
VennDiagram::venn.diagram(x = data.list, cex = 1.5,
                          filename = file.name,
                          imagetype = "tiff",
                          fill = col, col = "grey99",
                          cat.col = "black",
                          cat.cex = 1.5, cat.fontface = "bold",
                          lwd = 1.5, height = 1500, width = 1500,
                          resolution = 300,
                          fontface = "bold")
file <- list.files()
ind <- grep("log", file)
file.remove(file[ind])
p <- ggbitmap(file.name)
if (delete) {file.remove(file.name)}
return(p)
}

vennlist <- function(data){
  if ("group" != colnames(data)[1]) {
    stop("The first column must be the grouping variable!")
  }
  groupName <- unique(data$group)  |> as.character()
  data.list <- list()
  for (i in 1:length(groupName)) {
    venn <- subset(data, group==groupName[i])
    venn <- venn[-1]
    venn2 <- apply(venn, 2, sum)
    data.list[[i]] <- names(venn)[!venn2==0]
    names(data.list)[i] <- groupName[i]
  }
  return(data.list)
}
