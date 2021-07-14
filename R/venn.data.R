#' Generate input data for ggvenn()
#'
#' @param data input data, must be a list, this can be generated form vennlist() function
#'
#' @return a list as the input of ggvenn() function
#' @export
#'
#' @author ZHonghui Gai
#' @examples
#' data <- read.csv(file = "genus.Syn.csv", row.names = 1)
#' data <- ggvennEx:::vennlist(data)
#' v.d <- venn.data(data)
venn.data <- function(data) {
  if (!is.list(data)) stop("`data` should be a list")
  columns <- names(data)
  a <- unique(unlist(data[columns]))
  if (length(columns) == 2) {
    # step 1 generate data for ellipse drawing
    d <- rbind(ellipse(1L, -1/2, 0, 1), ellipse(2L, 1/2, 0, 1))
    # step 2 generate data for the position of text
    d1 <- data.frame(name = c("A", "B", "AB"),
                       x = c(-0.8, 0.8, 0),
                       y = c(0, 0, 0),
                       hjust = c(0.5, 0.5, 0.5),
                       vjust = c(0.5, 0.5, 0.5),
                       A = c(TRUE, FALSE, TRUE),
                       B = c(FALSE, TRUE, TRUE))  |>
      transform(n = 0, text = "")
    for (i in 1:nrow(d1)) {
      idx <- ((!xor(d1$A[[i]], a %in% data[[columns[[1]]]])) &
                  (!xor(d1$B[[i]], a %in% data[[columns[[2]]]])))
      d1$n[[i]] <- sum(idx)
      d1$text[[i]] <- paste(a[idx], collapse = ",")
    }
    # step 3 generate data for the position of group label
    d2 <- data.frame(name = c("A", "B"),
                       x = c(-0.6, 0.6),
                       y = c(0.75, 0.75),
                       hjust = c(0.5, 0.5),
                       vjust = c(0, 0))
    } else if (length(columns) == 3) {
      # step 1 generate data for ellipse drawing
      d <- rbind(ellipse(1L, -1/2, (sqrt(3) + 2) / 6, 1),
                 ellipse(2L, 1/2,(sqrt(3) + 2) / 6, 1),
                 ellipse(3L, 0, -(sqrt(3) + 1) / 6, 1))
      # step 2 generate data for the position of text
      d1 <- data.frame(name = c("A", "B", "C", "AB", "AC", "BC", "ABC"),
                       x = c(-0.9, 0.9, 0, 0, -0.55, 0.55, 0),
                       y = c(0.65, 0.65, -0.65, 0.85, 0, 0, 0.2),
                       hjust = rep(0.5, 7),
                       vjust = rep(0.5, 7),
                       A = c(T, F, F, T, T, F, T),
                       B = c(F, T, F, T, F, T, T),
                       C = c(F, F, T, F, T, T, T))  |>
        transform(n = 0, text = "")
      for (i in 1:nrow(d1)) {
        idx <- ((!xor(d1$A[[i]], a %in% data[[columns[[1]]]])) &
                  (!xor(d1$B[[i]], a %in% data[[columns[[2]]]])) &
                  (!xor(d1$C[[i]], a %in% data[[columns[[3]]]])))
        d1$n[[i]] <- sum(idx)
        d1$text[[i]] <- paste(a[idx], collapse = ",")
      }
      # step 3 generate data for the position of group label
      d2 <- data.frame(name = c("A", "B", "C"),
                       x = c(-0.7, 0.7, 0),
                       y = c(1.25, 1.25, -1.2),
                       hjust = rep(0.5, 3),
                       vjust = c(0, 0, 1))
    } else if (length(columns) == 4) {
      # step 1 generate data for ellipse drawing
      d <- rbind(ellipse(1L, -.7, -1/2, .75, 1.5, pi/4),
                 ellipse(2L, -.72+2/3, -1/6, .75, 1.5, pi/4),
                 ellipse(3L, .72-2/3, -1/6, .75, 1.5, -pi/4),
                 ellipse(4L, .7, -1/2, .75, 1.5, -pi/4))
      # step 2
      d1 <- data.frame(name = c("A", "B", "C", "D", "AB", "BC", "CD","AC", "BD", "AD", "ABC", "BCD", "ACD", "ABD", "ABCD"),
                       x = c(-1.5, -0.6, 0.6, 1.5, -0.9, 0, 0.9, -0.8, 0.8, 0, -0.5, 0.5, -0.3, 0.3, 0),
                       y = c(0, 0.7, 0.7, 0, 0.3, 0.4, 0.3, -0.9, -0.9, -1.4, -0.2, -0.2, -1.1, -1.1, -0.7),
                       hjust = rep(0.5, 15),
                       vjust = rep(0.5, 15),
                       A = c(T, F, F, F, T, F, F, T, F, T, T, F, T, T, T),
                       B = c(F, T, F, F, T, T, F, F, T, F, T, T, F, T, T),
                       C = c(F, F, T, F, F, T, T, T, F, F, T, T, T, F, T),
                       D = c(F, F, F, T, F, F, T, F, T, T, F, T, T, T, T))  |>
        transform(n = 0, text = "")
      for (i in 1:nrow(d1)) {
        idx <- ((!xor(d1$A[[i]], a %in% data[[columns[[1]]]])) &
                  (!xor(d1$B[[i]], a %in% data[[columns[[2]]]])) &
                  (!xor(d1$C[[i]], a %in% data[[columns[[3]]]])) &
                  (!xor(d1$D[[i]], a %in% data[[columns[[4]]]])))
        d1$n[[i]] <- sum(idx)
        d1$text[[i]] <- paste(a[idx], collapse = ",")
      }
      # step 3
      d2 <- data.frame(name = c("A", "B", "C", "D"),
                       x = c(-1.3, -0.8, 0.8, 1.3),
                       y = c(-1.1, 1.0, 1.0, -1.1),
                       hjust = c(1, 0.5, 0.5, 0),
                       vjust = c(1, 0, 0, 1))
    } else {
      stop("list `data` or vector `column` should be length between 2 and 4")
    }
    d2 <- d2  |>
      transform(text = columns)
  list(shapes = d, texts = d1, labels = d2)
}


ellipse <- function(group, x.offset = 0, y.offset = 0, radius = 1,
                    radius.b = radius, theta.offset = 0, length.out = 100) {
  data.frame(group = group,
             theta = seq(0, 2 * pi, length.out = length.out))  |>
    transform(x_raw = radius * cos(theta),
              y_raw = radius.b * sin(theta)) |>
    transform(x = x.offset + x_raw * cos(theta.offset) - y_raw * sin(theta.offset),
              y = y.offset + x_raw * sin(theta.offset) + y_raw * cos(theta.offset))
}
