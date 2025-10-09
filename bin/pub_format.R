# code defining ggplot theme to be used when generating manuscript figures
library(ggplot2)

pub_theme <- function() {
  ggplot2::theme_bw() +
    theme(
      # Remove all grid lines
      panel.grid = element_blank(),

      # Text #
      text = element_text(
        family = "Helvetica",
        size = 10,
        color = "black"
        ),
      axis.title = element_text(
        family = "Helvetica",
        size = 10,
        face = "bold"
        ),
      axis.text = element_text(
        family = "Helvetica",
        size = 10,
        face = "bold"),
        legend.title = element_text(
          family = "Helvetica",
          size = 10,
          face = "bold"
        ),
        legend.text = element_text(
          family = "Helvetica",
          size = 10
        )
    )
}
