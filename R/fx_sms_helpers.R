
fx_plot_theme <- function() {

  list(

    light_theme =
      theme_minimal() +
      theme(strip.text.y = element_text(angle = 0, face = "bold"),
            strip.text.x = element_text(face = "bold"),
            panel.background = element_rect(color = "gray"),
            panel.grid.major.x = element_line(color = "gray", linetype = 3),
            panel.grid.minor.x = element_line(color = "gray", linetype = 3),
            legend.title.align = 0.5),

    dark_theme =
      theme_minimal() +
      theme(panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.text.y = element_text(angle = 0, face = "bold"),
            strip.text.x = element_text(face = "bold"),
            panel.grid.major.y = element_line(linetype = "solid", color = "#707073"),
            panel.background = element_rect(color = "gray"),
            rect = element_rect(fill = "#2a2a2b"),
            legend.title.align = 0.5)
      )

}
