## Standard theme
gg_theme <-
  theme_classic() +
  theme(
    plot.title = element_text(size = 20),            # Increase title font size
    axis.title.x = element_text(size = 16),          # Increase x-axis label font size
    axis.title.y = element_text(size = 16),          # Increase y-axis label font size
    axis.text.x = element_text(size = 12),           # Increase x-axis tick label font size
    axis.text.y = element_text(size = 12),           # Increase y-axis tick label font size
    legend.title = element_text(size = 14),          # Increase legend title font size
    legend.text = element_text(size = 12)            # Increase legend text font size
  )
