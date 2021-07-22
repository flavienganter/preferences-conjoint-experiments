# theme inspired by theme_donors by Andrew Heiss (andrewheiss.com)

theme_fg <- function(base_size = 14, base_family = "Roboto Condensed") {
  update_geom_defaults("bar", list(fill = "grey20"))
  update_geom_defaults("line", list(colour = "grey30"))
  update_geom_defaults("label", list(family = "Roboto Condensed", face = "plain"))
  update_geom_defaults("text", list(family = "Roboto Condensed", face = "plain"))
  # update_geom_defaults("label_repel", list(family = "Work Sans", face = "plain))
  # update_geom_defaults("text_repel", list(family = "Work Sans", face = "plain))
  
  ret <- theme_bw(base_size, base_family) +
    theme(panel.background = element_rect(fill = "#ffffff", colour = NA),
          title = element_text(size = rel(1.1), vjust = 1.2, 
                               family = "Roboto Condensed Bold", face = "plain"),
          plot.subtitle = element_text(size = rel(0.8), 
                                       family = "Roboto Condensed", face = "plain"),
          plot.caption = element_text(margin = margin(t = 10), size = rel(0.6),
                                      family = "Roboto Condensed", face = "plain"),
          panel.border = element_rect(color = "grey50", fill = NA, size = 0.15),
          panel.spacing = unit(1, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.25, colour = "grey90"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size = rel(0.8), 
                                    family = "Roboto Condensed", face = "plain"),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)),
          legend.position = "bottom",
          legend.title = element_text(size = rel(0.8)),
          legend.key.size = unit(.7, "lines"),
          legend.key = element_blank(),
          legend.spacing = unit(0.1, "lines"),
          strip.text = element_text(size = rel(0.9), hjust = 0,
                                    family = "Roboto Condensed", face = "plain"),
          strip.background = element_rect(fill = "#ffffff", colour = NA))
  ret
}
