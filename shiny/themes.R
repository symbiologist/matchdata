library(ggthemes) # themes
library(patchwork) # compose ggplots
library(rcartocolor) # color palette
library(RColorBrewer) # color palette
library(ggforce) # plotting

theme_custom <- function(base_size = 16, 
                         base_family = 'Arial', 
                         axis = T, 
                         grid = F, 
                         legend_position = 'top') {
  
  if(axis) {
    axis_element <- element_line(color = 'black')
  } else {
    axis_element <- element_blank()
  }
  
  if(grid) {
    grid_element <- element_line(color = 'grey90')
  } else {
    grid_element <- element_blank()
  }
  
  theme_foundation(base_size = base_size, base_family = base_family) + 
    theme(plot.title = element_text(face = "plain", size = 16, hjust = 0.5),
          text = element_text(),
          panel.background = element_rect(color = NA),
          plot.background = element_rect(color = NA),
          panel.border = element_rect(color = 'black', fill = 'transparent'),
          axis.title = element_text(face = "plain", size = rel(1)),
          axis.title.y = element_text(angle=90, vjust = 0.5),
          axis.title.x = element_text(vjust = 0),
          axis.text = element_text(color = 'black', hjust = 0.5),
          axis.line = element_blank(),
          panel.grid.major = grid_element,
          panel.grid.minor = element_blank(),
          legend.key = element_rect(color = NA),
          legend.key.size = unit(1, "cm"),
          legend.background = element_rect(fill = NA),
          legend.spacing = unit(0, "cm"),
          legend.title = element_blank(),
          legend.position = legend_position,
          legend.text = element_text(size = 12),
          strip.background = element_rect(color="black",fill="#f0f0f0"),
          strip.text = element_text(face="plain", size = 14))
}