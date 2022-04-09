# Copy necessary files
setwd(rprojroot::find_rstudio_root_file())

file.copy(list.files('analysis/output', full.names = T),
          'shiny/data/',
          overwrite = TRUE, 
          recursive = TRUE)


