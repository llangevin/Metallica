# install.packages("quarto")
library(quarto)
quarto::quarto_render(
  "./quarto/Metallica_Shows_Stats/Metallica_Shows_Stats.qmd", 
  output_format = c("gfm")
)
