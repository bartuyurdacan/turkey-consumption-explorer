# Turkey Billed Consumption Explorer

Interactive Shiny app visualizing billed electricity consumption by province & consumer type.  
**Source:** EPDK 2024 Market Development Report  
**Developer:** Fikret Bartu Yurdacan — info@medyanistdanismanlik.com  

## Run Locally

```r
install.packages(c(
  "shiny","bslib","sf","dplyr","leaflet","ggplot2",
  "rnaturalearth","rnaturalearthdata","scales",
  "stringi","openxlsx","shinyWidgets"
))
shiny::runApp("consumption_map.R")
