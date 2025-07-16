# app.R

library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(stringi)
library(openxlsx)
library(shinyWidgets)

# 1) Load & clean data
load("C:/Users/bartu/Desktop/consumption-map-app/data.rda")  # expects `data` object
if (nrow(data) >= 487) data <- data[-487, ]

# 2) Prepare spatial data with recoded province names
turkey_prov <- ne_states(country = "Turkey", returnclass = "sf") %>%
  select(name, geometry) %>%
  mutate(
    ascii = toupper(stri_trans_general(name, "Latin-ASCII")),
    province = case_when(
      ascii %in% c("DUZCE","DUEZCE")                  ~ "DUZCE",
      ascii == "CANAKKALE"                              ~ "CANAKKALE",
      ascii == "KUTAHYA"                                ~ "KUTAHYA",
      ascii %in% c("ZINGULDAK","ZONGULDAK")           ~ "ZONGULDAK",
      ascii %in% c("K. MARAS","KHARAMANMARAS")        ~ "KAHRAMANMARAS",
      ascii == "KINKKALE"                               ~ "KIRIKKALE",
      TRUE                                                ~ ascii
    )
  )

# 3) Prepare consumption data
df <- data %>%
  rename(
    province      = Province,
    consumer_type = `Consumer Type`,
    amount_mwh    = Quantity
  ) %>%
  mutate(
    province   = toupper(province),
    amount_gwh = amount_mwh / 1000
  )

# 4) UI inputs
consumer_types <- sort(unique(df$consumer_type))
if ("Province Total" %in% consumer_types) {
  consumer_types <- c("Province Total", setdiff(consumer_types, "Province Total"))
}
province_list <- sort(unique(df$province))
gwh_range     <- range(df$amount_gwh, na.rm = TRUE)

# 5) UI definition
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly",
                   bg = "white",
                   fg = "#2E4053",
                   primary = "#117A65",
                   secondary = "#F39C12"),
  tags$head(
    tags$style(HTML(
      ".panel-custom { background-color: #F8F9F9; border-radius: 8px; padding: 15px; }
       .shiny-input-container { margin-bottom: 20px; }
       .leaflet-container { border: 2px solid #117A65; }
       h2, h3, h4 { color: #117A65; }
       .footer { margin-top: 20px; font-size: 0.9em; color: #555; text-align: center; }
       .footer a { color: #117A65; margin: 0 5px; }
       body { font-family: 'Lato', sans-serif; }"
    ))
  ),
  # Main title
  titlePanel(div(style = "font-weight:700; font-size:30px; text-align:center; margin-bottom:20px;",
                 "Distribution of Billed Consumption by Province and Consumer Type")),
  sidebarLayout(
    sidebarPanel(class = "panel-custom", width = 3,
                 pickerInput("consumer_type", "Consumer Type:",
                             choices = consumer_types,
                             selected = "Province Total",
                             multiple = FALSE,
                             options = list(`live-search` = TRUE)),
                 sliderInput("gwh_range", "Consumption Range (GWh):",
                             min = gwh_range[1], max = gwh_range[2],
                             value = gwh_range, step = diff(gwh_range)/100),
                 pickerInput("province_multi", "Provinces:",
                             choices = province_list,
                             multiple = TRUE,
                             selected = province_list,
                             options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 8)),
                 downloadButton("download_data", "Download Data (.xlsx)")
    ),
    mainPanel(class = "panel-custom", width = 9,
              leafletOutput("map", height = "450px"),
              # Subtitle/source
              h4("Source: ELECTRICITY MARKET | 2024 MARKET DEVELOPMENT REPORT", style = "text-align:center; color:#555; margin-top:10px;"),
              # Statistics section
              h3("Statistics"),
              fluidRow(
                column(4,
                       h4("Top 5 Provinces"),
                       tableOutput("top5"),
                       plotOutput("top5_pct", height = "200px")
                ),
                column(4,
                       h4("Bottom 5 Provinces"),
                       tableOutput("bottom5"),
                       plotOutput("bottom5_pct", height = "200px")
                ),
                column(4,
                       h4("Distribution Chart"),
                       plotOutput("hist", height = "300px")
                )
              ),
              tags$hr(),
              # Footer info
              div(class = "footer",
                  "Developed by Fikret Bartu Yurdacan | info@medyanistdanismanlik.com | ",
                  a(href = "https://www.linkedin.com/in/fikret-bartu-yurdacan/", "LinkedIn"),
                  " | ", a(href = "https://wa.me/05394893522", "WhatsApp"),
                  " | ", a(href = "https://github.com/bartuyurdacan", "GitHub")
              )
    )
  )
)

# 6) Server logic
server <- function(input, output, session) {
  # Reactive filtered data
  filtered_data <- reactive({
    df %>%
      filter(
        consumer_type == input$consumer_type,
        amount_gwh >= input$gwh_range[1],
        amount_gwh <= input$gwh_range[2],
        province %in% input$province_multi
      )
  })
  
  # Stats calculations
  stats <- reactive({
    d <- filtered_data() %>% arrange(desc(amount_gwh))
    list(
      top5    = head(d, 5),
      bottom5 = tail(d, 5),
      hist    = d
    )
  })
  
  # Spatial join for map
  map_data <- reactive({
    turkey_prov %>% left_join(filtered_data(), by = "province")
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    md <- map_data()
    pal <- colorNumeric("viridis", domain = md$amount_gwh, na.color = "transparent")
    leaflet(md) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(amount_gwh),
        weight    = 1,
        color     = "#fff",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 2, color = "#444", bringToFront = TRUE),
        label = ~lapply(sprintf("<strong>%s</strong><br/>%.1f GWh", province, amount_gwh), htmltools::HTML)
      ) %>%
      addLegend(
        pal = pal,
        values = ~amount_gwh,
        position = "bottomright",
        title = paste(input$consumer_type, "(GWh)"),
        labFormat = labelFormat(suffix = " GWh", big.mark = ",")
      )
  })
  
  # Top 5 table
  output$top5 <- renderTable({ stats()$top5 %>% transmute(Province = province, `Consumption (GWh)` = round(amount_gwh, 2)) }, striped = TRUE, hover = TRUE)
  
  # Bottom 5 table
  output$bottom5 <- renderTable({ stats()$bottom5 %>% transmute(Province = province, `Consumption (GWh)` = round(amount_gwh, 2)) }, striped = TRUE, hover = TRUE)
  
  # Top 5 percentage chart
  output$top5_pct <- renderPlot({
    df5 <- stats()$top5 %>% mutate(pct = amount_gwh / sum(filtered_data()$amount_gwh) * 100)
    ggplot(df5, aes(x = reorder(province, pct), y = pct)) +
      geom_col(fill = "darkgreen") + coord_flip() +
      labs(x = NULL, y = "% Share", title = "Top 5 Share") + theme_minimal()
  })
  
  # Bottom 5 percentage chart
  output$bottom5_pct <- renderPlot({
    df5 <- stats()$bottom5 %>% mutate(pct = amount_gwh / sum(filtered_data()$amount_gwh) * 100)
    ggplot(df5, aes(x = reorder(province, -pct), y = pct)) +
      geom_col(fill = "darkred") + coord_flip() +
      labs(x = NULL, y = "% Share", title = "Bottom 5 Share") + theme_minimal()
  })
  
  # Distribution histogram
  output$hist <- renderPlot({
    ggplot(stats()$hist, aes(x = amount_gwh)) +
      geom_histogram(fill = "steelblue", bins = 20) +
      labs(x = "GWh", y = "Number of Provinces") + theme_minimal()
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() paste0("billed_consumption_", Sys.Date(), ".xlsx"),
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Data")
      writeData(wb, sheet = "Data", filtered_data())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
