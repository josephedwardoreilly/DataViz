require(shiny)
require(shinyWidgets)
require(tidyverse)
require(sf)
require(data.table)
require(ggplot2)
require(viridis)

# Read and wrangle the case data 
x <- fread("ltla_2021-05-17_Cases.csv")
x[, Lacode := areaCode]
x[, `% Change In Number Of New Cases` := newCasesBySpecimenDateChangePercentage]

# Read and prep the shape data
Background <- st_read(
  "LocalAuthorities-lowertier.gpkg",
  layer="7 Background")

# Prep the label alignment
Group_labels <- st_read(
  "LocalAuthorities-lowertier.gpkg",
  layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

# merge on the case data to the shapes
ltladata <- st_read(
  "LocalAuthorities-lowertier.gpkg",
  layer="4 LTLA-2019") %>% 
  left_join(x, by="Lacode")

setDT(ltladata)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  shinyWidgets::setBackgroundColor("#19323C"),
  shinyWidgets::chooseSliderSkin("Modern", color = "#19323C"),
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: white;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')
  )),
  # App title ----

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 2, id = 'sidebar',
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "date",
                  label = "Date:",
                  min = as.Date("2020-02-01","%Y-%m-%d"),
                  max = as.Date(Sys.Date(),"%Y-%m-%d"),
                  value = as.Date("2020-12-01"),
                  timeFormat="%Y-%m-%d")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  

  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number ofs bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    

    ggplot()+
      geom_sf(
        data = Background,
        aes(geometry = geom),
        fill = NA, size = .5, color = 'grey40')+
      geom_sf(
        data=ltladata[date == input$date],  # subset the data table
        aes(geometry=geom, fill=`% Change In Number Of New Cases`),
        colour="Black",
        size=.35)+
      geom_sf_text(
        data=Group_labels,
        aes(geometry=geom, label= Group.labe, hjust=just),
        size=rel(2.4),
        colour="white",
        family = 'DIN Next LT Pro Light') + 
      guides(fill = guide_colourbar(
        ticks = FALSE,
        barwidth = 15,
        barheight = 0.5,
        title.position = 'top')) + 
      theme_void() +
      labs(title = 'Percentage Change In Daily Covid-19 Infections\nBy Lower Tier Local Authority',
           caption = "Visualisation by Joe O'Reilly (josephedwardoreilly.github.com)") + 
      theme(
        plot.background = element_rect(fill = '#19323C',color = NA),
        panel.background = element_rect(fill = '#19323C', color = NA),
        legend.justification = 'top',
        plot.margin = margin(10, 20, 10, 20),
        legend.position = 'bottom',
        legend.title = element_text(
          size = 8, hjust = 0.5,
          family = 'DIN Next LT Pro Bold',
          color = 'white'),
        legend.text = element_text(
          size = 7,
          family = 'DIN Next LT Pro Light',
          color = 'white'),
        plot.title = element_text(
          family = 'DIN Next LT Pro Bold',
          color = 'white',
          size = 11),
        plot.title.position = "plot",
        plot.caption = element_text(
          family = 'DIN Next LT Pro Light',
          color = 'white',
          size = 7,
          hjust = 1,
          margin = margin(10, 0, 0, 0)),
        plot.caption.position = 'plot') + 
      scale_fill_viridis(
        option = "turbo", 
        breaks = c(-100, 0, 100),
        labels = c('Less than -100 %', '0 %', 'Greater than 100 %'),
        limits = c(-100, 100), oob = scales::squish) 
    
  }, width = 500, height = 717.5175)
  
}

shinyApp(ui, server)

