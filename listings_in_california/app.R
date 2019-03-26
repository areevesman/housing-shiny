library(tidyr)
library(dplyr)
library(leaflet)
library(shinydashboard)
library(maps)
library(scales)

df1 = read.csv(url('https://raw.githubusercontent.com/areevesman/housing-shiny/master/listings.csv'))


df_ = df1 %>%
  drop_na(bedrooms, bathrooms, sqft, price, 
          latitude, longitude, date_posted, city,
          county) %>%
  filter(state == 'CA', sqft >= 200, sqft <= 10000, price >= 200, price <= 12000) %>%
  mutate(date_posted = as.Date(date_posted)) %>%
  select(latitude, longitude, date_posted, price, sqft,
         bedrooms, bathrooms, pets, laundry, parking,
         city, state, county) %>%
  arrange(desc(date_posted))




ui = dashboardPage(
  dashboardHeader(
    title = "Listings in California"
  ),
  dashboardSidebar(
    
    fluidRow(
      column(width = 12,
        
        textInput("city", label = "City:",
                 placeholder = 'ex: San Francisco',
                 value = ''),
        
        sliderInput("price", label = "Price:",
                   min = min(df_$price, na.rm=T), max = max(df_$price, na.rm=T),
                   value = c(min(df_$price, na.rm=T),
                             max(df_$price, na.rm=T)),
                   step = 25),
        
        sliderInput("bedrooms", label = "Number of bedrooms:",
                   min = min(df_$bedrooms, na.rm=T), max = max(df_$bedrooms, na.rm=T),
                   value = c(min(df_$bedrooms, na.rm=T),
                             max(df_$bedrooms, na.rm=T)),
                   step = 1),
        
        sliderInput("bathrooms", label = "Number of bathrooms:",
                   min = min(df_$bathrooms, na.rm=T), max = max(df_$bathrooms, na.rm=T),
                   value = c(min(df_$bathrooms, na.rm=T),
                             max(df_$bathrooms, na.rm=T)),
                   step = 0.5),
        
        sliderInput("sqft", label = "Square feet:",
                   min = min(df_$sqft, na.rm=T), max = max(df_$sqft, na.rm=T),
                   value = c(min(df_$sqft, na.rm=T),
                             max(df_$sqft, na.rm=T)),
                   step = 100)
      )
    ),
    disable = FALSE),
  
  dashboardBody(
    
    fluidRow(
      column(width = 12,
        box(width = 12, solidHeader = TRUE,
           # tags$style(type = "text/css", ".box-body {height:40vh}"),
           dataTableOutput("data_table")
        ),
        
        box(width = 12, solidHeader = TRUE,
           # tags$style(type = "text/css", ".box-body {height:40vh}"),
           leafletOutput("mymap", width = '100%')
        )
      )
    )
  )
)



server = function(input, output) {
  
  data <- reactive({
    x <- df_ %>%
      filter(bedrooms >= input$bedrooms[1] & bedrooms <= input$bedrooms[2],
             bathrooms >= input$bathrooms[1] & bathrooms <= input$bathrooms[2],
             sqft >= input$sqft[1] & sqft <= input$sqft[2],
             price >= input$price[1] & price <= input$price[2])
    
    if (input$city != ''){
      x <- x %>% filter(city == input$city)
    }
    x
    
  })
  
  output$data_table <- renderDataTable(
    
    data() %>%
      select(price, bedrooms, bathrooms, sqft, laundry, parking, pets, city, date_posted) %>%
      mutate(price = price, sqft = sqft),
    options = list(pageLength = 5,
                   dom = 'tipr',
                   scrollX = TRUE,
                   searching = FALSE)
    )
  
  output$mymap <- renderLeaflet({
    
    df <- data()
    
    m <- leaflet(
      data = data() %>% 
        sample_n(min(nrow(data()),2000))) %>%
      addTiles() %>%
      addCircles(lng = ~longitude, lat = ~latitude,
                 opacity = .7,
                 color='#3399FF')
    
    m
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)




