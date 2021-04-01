library(shiny)

library(ggplot2)
library(glue)
library(imgpalr)
library(magrittr)

kingfisher_url <- "https://www.gwct.org.uk/media/1076099/kingfisher-Ian-David.jpg"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("url", "Enter the URL for a picture:", value = kingfisher_url),
      numericInput("n_colours", "How many distinct colours should be in the palette?", value = 5, min = 3, step = 1),
      actionButton("make_palette", "Generate colour palette!")
    ),
    mainPanel(
      htmlOutput("picture"),
      textOutput("palette"),
      plotOutput("tile_plot")
    )
  )
)

server <- function(input, output, session) {
  # Display an image from a URL
  output$picture <- renderText(
    glue(
      "<img src='{input$url}' class='img-fluid' style='max-width:100%; height:auto'>"
    )
  )

  rgb_values <- eventReactive(
    input$make_palette,
    imgpalr::image_pal(input$url, n = input$n_colours)
  )

  output$palette <- renderText(rgb_values())

  output$tile_plot <- renderPlot({
    df <- data.frame(
      x = seq_along(rgb_values()), y = 1, z = rgb_values()
    )

    ggplot(df, aes(x, y)) +
      geom_tile(aes(fill = z)) +
      scale_fill_manual(values = df$z)
  })
}

shiny::shinyApp(ui, server)