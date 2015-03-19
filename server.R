library(shiny)
library(ggplot2)
library(scales)
library(ggd3)
library(htmlwidgets)
library(htmltools)
library(dplyr)
library(reshape2)

shinyServer(function(input, output, session) {

  observe({

    cat(sprintf("Tool: %s\n", input$tool))


    output$char = renderText(
      paste0("Tool: ", input$tool)
    )

  }, label='serverObserver')

  observe({
    if(input$tool == 'explore'){
      for(f in list.files('explore', pattern = '*.(R|r)$', full.names = TRUE))
        source(f, local=TRUE)
    }
    if(input$tool == 'models'){
      for(f in list.files('models', pattern = '*.(R|r)$', full.names = TRUE))
        source(f, local=TRUE)
    }
    if(input$tool == 'predict'){
      for(f in list.files('predict', pattern = '*.(R|r)$', full.names = TRUE))
        source(f, local=TRUE)
    }
  })

})
