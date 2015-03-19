
source('shinyUtil.R', local = TRUE)

# list of tools - could be dynamicaly generated
menus = tagList(
  tabPanel("Explore", id='explore',
           column(width = 5,
                  includeMarkdown('explore/explore.md'),
                  uiOutput('teamRecord_buttons'),
                  plotOutput('teamRecord')
                  ),
           column(width = 7,
                  uiOutput('teamScatter_buttons'),
                  h2("Average performance by team"),
                  ggd3Output('teamScatter')
           )
  ),
  tabPanel("Models", id='models', column(width = 12)
  ),
  tabPanel("Predict", id='predict', column(width = 12)
  )
)

shinyUI(
  bootstrapPage(
    tags$head(tags$link(rel='stylesheet', type='text/css', href='css/style.css')),
    nav(menus, id='tool', brand='explore'),
    fluidRow(column(width=10,
                    tabs(menus, id='tabs', brand='explore'),
                    offset = 1
    )
    ),
    title = "NBA shot explorer"
  )
)
