
# list of tools - could be dynamicaly generated
menus = tagList(
  tabPanel("Explore", id='explore',
           column(width = 4,
                  includeMarkdown('explore/explore.md'),
                  uiOutput('teamRecord_buttons'),
                  plotOutput('teamRecord')
                  ),
           column(width = 7, offset = 1,
                  uiOutput('teamScatter_buttons'),
                  h2("Averages by team"),
                  ggd3Output('teamScatter')
                  )
  ),
  tabPanel("Models", id='models',
           column(width = 12,
                  uiOutput('model_description'),
                  uiOutput('models')
                  )
  ),
  tabPanel("Predict", id='predict',
           column(width = 12,
                  uiOutput('predict_description')
                  )
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
    title = "NBA 2013-2014 Season"
  )
)
