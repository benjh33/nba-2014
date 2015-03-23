## nothing yet

output$predict_description <- renderUI({
  fluidRow(
    column(width = 4, offset = 1,
           includeMarkdown('predict/predict.md')
    ),
    column(width = 7,
           tabsetPanel(
             tabPanel('scatter matrix',
                      uiOutput('model_scatter')
             ),
             tabPanel('1st Period',
                      column(width = 12,
                             uiOutput('model_compare_select',
                                      class='pad-top'),
                             column(width = 8,
                                    plotOutput('model_roc_1')
                             )
                      )
             ),
             tabPanel('2nd Period',
                      column(width = 12,
                             uiOutput('model_compare_select2',
                                      class='pad-top'),
                             column(width = 8,
                                    plotOutput('model_roc_2')
                             )
                      )
             ),
             tabPanel('3rd Period',
                      column(width = 12,
                             uiOutput('model_compare_select3',
                                      class='pad-top'),
                             column(width = 8,
                                    plotOutput('model_roc_3')
                             )
                      )
             ),
             tabPanel('variable importance',
                      column(width = 12,
                             ggd3Output('varimp'),
                             class='pad-top')
                    )
           )
      ),
    class='pad-top'
  )
})

output$scatterMatrix <- renderPlot({
  featurePlot(x =pd[sample(1:nrow(pd), 500) , input$spmatrix ],
              y = as.factor(wins),
              plot = "pairs",
              ## Add a key at the top
              auto.key = list(columns = 2),
              par.settings=simpleTheme(col=c("red", 'blue')),
              main = '500 randomly chosen observations')
  },
width = 700,
height = 700)


output$model_scatter <- renderUI({
  list(
    column(width = 3,
      selectInput('spmatrix',
                  'variables for scatter matrix',
                  choices = selectList(names(pd)),
                  selected = sample(names(pd), 5),
                  multiple = TRUE, width = '100%'),
      class  ='pad-top'),
    column(width = 9,
           plotOutput('scatterMatrix'),
           class = 'pad-top'
           )
  )
})
roc_colors <- list(
  'glmnet' = 'darkgreen',
  'gbm' = 'darkorange',
  'rf' = 'blue'
)
plot_roc <- reactive({
  f <-function(period) {
    make_plot <- function(vars) {
      p <- paste0('period_', period)
      glm_preds <- predict(modelOutput[[p]]$glm$model,
                       pdtest, type='prob')
      glm_roc <- roc(pdtest$wins, glm_preds[,2],
                   levels=c('WIN', 'LOSS'),
                   percent = TRUE)
      plot(glm_roc, col='black',
         main = 'ROC of selected tests v. GLM with scores only')
      colors <- c('black')
      labels <- c('glm')
      for(v in vars){
        colors <- c(colors, roc_colors[[v]])
        labels <- c(labels, v)
        preds <- predict(modelOutput[[p]][[v]]$model,
                       pdtest, type='prob')
        c_roc <- roc(pdtest$wins, preds[,2],
                   levels = c('WIN', 'LOSS'), percent = TRUE)
        plot(c_roc, col = roc_colors[[v]], add = TRUE)
      }
      legend('bottomright',
             legend=labels,
             col=colors, lwd=5)
    }
    make_plot
  }
  f
})


output$model_roc_1 <- renderPlot({
  p <- plot_roc()('1')
  p(input$compare_models_1)
}, width = 500)
output$model_roc_2 <- renderPlot({
  p <- plot_roc()('2')
  p(input$compare_models_2)
}, width = 500)
output$model_roc_3 <- renderPlot({
  p <- plot_roc()('3')
  p(input$compare_models_3)
}, width = 500)

output$model_compare_select <- renderUI({
  column(width = 4,
         selectInput('compare_models_1', 'models to compare to glm',
                     choices = list(
                       'glmnet' = 'glmnet',
                       'gbm' = 'gbm',
                       'random forest' = 'rf'
                     ), selected = 'glmnet', multiple = TRUE,
                     width = '80%')
  )
})
output$model_compare_select2 <- renderUI({
  column(width = 4,
         selectInput('compare_models_2', 'models to compare to glm',
                     choices = list(
                       'glmnet' = 'glmnet',
                       'gbm' = 'gbm',
                       'random forest' = 'rf'
                     ), selected = 'glmnet', multiple = TRUE,
                     width = '80%')
  )
})
output$model_compare_select3 <- renderUI({
  column(width = 4,
         selectInput('compare_models_3', 'models to compare to glm',
                     choices = list(
                       'glmnet' = 'glmnet',
                       'gbm' = 'gbm',
                       'random forest' = 'rf'
                     ), selected = 'glmnet', multiple = TRUE,
                     width = '80%')
  )
})
draw_plot <- reactive({
  ggd3(varImps,
       width = 1100,
       layers = list(l1 = list(geom=list(type='bar'),
                               position = 'dodge',
                               stat=list(x='identity'))),
       aes = list(x = 'Overall', y = 'variable', fill='model'),
       settings = list(facet=list(x=c('period'),
                                  nrows = 1, type='grid'),
                       margins=list(left=140),
                       yGrid = FALSE,
                       width = 180,
                       yScale = list(scale = list(domain = varOrder),
                                     label = ''),
                       xScale = list(axis=list(ticks=4)),
                       height = 800))
})
output$varimp <- renderGgd3({
  draw_plot()

})
