# nothing.
m1 <- glm(WIN ~ Q1_diff, data=nba_wl_games, family=binomial, model = FALSE)
m2 <- glm(WIN ~ Q1_diff + Q2_diff, data=nba_wl_games, family=binomial, model = FALSE)
m3 <- glm(WIN ~ Q1_diff + Q2_diff + Q3_diff,
          data=nba_wl_games, family=binomial, model = FALSE)
# predict(m1, newdata = data.frame(Q1_diff = 10), type = 'response')
output$models <- renderUI({
  fluidRow(
    column(width = 2,
           sliderInput("mq1", h4('Q1 margin'), min = -30, max = 30,
                       step = 1, value = 0, ticks = FALSE, animate = TRUE),
           sliderInput("mq2", h4('Q2 margin'), min = -30, max = 30,
                       step = 1, value = 0, ticks = FALSE, animate = TRUE),
           sliderInput("mq3", h4('Q3 margin'), min = -30, max = 30,
                       step = 1, value = 0, ticks = FALSE, animate = TRUE)
    ),
    column(width = 8, offset = 1,
           ggd3Output('preds'),
           fluidRow(
             column(width = 4,
                    dataTableOutput('m1table')
             ),
             column(width = 4,
                    dataTableOutput('m2table')
             ),
             column(width = 4,
                    dataTableOutput('m3table')
             )
           )
    )
  )
})

output$model_description <- renderUI({
  fluidRow(
    column(width = 8, offset = 2,
        includeMarkdown('models/models.md')
    )
  )
})

output$preds <- renderGgd3({
  newd <- data.frame(
    margin = -30:30,
    q1_margin = predict(m1, newdata = data.frame(Q1_diff = -30:30), type='response'),
    q2_margin = predict(m2, newdata = data.frame(Q1_diff = input$mq1,
                                          Q2_diff = -30:30), type='response'),
    q3_margin = predict(m3, newdata = data.frame(Q1_diff = input$mq1,
                                          Q2_diff = input$mq2,
                                          Q3_diff = -30:30), type='response')
  )
  newd <- melt(newd, id.vars = 'margin')
  newd$variable <- as.character(newd$variable)
  newd$highlight <- '0'
  newd[(newd$margin == input$mq1) & (newd$variable == 'q1_margin'), 'highlight'] <- '1'
  newd[newd$margin == input$mq2 & newd$variable == 'q2_margin', 'highlight'] <- '1'
  newd[newd$margin == input$mq3 & newd$variable == 'q3_margin', 'highlight'] <- '1'
  ggd3(data = newd,
       height = '200px',
       layers = list(l1=list(geom='line'), l2=list(geom='point')),
       aes = list(x="margin", y='value', fill='highlight', size='highlight'),
       settings = list(facet = list(x='variable', nrows=1,
                                    titleSize=c(30,0)),
                       lineWidth = 2,
                       lineType = 'none',
                       sizeRange = c(4, 8),
                       dtypes = list(highlight = c('string', 'few')),
                       width = 280,
                       height = 300,
                       color='none',
                       xScale=list(axis=list(ticks=4)),
                       yScale=list(axis=list(ticks=4)),
                       margins=list(left=40))
       )
})
output$m1table <- renderDataTable({
    df <- as.data.frame(round(summary(m1)$coef[,1:2], 3))
    df$coef <- rownames(df)
    df <- df[, c('coef', 'Estimate', 'Std. Error')]
    df
  },
  options = list(
    searching = FALSE,
    paging = FALSE,
    callback = I(
      "function(oTable) {$('.dataTables_info').remove();}"
    ),
    rowCallback = I(
    'function(row, data) {
        // Bold cells for those >= 5 in the first column
        $(".dataTables_info").remove();
        if (parseFloat(data[0]) >= 5.0)
          $("td:eq(0)", row).css("font-weight", "bold");
      }'
  ))
)
output$m2table <- renderDataTable({
    df <- as.data.frame(round(summary(m2)$coef[,1:2], 3))
    df$coef <- rownames(df)
    df <- df[, c('coef', 'Estimate', 'Std. Error')]
    df
  },
  options = list(
    searching = FALSE,
    paging = FALSE,
    callback = I(
      "function(oTable) {$('.dataTables_info').remove();}"
    ),
    rowCallback = I(
    'function(row, data) {
        // Bold cells for those >= 5 in the first column
        $(".dataTables_info").remove();
        if (parseFloat(data[0]) >= 5.0)
          $("td:eq(0)", row).css("font-weight", "bold");
      }'
  ))
)
output$m3table <- renderDataTable({
    df <- as.data.frame(round(summary(m3)$coef[,1:2], 3))
    df$coef <- rownames(df)
    df <- df[, c('coef', 'Estimate', 'Std. Error')]
    df
  },
  options = list(
    searching = FALSE,
    paging = FALSE,
    callback = I(
      "function(oTable) {$('.dataTables_info').remove();}"
    ),
    rowCallback = I(
    'function(row, data) {
        // Bold cells for those >= 5 in the first column
        $(".dataTables_info").remove();
        if (parseFloat(data[0]) >= 5.0)
          $("td:eq(0)", row).css("font-weight", "bold");
      }'
  ))
)
