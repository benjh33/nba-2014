# Some basic linear models

Three logistic regressions are calculated using the point margins at the end of the first, second, and third quarters. Adjusting a slider calculates the prediction for that quarter and serves as a constant in calculations in subsequent quarters.

Mousing over any circle shows the win probability with that margin and the end of the quarter in question. There is some extraneous information in the tooltip due to the visualization library used. The orange point represents the margin chosen in the slider.

All available games were used, no cross-validation or test group was used to guage the predictive accuracy. It should be assumed these are grossly overfit models, and coefficients should be interpreted as rough guidelines for descriptive purposes.

In an effort to do something useful with these models, the 100 biggest upsets based on score information at the selected quarter are browseable at right with links to the [nba.com](www.nba.com) highlight video.
