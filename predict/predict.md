# Predictive power of game data

Obviously, the best indicator of a win is the score at any given moment of a game. I have attempted to improve on the score-only models by using game level event statistics. The models used are `glmnet`, `gbm`, and `randomForest`, chosen mostly because I have a fair number of variables and each algorithm has variable selection more or less baked in.

Each algorithm is executed with several combinations of parameters (lambda, alpha, mtry, etc.), each time with 10-fold cross-validation executed ten times. 

Variable names can be guessed at pretty easily knowing that the period number the variable refers to is appended after an underscore. Names like "PROP\_3\_3" mean the proportion of attempted shots that were three-pointers in the third period. Period point margins are called "P1_diff" - etc. 

A scatter plot matrix can be configured based on any of the available variables. As you can see, there is no overwhelming linear relationship between winning and predictive variables. Several are correlated, but only the `TOUCH_TIME` variables proved to be correlated above 0.75 (amongst themselves, not with wins).

ROC plots of the predictions for each of the three models on the test data set, per period, are shown as labeled in the tabs. I am suspicious of the random forest results, but it certainly is the winner.

Finally, the fourth panel has a plot of variable importance per period, colored by model type and sorted by the random forest 1st period results. Variable importance are calculated differently by each model, but `caret` scales them to 100. Therefore, they aren't really comparable, but relative importance within model type is meaningful.
  - Toggling to a different panel and back to "variable importance" will cause an HTML error - best to refresh the page. 
