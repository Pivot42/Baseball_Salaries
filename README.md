---
title: "Baseball Salaries"
author: "Jesus Marquez Garcia"
output: html_notebook
---

### Packages
```{r, message = FALSE, warning = FALSE}
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(skimr)
library(corrplot)
library(leaps)
source("useful_functions.R")
```

### Loading Data
```{r}
baseball <- read.table("https://www4.stat.ncsu.edu/~boos/var.select/baseball.txt", sep = ",", header = TRUE) # nolint

baseball <- baseball %>% mutate(y = log(y)) %>% rename(logSalary = y)

n <- NROW(baseball)

baseball
```

### Exploratory Data Analysis
```{r, message = FALSE, warning = FALSE}
require(psych)
```

```{r}
# MORE ENHANCED PAIRS PLOT
pairs.panels(baseball[, -c(13, 14, 15, 16)])
```

This pairs plot gives us an illustration of the data with the plots below the diagonal representing bivariate scatter plots, the diagonal line are histograms, and the numbers in top of the diagonal been the Pearson correlation from the given data.

```{r}
# CORRELAITON MATRIX
cor_matrix <- cor(baseball[, -c(13, 14, 15, 16)])

# VISUALIZE THE CORRELATION MATRIX
corrplot.mixed(cor_matrix, lower = "number", upper = "ellipse")
```

This correlation plot gives us the Pearson correlation below the diagonal, the diagonal is the name of the variable, and above the diagonal is a representation of correlation in the shape of ellipses.

### Both Direction Stepwise AIC 
```{r}
baseball_mlr_null <- lm(logSalary ~ 1, data = baseball)

baseball_mlr_all <- lm(logSalary ~ ., data = baseball) # nolint

fit_forward_AIC <- step(baseball_mlr_null, scope = list(lower = baseball_mlr_null, upper = baseball_mlr_all), direction = "both", k = 2) # nolint
```

Since there are multiple variables, it would be the best idea to select a model that best fits the data. One of the ways to do this is with a stepwise process with combination of estimates. In this case we are going to use both stepwise directions with a combination of AIC as our estimate.

### Both Direction Stepwise BIC
```{r}
fit_forward_BIC <- step(baseball_mlr_null, scope = list(lower = baseball_mlr_null, upper = baseball_mlr_all), direction = "both", k=log(n)) # nolint
```

Here we are going to use the same stepwise procedure as the previous test but instead of using AIC as our estimate. We are going to change the estimate to BIC.

### Leaps and Bounds
```{r}
all_possible <- regsubsets(logSalary ~ ., data = baseball, nbest = 1, method = "exhaustive") # nolint

all_possible_summ <- summary(all_possible)

all_possible_summ$bic

# BLACK(OR GRAY)=SELECTED; WHITE=NOT SELECTED
plot(all_possible)
```

We continue with our Model Selection process by utilizing the leaps and bounds algorithm to know what the best model is.

### Best Model chosen with help of Model Selection Procedures
```{r}
baseball_mlr_bestAIC <- lm(logSalary ~ x2 + x4 + x8 + x9 + x10 + x11 + x13 + x14 + x15, data = baseball) # nolint

baseball_mlr_bestBIC <- lm(logSalary ~ x4 + x8 + x10 + x13 + x14 + x15, data = baseball) # nolint

baseball_mlr_bestLeaps <- lm(logSalary ~ x4 + x8 + x9 + x10 + x13 + x14 + x15, data = baseball) # nolint

cbind(Best_Model_AIC = BIC(baseball_mlr_bestAIC), Best_Model_BIC = BIC(baseball_mlr_bestBIC), Best_Model_Leaps = BIC(baseball_mlr_bestLeaps)) # nolint
```

Now that we have the best model from each of the procedures, we can choose one. The way this can be done is by comparing the models estimate score to each other. The one that is the lowest would be the chosen one. In this case for the estimate for the model selection we use BIC. 

###  Parameter Estimates and ANOVA Tables of chosen Model
```{r}
summary(baseball_mlr_bestBIC)
```

In the Parameters Estimates table we can see how well each individual variable does on the model to predict the salary to which they were fitted. Here we can see that the best variables in the model are x13 and x15.

```{r}
anova(baseball_mlr_bestBIC)
```

With the ANOVA table we can see the individual sum of squares, mean squares, the F value, and the P value. This information is valuable as it can give us an insight to know if the variables are significant to the model and on the prediction of salary (y variable). If look at the P Value Colum we can see that all our variables are significantly impacting the model and prediction, reaffirming what we saw on our Parameters Estimates table.

```{r}
simple_ANOVA(baseball_mlr_bestBIC)
```

With the simplified ANOVA table, we get to see important information about the model in general compared to individual variables interacting on the model. He we can see that overall; the model is significant at predicting salary (y variable).

### Fitting Model 1
```{r}
baseball_mlr_model_1 <- lm(logSalary ~ x13 + x15 + x3 + x4, data = baseball)

summary(baseball_mlr_model_1)
```

We chose a second model to test. This model only contains a small set of variables.

We show the table of Parameters Estimates given us an idea of what variables are best utilized in the model to predict the salary (y variable). Looking at the P values of the variables we can see that the least significant variable is x3. It is still significant for the model for predicting the salary (y variable) but not as much compare to the others.

```{r}
anova(baseball_mlr_model_1)
```

Looking at the ANOVA table for the model we can see more detail on how each variable is significant to the model. With this information we can see that x3 is not the least significant predictor, making our previous stamen untrue. This is perfectly normal as it illustrates the need for multiple tests to be done to validate our model.

```{r}
simple_ANOVA(baseball_mlr_model_1)
```

With a glance at the simplified ANOVA table for the model we can see that this model can also significantly predict salary (y variable).

### Using BIC to compare Chosen Model and Model 1
```{r}
cbind(Chosen_Model = BIC(baseball_mlr_bestBIC), Model_1 = BIC(baseball_mlr_model_1)) # nolint
```

Now that we have two models, we need to know which one is the better one at predicting salary (y variable). To accomplish this, we can obtain both models BIC score and see which one is the lowest one. Looking at both models we can see that our chosen one is better than the one provided.

### Preform Hypothesis Test to see if H0: B3 = B4 = 0
```{r}
baseball_mlr_model_1_betas <- lm(logSalary ~ x3 + x4, data = baseball)

simple_ANOVA(baseball_mlr_model_1_betas)

anova(baseball_mlr_model_1_betas, baseball_mlr_model_1)
```

### 95% Prediction Interval
```{r}
predict(baseball_mlr_model_1, newdata = data.frame(x13 = 0, x15 = 1, x3 = 80, x4 = 120), interval = "prediction", se.fit = T, conf.level = 0.95) # nolint
```

Once we have already fitted the model we can use it for predictions with new data. We want to make a prediction with the following data (x13 = 0, x15 = 1, x3 = 80, x4 = 120). We also use a confidence interval of 95%. As we can see on the results above the predicted salary is 7.336288.
