#Simple models
library(modelr)
library(tidyverse)

#Basic intuition
ggplot(sim1, aes(x, y)) + 
  geom_point()

##PLot as many lines
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) + #linear line of the models
  geom_point() #scatter plot of sim1 points

## Find the line that has the lowest rmse

pred <- function(a, data) {a[1] + a[2]* data$x} #y = m + bx where a = c(m,b)
pred(c(7, 1.5), sim1) #predicts for all the value of x

rmse_ <- function(a, data) {
  diff <- data$y - pred(a, data) #y - yhat
  sqrt(mean(diff ^ 2))
}

rmse_wrapper <- function(a1, a2) {
  rmse_(c(a1,a2), sim1)
}

models %>% 
  mutate(rmse = map2_dbl(.$a1, .$a2, rmse_wrapper)) %>%
  arrange(rmse) %>%
  head(1) %>%
  select(a1, a2) #find the parameters a1 and a2 with lowest rmse

models <- models %>% mutate(rmse = map2_dbl(.$a1, .$a2, rmse_wrapper))

## PLot the 10 best lines
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -rmse),  #color negative to allow lowest rmse to be brightest
    data = filter(models, rank(rmse) <= 10)
  )

## Grid search
grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) #each row represents a combination from a1 and a2
grid_results <- grid %>% mutate(rmse = map2_dbl(a1, a2, rmse_wrapper)) # equivalent of saying; for a in a1, for b in a2, rmse(c(a,b), sim1)

#Plot the best few grids
grid_results %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid_results, rank(rmse) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -rmse)) 

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -rmse), 
    data = filter(grid_results, rank(rmse) <= 10)
  )

# Base lm approach, optimization
sim1_mod <- lm(y ~ x, data = sim1) #this approach guarantees a global minimum
coef(sim1_mod)
summary(sim1_mod)
predict(sim1_mod, newdata = data.frame(x = c(10,20,30))) #newdata must be in data.frame
rmse(sim1_mod, sim1) #calculate within-sample rmse
test_set <- data.frame(x = c(1.2,2.5,3.1),
                       y = c(5, 10.5, 11))
rmse(sim1_mod, test_set) #calculate test set rmse. Test set needs to have same columns as training set

# Standard diagnostic checks
par(mfrow = c(2, 2)) #make 4 plots into one diagram
plot(sim1_mod)


qqPlot(model1, main="QQ Plot")
ols_plot_resid_qq(model1) 
ols_test_normality(model1)
ols_plot_resid_fit(model1) #Looks like white noise
ols_plot_resid_hist(model1)
vif(model1) #All < 10, multicollinearity not a problem

bptest(model1) #p = 0.5756, cannot reject null that there is no heteroskedasticity
resettest(model1, power =2:3 , type='regressor', data=df) #adding 2nd and 3rd power of each regressor, since significance is not very strong (0.088), it is worth exploring whether it makes business sense to add non-linear terms


# Visualizing models

ggplot(sim3, aes(x1,y, color = x2)) + geom_point() + geom_smooth(method = 'lm', se=F) #visualize continuous and categorical variable
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)    
#Grid results -> 1 continuous, 1 categorical
grid <- sim3 %>% 
  data_grid(x1, x2) %>% #input the explanatory variables into data_grid; output unique combinations of input variables
  gather_predictions(mod1, mod2) 
grid

ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model) #plot separately for each model, overlay the predicted with actual scatter points

## Which model is better? Take a look at residuals
sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2) %>%
  gather_predictions(mod1, mod2)
sim3

# Grid results -> 2 continuous
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5),  #get 5 equally spaced values from x1 range
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)
grid
## visualize output
ggplot(grid, aes(x1, x2)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model) #highst when x1 is small and x2 is large

ggplot(grid, aes(x1, pred, colour = x2, group = x2)) + 
  geom_point(data = sim4, mapping = aes(x1,y, color = x2)) + 
  geom_line() + #for geom_line, need to use group to not join the lines
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_point(data = sim4, mapping = aes(x2,y, color = x1)) + 
  geom_line() +
  facet_wrap(~ model)

#grid results - 2 categorical
## Chi square test of independence for testing correlation between 2 categorical variables
## H0: the row and column variables of the contingency table are independent; H1: row and column variables are dependent
ggplot(diamonds %>% count(cut, color)) +
  geom_tile(mapping = aes(x = cut, y = color, fill=n))

crosstab <- xtabs(~cut + color,diamonds)
crosstab
chisq.test(crosstab)

# Splines
sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

ggplot(sim5, aes(x, y)) +
  geom_point() #sin graph

library(splines)
mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)


#Model building
library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)

## Why are low diamonds more expensive? --> poor cuts, bad colours, inferior quality
ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot

## Possiblility -> confounding variable : carat
ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50) #higher carat, higher price. Largest quantity in small carat. 99.7% of data has <2.5 carat

diamonds %>% count(carat < 2.5) %>%
  mutate(percentage = n / sum(n))

diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat)) #scale by log

ggplot(diamonds2, aes(lcarat, lprice)) + 
  geom_hex(bins = 50) #useful as it makes the pattern more linear

mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)
grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice) #backtransform to convert log price to actual price

#for full output, diamonds2 %>% mutate(pred = 2 ^ mod_diamond$fitted.values)

ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 50) + #actual plots
  geom_line(data = grid, colour = "red", size = 1) #predicted

# check validity of model
## residuals shouldnt have any pattern
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")

ggplot(diamonds2, aes(lcarat, lresid)) + 
  geom_hex(bins = 50)

#Now plot the residuals with the variables that define the quality of diamonds
ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot() #-? higher quality is now higher price
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

# Multivariate
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)
summary(mod_diamond2)

## Since predictors are independent, we can visualize individually
grid <- diamonds2 %>% 
  data_grid(cut, .model = mod_diamond2) %>% #if .model is supplied, it will include typical values for predictors which are not present. Typical refers to median for continuous, mode for factors
  add_predictions(mod_diamond2)
grid
ggplot(grid, aes(cut, pred)) + 
  geom_bar(stat="identity") +
  coord_cartesian(ylim=c(11,11.5))

## Look at residuals
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2")

ggplot(diamonds2, aes(lcarat, lresid2)) + 
  geom_hex(bins = 50) #There are some data points with large residuals

diamonds2 %>% filter(lresid2>2) %>% 
  add_predictions(mod_diamond2) %>%
  mutate(pred = round(2^pred)) %>%
  select(price, pred, carat:table, x:z) %>% 
  arrange(price) #check for the points with very high resid

#Example 2 - nycflights
## What affects the daily number of flights
library(nycflights13)
daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())
daily
ggplot(daily, aes(date, n)) + 
  geom_line()

daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))
ggplot(daily, aes(wday, n)) + 
  geom_boxplot() #Sat has significantly less number of flights

## First fit wday as a predictor
mod <- lm(n ~ wday, data = daily)

grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n") #predicted model output

ggplot(daily, aes(wday, n)) + 
  geom_boxplot() +
  geom_point(data = grid, colour = "red", size = 4) 

## Visualize residuals
daily <- daily %>% 
  add_residuals(mod)
daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + #draw y=0 horizontal line
  geom_line() #plot the residuals -> seems to fail starting in june

#Draw resid plot for each day. Resid is negative -> Model predict far fewer flights than expected
ggplot(daily, aes(date, resid, colour = wday)) + 
  geom_ref_line(h = 0) + 
  geom_line() #model seems to fail to predict for sat from jul 2013 - oct 2013

daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line(colour = "grey50") + 
  geom_smooth(se = FALSE, span = 0.20) #Fewer flights in January and December, more flights in May - Sep

## Filter out Saturday and plot over months
daily %>% filter(wday == "Sat") %>%
  ggplot() +
  geom_point(mapping = aes(date, n)) +
  geom_line(mapping = aes(date, n)) + 
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b") #seasonal effect -> Domain knowledge : summer holidays = more flights

## Cut date into 3 terms
term <- function(date) {
  cut(date, 
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall") 
  )
}

daily <- daily %>% 
  mutate(term = term(date)) 

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, colour = term)) +
  geom_point(alpha = 1/3) + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

daily %>% 
  ggplot(aes(wday, n, colour = term)) +
  geom_boxplot() #boxplot for each term for each weekday

#Interactive model
mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% #rename model1 and model2 to without_term and with_term respectively
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75) #with_term has smaller residuals

# rlm
## Since our models have big outliers, the mean tends to be far from the median
## Alleviate problem by using a model that is robust to effect of outliers

grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n") #predicted n

ggplot(daily, aes(wday, n)) +
  geom_boxplot() + 
  geom_point(data = grid, colour = "red") + 
  facet_wrap(~ term) #boxplot shows original counts, many outliers

mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) + 
  geom_hline(yintercept = 0, size = 2, colour = "white") + 
  geom_line() #more robust to outliers

#Many Models
## Nested dataframes
library(modelr)
library(tidyverse)
library(gapminder)
by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest() #creates a new column called data which contains a tibble for each group
by_country$data[[1]] #passthrough columns : year, lifeExp, pop, gdpPercap

country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}
models <- map(by_country$data, country_model) #apply country_model to each tibble
str(models)
by_country <- by_country %>% 
  mutate(model = map(data, country_model)) #create a new column for the saved model

by_country <- by_country %>% 
  mutate(resides = map2(data, model, add_residuals)) #apply add_residuals(data, model) for each row; output a 12x5 tibble with resids as 5th column
by_country$resides[[1]]

resids <- unnest(by_country, resides) #unstack the dataframe grouping by country and continent and append to the original dataframe 
#we now have residual for each row on n different models grouped by country and continent
nrow(resids) #1704 
nrow(gapminder) #1704

resids %>% 
  ggplot(aes(year, resid, group = country)) + #each line within a plot represents residuals grouped by country
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~continent)

broom::glance(by_country$model[[1]]) #glance outputs the values typical of summary(model)
broom::tidy(by_country$model[[1]]) #tidy output the coefficients estimate, std error, statistic, pvalue
broom::tidy(by_country$model[[1]], by_country$data[[1]]) 

by_country %>% 
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance) %>%
  arrange #r2 is very bad for african countries



