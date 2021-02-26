# Statistical Models in R workshop
# Feb 23 and 25, 2021
# Gil Henriques - EDUCER/ECOSCOPE


# Load tidyverse packages -------------------------------------------------
library("tidyverse")


# One-way ANOVA with two groups -------------------------------------------
df_fly <- faraway::fruitfly # Load data from faraway package

fly_2_groups <- df_fly %>% 
	# change factor columns to character columns
	mutate_if(is.factor, as.character) %>% 
	# change activity column to have only two groups
	mutate(activity = ifelse(activity %in% c("isolated", "one", "many"), 
													 "no", "yes") ) %>% 
	# change thorax column to have only two groups
	mutate(thorax = ifelse(thorax <= 0.8, "short", "long")) %>% 
	# randomly sample 20 rows for each combination of activity and thorax
	group_by(activity, thorax) %>% 
	sample_n(20)

# Visualize longevity as a function of activity
ggplot(data = fly_2_groups,
			 aes(x = activity, y = longevity)) +
	geom_boxplot() +
	# annotate figure with significant stars and line 
	# (answer to a question, not part of the workshop)
	annotate(geom = "text", x = 1.5, y = 105, 
					 label = "***", size = 10) +
	geom_segment(data = data.frame(x = 1, xend = 2, y = 100, yend = 100),
							 aes(x = x, xend = xend, y = y, yend = y)) 

# Formula syntax: response_variable ~ predictor_variables

# Create the model using aov() function
fly_2_groups_model <- aov(longevity ~ activity, 
													data = fly_2_groups)

# View output of our model
summary(fly_2_groups_model)

# Exercise:
# Using ANOVA, test is longevity is affected by 
# thorax length. Use the fly_2_groups data frame
summary(aov(longevity ~ thorax, data = fly_2_groups))


# One-way ANOVA with three groups -----------------------------------------

fly_3_groups <- df_fly %>% 
	mutate_if(is.factor, as.character) %>% 
	# change activity column to have three groups
	mutate(activity = ifelse(activity %in% c("isolated", "one", "many"),
													 "none", activity)) %>% 
	# randomly sample 25 rows per activity group
	group_by(activity) %>% 
	sample_n(25)

# Visualize
ggplot(data = fly_3_groups,
			 aes(x = activity, y = longevity)) +
	geom_boxplot()

# create a model object with aov()
fly_3_groups_model <- aov(longevity ~ activity,
													data = fly_3_groups)
summary(fly_3_groups_model)

# Assess which groups differ
# Method 1: pairwise t-test with multiple comparison correction
pairwise.t.test(fly_3_groups$longevity,
								fly_3_groups$activity,
								p.adjust.method = "bonferroni")

# Method 2: Tukey's honest significance difference
TukeyHSD(fly_3_groups_model, "activity")


# Two-way ANOVA (two predictors) ------------------------------------------
ggplot(data = fly_2_groups,
			 aes(x = activity, y = longevity, color = thorax)) +
	geom_boxplot()

# Formula syntax: longevity ~ activity + thorax
fly_2_vars <- aov(longevity ~ activity + thorax,
									data = fly_2_groups)
summary(fly_2_vars)

# 2-way ANOVA with interactions
ggplot(data = fly_2_groups,
			 aes(x = activity, y = longevity, color = thorax)) +
	stat_summary(fun = mean, geom = "point") +
	stat_summary(fun = mean, geom = "line", aes(group = thorax)) 
# An interaction means that the lines have different slopes

# Formula syntax: longevity ~ activity + thorax + activity:thorax
# Shorthand syntax: longevity ~ activity * thorax
fly_2_vars_int <- aov(longevity ~ activity * thorax,
											data = fly_2_groups)
summary(fly_2_vars_int)


# Linear regression -------------------------------------------------------

# Load data from gapminder package
df_gap <- gapminder::gapminder

# Visualize some relationships:
ggplot(data = df_gap,
			 aes(x = year, y = lifeExp)) +
	geom_point()

ggplot(data = df_gap,
			 aes(x = gdpPercap, y = lifeExp)) +
	geom_point()

# Model life expectancy as a linear function of year
life_exp_model <- lm(lifeExp ~ year, data = df_gap)
summary(life_exp_model)

# Visualize the linear regression, using the estimated coefficients
ggplot(data = df_gap,
			 aes(x = year, y = lifeExp)) +
	geom_point() +
	geom_abline(intercept = -585.65,
							slope = 0.326,
							color = "gold")

# Do the same thing using ggplot's geom_smooth() function
ggplot(data = df_gap,
			 aes(x = year, y = lifeExp)) +
	geom_point() +
	geom_smooth(method = "lm")

# Diagnostic plots
par(mfrow = c(2,2))		# This allows us to make four plots in one figure
plot(life_exp_model)	# Create the diagnostic plots
par(mfrow = c(1,1))		# Go back to a single plot per figure

# Exercise:
# Fit a linear model of life expectancy as a function of per-capita gdp. 
# Visualize the linear and the diagnostic plots.
# Think about whether this is a good fit for the data.

life_exp_gdp <- lm(lifeExp ~ gdpPercap, data = df_gap)
summary(life_exp_gdp)

ggplot(data = df_gap,
			 aes(x = gdpPercap, y = lifeExp)) +
	geom_point() +
	geom_smooth(method = "lm")

par(mfrow = c(2,2))
plot(life_exp_gdp)
par(mfrow = c(1,1))


# Multiple regression -----------------------------------------------------

# Formula syntax: lifeExp ~ year + gdpPercap

# Create model
life_exp_mult <- lm(lifeExp ~ year + gdpPercap,
										data = df_gap)
summary(life_exp_mult)


# Transformations of variables --------------------------------------------

# Example of transformation: gdpPercap --> sin(gdpPercap)
ggplot(data = df_gap, 
			 aes(x = sin(gdpPercap), y = lifeExp)) +
	geom_point()

# Exercise:
# Find a transformation that makes the plot look like a straight line 
# and fit a Linear Regression to the transformed variables

# Solution: log-transform the predictor
ggplot(data = df_gap, 
			 aes(x = log(gdpPercap), y = lifeExp)) +
	geom_point()

# Linear regression with transformed predictor
life_exp_log <- lm(lifeExp ~ log(gdpPercap) + year, 
									 data = df_gap)
summary(life_exp_log)


# Polynomial regression ---------------------------------------------------

# Formula for a polynomial regression:
# y(x) = a + b1*x + b2*x^2 + b3*x^3 + ...

# Visualize a polynomial regression of third degree:
ggplot(data = df_gap, 
			 aes(x = gdpPercap, y = lifeExp)) +
	geom_point() +
	geom_smooth(method = "lm", 
							formula = y ~ poly(x, 3, raw = TRUE))
# Note that the more terms we include in the polynomial regression, the
# better the fit to the existing data, but the worse the predictions
# about future data points. This is called "overfitting".


# Formula syntax for polynomial regression:
# lifeExp ~ year + gdpPercap + I(gdpPercap^2) + I(gdpPercap^3) + ...

# Shorthand syntax:
# lifeExp ~ year + poly(gdpPercap, 3, raw = TRUE)

# Fitting a polynomial regression:
summary(lm(lifeExp ~ year + poly(gdpPercap, 3, raw = TRUE), data = df_gap))


# Analysis of Covariance (ANCOVA) -----------------------------------------
# Used when one predictor is discrete, another is continuous, and we
# want to see whether there is an interaction between the two.
ggplot(data = df_gap, 
			 aes(x = year, y = lifeExp, color = continent)) +
	geom_point() +
	geom_smooth(method = "lm") 
# if there is an interaction, the lines are not going to be parallel

# Fitting an ANCOVA:
life_exp_int <- lm(lifeExp ~ year * continent, data = df_gap)
summary(aov(life_exp_int))

# Example where there is no interaction: 
# let's keep only the rows corresponding to Oceania and Europe
df_gap2 <- df_gap %>% filter(continent %in% c("Oceania", "Europe"))

summary(aov(lm(lifeExp ~ year * continent, data = df_gap2)))



# Linear Mixed-Effects Models ---------------------------------------------

# This plot shows how the effect of ncontrols on ncases is influenced
# by the age group. 
ggplot(data = esoph,
			 aes(x = ncontrols, y = ncases,
			 		group = agegp, color = agegp)) +
	geom_point(position = position_jitter(height = 0.2)) +
	geom_smooth(method = "lm") # Pay attention to the very large error bars

# We use LME when we want to know the effect of a given predictor
# (effect of ncontrols on ncases) but the data are measured on groups
# (agegp) that can affect the relationship. 

# Note how few points per age group we have
esoph %>% 
	group_by(agegp) %>% 
	tally() # This is the reason why we have such big error bars

# Mathematically, LMEs use all points to estimate the slopes/intercepts
# This increases their power relative to a multiple regression
# How it works: 
# There is one, "mean" or "grand" slope/intercept that fits the entire 
# dataset (fixed effect) and each group's slope/intercept (random effect) 
# is assumed to be drawn from a normal distribution centered at this 
# grand-slope and grand-intercept.

# Formula syntax:
# ncases ~ ncontrols + (ncontrols | agegp)
# Read (ncontrols | agegp) as: "ncontrols grouped by age group"
# Random effect of age group on slope and intercept
# If we want only random intercepts then (1 | agegp)

# Make model
esoph_model <- lme4::lmer(ncases ~ ncontrols + (ncontrols | agegp),
													data = esoph)
summary(esoph_model)

coef(esoph_model) # extract slope & intercept for each group

# The following turns the output of coef(esoph_model) into a convenient
# data frame
coefs <- coef(esoph_model)$agegp %>% 
rownames_to_column() %>% # add row names as a new column
	rename(intercept = `(Intercept)`,
				 agegp = rowname) # rename columns 

# In this plot we show the results of a multiple linear regression 
# (solid lines, same as last plot) and the results of our LME model
# (dashed lines) to show that they are not the same
ggplot(data = esoph,
			 aes(x = ncontrols, y = ncases,
			 		group = agegp, color = agegp)) +
	geom_point(position = position_jitter(height = 0.2)) +
	geom_smooth(method = "lm", se = FALSE) +
	geom_abline(data = coefs, 
							aes(intercept = intercept,
									slope = ncontrols,
									color = agegp),
							size = 1, linetype = "dashed")

# Exercise:
# 1. use the lme4::sleepstudy dataset to fit an LME on Reaction against 
# Days, grouped by Subject
# 2. Find the intercept and slope for subject #310

# Visualize
ggplot(data = lme4::sleepstudy,
			 aes(x = Days, y = Reaction, 
			 		group = Subject, color = Subject)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE)

# Fit model
sleep <- lme4::lmer(Reaction ~ Days + (Days | Subject), 
						data = lme4::sleepstudy)
summary(sleep)

# Get coefficients for subject 310
coefficients <- coef(sleep)$Subject %>% 
	rownames_to_column()
coefficients %>% filter(rowname == 310)



# Generalized linear models -----------------------------------------------

# We use GLMs when the data are not continuous, e.g. count or binary data
# Instead of y = a + b*x, we have g(y) = a + b*x
# g(y) is called a "link function"


#...GLM for binary data: family = binomial --------------------------------
# Logistic regression (aka Logit regression)

# Read in data: admissions to UCB as a function of department & gender
df_ucb <- as.data.frame(UCBAdmissions)

# Make GLM model
ucb_model <- glm(Admit ~ Gender * Dept,
								 data = df_ucb,
								 family = binomial, # this tells R data is binary
								 weights = Freq) # because our data set is summarized

summary(ucb_model) # coefficients may be hard to interpret

# Find significance of each of the two covariates as a whole:
car::Anova(ucb_model)

# Show whether we can drop predictors without degrading the fit
drop1(ucb_model, test = "Chisq")

# Show probability per depart per gender (easier to interpret)
lsmeans::lsmeans(ucb_model,
								 ~ Gender + Dept,
								 type = "response")


# ...GLM for count data: family = poisson ---------------------------------

# This dataset shows the number of infections (colonic polyps) after 12 
# months in a clinical trial as a function of treatment (placebo vs drug)
# and patient age
df_polyps <- HSAUR3::polyps

# Estimate number of infections as a function of treatment and age
polyps_model <- glm(number ~ treat + age,
										data = df_polyps,
										family = poisson) # family = poisson because nr of
																			# infections is count data
summary(polyps_model) # shows effect of each predictor

# Effect of each predictor is hard to interpret, this shows the rate
# of infection for each treatment (placebo vs drug)
lsmeans::lsmeans(polyps_model, ~ treat, 
								 type = "response")
