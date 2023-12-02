Homework 6
================
Yiying Wu

## 1

### Importing data

``` r
# import dataset
homicide <- read_csv("homicide-data.csv")
```

### Tidying and wrangling the data

``` r
homicide_clean <- homicide %>%
  mutate(city_state = paste(city, state, sep = ", ")) %>%
  filter(!(city_state %in% c("Dallas, TX", "Phoenix, AZ", "Kansas City, MO", "Tulsa, AL"))) %>%
  filter(victim_race %in% c("White", "Black")) %>%
  mutate(solved = if_else(disposition == "Closed by arrest", 1, 0)) %>%
  mutate(victim_age = as.numeric(victim_age))

# head(homicide_clean)
```

### Fit a logistic regression model for Baltimore, MD

``` r
baltimore_data <- homicide_clean %>%
                  filter(city_state == "Baltimore, MD")

# Fit logistic regression
model <- glm(solved ~ victim_age + victim_sex + victim_race, data = baltimore_data, family = "binomial")

# Use broom::tidy to obtain estimates and confidence intervals
broom::tidy(model) %>%knitr::kable(digits = 4)
```

| term             | estimate | std.error | statistic | p.value |
|:-----------------|---------:|----------:|----------:|--------:|
| (Intercept)      |   0.3100 |    0.1713 |    1.8096 |  0.0704 |
| victim_age       |  -0.0067 |    0.0033 |   -2.0241 |  0.0430 |
| victim_sexMale   |  -0.8545 |    0.1382 |   -6.1839 |  0.0000 |
| victim_raceWhite |   0.8418 |    0.1747 |    4.8179 |  0.0000 |

Calculate adjusted odds ratios and 95% CI

``` r
# Calculate adjusted odds ratios 
adjusted_or <- exp(coef(model)["victim_sexMale"])  # For Male vs Female comparison
CI <- confint(model)  # Default confidence interval
CI_adjusted_or <- exp(CI["victim_sexMale", ])

# Displaying the adjusted odds ratio and its confidence interval
adjusted_or
```

    ## victim_sexMale 
    ##      0.4255117

``` r
CI_adjusted_or
```

    ##     2.5 %    97.5 % 
    ## 0.3241908 0.5575508
