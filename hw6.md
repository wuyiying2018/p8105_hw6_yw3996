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
  mutate(is_solved = if_else(disposition == "Closed by arrest", 1, 0)) %>%
  mutate(victim_age = as.numeric(victim_age))

head(homicide_clean)
```

    ## # A tibble: 6 × 14
    ##   uid   reported_date victim_last victim_first victim_race victim_age victim_sex
    ##   <chr>         <dbl> <chr>       <chr>        <chr>            <dbl> <chr>     
    ## 1 Alb-…      20100601 SATTERFIELD VIVIANA      White               15 Female    
    ## 2 Alb-…      20100102 MULA        VIVIAN       White               72 Female    
    ## 3 Alb-…      20100126 BOOK        GERALDINE    White               91 Female    
    ## 4 Alb-…      20100130 MARTIN-LEY… GUSTAVO      White               56 Male      
    ## 5 Alb-…      20100218 LUJAN       KEVIN        White               NA Male      
    ## 6 Alb-…      20100308 GRAY        STEFANIA     White               43 Female    
    ## # ℹ 7 more variables: city <chr>, state <chr>, lat <dbl>, lon <dbl>,
    ## #   disposition <chr>, city_state <chr>, is_solved <dbl>
