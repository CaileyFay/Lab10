Lab 10 - Grading the professor
================
Insert your name here
Insert date here

Here is a link to the [lab
instructions](https://datascience4psych.github.io/DataScience4Psych/lab10.html).

## Load Packages and Data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
evals <- as.data.frame(evals)
```

### Part 1: Getting to know the outcome

Before we model anything, let’s look at what students actually do with
the evaluation scale.

1.  The distribution is left skewed. Students are pretty generous -
    there is clustering around the higher scores. The average score is a
    4.3 and the median is 4.3. The minimum score is 2.3.

``` r
ggplot(evals, aes(x=score)) +
  geom_histogram() +
  labs(title = "Distribution of Evaluation Scores")
```

    ## `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

![](lab-10_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
summary(evals$score) %>% tidy()
```

    ## Warning in tidy.summaryDefault(.): `tidy.summaryDefault()` is deprecated.
    ## Please use `skimr::skim()` instead.

    ## # A tibble: 1 × 6
    ##   minimum    q1 median  mean    q3 maximum
    ##     <dbl> <dbl>  <dbl> <dbl> <dbl>   <dbl>
    ## 1     2.3   3.8    4.3  4.17   4.6       5

2.  It looks like there could be a ceiling effect. The scatterplot shows
    a very weak positive association, but the concentration of high
    evaluation scores makes interpretation challenging. Scores are all
    over the place.

``` r
ggplot(evals, aes(x=bty_avg, y=score)) +
  geom_jitter() +
  labs(title = "Attractiveness and Evaluation Scores",
       x = "Attractiveness Rating",
       y = "Evaluation Score") +
  theme_minimal()
```

![](lab-10_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

3.  Whoops I used jitter to begin with, because I looked at that regular
    scatterplot and said “no.” Heres the plot without jitter. You can
    also tell from this plot that there are a lot of high evaluation
    scores and maybe there is a positve trend, but jitter is better.
    Jitter shows more of the dots (if I’m not mistaken) which can help
    highlight concentrations of observations with similar scores on x
    and y.

``` r
ggplot(evals, aes(x=bty_avg, y=score)) +
  geom_point() +
  labs(title = "Attractiveness and Evaluation Scores",
       x = "Attractiveness Rating",
       y = "Evaluation Score") +
  theme_minimal()
```

![](lab-10_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Part 2: Beauty as a predictor

Let’s see if the apparent trend in the plot is something more than
natural variation.

## Exercise 1.

A one-point increase in beauty predicts a .066 point increase in
evaluation score. If someone had a beauty score of 0, their predicted
evaluation score is 3.88. Score = .066(beauty) + 3.88

``` r
m_bty <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg, evals) 

glance(m_bty)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0350        0.0329 0.535      16.7 0.0000508     1  -366.  738.  751.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

## Exercise 2.

Replot your existing visualization, this time add a regression line in
orange. Turn off the default shading around the line. (By default, the
plot includes shading around the line.)

``` r
ggplot(evals, aes(x=bty_avg, y=score)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(title = "Attractiveness and Evaluation Scores",
       x = "Attractiveness Rating",
       y = "Evaluation Score") +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](lab-10_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Then, stepping back, interpret what this model is really saying:

## Exercise 3.

How much do evaluation scores change with beauty ratings (slope)? The
slope is .066, meaning that evaluation scores increase by .066 for every
one-point increase in beauty. The intercept is 3.88, which means that if
someone had an attractiveness rating of 0, their predicted evaluation
score is 3.88. In this case, the intercept is meaningful, since it is
possible someone could get a 0 on attractiveness. The R-squared value is
.035, which means that 3.5% of the variance in evaluation scores is
explained by attractiveness. Therefore, attractiveness matters, but
96.5% of the variance in evaluation scores are left unexplained, so it
is a minor piece of the story.

The shading is turned off because right not we are just looking at how
the model fits the data in our sample. We are not trying to make any
inferences about the population just yet.

``` r
# Add your R code here
```

## Exercise 2

*Provide your answer here.*  
Add code chunks as needed.

``` r
# Add your R code here
```

## Additional Exercises

*Repeat the format above for additional exercises.*

## Hint

For Exercise 12, the `relevel()` function can be helpful!
