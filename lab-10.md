Lab 10 - Grading the professor
================
Cailey Fay
3.20.26

Here is a link to the [lab
instructions](https://datascience4psych.github.io/DataScience4Psych/lab10.html).

## Load Packages and Data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
evals <- as.data.frame(evals)
```

## Part 1: Getting to know the outcome

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

1.  A one-point increase in beauty predicts a .066 point increase in
    evaluation score. If someone had a beauty score of 0, their
    predicted evaluation score is 3.88. Score = .066(beauty) + 3.88

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

2.  Replot your existing visualization, this time add a regression line
    in orange. Turn off the default shading around the line. (By
    default, the plot includes shading around the line.)

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

3.  How much do evaluation scores change with beauty ratings (slope)?
    The slope is .066, meaning that evaluation scores increase by .066
    for every one-point increase in beauty. The intercept is 3.88, which
    means that if someone had an attractiveness rating of 0, their
    predicted evaluation score is 3.88. In this case, the intercept is
    meaningful, since it is possible someone could get a 0 on
    attractiveness. The R-squared value is .035, which means that 3.5%
    of the variance in evaluation scores is explained by attractiveness.
    Therefore, attractiveness matters, but 96.5% of the variance in
    evaluation scores are left unexplained, so it is a minor piece of
    the story.

The shading is turned off because right not we are just looking at how
the model fits the data in our sample. We are not trying to make any
inferences about the population just yet.

## Part 3

1.  If I am correct in thinking female is coded as 1 and male as 2: the
    coefficients suggest that the average male professor has an
    evaluation score .14 points higher than the average female
    professor. Mean male = 4.37, mean female = 4.233.

``` r
m_gen <- lm(score ~ gender, data = evals)
tidy(m_gen)
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)    4.09     0.0387    106.   0      
    ## 2 gendermale     0.142    0.0508      2.78 0.00558

2.  Mutating:

``` r
evals <- evals %>%
  mutate(rank_relevel = as.factor(case_when(
    rank == "teaching" ~ 2,
    rank == "tenure track" ~ 0,
    rank == "tenured" ~ 1)),
    tenure_eligible = case_when(
      rank == "teaching" ~ "no",
      rank == "tenure track" ~ "yes",
      rank == "tenured" ~ "yes"
    ))
```

Modeling:

``` r
#evaluations by rank regular 
m_rank <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ rank, evals) 
glance(m_rank)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0116       0.00733 0.542      2.71  0.0679     2  -372.  752.  768.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
#evaluations by rank_relevel
m_rel <- lm(score ~ rank_relevel, data = evals)
tidy(m_rel)
```

    ## # A tibble: 3 × 5
    ##   term          estimate std.error statistic   p.value
    ##   <chr>            <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     4.15      0.0521    79.7   2.58e-271
    ## 2 rank_relevel1  -0.0155    0.0623    -0.249 8.04e-  1
    ## 3 rank_relevel2   0.130     0.0748     1.73  8.37e-  2

``` r
#evaluations by tenure_eligible 
m_ten <- lm(score ~ tenure_eligible, data = evals)
tidy(m_ten)
```

    ## # A tibble: 2 × 5
    ##   term               estimate std.error statistic   p.value
    ##   <chr>                 <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           4.28     0.0536     79.9  2.72e-272
    ## 2 tenure_eligibleyes   -0.141    0.0607     -2.32 2.10e-  2

3.  Based on the regression outputs, interpret how teaching faculty and
    tenured faculty differ from that baseline. (Hint you should
    interpret the slopes and intercepts for all three models in context
    of the data.)

In model 1, the baseline is teaching. The intercept is 4.28, which is
the average score for teaching track professors. Tenure track
professors, on average, score .13 below baseline, and tenured professors
are .14 points below baseline.

In model 2, the baseline is tenure. Now the intercept is 4.15
(representing the mean score for the tenured profs), and tenure track
professors score .02 below the tenured profs, and teaching professors
score .13 above tenured profs.

In model 3, it is broken down into tenure eligible vs not, and so tenure
track and tenure are collapsed into one group. The intercept is 4.28,
which (once again) representings the teaching profs score. on average,
the combined tenure eligible group is .14 points below the teaching
score.

4.  These models are all communicating the same thing, but swapping
    around whose mean is in the intercept slot. R2 is .01, meaning that
    it explains only 1% of the variance in evaluation scores.

## Part 4 Multiple Linear Regression

Is the “beauty effect” still there once we account for gender? Short
answer is yes. Adding gender to the model increases the R2 from .03 to
.06. Furthermore, attractiveness is still a significant predictor,
controlling for gender.

``` r
#model 1
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

``` r
#model 2
m_bty_gen<- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg + gender, evals) #%>%
 # tidy()
glance(m_bty_gen)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic     p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>       <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0591        0.0550 0.529      14.5 0.000000818     2  -360.  729.  745.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

Then answer:

1.  What changes in the beauty slope when gender is added? Originally,
    the beauty slope was 0.066, and after adding in gender, it was .07,
    so it didn’t really change. This means that gender and beauty
    explain different proportions of variance in scores.

2.  For two professors with the same beauty rating, does gender still
    shift the predicted score? Yes. gender is a significant predictor of
    scores, over and above beauty.

3.  Compare the adjusted R2 values. Is gender actually helping much, or
    is beauty doing most of the work already? Both gender and beauty
    explain a small proportion of variance. beauty explains 3.5%, and
    then gender explains an additional 2.5%.

4.  The beauty slope is just about the same as before (.069), meaning
    that holding rank constant, for every one point increase in beauty,
    the predicted score increases by .07 points. The tenure track
    coefficient is -.16. That means that controlling for beauty, (on
    average) tenure profs score 16 pts lower than teaching profs. In
    other words, if you consider two professors who are equally
    attractive, the teaching prof is expected to score .16pts higher on
    evaluations than the tenured prof. I hope I am interpretting this
    correctly - trying to get used to these R outputs after looking at
    beautiful SPSS tables for so long.

``` r
m_bty_rank <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg + rank, evals) %>%
  tidy()
 #glance(m_bty_rank)
 #rsquared is .047
```

## Part 5

Going forward, only consider the following variables as potential
predictors: rank, ethnicity, gender, language, age, cls_perc_eval,
cls_did_eval, cls_students, cls_level, cls_profs, cls_credits, bty_avg.

1.  I would expect cls_did_eval to be the worst predictor, since its
    just the total number of students and that doesn’t mean anything.
    The proportion of students who did the eval might have some
    predictive power, but just count data on this seems like a stretch.

2.  Its not a significant predictor. The R2 is basically 0. I am
    assuming you don’t want me to throw every variable you listed in the
    model and see what sticks though?

``` r
p5 <- lm(score ~ cls_did_eval, evals)
summary(p5)
```

    ## 
    ## Call:
    ## lm(formula = score ~ cls_did_eval, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8545 -0.3595  0.1303  0.4269  0.8485 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.1469347  0.0325682 127.331   <2e-16 ***
    ## cls_did_eval 0.0007589  0.0005616   1.351    0.177    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5434 on 461 degrees of freedom
    ## Multiple R-squared:  0.003946,   Adjusted R-squared:  0.001786 
    ## F-statistic: 1.827 on 1 and 461 DF,  p-value: 0.1772

3.  You should not include cls_did_eval, because that is already
    accounted for by these two variables - if you multiply them, it
    gives the total number of students who completed the evaluation.

4.  Fit a full model with all predictors listed above (except for the
    one you decided to exclude) in the previous question.

``` r
full <- lm(score ~ rank + ethnicity+ gender + language + age + cls_perc_eval + cls_students + cls_level +cls_profs + cls_credits + bty_avg, evals)
summary(full)
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank + ethnicity + gender + language + age + 
    ##     cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + 
    ##     bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84482 -0.31367  0.08559  0.35732  1.10105 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.5305036  0.2408200  14.660  < 2e-16 ***
    ## ranktenure track      -0.1070121  0.0820250  -1.305 0.192687    
    ## ranktenured           -0.0450371  0.0652185  -0.691 0.490199    
    ## ethnicitynot minority  0.1869649  0.0775329   2.411 0.016290 *  
    ## gendermale             0.1786166  0.0515346   3.466 0.000579 ***
    ## languagenon-english   -0.1268254  0.1080358  -1.174 0.241048    
    ## age                   -0.0066498  0.0030830  -2.157 0.031542 *  
    ## cls_perc_eval          0.0056996  0.0015514   3.674 0.000268 ***
    ## cls_students           0.0004455  0.0003585   1.243 0.214596    
    ## cls_levelupper         0.0187105  0.0555833   0.337 0.736560    
    ## cls_profssingle       -0.0085751  0.0513527  -0.167 0.867458    
    ## cls_creditsone credit  0.5087427  0.1170130   4.348  1.7e-05 ***
    ## bty_avg                0.0612651  0.0166755   3.674 0.000268 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.504 on 450 degrees of freedom
    ## Multiple R-squared:  0.1635, Adjusted R-squared:  0.1412 
    ## F-statistic: 7.331 on 12 and 450 DF,  p-value: 2.406e-12

5.  The best model uses ethnicity, language, gender, age, percent eval,
    credits, and beauty. It explains nearly 16% of the variance in
    evaluation scores.

``` r
final <- lm(score ~  ethnicity + gender + language + age + cls_perc_eval + cls_credits + bty_avg, evals)
summary(final)
```

    ## 
    ## Call:
    ## lm(formula = score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_credits + bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9067 -0.3103  0.0849  0.3712  1.0611 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.446967   0.203191  16.964  < 2e-16 ***
    ## ethnicitynot minority  0.204710   0.074710   2.740 0.006384 ** 
    ## gendermale             0.184780   0.049889   3.704 0.000238 ***
    ## languagenon-english   -0.161463   0.103213  -1.564 0.118427    
    ## age                   -0.005008   0.002606  -1.922 0.055289 .  
    ## cls_perc_eval          0.005094   0.001438   3.543 0.000436 ***
    ## cls_creditsone credit  0.515065   0.104860   4.912 1.26e-06 ***
    ## bty_avg                0.064996   0.016327   3.981 7.99e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.503 on 455 degrees of freedom
    ## Multiple R-squared:  0.1576, Adjusted R-squared:  0.1446 
    ## F-statistic: 12.16 on 7 and 455 DF,  p-value: 2.879e-14

6.  Interpret the slopes of one numerical and one categorical predictor
    based on your final model. numerical: holding all other variables
    constant, for every additional year older, the evaluation score is
    expected to drop by .005 points. categorical: the ethnicity slope is
    .20. This means that holding the other variables constant, a
    non-minority prof will on average have an evaluation score .2 points
    higher than a minority prof.

7.  A handsome male english-speaking professor who got a large
    proportion of students to take the evaluation is more likely to
    receive a high score (but the model only predicts 16% of the
    variance, so its far from a sure bet).

8.  I am sort of comfortable. The model predicts a significant
    proportion of variance, and its plausible that demographic and
    course variables are relevant to the evaluation score.
    Inferentiallly it technically holds up, but its also unlikely that
    the sample is completely generalizable to other universitys.
