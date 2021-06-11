Experiments and Causality: Problem Set 3
================
Alex, Micah and Scott
12/7/2020

``` r
library(data.table)

library(sandwich)
library(lmtest)

library(ggplot2)
library(patchwork)

library(foreign)
library(knitr)
```

# 1\. Replicate Results

Skim [Broockman and Green’s](./readings/brookman_green_ps3.pdf) paper on
the effects of Facebook ads and download an anonymized version of the
data for Facebook users only.

``` r
d <- fread("../data/broockman_green_anon_pooled_fb_users_only.csv")

library(lmtest)
library(sandwich)
library(data.table)
library(stargazer)
```

    ## 
    ## Please cite as:

    ##  Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.

    ##  R package version 5.2.2. https://CRAN.R-project.org/package=stargazer

1.  Using regression without clustered standard errors (that is,
    ignoring the clustered assignment), compute a confidence interval
    for the effect of the ad on candidate name recognition in Study 1
    only (the dependent variable is `name_recall`). After you estimate
    your model, write a narrative description about what you’ve learned.

<!-- end list -->

  - **Note**: Ignore the blocking the article mentions throughout this
    problem.
  - **Note**: You will estimate something different than is reported in
    the study.

<!-- end list -->

``` r
#m1 <- d[ studyno == '1', lm(name_recall ~ treat_ad + positive_impression)]
m1 <- d[ studyno == 1, lm(name_recall ~ treat_ad )] # + positive_impression)]

# mod_study1 <- d[studyno == 1, lm(name_recall ~ treat_ad)]

m1$vcovHC_ <- vcovHC(m1)
coeftest(m1, vcov. = m1$vcovHC_)
```

    ## 
    ## t test of coefficients:
    ## 
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.1824687  0.0163651 11.1499   <2e-16 ***
    ## treat_ad    -0.0097979  0.0211120 -0.4641   0.6427    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# # 
# stargazer(
#   m1,
#   type = 'text',
#   se=list(sqrt(diag(m1$vcovHC_))),
#   header=F
#   )
# 
# confint(m1, level=0.95)                        #conf interval using built-in formula
# sum_ols <- summary(m1)$coefficients  
# sum_ols

coefci(m1, vcov. = vcovHC)
```

    ##                   2.5 %     97.5 %
    ## (Intercept)  0.15036520 0.21457219
    ## treat_ad    -0.05121351 0.03161774

``` r
# mod_study1 <- 'fill this in' # should be a lm class object

#Note that it is possible to get a negative R-square for equations that do not contain a constant term. Because R-square is defined as the proportion of variance explained by the fit, if the fit #is actually worse than just fitting a horizontal line then R-square is negative.
```

2.  What are the clusters in Broockman and Green’s study? Why might
    taking clustering into account increase the standard errors?

> Answer here.

the study is clustered on similar

3.  Estimate a regression that estimates the effect of the ad on
    candidate name recognition in Study 1, but this time take take
    clustering into account when you compute the standard errors.

<!-- end list -->

  - The estimation of the *model* does not change, only the estimation
    of the standard errors.
  - You can estimate these clustered standard errors using
    `sandwich::vcovCL`, which means: "The `vcovCL` function from the
    `sandwich` package.
  - We talk about this more in code that is availbe in the course repo.

<!-- end list -->

``` r
## one way clustering 

#m1_cl <- d[ studyno == '1', lm(name_recall ~ treat_ad + positive_impression)]
m1_cl <- d[ studyno == 1, lm(name_recall ~ treat_ad )]

m1_cl$vcovCL1_ <- vcovCL(m1_cl, cluster = d[ studyno == 1, cluster])
m1_cl$vcovCL1_ <- vcovCL(m1_cl, cluster = d[ studyno == 1, cluster])

#coeftest(m1_cl, m1_cl$vcovCL1_)

coefci(m1_cl, vcov. = vcovCL)
```

    ##                   2.5 %     97.5 %
    ## (Intercept)  0.15039911 0.21453828
    ## treat_ad    -0.05117881 0.03158304

``` r
coefci(m1_cl, vcov = m1_cl$vcovCL_)
```

    ##                   2.5 %     97.5 %
    ## (Intercept)  0.15080247 0.21413492
    ## treat_ad    -0.05101765 0.03142188

``` r
coefci(m1_cl, vcov. = vcovCL)
```

    ##                   2.5 %     97.5 %
    ## (Intercept)  0.15039911 0.21453828
    ## treat_ad    -0.05117881 0.03158304

``` r
# 
# confint(m1_cl, level=0.95)                        #conf interval using built-in formula
# sum_ols <- summary(m1_cl)$coefficients  
# sum_ols





m1_cl <- d[ studyno == 1, lm(name_recall ~ treat_ad )]
m1_cl$vcovCL1_ <- vcovCL(m1_cl, cluster = d[ studyno == 1, cluster])
coefci(m1_cl, vcov = m1_cl$vcovCL_)
```

    ##                   2.5 %     97.5 %
    ## (Intercept)  0.15080247 0.21413492
    ## treat_ad    -0.05101765 0.03142188

``` r
# 
# 2.5 %     97.5 %
# (Intercept)  0.14619376 0.21874363
# treat_ad    -0.05639555 0.03679977

mod_study1 <- d[studyno == 1, lm(name_recall ~ treat_ad)]
mod_study1$vcovCL_ <- vcovCL(mod_study1, cluster = d[studyno == 1, cluster])
coefci(mod_study1, vcov = mod_study1$vcovCL_)
```

    ##                   2.5 %     97.5 %
    ## (Intercept)  0.14619376 0.21874363
    ## treat_ad    -0.05639555 0.03679977

4.  Change the context: estimate the treatment effect in Study 2, using
    clustered standard errors. If you’ve written your code for part 3
    carefully, you should be able to simply change the row-scoping that
    you’re calling. If you didn’t write it carefully, for legibility for
    your colleagues, you might consider re-writting your solution to the
    last question. Descriptively, do the treatment effects look
    different between the two studies? Are you able to conduct a formal
    test by comparing these coefficients? Why, or why not?

<!-- end list -->

``` r
mod_study2 <- 'fill this in' # should be a lm class object

mod_study2 <- d[studyno == 2, lm(name_recall ~ treat_ad)]
mod_study2$vcovCL_ <- vcovCL(mod_study2, cluster = d[studyno == 2, cluster])
coefci(mod_study2, vcov = mod_study2$vcovCL_)
```

    ##                   2.5 %     97.5 %
    ## (Intercept)  0.57010643 0.64147042
    ## treat_ad    -0.07245176 0.06684507

``` r
stargazer(
  mod_study2,
  type = 'text',
  se=list(sqrt(diag(mod_study2$vcovCL_))),
  header=F
  )
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                             name_recall        
    ## -----------------------------------------------
    ## treat_ad                      -0.003           
    ##                               (0.036)          
    ##                                                
    ## Constant                     0.606***          
    ##                               (0.018)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   1,337           
    ## R2                            0.00001          
    ## Adjusted R2                   -0.001           
    ## Residual Std. Error      0.489 (df = 1335)     
    ## F Statistic            0.008 (df = 1; 1335)    
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
#narrative: can you run a formal test or not? alex OH: min 40. Deploy graphs and tell a story. 
#https://statisticsbyjim.com/regression/interaction-effects/
#treatment group in study 1: 805, study2: 
#one study has overwhelmingly large number of people in the treatment and control
#plot interaction plot between study1 * treatment -> interaction between treatment and study 
#candidates -> advertising different context in study1 & 2 (cant compare apples to apples)
#
```

> 

5.  Run a regression to test for the effect of the ad on candidate name
    recognition, but this time use the entire sample from both studies –
    do not take into account which study the data is from (more on this
    in a moment), but just “pool” the data.

<!-- end list -->

  - Does this estimate tell you anything useful?
  - Why or why not?
  - Can you say that the treatment assignment procedure used is fully
    random when you estimate this model? Or is there some endogeneous
    process that could be confounding your estimate?

<!-- end list -->

``` r
mod_pooled <- 'fill this in' # should be a lm class object

mod_pooled <- d[, lm(name_recall ~ treat_ad)]
mod_pooled$vcovCL_ <- vcovCL(mod_pooled, cluster = d[, cluster])
coefci(mod_pooled, vcov = mod_pooled$vcovCL_)
```

    ##                  2.5 %     97.5 %
    ## (Intercept)  0.4177709  0.4906211
    ## treat_ad    -0.2074875 -0.1026589

``` r
coeftest(mod_pooled, mod_pooled$vcovCL1_)
```

    ## 
    ## t test of coefficients:
    ## 
    ##              Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)  0.454196   0.012189 37.2622 < 2.2e-16 ***
    ## treat_ad    -0.155073   0.018762 -8.2652 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
## call out differences in context here
# cant connect a formal test with the model 
## treatment assignment process is not fully rndom -- you shouldnt take out the study that someone else did 
## could violate the inference/excludability
# how many are in treatment and contorl in each study (e.g. study 2 has more people --> therefore candidate2 is lot more favorable in study 2)
# the study1 took place a month before and study 2 took place a week before the election. 
```

> 

6.  Estimate a model that uses all the data, but this time include a
    variable that identifies whether an observation was generated during
    Study 1 or Study 2.

<!-- end list -->

  - What is estimated in the “Study 2 Fixed Effect”?
  - What is the treatment effect estimate and associated p-value?
  - Think a little bit more about the treatment effect that you’ve
    estimated: Can this treatment effect, as you’ve entered it in the
    model be *different* between Study 1 and Study 2?
  - Why or why not?

<!-- end list -->

``` r
mod_fe <- 'fill this in'

mod_fe <- d[, lm(name_recall ~ treat_ad + as.factor(studyno))] # as.factor -->> to 
summary(mod_fe)
```

    ## 
    ## Call:
    ## lm(formula = name_recall ~ treat_ad + as.factor(studyno))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.6068 -0.1807 -0.1739  0.3932  0.8261 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.180685   0.015994  11.297   <2e-16 ***
    ## treat_ad            -0.006775   0.018177  -0.373    0.709    
    ## as.factor(studyno)2  0.426099   0.017955  23.731   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4381 on 2698 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.1931, Adjusted R-squared:  0.1925 
    ## F-statistic: 322.8 on 2 and 2698 DF,  p-value: < 2.2e-16

``` r
#function form doesnt allow treatment effect ot change between studies
```

> 

7.  Estimate a model that lets the treatment effects be different
    between Study 1 and Study 2. With this model, conduct a formal test
    – it must have a p-value associated with the test – for whether
    the treatment effects are different in Study 1 than Study 2.

<!-- end list -->

``` r
mod_interaction <- 'fill this in'

mod_interaction <- d[, lm(name_recall ~ treat_ad + as.factor(studyno) + (as.factor(studyno) * treat_ad))] 
summary(mod_interaction)
```

    ## 
    ## Call:
    ## lm(formula = name_recall ~ treat_ad + as.factor(studyno) + (as.factor(studyno) * 
    ##     treat_ad))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.6058 -0.1825 -0.1727  0.3942  0.8273 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   0.182469   0.018534   9.845   <2e-16 ***
    ## treat_ad                     -0.009798   0.024125  -0.406    0.685    
    ## as.factor(studyno)2           0.423320   0.023133  18.299   <2e-16 ***
    ## treat_ad:as.factor(studyno)2  0.006995   0.036699   0.191    0.849    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4382 on 2697 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.1931, Adjusted R-squared:  0.1922 
    ## F-statistic: 215.2 on 3 and 2697 DF,  p-value: < 2.2e-16

``` r
# you are not using an interaction term -> the coeff 

# 0.485
```

# 2\. Peruvian Recycling

Look at [this article](./readings/recycling_peru.pdf) about encouraging
recycling in Peru. The paper contains two experiments, a “participation
study” and a “participation intensity study.” In this problem, we will
focus on the latter study, whose results are contained in Table 4 in
this problem. You will need to read the relevant section of the paper
(starting on page 20 of the manuscript) in order to understand the
experimental design and variables. (*Note that “indicator variable” is a
synonym for “dummy variable,” in case you haven’t seen this language
before.*)

1.  In Column 3 of Table 4A, what is the estimated ATE of providing a
    recycling bin on the average weight of recyclables turned in per
    household per week, during the six-week treatment period? Provide a
    95% confidence interval.

ATE is the same as what is mentioned as the coefficient estimate by the
model at 0.187 Confidence interval 0.251 and 0.123

2.  In Column 3 of Table 4A, what is the estimated ATE of sending a text
    message reminder on the average weight of recyclables turned in per
    household per week? Provide a 95% confidence interval. ATE is the
    same as what is mentioned as the coefficient estimate by the model
    at -0.024 Confidence interval 0.054 and 0.054

3.  Which outcome measures in Table 4A show statistically significant
    effects (at the 5% level) of providing a recycling bin?

<!-- end list -->

  - Percentage of visits turned in a bag
  - Avg. no. of bins turned in per week
  - Avg. weight (in kg) of recyclables turned in per week

<!-- end list -->

4.  Which outcome measures in Table 4A show statistically significant
    effects (at the 5% level) of sending text messages? None

5.  Suppose that, during the two weeks before treatment, household A
    turns in 2kg per week more recyclables than household B does, and
    suppose that both households are otherwise identical (including
    being in the same treatment group). From the model, how much more
    recycling do we predict household A to have than household B, per
    week, during the six weeks of treatment? Provide only a point
    estimate, as the confidence interval would be a bit complicated.
    This question is designed to test your understanding of slope
    coefficients in regression.

\(Y_{AB} = [\beta _{A1} B + \beta _{A2} S + \lambda Ybl_{A} + P_{A} + \alpha_{j} + \epsilon_{i}] - [\beta _{B1} B + \beta _{B2} S + \lambda Ybl_{j} + P_{B} + \alpha_{j} + \epsilon_{i}]\)
represents the difference in model output for households A and B

\(\alpha_{j}\) cancels out because of fixed street effects that are same
for both households A and B. \(\epsilon_{i}\) cancels out because
experiments are randomized and observations are identical and
independently distributed (i.i.d), hence we can assume error terms are
homoskedastic (constant variance of error terms). \(B, S, P_{i}\) are
indicator variables, hence cancel out between A and B households (as
they are identical being in the same treatment groups) Therefore, we
only have difference in model prediction due to \(\lambda Ybl_{AB}\)
only.

Which comes to 0.562

6.  Suppose that the variable “percentage of visits turned in bag,
    baseline” had been left out of the regression reported in Column 1.
    What would you expect to happen to the results on providing a
    recycling bin? Would you expect an increase or decrease in the
    estimated ATE? Would you expect an increase or decrease in the
    standard error? Explain our reasoning.

7.  In column 1 of Table 4A, would you say the variable “has cell phone”
    is a bad control? Explain your reasoning.

8.  If we were to remove the “has cell phone” variable from the
    regression, what would you expect to happen to the coefficient on
    “Any SMS message”? Would it go up or down? Explain your reasoning.

# 3\. Multifactor Experiments

Staying with the same experiment, now think about multifactor
experiments.

1.  What is the full experimental design for this experiment? Tell us
    the dimensions, such as 2x2x3. (Hint: the full results appear in
    Panel 4B.)

> 

2.  In the results of Table 4B, describe the baseline category. That is,
    in English, how would you describe the attributes of the group of
    people for whom all dummy variables are equal to zero?

> 

3.  In column (1) of Table 4B, interpret the magnitude of the
    coefficient on “bin without sticker.” What does it mean?

> 

4.  In column (1) of Table 4B, which seems to have a stronger treatment
    effect, the recycling bin with message sticker, or the recycling bin
    without sticker? How large is the magnitude of the estimated
    difference?

> 

5.  Is this difference you just described statistically significant?
    Explain which piece of information in the table allows you to answer
    this question.

> 

6.  Notice that Table 4C is described as results from “fully saturated”
    models. What does this mean? Looking at the list of variables in the
    table, explain in what sense the model is “saturated.”

> 

# 4\. Now\! Do it with data

Download the data set for the recycling study in the previous problem,
obtained from the authors. We’ll be focusing on the outcome variable
Y=“number of bins turned in per week” (avg\_bins\_treat).

``` r
d <- foreign::read.dta("../data/karlan_data_subset_for_class.dta")
d <- data.table(d)
head(d)
```

    ##    street havecell avg_bins_treat base_avg_bins_treat bin sms bin_s bin_g sms_p
    ## 1:      7        1      1.0416666               0.750   1   1     1     0     0
    ## 2:      7        1      0.0000000               0.000   0   1     0     0     1
    ## 3:      7        1      0.7500000               0.500   0   0     0     0     0
    ## 4:      7        1      0.5416667               0.500   0   0     0     0     0
    ## 5:      6        1      0.9583333               0.375   1   0     0     1     0
    ## 6:      8        0      0.2083333               0.000   1   0     0     1     0
    ##    sms_g
    ## 1:     1
    ## 2:     0
    ## 3:     0
    ## 4:     0
    ## 5:     0
    ## 6:     0

``` r
## Do some quick exploratory data analysis with this data. 
## There are some values in this data that seem a bit strange. 

## Determine what these are. 
## Don't make an assessment about keeping, changing, or 
## dropping just yet, but at any point that your analysis touches 
## these variables, you'll have to determine what is appropriate 
## given the analysis you are conducting. 
```

1.  For simplicity, let’s start by measuring the effect of providing a
    recycling bin, ignoring the SMS message treatment (and ignoring
    whether there was a sticker on the bin or not). Run a regression of
    Y on only the bin treatment dummy, so you estimate a simple
    difference in means. Provide a 95% confidence interval for the
    treatment effect, using **of course** robust standard errors (use
    these throughout).

<!-- end list -->

``` r
mod_1 <- 'fill this in'
```

2.  Now add the pre-treatment value of Y as a covariate. Provide a 95%
    confidence interval for the treatment effect. Explain how and why
    this confidence interval differs from the previous one.

<!-- end list -->

``` r
mod_2 <- 'fill this in'
```

3.  Now add the street fixed effects. (You’ll need to use the R command
    factor().) Provide a 95% confidence interval for the treatment
    effect.

<!-- end list -->

``` r
mod_3 <- 'fill this in'
```

4.  Recall that the authors described their experiment as “stratified at
    the street level,” which is a synonym for blocking by street. Does
    including these block fixed effects change the standard errors of
    the estimates *very much*? Conduct the appropriate test for the
    inclusion of these block fixed effects, and interpret them in the
    context of the other variables in the regression.

<!-- end list -->

``` r
mod_4 <- 'fill this in'
```

``` r
test_fixed_effects <- 'fill this in'
```

> 

5.  Perhaps having a cell phone helps explain the level of recycling
    behavior. Instead of “has cell phone,” we find it easier to
    interpret the coefficient if we define the variable " no cell
    phone." Give the R command to define this new variable, which equals
    one minus the “has cell phone” variable in the authors’ data set.
    Use “no cell phone” instead of “has cell phone” in subsequent
    regressions with this dataset.

6.  Now add “no cell phone” as a covariate to the previous regression.
    Provide a 95% confidence interval for the treatment effect. Explain
    why this confidence interval does not differ much from the previous
    one.

<!-- end list -->

``` r
mod_5 <- 'fill this in'
```

7.  Now let’s add in the SMS treatment. Re-run the previous regression
    with “any SMS” included. You should get the same results as in Table
    4A. Provide a 95% confidence interval for the treatment effect of
    the recycling bin. Explain why this confidence interval does not
    differ much from the previous one.

<!-- end list -->

``` r
mod_6 <- 'fill this in'
```

8.  Now reproduce the results of column 2 in Table 4B, estimating
    separate treatment effects for the two types of SMS treatments and
    the two types of recycling-bin treatments. Provide a 95% confidence
    interval for the effect of the unadorned recycling bin. Explain how
    your answer differs from that in part (g), and explain why you think
    it differs.

<!-- end list -->

``` r
mod_7 <- 'fill this in'
```

# 5\. A Final Practice Problem

Now for a fictional scenario. An emergency two-week randomized
controlled trial of the experimental drug ZMapp is conducted to treat
Ebola. (The control represents the usual standard of care for patients
identified with Ebola, while the treatment is the usual standard of care
plus the drug.)

Here are the (fake) data.

``` r
d <- fread("../data/ebola_rct2.csv")
head(d)
```

    ##    temperature_day0 dehydrated_day0 treat_zmapp temperature_day14
    ## 1:         99.53168               1           0          98.62634
    ## 2:         97.37372               0           0          98.03251
    ## 3:         97.00747               0           1          97.93340
    ## 4:         99.74761               1           0          98.40457
    ## 5:         99.57559               1           1          99.31678
    ## 6:         98.28889               1           1          99.82623
    ##    dehydrated_day14 male
    ## 1:                1    0
    ## 2:                1    0
    ## 3:                0    1
    ## 4:                1    0
    ## 5:                1    0
    ## 6:                1    1

You are asked to analyze it. Patients’ temperature and whether they are
dehydrated is recorded on day 0 of the experiment, then ZMapp is
administered to patients in the treatment group on day 1. Dehydration
and temperature is again recorded on day 14.

1.  Without using any covariates, answer this question with regression:
    What is the estimated effect of ZMapp (with standard error in
    parentheses) on whether someone was dehydrated on day 14? What is
    the p-value associated with this estimate?

<!-- end list -->

``` r
mod_1 <- 'fill this in'
```

2.  Add covariates for dehydration on day 0 and patient temperature on
    day 0 to the regression from part (a) and report the ATE (with
    standard error). Also report the p-value.

<!-- end list -->

``` r
mod_2 <- 'fill this in'
```

3.  Do you prefer the estimate of the ATE reported in part (a) or part
    (b)? Why? Report the results of the F-test that you used to form
    this opinion.

<!-- end list -->

``` r
test_object <- 'fill this in'
```

> 

4.  The regression from part (2) suggests that temperature is highly
    predictive of dehydration. Add, temperature on day 14 as a covariate
    and report the ATE, the standard error, and the p-value.

<!-- end list -->

``` r
mod_3 <- 'fill this in'
```

5.  Do you prefer the estimate of the ATE reported in part (b) or part
    (d)? What is this preference based on?

> 

6.  Now let’s switch from the outcome of dehydration to the outcome of
    temperature, and use the same regression covariates as in the chunk
    titled `add pre-treatment measures`. Test the hypothesis that ZMapp
    is especially likely to reduce mens’ temperatures, as compared to
    womens’, and describe how you did so. What do the results suggest?

<!-- end list -->

``` r
mod_4 <- 'fill this in'
```

7.  Which group – those that are coded as `male == 0` or `male == 1`
    have better health outcomes in control? What about in treatment? How
    does this help to contextualize whatever heterogeneous treatment
    effect you might have estimated?

> 

8.  Suppose that you had not run the regression in part (7). Instead,
    you speak with a colleague to learn about heterogeneous treatment
    effects. This colleague has access to a non-anonymized version of
    the same dataset and reports that they looked at heterogeneous
    effects of the ZMapp treatment by each of 80 different covariates to
    examine whether each predicted the effectiveness of ZMapp on each of
    20 different indicators of health. Across these regressions your
    colleague ran, the treatment’s interaction with gender on the
    outcome of temperature is the only heterogeneous treatment effect
    that he found to be statistically significant. They reason that this
    shows the importance of gender for understanding the effectiveness
    of the drug, because nothing else seemed to indicate why it worked.
    Bolstering your colleague’s confidence, after looking at the data,
    they also returned to his medical textbooks and built a theory about
    why ZMapp interacts with processes only present in men to cure.
    Another doctor, unfamiliar with the data, hears your colleague’s
    theory and finds it plausible. How likely do you think it is ZMapp
    works especially well for curing Ebola in men, and why? (This
    question is conceptual can be answered without performing any
    computation.)

> 

9.  Now, imagine that your colleague’s fishing expedition did not
    happen, but that you had tested this heterogeneous treatment effect,
    and only this heterogeneous treatment effect, of your own accord.
    Would you be more or less inclined to believe that the heterogeneous
    treatment effect really exists? Why?

> 

10. Now, imagine that your colleague’s fishing expedition **did**
    happen, but that you on your own tested this and only this HTE,
    discover a positive result and conclude there is an effect. How does
    your colleague’s behavior change the interpretation of your test?
    Does this seem fair or reasonable?

>
