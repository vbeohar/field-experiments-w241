Why is a random sample an unbiased estimator?
================

**The** crucial feature that makes an experiment an experiment is the
*intervention* that we perform. But, since we’re not only looking to
conduct an experiment, but also reliably demonstrate with data that some
outcome was *caused* by some action, we’re going to need a way to
demonstrate this.

## What does it mean for an estimator to be unbiased?

In order to understand whether an estimator is biased or unbiased, we’ve
got to be able to make a statement about the *truth*. The most
interesting questions that we ask as data scientists typically concern
statements about population characteristics that we can’t *directly*
observe.

  - What feeling did this person have when they wrote this sentence?
  - What *real world object* is in this picture?
  - What is *this person’s* likelihood to take action?
  - What is the *causal* effect of X on Y?

For each of these circumstances we’ve got to put together an estimate of
the population value.

Throughout this course, and in most statistics courses, it is convention
to denote *estimators* of a parameter value with a “hat”. And so, if the
true parameter that we’re interested in knowing about is the individual
causal effect for some person
![D](https://latex.codecogs.com/png.latex?D "D"),
![\\tau\_{D}](https://latex.codecogs.com/png.latex?%5Ctau_%7BD%7D
"\\tau_{D}"), then we might denote the estimate for that person as
![\\hat{\\tau}\_{D}](https://latex.codecogs.com/png.latex?%5Chat%7B%5Ctau%7D_%7BD%7D
"\\hat{\\tau}_{D}").

An **unbiased** estimator of an unknown value is an estimator whose
*expected value* is equal to the *true* parameter value. And so, in the
case of an individual causal effect we might say that an an estimator of
an individual causal effect,
![\\hat{\\tau}\_{i}](https://latex.codecogs.com/png.latex?%5Chat%7B%5Ctau%7D_%7Bi%7D
"\\hat{\\tau}_{i}") is unbiased if

  
![&#10;E\[\\hat{\\tau}\_{i}\] =
\\tau&#10;](https://latex.codecogs.com/png.latex?%0AE%5B%5Chat%7B%5Ctau%7D_%7Bi%7D%5D%20%3D%20%5Ctau%0A
"
E[\\hat{\\tau}_{i}] = \\tau
")  

Or equivalently, if

  
![&#10;E\[\\hat{\\tau}\_{i} - \\tau\] = 0
&#10;](https://latex.codecogs.com/png.latex?%0AE%5B%5Chat%7B%5Ctau%7D_%7Bi%7D%20-%20%5Ctau%5D%20%3D%200%20%0A
"
E[\\hat{\\tau}_{i} - \\tau] = 0 
")  

## Is the mean an unbiased estimator of a population parameter?

Consider the following example: Your section instructor, wanting to
choose his or her examples to fit in with the pop-culture references
that *you* are most familiar with, wants to asses how old you are. But,
the section instructor also is not so impolite as to ask you directly.
How could they construct an estimate for your age?

``` r
library(data.table)

class_population <- data.table( 
  'id'    = 1:15, 
  'names' = LETTERS[1:15], 
  'age'  = sample(24:44, size = 15, replace = TRUE)
  ) 
class_population[ , you := c(1, rep(0,14))] # indicator for "you";  a single 1 and 14 zeros 
class_population[ , you := sample(you)] # shuffle that indicator
head(class_population)
```

    ##    id names age you
    ## 1:  1     A  31   0
    ## 2:  2     B  40   0
    ## 3:  3     C  29   0
    ## 4:  4     D  32   0
    ## 5:  5     E  31   0
    ## 6:  6     F  27   0

So, how old are *you*?

``` r
your_age <- class_population[you == 1, age]
your_age
```

    ## [1] 42

How old are the other people in the class? Write the slice that will
pull this.

Here is *one* way that I could make a guess about how old you are – pick
one person at random from the class, and them their age, and make that
my guess for your age.

``` r
first_estimator <- class_population[you == 0 , sample(age, 1)]
```

How far off was I?

``` r
first_error = your_age - first_estimator 
first_error
```

    ## [1] 15

So, I wasn’t **right** on. But, that’s just a result of only having done
this once. The expectation operator places a probability distribution
across each of the possible realizations, and then multiplies by the
value of that realization. Then we sum those results.

If I am sampling one person at random, then that would look like this,
the probability of sampling a single person is:

``` r
class_population[ , prob_sample := (1 / sum(you==0))]
head(class_population)
```

    ##    id names age you prob_sample
    ## 1:  1     A  31   0  0.07142857
    ## 2:  2     B  40   0  0.07142857
    ## 3:  3     C  29   0  0.07142857
    ## 4:  4     D  32   0  0.07142857
    ## 5:  5     E  31   0  0.07142857
    ## 6:  6     F  27   0  0.07142857

Each persons contribution to the expectation operator is:

``` r
class_population[ , p_times_age := prob_sample * age]
```

And so the expectation of this **estimator**, which comes from sampling
one person from the classroom is:

``` r
expectation <- class_population[ , sum(p_times_age)]
expectation
```

    ## [1] 34.42857

How far off was I?

``` r
your_age - expectation
```

    ## [1] 7.571429

Think about doing this a large number of times, where the ‘you’
indicator is randomly sampled from the population, and then the
estimator of one draw is conducted. To do this, I’m going to pull all
the pieces from the last set into a function, so we can run this
function a number of times.

``` r
NSIMS <- 1000

class_population <- data.table( 
  'id'    = 1:15, 
  'names' = LETTERS[1:15], 
  'age'  = sample(24:44, size = 15, replace = TRUE)
  ) 

class_example <- function(class_object=class_population) { 
    class_object[ , you := c(1, rep(0,14))] # indicator for "you";  a single 1 and 14 zeros 
    class_object[ , you := sample(you)] # shuffle that indicator
    
    class_mean <- class_object[you==0, mean(age)]
    your_age   <- class_object[you==1, age]
    
    difference <- your_age - class_mean
    
    return(difference)
}
```

Now that we have the function built, you can imagine looping through a
large number of these. To do so, first create a results vector, and then
into each of the positions in the results vector store the results of
the the `class_example()` function.

``` r
diff_vector <- rep(NA, NSIMS)

for(i in 1:NSIMS){ 
  diff_vector[i] <- class_example()
}
```

(While this loop is really explicit, it isn’t the most efficient or
preferred way of performing this task. Instead, using the
`replicate(1000, class_example())` is preferred.)

# Answers from that task

The difference between your age and the estimate tor, across these runs
is -0.0325. That’s mighty good\!

# Relate this directly to potential outcomes

How does this relate directly to potential outcomes? Consider the
*science* table that is provided to us in table 2.1 of *Field
Experiments*?

``` r
d <- data.table(
  'id'  = 1:7, 
  'y_0' = c(10,15,20,20,10,15,15), 
  'y_1' = c(15,15,30,15,20,15,30), 
  'tau' = c(5, 0,10,-5,10,0,15)
  )
```

We can’t *actually* see both the potential outcome to control and
treatment for any **single** person. But, what if we can come up with an
unbiased estimate for the potential outcomes to control for the seven
villages?

Randomly sample 5 of the villages and take the average of their
potential outcomes. How close does this come to the true mean of 15?

``` r
control_mean <- '[replace with your code]'
```

Randomly sample 2 village and take the average of their potential
outcomes. How close does this come to the true mean of 20?

``` r
treat_mean <- '[replace with your code]'
```

Calculate the difference of these. How close does this come to the true
treatment effect of 5?

Note that because we’ve got pretty small sample sizes here – 5 in
control and 2 in treatment – we might actually not get that close in any
*particular* run of this experiment. But, across all the experiments
that we *could* have conducted, we will be dead on.

As we increase the sample size of our experiments, the amount that these
estimators in *any one* experiment that we run will produce estimates
that are much closer to the true parameter values. We characterize this
through standard errors, which you have covered in previous stats
classes in the program, but which we’ll talk about in detail in week 4.
