PS 0: Sample Work
================
Alex, Scott & Mmicah
12/21/2020

There’s not much for you to do here. Just knit this work, commit all the
files (included those that are created from the plotting) and push back
to your repo.

Then, when you’re done adding/committing/pushing to your repo, you can
drop the repo link into Gradescope to turn this work in.

``` r
library(data.table) 
library(magrittr)

library(ggplot2)
library(patchwork)
```

``` r
d <- data.table(
  x = rnorm(1000), 
  y = rnorm(1000)
)
```

# This is a heading\!

``` r
hist_x <- d %>% 
  ggplot(aes(x = x)) + 
  geom_histogram(bins = 30) + 
  labs(
    title = 'Quite informative Plot of X!', 
    x = 'Some X variable'
  ) + 
  theme_minimal()

hist_y <- d %>% 
  ggplot(aes(x = y)) + # 'you aaaare freaking out, man'
  geom_histogram(bins = 30) + 
  labs(
    title = 'Quite informative Plot of Y!', 
    x = 'Some Y variable'
  ) + 
  theme_minimal()

scatter_xy <- d %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point() + 
  theme_bw()
```

``` r
(hist_x | hist_y) /
scatter_xy
```

![](ps0_files/figure-gfm/render%20plots-1.png)<!-- -->
