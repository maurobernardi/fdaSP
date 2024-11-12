
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active)
[![Travis-CI Build
Status](https://travis-ci.org/hrbrmstr/ggalt.svg?branch=master)](https://travis-ci.org/hrbrmstr/ggalt)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/hrbrmstr/ggalt?branch=master&svg=true)](https://ci.appveyor.com/project/hrbrmstr/ggalt)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ggalt)](https://CRAN.R-project.org/package=ggalt)
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/ggalt)

`ggalt` : Extra Coordinate Systems, Geoms, Statistical Transformations,
Scales & Fonts for ‘ggplot2’

A compendium of ‘geoms’, ‘coords’, ‘stats’, scales and fonts for
‘ggplot2’, including splines, 1d and 2d densities, univariate average
shifted histograms, a new map coordinate system based on the
‘PROJ.4’-library and the ‘StateFace’ open source font ‘ProPublica’.

The following functions are implemented:

- `geom_ubar` : Uniform width bar charts

- `geom_horizon` : Horizon charts (modified from
  <https://github.com/AtherEnergy/ggTimeSeries>)

- `coord_proj` : Like `coord_map`, only better (prbly shld use this with
  `geom_cartogram` as `geom_map`’s new defaults are ugh)

- `geom_xspline` : Connect control points/observations with an X-spline

- `stat_xspline` : Connect control points/observations with an X-spline

- `geom_bkde` : Display a smooth density estimate (uses
  `KernSmooth::bkde`)

- `geom_stateface`: Use ProPublica’s StateFace font in ggplot2 plots

- `geom_bkde2d` : Contours from a 2d density estimate. (uses
  `KernSmooth::bkde2D`)

- `stat_bkde` : Display a smooth density estimate (uses
  `KernSmooth::bkde`)

- `stat_bkde2d` : Contours from a 2d density estimate. (uses
  `KernSmooth::bkde2D`)

- `stat_ash` : Compute and display a univariate averaged shifted
  histogram (polynomial kernel) (uses `ash::ash1`/`ash::bin1`)

- `geom_encircle`: Automatically enclose points in a polygon

- `byte_format`: + helpers. e.g. turn `10000` into `10 Kb`

- `geom_lollipop()`: Dead easy lollipops (horizontal or vertical)

- `geom_dumbbell()` : Dead easy dumbbell plots

- `stat_stepribbon()` : Step ribbons

- `annotation_ticks()` : Add minor ticks to identity, exp(1) and exp(10)
  axis scales independently of each other.

- `geom_spikelines()` : Instead of geom_vline and geom_hline a pair of
  segments that originate from same c(x,y) are drawn to the respective
  axes.

- plotly integration for a few of the ^^ geoms

### Installation

``` r
# you'll want to see the vignettes, trust me
install.packages("ggplot2")
install.packages("ggalt")
# OR: devtools::install_github("hrbrmstr/ggalt")
```

### Usage

``` r
library(ggplot2)
library(gridExtra)
library(ggalt)
## Registered S3 methods overwritten by 'ggalt':
##   method                  from   
##   grid.draw.absoluteGrob  ggplot2
##   grobHeight.absoluteGrob ggplot2
##   grobWidth.absoluteGrob  ggplot2
##   grobX.absoluteGrob      ggplot2
##   grobY.absoluteGrob      ggplot2
```

``` r

# current verison
packageVersion("ggalt")
## [1] '0.4.0'
```

``` r

set.seed(1492)
dat <- data.frame(x=c(1:10, 1:10, 1:10),
                  y=c(sample(15:30, 10), 2*sample(15:30, 10), 3*sample(15:30, 10)),
                  group=factor(c(rep(1, 10), rep(2, 10), rep(3, 10)))
)
```

### Horzon Chart

Example carved from:
<https://github.com/halhen/viz-pub/blob/master/sports-time-of-day/2_gen_chart.R>

``` r
library(hrbrthemes)
library(ggalt)
library(tidyverse)

sports <- read_tsv("https://github.com/halhen/viz-pub/raw/master/sports-time-of-day/activity.tsv")

sports %>%
  group_by(activity) %>% 
  filter(max(p) > 3e-04, 
         !grepl('n\\.e\\.c', activity)) %>% 
  arrange(time) %>%
  mutate(p_peak = p / max(p), 
         p_smooth = (lag(p_peak) + p_peak + lead(p_peak)) / 3,
         p_smooth = coalesce(p_smooth, p_peak)) %>% 
  ungroup() %>%
  do({ 
    rbind(.,
          filter(., time == 0) %>%
            mutate(time = 24*60))
  }) %>%
  mutate(time = ifelse(time < 3 * 60, time + 24 * 60, time)) %>%
  mutate(activity = reorder(activity, p_peak, FUN=which.max)) %>% 
  arrange(activity) %>%
  mutate(activity.f = reorder(as.character(activity), desc(activity))) -> sports

sports <- mutate(sports, time2 = time/60)

ggplot(sports, aes(time2, p_smooth)) +
  ggHoriPlot::geom_horizon(bandwidth=0.1) +
  facet_grid(activity.f~.) +
  scale_x_continuous(expand=c(0,0), breaks=seq(from = 3, to = 27, by = 3), labels = function(x) {sprintf("%02d:00", as.integer(x %% 24))}) +
  viridis::scale_fill_viridis(name = "Activity relative to peak", discrete=TRUE,
                              labels=scales::percent(seq(0, 1, 0.1)+0.1)) +
  labs(x=NULL, y=NULL, title="Peak time of day for sports and leisure",
       subtitle="Number of participants throughout the day compared to peak popularity.\nNote the morning-and-evening everyday workouts, the midday hobbies,\nand the evenings/late nights out.") +
  theme_ipsum_rc(grid="") +
  theme(panel.spacing.y=unit(-0.05, "lines")) +
  theme(strip.text.y = element_text(hjust=0, angle=360)) +
  theme(axis.text.y=element_blank())
```

<img src="README_figs/README-horizon-1.png" width="912" />

### Splines!

``` r
ggplot(dat, aes(x, y, group=group, color=group)) +
  geom_point() +
  geom_line()
```

<img src="README_figs/README-splines-1.png" width="672" />

``` r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point() +
  geom_line() +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5)
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="README_figs/README-splines-2.png" width="672" />

``` r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(size=0.5)
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="README_figs/README-splines-3.png" width="672" />

``` r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=-0.4, size=0.5)
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="README_figs/README-splines-4.png" width="672" />

``` r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=0.4, size=0.5)
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="README_figs/README-splines-5.png" width="672" />

``` r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=1, size=0.5)
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="README_figs/README-splines-6.png" width="672" />

``` r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=0, size=0.5)
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="README_figs/README-splines-7.png" width="672" />

``` r

ggplot(dat, aes(x, y, group=group, color=factor(group))) +
  geom_point(color="black") +
  geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
  geom_xspline(spline_shape=-1, size=0.5)
## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="README_figs/README-splines-8.png" width="672" />

#### Alternate (better) density plots

``` r
# bkde

data(geyser, package="MASS")

ggplot(geyser, aes(x=duration)) + 
  stat_bkde(alpha=1/2)
## Bandwidth not specified. Using '0.14', via KernSmooth::dpik.
```

<img src="README_figs/README-bkde_ash-1.png" width="672" />

``` r

ggplot(geyser, aes(x=duration)) +
  geom_bkde(alpha=1/2)
## Bandwidth not specified. Using '0.14', via KernSmooth::dpik.
```

<img src="README_figs/README-bkde_ash-2.png" width="672" />

``` r

ggplot(geyser, aes(x=duration)) + 
  stat_bkde(bandwidth=0.25)
```

<img src="README_figs/README-bkde_ash-3.png" width="672" />

``` r

ggplot(geyser, aes(x=duration)) +
  geom_bkde(bandwidth=0.25)
```

<img src="README_figs/README-bkde_ash-4.png" width="672" />

``` r

set.seed(1492)
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)), 
                   rating = c(rnorm(200),rnorm(200, mean=.8)))

ggplot(dat, aes(x=rating, color=cond)) + geom_bkde(fill="#00000000")
## Bandwidth not specified. Using '0.36', via KernSmooth::dpik.
## Bandwidth not specified. Using '0.31', via KernSmooth::dpik.
```

<img src="README_figs/README-bkde_ash-5.png" width="672" />

``` r

ggplot(dat, aes(x=rating, fill=cond)) + geom_bkde(alpha=0.3)
## Bandwidth not specified. Using '0.36', via KernSmooth::dpik.
## Bandwidth not specified. Using '0.31', via KernSmooth::dpik.
```

<img src="README_figs/README-bkde_ash-6.png" width="672" />

``` r

# ash

set.seed(1492)
dat <- data.frame(x=rnorm(100))
grid.arrange(ggplot(dat, aes(x)) + stat_ash(),
             ggplot(dat, aes(x)) + stat_bkde(),
             ggplot(dat, aes(x)) + stat_density(),
             nrow=3)
## Estimate nonzero outside interval ab.
## Bandwidth not specified. Using '0.43', via KernSmooth::dpik.
```

<img src="README_figs/README-bkde_ash-7.png" width="672" />

``` r

cols <- RColorBrewer::brewer.pal(3, "Dark2")
ggplot(dat, aes(x)) + 
  stat_ash(alpha=1/3, fill=cols[3]) + 
  stat_bkde(alpha=1/3, fill=cols[2]) + 
  stat_density(alpha=1/3, fill=cols[1]) + 
  geom_rug() +
  labs(x=NULL, y="density/estimate") +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(panel.border=element_blank())
## Estimate nonzero outside interval ab.
## Bandwidth not specified. Using '0.43', via KernSmooth::dpik.
```

<img src="README_figs/README-bkde_ash-8.png" width="672" />

### Alternate 2D density plots

``` r
m <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
       geom_point() +
       xlim(0.5, 6) +
       ylim(40, 110)

m + geom_bkde2d(bandwidth=c(0.5, 4))
## Warning: Computation failed in `stat_bkde2d()`.
## Caused by error in `compute_group()`:
## ! argument "z.range" is missing, with no default
```

<img src="README_figs/README-bkde2d-1.png" width="672" />

``` r

m + stat_bkde2d(bandwidth=c(0.5, 4), aes(fill = ..level..), geom = "polygon")
## Warning: The dot-dot notation (`..level..`) was deprecated in ggplot2 3.4.0.
## ℹ Please use `after_stat(level)` instead.
## Computation failed in `stat_bkde2d()`.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
```

<img src="README_figs/README-bkde2d-2.png" width="672" />

### `coord_proj` LIVES! (still needs a teensy bit of work)

### ProPublica StateFace

``` r
# Run show_stateface() to see the location of the TTF StateFace font
# You need to install it for it to work

set.seed(1492)
dat <- data.frame(state=state.abb,
                  x=sample(100, 50),
                  y=sample(100, 50),
                  col=sample(c("#b2182b", "#2166ac"), 50, replace=TRUE),
                  sz=sample(6:15, 50, replace=TRUE),
                  stringsAsFactors=FALSE)
gg <- ggplot(dat, aes(x=x, y=y))
gg <- gg + geom_stateface(aes(label=state, color=col, size=sz))
gg <- gg + scale_color_identity()
gg <- gg + scale_size_identity()
gg
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family
## "StateFace"
```

<img src="README_figs/README-stateface-1.png" width="672" />

### Encircling points automagically

``` r
d <- data.frame(x=c(1,1,2),y=c(1,2,2)*100)

gg <- ggplot(d,aes(x,y))
gg <- gg + scale_x_continuous(expand=c(0.5,1))
gg <- gg + scale_y_continuous(expand=c(0.5,1))

gg + geom_encircle(s_shape=1, expand=0) + geom_point()
```

<img src="README_figs/README-encircle-1.png" width="672" />

``` r

gg + geom_encircle(s_shape=1, expand=0.1, colour="red") + geom_point()
```

<img src="README_figs/README-encircle-2.png" width="672" />

``` r

gg + geom_encircle(s_shape=0.5, expand=0.1, colour="purple") + geom_point()
```

<img src="README_figs/README-encircle-3.png" width="672" />

``` r

gg + geom_encircle(data=subset(d, x==1), colour="blue", spread=0.02) +
  geom_point()
```

<img src="README_figs/README-encircle-4.png" width="672" />

``` r

gg +geom_encircle(data=subset(d, x==2), colour="cyan", spread=0.04) + 
  geom_point()
```

<img src="README_figs/README-encircle-5.png" width="672" />

``` r

gg <- ggplot(mpg, aes(displ, hwy))
gg + geom_encircle(data=subset(mpg, hwy>40)) + geom_point()
```

<img src="README_figs/README-encircle-6.png" width="672" />

``` r

ss <- subset(mpg,hwy>31 & displ<2)

gg + geom_encircle(data=ss, colour="blue", s_shape=0.9, expand=0.07) +
  geom_point() + geom_point(data=ss, colour="blue")
```

<img src="README_figs/README-encircle-7.png" width="672" />

### Step ribbons

``` r
x <- 1:10
df <- data.frame(x=x, y=x+10, ymin=x+7, ymax=x+12)

gg <- ggplot(df, aes(x, y))
gg <- gg + geom_ribbon(aes(ymin=ymin, ymax=ymax),
                      stat="stepribbon", fill="#b2b2b2")
gg <- gg + geom_step(color="#2b2b2b")
gg
```

<img src="README_figs/README-stepribbon-1.png" width="672" />

``` r

gg <- ggplot(df, aes(x, y))
gg <- gg + geom_ribbon(aes(ymin=ymin, ymax=ymax),
                      stat="stepribbon", fill="#b2b2b2",
                      direction="vh")
gg <- gg + geom_step(color="#2b2b2b")
gg
```

<img src="README_figs/README-stepribbon-2.png" width="672" />

### Lollipop charts

``` r
df <- read.csv(text="category,pct
Other,0.09
South Asian/South Asian Americans,0.12
Interngenerational/Generational,0.21
S Asian/Asian Americans,0.25
Muslim Observance,0.29
Africa/Pan Africa/African Americans,0.34
Gender Equity,0.34
Disability Advocacy,0.49
European/European Americans,0.52
Veteran,0.54
Pacific Islander/Pacific Islander Americans,0.59
Non-Traditional Students,0.61
Religious Equity,0.64
Caribbean/Caribbean Americans,0.67
Latino/Latina,0.69
Middle Eastern Heritages and Traditions,0.73
Trans-racial Adoptee/Parent,0.76
LBGTQ/Ally,0.79
Mixed Race,0.80
Jewish Heritage/Observance,0.85
International Students,0.87", stringsAsFactors=FALSE, sep=",", header=TRUE)
 
library(ggplot2)
library(ggalt)
library(scales)
## 
## Attaching package: 'scales'
## The following object is masked from 'package:purrr':
## 
##     discard
## The following object is masked from 'package:readr':
## 
##     col_factor
```

``` r
 
gg <- ggplot(df, aes(y=reorder(category, pct), x=pct))
gg <- gg + geom_lollipop(point.colour="steelblue", point.size=2, horizontal=TRUE)
gg <- gg + scale_x_continuous(expand=c(0,0), labels=percent,
                              breaks=seq(0, 1, by=0.2), limits=c(0, 1))
gg <- gg + labs(x=NULL, y=NULL, 
                title="SUNY Cortland Multicultural Alumni survey results",
                subtitle="Ranked by race, ethnicity, home land and orientation\namong the top areas of concern",
                caption="Data from http://stephanieevergreen.com/lollipop/")
gg <- gg + theme_minimal(base_family="Arial Narrow")
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
## Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
## ℹ Please use the `linewidth` argument instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
```

``` r
gg <- gg + theme(axis.text.y=element_text(margin=margin(r=0, l=0)))
gg <- gg + theme(plot.margin=unit(rep(30, 4), "pt"))
gg <- gg + theme(plot.title=element_text(face="bold"))
gg <- gg + theme(plot.subtitle=element_text(margin=margin(b=10)))
gg <- gg + theme(plot.caption=element_text(size=8, margin=margin(t=10)))
gg
## Warning: Using the `size` aesthetic with geom_segment was deprecated in ggplot2 3.4.0.
## ℹ Please use the `linewidth` aesthetic instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
```

<img src="README_figs/README-lollipop-1.png" width="672" />

``` r
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(ggalt) # devtools::install_github("hrbrmstr/ggalt")

health <- read.csv("https://rud.is/dl/zhealth.csv", stringsAsFactors=FALSE, 
                   header=FALSE, col.names=c("pct", "area_id"))

areas <- read.csv("https://rud.is/dl/zarea_trans.csv", stringsAsFactors=FALSE, header=TRUE)

health %>% 
  mutate(area_id=trunc(area_id)) %>% 
  arrange(area_id, pct) %>% 
  mutate(year=rep(c("2014", "2013"), 26),
         pct=pct/100) %>% 
  left_join(areas, "area_id") %>% 
  mutate(area_name=factor(area_name, levels=unique(area_name))) -> health

setNames(bind_cols(filter(health, year==2014), filter(health, year==2013))[,c(4,1,5)],
         c("area_name", "pct_2014", "pct_2013")) -> health

gg <- ggplot(health, aes(x=pct_2014, xend=pct_2013, y=area_name, group=area_name))
gg <- gg + geom_dumbbell(colour="#a3c4dc", size=1.5, colour_xend="#0e668b", 
                         dot_guide=TRUE, dot_guide_size=0.15)
gg <- gg + scale_x_continuous(label=percent)
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + theme_bw()
gg <- gg + theme(plot.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.major.x=element_line())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(legend.position="top")
gg <- gg + theme(panel.border=element_blank())
gg
```

<img src="README_figs/README-dumbbell-1.png" width="672" />

``` r
library(hrbrthemes)

df <- data.frame(trt=LETTERS[1:5], l=c(20, 40, 10, 30, 50), r=c(70, 50, 30, 60, 80))

ggplot(df, aes(y=trt, x=l, xend=r)) + 
  geom_dumbbell(size=3, color="#e3e2e1", 
                colour_x = "#5b8124", colour_xend = "#bad744",
                dot_guide=TRUE, dot_guide_size=0.25) +
  labs(x=NULL, y=NULL, title="ggplot2 geom_dumbbell with dot guide") +
  theme_ipsum_rc(grid="X") +
  theme(panel.grid.major.x=element_line(size=0.05))
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : Unable to calculate text width/height (using
## zero)
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : Unable to calculate text width/height (using
## zero)
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : Unable to calculate text width/height (using
## zero)
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : Unable to calculate text width/height (using
## zero)
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : Unable to calculate text width/height (using
## zero)
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : Unable to calculate text width/height (using
## zero)
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : Unable to calculate text width/height (using
## zero)
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : Unable to calculate text width/height (using
## zero)
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : Unable to calculate text width/height (using
## zero)
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : Unable to calculate text width/height (using
## zero)
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, : no font could be found for family "Roboto
## Condensed"
```

<img src="README_figs/README-dumbbell2-1.png" width="672" />

### Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.
