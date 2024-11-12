
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
install.packages("fdaSP", dependencies = TRUE)
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

### Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.