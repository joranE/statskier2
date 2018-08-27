
<!-- README.md is generated from README.Rmd. Please edit that file -->
statskier2
==========

This package is a collection of functions I use to explore and analyze cross- country skiing results data that I have collected over the years. The data itself is not included in the package, so the this package isn't actually functional on its own.

Examples
--------

Some basic plots of results by skier:

``` r
library(statskier2)
p <- ath_plot_dst(ath_names = c("WENG Heidi","SOMMERFELDT Rene","BJOERGEN Marit"))
print(p$plot)
```

<img src="man/figures/README-ath-plots-1.png" width="100%" />

``` r

p <- ath_plot_spr(ath_name = c("NEWELL Andrew","RANDALL Kikkan"))
print(p$plot)
```

<img src="man/figures/README-ath-plots-2.png" width="100%" />

``` r

p <- spr_plot(ath_names = c('NEWELL Andrew','HAMILTON Simeon'))
print(p$plot)
```

<img src="man/figures/README-ath-plots-3.png" width="100%" />

Some head-to-head plots:

``` r
p <- hth_race(ath_name = 'DIGGINS Jessica',race_id = 9232,num_opp = 30,min_encounters = 3,measure = 'pb')
print(p$plot)
```

<img src="man/figures/README-ath-hth-1.png" width="100%" />

``` r

p <- hth_lmer(ath_names = 'DIGGINS Jessica',race_id = 9232,num_opp = 30,min_encounters = 3,measure = 'pb')
print(p$plot)
```

<img src="man/figures/README-ath-hth-2.png" width="100%" />

Results trends by nation:

``` r
p <- nation_trend(nations = c('USA','CAN','RUS','SWE'),
                  race_gender = 'Men',
                  race_type = 'Distance')
print(p$plot)
```

<img src="man/figures/README-nation-trend-1.png" width="100%" />

Individual race snapshots:

``` r
p <- race_snapshot_dst(race_id = 7902,title = "Men's 15k Classic - 2014-11-30")
print(p$plot)
```

<img src="man/figures/README-race-snapshot-1.png" width="100%" />

World Junior & U23 results over time by nation:

``` r
p <- wjc_u23_plot(nations = c('USA','CAN'),races = 'WJC')
print(p$plots$USA)
```

<img src="man/figures/README-wjc-u23-1.png" width="100%" />

``` r
print(p$plots$CAN)
```

<img src="man/figures/README-wjc-u23-2.png" width="100%" />
