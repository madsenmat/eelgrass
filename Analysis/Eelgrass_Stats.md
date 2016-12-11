Eelgrass\_Stats
================
Matt Madsen
December 11, 2016

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.3.2

``` r
library(tidyverse)
```

    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(viridis)
library(data.table)
```

    ## -------------------------------------------------------------------------

    ## data.table + dplyr code now lives in dtplyr.
    ## Please library(dtplyr)!

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, mday, month, quarter, wday, week, yday, year

    ## The following objects are masked from 'package:reshape2':
    ## 
    ##     dcast, melt

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

``` r
library(forcats)
library(RColorBrewer)
```

Load all Data, pull out information we want.
============================================

``` r
invert_all <- read_csv("CountNormalizedEelgrassCompiledDateChange.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   region = col_character(),
    ##   date = col_character(),
    ##   set = col_double(),
    ##   location = col_character(),
    ##   eel = col_integer(),
    ##   Paired = col_integer(),
    ##   comp = col_integer(),
    ##   date_proc = col_character(),
    ##   proc = col_character(),
    ##   `1_frac_corr` = col_double(),
    ##   sz_frac = col_integer(),
    ##   orgnsm = col_character(),
    ##   count = col_double(),
    ##   rel_abun = col_double(),
    ##   Sum = col_double(),
    ##   act_count = col_character()
    ## )

``` r
invert_all$date <- as.Date(invert_all$date, format = "%d-%h-%y")
invert_all_comp <- invert_all %>%
     filter(comp == 1)

invert_all_comp$eel <- as.numeric(invert_all_comp$eel)

invert_all_comp <- invert_all_comp %>% 
  mutate(rel_abun_per = rel_abun*100) 

#change Eelgrass

invert_all_comp <- invert_all_comp %>% 
  mutate(eel = ifelse(eel == 1, "Eel", eel))
invert_all_comp <- invert_all_comp %>% 
  mutate(eel = ifelse(eel == 0, "No_Eel", eel))

#join sz and orgnsm
invert_all_comp <- invert_all_comp %>% 
  unite("sz_orgnsm", sz_frac, orgnsm, sep = "_")

#Renames Column from Eel to Habitat
invert_all_comp <- rename(invert_all_comp, habitat = eel)

invert_all_digested <- invert_all_comp %>% 
  select(region, date, set, habitat, Paired, sz_orgnsm, count, rel_abun, rel_abun_per)


write_csv(invert_all_digested, "eelgrass_digested.csv")
```

Summarize Data for mean across the regions.
===========================================

``` r
(invert_mean <- invert_all_digested %>% 
  select(region, habitat, rel_abun_per, sz_orgnsm) %>% 
   group_by(region, habitat, sz_orgnsm) %>% 
   summarise(mean_abun_per = mean(rel_abun_per), n= n_distinct(rel_abun_per)))
```

    ## Source: local data frame [256 x 5]
    ## Groups: region, habitat [?]
    ## 
    ##     region habitat      sz_orgnsm mean_abun_per     n
    ##      <chr>   <chr>          <chr>         <dbl> <int>
    ## 1  Bedwell     Eel     1_Calanoid     4.0834474     2
    ## 2  Bedwell     Eel   1_Cladoceran     0.7594682     2
    ## 3  Bedwell     Eel      1_Cumacea    11.1128813     4
    ## 4  Bedwell     Eel     1_Gammarid     4.4612059     3
    ## 5  Bedwell     Eel       1_Harpac    74.2944605     4
    ## 6  Bedwell     Eel 1_Phyllodocida     4.2495161     4
    ## 7  Bedwell     Eel    2_Brachyura     0.3424658     1
    ## 8  Bedwell     Eel      2_Cumacea     2.9182690     4
    ## 9  Bedwell     Eel     2_Gammarid     0.9579528     3
    ## 10 Bedwell     Eel       2_Harpac     0.5386002     1
    ## # ... with 246 more rows
