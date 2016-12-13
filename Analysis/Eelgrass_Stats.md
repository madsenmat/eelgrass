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
library(forestplot)
```

    ## Warning: package 'forestplot' was built under R version 3.3.2

    ## Loading required package: grid

    ## Loading required package: magrittr

    ## 
    ## Attaching package: 'magrittr'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

    ## Loading required package: checkmate

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

Summarize Data for mean, n and total across the regions for each respective individual and habitat.
===================================================================================================

``` r
SE <- function(x) sd(x)/sqrt(length(x))

(invert_mean <- invert_all_digested %>% 
  select(region, habitat, rel_abun_per, sz_orgnsm, count) %>% 
   group_by(region, sz_orgnsm, habitat) %>% 
   summarise(mean_abun_per = mean(rel_abun_per), SE = SE(rel_abun_per), n= n_distinct(rel_abun_per), total= sum(count)))
```

    ## Source: local data frame [256 x 7]
    ## Groups: region, sz_orgnsm [?]
    ## 
    ##     region        sz_orgnsm habitat mean_abun_per         SE     n
    ##      <chr>            <chr>   <chr>         <dbl>      <dbl> <int>
    ## 1  Bedwell 1_Appendicularia  No_Eel    16.8674699         NA     1
    ## 2  Bedwell       1_Calanoid     Eel     4.0834474  1.7305062     2
    ## 3  Bedwell       1_Calanoid  No_Eel    17.8115760 13.4975520     3
    ## 4  Bedwell     1_Cladoceran     Eel     0.7594682  0.4170024     2
    ## 5  Bedwell     1_Cladoceran  No_Eel     1.2689167  0.2236206     2
    ## 6  Bedwell        1_Cumacea     Eel    11.1128813  7.5156925     4
    ## 7  Bedwell        1_Cumacea  No_Eel    12.8148456  2.8645968     2
    ## 8  Bedwell       1_Gammarid     Eel     4.4612059  1.0048595     3
    ## 9  Bedwell       1_Gammarid  No_Eel     2.7273899  0.1688072     3
    ## 10 Bedwell         1_Harpac     Eel    74.2944605 10.6591816     4
    ## # ... with 246 more rows, and 1 more variables: total <dbl>

Make all NA turn into 0
=======================

``` r
invert_mean[is.na(invert_mean)] <- 0
```

Spread Data Out for Eelgrass and Non Eelgrass
=============================================

``` r
#set up in nice table to ensure doing right thing
  spread_mean <- invert_mean %>% 
  select(region, habitat, mean_abun_per, sz_orgnsm) %>% 
  spread(habitat,mean_abun_per)

spread_mean[is.na(spread_mean)] <- 0

  spread_SE <- invert_mean %>% 
  select(region, habitat, SE, sz_orgnsm) %>% 
  spread(habitat,SE) 
  
spread_SE[is.na(spread_SE)] <- 0

  spread_n <- invert_mean %>% 
  select(region, habitat, n, sz_orgnsm) %>% 
  spread(habitat,n)
  
spread_n[is.na(spread_n)] <- 0
```

Take the difference between Eel and Non Eel, add the n and add the SE.
======================================================================

``` r
  (spread_mean <- spread_mean %>% 
  mutate(Hab_diff = Eel - No_Eel) %>% 
  select(region, sz_orgnsm, Hab_diff))
```

    ## Source: local data frame [170 x 3]
    ## Groups: region, sz_orgnsm [170]
    ## 
    ##     region        sz_orgnsm    Hab_diff
    ##      <chr>            <chr>       <dbl>
    ## 1  Bedwell 1_Appendicularia -16.8674699
    ## 2  Bedwell       1_Calanoid -13.7281287
    ## 3  Bedwell     1_Cladoceran  -0.5094485
    ## 4  Bedwell        1_Cumacea  -1.7019644
    ## 5  Bedwell       1_Gammarid   1.7338161
    ## 6  Bedwell         1_Harpac  16.4549968
    ## 7  Bedwell       1_Ostracod  -2.8022538
    ## 8  Bedwell   1_Phyllodocida   0.4733492
    ## 9  Bedwell      1_Valvifera  -0.4975124
    ## 10 Bedwell      2_Brachyura   0.3424658
    ## # ... with 160 more rows

``` r
  (spread_SE <- spread_SE %>% 
  mutate(SE_Total = Eel + No_Eel) %>% 
  select(region, sz_orgnsm, SE_Total))
```

    ## Source: local data frame [170 x 3]
    ## Groups: region, sz_orgnsm [170]
    ## 
    ##     region        sz_orgnsm  SE_Total
    ##      <chr>            <chr>     <dbl>
    ## 1  Bedwell 1_Appendicularia  0.000000
    ## 2  Bedwell       1_Calanoid 15.228058
    ## 3  Bedwell     1_Cladoceran  0.640623
    ## 4  Bedwell        1_Cumacea 10.380289
    ## 5  Bedwell       1_Gammarid  1.173667
    ## 6  Bedwell         1_Harpac 25.724280
    ## 7  Bedwell       1_Ostracod  0.812204
    ## 8  Bedwell   1_Phyllodocida  2.429262
    ## 9  Bedwell      1_Valvifera  0.000000
    ## 10 Bedwell      2_Brachyura  0.000000
    ## # ... with 160 more rows

``` r
  (spread_ntot <- spread_n %>% 
    mutate(n_total= Eel + No_Eel) %>% 
  select(region, sz_orgnsm, n_total))
```

    ## Source: local data frame [170 x 3]
    ## Groups: region, sz_orgnsm [170]
    ## 
    ##     region        sz_orgnsm n_total
    ##      <chr>            <chr>   <dbl>
    ## 1  Bedwell 1_Appendicularia       1
    ## 2  Bedwell       1_Calanoid       5
    ## 3  Bedwell     1_Cladoceran       4
    ## 4  Bedwell        1_Cumacea       6
    ## 5  Bedwell       1_Gammarid       6
    ## 6  Bedwell         1_Harpac       7
    ## 7  Bedwell       1_Ostracod       2
    ## 8  Bedwell   1_Phyllodocida       7
    ## 9  Bedwell      1_Valvifera       1
    ## 10 Bedwell      2_Brachyura       1
    ## # ... with 160 more rows

``` r
  (joined <- full_join(spread_mean,spread_SE))
```

    ## Joining, by = c("region", "sz_orgnsm")

    ## Source: local data frame [170 x 4]
    ## Groups: region, sz_orgnsm [?]
    ## 
    ##     region        sz_orgnsm    Hab_diff  SE_Total
    ##      <chr>            <chr>       <dbl>     <dbl>
    ## 1  Bedwell 1_Appendicularia -16.8674699  0.000000
    ## 2  Bedwell       1_Calanoid -13.7281287 15.228058
    ## 3  Bedwell     1_Cladoceran  -0.5094485  0.640623
    ## 4  Bedwell        1_Cumacea  -1.7019644 10.380289
    ## 5  Bedwell       1_Gammarid   1.7338161  1.173667
    ## 6  Bedwell         1_Harpac  16.4549968 25.724280
    ## 7  Bedwell       1_Ostracod  -2.8022538  0.812204
    ## 8  Bedwell   1_Phyllodocida   0.4733492  2.429262
    ## 9  Bedwell      1_Valvifera  -0.4975124  0.000000
    ## 10 Bedwell      2_Brachyura   0.3424658  0.000000
    ## # ... with 160 more rows

``` r
joined <- joined %>% 
mutate(upper = Hab_diff + 1.96*SE_Total, lower = Hab_diff - 1.96*SE_Total) %>% 
  select(Hab_diff, upper, lower)
```

    ## Adding missing grouping variables: `region`, `sz_orgnsm`

Fraser, need to write this into a function
==========================================

Now need to split here, is there a better way to do this? Need to figure out.
-----------------------------------------------------------------------------

Picking Region and Padding out
------------------------------

``` r
#filters out Fraser 
joined_fraser <-  joined %>% filter(region == "Fraser")

#Made larger DataFrame to Pad Dada
pad <- (data_frame(region= c("Fraser", "Fraser"), Hab_diff= c(NA, NA)))

#Padded joined with Fraser
padded <- full_join(pad, joined_fraser)
```

    ## Joining, by = c("region", "Hab_diff")

Working to put into proper data table format for text input
-----------------------------------------------------------

``` r
spread_fraser <- spread_n %>% filter(region == "Fraser")

new <- spread_fraser[,c(2,3,4)]

middle <- c("", "(n)", "(n)")

new1 <- rbind(middle, new)

top <- c("SizeFraction_Organism","Eelgrass", "Non-Eelgrass")

frasertext <- rbind(top,new1)
```

Fraser Plot
-----------

``` r
png("forestfraser.png", width=8, height=11, units="in", res=300)
forestfraser <- forestplot(frasertext, padded$Hab_diff, padded$lower, padded$upper, zero= 0, txt_gp= fpTxtGp(cex= 1, xlab = gpar(cex=0.9), ticks = gpar(cex=0.75)), col=fpColors(box="royalblue",line="darkblue"), lineheight = "auto", title = "Fraser", xlab= "Diff. in R.A. of Eel vs. Non Eelgrass (%)", boxsize = 0.2, hrzl_lines = list("3" = gpar(col="#444444")), is.summary=c(TRUE,TRUE,rep(FALSE,30)))

dev.off()
```

    ## quartz_off_screen 
    ##                 2

Tahsis, spot the function
=========================

``` r
#need to into function (x) { all of this stuff, how to return certain parts in a function???, more reading}
joined_tahsis<-  joined %>% filter(region == "Tahsis") #put in x

#Made larger DataFrame to Pad Dada
pad <- (data_frame(region= c("tahsis", "tahsis"), Hab_diff= c(NA, NA))) #put in x 

#Padded joined with Fraser
padded <- full_join(pad, joined_tahsis)
```

    ## Joining, by = c("region", "Hab_diff")

``` r
#spread it out
spread_tahsis <- spread_n %>% filter(region == "Tahsis")

#pulled out needed data, won't break otherwise
new <- spread_tahsis[,c(2,3,4)]

#middle 
middle <- c("", "(n)", "(n)")

#make new 
new1 <- rbind(middle, new)

#add last piece to top
top <- c("SizeFraction_Organism","Eelgrass", "Non-Eelgrass")

#assemble text 
tahsistext <- rbind(top,new1)
```

``` r
png("foresttahsis.png", width=8, height=11, units="in", res=300)

foresttahsis <- forestplot(tahsistext, padded$Hab_diff, padded$lower, padded$upper,
                           zero= 0, 
                           txt_gp= fpTxtGp(cex= 1, xlab = gpar(cex=0.9), ticks = gpar(cex=0.75)), 
                           col=fpColors(box="royalblue",line="darkblue"), 
                           lineheight = "auto",
                           title = "tahsis",
                           xlab= "Diff. in R.A. of Eel vs. Non Eelgrass (%)",
                           boxsize = 0.2,
                           hrzl_lines = list("3" = gpar(col="#444444")), 
                           is.summary=c(TRUE,TRUE,rep(FALSE,(length(padded$Hab_diff)-2))))
  dev.off()
```

    ## quartz_off_screen 
    ##                 2

Qualicum
========

``` r
#need to into function (x) { all of this stuff, how to return certain parts in a function???, more reading}
joined_Qualicum<-  joined %>% filter(region == "Qualicum") #put in x

#Made larger DataFrame to Pad Dada
pad <- (data_frame(region= c("Qualicum", "Qualicum"), Hab_diff= c(NA, NA))) #put in x 

#Padded joined with Fraser
padded <- full_join(pad, joined_Qualicum)
```

    ## Joining, by = c("region", "Hab_diff")

``` r
#spread it out
spread_Qualicum <- spread_n %>% filter(region == "Qualicum")

#pulled out needed data, won't break otherwise
new <- spread_Qualicum[,c(2,3,4)]

#middle 
middle <- c("", "(n)", "(n)")

#make new 
new1 <- rbind(middle, new)

#add last piece to top
top <- c("SizeFraction_Organism","Eel", "Non-Eel")

#assemble text 
Qualicumtext <- rbind(top,new1)
```

``` r
png("forestqualicum.png", width= 15 , height=20, units="in", res=300)

forestQualicum <- forestplot(Qualicumtext, padded$Hab_diff, padded$lower, padded$upper,
                           zero= 0, 
                           txt_gp= fpTxtGp(cex= 1.5, xlab = gpar(cex=2), ticks = gpar(cex=1.5)), 
                           col=fpColors(box="royalblue",line="darkblue"), 
                           lineheight = "auto",
                           title = "Qualicum",
                           xlab= "Diff. in R.A. of Eel vs. Non Eelgrass (%)",
                           boxsize = 0.3,
                           hrzl_lines = list("3" = gpar(col="#444444")), 
                           is.summary=c(TRUE,TRUE,rep(FALSE,(length(padded$Hab_diff)-2))))
  dev.off()
```

    ## quartz_off_screen 
    ##                 2

Koeye
=====

``` r
#need to into function (x) { all of this stuff, how to return certain parts in a function???, more reading}
joined_koeye<-  joined %>% filter(region == "Koeye") #put in x

#Made larger DataFrame to Pad Dada
pad <- (data_frame(region= c("Koeye", "Koeye"), Hab_diff= c(NA, NA))) #put in x 

#Padded joined with Fraser
padded <- full_join(pad, joined_koeye)
```

    ## Joining, by = c("region", "Hab_diff")

``` r
#spread it out
spread_koeye <- spread_n %>% filter(region == "Koeye")

#pulled out needed data, won't break otherwise
new <- spread_koeye[,c(2,3,4)]

#middle 
middle <- c("", "(n)", "(n)")

#make new 
new1 <- rbind(middle, new)

#add last piece to top
top <- c("SizeFraction_Organism","Eel", "Non-Eel")

#assemble text 
koeyetext <- rbind(top,new1)
```

``` r
png("forestkoeye.png", width= 15 , height=20, units="in", res=300)

forestkoeye <- forestplot(koeyetext, padded$Hab_diff, padded$lower, padded$upper,
                           zero= 0, 
                           txt_gp= fpTxtGp(cex= 1.5, xlab = gpar(cex=2), ticks = gpar(cex=1.5)), 
                           col=fpColors(box="royalblue",line="darkblue"), 
                           lineheight = "auto",
                           title = "Koeye",
                           xlab= "Diff. in R.A. of Eel vs. Non Eelgrass (%)",
                           boxsize = 0.3,
                           hrzl_lines = list("3" = gpar(col="#444444")), 
                           is.summary=c(TRUE,TRUE,rep(FALSE,(length(padded$Hab_diff)-2))))
  dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
  (forestkoeye)
```

    ## GRID.VP.31::forestplot_margins
