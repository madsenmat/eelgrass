Eelgrass Graphs
================
Matt Madsen
October 29, 2016

Things to Do

1.  All Sites Abundance by Eel and Non Eel
2.  All Sites by Size Fraction (maybe do an add on to the above graphs, like weighted mean with geom\_smooth, 2nd axis)
3.  Josie iNext package by region, by date, over all regions
4.  Vegan to do nMDS with bray curtis -&gt; default in vegan
5.  Cluster want to transform grouping with log transtorm data abundances

Transfer all to RA.

Species Richness

Within and between

Link to Diet Work

Histogram with what you get and what are in diet.

Historgram numbers by size.

Separate by size class

top column is species

row is

Load Packages

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

Load all Data
=============

``` r
invert_all <- read_csv("eelgrasscompileddatechange.csv")
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
    ##   Sum = col_double()
    ## )

``` r
invert_all$date <- as.Date(invert_all$date, format = "%d-%h-%y")
invert_all_comp <- invert_all %>%
     filter(comp == 1)

invert_all_comp$eel <- as.numeric(invert_all_comp$eel)

invert_all_comp <- invert_all_comp %>% 
  mutate(rel_abun_per = rel_abun*100) 

#non working code 

invert_all_comp %>% 
  group_by(region, date, orgnsm, set) %>% 
  mutate(rel_abun_new = count/sum(count)) %>% 
  select(rel_abun_new, rel_abun, date, orgnsm,count) %>% 
  print(n=20)
```

    ## Adding missing grouping variables: `region`, `set`

    ## Source: local data frame [896 x 7]
    ## Groups: region, date, orgnsm, set [478]
    ## 
    ##    region   set rel_abun_new    rel_abun       date         orgnsm
    ##     <chr> <dbl>        <dbl>       <dbl>     <date>          <chr>
    ## 1  Fraser   1.1   0.38223938 0.070063694 2016-07-12     Cladoceran
    ## 2  Fraser   1.1   1.00000000 0.076433121 2016-07-12   Phyllodocida
    ## 3  Fraser   1.1   0.13043478 0.012738854 2016-07-12 Appendicularia
    ## 4  Fraser   1.1   0.16998012 0.484076433 2016-07-12         Harpac
    ## 5  Fraser   1.1   0.37163375 0.292993631 2016-07-12       Calanoid
    ## 6  Fraser   1.1   0.31034483 0.012738854 2016-07-12      Brachyura
    ## 7  Fraser   1.1   0.28571429 0.050955414 2016-07-12       Barnacle
    ## 8  Fraser   2.1   1.00000000 0.007905138 2016-07-12       Gammarid
    ## 9  Fraser   2.1   0.25000000 0.007905138 2016-07-12   Phyllodocida
    ## 10 Fraser   2.1   0.84210526 0.015810277 2016-07-12 Appendicularia
    ## 11 Fraser   2.1   0.74285714 0.154150198 2016-07-12         Harpac
    ## 12 Fraser   2.1   0.94144661 0.810276680 2016-07-12       Calanoid
    ## 13 Fraser   2.1   1.00000000 0.003952569 2016-07-12         Mysida
    ## 14 Fraser   2.1   1.00000000 0.078431373 2016-07-12     Cladoceran
    ## 15 Fraser   2.1   0.75000000 0.156862745 2016-07-12   Phyllodocida
    ## 16 Fraser   2.1   0.15789474 0.019607843 2016-07-12 Appendicularia
    ## 17 Fraser   2.1   0.25714286 0.352941176 2016-07-12         Harpac
    ## 18 Fraser   2.1   0.05855339 0.333333333 2016-07-12       Calanoid
    ## 19 Fraser   2.1   1.00000000 0.058823529 2016-07-12       Barnacle
    ## 20 Fraser   1.1   1.00000000 0.009049774 2016-07-12       Gammarid
    ## # ... with 876 more rows, and 1 more variables: count <dbl>

Put into respective regions for plots
=====================================

``` r
(invert_all_comp_reg <- invert_all_comp %>% 
  mutate(eel = ifelse(eel == 1, "Eelgrass", eel)))
```

    ## # A tibble: 896 × 16
    ##    region       date   set          location      eel Paired  comp
    ##     <chr>     <date> <dbl>             <chr>    <chr>  <int> <int>
    ## 1  Fraser 2016-07-12   1.1     MRB SF FR 016        0     NA     1
    ## 2  Fraser 2016-07-12   1.1     MRB SF FR 016        0     NA     1
    ## 3  Fraser 2016-07-12   1.1     MRB SF FR 016        0     NA     1
    ## 4  Fraser 2016-07-12   1.1     MRB SF FR 016        0     NA     1
    ## 5  Fraser 2016-07-12   1.1     MRB SF FR 016        0     NA     1
    ## 6  Fraser 2016-07-12   1.1     MRB SF FR 016        0     NA     1
    ## 7  Fraser 2016-07-12   1.1     MRB SF FR 016        0     NA     1
    ## 8  Fraser 2016-07-12   2.1 MRB FR 017B (NEW) Eelgrass     NA     1
    ## 9  Fraser 2016-07-12   2.1 MRB FR 017B (NEW) Eelgrass     NA     1
    ## 10 Fraser 2016-07-12   2.1 MRB FR 017B (NEW) Eelgrass     NA     1
    ## # ... with 886 more rows, and 9 more variables: date_proc <chr>,
    ## #   proc <chr>, `1_frac_corr` <dbl>, sz_frac <int>, orgnsm <chr>,
    ## #   count <dbl>, rel_abun <dbl>, Sum <dbl>, rel_abun_per <dbl>

``` r
invert_all_comp_reg <- invert_all_comp_reg %>% 
  mutate(eel = ifelse(eel == 0, "NonEelgrass", eel))

tahsis <- invert_all_comp %>% 
  filter(region == "Tahsis")
qual <- invert_all_comp %>% 
  filter(region == "Qualicum")
fraser <- invert_all_comp %>% 
  filter(region == "Fraser")
bed <- invert_all_comp %>% 
  filter(region == "Bedwell")
koeye <- invert_all_comp %>% 
  filter(region == "Koeye")

#groups together replicates

tahsis1 <- tahsis %>% group_by(date, orgnsm, eel) %>% 
  summarise(rel_abun_per= mean(rel_abun_per))

qual1 <- qual %>% group_by(date, orgnsm, eel) %>% 
  summarise(rel_abun_per= mean(rel_abun_per))

fraser1 <- fraser %>% group_by(date, orgnsm, eel) %>% 
  summarise(rel_abun_per= mean(rel_abun_per))

bed1 <- bed %>%  group_by(date, orgnsm, eel) %>% 
  summarise(rel_abun_per= mean(rel_abun_per))

koeye1 <- koeye %>% group_by(date, orgnsm, eel) %>% 
  summarise(rel_abun_per= mean(rel_abun_per))


#arranges to have top to bottom 

tahsis1 <- tahsis1 %>% 
  select (eel, rel_abun_per, orgnsm, date) %>% 
  arrange(desc(rel_abun_per)) 


qual1 <- qual1 %>% 
  select (eel, rel_abun_per, orgnsm, date) %>% 
  arrange(desc(rel_abun_per))

bed1 <- bed1 %>% 
  select (eel, rel_abun_per, orgnsm, date) %>% 
  arrange(desc(rel_abun_per))

fraser1 <- fraser1 %>% 
  select (eel, rel_abun_per, orgnsm, date) %>% 
  arrange(desc(rel_abun_per))

koeye1 <- koeye1 %>% 
  select (eel, rel_abun_per, orgnsm, date) %>% 
  arrange(desc(rel_abun_per))
```

Make colors for all plots

``` r
coul <- brewer.pal(9, "Set1")
coul <- colorRampPalette(coul)(40)

names <- unique(invert_all_comp$orgnsm)           

names(coul) <- levels(invert_all_comp$orgnsm)

group.colors <- c(Cladoceran = "#E41A1C", Phyllodocida = "#C02E3C", Appendicularia = "#9D435C", Harpac =  "#79577C",  Calanoid = "#566C9C", Brachyura = "#377FB5", Barnacle = "#3C899E", Gammarid = "#409388", Mysida = "#459D71", Cumacea = "#49A75A", Cyclopoid = "#50AA4E", Cnetophora = "#609660", Echinoderm = "#6F8273", Gastropod = "#7E6E85", Valvifera ="#8E5A97", UK4 = "#9F5196", Natantia = "#B55B75", Ostracod = "#CA6553", UK16 = "#DF6F32", UK21 = "#F47910", UK9 = "#FF8C05", UK10 = "#FFA60F", UK13 = "#FFC01A", Caprellidea = "#FFDA24", Fish_Larvae = "#FFF52F", UK11 = "#F3E931", UK12 = "#AC94A0", UK7 = "#E1C62F", Cardidae = "#CFA42D", UK19 = "#BC812A", Bivalvia = "#AA5E28", Pleuronecti = "#B25C3F", UK15 = "#C3655E", UK8 =  "#D36E7D", Euphausiid = "#E4779C", Hyperiidea = "#F47FBB", UK25 = "#E685B8", UK24 = "#D28AB0", UK27 = "#BF8FA8")
```

Plot of Non Eelgrass and Eelgrass with Different Organisms
----------------------------------------------------------

Plot Tahsis

``` r
cols <- c("white", "black") 
colors <- 
numlabs <- c('No Eel', 'Eel')
numcols <- as.numeric(n_distinct(tahsis$orgnsm))
ncols = as.numeric(n_distinct(tahsis$date))


p <- ggplot(tahsis1, aes(x= eel, y= (rel_abun_per), fill= fct_reorder(orgnsm, rel_abun_per, .desc= TRUE), color = factor(eel))) +
   geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap("date", ncol = ncols) +
     scale_color_manual(values = cols, labels = c("Non Eelgrass", "Eelgrass"), guide = FALSE)  +
    scale_fill_manual(values = group.colors) +
   scale_x_continuous(breaks= c(0,1), labels= numlabs) +
  scale_y_sqrt()+
 theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 10, colour = "black", face = "bold"),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.key = element_rect(fill = "black")) + 
  labs(x = NULL, y = "Relative Abundance (%)") + 
  ggtitle("Tahsis") 

p
```

![](eelgrass1_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
ggsave("Eelgrasstahsis.png", p)
```

    ## Saving 7 x 5 in image

Fraser Plot

``` r
numcols <- as.numeric(n_distinct(fraser1$orgnsm))
ncols = as.numeric(n_distinct(fraser1$date))


p <- ggplot(fraser1, aes(x= eel, y= (rel_abun_per), fill= fct_reorder(orgnsm, rel_abun_per, .desc= TRUE), color = factor(eel))) +
   geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap("date", ncol = 4) +
     scale_color_manual(values = cols, labels = c("Non Eelgrass", "Eelgrass"), guide = FALSE)  +
    scale_fill_manual(values = group.colors) +
   scale_x_continuous(breaks= c(0,1), labels= numlabs) +
  scale_y_sqrt()+
 theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        strip.text.x = element_text(size = 10, colour = "black", face = "bold"),
        plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.key = element_rect(fill = "black")) + 
  labs(x = NULL, y = "Relative Abundance (%)") + 
  ggtitle("Fraser") 

p
```

![](eelgrass1_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
ggsave("EelgrassFraser.png", p)
```

    ## Saving 7 x 5 in image

Qualicum Plot

``` r
numcols <- as.numeric(n_distinct(qual1$orgnsm))
ncols = as.numeric(n_distinct(qual1$date))


p <- ggplot(qual1, aes(x= eel, y= (rel_abun_per), fill= fct_reorder(orgnsm, rel_abun_per, .desc= TRUE), color = factor(eel))) +
   geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap("date", ncol = 4) +
     scale_color_manual(values = cols, labels = c("Non Eelgrass", "Eelgrass"), guide = FALSE)  +
    scale_fill_manual(values = group.colors) +
   scale_x_continuous(breaks= c(0,1), labels= numlabs) +
  scale_y_sqrt()+
 theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        strip.text.x = element_text(size = 10, colour = "black", face = "bold"),
        plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.key = element_rect(fill = "black")) + 
  labs(x = NULL, y = "Relative Abundance (%)") + 
  ggtitle("Qualicum") 

p
```

![](eelgrass1_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
ggsave("EelgrassQual.png", p)
```

    ## Saving 7 x 5 in image

Bedwell Plots

``` r
numcols <- as.numeric(n_distinct(bed1$orgnsm))
ncols = as.numeric(n_distinct(bed1$date))


p <- ggplot(bed1, aes(x= eel, y= (rel_abun_per), fill= fct_reorder(orgnsm, rel_abun_per, .desc= TRUE), color = factor(eel))) +
   geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap("date", ncol = ncols) +
   scale_color_manual(values = cols, labels = c("Non Eelgrass", "Eelgrass"), guide = FALSE) +
    scale_fill_manual(values = group.colors) +
   scale_x_continuous(breaks= c(0,1), labels= numlabs) +
  scale_y_sqrt()+
 theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        strip.text.x = element_text(size = 10, colour = "black", face = "bold"),
        plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.key = element_rect(fill = "black")) + 
  labs(x = NULL, y = "Relative Abundance (%)") + 
  ggtitle("Bedwell") 

p
```

![](eelgrass1_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
ggsave("EelgrassBedwell.png", p)
```

    ## Saving 7 x 5 in image

Koeye Plot

``` r
numcols <- as.numeric(n_distinct(koeye1$orgnsm))
ncols = as.numeric(n_distinct(koeye1$date))


p <- ggplot(koeye1, aes(x= eel, y= (rel_abun_per), fill= fct_reorder(orgnsm, rel_abun_per, .desc= TRUE), color = factor(eel))) +
   geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap("date", ncol = ncols) +
    scale_color_manual(values = cols, labels = c("Non Eelgrass", "Eelgrass"), guide = FALSE)  +
    scale_fill_manual(values = group.colors) +
   scale_x_continuous(breaks= c(0,1), labels= numlabs) +
  scale_y_sqrt()+
 theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        strip.text.x = element_text(size = 10, colour = "black", face = "bold"),
        plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.key = element_rect(fill = "black")) + 
  labs(x = NULL, y = "Relative Abundance (%)") + 
  ggtitle("Koeye") 

p
```

![](eelgrass1_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
ggsave("Eelgrasskoeye.png", p)
```

    ## Saving 7 x 5 in image

Salmon Plots
============

``` r
salmon <- invert_all_comp %>% 
  filter(Paired == 1)

salmon.fras <- salmon %>% 
  filter(region == "Fraser")

salmon.qual <- salmon %>% 
  filter(region == "Qualicum")

salmon.koeye <- salmon %>% 
  filter(region == "Koeye")
```

Seeting Up Fraser Data
======================

``` r
#readjust to allow only three groupings 1, 2, greater than 4 


salmon.fras$sz_frac <- as.numeric(salmon.fras$sz_frac)

salmon.fras1 <- salmon.fras %>% 
  select(date, orgnsm, eel, count, sz_frac) %>% 
  group_by(date, eel, orgnsm, sz_frac) %>% 
  summarise(mean_count= mean(count))

salmon.fras1eel <- salmon.fras1 %>% 
  filter(eel == 1)

salmon.fras1noeel <- salmon.fras1 %>% 
  filter(eel != 1)


salmon.fras1eel$sz_frac <- as.character(salmon.fras1eel$sz_frac)
salmon.fras1noeel$sz_frac <- as.character(salmon.fras1noeel$sz_frac)

 salmon.fras1eel<- salmon.fras1eel %>% 
  select (eel, mean_count, orgnsm, date, sz_frac) %>% 
  arrange(desc(mean_count)) 
 
  salmon.fras1noeel<- salmon.fras1noeel %>% 
  select (eel, mean_count, orgnsm, date, sz_frac) %>% 
  arrange(desc(mean_count)) 
```

#### Plot for Fraser Salmon Eelgrass

``` r
p1 <- ggplot(salmon.fras1eel, aes(x= sz_frac, y = (mean_count), fill = fct_reorder(orgnsm, mean_count, .desc= TRUE))) +
   geom_bar(stat = "identity", position = 'stack') + 
  facet_wrap("date") +
  theme(axis.text.x=element_text(angle= 300, vjust = 0.75, hjust= 0.2))+
              theme_bw() +
  scale_y_sqrt()+
   scale_fill_manual(values = group.colors)+
  scale_x_discrete(labels= c("<1", "1-4")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        strip.text.x = element_text(size = 10, colour = "black", face = "bold"),
        plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.key = element_rect(fill = "black")) + 
  ggtitle("Fraser Salmon Eelgrass") +
  labs(x = "Size Fraction (mm)", y = "Abundance")

ggsave("frasersalmoneelgrass.png")
```

    ## Saving 7 x 5 in image

``` r
p1
```

![](eelgrass1_files/figure-markdown_github/unnamed-chunk-12-1.png)

### Fraser Salmon Non Eelgrass

``` r
p1 <- ggplot(salmon.fras1noeel, aes(x= sz_frac, y = mean_count, fill = fct_reorder(orgnsm, mean_count, .desc= TRUE)))+
   geom_bar(stat = "identity", position = 'stack') + 
  facet_wrap("date") +
  theme(axis.text.x=element_text(angle= 300, vjust = 0.75, hjust= 0.2))+
              theme_bw() +
  scale_y_sqrt()+
   scale_fill_manual(values = group.colors)+
  scale_x_discrete(labels= c("<1", "1-4", "10")) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        strip.text.x = element_text(size = 10, colour = "black", face = "bold"),
        plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.position = "top", 
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.key = element_rect(fill = "black")) + 
  ggtitle("Fraser Salmon No Eelgrass ") +
  labs(x = "Size Fraction (mm)", y = "Abundance")

ggsave("frasersalmonnoeelgrass.png")
```

    ## Saving 7 x 5 in image

``` r
p1
```

![](eelgrass1_files/figure-markdown_github/unnamed-chunk-13-1.png)

### Qualicum Salmon Plots

##### Setting up Data again

``` r
salmon.qual$sz_frac <- as.numeric(salmon.qual$sz_frac)

salmon.qual1 <- salmon.qual %>% 
  select(date, orgnsm, eel, count, sz_frac) %>% 
  group_by(date, eel, orgnsm, sz_frac) %>% 
  summarise(mean_count= mean(count))

salmon.qual1eel <- salmon.qual1 %>% 
  filter(eel == 1) %>% arrange(sz_frac)

salmon.qual1noeel <- salmon.qual1 %>% 
  filter(eel != 1)

salmon.qual1eel<- salmon.qual1eel %>% 
  select (eel, mean_count, orgnsm, date, sz_frac) %>% 
  arrange(desc(mean_count)) 
 
  salmon.qual1noeel<- salmon.qual1noeel %>% 
  select (eel, mean_count, orgnsm, date, sz_frac) %>% 
  arrange(desc(mean_count)) 
#salmon.qual1eel$sz_frac <- as.character(salmon.qual1eel$sz_frac)
#salmon.qual1noeel$sz_frac <- as.character(salmon.qual1noeel$sz_frac)
```

#### Plot for Qualicum Salmon Eelgrass

``` r
p1 <- ggplot(salmon.qual1eel, aes(x=factor(sz_frac), y = mean_count, fill = fct_reorder(orgnsm, mean_count, .desc= TRUE))) +
   geom_bar(stat = "identity", position = 'stack') + 
  facet_wrap("date") +
              theme_bw() +
  scale_y_sqrt()+
   scale_fill_manual(values = group.colors)+
  scale_x_discrete(labels= c("<1", "1-4", "4-8", "8", "8-12", "12-16", "16-20", "20-24", "24-28"))+
 theme()+
  theme(panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 10, colour = "black", face = "bold"),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        axis.text.x=element_text(angle= 300, vjust = 0.75, hjust= 0.2),
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.key = element_rect(fill = "black")) +
  ggtitle("Qualicum Salmon Eelgrass") +
  labs(x = "Size Fraction (mm)", y = "Abundance")

ggsave("QualicumSalmonEelgrass.png")
```

    ## Saving 7 x 5 in image

``` r
p1
```

![](eelgrass1_files/figure-markdown_github/unnamed-chunk-15-1.png)

### Qualicum Salmon Non Eelgrass

``` r
p1 <- ggplot(salmon.qual1noeel, aes(x=factor(sz_frac), y = mean_count, fill = fct_reorder(orgnsm, mean_count, .desc= TRUE))) +
   geom_bar(stat = "identity", position = 'stack') + 
  facet_wrap("date") +
              theme_bw() +
  scale_y_sqrt()+
   scale_fill_manual(values = group.colors) +
  scale_x_discrete(labels= c("<1", "1-4", "4-8", "8-12", "12-16", "20-24", "28-32"))+
 theme()+
  theme(panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = 10, colour = "black", face = "bold"),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        axis.text.x=element_text(angle= 300, vjust = 0.75, hjust= 0.2),
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.key = element_rect(fill = "black")) +
  ggtitle("Qualicum Salmon No Eelgrass") +
  labs(x = "Size Fraction (mm)", y = "Abundance")

ggsave("QualicumSalmonNoEelgrass.png")
```

    ## Saving 7 x 5 in image

``` r
p1
```

![](eelgrass1_files/figure-markdown_github/unnamed-chunk-16-1.png)

### Koeye Salmon Eelgrass

##### Setting up Data again

``` r
salmon.koeye$sz_frac <- as.numeric(salmon.koeye$sz_frac)

salmon.koeye1 <- salmon.koeye %>% 
  select(date, orgnsm, eel, count, sz_frac) %>% 
  group_by(date, eel, orgnsm, sz_frac) %>% 
  summarise(mean_count= mean(count))

salmon.koeye1eel <- salmon.koeye1 %>% 
  filter(eel == 1) %>% arrange(sz_frac)

salmon.koeye1noeel <- salmon.koeye1 %>% 
  filter(eel != 1)

salmon.koeye1eel<- salmon.koeye1eel %>% 
  select (eel, mean_count, orgnsm, date, sz_frac) %>% 
  arrange(desc(mean_count)) 
 
  salmon.koeye1noeel<- salmon.koeye1noeel %>% 
  select (eel, mean_count, orgnsm, date, sz_frac) %>% 
  arrange(desc(mean_count)) 
#salmon.qual1eel$sz_frac <- as.character(salmon.qual1eel$sz_frac)
#salmon.qual1noeel$sz_frac <- as.character(salmon.qual1noeel$sz_frac)
```

### Eelgrass

Koeye Eelgrass
==============

``` r
p1 <- ggplot(salmon.koeye1eel, aes(x=factor(sz_frac), y = mean_count, fill = fct_reorder(orgnsm, mean_count, .desc= TRUE))) +
   geom_bar(stat = "identity", position = 'stack') + 
  facet_wrap("date") +
              theme_bw() +
   scale_fill_manual(values = group.colors)+
  scale_x_discrete(labels= c("<1", "1-4", "4-8", "12-16"))+
 theme()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        strip.text.x = element_text(size = 10, colour = "black", face = "bold"),
        plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        axis.text.x=element_text(angle= 300, vjust = 0.75, hjust= 0.2),
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.key = element_rect(fill = "black")) +
  ggtitle("Koeye Salmon Eelgrass") +
  labs(x = "Size Fraction (mm)", y = "Abundance")

ggsave("KoeyeSalmonEelgrass.png")
```

    ## Saving 7 x 5 in image

``` r
p1
```

![](eelgrass1_files/figure-markdown_github/unnamed-chunk-18-1.png)

No Eelgrass Koeye
-----------------

``` r
p1 <- ggplot(salmon.koeye1noeel, aes(x=factor(sz_frac), y = mean_count, fill = fct_reorder(orgnsm, mean_count, .desc= TRUE))) +
   geom_bar(stat = "identity", position = 'Stack') + 
  facet_wrap("date") +
              theme_bw() +
  scale_y_sqrt()+
   scale_fill_manual(values = group.colors) +
  scale_x_discrete(labels= c("<1", "1-4", "8-12", "20-24", "28-32"))+
 theme()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        strip.text.x = element_text(size = 10, colour = "black", face = "bold"),
        plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        axis.text.x=element_text(angle= 300, vjust = 0.75, hjust= 0.2),
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.key = element_rect(fill = "black")) +
  ggtitle("Koeye Salmon No Eelgrass") +
  labs(x = "Size Fraction (mm)", y = "Abundance")

ggsave("KoeyeSalmonNoEelgrass.png")
```

    ## Saving 7 x 5 in image

``` r
p1
```

![](eelgrass1_files/figure-markdown_github/unnamed-chunk-19-1.png)
