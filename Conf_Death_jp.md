---
title: "Conf_Death_jp"
author: "km"
date: "2020/03/18"
output: 
  html_document:
    keep_md: true
---

## link: [WHO Coronavirus disease (COVID-2019) situation reports](https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports/)





```r
library(tidyverse)
library(ggrepel)
library(lubridate)
```


```r
dat <- 
  "data/corona_conf_death_jp.csv" %>% 
  read.csv(stringsAsFactors = F) %>% 
  filter(!is.na(Confirmed)) %>% 
  mutate(Date = ymd(Date))
```

<!--html_preserve--><div id="htmlwidget-0f4eb4a1b178f9facd05" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0f4eb4a1b178f9facd05">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57"],["2020-03-17","2020-03-16","2020-03-15","2020-03-14","2020-03-13","2020-03-12","2020-03-11","2020-03-10","2020-03-09","2020-03-08","2020-03-07","2020-03-06","2020-03-05","2020-03-04","2020-03-03","2020-03-02","2020-03-01","2020-02-29","2020-02-28","2020-02-27","2020-02-26","2020-02-25","2020-02-24","2020-02-23","2020-02-22","2020-02-21","2020-02-20","2020-02-19","2020-02-18","2020-02-17","2020-02-16","2020-02-15","2020-02-14","2020-02-13","2020-02-12","2020-02-11","2020-02-10","2020-02-09","2020-02-08","2020-02-07","2020-02-06","2020-02-05","2020-02-04","2020-02-03","2020-02-02","2020-02-01","2020-01-31","2020-01-30","2020-01-29","2020-01-28","2020-01-27","2020-01-26","2020-01-25","2020-01-24","2020-01-23","2020-01-22","2020-01-21"],[829,814,780,716,675,620,568,514,488,455,408,349,313,284,268,254,239,230,210,186,164,157,144,132,105,93,85,73,65,59,53,41,33,29,28,26,26,26,25,25,25,23,20,2,20,17,14,11,7,6,4,3,3,1,1,1,1],[28,24,22,21,19,15,12,9,7,6,6,6,6,6,6,6,5,5,4,3,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Date<\/th>\n      <th>Confirmed<\/th>\n      <th>Death<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


```r
.date <-
  c("2020-02-27", "2020-03-08") %>% 
  ymd()

dat %>% 
  ggplot()+
  aes(Confirmed, Death)+
  geom_path()+
  geom_point()+
  geom_vline(data = dat %>% filter(Date %in% .date),
             aes(xintercept = Confirmed), 
             linetype = "dotted")+ 
  geom_text(data = dat %>% filter(Date %in% .date),
            aes(label = Date, y = Death + 5))+
  theme_classic()
```

![](Conf_Death_jp_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
.date <- 
  dat %>% 
  rowid_to_column() %>% 
  mutate(rowid = rowid - 38) %>% 
  filter(rowid %% 7 == 0) %>% 
  .$Date

dat %>% 
  ggplot()+
  aes(Confirmed, Death)+
  geom_path()+
  geom_point()+
  geom_vline(data = dat %>% filter(Date %in% .date),
             aes(xintercept = Confirmed), 
             linetype = "dotted")+ 
  geom_text(data = dat %>% filter(Date %in% .date),
            aes(label = Date, y = Death + 5))+
  theme_classic()
```

![](Conf_Death_jp_files/figure-html/unnamed-chunk-4-2.png)<!-- -->
