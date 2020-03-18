---
title: "Conf_Death"
author: "km"
date: "2020/03/18"
output: 
  html_document:
    keep_md: true
---


## link: [WHO Coronavirus disease (COVID-2019) situation reports](https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports/)





```r
dat <-
  "data" %>% 
  list.files(full.names = TRUE) %>% 
  str_subset(".csv") %>% 
  set_names(.) %>% 
  imap_dfr(~ read.csv(.x, stringsAsFactors = F) %>% 
             mutate(file = basename(.y))) %>% 
  filter(!is.na(Confirmed)) %>% 
  mutate(Area = str_remove(file, "corona_conf_death_"),
         Area = str_remove(Area, ".csv"),
         Area = if_else(Area == "jp", "Japan", Area))

dat %>% 
  ggplot()+
  aes(Confirmed, Death, color = Area)+
  geom_point()+
  geom_path()+
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(~Area)
```

![](Conf_Death_files/figure-html/unnamed-chunk-1-1.png)<!-- -->
