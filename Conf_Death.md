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

g <-
  dat %>% 
  ggplot()+
  aes(Confirmed, Death, color = Area)+
  geom_point()+
  geom_path()+
  geom_text(data = dat %>% 
              group_by(Area) %>% 
              filter(Death == max(Death) & 
                       Confirmed == max(Confirmed)),
            aes(label = Area, x = Confirmed + 20),
            hjust = 0)+
  scale_x_continuous(limits = c(0, 3000))+
  theme_bw()+
  theme(legend.position = "none")
g
```

![](Conf_Death_files/figure-html/unnamed-chunk-1-1.png)<!-- -->


```r
g + 
  facet_wrap(~Area)
```

![](Conf_Death_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


```r
dat_week <-
  dat %>% 
  mutate(Date = ymd(Date)) %>% 
  arrange(desc(Date)) %>% 
  group_by(Area) %>% 
  mutate(tag = if_else(Date == max(Date), 0, 1),
         tag = cumsum(tag) %% 7)

dat_week %>% 
  ggplot()+
  aes(Confirmed, Death, color = Area)+
  geom_point()+
  geom_path()+
  geom_text(data = dat %>% 
              group_by(Area) %>% 
              filter(Death == max(Death) & 
                       Confirmed == max(Confirmed)),
            aes(label = Area, x = Confirmed + 20),
            hjust = 0)+
  geom_vline(data = dat_week %>% filter(tag == 0),
             aes(xintercept = Confirmed), linetype = "dotted")+
  scale_x_continuous(limits = c(0, 3000))+
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(~Area)
```

![](Conf_Death_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
