---
title: "Tokyo"
author: "km"
date: "2020/04/10"
output: 
  html_document:
    keep_md: true
---




```r
library(tidyverse)
library(ggrepel)
library(lubridate)
library(DT)
library(data.table)
```


```r
.url <- "https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_patients.csv"

dat_raw <- 
  .url %>% 
  fread(data.table = F)
```


```r
dat <-
  dat_raw %>% 
  rename(date = 公表_年月日,
         age = 患者_年代) %>% 
  mutate(date = ymd(date)) %>% 
  mutate(age = if_else(age == "", "不明", age),
         age = if_else(age == "-", "不明", age))

asof <- 
  dat$date %>% 
  max %>% 
  as.character()

.subtitle <- 
  str_c("As of ", asof,", Tokyo")

dat_nest_age <-
  dat %>% 
  group_nest(age) %>% 
  mutate(n = map_dbl(data, nrow))

dat_nest_age %>% 
  ggplot()+
  aes(age, n/sum(n))+
  geom_bar(stat = "identity")+
  theme(text = element_text(family = "HiraKakuPro-W3"),
        axis.title = element_blank())+
  labs(caption = "https://stopcovid19.metro.tokyo.lg.jp/",
       subtitle = .subtitle)
```

![](Tokyo_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
dat_n <-
  dat %>% 
  group_by(age) %>% 
  mutate(n = 1,
         n = cumsum(n)) %>% 
  ungroup() %>% 
  group_by(age, date) %>% 
  filter(n == max(n)) %>% 
  ungroup()

.xmin <- "2020-01-24" %>% ymd
.xmax <- today() %>% ymd %>% {. + 10}
g_age <-
  dat_n %>% 
  ggplot()+
  aes(date, n, color = age)+
  geom_path()+
  geom_point(alpha = 0.5)+
  geom_text(data = dat_n %>%
              group_by(age) %>% 
              filter(date == max(date)),
            aes(label = age), 
            x = today() %>% ymd %>% {. + 3},
            family = "HiraKakuPro-W3")+
  scale_x_date(expand = c(0.1, 2))+
  theme_classic()+
  theme(text = element_text(family = "HiraKakuPro-W3"),
        legend.position = "none",
        axis.title.x = element_blank())+
  labs(caption = "https://stopcovid19.metro.tokyo.lg.jp/",
       subtitle = .subtitle,
       y = "Total Confirmed")

g_age
```

![](Tokyo_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
ggsave("fig/fig_Tokyo.png", g_age,
       width = 7, height = 4)

g_age_log <-
  g_age+
  scale_y_log10()

g_age_log
```

![](Tokyo_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

```r
ggsave("fig/fig_Tokyo_log.png", g_age_log,
       width = 7, height = 4)
```


```r
g_age + 
  geom_vline(xintercept = "2020-03-23" %>% ymd,
             linetype = "dotted")+
  geom_text(aes(label = "03-23"),
            x = "2020-03-23" %>% ymd,
            y = 100)
```

![](Tokyo_files/figure-html/unnamed-chunk-4-1.png)<!-- -->



