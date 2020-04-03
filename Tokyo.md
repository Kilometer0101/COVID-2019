---
title: "Tokyo"
author: "km"
date: "2020/04/02"
output: 
  html_document:
    keep_md: true
---




```r
dat_raw <- "data" %>% 
  list.files(full.names = TRUE) %>% 
  str_subset("patients.csv") %>% 
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

![](Tokyo_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

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
```

```
## Warning: Removed 1 rows containing missing values (geom_path).
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

![](Tokyo_files/figure-html/unnamed-chunk-2-2.png)<!-- -->


```r
.d <- {asof %>% dmy} - {"2020-01-24" %>% ymd}

.date <- 
  data.frame(x = seq(0, .d),
             date = "2020-01-24" %>% ymd) %>% 
  mutate(date = date + x)

dat_n %>% 
  select(age, date, n) %>% 
  group_nest(age)
```

```
## # A tibble: 11 x 2
##    age      data             
##    <chr>    <list>           
##  1 10歳未満 <tibble [5 × 2]> 
##  2 10代     <tibble [8 × 2]> 
##  3 20代     <tibble [17 × 2]>
##  4 30代     <tibble [23 × 2]>
##  5 40代     <tibble [26 × 2]>
##  6 50代     <tibble [28 × 2]>
##  7 60代     <tibble [23 × 2]>
##  8 70代     <tibble [27 × 2]>
##  9 80代     <tibble [15 × 2]>
## 10 90代     <tibble [7 × 2]> 
## 11 不明     <tibble [3 × 2]>
```
