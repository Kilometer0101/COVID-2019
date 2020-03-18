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


```
##          Date Confirmed Death
## 54 2020-03-13       675    19
## 55 2020-03-14       716    21
## 56 2020-03-15       780    22
## 57 2020-03-16       814    24
## 58 2020-03-17       829    28
## 59 2020-03-18       872    29
```


```r
gg_cdplot <- 
  function(dat, .date){
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
  }
```



```r
.date <-
  c("2020-02-27", "2020-03-08") %>% 
  ymd()

dat %>% 
  gg_cdplot(.date)
```

![](Conf_Death_jp_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
.date <- 
  dat %>% 
  rowid_to_column() %>% 
  mutate(tag = ifelse(Date == "2020-02-27", rowid,0),
         tag = sum(tag)) %>% 
  mutate(rowid = rowid - tag) %>% 
  filter(rowid %% 7 == 0) %>% 
  .$Date

dat %>% 
  gg_cdplot(.date)+
  scale_x_continuous(limits = c(0, max(dat$Confirmed) + 15))
```

![](Conf_Death_jp_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
.date <-
  dat %>% 
  rowid_to_column() %>% 
  mutate(rowid = rowid - max(rowid)) %>% 
  filter(rowid %% 7 == 0) %>% 
  .$Date

dat %>% 
  gg_cdplot(.date)+
  scale_x_continuous(limits = c(0, max(dat$Confirmed) + 15))
```

![](Conf_Death_jp_files/figure-html/unnamed-chunk-5-3.png)<!-- -->


```r
dat_d <-
  dat %>% 
  mutate(d7_Death = Death - lag(Death, 7),
         d7_Conf = Confirmed - lag(Confirmed, 7))

dat_d %>%
  select(Date, d7_Conf, d7_Death) %>% 
  pivot_longer(cols = c(d7_Conf, d7_Death)) %>%
  filter(!is.na(value)) %>% 
  ggplot()+
  aes(Date, value, color = name)+
  geom_point()+
  geom_path()+
  theme_bw()+
  facet_wrap(~name, scales = "free", nrow = 1)+
  theme(legend.position = "none")
```

![](Conf_Death_jp_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
dat %>% 
  filter(Date >= ymd("2020-03-08")) %>% 
  lm(Death ~ Confirmed, data = .) %>% 
  summary()
```

```
## 
## Call:
## lm(formula = Death ~ Confirmed, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.7113 -0.3374  0.1053  0.5172  1.5886 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -19.269478   1.560164  -12.35 6.02e-07 ***
## Confirmed     0.055104   0.002291   24.06 1.77e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.068 on 9 degrees of freedom
## Multiple R-squared:  0.9847,	Adjusted R-squared:  0.983 
## F-statistic: 578.7 on 1 and 9 DF,  p-value: 1.771e-09
```

```r
dat %>% 
  filter(Date >= ymd("2020-02-26"), 
         Date <= ymd("2020-03-02")) %>% 
  lm(Death ~ Confirmed, data = .) %>% 
  summary()
```

```
## 
## Call:
## lm(formula = Death ~ Confirmed, data = .)
## 
## Residuals:
##        1        2        3        4        5        6 
## -0.42249  0.43961  0.19827  0.16382 -0.30169 -0.07753 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -7.06003    1.03927  -6.793 0.002452 ** 
## Confirmed    0.05172    0.00481  10.754 0.000424 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3657 on 4 degrees of freedom
## Multiple R-squared:  0.9666,	Adjusted R-squared:  0.9582 
## F-statistic: 115.6 on 1 and 4 DF,  p-value: 0.0004239
```


