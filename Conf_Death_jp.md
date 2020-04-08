---
title: "Conf_Death_jp"
author: "km"
date: "2020/04/08"
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
  "data/corona_jp.csv" %>% 
  fread(data.table = F) %>% 
  filter(!is.na(Confirmed)) %>% 
  mutate(Date = ymd(Date)) %>%
  arrange(Date) %>% 
  mutate(from = "JP Ministry")
```


```
##          Date Confirmed  Test Death comment        from
## 52 2020-04-03      2617 39446    63         JP Ministry
## 53 2020-04-04      2935 42882    69         JP Ministry
## 54 2020-04-05      3271 44639    70         JP Ministry
## 55 2020-04-06      3654 46172    73         JP Ministry
## 56 2020-04-07      3906 55311    80         JP Ministry
## 57 2020-04-08      4257 61498    81         JP Ministry
```


```r
gg_cdplot <- 
  function(dat, .date){
    dat %>% 
      ggplot()+
      aes(Confirmed, Death)+
      geom_path()+
      geom_point(aes(shape = from))+
      geom_vline(data = dat %>% filter(Date %in% .date),
                 aes(xintercept = Confirmed), 
                 linetype = "dotted")+ 
      geom_text(data = dat %>% filter(Date %in% .date),
                aes(label = Date, y = Death + 10))+
      theme_classic()+
  labs(subtitle = .subtitle,
       caption = "JP Ministry: https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/0000121431_00086.html")+
  xlab("Total Confiremed")+
  ylab("Total Death")+
      theme(legend.title = element_blank(),
            legend.position = c(0.9, 0.15))
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


g1 <-
  dat %>% 
  gg_cdplot(.date)+
  scale_x_continuous(limits = c(0, max(dat$Confirmed) + 15))
```


```r
dat_d <-
  dat %>% 
  mutate(weekly_Death = Death - lag(Death, 7),
         weekly_Confirmed = Confirmed - lag(Confirmed, 7),
         weekly_Test = Test - lag(Test, 7),
         weekly_positive = weekly_Confirmed / weekly_Test)

g2 <-
  dat_d %>%
  select(Date, weekly_Death, weekly_Confirmed, weekly_Test, weekly_positive) %>% 
  pivot_longer(cols = c(weekly_Confirmed, weekly_Death, weekly_Test, weekly_positive)) %>%
  filter(!is.na(value)) %>% 
  ggplot()+
  aes(Date, value, color = name)+
  geom_point()+
  geom_path()+
  theme_bw()+
  facet_wrap(~name, scales = "free", nrow = 2)+
  scale_y_continuous(limits = c(0, NA))+
  theme(legend.position = "none")+
  xlab("Date")+
  ylab("Weekly total value")+
  labs(subtitle = .subtitle,
       caption = "JP Ministry: https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/0000121431_00086.html")
```


```
## Warning in (function (..., na.rm = FALSE) : 引数は部分的に再利用されます
```

![](Conf_Death_jp_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```
## Warning in (function (..., na.rm = FALSE) : 引数は部分的に再利用されます
```


```r
g2+
  geom_vline(xintercept = c("2020-03-23") %>% ymd,
             linetype = "dotted")+
  geom_text(aes(label = "03-23",
                x = "2020-03-23" %>% ymd,
                y = 0))
```

![](Conf_Death_jp_files/figure-html/unnamed-chunk-8-1.png)<!-- -->



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
## -14.177  -5.398   1.843   7.167   9.057 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 11.578461   2.403802   4.817 3.91e-05 ***
## Confirmed    0.018940   0.001285  14.744 2.77e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.653 on 30 degrees of freedom
## Multiple R-squared:  0.8787,	Adjusted R-squared:  0.8747 
## F-statistic: 217.4 on 1 and 30 DF,  p-value: 2.766e-15
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


