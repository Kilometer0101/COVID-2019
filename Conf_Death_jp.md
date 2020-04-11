---
title: "Conf_Death_jp"
author: "km"
date: "2020/04/11"
output: 
  html_document:
    keep_md: true
---

## link: [WHO Coronavirus disease (COVID-2019) situation reports](https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports/)





```r
library(tidyverse)
library(ggrepel)
library(lubridate)
library(DT)
library(data.table)
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


```r
.day <- 
  dat$Date %>% max %>% as.character

.subtitle <- str_c("As of ", .day)
```



```
##          Date Confirmed  Test Death comment        from
## 55 2020-04-06      3654 46172    73         JP Ministry
## 56 2020-04-07      3906 55311    80         JP Ministry
## 57 2020-04-08      4257 61498    81         JP Ministry
## 58 2020-04-09      4768 64378    85         JP Ministry
## 59 2020-04-10      5347 68771    88         JP Ministry
## 60 2020-04-11      6005 74891    94         JP Ministry
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
  dat %>% 
  rowid_to_column() %>% 
  mutate(rowid = rowid - max(rowid)) %>% 
  filter(rowid %% 7 == 0) %>% 
  .$Date


g1 <-
  dat %>% 
  gg_cdplot(.date)+
  scale_x_continuous(limits = c(0, max(dat$Confirmed) + 15))

g1
```

![](Conf_Death_jp_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
dat_d <-
  dat %>% 
  mutate(weekly_Death = Death - lag(Death, 7),
         weekly_Confirmed = Confirmed - lag(Confirmed, 7),
         weekly_Test = Test - lag(Test, 7),
         weekly_positive = weekly_Confirmed / weekly_Test)

g2 <-
  dat_d %>%
  select(Date, starts_with("weekly")) %>% 
  pivot_longer(cols = starts_with("weekly")) %>%
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

![](Conf_Death_jp_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

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

![](Conf_Death_jp_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



```r
dat %>% 
  filter(Date >= ymd("2020-04-01")) %>% 
  lm(Death ~ Confirmed, data = .) %>% 
  summary()
```

```
## 
## Call:
## lm(formula = Death ~ Confirmed, data = .)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -2.507 -1.572 -0.571  1.249  4.028 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 3.875e+01  2.133e+00   18.16 2.12e-08 ***
## Confirmed   9.528e-03  5.416e-04   17.59 2.80e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.131 on 9 degrees of freedom
## Multiple R-squared:  0.9717,	Adjusted R-squared:  0.9686 
## F-statistic: 309.5 on 1 and 9 DF,  p-value: 2.804e-08
```


