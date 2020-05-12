---
title: "Conf_Death_jp"
author: "km"
date: "2020/05/13"
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
#  filter(!is.na(Confirmed)) %>% 
  mutate(Date = ymd(Date)) %>%
  arrange(Date) %>% 
  mutate(from = if_else(from == "Ministry", "JP Ministry", "Prefecture total"))
```


```r
.day <- 
  dat$Date %>% max %>% as.character

.subtitle <- str_c("As of ", .day)
```



```
##          Date Confirmed   Test Death        from comment
## 94 2020-05-07     15463 190030   551 JP Ministry        
## 95 2020-05-08     15547 202013   557 JP Ministry        
## 96 2020-05-09     15649 211997   600 JP Ministry        
## 97 2020-05-10     15747 214256   613 JP Ministry        
## 98 2020-05-11     15798 218204   621 JP Ministry        
## 99 2020-05-12     15874 223649   643 JP Ministry
```


```r
gg_cdplot <- 
  function(dat, .date){
    dat %>% 
      ggplot()+
      aes(Confirmed, Death)+
      geom_path(aes(linetype = from))+
      geom_point(aes(shape = from))+
      geom_vline(data = dat %>% filter(Date %in% .date),
                 aes(xintercept = Confirmed), 
                 linetype = "dotted")+ 
      geom_text(data = dat %>%
                  filter(from == "JP Ministry") %>% 
                  filter(Date %in% .date),
                aes(label = Date, y = Death + 20))+
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
  filter(from == "JP Ministry") %>% 
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

![](Conf_Death_jp_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
dat_d <-
  dat %>% 
  group_by(Date) %>% 
  filter(Death == max(Death)) %>% 
  ungroup() %>% 
#  filter(from == "JP Ministry") %>% 
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
  ylab("Rolling 7days sum")+
  labs(subtitle = .subtitle,
       caption = "JP Ministry: https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/0000121431_00086.html")
```


```
## Warning in (function (..., na.rm = FALSE) : 引数は部分的に再利用されます
```

![](Conf_Death_jp_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

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

![](Conf_Death_jp_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



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
##     Min      1Q  Median      3Q     Max 
## -120.79  -51.12  -26.84   62.02  147.96 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.347e+02  2.636e+01   -5.11 5.53e-06 ***
## Confirmed    3.967e-02  2.399e-03   16.53  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 72.48 on 48 degrees of freedom
## Multiple R-squared:  0.8506,	Adjusted R-squared:  0.8475 
## F-statistic: 273.4 on 1 and 48 DF,  p-value: < 2.2e-16
```


