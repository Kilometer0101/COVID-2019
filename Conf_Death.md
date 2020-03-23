---
title: "Conf_Death"
author: "km"
date: "2020/03/23"
output: 
  html_document:
    keep_md: true
---


## link: [WHO Coronavirus disease (COVID-2019) situation reports](https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports/)





```r
dat <- 
  "data/dat_cofdeath.csv" %>% 
  read.csv(stringsAsFactors = F) %>% 
  group_by(Area) %>% 
  mutate(Date = ymd(Date)) %>% 
  arrange(desc(Date)) %>% 
  ungroup()

.xmax <- dat$Confirmed %>% max() %>% {. + 500}
```


```r
gg_confdeath <- function(dat, .exclude = NULL){
  
  dat_g <- dat %>%
    filter(! Area %in% .exclude)

  .xmax <- dat_g$Confirmed %>% max() %>% {. + 500}

  dat_g %>% 
  ggplot()+
  aes(Confirmed, Death, color = Area)+
  geom_point()+
  geom_path()+
  geom_text(data = dat_g %>% 
              group_by(Area) %>% 
              filter(Death == max(Death) & 
                       Confirmed == max(Confirmed)),
            aes(label = Area, x = Confirmed + 20),
            hjust = 0)+
  scale_x_continuous(limits = c(0, .xmax))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(subtitle = .subtitle,
       caption = "WHO: https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports")+
  xlab("Total Confirmed")+
  ylab("Total Death")
}
```


```r
g <-
  dat %>% 
  gg_confdeath(c("US", "UK", "Netherlands"))
g
```

![](Conf_Death_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
  dat %>% 
  gg_confdeath(c("US", "Germany", "UK", "Netherlands"))
```

![](Conf_Death_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
g_jp_G <-
  dat %>%
  filter(Area %in% c("Japan", "Germany")) %>% 
  ggplot()+
  aes(Confirmed, Death, color = Area)+
  geom_point()+
  geom_path()+
  geom_text(data = dat %>% 
              filter(Area %in% c("Japan", "Germany")) %>% 
              group_by(Area) %>% 
              filter(Death == max(Death) & 
                       Confirmed == max(Confirmed)),
            aes(label = Area, x = Confirmed + 20),
            hjust = 0)+
  scale_x_continuous(limits = c(0, .xmax))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(subtitle = .subtitle)

g_jp_G
```

![](Conf_Death_files/figure-html/unnamed-chunk-3-3.png)<!-- -->


```r
g + 
  facet_wrap(~Area)
```

![](Conf_Death_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


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
  scale_x_continuous(limits = c(0, .xmax))+
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(~Area)+
  labs(subtitle = .subtitle)
```

![](Conf_Death_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
dat_week %>% 
  filter(Area %in% c("Japan", "Germany")) %>% 
  ggplot()+
  aes(Confirmed, Death, color = Area)+
  geom_point()+
  geom_path()+
  geom_text(data = dat %>% 
              filter(Area %in% c("Japan", "Germany")) %>%
              group_by(Area) %>% 
              filter(Death == max(Death) & 
                       Confirmed == max(Confirmed)),
            aes(label = Area, x = Confirmed + 20),
            hjust = 0)+
  geom_vline(data = dat_week %>%
               filter(tag == 0) %>%
               filter(Area %in% c("Japan", "Germany")),
             aes(xintercept = Confirmed), linetype = "dotted")+
#  scale_x_continuous(limits = c(0, 3000))+
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(~Area, scales = "free_x")+
  labs(subtitle = .subtitle)
```

![](Conf_Death_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
dat %>% 
  filter(Area == "Germany") %>% 
  filter(Death >= 2) %>% 
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
## -7.698 -3.526  0.874  2.126  9.544 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3.5818832  1.9757548  -1.813   0.0972 .  
## Confirmed    0.0028439  0.0002119  13.421 3.65e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.758 on 11 degrees of freedom
## Multiple R-squared:  0.9424,	Adjusted R-squared:  0.9372 
## F-statistic: 180.1 on 1 and 11 DF,  p-value: 3.654e-08
```
