## ---- echo=FALSE--------------------------------------------------------------
library(knitr)
library(rmarkdown)
library(tidyverse, quietly = TRUE)
#render("text.Rmd")

## Global options
options(max.print="75")
opts_chunk$set(echo=TRUE,
#                     cache=TRUE,
               prompt=FALSE,
               tidy=FALSE,
               comment=NA,
               message=FALSE,
               warning=FALSE)
opts_knit$set(fig.width=6,fig.height=3.5)


## ---- echo=TRUE---------------------------------------------------------------
library(tidyverse)
# 例データの作成
set.seed(1)
n_sample <- 500
data_example <- tibble(Year=sample(2010:2019,n_sample,replace=TRUE),
                       Area=factor(sample(str_c("Area",1:5),n_sample,replace=TRUE)),
		       Temp=runif(n_sample,0,30)) %>%
		mutate(log_CPUE=(-0.05)*Year-0.005*(Temp)^2+0.15*Temp+1.2*as.numeric(Area)+100) %>%
		mutate(log_CPUE=log_CPUE+rnorm(n_sample,0,0.3)) %>%
		mutate(CPUE = exp(log_CPUE))


## ---- echo=TRUE---------------------------------------------------------------
g1 <- data_example %>% ggplot() +
  geom_point(aes(y=CPUE, x=Year))
print(g1) # 単にg1だけでもOK


## ---- echo=TRUE---------------------------------------------------------------
plot(CPUE~Year,data=data_example)


## ---- echo=TRUE---------------------------------------------------------------
g1 <- data_example %>% ggplot() +
  geom_point(aes(y=CPUE, x=Year)) +
  theme_bw(base_size=18) # テーマを足す
print(g1)


## ---- echo=TRUE---------------------------------------------------------------
# theme_dark() を試す
g1 + theme_dark(base_size=18)


## ---- echo=TRUE---------------------------------------------------------------
# theme_classic() を試す
g1 + theme_classic(base_size=18)


## ---- echo=TRUE---------------------------------------------------------------
# frasyrで配布している資源評価報告書用のテーマ
g1 + frasyr::theme_SH()


## ---- echo=TRUE---------------------------------------------------------------
# ラベルを変更する
g1 + frasyr::theme_SH() + xlab("Fishing year") + ylab("CPUE of chub mackerel (kg/net)")



## ---- echo=TRUE---------------------------------------------------------------
# 縦軸の範囲を変更する
g1 + frasyr::theme_SH() + coord_cartesian(ylim=c(0,500))


## ---- echo=TRUE---------------------------------------------------------------
data_example %>% ggplot() +
  geom_point(aes(y=CPUE, x=Year, color=Area)) +
  theme_bw(base_size=18)


## ---- echo=TRUE---------------------------------------------------------------
data_example %>% ggplot() +
  geom_point(aes(y=CPUE, x=Year, color=Area, shape=Area)) +
  theme_bw(base_size=18)


## ---- echo=TRUE---------------------------------------------------------------
data_example %>% ggplot() +
  geom_boxplot(aes(y=CPUE, x=factor(Year), color=Area, shape=Area)) +
  theme_bw(base_size=18)


## ---- echo=TRUE---------------------------------------------------------------
data_example %>% ggplot() +
  geom_boxplot(aes(y=CPUE, x=factor(Year), color=Area, shape=Area)) +
  facet_wrap(.~Area) + # Area別に図を分ける
  theme_bw(base_size=18)


## ---- echo=TRUE---------------------------------------------------------------
data_example %>% ggplot() +
  geom_boxplot(aes(y=CPUE, x=factor(Year), color=Area, shape=Area)) +
  facet_wrap(.~Area, scale="free_y") + # 図によってy軸の範囲を変える
  theme_bw(base_size=18) +
  ylim(0,NA)　+ # 最小値をゼロにする
  theme(axis.text.x=element_text(angle = 90)) # 軸のラベルを回転する


## ---- echo=TRUE---------------------------------------------------------------
data_example %>% ggplot() +
  geom_point(aes(y=CPUE, x=Temp, color=Area)) +
  theme_bw(base_size=18) +
  ylim(0,NA)　 # 最小値をゼロにする


## ---- echo=TRUE---------------------------------------------------------------
data_example %>% ggplot() +
  geom_point(aes(y=CPUE, x=Temp, color=Area)) +
  facet_wrap(.~Area, scale="free_y") + 
  theme_bw(base_size=18) +
  ylim(0,NA)　 # 最小値をゼロにする


## ---- echo=TRUE, fig.width=7,fig.height=5-------------------------------------
g2 <- data_example %>% mutate(Temp_category=case_when(Temp<10~"T00-10",
                                                Temp>10 & Temp<20~"T10-20",
						Temp>20~"T20-30")) %>%
		 mutate(Year = factor(Year)) %>%
     ggplot() +
       geom_boxplot(aes(y=CPUE, x=Year, fill=Temp_category)) +
       facet_wrap(.~Area, scale="free_y") +
       theme_bw(base_size=18) +
       theme(legend.position="top")+
       ylim(0,NA)　 # 最小値をゼロにする
print(g2)       


## ---- echo=TRUE, fig.width=7,fig.height=5-------------------------------------
# データ数を1万件に増やす
set.seed(1)
n_sample <- 10000
data_example2 <- tibble(Year=sample(2010:2019,n_sample,replace=TRUE),
                       Area=factor(sample(str_c("Area",1:5),n_sample,replace=TRUE)),
		       Temp=runif(n_sample,0,30)) %>%
		mutate(log_CPUE=(-0.05)*Year-0.005*(Temp)^2+0.15*Temp+1.2*as.numeric(Area)+100) %>%
		mutate(log_CPUE=log_CPUE+rnorm(n_sample,0,0.3)) %>%
		mutate(CPUE = exp(log_CPUE)) %>%
		mutate(Temp_category=case_when(Temp<10~"T00-10",
                                                Temp>10 & Temp<20~"T10-20",
						Temp>20~"T20-30")) %>%
		 mutate(Year = factor(Year))
		 


## ---- echo=TRUE, fig.width=7,fig.height=5-------------------------------------
g2 %+% data_example2

