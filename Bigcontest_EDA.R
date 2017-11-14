setwd("C:\\Users\\Kim Sang-Hyun\\Desktop\\BigContest")
library(tidyverse)
mydata <- read_csv('Data_set.csv')
mydata$TARGET <- as.factor(mydata$TARGET) 

## 제2금융권 이하 대출 이력이 많을수록 대출연체 확률이 높을 것이다

ggplot(mydata, aes(BNK_LNIF_CNT)) +
  geom_bar(aes(color = TARGET, fill = TARGET), alpha = 0.3) +
  facet_wrap(~TARGET, scales = 'free_y') +
  scale_fill_manual(values = c('blue', 'red')) +
  scale_color_manual(values = c('blue', 'red')) +
  theme(legend.position = 'none') +
  labs(x = 'BNK_LNIF_CNT', y = '')

ggplot(mydata, aes(CPT_LNIF_CNT)) +
  geom_bar(aes(color = TARGET, fill = TARGET), alpha = 0.3) +
  facet_wrap(~TARGET, scales = 'free_y') +
  scale_fill_manual(values = c('blue', 'red')) +
  scale_color_manual(values = c('blue', 'red')) +
  theme(legend.position = 'none') +
  labs(x = 'CPT_LNIF_CNT', y = '')

ggplot(mydata, aes(SPART_LNIF_CNT)) +
  geom_bar(aes(color = TARGET, fill = TARGET), alpha = 0.3) +
  facet_wrap(~TARGET, scales = 'free_y') +
  scale_fill_manual(values = c('blue', 'red')) +
  scale_color_manual(values = c('blue', 'red')) +
  theme(legend.position = 'none') +
  labs(x = 'SPART_LNIF_CNT', y = '')

ggplot(mydata, aes(ECT_LNIF_CNT)) +
  geom_bar(aes(color = TARGET, fill = TARGET), alpha = 0.3) +
  facet_wrap(~TARGET, scales = 'free_y') +
  scale_fill_manual(values = c('blue', 'red')) +
  scale_color_manual(values = c('blue', 'red')) +
  theme(legend.position = 'none') +
  labs(x = 'ECT_LNIF_CNT', y = '')

## 대출액이 클수록 상환연체 가능성이 높을 것이다

# 오히려 소액대출일수록 TARGET일 확률이 높은 것 같다

# False Nagative인 애들과 이것이 좀 관련이 있진 않을까

mydata %>%
  filter(TOT_LNIF_AMT < 50000) %>%
  ggplot(aes(TOT_LNIF_AMT)) +
  geom_bar(aes(color = TARGET, fill = TARGET), alpha = 0.3) +
  facet_wrap(~TARGET, scales = 'free_y') +
  scale_fill_manual(values = c('blue', 'red')) +
  scale_color_manual(values = c('blue', 'red')) +
  theme(legend.position = 'none') +
  labs(x = 'TOT_LNIF_AMT', y = '')

## 신용카드 발급 개수와 대출연체와의 관계

ggplot(data = mydata, aes(CRDT_CARD_CNT)) +
  geom_bar(aes(color = TARGET, fill = TARGET), alpha = 0.3) +
  facet_wrap(~TARGET, scales = 'free_y') +
  scale_fill_manual(values = c('blue', 'red')) +
  scale_color_manual(values = c('blue', 'red')) +
  theme(legend.position = 'none') +
  labs(x = 'CRDT_CARD_CNT', y = '')

# 신용등급이 낮아서 발급받지 못한 것일까?

## 신용등급과 신용카드 발급 개수와의 관계

ggplot(mydata, aes(CRDT_CARD_CNT, LTST_CRDT_GRAD)) +
  geom_count()

# 신용등급과 대출액수와의 관계

ggplot(mydata, aes(LTST_CRDT_GRAD, TOT_LNIF_AMT)) +
  geom_boxplot(aes(group = LTST_CRDT_GRAD))

## 소득대비대출액과 대출연체와의 관계

mydata %>%
  mutate(TLC_PER_INCM = round(TOT_LNIF_AMT/CUST_JOB_INCM)) %>%
  filter(TLC_PER_INCM <= 100) %>%
  ggplot(aes(TLC_PER_INCM)) +
  geom_bar(aes(color = TARGET, fill = TARGET), alpha = 0.3) +
  facet_wrap(~TARGET, scales = 'free_y') +
  scale_fill_manual(values = c('blue', 'red')) +
  scale_color_manual(values = c('blue', 'red')) +
  theme(legend.position = 'none') +
  labs(x = 'TLC_PER_INCM', y = '')
