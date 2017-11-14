setwd("C:\\Users\\Kim Sang-Hyun\\Desktop\\BigContest")
library(tidyverse)
mydata <- read_csv('Data_set.csv')
mydata$TARGET <- as.factor(mydata$TARGET) 

## ��2������ ���� ���� �̷��� �������� ���⿬ü Ȯ���� ���� ���̴�

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

## ������� Ŭ���� ��ȯ��ü ���ɼ��� ���� ���̴�

# ������ �Ҿ״����ϼ��� TARGET�� Ȯ���� ���� �� ����

# False Nagative�� �ֵ�� �̰��� �� ������ ���� ������

mydata %>%
  filter(TOT_LNIF_AMT < 50000) %>%
  ggplot(aes(TOT_LNIF_AMT)) +
  geom_bar(aes(color = TARGET, fill = TARGET), alpha = 0.3) +
  facet_wrap(~TARGET, scales = 'free_y') +
  scale_fill_manual(values = c('blue', 'red')) +
  scale_color_manual(values = c('blue', 'red')) +
  theme(legend.position = 'none') +
  labs(x = 'TOT_LNIF_AMT', y = '')

## �ſ�ī�� �߱� ������ ���⿬ü���� ����

ggplot(data = mydata, aes(CRDT_CARD_CNT)) +
  geom_bar(aes(color = TARGET, fill = TARGET), alpha = 0.3) +
  facet_wrap(~TARGET, scales = 'free_y') +
  scale_fill_manual(values = c('blue', 'red')) +
  scale_color_manual(values = c('blue', 'red')) +
  theme(legend.position = 'none') +
  labs(x = 'CRDT_CARD_CNT', y = '')

# �ſ����� ���Ƽ� �߱޹��� ���� ���ϱ�?

## �ſ��ް� �ſ�ī�� �߱� �������� ����

ggplot(mydata, aes(CRDT_CARD_CNT, LTST_CRDT_GRAD)) +
  geom_count()

# �ſ��ް� ����׼����� ����

ggplot(mydata, aes(LTST_CRDT_GRAD, TOT_LNIF_AMT)) +
  geom_boxplot(aes(group = LTST_CRDT_GRAD))

## �ҵ������װ� ���⿬ü���� ����

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