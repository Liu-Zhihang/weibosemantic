library(tidyverse)
library(dplyr)
library(ggalluvial)
library(ggthemes)
library(ggrepel)
library(scales)
library(rcartocolor)
library(viridisLite)
library(xts)
library(magrittr)
library(tidyr)
library(lubridate)
library(ggpubr)

# 设置字体
library(showtext)
font_add('timesbd', '/home/lzh/personal/fonts/timesbd.ttf')
showtext_auto()
myfonts = 'timesbd'

# 将图像中的中文日期设置为英文格式
Sys.setlocale("LC_TIME", "English") 


dt_wh = read.csv('data_wh/dt_lda.csv') %>% 
  mutate(date = as.Date(created_at)) %>% 
  arrange(date)

dt_wh %>% 
  mutate(
    year = year(date),
    month = month(date),
    week = week(date),
    day = day(date),
    topic_label = as.numeric(topic_label)
  ) -> dt_wh

# 2. Create an xts object ----
weibo_xts <- xts(unlist(dt_wh$topic_label), order.by = dt_wh$date)
time(weibo_xts) <- lubridate::floor_date(time(weibo_xts), unit = "week")

names(weibo_xts) <- 'topic_label'

data = tibble(date = index(weibo_xts), topic_label = as.numeric(weibo_xts$topic_label))

data %>% 
  group_by(date) %>% 
  nest() -> ls

x = sapply(ls$data, FUN = function(x) table(x$topic_label))
y = bind_rows(x)
y = apply(y, MARGIN = 2, FUN = function(x) as.numeric(x))
y[is.na(y)] = 0

names = c('加油 英雄 抗疫 感谢 致敬 医护人员',
          '医院 隔离 感染 医生 发热 发烧',
          '病毒 美国 风险 国家 复工 健康',
          '希望 口罩 封城 平安 家人 朋友',
          '封城 在家 生活 工作 加油 过年',
          '病例 新增 死亡 累计 人数 数据',
          '封城 在家 结束 开心 希望 喜欢',
          '解封 春天 期待 热干面 樱花 恢复',
          '医院 患者 物资 支援 隔离 捐赠',
          '小区 志愿者 团购 物资 快递 蔬菜',
          '封城 老师 回家 同事 下班 早餐',
          '口罩 证明 防疫 封城 绿码 出行')

names_en = c(
  "Encouragement, Heroes, Anti-epidemic, Gratitude, Tribute",
  "Hospital, Isolation, Infection, Doctor, Fever",
  "Virus, United States, Risk, economy, Health",
  "Hope, Masks, Lockdown, Safety, Family",
  "Lockdown, Home, Life, Work, New Year",
  "Cases, New additions, Deaths, Cumulative number",
  "Lockdown, Home, End, Happy, Hope, Like",
  "Spring, Anticipation, Cherry blossoms, Recovery",
  "Hospital, Patients, Supplies, Support, Donation",
  "Community, Volunteers, Group purchases, Delivery, Vegetables",
  "Lockdown, Teacher, Return home, Colleague, Off work",
  "Masks, Proof, Epidemic prevention, Lockdown, Health code"
)

colnames(y) <- names_en[as.numeric(colnames(y)) + 1]


alluvial_topic <- y %>% as.data.frame() %>% mutate(date = ls$date) %>%
  pivot_longer(cols = 1:length(names), names_to = "topic", values_to = "freq")




## Create a rank of each month and reorder countries based on the final month
alluvial_topic_rank <- alluvial_topic %>% group_by(date) %>%
  mutate(rank = order(order(freq, decreasing=TRUE))) %>%
  mutate(date = ymd(date)) 

#alluvial_topic_tail <- alluvial_topic_rank %>% tail(length(lda_topic)) %>% arrange(rank)

# alluvial_topic_rank <- alluvial_topic_rank %>%
#   mutate(topic = factor(topic, levels = alluvial_topic_tail$topic)) %>%
#   mutate(topic = factor(topic, level = lda_topic_reordered))

## Plot
ggplot(alluvial_topic_rank, aes(x = date, y = freq, alluvium = topic)) +
  geom_alluvium(aes(fill = topic),
                alpha = .9, decreasing = FALSE) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(expand= expansion(mult = c(0,0),
                                 add = c(0,1))) +
  ylab("Number of posts by Topic") +
  scale_fill_viridis_d(option = "turbo") +
  scale_size(guide = "none",
             range = c(1, 10)) +
  #geom_vline(xintercept = wuhan_time_line) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    text = element_text(family = 'Times New Roman', face = 'bold'),
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(family = myfonts, size = 24),
    axis.text = element_text(family = myfonts, size = 20),
    axis.text.x = element_text(family = myfonts, size = 16),
    legend.position = 'top',
    legend.title = element_blank(),
    legend.key.size = unit(5, 'pt'), 
    legend.text = element_text(family = myfonts, size = 16)) +
  scale_x_date(breaks = '2 weeks', date_labels = '%b %d %Y') +
  # 通过gudie来调整图例布局
  guides(fill = guide_legend(
    nrow = 4, 
    byrow = TRUE,
    reverse = T))




