names = c(
  '学校 校区 解封 宿舍 封校',
  '隔离 阳性 小区 防疫 方舱',
  '抗原 通知 排队 下楼 封控 静默',
  '解封 希望 结束 生活 自由',
  '无症状 病毒 感染者 病例 恢复',
  '物资 团购 感谢 蔬菜 志愿者',
  '情绪 居家 隔离 影响 焦虑'
)

names_en = c('School, Campus, Reopening, Dormitory, Lockdown',
             'Quarantine, Positive, Residential Community, Epidemic Prevention, Temporary Hospital',
             'Antigen, Notification, Queueing, Downstairs, Blockade, Silence',
             'Reopening, Hope, Ending, Life, Freedom',
             'Asymptomatic, Virus, Infected Person, Cases, Recovery',
             'Supplies, Group Buying, Gratitude, Vegetables, Volunteers',
             'Emotions, Home Stay, Isolation, Impact, Anxiety')

dt_sh = read.csv('data_sh/dt_lda.csv') %>% 
  mutate(date = as.Date(publish_time)) %>% 
  arrange(date)

dt_sh %>% 
  mutate(
    year = year(date),
    month = month(date),
    week = week(date),
    day = day(date),
    topic_label = as.numeric(topic_label)
  ) -> dt_sh

# 2. Create an xts object ----
weibo_xts <- xts(unlist(dt_sh$topic_label), order.by = dt_sh$date)
time(weibo_xts) <- lubridate::floor_date(time(weibo_xts), unit = "week")

names(weibo_xts) <- 'topic_label'

data = tibble(date = index(weibo_xts), topic_label = as.numeric(weibo_xts$topic_label))

data %>% 
  group_by(date) %>% 
  nest() -> ls

x = sapply(ls$data, FUN = function(x) table(x$topic_label))
y = as_tibble(t(x))

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
    #panel.grid = element_blank(),
    text = element_text(family = 'Times New Roman', face = 'bold'),
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(family = myfonts, size = 24),
    axis.text = element_text(family = myfonts, size = 20),
    axis.text.x = element_text(family = myfonts, size = 18),
    legend.position = 'top',
    legend.title = element_blank(),
    legend.key.size = unit(5, 'pt'), 
    legend.text = element_text(family = myfonts, size = 18)) +
  scale_x_date(breaks = '2 weeks', date_labels = '%b %d %Y') +
  # 通过gudie来调整图例布局
  guides(fill = guide_legend(
    nrow = 4, 
    byrow = TRUE,
    reverse = T))






