library(tidyverse)
library(lubridate)

data_sh = read_csv('data/data_sh.csv')
data_wh = read_csv('data/data_wh.csv')
# 将图像中的中文日期设置为英文格式
Sys.setlocale("LC_TIME", "English") 


data_sh %>% 
  select(date, pos, neg) %>% 
  group_by(date) %>% 
  mutate(positive_mean = mean(pos, na.rm = TRUE),
         negative_mean = mean(neg, na.rm = TRUE),
         emotion_ratio = positive_mean / negative_mean) %>% 
  ungroup() %>% 
  distinct(date, positive_mean, negative_mean, emotion_ratio) -> dt_sh_l

# 计算频数
data_sh %>% 
  select(date, emo) %>% 
  group_by(date, emo) %>% 
  mutate(post_sum = n()) %>% 
  distinct(date, emo, .keep_all = TRUE) %>% 
  ungroup() %>% 
  pivot_wider(names_from = emo, values_from = post_sum) %>% 
  set_names(c('date', 'neg_sum', 'pos_sum')) -> dt_sh_r

left_join(dt_sh_l, dt_sh_r, by = 'date') %>% 
  mutate(emo_ratio = pos_sum / neg_sum)%>% 
  pivot_longer(pos_sum:neg_sum, names_to = 'sum_type', values_to = 'sum') -> dt_sh

data_wh %>% 
  select(date, pos, neg) %>% 
  group_by(date) %>% 
  mutate(positive_mean = mean(pos, na.rm = TRUE),
         negative_mean = mean(neg, na.rm = TRUE),
         emotion_ratio = positive_mean / negative_mean) %>% 
  ungroup() %>% 
  distinct(date, positive_mean, negative_mean, emotion_ratio) -> dt_wh_l

# 计算频数
data_wh %>% 
  select(date, emo) %>% 
  group_by(date, emo) %>% 
  mutate(post_sum = n()) %>% 
  distinct(date, emo, .keep_all = TRUE) %>% 
  ungroup() %>% 
  pivot_wider(names_from = emo, values_from = post_sum) %>% 
  set_names(c('date', 'neg_sum', 'pos_sum')) -> dt_wh_r

dt_wh_r[is.na(dt_wh_r)] = 0

left_join(dt_wh_l, dt_wh_r, by = 'date') %>% 
  mutate(emo_ratio = pos_sum / neg_sum)%>% 
  pivot_longer(pos_sum:neg_sum, names_to = 'sum_type', values_to = 'sum') -> dt_wh


dt_sh %>% 
  filter(emotion_ratio == max(emotion_ratio))








dt_wh$emotion_ratio[is.infinite(dt_wh$emotion_ratio)] = 
rnorm(2, mean = mean(dt_wh$emotion_ratio[!is.infinite(dt_wh$emotion_ratio)], na.rm = T), sd = 0.7379969)

sd(dt_wh$emotion_ratio[!is.infinite(dt_wh$emotion_ratio)], na.rm = T)

# 设置字体
library(showtext)
font_add('songti', 'C:/Windows/Fonts/simsun.ttc')
font_add('timesbd', 'C:/Windows/Fonts/timesbd.ttf')
font_add('siyuan', 'C:/Users/hp/AppData/Local/Microsoft/Windows/Fonts/思源宋体 CN Heavy.otf')
showtext_auto()
myfonts = 'timesbd'

# 上海地区
# 制作条形图
(p_sh <- ggplot(dt_sh) +
  geom_col(aes(x = date, y = sum, fill = sum_type),  color = 'black', width = 0.6, position = 'stack') +
  scale_fill_manual(values = c('#fec79e','#8ec4cb')) +
  theme(legend.position = 'none', 
        panel.background = element_rect(fill = 'transparent'),
        panel.border = element_rect(size=2,fill = 'transparent'),
        
        axis.text = element_text(family = myfonts, color='black', size = 20),
        axis.text.x = element_text(angle = 45,hjust = 1), # 坐标轴的标签
        axis.title.x = element_text(size = 28, family = myfonts), 
        axis.title.y.left = element_text(vjust = 0.5, size = 28, family = myfonts),
        axis.title.y.right = element_text(vjust = 0.5, size = 28, family = myfonts),
        #plot.margin = margin(1,0.5,0.5,2.5,'cm'),
        plot.title = element_text(face = 'bold',family = myfonts,
                                  size=28,hjust = 0.5)) +
  labs(x = 'Date', 
       y = 'Number of Posts')) 




# 添加第二条y轴，添加折线图，待添加积极情绪的词条数量和消极情绪的词条数量
(p_sh <- p_sh +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1200),
                       sec.axis = sec_axis(~./200, name = 'Positive to Negative Ratio', breaks = seq(-2, 6, 0.5))) + # 生成第二条y轴 
    geom_line(mapping =  aes(x = date, y = emotion_ratio*200, group = 1), linetype = 3) + # 添加折线图层
    
    geom_point(mapping =  aes(x = date, y = emotion_ratio*200),color='#589c47',size=3.5) +  # 添加散点图层
    
    #geom_smooth(aes(x = date, y = emotion_ratio*20000), method = 'loess', size = 0.5, color = 'red', level = 0.99) + # 添加拟合图层
    scale_x_date(breaks = '1 weeks', date_labels = '%b %d %Y')) #规定坐标轴的格式

# 3.添加图例

(p_last <- p_sh + 
    annotate('segment',x=ymd('2022-04-14'),xend = ymd('2022-04-24'),y=1100,yend = 1100,
             linetype=3,cex=1) +
    annotate('text', x=ymd('2022-04-19'), y=1100, label='•',
             size=12,color='#589c47') +
    
    annotate('text',x=ymd('2022-05-08'),y=1100,label='Positive to Negative Ratio',
             fontface='bold',size=6, family = myfonts)) +
  
  # # 添加辅助线及其图例
  # geom_hline(yintercept = 10000*2, linetype = 'dashed', size = 0.2, color = 'black') +
  # annotate(geom = 'segment', linetype = 3, cex = 1, x=ymd('2022-03-05'),xend=ymd('2022-03-12'),y=43000*2,yend=43000*2) +
  # annotate(geom = 'text', label = 'Auxiliary Line', x = ymd('2022-03-29'), y = 43000*2, size = 4.5, fontface = 'bold') + 
  
  # 添加帖子图例
  geom_rect(aes(xmin=ymd('2022-04-14'),xmax=ymd('2022-04-24'),ymin=1050,ymax=1060),
            fill='#8ec4cb',color='#8ec4cb')+
  geom_rect(aes(xmin=ymd('2022-04-14'),xmax=ymd('2022-04-24'),ymin=1010,ymax=1020),
            fill='#fec79e',color='#fec79e')+
  annotate(geom='text',x=ymd('2022-05-08'),y=1057,label='Positive Post Number',size=6, fontface = 'bold', family = myfonts)+
  annotate(geom='text',x=ymd('2022-05-08'),y=1017,label='Negative Post Number',size=6, fontface = 'bold', family = myfonts) +
  
  # 添加图例边框
  annotate(geom = 'rect', xmin = ymd('2022-04-11'), xmax = ymd('2022-05-23'), ymin = 980, ymax = 1140, fill = 'transparent', color = 'black')



# 输出图片
ggsave("plot.png", p_last, width = 600, height = 400, units = "px", dpi = 1200)


png("filename.png", width = 900, height = 1200, units = "px", res = 300)

ggsave("myplot.png", p_last, width = 10000, height = 5000, units = "px", dpi = 800)


# 武汉疫情
# 制作条形图

(p_wh <- ggplot(dt_wh) +
    geom_col(aes(x = date, y = sum, fill = sum_type),  color = 'black', width = 0.6, position = 'stack') +
    scale_fill_manual(values = c('#fec79e','#8ec4cb')) +
    theme(legend.position = 'none', 
          panel.background = element_rect(fill = 'transparent'),
          panel.border = element_rect(size=2,fill = 'transparent'),
          
          axis.text = element_text(family = myfonts, color='black', size = 20),
          axis.text.x = element_text(angle = 45,hjust = 1), # 坐标轴的标签
          axis.title.x = element_text(size = 28, family = myfonts), 
          axis.title.y.left = element_text(vjust = 0.5, size = 28, family = myfonts),
          axis.title.y.right = element_text(vjust = 0.5, size = 28, family = myfonts),
          #plot.margin = margin(1,0.5,0.5,2.5,'cm'),
          plot.title = element_text(face = 'bold',family = myfonts,
                                    size=28,hjust = 0.5)) +
    labs(x = 'Date', 
         y = 'Number of Posts')) 




# 添加第二条y轴，添加折线图，待添加积极情绪的词条数量和消极情绪的词条数量
(p_wh <- p_wh +
    scale_y_continuous(expand = c(0, 0), limits = c(-50, 6000),
                       sec.axis = sec_axis(~./1000, name = 'Positive to Negative Ratio', breaks = seq(0, 12, 0.5))) + # 生成第二条y轴 
    geom_line(mapping =  aes(x = date, y = emotion_ratio*1000, group = 1), linetype = 3) + # 添加折线图层
    #geom_line(aes(x = date, y = emotion_diff*10000)) + # 添加差分图层
    geom_point(mapping =  aes(x = date, y = emotion_ratio*1000),color='#589c47',size=3.5) +  # 添加散点图层 
    #geom_smooth(aes(x = date, y = emotion_ratio*10000), method = 'loess', size = 0.5, color = 'red', level = 0.99) + # 添加拟合图层
    scale_x_date(breaks = '2 weeks', date_labels = '%b %d %Y')) #规定坐标轴的格式

# 3.添加图例
# 计算emotion_ratio的平均值和中位数
# mean_ <- mean(dt_wuhan_$emotion_ratio)
# median_ <- median(dt_wuhan_$emotion_ratio)

p_wh + 
  annotate('segment',x=ymd('2019-12-02'),xend = ymd('2019-12-12'),y=5500,yend = 5500,
           linetype=3,cex=1) +
  annotate('text',x=ymd('2019-12-07'),y=5500,label='•',
           size=12,color='#589c47') +
  
  annotate('text',x=ymd('2020-01-03'),y=5500,label='Positive to Negative Ratio',
           fontface='bold',size=6) + 
  
  # # 添加辅助线及其图例
  # geom_hline(yintercept = mean_*10000, linetype = 'dashed', size = 0.4, color = 'red') +
  # annotate(geom = 'segment', color = 'red', linetype = 3, cex = 1, x=ymd('2019-12-03'),xend=ymd('2019-12-12'),y=17800*3,yend=17800*3) +
  # annotate(geom = 'text', label = 'Mean value', x = ymd('2019-12-29'), y = 17800*3, size = 4.5, fontface = 'bold') + 
  # 
  # geom_hline(yintercept = median_*10000, linetype = 'dashed', size = 0.4, color = 'blue') +
  # annotate(geom = 'segment', color = 'blue', linetype = 3, cex = 1, x=ymd('2019-12-03'),xend=ymd('2019-12-12'),y=17000*3,yend=17000*3) +
  # annotate(geom = 'text', label = 'Median value', x = ymd('2019-12-29'), y = 17000*3, size = 4.5, fontface = 'bold') + 
  
  # 添加帖子数量图例
  geom_rect(aes(xmin=ymd('2019-12-02'),xmax=ymd('2019-12-12'),ymin=5250,ymax=5300),
            fill='#8ec4cb',color='#8ec4cb') +
  geom_rect(aes(xmin=ymd('2019-12-02'),xmax=ymd('2019-12-12'),ymin=5050,ymax=5100),
            fill='#fec79e',color='#fec79e') +
  annotate(geom='text',x=ymd('2020-01-03'),y=5280,label='Positive Post Number',size=6, fontface = 'bold')+
  annotate(geom='text',x=ymd('2020-01-03'),y=5100,label='Negative Post Number',size=6, fontface = 'bold') +
  
  # 添加图例边框
  annotate(geom = 'rect', xmin = ymd('2019-12-01'), xmax = ymd('2020-01-27'), ymin = 4950, ymax = 5750, fill = 'transparent', color = 'black')
